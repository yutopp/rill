//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/code_generator/llvm_ir_support.hpp>
#include <rill/code_generator/llvm_ir_generator/helper.hpp>
#include <rill/behavior/intrinsic_action_holder.hpp>
#include <rill/semantic_analysis/analyzer.hpp>

#include <rill/environment/environment.hpp>
#include <rill/environment/make_module_name.hpp>

#include <iterator>
#include <cstdint>

#include <boost/scope_exit.hpp>

#include <boost/range/algorithm/copy.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/adaptor/sliced.hpp>
#include <boost/range/join.hpp>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Verifier.h>

#include <rill/ast/ast.hpp>
#include <rill/ast/detail/tree_filter.hpp>

#define RILL_TID_TO_LLVM_TYPE_TRANSFORMAER                              \
    boost::adaptors::transformed( [this]( auto&&... args ) {            \
        return this->type_id_to_llvm_type_ptr(                          \
            std::forward<decltype(args)>( args )...                     \
            );                                                          \
        } )


namespace rill
{
    namespace code_generator
    {
        // ========================================
        // ========================================


        llvm_ir_generator::llvm_ir_generator(
            const_global_environment_ptr const& g_env,
            intrinsic_action_holder_ptr const& action_holder,
            llvm_ir_generator_context_ptr const& context,
            semantic_analysis::analyzer* const analyzer
            )
            : g_env_( g_env )
            , action_holder_( action_holder )
            , context_( context )
            , analyzer_( analyzer )
        {}



        auto llvm_ir_generator::function_env_to_llvm_constatnt_ptr(
            const_function_symbol_environment_ptr const& f_env
            )
            -> llvm::Constant*
        {
            // if function was not generated, generate function IR
            if ( !context_->env_conversion_table.is_defined( f_env->get_id() ) ) {
                //
                std::cout
                    << "LLVM ir gen / function_env_to_llvm_constatnt_ptr(not defined): " << f_env->get_mangled_name()
                    << std::endl;

                assert( f_env->get_related_ast() != nullptr );
                dispatch( f_env->get_related_ast(), f_env );

                std::cout << "LLVM ir gen / dispatched function_env_to_llvm_constatnt_ptr" << std::endl;
            }

            return [&]() -> llvm::Constant*
            {
                if ( f_env->has_attribute( attribute::decl::k_extern )
                     && !f_env->has_attribute( attribute::decl::k_intrinsic )
                    )
                {
                    // external function
                    llvm::FunctionType* const func_type
                        = context_->env_conversion_table.ref_function_type( f_env->get_id() );

                    auto const& s
                        = std::static_pointer_cast<ast::extern_function_declaration_statement const>( f_env->get_related_ast() );
                    assert( s != nullptr );

                    return context_->llvm_module->getOrInsertFunction( s->get_extern_symbol_name(), func_type );

                } else {
                    // nornal function
                    auto const& target_name
                        = f_env->get_mangled_name();
                    std::cout << "SS: " << target_name << std::endl;

                    if ( auto const f = context_->llvm_module->getFunction( target_name ) )
                        return f;

                    return nullptr;
                }

            }();
        }


        //
        // Root Scope
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::module, s, parent_env )
        {
            auto const import_base
                = s->fullpath.empty()
                ? boost::filesystem::current_path()
                : s->fullpath.parent_path();
            auto const& module_name = make_module_name( import_base, s );
            auto module_env = g_env_->find_module( module_name );

            //
            dispatch( s->program, module_env );
        }

        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::statements, s, parent_env )
        {
            //
            for( auto const& ss : s->statements_ )
                dispatch( ss, parent_env );
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::block_statement, s, parent_env )
        {
            auto const& scope_env = g_env_->get_related_env_by_ast_ptr( s->statements_ );
            assert( scope_env != nullptr );

            dispatch( s->statements_, scope_env );
        }


        //
        // Expression Statement
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::expression_statement, s, parent_env )
        {
            dispatch( s->expression_, parent_env );
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::return_statement, s, parent_env )
        {
            // Return Statement is valid only in Function Envirionment...
            auto const& a_env = parent_env->lookup_layer( kind::type_value::e_function );
            assert( a_env != nullptr ); // TODO: change to error_handler

            auto const& callee_f_env = cast_to<function_symbol_environment>( a_env );
            assert( callee_f_env != nullptr );

            auto const& ret_type_id = callee_f_env->get_return_type_id();
            auto const& ret_type
                = g_env_->get_type_at( callee_f_env->get_return_type_id() );
            bool const returns_heavy_object = is_heavy_object( ret_type );

            llvm::Value* v = dispatch( s->expression_, parent_env );

            if ( returns_heavy_object ) {
                // TODO: implement invokacion of move ctor
                context_->ir_builder.CreateRetVoid();

            } else {
                context_->ir_builder.CreateRet( v );
            }
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::function_definition_statement, s, parent_env )
        {
            std::cout
                << "= function_definition_statement:" << std::endl
                << " Name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << " Args num -- " << s->get_parameter_list().size() << std::endl
                << " Parent env -- " << (const_environment_base_ptr)parent_env << std::endl;

            // ========================================
            //
            auto const& f_env
                = cast_to<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            // ========================================
            // information about paramaters
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            std::cout << "()()=> :" << f_env->get_mangled_name() << std::endl;


            // ========================================
            //
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END


            // ========================================
            // TODO: use current_insert_point.isSet(), if it is false, this function needs external linkage( currently, all functions are exported as external linkage )
            auto const linkage = llvm::Function::ExternalLinkage;
            if ( current_insert_point.isSet() ) {
                //
                std::cout << "not external." << std::endl;
            }


            // ========================================
            auto const& ret_type_id = f_env->get_return_type_id();
            auto const& ret_type
                = g_env_->get_type_at( f_env->get_return_type_id() );
            bool const returns_heavy_object = is_heavy_object( ret_type );


            // ========================================
            // signature
            std::vector<llvm::Type*> parameter_types;
            if ( returns_heavy_object ) {
                parameter_types.push_back(
                    type_id_to_llvm_type_ptr( ret_type_id )
                    );
            }
            boost::copy(
                parameter_variable_type_ids | RILL_TID_TO_LLVM_TYPE_TRANSFORMAER,
                std::back_inserter( parameter_types )
                );


            // ========================================
            // return type
            llvm::Type* const return_type
                = returns_heavy_object
                ? llvm::Type::getVoidTy( context_->llvm_context )
                : type_id_to_llvm_type_ptr( ret_type_id );

            // ========================================
            // function type signature
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // ========================================
            // function body
            llvm::Function* const func = llvm::Function::Create( func_type, linkage, f_env->get_mangled_name(), context_->llvm_module.get() );


            // ========================================
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_block
                = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( basic_block );


            // ========================================
            std::size_t i = 0;
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;

                if ( ait == func->arg_begin() && returns_heavy_object ) {
                    ait->setName( "__ret_target" );

                } else {
                    auto const& var =
                        g_env_->get_env_at_as_strong_ref<variable_symbol_environment const>( parameter_variable_decl_env_ids[i] );
                    ait->setName( var->get_mangled_name() );

                    store_value( ait, var );
                    ++i;
                }
            }


            // ========================================
            // func->setGC( "shadow-stack" );


            // ========================================
            // generate statements
            dispatch( s->inner_, f_env );

            //
            if ( !f_env->is_closed() ) {
                if ( returns_heavy_object ) {
                    ;
                } else {
                    context_->ir_builder.CreateRetVoid();
                }
            }

            //
            llvm::verifyFunction( *func );
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::class_function_definition_statement, s, parent_env )
        {
            std::cout
                << "= class_function_definition_statement:" << std::endl
                << " Name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << " Args num -- " << s->get_parameter_list().size() << std::endl
                << " Parent env -- " << (const_environment_base_ptr)parent_env << std::endl;

            // ========================================
            // get current function environment
            auto const& f_env
                = cast_to<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;
            context_->env_conversion_table.bind_function_type( f_env->get_id(), nullptr ); // guard

            // ========================================
            // information about paramaters
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            std::cout << "()()=> :" << f_env->get_mangled_name() << std::endl;


            // ========================================
            // register LLVM instruction point
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END


            // ========================================
            // TODO: use current_insert_point.isSet(), if it is false, this function needs external linkage( currently, all functions are exported as external linkage )
            auto const linkage = llvm::Function::ExternalLinkage;
            if ( current_insert_point.isSet() ) {
                //
                std::cout << "not external." << std::endl;
            }


            // ========================================
            auto const& ret_type_id = f_env->get_return_type_id();
            auto const& ret_type
                = g_env_->get_type_at( f_env->get_return_type_id() );
            bool const returns_heavy_object = is_heavy_object( ret_type );


            // ========================================
            // parameter signature
            std::vector<llvm::Type*> parameter_types;
            if ( returns_heavy_object ) {
                parameter_types.push_back(
                    type_id_to_llvm_type_ptr( ret_type_id )
                    );
            }
            boost::copy(
                parameter_variable_type_ids | RILL_TID_TO_LLVM_TYPE_TRANSFORMAER,
                std::back_inserter( parameter_types )
                );


            // ========================================
            // return type
            llvm::Type* const return_type
                = returns_heavy_object
                ? llvm::Type::getVoidTy( context_->llvm_context )
                : type_id_to_llvm_type_ptr( ret_type_id );

            // ========================================
            // function type signature
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // ========================================
            // function body
            llvm::Function* const func
                = llvm::Function::Create( func_type, linkage, f_env->get_mangled_name(), context_->llvm_module.get() );

            // ========================================
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_block
                = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( basic_block );

            // ========================================
            // function parameter variables
            // and, make local variable creation
            std::size_t i = 0;
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;
                if ( ait == func->arg_begin() && returns_heavy_object ) {
                    ait->setName( "__ret_target" );

                } else {
                    auto const& var =
                        g_env_->get_env_at_as_strong_ref<variable_symbol_environment const>( parameter_variable_decl_env_ids[i] );
                    ait->setName( var->get_mangled_name() );

                    store_value( ait, var );
                    ++i;
                }
            }

            // ========================================
            // func->setGC( "shadow-stack" );

            // ========================================
            // generate statements
            dispatch( s->inner_, f_env );

            // ========================================
            // Only the function that returns void is allowed to has no return statement
            if ( f_env->get_return_type_candidates().size() == 0 )
                context_->ir_builder.CreateRetVoid();

            //
            llvm::verifyFunction( *func );

            //
            std::cout << "class function" << std::endl;
            func->dump();
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::class_definition_statement, s, parent_env )
        {
            //
            auto const& c_env
                = cast_to<class_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( c_env != nullptr );
            if ( context_->env_conversion_table.is_defined( c_env->get_id() ) )
                return;

            std::cout << "class! : " << c_env->get_base_name() << std::endl;

            if ( s->inner_ ) {
                // if inner statements, it will be USER DEFINED NORMAL class

                //
                context_->env_conversion_table.create_class_variable_type_holder(
                    c_env->get_id()
                    );

                // construct incomplete Struct Type...
                llvm::StructType* const llvm_struct_type
                    = llvm::StructType::create(
                        context_->llvm_context,
                        c_env->get_mangled_name()/*, add is_packed*/
                        );
                context_->env_conversion_table.bind_type( c_env, llvm_struct_type );

                // filter statements
                // collect class variables
                ast::detail::apply_to_const_node<ast::class_variable_declaration_statement>(
                    s->inner_,
                    c_env,
                    [&](
                        ast::const_class_variable_declaration_statement_ptr const& node,
                        const_environment_base_ptr const& env
                        )
                    {
                        //
                        std::cout << "ast::class_variable_declaration_statement" << std::endl;
                        this->dispatch( node, env );
                    } );

                std::cout
                    << "class ------> " << c_env->get_id() << std::endl
                    << "  member num: " << context_->env_conversion_table.ref_class_variable_type_list( c_env->get_id() ).size() << std::endl;

                llvm_struct_type->setBody( context_->env_conversion_table.ref_class_variable_type_list( c_env->get_id() ) );

                //
                dispatch( s->inner_, c_env );

            } else {
                assert( false && "[ice] invalid type" );
            }
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::variable_declaration_statement, s, parent_env )
        {
            // TODO: all of variablea(mutable) should be allocated at head of function...
            // TODO: check kind...

            // cast to variable symbol env
            auto const& v_env
                = std::static_pointer_cast<variable_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( v_env != nullptr );

            // initial value
            if ( s->declaration_.decl_unit.init_unit.initializer ) {
                // has default value
                auto const& initial_llvm_value
                    = dispatch( s->declaration_.decl_unit.init_unit.initializer, v_env );
                assert( initial_llvm_value != nullptr && "[ice]" );

                store_value( initial_llvm_value, v_env );

            } else {
                // has NO initial value...
                // TODO: implement default call constructor call by semantics analizer

                auto const& initial_llvm_value = nullptr;

                store_value( initial_llvm_value, v_env );
            }
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::class_variable_declaration_statement, s, parent_env )
        {
            assert( parent_env != nullptr );
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // cast to variable symbol env
            auto const& v_env
                = std::static_pointer_cast<variable_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( v_env != nullptr );

            // duplicate check
            assert( v_env->is_in_class() );
            if ( context_->env_conversion_table.is_defined( v_env->get_parent_class_env_id(), v_env->get_id() ) )
                return;


            //
            std::cout << "v_env->get_type_id() = " << v_env->get_type_id() << std::endl;


            auto const& variable_type = g_env_->get_type_at( v_env->get_type_id() );
            if ( !context_->env_conversion_table.is_defined( variable_type.class_env_id ) ) {
                auto const& c_env = g_env_->get_env_at_as_strong_ref( variable_type.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }


            auto const& variable_llvm_type = context_->env_conversion_table.ref_type( variable_type.class_env_id );
            auto const& variable_attr =  variable_type.attributes;


            // in struct, variable is all normal type
            context_->env_conversion_table.bind_class_variable_type(
                v_env->get_parent_class_env_id(),
                v_env->get_id(),
                variable_llvm_type
                );
#if 0
            //
            switch( variable_attr.quality )
            {
            case attribute::holder_kind::k_val:

                switch( variable_attr.modifiability )
                {
                case attribute::modifiability_kind::k_immutable:
                {
                    std::cout << "ABABABAB: " << parent_env->get_id() << std::endl;
                    context_->env_conversion_table.bind_class_variable_type( v_env->get_parent_class_env_id(), v_env->get_id(), variable_llvm_type );
                }
                    break;

                case attribute::modifiability_kind::k_const:
                    assert( false && "[ice]" );
                    break;

                case attribute::modifiability_kind::k_mutable:
                {
                    context_->env_conversion_table.bind_class_variable_type( v_env->get_parent_class_env_id(), v_env->get_id(), variable_llvm_type->getPointerTo() );
                }
                    break;
                }

                break;

            case attribute::holder_kind::k_ref:
                assert( false && "not implemented..." );
                break;

            default:
                assert( false && "[ice]" );
                break;
            }
#endif
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::extern_function_declaration_statement, s, parent_env )
        {
            // cast to function symbol env
            auto const& f_env
                = cast_to<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) ) {
                return;
            }
            if ( f_env->has_attribute( attribute::decl::k_intrinsic ) ) {
                return;
            }

            //
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END

            //
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();

            // define paramter and return types
            std::vector<llvm::Type*> parmeter_types;
            for( auto const& var_type_id : parameter_variable_type_ids ) {
                auto const& v = g_env_->get_type_at( var_type_id );
                if ( !context_->env_conversion_table.is_defined( v.class_env_id ) ) {
                    auto const& c_env = g_env_->get_env_at_as_strong_ref( v.class_env_id );
                    dispatch( c_env->get_related_ast(), c_env );
                }
                parmeter_types.push_back( context_->env_conversion_table.ref_type( v.class_env_id ) );
            }
            auto const& v = g_env_->get_type_at( f_env->get_return_type_id() );
            if ( !context_->env_conversion_table.is_defined( v.class_env_id ) ) {
                auto const& c_env = g_env_->get_env_at_as_strong_ref( v.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }
            auto const& return_type = context_->env_conversion_table.ref_type( v.class_env_id );

            // get function type
            llvm::FunctionType* const func_type
                = llvm::FunctionType::get( return_type, parmeter_types, false/*is not variadic*/ );

            //
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            //
            std::cout << "extern" << std::endl;
            func_type->dump();
            //assert( false );
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::extern_class_declaration_statement, s, parent_env )
        {
            auto const& c_env
                = cast_to<class_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( s ) );
            assert( c_env != nullptr );
            if ( context_->env_conversion_table.is_defined( c_env->get_id() ) ) {
                return;
            }

            if ( c_env->has_attribute( attribute::decl::k_intrinsic ) ) {
                // it will be BUILTIN class
                std::cout << "builtin! : " << c_env->get_base_name() << std::endl;

                if ( auto&& id = action_holder_->is_registered( s->extern_symbol_name_ ) ) {
                    auto const& action = action_holder_->at( *id );
                    assert( action != nullptr );

                    action->invoke(
                        processing_context::k_llvm_ir_generator_typing,
                        context_,
                        c_env
                        );

                } else {
                    // special treatment for Array...
                    if ( c_env->is_array() ) {
                        std::cout << "array" << std::endl;
                        auto const& array_detail = c_env->get_array_detail();
                        assert( array_detail != nullptr );
                        auto const& array_inner_type
                            = g_env_->get_type_at( array_detail->inner_type_id );
                        regard_env_is_defined( array_inner_type.class_env_id );

                        llvm::Type* const inner_type
                            = type_id_to_llvm_type_ptr( array_detail->inner_type_id );

                        std::cout << "NUM: " << array_detail->elements_num << std::endl;

                        auto const& array_ty = llvm::ArrayType::get(
                            inner_type,
                            array_detail->elements_num
                            );

                        context_->env_conversion_table.bind_type(
                            c_env,
                            array_ty
                            );

                    } else if ( c_env->is_pointer() ) {
                        std::cout << "pointer" << std::endl;
                        auto const& ptr_detail = c_env->get_pointer_detail();
                        assert( ptr_detail != nullptr );
                        auto const& ptr_inner_type
                            = g_env_->get_type_at( ptr_detail->inner_type_id );
                        regard_env_is_defined( ptr_inner_type.class_env_id );

                        llvm::Type* ptr_inner_ty
                            = context_->env_conversion_table.ref_type( ptr_inner_type.class_env_id );
                        llvm::Type* ptr_ty = ptr_inner_ty->getPointerTo();

                        context_->env_conversion_table.bind_type(
                            c_env,
                            ptr_ty
                            );

                    } else {
                        // another builtin types are defined at beheviour/register_default_core.cpp ...
                        assert( false && "[[ice]] reached..." );
                    }
                }

            } else {
                std::cout << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;
                assert( false && "[ice] invalid type" );
            }
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::while_statement, s, parent_env )
        {
            llvm::BasicBlock* const while_begin_block
                = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );
            llvm::BasicBlock* const body_block
                = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );
            llvm::BasicBlock* const final_block
                = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );

            //
            context_->ir_builder.CreateBr( while_begin_block );
            context_->ir_builder.SetInsertPoint( while_begin_block );

            //
            auto const& scope_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( scope_env != nullptr );

            // conditional
            auto const& cond_llvm_value = dispatch( s->conditional_, scope_env );
            if ( scope_env->is_closed() ) {
                context_->ir_builder.CreateUnreachable();
            }

            context_->ir_builder.CreateCondBr( cond_llvm_value, body_block, final_block );

            //
            context_->ir_builder.SetInsertPoint( body_block );
            auto const& body_scope_env = g_env_->get_related_env_by_ast_ptr( s->body_statement_ );
            assert( scope_env != nullptr );
            dispatch( s->body_statement_, body_scope_env );
            if ( !body_scope_env->is_closed() ) {
                context_->ir_builder.CreateBr( while_begin_block );
            }

            //
            context_->ir_builder.SetInsertPoint( final_block );
            if ( body_scope_env->is_closed() ) {
                context_->ir_builder.CreateUnreachable();
            }
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::if_statement, s, parent_env )
        {
            llvm::BasicBlock* const if_begin_block
                = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );
            llvm::BasicBlock* const then_block
                = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );
            llvm::BasicBlock* const else_block
                = s->else_statement_
                ? llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() )
                : nullptr
                ;
            llvm::BasicBlock* const final_block
                = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );

            //
            context_->ir_builder.CreateBr( if_begin_block );
            context_->ir_builder.SetInsertPoint( if_begin_block );

            //
            auto const& scope_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( scope_env != nullptr );

            // conditional
            auto const& cond_llvm_value = dispatch( s->conditional_, scope_env );
            assert( scope_env != nullptr );
            if ( scope_env->is_closed() ) {
                context_->ir_builder.CreateUnreachable();
            }

            if ( s->else_statement_ ) {
                // true -> true block, false -> else block
                context_->ir_builder.CreateCondBr( cond_llvm_value, then_block, else_block );
            } else {
                // true -> true block, false -> final block
                context_->ir_builder.CreateCondBr( cond_llvm_value, then_block, final_block );
            }

            int unreachable_count = 0;

            // then
            context_->ir_builder.SetInsertPoint( then_block );
            auto const& then_scope_env = g_env_->get_related_env_by_ast_ptr( s->then_statement_ );
            assert( then_scope_env != nullptr );
            dispatch( s->then_statement_, then_scope_env );
            if ( !then_scope_env->is_closed() ) {
                context_->ir_builder.CreateBr( final_block );
            } else {
                ++unreachable_count;
            }

            //
            if ( s->else_statement_ ) {
                context_->ir_builder.SetInsertPoint( else_block );
                auto const& else_scope_env = g_env_->get_related_env_by_ast_ptr( s->else_statement_ );
                assert( else_scope_env != nullptr );
                dispatch( s->else_statement_, else_scope_env );
                if ( !else_scope_env->is_closed() ) {
                    context_->ir_builder.CreateBr( final_block );
                } else {
                    ++unreachable_count;
                }
            }

            //
            context_->ir_builder.SetInsertPoint( final_block );
            if ( unreachable_count == 2 ) {
                context_->ir_builder.CreateUnreachable();
            }
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::binary_operator_expression, e, parent_env )
        {
            // Look up Function
            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );

            std::cout << "current : " << f_env->get_mangled_name() << std::endl;

            // call function that defined in rill modules
            // evaluate argument from last to front(but ordering of vector is from front to last)
            ast::expression_list const& e_arguments = { e->lhs_, e->rhs_ };
            std::vector<llvm::Value*> args = eval_args(
                f_env->get_parameter_type_ids(),
                e_arguments,
                parent_env
                );

            std::cout << "CALL!!!!!" << std::endl;

            // if intrinsic function
            if ( f_env->has_attribute( attribute::decl::k_intrinsic ) ) {
                // look up the action
                auto const& action = action_holder_->at( f_env->get_action_id() );
                assert( action != nullptr );

                // generate codes into this context
                auto const value = action->invoke(
                    processing_context::k_llvm_ir_generator,
                    context_,
                    f_env,
                    args
                    );
                assert( value != nullptr );
                return value;

            } else {
                auto const& callee_function = function_env_to_llvm_constatnt_ptr( f_env );
                if ( !callee_function ) {
                    // unexpected error...
                    assert( false && "unexpected... callee_function was not found" );
                }

                if ( is_jit() ) {
                    assert( false && "pyaaaaaaaaa" );
//                analyzer_->ctfe_engine_->execution_engine_;//();
                    return nullptr;
                } else {
                    // invocation
                    return context_->ir_builder.CreateCall( callee_function, args, "calltmp" );
                }
            }
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::unary_operator_expression, e, parent_env )
        {
            // Look up Function
            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );

            std::cout << "current : " << f_env->get_mangled_name() << std::endl;

            // call function that defined in rill modules
            // evaluate argument from last to front(but ordering of vector is from front to last)
            ast::expression_list const& e_arguments = { e->src };
            std::vector<llvm::Value*> args = eval_args(
                f_env->get_parameter_type_ids(),
                e_arguments,
                parent_env
                );

            std::cout << "CALL!!!!!" << std::endl;

            // if intrinsic function
            if ( f_env->has_attribute( attribute::decl::k_intrinsic ) ) {
                // look up the action
                auto const& action = action_holder_->at( f_env->get_action_id() );
                assert( action != nullptr );

                // generate codes into this context
                auto const value = action->invoke(
                    processing_context::k_llvm_ir_generator,
                    context_,
                    f_env,
                    args
                    );
                assert( value != nullptr );
                return value;

            } else {
                auto const& callee_function = function_env_to_llvm_constatnt_ptr( f_env );
                if ( !callee_function ) {
                    // unexpected error...
                    assert( false && "unexpected... callee_function was not found" );
                }

                if ( is_jit() ) {
                    assert( false && "pyaaaaaaaaa" );
//                analyzer_->ctfe_engine_->execution_engine_;//();
                    return nullptr;
                } else {
                    // invocation
                    return context_->ir_builder.CreateCall( callee_function, args, "calltmp" );
                }
            }
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::element_selector_expression, e, parent_env )
        {
            //
            std::cout << "element selection" << std::endl;

            auto const& element_env = g_env_->get_related_env_by_ast_ptr( e );
            if ( element_env != nullptr ) {
                // this pass processes variables belongs the class
                std::cout << "has element / kind " << debug_string( element_env->get_symbol_kind() ) << std::endl;

                // this element selsction affects to the reciever

                if ( element_env->get_symbol_kind() == kind::type_value::e_variable ) {
                    // variable that belonged to class
                    auto const& v_env
                        = std::static_pointer_cast<variable_symbol_environment const>(
                            element_env
                            );
                    assert( v_env != nullptr );
                    assert( v_env->is_in_class() );

                    //return context_->env_conversion_table.ref_value( v_env->get_id() );
                    llvm::Value* const lhs = dispatch( e->reciever_, parent_env );

                    lhs->dump();
                    lhs->getType()->dump();

                    // data index in struct
                    auto const& index
                        = context_->env_conversion_table.get_class_variable_index(
                            v_env->get_parent_class_env_id(),
                            v_env->get_id()
                            );
                    std::cout << "index: " << index << std::endl;

                    auto value
                        = context_->ir_builder.CreateStructGEP( lhs, index );
                    context_->represented_as_pointer_set.emplace( value );

                    return value;

                } else if ( element_env->get_symbol_kind() == kind::type_value::e_multi_set ) {
                    // TODO: see representation kind

                    // class function invocation
                    std::cout << "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" << std::endl;
                    std::cout << "class function invocation" << std::endl;


                    // eval reciever
                    llvm::Value* const lhs = dispatch( e->reciever_, parent_env );
                    std::cout << "GEN" << std::endl;
                    lhs->dump();
                    lhs->getType()->dump();

                    // push temporary value
                    context_->temporary_reciever_stack_.push(
                        std::make_tuple(
                            g_env_->get_related_type_id_by_ast_ptr( e->reciever_ ),
                            lhs
                            )
                        );

                    //
                    llvm::Value* const rhs = dispatch( e->selector_id_, parent_env );

                    // eval reciever
                    return rhs;

                } else {
                    assert( false && "[[ice]]" );
                }

            } else {
                // namespace
                assert( false && "[ababa!w]" );
                return nullptr;
                //std::cout << "has NO selection" << std::endl;
                //llvm::Value* const rhs = dispatch( e->selector_id_, parent_env );
                //llvm::Value* const lhs = dispatch( e->reciever_, parent_env );

                //return lhs;
            }
        }


        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::subscrpting_expression, e, parent_env )
        {
            std::cout << "subscripting selection" << std::endl;

            auto const& target_env = g_env_->get_related_env_by_ast_ptr( e );
            assert( target_env != nullptr );

            // evaluate [rhs] first
            llvm::Value* rhs_value = e->rhs_ ? dispatch( *e->rhs_, parent_env ) : nullptr;

            //
            llvm::Value* lhs_value = dispatch( e->lhs_, parent_env );

            //
            std::cout << debug_string( target_env->get_symbol_kind() ) << std::endl;
            if ( target_env->get_symbol_kind() == kind::type_value::e_class ) {
                // builtin array type...
                auto const& rhs_c_env
                    = std::static_pointer_cast<class_symbol_environment const>(
                        target_env
                        );
                lhs_value->dump();

                assert( rhs_c_env->is_array() );    // TODO: remove

                // TODO: fix...
                // load array address
                auto const& pp = [&]() -> llvm::Value* {
                    if ( rhs_c_env->is_array() ) {
                        return lhs_value;
                    } else {
                        return context_->ir_builder.CreateLoad( lhs_value );
                    }
                }();

                pp->dump();
                context_->llvm_module->dump();

                // TODO: fix...
                // rhs must be integer. if it is pointer, load instruction is required...
                auto const& idx_value = rhs_value->getType()->isPointerTy() ? context_->ir_builder.CreateLoad( rhs_value ) : rhs_value;
                // load address of index 0
                llvm::Value *head
                    = context_->ir_builder.CreateConstInBoundsGEP2_32( pp, 0, 0 );

                llvm::Value* elem_v
                    = context_->ir_builder.CreateInBoundsGEP(
                        head,
                        idx_value
                        );

                context_->represented_as_pointer_set.emplace( elem_v );

                return elem_v;

            } else if ( target_env->get_symbol_kind() == kind::type_value::e_function ) {
                //
                assert( false && "[ICE] TODO: call operator[]");

            } else {
                assert( false );
            }

            return nullptr;
        }


        //
        //
        //
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::call_expression, e, parent_env )
        {
            std::cout << "CALL expr" << std::endl;

            // ========================================
            // look up self function
            auto const f_env
                = cast_to<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );

            // evaluate lhs(reciever)
            // if reciever is exist, valid value and type will be stacked
            dispatch( e->reciever_, parent_env );

            // ========================================
            std::cout << "current : " << f_env->get_mangled_name() << std::endl;
            auto const& callee_function = function_env_to_llvm_constatnt_ptr( f_env );
            if ( !callee_function ) {
                // unexpected error...
                assert( false && "unexpected... callee_function was not found" );
            }

            auto const& ret_type_id = f_env->get_return_type_id();
            auto const& ret_type = g_env_->get_type_at( ret_type_id );
            bool const returns_heavy_object = is_heavy_object( ret_type );



            // arguments
            // TODO: reduce coping
            std::vector<llvm::Value*> total_args = [&]() -> std::vector<llvm::Value*> {
                std::vector<llvm::Value*> args;

                if ( returns_heavy_object ) {
                    args.push_back( nullptr );
                }

                if ( f_env->is_in_class() ) {
                    // if member function, take temporary space for "this" pointer
                    args.push_back( nullptr );

                    auto const& parameter_type_ids
                        = f_env->get_parameter_type_ids();
                    // this is not included in e->arguments_
                    auto const& args_without_this
                        = eval_args(
                            parameter_type_ids
                                | boost::adaptors::sliced( 1, parameter_type_ids.size() ),
                            e->arguments_,
                            parent_env
                            );

                    // copy to total_args
                    std::copy(
                        args_without_this.cbegin(),
                        args_without_this.cend(),
                        std::back_inserter( args )
                        );

                } else {
                    auto const& parameter_type_ids
                    = f_env->get_parameter_type_ids();
                    if( parameter_type_ids.size() != e->arguments_.size() ) {
                        // maybe UFCS, take space for 1st arg
                        args.push_back( nullptr );

                    // this is not included in e->arguments_
                    auto const& args_without_this
                        = eval_args(
                            parameter_type_ids
                                | boost::adaptors::sliced( 1, parameter_type_ids.size() ),
                            e->arguments_,
                            parent_env
                            );

                    // copy to total_args
                    std::copy(
                        args_without_this.cbegin(),
                        args_without_this.cend(),
                        std::back_inserter( args )
                        );

                    } else {
                        auto const& args_normal
                        = eval_args(
                            parameter_type_ids,
                            e->arguments_,
                            parent_env
                            );

                    std::copy(
                        args_normal.cbegin(),
                        args_normal.cend(),
                        std::back_inserter( args )
                        );
                    }


                }

                return args;
            }();



            if ( returns_heavy_object ) {
                // create storage
                llvm::Type* llvm_ty
                    = context_->env_conversion_table.ref_type( ret_type.class_env_id );
                llvm::AllocaInst* const allca_inst
                    = context_->ir_builder.CreateAlloca( llvm_ty );

                //
                total_args[0] = allca_inst;
            }

            // save the reciever object to the temprary space
            if ( f_env->is_in_class() || context_->temporary_reciever_stack_.size() > 0 ) {
                auto const this_index
                    = returns_heavy_object
                    ? 1
                    : 0;
                //
                if ( f_env->is_initializer() ) {
                    assert( false );
                    assert( returns_heavy_object == false );

                    // constructor
                    // the first argument will be this
                    auto const this_var_type_id = f_env->get_parameter_type_ids()[0];
                    auto const& this_var_type = g_env_->get_type_at( this_var_type_id );
                    if ( !context_->env_conversion_table.is_defined( this_var_type.class_env_id ) ) {
                        auto const& c_env = g_env_->get_env_at_as_strong_ref( this_var_type.class_env_id );
                        dispatch( c_env->get_related_ast(), c_env );
                    }

                    auto const& variable_llvm_type
                        = context_->env_conversion_table.ref_type( this_var_type.class_env_id );
                    assert( variable_llvm_type != nullptr );

                    llvm::AllocaInst* const allca_inst
                        = context_->ir_builder.CreateAlloca(
                            variable_llvm_type
                            );

                    context_->temporary_reciever_stack_.push(
                        std::make_tuple( this_var_type_id, allca_inst )
                        );
                }
                assert( context_->temporary_reciever_stack_.size() > 0 );

                auto const& reciever_obj_value
                    = context_->temporary_reciever_stack_.top();

                auto const& parameter_type
                    = g_env_->get_type_at( f_env->get_parameter_type_ids().at( 0 ) );

                auto const& ty
                    = g_env_->get_type_at(
                        std::get<0>( reciever_obj_value )
                        );
                auto const val
                    = std::get<1>( reciever_obj_value );
                assert( val != nullptr );

                std::cout << "SIZE: " << total_args.size() << " // " << this_index << std::endl;
                // set "this"
                total_args[this_index] = convert_value_by_attr(
                    parameter_type,
                    ty,
                    val
                    );
                //assert( total_args[this_index] != nullptr );
                context_->temporary_reciever_stack_.pop();

                auto const ret
                    = context_->ir_builder.CreateCall( callee_function, total_args );

                std::cout << "RERERERERERE" << std::endl;
                ret->dump();

                if ( f_env->is_initializer() ) {
                    // ret call will be ctor call, so return a reciever value
                    return val;

                } else {
                    if ( returns_heavy_object ) {
                        return total_args[0];
                    } else {
                        return ret;
                    }
                }

            } else {
                // normal call
                // invocation
                auto const ret
                    = context_->ir_builder.CreateCall( callee_function, total_args );

                if ( returns_heavy_object ) {
                    return total_args[0];
                } else {
                    return ret;
                }
            }
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::id_expression, e, parent_env )
        {
            return dispatch( e->expression_, parent_env );
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::term_expression, e, parent_env )
        {
            return dispatch( e->value_, parent_env );
        }

        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::evaluated_type_expression, e, parent_env )
        {
            // return id of type!
            llvm::Value* type_id_ptr
                = llvm::ConstantInt::get(
                    context_->llvm_context,
                    llvm::APInt( sizeof( e->type_id ), e->type_id )
                    );

            return type_id_ptr;
        }

        // identifier node returns Variable
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::identifier_value, v, parent_env )
        {
            //
            std::cout << "ir sym solving: "
                      << v->get_inner_symbol()->to_native_string() << std::endl
                      << "ast ptr: " << v.get() << std::endl
                      << (const_environment_base_ptr)parent_env << std::endl;

            //
            auto const& id_env = g_env_->get_related_env_by_ast_ptr( v );
            if ( id_env == nullptr ) {
                std::cout << "skipped" << std::endl;
                return nullptr;
            }


            switch( id_env->get_symbol_kind() )
            {
            case kind::type_value::e_variable:
            {
                std::cout << "llvm_ir_generator -> case Variable!" << std::endl;
                auto const& v_env
                    = std::static_pointer_cast<variable_symbol_environment const>( id_env );
                assert( v_env != nullptr );



                // TODO: check the type of variable !
                // if type is "type", ...(should return id of type...?)

                // reference the holder of variable...
                if ( context_->env_conversion_table.is_defined( v_env->get_id() ) ) {
                    return context_->env_conversion_table.ref_value( v_env->get_id() );

                } else {
#if 0
                    // fallback...
                    if ( is_jit() ) {
                        if ( analyzer_->ctfe_engine_->value_holder()->is_defined( v_env->get_id() ) ) {
                            auto const& d_val
                                = analyzer_->ctfe_engine_->value_holder()->ref_value(
                                    v_env->get_id()
                                    );
                            assert( d_val.is_type() );
                            return static_cast<llvm::Value*>( d_val.element );

                        } else {
                            assert( false && "[[ice]] llvm-jit -> value was not found..." );
                        }
                    } else {
                        assert( false && "[[ice]] llvm -> value was not found..." );
                    }
#endif
                    assert( false && "[[ice]]" );
                    return nullptr;
                }
            }

            default:
                std::cout << "skipped" << std::endl;
                return nullptr;
            }
        }







        // identifier node returns Variable
        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::template_instance_value, v, parent_env )
        {
            //
            std::cout << "ir sym solving: "
                      << v->get_inner_symbol()->to_native_string() << std::endl
                      << "ast ptr: " << v.get() << std::endl
                      << (const_environment_base_ptr)parent_env << std::endl;

            //
            //
            auto const& id_env = g_env_->get_related_env_by_ast_ptr( v );
            if ( id_env == nullptr ) {
                std::cout << "skipped" << std::endl;
                return nullptr;
            }


            switch( id_env->get_symbol_kind() )
            {
            case kind::type_value::e_variable:
            {
                std::cout << "llvm_ir_generator -> case Variable!" << std::endl;
                auto const& v_env
                    = std::static_pointer_cast<variable_symbol_environment const>( id_env );
                assert( v_env != nullptr );

                // TODO: check the type of variable !
                // if type is "type", ...(should return id of type...?)

                // reference the holder of variable...
                if ( context_->env_conversion_table.is_defined( v_env->get_id() ) ) {
                    return context_->env_conversion_table.ref_value( v_env->get_id() );

                } else {
#if 0
                    // fallback...
                    if ( is_jit() ) {
                        if ( analyzer_->ctfe_engine_->value_holder()->is_defined( v_env->get_id() ) ) {
                            auto const& d_val
                                = analyzer_->ctfe_engine_->value_holder()->ref_value(
                                    v_env->get_id()
                                    );
                            assert( d_val.is_type() );
                            return static_cast<llvm::Value*>( d_val.element );

                        } else {
                            assert( false && "[[ice]] llvm-jit -> value was not found..." );
                        }
                    } else {
                        assert( false && "[[ice]] llvm -> value was not found..." );
                    }
#endif
                    assert( false && "[[ice]]" );
                    return nullptr;
                }
            }

            default:
                std::cout << "skipped " << debug_string( id_env->get_symbol_kind() ) << std::endl;
                return nullptr;
            }
        }


        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::intrinsic::int32_value, v, parent_env )
        {
            // Currently, return int type( 32bit, integer )
            return llvm::ConstantInt::get( context_->llvm_context, llvm::APInt( 32, v->get_value() ) );
        }

        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::intrinsic::float_value, v, parent_env )
        {
            // Currently, return int type( float )
            return llvm::ConstantFP::get( context_->llvm_context, llvm::APFloat( v->get_value() ) );
        }

        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::intrinsic::boolean_value, v, parent_env )
        {
            return llvm::ConstantInt::get( context_->llvm_context, llvm::APInt( 1, v->get_value() ) );
        }

        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::intrinsic::string_value, v, parent_env )
        {
            return context_->ir_builder.CreateGlobalStringPtr( v->get_value().c_str() );
        }

        RILL_VISITOR_READONLY_OP( llvm_ir_generator, ast::intrinsic::array_value, v, parent_env )
        {
            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    g_env_->get_related_env_by_ast_ptr( v )
                    );
            assert( c_env != nullptr );
            assert( c_env->is_array() );
            auto const& array_detail = c_env->get_array_detail();

            llvm::Type* const inner_type
                = type_id_to_llvm_type_ptr( array_detail->inner_type_id );

            std::cout << "NUM: " << array_detail->elements_num << std::endl;

            auto const& array_ty = llvm::ArrayType::get(
                inner_type,
                array_detail->elements_num
                );

            llvm::AllocaInst* const alloca_inst
                = context_->ir_builder.CreateAlloca(
                    array_ty,
                    0/*length of array*/
                    );

            for( std::size_t i=0; i<array_detail->elements_num; ++i ) {
                llvm::Value* elem_v
                    = context_->ir_builder.CreateConstInBoundsGEP2_64(
                        alloca_inst,
                        0,
                        i
                        );

                auto const& e = v->elements_list_[i];
                llvm::Value* inner = dispatch( e, parent_env );

                context_->ir_builder.CreateStore(
                    inner,
                    elem_v /*, is_volatile */
                    );
            }

            return alloca_inst;
        }


        // store and bind suitable value to holder
        auto llvm_ir_generator::store_value(
            llvm::Value* const value,
            const_variable_symbol_environment_ptr const& v_env
            )
            -> void
        {
            auto const& v_type = g_env_->get_type_at( v_env->get_type_id() );
            if ( !context_->env_conversion_table.is_defined( v_type.class_env_id ) ) {
                auto const& c_env = g_env_->get_env_at_as_strong_ref( v_type.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }
            auto const& variable_attr = v_type.attributes;

            auto const& variable_llvm_type
                = context_->env_conversion_table.ref_type( v_type.class_env_id );

            auto const& c_env
                = cast_to<class_symbol_environment const>(
                    g_env_->get_env_at_as_strong_ref( v_type.class_env_id )
                    );

            std::cout << "Store value" << std::endl;
            value->dump();

            if ( is_heavy_object( v_type ) ) {
                //
                context_->env_conversion_table.bind_value(
                    v_env->get_id(),
                    value
                    );

            } else {
                //

                switch( variable_attr.quality )
                {
                case attribute::holder_kind::k_val:
                {
                    switch( variable_attr.modifiability )
                    {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_const:
                    {
                        assert( value != nullptr );
                        context_->env_conversion_table.bind_value(
                            v_env->get_id(),
                            value
                            );

                        break;
                    }

                    case attribute::modifiability_kind::k_mutable:
                    {
                        // FIXME:
                        llvm::AllocaInst* const allca_inst
                            = context_->ir_builder.CreateAlloca(
                                variable_llvm_type,
                                0/*length*/
                                );
                        if ( value ) {
                            context_->ir_builder.CreateStore(
                                value,
                                allca_inst /*, is_volatile */
                                );
                        }

                        context_->env_conversion_table.bind_value(
                            v_env->get_id(),
                            allca_inst
                            );

                        break;
                    }
                    } // switch

                    break;
                }

                case attribute::holder_kind::k_ref:
                {
                    context_->env_conversion_table.bind_value(
                            v_env->get_id(),
                            value
                            );
                    break;
                }

                default:
                {
                    assert( false && "[ice]" );
                    break;
                }
                } // switch
            }
        }


        auto llvm_ir_generator::define_intrinsic_function_frame(
            const_function_symbol_environment_ptr const& f_env
            )
            -> void
        {
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            // ========================================
            // information about paramaters
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            std::cout << "()()=> :" << f_env->get_base_name() << std::endl;
            // assert(false);

            // ========================================
            //
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END


            // ========================================
            auto const linkage = llvm::Function::InternalLinkage;


            // ========================================
            auto const& ret_type_id = f_env->get_return_type_id();
            auto const& ret_type
                = g_env_->get_type_at( f_env->get_return_type_id() );
            bool const returns_heavy_object = is_heavy_object( ret_type );


            // ========================================
            // signature
            std::vector<llvm::Type*> parameter_types;
            if ( returns_heavy_object ) {
                parameter_types.push_back(
                    type_id_to_llvm_type_ptr( ret_type_id )
                    );
            }
            boost::copy(
                parameter_variable_type_ids | RILL_TID_TO_LLVM_TYPE_TRANSFORMAER,
                std::back_inserter( parameter_types )
                );


            // ========================================
            // return type
            llvm::Type* const return_type
                = returns_heavy_object
                ? llvm::Type::getVoidTy( context_->llvm_context )
                : type_id_to_llvm_type_ptr( ret_type_id );

            // ========================================
            // function type signature
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // ========================================
            // function body
            llvm::Function* const func = llvm::Function::Create( func_type, linkage, f_env->get_mangled_name(), context_->llvm_module.get() );

            // ========================================
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_block
                = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( basic_block );

            // ========================================
            std::size_t i = 0;
            std::vector<llvm::Value*> args;
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;

                if ( ait == func->arg_begin() && returns_heavy_object ) {
                    ait->setName( "__ret_target" );

                } else {
                    auto const& var =
                        g_env_->get_env_at_as_strong_ref<variable_symbol_environment const>( parameter_variable_decl_env_ids[i] );
                    ait->setName( var->get_mangled_name() );

                    store_value( ait, var );
                    ++i;
                }

                args.push_back( ait );
            }

            // ========================================
            // generate statements
            // look up the action
            auto const& action = action_holder_->at( f_env->get_action_id() );
            assert( action != nullptr );

            // generate codes into this context
            auto const value = action->invoke(
                processing_context::k_llvm_ir_generator,
                context_,
                f_env,
                args
                );
            assert( value != nullptr );

            context_->ir_builder.CreateRet( value );

            std::cout << "created!" << std::endl;
            func->dump();

            //
            llvm::verifyFunction( *func );
        }


        auto llvm_ir_generator::regard_env_is_defined( environment_id_t const& env_id )
            -> void
        {
            if ( !context_->env_conversion_table.is_defined( env_id ) ) {
                auto const& env = g_env_->get_env_at_as_strong_ref( env_id );
                dispatch( env->get_related_ast(), env );
            }
        }

    } // namespace code_generator
} // namespace rill

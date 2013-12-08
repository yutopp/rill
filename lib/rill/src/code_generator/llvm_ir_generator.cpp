//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/behavior/intrinsic_function_holder.hpp>
#include <rill/environment/environment.hpp>

#include <boost/scope_exit.hpp>
#include <boost/range/adaptor/reversed.hpp>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Analysis/Verifier.h>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace code_generator
    {
        // = definition ===
        // class llvm_ir_generator
        // ================

        llvm_ir_generator::llvm_ir_generator(
            const_environment_base_ptr const& root_env,
            intrinsic_function_action_holder_ptr const& action_holder,
            llvm_ir_generator_context_ptr const& context
            )
            : root_env_( root_env )
            , action_holder_( action_holder )
            , context_( context )
        {}


        //
        // Root Scope
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::root, r, root_env )
        {
            //
            for( auto const& node : r->statements_ )
                dispatch( node );
        }

      
  
        //
        // Expression Statement
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::expression_statement, s, env )
        {
            dispatch( s->expression_, root_env_->get_related_env_by_ast_ptr( s->expression_ ) );
        }



        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::return_statement, s, env )
        {
            context_->ir_builder.CreateRet( dispatch( s->expression_, env ) );
        }



        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::function_definition_statement, s, self_env )
        {
            //
            std::cout << "!!!!!!ast::function_definition_statement" << self_env << " / " << root_env_->get_id() << std::endl;

            //
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( ( self_env != nullptr ) ? self_env : root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            // information about paramaters
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            std::cout << "()()=> :" << f_env->mangled_name() << std::endl;

            //
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END

            //
            // TODO: use current_insert_point.isSet(), if it is false, this function needs external linkage( currently, all functions are exported as external linkage )
            auto const linkage = llvm::Function::ExternalLinkage;
            if ( current_insert_point.isSet() ) {
                //
                std::cout << "not external." << std::endl;
            }

            // signature
            std::vector<llvm::Type*> parameter_types;
//            for( auto const& type_env_id : parameter_variable_type_env_ids ) {
            for( auto const& decl_type_id : parameter_variable_type_ids ) {
/*
                auto const& pv_decl_env = std::static_pointer_cast<variable_symbol_environment const>( self_env->get_env_strong_at( decl_env_id ) );
                assert( pv_decl_env != nullptr );

                auto const& type_env_id = pv_decl_env->get_type_environment_id();
                auto const& type_attr = pv_decl_env->get_type_attributes();
*/
                auto const& v = f_env->get_type_at( decl_type_id );
                if ( !context_->env_conversion_table.is_defined( v.class_env_id ) ) {
                    auto const& c_env = root_env_->get_env_strong_at( v.class_env_id );
                    dispatch( c_env->get_related_ast(), c_env );
                }

                auto const& class_env_id = v.class_env_id;
                auto const& type_attr = v.attributes;

                std::cout << class_env_id << std::endl;
                parameter_types.push_back( context_->env_conversion_table.ref_type( class_env_id ) );
            }
            auto const& v = f_env->get_type_at( f_env->get_return_type_id() );
            llvm::Type* const return_type = context_->env_conversion_table.ref_type( v.class_env_id );
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            

            // function body
            llvm::Function* const func = llvm::Function::Create( func_type, linkage, f_env->mangled_name(), &context_->llvm_module );
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;
                auto const& var = std::static_pointer_cast<variable_symbol_environment const>( f_env->get_env_at( parameter_variable_decl_env_ids[ait->getArgNo()] ).lock() );
                ait->setName( var->mangled_name() );

                context_->env_conversion_table.bind_value( var->get_id(), ait );
            }

//            func->setGC( "shadow-stack" );

            //
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_brock = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( basic_brock );

            // generate statements
            for( auto const& node : s->statements_ )
                dispatch( node, root_env_->get_related_env_by_ast_ptr( node ) );

            // Only the function that returns void is allowed to has no return statement
            if ( f_env->get_return_type_candidates().size() == 0 )
                context_->ir_builder.CreateRetVoid();

            //
            llvm::verifyFunction( *func, llvm::PrintMessageAction );
        }




        RILL_TV_OP_CONST( llvm_ir_generator, ast::class_definition_statement, s, self_env )
        {
            // TODO: support structures contain self type. Ex, class T { ref T; val T; }

            //
            auto const& c_env = std::static_pointer_cast<class_symbol_environment const>( ( self_env != nullptr ) ? self_env : root_env_->get_related_env_by_ast_ptr( s ) );

            //
            context_->env_conversion_table.create_class_variable_type_holder(
                c_env->get_id()
                );

            // construct incomplete Struct Type...
            llvm::StructType* const llvm_struct_type = llvm::StructType::create( context_->llvm_context /* TODO: add "name", add is_packed*/ );
            context_->env_conversion_table.bind_type( c_env->get_id(), llvm_struct_type );

            // generate statements
            for( auto const& node : s->statements_ )
                dispatch( node, c_env );

            std::cout << "------> " << c_env->get_id() << std::endl;
            llvm_struct_type->setBody( context_->env_conversion_table.ref_class_variable_type_list( c_env->get_id() ) );
        }














        RILL_TV_OP_CONST( llvm_ir_generator, ast::intrinsic_function_definition_statement, s, env )
        {
            // cast to function symbol env
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( ( env != nullptr ) ? env : root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            auto const& parameter_variable_type_env_ids = f_env->get_parameter_type_ids();


            //
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END


            //
            // TODO: use current_insert_point.isSet(), if it is false, this function needs external linkage( currently, all functions are exported as external linkage )
            auto const linkage = llvm::Function::ExternalLinkage;
            if ( current_insert_point.isSet() ) {
                //
                std::cout << "not external." << std::endl;
            }

            // define paramter and return types
            std::vector<llvm::Type*> parmeter_types;
            for( auto const& type_env_id : parameter_variable_type_env_ids ) {
                auto const& v = f_env->get_type_at( type_env_id );
                if ( !context_->env_conversion_table.is_defined( v.class_env_id ) ) {
                    auto const& c_env = root_env_->get_env_strong_at( v.class_env_id );
                    dispatch( c_env->get_related_ast(), c_env );
                }

                auto const& llvm_type = [&]() -> llvm::Type* {
                    //
                    switch( v.attributes.quality )
                    {
                    case attribute::quality_kind::k_val:
                        // TODO: implement
                        return context_->env_conversion_table.ref_type( v.class_env_id );

                    case attribute::quality_kind::k_ref:
                        // TODO: implement
                        return context_->env_conversion_table.ref_type( v.class_env_id )->getPointerTo();

                    default:
                        assert( false && "[ice]" );
                        break;
                    }
                }();

                parmeter_types.push_back( llvm_type );
            }
            auto const& v = f_env->get_type_at( f_env->get_return_type_id() );
            auto const& return_type = [&]() -> llvm::Type* {
                //
                switch( v.attributes.quality )
                {
                case attribute::quality_kind::k_val:
                    // TODO: implement
                    return context_->env_conversion_table.ref_type( v.class_env_id );

                    case attribute::quality_kind::k_ref:
                        // TODO: implement
                        return context_->env_conversion_table.ref_type( v.class_env_id )->getPointerTo();

                    default:
                        assert( false && "[ice]" );
                        break;
                    }
                }();
            //context_->env_conversion_table.ref_type( v.class_env_id );

            // get function type
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parmeter_types, false/*is not variadic*/ );

            //
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // construct function and paramter variables
            llvm::Function* const func = llvm::Function::Create( func_type, llvm::Function::ExternalLinkage, f_env->mangled_name(), &context_->llvm_module );
            for( llvm::Function::arg_iterator ait=func->arg_begin(); ait!=func->arg_end(); ++ait ) {
                auto const& var = std::static_pointer_cast<variable_symbol_environment const>( f_env->get_env_at( parameter_variable_decl_env_ids[ait->getArgNo()] ).lock() );
                ait->setName( var->mangled_name() );

                context_->env_conversion_table.bind_value( var->get_id(), ait );
            }
            func->addFnAttr( llvm::Attribute::AlwaysInline );

            // set initial insert point to entry
            llvm::BasicBlock* const function_entry_block = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( function_entry_block );

            // build inner statements(that contain intrinsic_function_call)
            for( auto const& node : s->statements_ )
                dispatch( node, root_env_->get_related_env_by_ast_ptr( node ) );

            //
            llvm::verifyFunction( *func, llvm::PrintMessageAction );
        }


        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::variable_declaration_statement, s, env )
        {
            // TODO: all of variablea(mutable) should be allocated at head of function...
            // TODO: check kind...

            // cast to variable symbol env
            auto const& v_env
                = std::static_pointer_cast<variable_symbol_environment const>( ( env != nullptr ) ? env : root_env_->get_related_env_by_ast_ptr( s ) );
            assert( v_env != nullptr );

            

            // ???:
            auto const& variable_type = v_env->get_type_at( v_env->get_type_id() );
            if ( !context_->env_conversion_table.is_defined( variable_type.class_env_id ) ) {
                auto const& c_env = root_env_->get_env_strong_at( variable_type.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }

            auto const& variable_llvm_type = context_->env_conversion_table.ref_type( variable_type.class_env_id );
            auto const& variable_attr =  variable_type.attributes;

            // initial value
            // TODO: call constructor if there are no initial value
            auto const& initial_llvm_value
                = s->declaration_.decl_unit.init_unit.initializer
                ? dispatch( s->declaration_.decl_unit.init_unit.initializer )
                : (llvm::Value*)llvm::ConstantStruct::get( (llvm::StructType*)variable_llvm_type, llvm::ConstantInt::get( context_->llvm_context, llvm::APInt( 32, 42 ) ), nullptr );//nullptr;

            //
            switch( variable_attr.quality )
            {
            case attribute::quality_kind::k_val:

                switch( variable_attr.modifiability )
                {
                case attribute::modifiability_kind::k_immutable:
                {
                    if ( !initial_llvm_value ) {
                        assert( false && "[ice" );
                    }

                    context_->env_conversion_table.bind_value( v_env->get_id(), initial_llvm_value );      
                }
                    break;

                case attribute::modifiability_kind::k_const:
                    assert( false && "[ice]" );
                    break;

                case attribute::modifiability_kind::k_mutable:
                {
                    llvm::AllocaInst* const allca_inst = context_->ir_builder.CreateAlloca( variable_llvm_type, 0/*length*/ );
                    if ( initial_llvm_value ) {
                        context_->ir_builder.CreateStore( initial_llvm_value, allca_inst /*, is_volatile */ );
                    }

                    context_->env_conversion_table.bind_value( v_env->get_id(), allca_inst );
                }
                    break;
                }

                break;

            case attribute::quality_kind::k_ref:
                assert( false && "not implemented..." );
                break;

            default:
                assert( false && "[ice]" );
                break;
            }
        }




        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::class_variable_declaration_statement, s, parent_env )
        {
            assert( parent_env != nullptr );
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // cast to variable symbol env
            auto const& v_env
                = std::static_pointer_cast<variable_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( s ) );
            assert( v_env != nullptr );

            

            // ???:
            auto const& variable_type = v_env->get_type_at( v_env->get_type_id() );
            if ( !context_->env_conversion_table.is_defined( variable_type.class_env_id ) ) {
                auto const& c_env = root_env_->get_env_strong_at( variable_type.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }

            auto const& variable_llvm_type = context_->env_conversion_table.ref_type( variable_type.class_env_id );
            auto const& variable_attr =  variable_type.attributes;

            //
            switch( variable_attr.quality )
            {
            case attribute::quality_kind::k_val:

                switch( variable_attr.modifiability )
                {
                case attribute::modifiability_kind::k_immutable:
                {
                    std::cout << "ABABABAB: " << parent_env->get_id() << std::endl;
                    context_->env_conversion_table.bind_class_variable_type( parent_env->get_id(), v_env->get_id(), variable_llvm_type );      
                }
                    break;

                case attribute::modifiability_kind::k_const:
                    assert( false && "[ice]" );
                    break;

                case attribute::modifiability_kind::k_mutable:
                {
                    context_->env_conversion_table.bind_class_variable_type( parent_env->get_id(), v_env->get_id(), variable_llvm_type->getPointerTo() );      
                }
                    break;
                }

                break;

            case attribute::quality_kind::k_ref:
                assert( false && "not implemented..." );
                break;

            default:
                assert( false && "[ice]" );
                break;
            }
        }




        RILL_TV_OP_CONST( llvm_ir_generator, ast::extern_function_declaration_statement, s, env )
        {
            // cast to function symbol env
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( ( env != nullptr ) ? env : root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();


            //
            auto const current_insert_point = context_->ir_builder.saveIP();
            BOOST_SCOPE_EXIT((&context_)(&current_insert_point)) {
                // restore insert point
                context_->ir_builder.restoreIP( current_insert_point );
            } BOOST_SCOPE_EXIT_END



            // define paramter and return types
            std::vector<llvm::Type*> parmeter_types;
            for( auto const& var_type_id : parameter_variable_type_ids ) {
                auto const& v = f_env->get_type_at( var_type_id );
                parmeter_types.push_back( context_->env_conversion_table.ref_type( v.class_env_id ) );
            }
            auto const& v = f_env->get_type_at( f_env->get_return_type_id() );
            if ( !context_->env_conversion_table.is_defined( v.class_env_id ) ) {
                auto const& c_env = root_env_->get_env_strong_at( v.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }
            auto const& return_type = context_->env_conversion_table.ref_type( v.class_env_id );

            // get function type
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parmeter_types, false/*is not variadic*/ );

            //
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );
        }



        RILL_TV_OP_CONST( llvm_ir_generator, ast::test_while_statement, s, _ )
        {                  
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const while_begin_block = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );
            context_->ir_builder.CreateBr( while_begin_block );

            // create a new basic block to start insertion into.
            llvm::BasicBlock* const true_block = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const false_block = llvm::BasicBlock::Create( context_->llvm_context, "", context_->ir_builder.GetInsertBlock()->getParent() );

            //
            context_->ir_builder.SetInsertPoint( while_begin_block );
            auto const& cond_llvm_value = dispatch( s->conditional_, _ );
            context_->ir_builder.CreateCondBr( cond_llvm_value, true_block, false_block );

            context_->ir_builder.SetInsertPoint( true_block );
            // build inner statements(that contain intrinsic_function_call)
            for( auto const& node : s->statements_ )
                dispatch( node, _ );

            context_->ir_builder.CreateBr( while_begin_block );

            context_->ir_builder.SetInsertPoint( false_block );
        }



        RILL_TV_OP_CONST( llvm_ir_generator, ast::binary_operator_expression, e, _ )
        {
            // Look up Function
            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );
            auto const& target_name = f_env->mangled_name();
            auto const& callee_function = [&]() -> llvm::Function* const {
                if ( auto const f = context_->llvm_module.getFunction( target_name ) )
                    return f;

                // generate
                dispatch( f_env->get_related_ast(), f_env );
                if ( auto const f = context_->llvm_module.getFunction( target_name ) )
                    return f;

                return nullptr;
            }();
            if ( !callee_function ) {
                // unexpected error...
                assert( false );
            }

/*
            // evaluate values(and push to stack) and returned value type
            // evalute rhs -> lhs
            auto const& rhs_value = dispatch( e->rhs_, _ );
            auto const& lhs_value = dispatch( e->lhs_, _ );
            assert( rhs_value != nullptr && lhs_value != nullptr );



            std::vector<llvm::Value*> const args = { lhs_value, rhs_value };
*/           
            // call function that defined in rill modules
            // evaluate argument from last to front(but ordering of vector is from front to last)
            ast::expression_list const& e_arguments = { e->lhs_, e->rhs_ };
            auto const& parameter_type_ids = f_env->get_parameter_type_ids();
            std::vector<llvm::Value*> args( e_arguments.size() );

            for( std::size_t i=0; i<e_arguments.size(); ++i ) {
                auto const& type = f_env->get_type_at( parameter_type_ids[e_arguments.size()-i-1] );
                if ( !context_->env_conversion_table.is_defined( type.class_env_id ) ) {
                    auto const& c_env = root_env_->get_env_strong_at( type.class_env_id );
                    dispatch( c_env->get_related_ast(), c_env );
                }

                auto const value = dispatch( e_arguments[e_arguments.size()-i-1], _ );
                assert( value != nullptr );

                // TODO: check more strict
                auto const& result_value = [&]() -> llvm::Value* {
                    switch( type.attributes.quality )
                    {
                    case attribute::quality_kind::k_val:
                    {
                        // TODO: implement
                        if ( value->getType()->isPointerTy() ) {
                            return context_->ir_builder.CreateLoad( value );
                        } else {
                            return value;
                        }
                    }

                    case attribute::quality_kind::k_ref:
                        // TODO: implement
                        if ( value->getType()->isPointerTy() ) {
                            return value;
                        } else {
                            assert( false && "[ice]" );
                            return value;
                        }
                        return value;

                    default:
                        assert( false && "[ice]" );
                        break;                  
                    }
                }();

                // TODO: value conversion...(ref, val, etc...)
                args[e_arguments.size()-i-1] = result_value;
            }



            std::cout << "CALL!!!!!" << std::endl;

            // invocation
            return context_->ir_builder.CreateCall( callee_function, args, "calltmp" );
        }



        RILL_TV_OP_CONST( llvm_ir_generator, ast::call_expression, e, _ )
        {
            // Look up Function
            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );

            //
            if ( !context_->env_conversion_table.is_defined( f_env->get_id() ) ) {
                // 
                std::cout << "!context_->env_conversion_table.is_defined( f_env->get_id() ): " << f_env->mangled_name() << std::endl;
                dispatch( f_env->get_related_ast(), f_env );
            }


            std::cout << "current : " << f_env->mangled_name() << std::endl;
            auto const& callee_function
                = ( f_env->has_attribute( function_symbol_environment::attr::e_extern ) )
                ? [&, this]() -> llvm::Constant* const {
                    llvm::FunctionType* const func_type = context_->env_conversion_table.ref_function_type( f_env->get_id() );
                    auto const& s = f_env->get_related_ast();
                    assert( s != nullptr );
                    return context_->llvm_module.getOrInsertFunction( std::static_pointer_cast<ast::extern_function_declaration_statement const>( s )->get_extern_symbol_name(), func_type );
                }()
                : [&, this]() -> llvm::Constant* const {
                    auto const& target_name = f_env->mangled_name();
                    std::cout << "SS: " << target_name << std::endl;
                    if ( auto const f = context_->llvm_module.getFunction( target_name ) )
                        return f;

                    return nullptr;
                }()
                ;
            if ( !callee_function ) {
                // unexpected error...
                assert( false && "unexpected... callee_function was not found" );
            }


            // call function that defined in rill modules
            // evaluate argument from last to front(but ordering of vector is from front to last)
            auto const& parameter_type_ids = f_env->get_parameter_type_ids();
            std::vector<llvm::Value*> args( e->arguments_.size() );
            for( std::size_t i=0; i<e->arguments_.size(); ++i ) {
                auto const& type = f_env->get_type_at( parameter_type_ids[e->arguments_.size()-i-1] );                
                if ( !context_->env_conversion_table.is_defined( type.class_env_id ) ) {
                    auto const& c_env = root_env_->get_env_strong_at( type.class_env_id );
                    dispatch( c_env->get_related_ast(), c_env );
                }
                auto const value = dispatch( e->arguments_[e->arguments_.size()-i-1], _ );
                assert( value != nullptr );

                // TODO: check more strict
                auto const& result_value = [&]() -> llvm::Value* {
                    switch( type.attributes.quality )
                    {
                    case attribute::quality_kind::k_val:
                    {
                        // TODO: implement
                        if ( value->getType()->isPointerTy() ) {
                            return context_->ir_builder.CreateLoad( value );
                        } else {
                            return value;
                        }
                    }

                    case attribute::quality_kind::k_ref:
                        // TODO: implement
                        if ( value->getType()->isPointerTy() ) {
                            return value;
                        } else {
                            assert( false && "[ice]" );
                            return value;
                        }
                        return value;

                    default:
                        assert( false && "[ice]" );
                        break;                  
                    }
                }();

                // TODO: value conversion...(ref, val, etc...)
                args[e->arguments_.size()-i-1] = result_value;
            }

            // invocation
            return context_->ir_builder.CreateCall( callee_function, args/*, "calltmp"*/ );
        }


        // TODO: change name to native code injection expression
        RILL_TV_OP_CONST( llvm_ir_generator, ast::intrinsic_function_call_expression, e, _ )
        {
            // look up the function
            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );

            // look up the action
            auto const& action = action_holder_->at( e->action_id_ );
            assert( action != nullptr );

            // generate codes into this context

            auto const& v = f_env->get_type_at( f_env->get_return_type_id() );
            if ( !context_->env_conversion_table.is_defined( v.class_env_id ) ) {
                auto const& c_env = root_env_->get_env_strong_at( v.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }

            auto const value = action->invoke(
                processing_context::k_llvm_ir_generator,
                context_,
                f_env,
                f_env->get_parameter_decl_ids()
                );
            //assert( value != nullptr );

            return value;
        }


        RILL_TV_OP_CONST( llvm_ir_generator, ast::term_expression, e, _ )
        {
            return dispatch( e->value_, _ );
        }

        RILL_TV_OP_CONST( llvm_ir_generator, ast::intrinsic_value, v, _ )
        {
            // TODO: check primitive type
            if ( v->literal_type_name_->get_inner_symbol()->to_native_string() == "int" ) {
                // Currently, return int type( 32bit, integer )
                return llvm::ConstantInt::get( context_->llvm_context, llvm::APInt( 32, std::static_pointer_cast<ast::intrinsic::int32_value const>( v->value_ )->get_value() ) );

            } else if ( v->literal_type_name_->get_inner_symbol()->to_native_string() == "string" ) {
                // char pointer...(string?)
                return context_->ir_builder.CreateGlobalStringPtr( std::static_pointer_cast<ast::intrinsic::string_value const>( v->value_ )->value_.c_str() );

            } else {
                assert( false );
                return nullptr;
            }
        }

        RILL_TV_OP_CONST( llvm_ir_generator, ast::variable_value, v, _ )
        {
            auto const v_env = std::static_pointer_cast<variable_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( v ) );
            assert( v_env != nullptr );

            return context_->env_conversion_table.ref_value( v_env->get_id() );
        }

    } // namespace code_generator
} // namespace rill

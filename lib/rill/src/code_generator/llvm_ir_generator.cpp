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

#include <iterator>

#include <boost/scope_exit.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/join.hpp>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Analysis/Verifier.h>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace code_generator
    {
        // ========================================
        //
        // ========================================
        class type_id_to_llvm_type_ptr
        {
        public:
            typedef llvm::Type*    result_type;

        public:
            type_id_to_llvm_type_ptr(
                std::reference_wrapper<llvm_ir_generator const> const& gen
                )
                : gen_( gen )
            {}

        public:
            auto operator()( type_id_t const& type_id ) const
                -> result_type
            {
                auto const& generator = gen_.get();

                auto const& t = generator.root_env_->get_type_at( type_id );
                if ( !generator.context_->env_conversion_table.is_defined( t.class_env_id ) ) {
                    auto const& c_env = generator.root_env_->get_env_strong_at( t.class_env_id );
                    generator.dispatch( c_env->get_related_ast(), c_env );
                }
                auto const& type_class_env_id = t.class_env_id;
                auto const& type_attr = t.attributes;

                std::cout << "class env id: " << type_class_env_id << std::endl;
                llvm::Type* ty
                    = generator.context_->env_conversion_table.ref_type( type_class_env_id );

                return [&]() -> result_type
                {
                    if ( ty->isStructTy() ) {
                        return ty->getPointerTo();

                    } else {
                        switch( type_attr.quality )
                        {
                        case attribute::quality_kind::k_val:
                            // TODO: implement
                            return ty;

                        case attribute::quality_kind::k_ref:
                            // TODO: implement
                            return ty->getPointerTo();

                        default:
                            assert( false && "[ice]" );
                            break;
                        }
                    }
                }();
            }

        private:
            std::reference_wrapper<llvm_ir_generator const> gen_;
        };





        // ========================================
        //
        // ========================================
        class function_env_to_llvm_constatnt_ptr
        {
        public:
            typedef llvm::Constant*     result_type;

        public:
            function_env_to_llvm_constatnt_ptr(
                std::reference_wrapper<llvm_ir_generator const> const& gen
                )
                : gen_( gen )
            {}

        public:
            auto operator()( const_function_symbol_environment_ptr const& f_env ) const
                -> result_type
            {
                auto const& generator = gen_.get();

                // if function was not generated, generate function IR
                if ( !generator.context_->env_conversion_table.is_defined( f_env->get_id() ) ) {
                    //
                    std::cout
                        << "!context_->env_conversion_table.is_defined( f_env->get_id() ): "
                        << f_env->mangled_name()
                        << std::endl;

                    generator.dispatch( f_env->get_related_ast(), f_env );
                }

                return [&]() -> llvm::Constant*
                {
                    if ( f_env->has_attribute( function_symbol_environment::attr::e_extern ) ) {
                        // external function
                        llvm::FunctionType* const func_type
                            = generator.context_->env_conversion_table.ref_function_type( f_env->get_id() );

                        auto const& s
                            = std::static_pointer_cast<ast::extern_function_declaration_statement const>( f_env->get_related_ast() );
                        assert( s != nullptr );
                        return generator.context_->llvm_module.getOrInsertFunction( s->get_extern_symbol_name(), func_type );

                    } else {
                        // nornal function
                        auto const& target_name
                            = f_env->mangled_name();
                        std::cout << "SS: " << target_name << std::endl;

                        if ( auto const f = generator.context_->llvm_module.getFunction( target_name ) )
                            return f;

                        return nullptr;
                    }

                }();
            }

        private:
            std::reference_wrapper<llvm_ir_generator const> gen_;
        };







        // ========================================
        //
        // ========================================
        template<typename MatchNode, typename F>
        class node_filter RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<node_filter<MatchNode, F>, llvm::Value*>
        {
        public:
                template<typename NodeT>
                struct result
                {
                    typedef typename ast::detail::tree_visitor_result<
                        llvm::Value*,
                        typename ast::detail::base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

        public:
            node_filter( F const& f )
                : f_( f )
            {}

        public:
            RILL_TV_OP_INDIRECT_CONST( ast::statements, s, _ )
            {
                for( auto const& ss : s->statement_list_ )
                    this->dispatch( ss, _ );
            }

            RILL_TV_OP_INDIRECT_CONST( ast::block_statement, s, _ )
            {
                //
                this->dispatch( s->statements_, _ );
            }

            RILL_TV_OP_INDIRECT_CONST( MatchNode, node, _ )
            {
                //
                return f_( node, _ );
            }

            RILL_TV_OP_FAIL

            template<typename NodeT>
            auto failed_to_dispatch() const
                -> void
            {}

        private:
            F const& f_;
        };

        template<typename MatchNode, typename Node, typename Env, typename F>
        auto do_filter( Node const& node, Env const& env, F const& f )
            -> decltype( node_filter<MatchNode, F>( f ).dispatch( node, env ) )
        {
            node_filter<MatchNode, F> filter( f );
            return filter.dispatch( node, env );
        }

        template<typename MatchNode, typename Node, typename Env, typename F>
        auto do_filter_const( Node const& node, Env const& env, F const& f )
            -> decltype( node_filter<MatchNode, F>( f ).dispatch( node, env ) )
        {
            node_filter<MatchNode, F> const filter( f );
            return filter.dispatch( node, env );
        }





        // ========================================
        // ========================================



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
        RILL_TV_OP_CONST( llvm_ir_generator, ast::statements, s, parent_env )
        {
            //
            for( auto const& ss : s->statement_list_ )
                dispatch( ss, parent_env );
        }


        RILL_TV_OP_CONST( llvm_ir_generator, ast::block_statement, s, parent_env )
        {
            dispatch( s->statements_, parent_env );
        }


        //
        // Expression Statement
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::expression_statement, s, parent_env )
        {
            dispatch( s->expression_, parent_env );
        }



        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::return_statement, s, parent_env )
        {
            context_->ir_builder.CreateRet( dispatch( s->expression_, parent_env ) );
        }



        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::function_definition_statement, s, parent_env )
        {
            //
            //std::cout << "!!!!!!ast::function_definition_statem" << self_env << " / " << root_env_->get_id() << std::endl;

            //
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            // information about paramaters
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
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
            boost::copy(
                parameter_variable_type_ids | boost::adaptors::transformed( type_id_to_llvm_type_ptr( std::cref( *this ) ) ),
                std::back_inserter( parameter_types )
                );


            // ========================================
            // return type
            llvm::Type* const return_type
                = type_id_to_llvm_type_ptr( std::cref( *this ) )( f_env->get_return_type_id() );

            // ========================================
            // function type signature
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // ========================================
            // function body
            llvm::Function* const func = llvm::Function::Create( func_type, linkage, f_env->mangled_name(), &context_->llvm_module );

            // ========================================
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;
                auto const& var = std::static_pointer_cast<variable_symbol_environment const>( f_env->get_env_at( parameter_variable_decl_env_ids[ait->getArgNo()] ).lock() );
                ait->setName( var->mangled_name() );

                context_->env_conversion_table.bind_value( var->get_id(), ait );
            }

//            func->setGC( "shadow-stack" );

            // ========================================
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_brock
                = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( basic_brock );

            // generate statements
            dispatch( s->inner_, f_env );


            // Only the function that returns void is allowed to has no return statement
            if ( f_env->get_return_type_candidates().size() == 0 )
                context_->ir_builder.CreateRetVoid();

            //
            llvm::verifyFunction( *func, llvm::PrintMessageAction );
        }




        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::class_function_definition_statement, s, parent_env )
        {
            // ========================================
            // get current function environment
            auto const& f_env
                = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;


            // ========================================
            // information about paramaters
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            std::cout << "()()=> :" << f_env->mangled_name() << std::endl;


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
            // parameter signature
            std::vector<llvm::Type*> parameter_types;
            boost::copy(
                parameter_variable_type_ids | boost::adaptors::transformed( type_id_to_llvm_type_ptr( std::cref( *this ) ) ),
                std::back_inserter( parameter_types )
                );

            // ========================================
            // return type
            llvm::Type* const return_type
                = type_id_to_llvm_type_ptr( std::cref( *this ) )( f_env->get_return_type_id() );

            // ========================================
            // function type signature
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            context_->env_conversion_table.bind_function_type( f_env->get_id(), func_type );

            // ========================================
            // function body
            llvm::Function* const func
                = llvm::Function::Create( func_type, linkage, f_env->mangled_name(), &context_->llvm_module );

            // ========================================
            // function parameter variables
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;
                auto const& v_env
                    = std::static_pointer_cast<variable_symbol_environment const>(
                        f_env->get_env_strong_at( parameter_variable_decl_env_ids[ait->getArgNo()] )
                        );
                ait->setName( v_env->mangled_name() );

                context_->env_conversion_table.bind_value( v_env->get_id(), ait );
            }

//            func->setGC( "shadow-stack" );

            // ========================================
            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_brock
                = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            context_->ir_builder.SetInsertPoint( basic_brock );

            // ========================================
            // generate statements
            dispatch( s->inner_, f_env );

            // ========================================
            // Only the function that returns void is allowed to has no return statement
            if ( f_env->get_return_type_candidates().size() == 0 )
                context_->ir_builder.CreateRetVoid();

            //
            llvm::verifyFunction( *func, llvm::PrintMessageAction );
        }





        RILL_TV_OP_CONST( llvm_ir_generator, ast::class_definition_statement, s, parent_env )
        {
            // TODO: support structures contain self type. Ex, class T { ref T; val T; }

            //
            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( s ) );

            //
            context_->env_conversion_table.create_class_variable_type_holder(
                c_env->get_id()
                );

            // construct incomplete Struct Type...
            llvm::StructType* const llvm_struct_type
                = llvm::StructType::create(
                    context_->llvm_context,
                    c_env->mangled_name()/*, add is_packed*/
                    );
            context_->env_conversion_table.bind_type( c_env->get_id(), llvm_struct_type );

            // generate statements
            do_filter_const<ast::class_variable_declaration_statement>(
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


            dispatch( s->inner_, c_env );
        }














        RILL_TV_OP_CONST( llvm_ir_generator, ast::intrinsic_function_definition_statement, s, parent_env )
        {
            // cast to function symbol env
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            if ( context_->env_conversion_table.is_defined( f_env->get_id() ) )
                return;

            // ========================================
            // information about paramaters
            auto const& parameter_variable_type_ids = f_env->get_parameter_type_ids();
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
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

            // define paramter and return types
            std::vector<llvm::Type*> parameter_types;
            boost::copy(
                parameter_variable_type_ids | boost::adaptors::transformed( type_id_to_llvm_type_ptr( std::cref( *this ) ) ),
                std::back_inserter( parameter_types )
                );





            auto const& return_type
                = type_id_to_llvm_type_ptr( std::cref( *this ) )( f_env->get_return_type_id() );

            //context_->env_conversion_table.ref_type( v.class_env_id );

            // get function type
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*is not variadic*/ );

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
            dispatch( s->inner_, f_env );

            //
            llvm::verifyFunction( *func, llvm::PrintMessageAction );
        }


        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::variable_declaration_statement, s, parent_env )
        {
            // TODO: all of variablea(mutable) should be allocated at head of function...
            // TODO: check kind...

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

            auto const& variable_llvm_type
                = context_->env_conversion_table.ref_type( variable_type.class_env_id );
            auto const& variable_attr
                = variable_type.attributes;

            // initial value
            // !!!!!!TODO: call constructor if there are no initial value!!!!!!
            auto const& initial_llvm_value
                = s->declaration_.decl_unit.init_unit.initializer
                ? dispatch( s->declaration_.decl_unit.init_unit.initializer, v_env )
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

                    if ( variable_llvm_type->isStructTy() ) {
                        llvm::AllocaInst* const allca_inst = context_->ir_builder.CreateAlloca( variable_llvm_type, 0/*length*/ );
                        context_->env_conversion_table.bind_value( v_env->get_id(), allca_inst );

                    } else {
                        context_->env_conversion_table.bind_value( v_env->get_id(), initial_llvm_value );
                    }
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

            // duplicate check
            assert( v_env->is_in_class() );
            if ( context_->env_conversion_table.is_defined( v_env->get_parent_class_env_id(), v_env->get_id() ) )
                return;


            //
            std::cout << "v_env->get_type_id() = " << v_env->get_type_id() << std::endl;


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

            case attribute::quality_kind::k_ref:
                assert( false && "not implemented..." );
                break;

            default:
                assert( false && "[ice]" );
                break;
            }
        }




        RILL_TV_OP_CONST( llvm_ir_generator, ast::extern_function_declaration_statement, s, parent_env )
        {
            // cast to function symbol env
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( s ) );
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



        RILL_TV_OP_CONST( llvm_ir_generator, ast::test_while_statement, s, parent_env )
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
            auto const& scope_env = root_env_->get_related_env_by_ast_ptr( s ); assert( scope_env != nullptr );
            auto const& cond_llvm_value = dispatch( s->conditional_, scope_env );
            context_->ir_builder.CreateCondBr( cond_llvm_value, body_block, final_block );

            //
            context_->ir_builder.SetInsertPoint( body_block );
//            auto const& body_scope_env = root_env_->get_related_env_by_ast_ptr( s->body_statement_ ); assert( scope_env != nullptr );
            dispatch( s->body_statement_, scope_env/*body_scope_env*/ );
            context_->ir_builder.CreateBr( while_begin_block );

            //
            context_->ir_builder.SetInsertPoint( final_block );
        }






        RILL_TV_OP_CONST( llvm_ir_generator, ast::test_if_statement, s, parent_env )
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
            auto const& scope_env = root_env_->get_related_env_by_ast_ptr( s ); assert( scope_env != nullptr );
            auto const& cond_llvm_value = dispatch( s->conditional_, scope_env );
            // else( optional )
            if ( s->else_statement_ ) {
                context_->ir_builder.CreateCondBr( cond_llvm_value, then_block, else_block );
            } else {
                context_->ir_builder.CreateCondBr( cond_llvm_value, then_block, final_block );
            }

            //
            context_->ir_builder.SetInsertPoint( then_block );
            //auto const& then_scope_env = root_env_->get_related_env_by_ast_ptr( s->then_statement_ ); assert( then_scope_env != nullptr );
            dispatch( s->then_statement_, scope_env/*then_scope_env*/ );
            context_->ir_builder.CreateBr( final_block );

            //
            if ( s->else_statement_ ) {
                context_->ir_builder.SetInsertPoint( else_block );
                //auto const& else_scope_env = root_env_->get_related_env_by_ast_ptr( *s->else_statement_ ); assert( else_scope_env != nullptr );
                dispatch( *s->else_statement_, scope_env/*else_scope_env*/ );
                context_->ir_builder.CreateBr( final_block );
            }

            //
            context_->ir_builder.SetInsertPoint( final_block );
        }






        RILL_TV_OP_CONST( llvm_ir_generator, ast::binary_operator_expression, e, parent_env )
        {
            // Look up Function
            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );

            std::cout << "current : " << f_env->mangled_name() << std::endl;
            auto const& callee_function
                = function_env_to_llvm_constatnt_ptr( *this )( f_env );
            if ( !callee_function ) {
                // unexpected error...
                assert( false && "unexpected... callee_function was not found" );
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

                auto const value = dispatch( e_arguments[e_arguments.size()-i-1], parent_env );
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



        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::element_selector_expression, e, parent_env )
        {
            //
            auto const& element_env = root_env_->get_related_env_by_ast_ptr( e );
            if ( element_env != nullptr ) {
                //

                if ( element_env->get_symbol_kind() == kind::type_value::e_variable ) {
                    // variable that belonged to class
                    auto const& v_env
                        = std::static_pointer_cast<variable_symbol_environment const>( element_env );
                    assert( v_env != nullptr );
                    assert( v_env->is_in_class() );

                    //return context_->env_conversion_table.ref_value( v_env->get_id() );
                    llvm::Value* const lhs = dispatch( e->reciever_, parent_env );

                    lhs->dump();
                    lhs->getType()->dump();

                    auto const& index
                        = context_->env_conversion_table.get_class_variable_index(
                            v_env->get_parent_class_env_id(),
                            v_env->get_id()
                            );
                    std::cout << "index: " << index << std::endl;

                    return context_->ir_builder.CreateStructGEP( lhs, index );

                } else {
                    assert( false && "[[ice]]" );
                }

            } else {
                llvm::Value* const rhs = dispatch( e->selector_id_, parent_env );
                llvm::Value* const lhs = dispatch( e->reciever_, parent_env );

                return lhs;
            }
        }



        //
        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::call_expression, e, parent_env )
        {
            // ========================================
            // look up self function
            auto const f_env
                = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );


            // ========================================
            std::cout << "current : " << f_env->mangled_name() << std::endl;
            auto const& callee_function
                = function_env_to_llvm_constatnt_ptr( *this )( f_env );
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
                auto const value = dispatch( e->arguments_[e->arguments_.size()-i-1], parent_env );
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

                result_value->dump();
//                assert( false );

                // TODO: value conversion...(ref, val, etc...)
                args[e->arguments_.size()-i-1] = result_value;
            }


            // evaluate lhs
            llvm::Value* const lhs = dispatch( e->reciever_, parent_env );
            if ( lhs != nullptr ) {
                args.insert( args.begin(), lhs );
            }

            // invocation
            return context_->ir_builder.CreateCall( callee_function, args/*, "calltmp"*/ );
        }


        // TODO: change name to native code injection expression
        RILL_TV_OP_CONST( llvm_ir_generator, ast::intrinsic_function_call_expression, e, parent_env )
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


        RILL_TV_OP_CONST( llvm_ir_generator, ast::term_expression, e, parent_env )
        {
            return dispatch( e->value_, parent_env );
        }


        // identifier node returns Variable
        RILL_TV_OP_CONST( llvm_ir_generator, ast::identifier_value, v, parent_env )
        {
            //
            std::cout << "solving: " << v->get_inner_symbol()->to_native_string() << std::endl;

            //
            auto const& env = root_env_->get_related_env_by_ast_ptr( v );
            if ( env == nullptr ) {
                std::cout << "skiped" << std::endl;
                return nullptr;
            }

            auto const& v_env
                = std::static_pointer_cast<variable_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( v ) );
            assert( v_env != nullptr );

            return context_->env_conversion_table.ref_value( v_env->get_id() );
        }



        //
        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::literal_value, v, parent_env )
        {
            // TODO: check primitive type
            if ( v->literal_type_name_->get_inner_symbol()->to_native_string() == "int" ) {
                // Currently, return int type( 32bit, integer )
                return llvm::ConstantInt::get( context_->llvm_context, llvm::APInt( 32, std::static_pointer_cast<ast::intrinsic::int32_value const>( v->holder_ )->get_value() ) );

            } else if ( v->literal_type_name_->get_inner_symbol()->to_native_string() == "string" ) {
                // char pointer...(string?)
                return context_->ir_builder.CreateGlobalStringPtr( std::static_pointer_cast<ast::intrinsic::string_value const>( v->holder_ )->value_.c_str() );

            } else {
                assert( false && "[ice] this primitive type is not supported" );
                return nullptr;
            }
        }


    } // namespace code_generator
} // namespace rill

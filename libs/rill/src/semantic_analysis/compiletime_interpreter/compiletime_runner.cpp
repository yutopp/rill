//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//


//
// Compile time interpreter
// runner
//


#include <rill/semantic_analysis/compiletime_interpreter/runner.hpp>
#include <rill/semantic_analysis/compiletime_interpreter/invoke.hpp>

#include <rill/semantic_analysis/invoke.hpp>
#include <rill/semantic_analysis/helper.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>

#include <rill/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            environment_constructor::environment_constructor( context_ptr const& ctx )
                : context_( ctx )
            {}

            // Root Scope
            RILL_TV_OP( environment_constructor, ast::root, r, env )
            {
                collect_type_identifier( env, r.statements_ );
                collect_identifier( env, r.statements_ );

                for( auto const& node : r.statements_ )
                    node->dispatch_as_env( *this,  env );
            }

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP( environment_constructor, ast::expression_statement, s, env )
            {
                std::cout
                    << "in expression_statement dispach of runner" << std::endl
                    << s.expression_->dispatch_as_env( *this, env ) << std::endl;
                std::cout << "Value(current stack top) => " << *context_->current_stack_value() << std::endl;
            }

            RILL_TV_OP( environment_constructor, ast::return_statement, s, env )
            {
                s.expression_->dispatch_as_env( *this, env );

                //std::cout << "!!!!!!!" << s.expression_->dispatch( *this, env ) << std::endl;
                //context_->current_scope()->set_return_value( s.expression_->dispatch( *this, env ) );
            }


            RILL_TV_OP( environment_constructor, ast::function_definition_statement, s, env )
            {
                // TODO: remove this case in syntax analysis phase
                if ( s.get_identifier()->nest_size() != 1 )
                    std::cout << "function_definition_statement error!!!!!!! can not specified nested definition here." << std::endl;//error()

                // TODO: add steady step to check
                //     : OR CHANGE THE PARSER
                assert( s.get_identifier()->nest_size() == 1 ); // can not use nested type here


                // construct function environment in progress phase
                auto const& f_env = env->construct(
                    kind::function_k,
                    s.get_identifier()->get_last_identifier(),
                    [&]( function_symbol_environment_ptr const& fenv ) {
                        // parameter variable declaration
                        for( auto const& e : s.get_parameter_list() ) {
                            // 
                            assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                            if ( e.decl_unit.init_unit.type ) { // is type specified ?
                                // evaluate constant expresison as type
                                auto const& type_identifier_pointer = evaluate_as_type( env, e.decl_unit.init_unit.type ); // e.decl_unit.init_unit.type->dispatch_as_env( *this, env );

                                if ( auto const param_env = lookup_with_instanciation( env, type_identifier_pointer ) ) {
                                    assert( param_env != nullptr );
                                    assert( param_env->get_symbol_kind() == kind::type_value::class_e );

                                    // declare
                                    fenv->parameter_variable_construct(
                                        /*TODO: add attributes, */
                                        nullptr,
                                        std::dynamic_pointer_cast<class_symbol_environment const>( param_env )
                                        );

                                } else {
                                    // type was not found, compilation error
                                    assert( false );
                                }

                            } else {
                                // type inferenced by result of evaluated expression

                                // TODO: implement type inference
                                assert( false );
                            }
                        }
                        return fenv;
                }, s.statements_ );

                assert( f_env != nullptr );

                // TODO: implement return type inference
            }

            //void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP( environment_constructor, ast::class_definition_statement, s, env )
            {
            }


            // expression
            RILL_TV_OP( environment_constructor, ast::binary_operator_expression, e, env )
            {
                return nullptr;
            }

            //
            RILL_TV_OP( environment_constructor, ast::call_expression, e, env )
            {
                return nullptr;
            }

            //
            // embeded function must return values in intrinsic namespace.
            //
            RILL_TV_OP( environment_constructor, ast::embedded_function_call_expression, e, env )
            {
                return nullptr;
            }

            RILL_TV_OP( environment_constructor, ast::term_expression, e, env )
            {
                return e.value_->dispatch_as_env( *this, env );
            }


            //
            RILL_TV_OP( environment_constructor, ast::intrinsic_value, v, env )
            {
               return nullptr;
            }

            RILL_TV_OP( environment_constructor, ast::variable_value, v, env )
            {
                return nullptr;
            }




            //
            //
            //
            //
            //
            RILL_TV_OP( type_evaluator, ast::type_identifier_expression, e, env )
            {
                return e.value_;
            }

            RILL_TV_OP( type_evaluator, ast::compiletime_return_type_expression, e, env )
            {
                // !!! Unimplemented !!!
                // TODO: evaluate by value as constant and cast to type identifiern
                throw -1;
                return nullptr;
            }
        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill
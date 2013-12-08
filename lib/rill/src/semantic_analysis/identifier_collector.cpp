//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/identifier_collector.hpp>
#include <rill/environment/environment.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // Root Scope
        RILL_TV_OP( identifier_collector, ast::root, r, env )
        {
            // build environment
            for( auto const& node : r->statements_ )
                dispatch( node, env );
        }



        //
        RILL_TV_OP( identifier_collector, ast::expression_statement, s, env )
        {
            // DO NOT COLLECT IDENTIFIERS
        }



        //
        RILL_TV_OP( identifier_collector, ast::function_definition_statement, s, env )
        {
            // TODO: remove this case in syntax analysis phase
            if ( s->get_identifier()->nest_size() != 1 )
                std::cout << "function_definition_statement error!!!!!!! can not specified nested definition here." << std::endl;//error()

            // TODO: add steady step to check
            //     : OR CHANGE THE PARSER
            assert( s->get_identifier()->nest_size() == 1 ); // can not use nested type here

            std::cout << "collected : " << s->get_identifier()->get_last_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            // add function symbol to current environment
            env->mark_as( kind::k_function, s->get_identifier()->get_last_identifier(), s );
        }



        //
        RILL_TV_OP( identifier_collector, ast::extern_function_declaration_statement, s, env )
        {
            // TODO: remove this case in syntax analysis phase
            if ( s->get_identifier()->nest_size() != 1 )
                std::cout << "function_definition_statement error!!!!!!! can not specified nested definition here." << std::endl;//error()

            // TODO: add steady step to check
            //     : OR CHANGE THE PARSER
            assert( s->get_identifier()->nest_size() == 1 ); // can not use nested type here

            std::cout << "collected : " << s->get_identifier()->get_last_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            // add function symbol to current environment
            env->mark_as( kind::k_function, s->get_identifier()->get_last_identifier(), s );
        }



        //
        RILL_TV_OP( identifier_collector, ast::class_function_definition_statement, s, parent_env )
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            std::cout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            // add function symbol to current environment
            auto const& f_env_pair = parent_env->mark_as( kind::k_function, s->get_identifier(), s );
            f_env_pair.second->set_parent_class_env_id( parent_env->get_id() );
        }



        //
        RILL_TV_OP( identifier_collector, ast::variable_declaration_statement, s, env )
        {
            // NOTHING TO DO
        }



        //
        RILL_TV_OP( identifier_collector, ast::class_variable_declaration_statement, s, parent_env )
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // variable declared in class scope should be forward referencable
            std::cout << "gfphjdfgphjd@fpgij@" << std::endl;

            // add variable symbol to current environment
            auto const& v_env = parent_env->mark_as( kind::k_variable, s->get_identifier(), s );
            v_env->set_parent_class_env_id( parent_env->get_id() );
        }




        //
        RILL_TV_OP( identifier_collector, ast::class_definition_statement, s, env )
        {
            // add class symbol to current environment
            auto c_env = env->mark_as( kind::k_class, s->get_identifier(), s );

            // build environment
            for( auto const& node : s->statements_ )
                dispatch( node, c_env );
        }

    } // namespace semantic_analysis
} // namespace rill

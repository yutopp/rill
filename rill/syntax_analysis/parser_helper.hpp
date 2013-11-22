//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_PARSER_HELPER_HPP
#define RILL_SYNTAX_ANALYSIS_PARSER_HELPER_HPP

#include <string>
#include <vector>
#include <memory>
#include <iostream>

#define BOOST_SPIRIT_USE_PHOENIX_V3 1
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"
#include "../ast/root.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace helper
        {
            template<typename T>
            struct add_ {
                typedef std::shared_ptr<T> result_type;

                template<class>
                struct result {
                    typename std::shared_ptr<T> type;
                };

                template<typename... Args>
                auto operator()( Args&&... args ) const
                    -> result_type
                {
                    return std::make_shared<T>( std::forward<Args>( args )... );
                }
            };


            template<typename T, typename... Args>
            auto make_node_ptr( Args&&... args )
                -> decltype( boost::phoenix::function<add_<T>>()( std::forward<Args>( args )... ) )
            {
                return boost::phoenix::function<add_<T>>()( std::forward<Args>( args )... );
            }


            template<typename T0, typename T1>
            auto make_binary_op_node_ptr( T0 const& lhs, ast::native_string_t const& op, T1 const& rhs )
                -> decltype(
                    make_node_ptr<ast::binary_operator_expression>(
                        lhs,
                        ast::intrinsic::make_binary_operator_identifier( op ),
                        rhs
                        )
                    )
            {
                return make_node_ptr<ast::binary_operator_expression>(
                        lhs,
                        ast::intrinsic::make_binary_operator_identifier( op ),
                        rhs
                        );
            }


            template<typename T, typename... Args>
            auto make_intrinsic_value_ptr( Args const&... xs )
                -> decltype( make_node_ptr<ast::intrinsic_value>( make_node_ptr<T>( xs... ) ) )
            {
                return make_node_ptr<ast::intrinsic_value>( make_node_ptr<T>( xs... ) );
            }

        } // namespace helper
    } // namespace sytax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_PARSER_HELPER_HPP*/

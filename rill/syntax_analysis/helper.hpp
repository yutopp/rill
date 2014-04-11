//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_HELPER_HPP
#define RILL_SYNTAX_ANALYSIS_HELPER_HPP

#include <string>
#include <memory>

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3
#endif

#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/spirit/include/phoenix_core.hpp>

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace helper
        {
            template<typename T>
            struct make_node_pointer_lazy
            {
                typedef std::shared_ptr<T>      result_type;

                template<typename... Args>
                auto operator()( Args&&... args ) const
                    -> result_type
                {
                    return std::make_shared<T>( std::forward<Args>( args )... );
                }
            };


            template<typename T, typename... Args>
            auto make_node_ptr( Args&&... args )
                -> decltype( boost::phoenix::function<make_node_pointer_lazy<T>>()( std::forward<Args>( args )... ) )
            {
                return boost::phoenix::function<make_node_pointer_lazy<T>>()( std::forward<Args>( args )... );
            }


            template<typename T0, typename T1>
            auto make_binary_op_node_ptr( T0 const& lhs, ast::native_string_t const& op, T1 const& rhs )
                -> decltype(
                    make_node_ptr<ast::binary_operator_expression>(
                        lhs,
                        ast::make_binary_operator_identifier( op ),
                        rhs
                        )
                    )
            {
                return make_node_ptr<ast::binary_operator_expression>(
                        lhs,
                        ast::make_binary_operator_identifier( op ),
                        rhs
                        );
            }


            template<typename T, typename... Args>
            auto make_literal_value_ptr( Args const&... xs )
                -> decltype( make_node_ptr<ast::literal_value>( make_node_ptr<T>( xs... ) ) )
            {
                return make_node_ptr<ast::literal_value>( make_node_ptr<T>( xs... ) );
            }

        } // namespace helper
    } // namespace syntax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_HELPER_HPP*/

//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_ON_SUCCESS_HPP
#define RILL_SYNTAX_ANALYSIS_ON_SUCCESS_HPP

#include <iostream>
#include <type_traits>

#include <boost/spirit/home/x3.hpp>

#include "position.hpp"
#include "tag.hpp"
#include "../ast/ast_base.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        class on_success_annotator_base
        {
        public:
            template<typename Iterator, typename T, typename Context>
            inline auto on_success(
                Iterator const& first,
                Iterator const& last,
                std::shared_ptr<T>& t,
                Context const& context
                ) const
                -> void
            {
                tagging<Iterator, T, Context>( first, last, t, context );
            }

        private:
            template<typename Iterator, typename T, typename Context>
            inline auto tagging(
                Iterator const& first,
                Iterator const& last,
                std::shared_ptr<T>& ast,
                Context const& context
                ) const
                -> std::enable_if_t<
                    std::is_base_of<ast::ast_base, T>::value,
                    void
                >
            {
                auto const line = spirit::get_line( first );

                auto const& orig_begin
                    = x3::get<iterator_orig_begin_tag>( context );
                auto const line_first
                    = Iterator( detail::get_line_start( orig_begin, first.base() ) );
                auto const column = spirit::get_column( line_first, first );

                ast->line = line;
                ast->column = column;
            }

            template<typename Iterator, typename T, typename Context>
            inline auto tagging(
                Iterator const&,
                Iterator const&,
                std::shared_ptr<T> const&,
                Context const&
                ) const
                -> std::enable_if_t<
                    !std::is_base_of<ast::ast_base, T>::value,
                    void
                >
            {}
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_ON_SUCCESS_HPP*/

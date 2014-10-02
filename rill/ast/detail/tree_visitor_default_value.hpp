//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_TREE_VISITOR_DEFAULT_VALUE_HPP
#define RILL_AST_DETAIL_TREE_VISITOR_DEFAULT_VALUE_HPP


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            //
            template<typename ReturnT>
            struct make_return_value
            {
                auto operator()( char* const storage ) const
                    -> ReturnT
                {
                    return std::move( *reinterpret_cast<ReturnT* const>( storage ) );
                }
            };
            template<>
            struct make_return_value<void>
            {
                auto operator()( char* const/* unused */) const
                    -> void
                {}
            };

            //
            template<typename ReturnT>
            struct make_default_return_value
            {
                auto operator()() const
                    -> ReturnT
                {
                    return ReturnT{};
                }
            };
            template<>
            struct make_default_return_value<void>
            {
                auto operator()() const
                    -> void
                {}
            };

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_TREE_VISITOR_DEFAULT_VALUE_HPP*/

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_VISITOR_DELEGATOR_HPP
#define RILL_AST_DETAIL_VISITOR_DELEGATOR_HPP

#include <memory>
#include <type_traits>

#include "../value_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../statement_fwd.hpp"
#include "tree_visitor_result_t.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            //
            template<typename Derived, typename ReturnT>
            struct tree_visitor_base;

            // ========================================
            //
            // ========================================
            class visitor_delegator_base
            {
            public:
#define RILL_AST_ENUM_DELEGETOR_SIGNATURE
#define RILL_AST_FILE_RELOAD
# include "../value_def.ipp"
# include "../expression_def.ipp"
# include "../statement_def.ipp"
#undef RILL_AST_FILE_RELOAD
#undef RILL_AST_ENUM_DELEGETOR_SIGNATURE
            };


            // ========================================
            //
            // ========================================
            template<typename Visitor, typename ReturnT>
            class visitor_delegator
                : public visitor_delegator_base
            {
            public:
                visitor_delegator( Visitor* v )
                    : v_( v )
                {}

            public:
#define RILL_AST_ENUM_DELEGETOR_FUNCTION
#define RILL_AST_FILE_RELOAD
# include "../value_def.ipp"
# include "../expression_def.ipp"
# include "../statement_def.ipp"
#undef RILL_AST_FILE_RELOAD
#undef RILL_AST_ENUM_DELEGETOR_FUNCTION

            private:
                template<typename ResultT, typename Node, typename Env>
                auto call_visitor(
                    std::shared_ptr<Node> const& node,
                    std::shared_ptr<Env> const& env,
                    void* const /* unused */,
                    void* /* tag */
                    ) const
                    -> void
                {
                    ( *v_ ).operator()( node, env );
                }

                template<typename ResultT, typename Node, typename Env>
                auto call_visitor(
                    std::shared_ptr<Node> const& node,
                    std::shared_ptr<Env> const& env,
                    void* const storage,
                    ResultT* /* tag */
                    ) const
                    -> void
                {
                    new( storage ) ReturnT(
                        std::move( ( *v_ ).operator()( node, env ) )
                        );
                }

            private:
                Visitor* v_;
            };

        } // namespace detail
    } // namespace ast
} // namespace rill


#endif /*RILL_AST_DETAIL_VISITOR_DELEGATOR_HPP*/

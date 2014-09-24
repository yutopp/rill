//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP
#define RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // ========================================
            //
            // ========================================
            class visitor_delegator_base
            {
            public:
#define RILL_AST_ENUM_DELEGETOR_SIGNATURE

#undef RILL_AST_ENUM_DELEGETOR_SIGNATURE
            };


            // ========================================
            //
            // ========================================
            template<typename Visitor, typename ReturnT>
            class visitor_delegator
                : public visitor_invoker_base
            {
            public:
                template<typename NodeT>
                using result = visitor_result_traits<ReturnT, NodeT>;

            public:
                BOOST_PP_SEQ_FOR_EACH( RILL_VISITOR_INVOKER_OP_GEN, _, RILL_AST_NODE_TYPE_SEQ )
            };

        } // namespace detail
    } // namespace ast
} // namespace rill


#endif /*RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP*/

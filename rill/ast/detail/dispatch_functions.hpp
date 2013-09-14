//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_DISPATCH_FUNCTIONS_HPP
#define RILL_AST_DETAIL_DISPATCH_FUNCTIONS_HPP

#include <memory>

#include <boost/mpl/map.hpp>
#include <boost/mpl/at.hpp>

#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>

#include <llvm/IR/Value.h>

#include "../../config/macros.hpp"
#include "../../environment_fwd.hpp"

#include "../statement_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../value_fwd.hpp"
#include "../root_fwd.hpp"


// important
#define RILL_DISPATCH_TYPES_SEQ \
    (( dispatch_as_environment_tag,     environment_ptr )) \
    (( dispatch_as_value_tag,           ast::value_ptr )) \
    (( dispatch_as_type_tag,            ast::intrinsic::identifier_value_ptr )) \
    (( dispatch_as_llvm_ir_value_tag,   llvm::Value* ))


#define RILL_DETAIL_PP_MAKE_TAG_STRUCTURE(r, unused, elem) \
    struct BOOST_PP_TUPLE_ELEM(2, 0, elem) {};

#define RILL_DETAIL_PP_MPL_PAIR_NAME_A_B \
    mpl_pair_a_b_

#define RILL_DETAIL_PP_MPL_PAIR_NAME_B_A \
    mpl_pair_b_a_

#define RILL_DETAIL_PP_TYPEDEF_MPL_PAIR(r, unused, i, elem) \
    typedef boost::mpl::pair<BOOST_PP_TUPLE_ELEM(2, 0, elem), BOOST_PP_TUPLE_ELEM(2, 1, elem)> BOOST_PP_CAT( RILL_DETAIL_PP_MPL_PAIR_NAME_A_B, i ); \
    typedef boost::mpl::pair<BOOST_PP_TUPLE_ELEM(2, 1, elem), BOOST_PP_TUPLE_ELEM(2, 0, elem)> BOOST_PP_CAT( RILL_DETAIL_PP_MPL_PAIR_NAME_B_A, i );

namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // dispatch types(tag structures)
            BOOST_PP_SEQ_FOR_EACH(RILL_DETAIL_PP_MAKE_TAG_STRUCTURE, _, RILL_DISPATCH_TYPES_SEQ)

            // temporary typedefs
            namespace temporary_typedef { BOOST_PP_SEQ_FOR_EACH_I(RILL_DETAIL_PP_TYPEDEF_MPL_PAIR, _, RILL_DISPATCH_TYPES_SEQ) }

            // tag -> dispatch type mapping
            typedef boost::mpl::map<
                BOOST_PP_ENUM_PARAMS(BOOST_PP_SEQ_SIZE(RILL_DISPATCH_TYPES_SEQ), temporary_typedef::RILL_DETAIL_PP_MPL_PAIR_NAME_A_B)
            > as_type;
            // dispatch type -> tag mapping
            typedef boost::mpl::map<
                BOOST_PP_ENUM_PARAMS(BOOST_PP_SEQ_SIZE(RILL_DISPATCH_TYPES_SEQ), temporary_typedef::RILL_DETAIL_PP_MPL_PAIR_NAME_B_A)
            > as_reverse_type;
        } // namespace detail


        //
        template<typename ReturnT, typename NodePtr, typename VisitorT>
        auto dispatch_as(
            NodePtr&& node,
            VisitorT&& visitor,
            environment_ptr const& env
            )
            -> decltype((
                    node->dispatch(
                        typename boost::mpl::at<detail::as_reverse_type, ReturnT>::type(),
                        std::forward<NodePtr>( node ),
                        std::forward<VisitorT>( visitor ),
                        env
                        )
               ))
        {
            return node->dispatch(
                typename boost::mpl::at<detail::as_reverse_type, ReturnT>::type(),
                std::forward<NodePtr>( node ),
                std::forward<VisitorT>( visitor ),
                env
                );
        }




        template<typename NodeT, typename VisitorT>
        auto dispatch_as_env(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            // -> std::shared_ptr<void>
            -> decltype(( node->dispatch( detail::dispatch_as_environment_tag(), node, visitor, env ) ))
        {
            return node->dispatch( detail::dispatch_as_environment_tag(), node, visitor, env );
        }

        template<typename NodeT, typename VisitorT>
        auto dispatch_as_value(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            -> decltype( node->dispatch( detail::dispatch_as_value_tag(), node, visitor, env ) )
        {
            return node->dispatch( detail::dispatch_as_value_tag(), node, visitor, env );
        }

        template<typename NodeT, typename VisitorT>
        auto dispatch_as_type(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            -> decltype( node->dispatch( detail::dispatch_as_type_tag(), node, visitor, env ) )
        {
            return node->dispatch( detail::dispatch_as_type_tag(), node, visitor, env );
        }

    } // namespace ast

    using ast::dispatch_as_env;
    using ast::dispatch_as_value;
    using ast::dispatch_as_type;
} // namespace rill

#undef RILL_DETAIL_PP_MPL_PAIR_NAME_A_B
#undef RILL_DETAIL_PP_MPL_PAIR_NAME_B_A
#undef RILL_DETAIL_PP_TYPEDEF_MPL_PAIR

#endif /*RILL_AST_DETAIL_DISPATCH_FUNCTIONS_HPP*/

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_AST_BASE_HPP
#define RILL_AST_AST_BASE_HPP

#include <cstddef>
#ifdef RILL_DEBUG
# include <iostream>
#endif

#include "detail/tree_visitor_base.hpp"
#include "detail/visitor_delegator.hpp"
#include "detail/ast_def_macro.hpp"


namespace rill
{
    namespace ast
    {
        typedef std::size_t         ast_id_t;

        namespace detail
        {
            // TODO: supoprt for thread safe
            class ast_id_generator
            {
            public:

                ast_id_generator()
#ifdef RILL_DEBUG
                {
                    std::cout << "AST ID GENERATOR" << std::endl;
                }
#else
                = default;
#endif

            public:
                auto operator()()
                    -> ast_id_t
                {
                    // TODO: THREAD SAFE
                    return ++generated_counter_;
                }

            private:
                ast_id_t generated_counter_ = 0;
            };
        } // namespace detail


        // base class for all AST
        class ast_base
        {
        public:
            ast_base()
            {
                id_ = igen_();  // generate id
#ifdef RILL_DEBUG
                std::cout << "NEW AST( " << typeid( this ).name() << " )@ ID: " << id_ << std::endl;
#endif
            }

        public:
            inline auto get_id() const -> ast_id_t {
                return id_;
            }

        public:
            std::size_t line, column;

        private:
            static detail::ast_id_generator igen_;
            ast_id_t id_;
        };

        //
        typedef std::shared_ptr<ast_base> ast_base_ptr;
        typedef std::shared_ptr<ast_base const> const_ast_base_ptr;

    } // namespace ast
} // namespace rill

#endif /*RILL_AST_AST_BASE_HPP*/

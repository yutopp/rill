//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_HANDLERS_HPP
#define RILL_SYNTAX_ANALYSIS_HANDLERS_HPP

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3
#endif

#include <boost/spirit/include/support_line_pos_iterator.hpp>

#include "error.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        // set position data to AST
        template<typename It>
        class position_annotator_lazy
        {
        public:
            typedef void result_type;

        public:
            position_annotator_lazy( It first )
                : head_( first )
                {}

        public:
            template<typename Val, typename First, typename Last>
            void operator()( Val& v, First f, Last l ) const
                {
                    do_annotate( v, f, l );
                }

        private:
            template<typename AstNode>
            void do_annotate(
                std::shared_ptr<AstNode> const& li,
                It f,
                It l
                ) const
            {
                using boost::spirit::get_line;
                using boost::spirit::get_column;
                using std::distance;

                li->line   = get_line( f );
                li->column = get_column( head_, f );
                li->length = distance( f, l );
            }

            // to discard unexpected object
            template<typename... Args>
            void do_annotate( Args&&... ) const
            {}

        private:
            It const head_;
        };


        // give information to error reporter
        template<typename It, typename ErrorHolder>
        class error_handler_lazy
        {
        public:
            typedef void    result_type;

        public:
            error_handler_lazy( ErrorHolder const& es )
                : error_holder_( es )
            {}

        public:
            template<typename T>
            auto operator()(
                It& first,
                It const& end,
                It const& where,
                T const& what
                ) const
                -> result_type
            {
                report_and_repair_status( first, end, where, what, error_holder_ );
            }

        private:
            ErrorHolder error_holder_;
        };
    } // namespace syntax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_HANDLERS_HPP*/

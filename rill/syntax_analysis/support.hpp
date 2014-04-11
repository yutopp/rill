//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_SUPPORT_HPP
#define RILL_SYNTAX_ANALYSIS_SUPPORT_HPP

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3 1
#endif
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_object.hpp>

#include "error.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace qi = boost::spirit::qi;
        namespace phx = boost::phoenix;

        //
        template<typename Iterator>
        struct attacher
        {
            attacher( Iterator const& head, std::shared_ptr<error_container> const& ec )
                : position_annotator_( head )
                , error_handler_( ec )
            {}

        public:
            template<qi::error_handler_result E, typename Rule, typename T>
            auto operator()( Rule& rule, T const& n ) const
                -> void
            {
                rule.name( n );

                auto const err_handler   = error_handler_( qi::_1, qi::_2, qi::_3, qi::_4 );
                auto const pos_annotator = position_annotator_( qi::_val, qi::_1, qi::_3 );

                qi::on_error<E>( rule, err_handler );
                qi::on_success( rule, pos_annotator );

#ifdef RILL_DEBUG
                qi::debug( rule );
#endif
            }

            template<typename Rule, typename T>
            auto operator()( Rule& rule, T const& name ) const
                -> void
            {
                this->operator()<qi::retry>( rule, name );
            }

        private:
            phx::function<position_annotator_lazy<Iterator>> position_annotator_;
            phx::function<error_handler_lazy<Iterator, std::shared_ptr<error_container>>> error_handler_;
        };
    } // namespace syntax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_SUPPORT_HPP*/

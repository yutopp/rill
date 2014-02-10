//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP
#define RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP

#include <cassert>
#include <memory>
//#include <unordered_map>
//#include <bitset>
//#include <vector>
//#include <utility>
//#include <boost/range/adaptor/transformed.hpp>

//#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"


namespace rill
{
    //
    // template environment
    // this environment holds template parameters
    // and bacomes the POINT linked to TEMPLATE symbol's AST.
    // So, to get inner AST(AST of class, function, etc...), get 's', the template AST, linked from this env, then call s.get_inner_statement() member function
    //
    class template_environment RILL_CXX11_FINAL
        : public single_identifier_environment_base
    {
    public:
        typedef std::size_t     template_argument_length_type;
        static template_argument_length_type const variadic_length = -1;

    public:
        template_environment( environment_parameter_t&& pp, environment_id_t const& template_set_env_id )
            : single_identifier_environment_base( std::move( pp ) )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_template;
        }

        auto get_arg_size() const
            -> template_argument_length_type
        {
            // TODO: IMPLEMENT!!!!!!!
            return 1;
        }

    private:
        type_id_list_t template_parameter_type_ids_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP*/

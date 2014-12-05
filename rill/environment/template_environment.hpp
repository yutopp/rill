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

#include "environment_base.hpp"


namespace rill
{
    //
    // template environment
    //   bacomes the POINT that is linked to TEMPLATE symbol's AST.
    // So, to get inner AST(AST of class, function, etc...), do like below.
    //   1. get the template AST(e.g. named 's'), that is linked from this env
    //   2. call s.get_inner_statement() member function
    //
    class template_environment RILL_CXX11_FINAL
        : public environment_base
    {
    public:
        static kind::type_value const KindValue = kind::type_value::e_template;

    public:
        template_environment(
            environment_parameter_t&& pp,
            weak_multiple_set_environment_ptr const& multiset_env_ptr
            )
            : environment_base( std::move( pp ) )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_template;
        }
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP*/

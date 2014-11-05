//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment/environment.hpp>


namespace rill
{
    kind::type_value const class_symbol_environment::KindValue
        = kind::type_value::e_class;

    auto class_symbol_environment::make_type_id_from(
        attribute::type_attributes const& type_attr
        ) const
        -> type_id_t
    {
        return b_.lock()->make_type_id(
            cast_to<class_symbol_environment const>( shared_from_this() ),
            type_attr
            );
    }

} // namespace rill

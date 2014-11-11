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

    auto class_symbol_environment::is_default_copyable() const
        -> bool
    {
        return !has_traits_flag( class_traits_kind::k_has_non_trivial_copy_ctor )
            && !has_traits_flag( class_traits_kind::k_has_non_trivial_move_ctor )
            && !has_traits_flag( class_traits_kind::k_has_non_trivial_copy_assign )
            && !has_traits_flag( class_traits_kind::k_has_non_trivial_move_assign )
            && !has_traits_flag( class_traits_kind::k_has_non_trivial_dtor )
            && !has_traits_flag( class_traits_kind::k_has_non_trivial_copy_ctor )
            && !has_traits_flag( class_traits_kind::k_has_non_default_copyable_member )
            ;
    }

} // namespace rill

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>

#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

#include <rill/environment.hpp>



std::ostream& operator<<( std::ostream& os, value const& vp )
{
    if ( vp.is_intrinsic() || vp.is_system() ) {
        auto const& iv = vp.is_intrinsic()
                        ? *dynamic_cast<intrinsic_value const&>( vp ).value_
                        : dynamic_cast<intrinsic::value_base const&>( vp )
                        ;
        
        os << "  type  is " << iv.get_native_type_name_string() << std::endl;
        if ( iv.get_native_type_name_string() == "int" ) {
            os << "  value is " << dynamic_cast<intrinsic::int32_value const&>( iv ).value_ << std::endl;
        } else {
            os << "  value is unknown." << std::endl;
        }
    } else {
        os << "  NOT typed value." << std::endl;
    }

    return os;
}

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_TYPE_TYPE_HPP
#define RILL_TYPE_TYPE_HPP

#include "../environment/environment_fwd.hpp"
#include "attribute.hpp"


namespace rill
{
    // type is composed to Class and Attributes
    struct type
    {
        environment_id_t class_env_id;
        attribute::type_attributes attributes;

        auto is_incomplete() const
            -> bool
        {
            return class_env_id == environment_id_undefined;
        }
    };
} // namespace rill

#endif /*RILL_TYPE_TYPE_HPP*/

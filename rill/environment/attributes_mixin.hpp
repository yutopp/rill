//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once


namespace rill
{
    class attributes_mixin
    {
    public:
        attributes_mixin()
            : decl_attr_( attribute::decl::k_default )
        {}

    public:
        bool has_attribute( attribute::decl::type const& attribute ) const
        {
            return ( decl_attr_ & attribute ) != 0;
        }

        void set_attribute( attribute::decl::type const& attribute )
        {
            decl_attr_ |= attribute;
        }

        void unset_attribute( attribute::decl::type const& attribute )
        {
            decl_attr_ ^= attribute;
        }

    private:
        attribute::decl::type decl_attr_;
    };

} // namespace rill

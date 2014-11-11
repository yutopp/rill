//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // debug

#include <rill/environment/environment.hpp>

#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    std::ostream& operator<<( std::ostream& os, const_environment_base_ptr const& env )
    {
        os << "DEBUG: environment" << std::endl;
        auto e = env;
        std::string indent = "^ ";

        if ( e ) {
            while( !e->is_root() ) {
                e->dump( os, indent );
                e = e->get_parent_env();
                indent += "  ";
            }
            return e->dump( os, indent );
        } else {
            os << indent << "nullptr." << std::endl;
            return os;
        }
    }


















} // namespace rill

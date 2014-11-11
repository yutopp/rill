//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment/make_module_name.hpp>

#include <iostream> // debug
#include <boost/filesystem.hpp>


namespace rill
{
    auto make_module_name(
        boost::filesystem::path const& base_path,
        ast::const_module_ptr const& module
        )
        -> std::string
    {
        namespace fs = boost::filesystem;

        std::cout << "base: " << base_path << std::endl
                  << "src : " << module->fullpath << std::endl;

        if ( module->fullpath.empty() ) {
            // empty module name
            return "";
        }

        assert( base_path.is_absolute() );
        assert( module->fullpath.is_absolute() );
        assert( module->fullpath > base_path );

        auto m_it = module->fullpath.begin();

        // skip base
        for( auto it = base_path.begin(); it != base_path.end(); ++it ) {
            ++m_it;
        }

        //
        fs::path module_abs_path;
        for( auto it = m_it; it != module->fullpath.end(); ++it ) {
            module_abs_path /= *it;
        }

        //
        std::cout << module_abs_path << std::endl;

        std::string module_name = module_abs_path.stem().string();
        std::cout << "module_name: " << module_name << std::endl;

        return module_name;
    }

} // namespace rill

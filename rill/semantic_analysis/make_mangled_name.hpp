//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_MAKE_MANGLED_NAME_HPP
#define RILL_SEMANTIC_ANALYSIS_MAKE_MANGLED_NAME_HPP

#include <string>
#include <functional>

#include <boost/optional.hpp>

#include "../environment/environment_base.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        auto make_qualified_name(
            const_class_symbol_environment_ptr const& c_env,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
            )
            -> std::string;


        auto make_mangled_name(
            const_class_symbol_environment_ptr const& c_env,
            attribute::type_attributes const& attr,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
            )
            -> std::string;

        auto make_mangled_name(
            const_global_environment_ptr const& global_env,
            const_function_symbol_environment_ptr const& f_env,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
            )
            -> std::string;

        auto make_signature_for_virtual_function(
            const_global_environment_ptr const& global_env,
            const_function_symbol_environment_ptr const& f_env
            )
            -> std::string;

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_MAKE_MANGLED_NAME_HPP*/

//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/make_mangled_name.hpp>

#include <rill/environment/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        auto make_qualified_name(
            const_class_symbol_environment_ptr const& c_env,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature
            )
            -> std::string
        {
            assert( c_env != nullptr );
            //assert( c_env->is_checked() );

            std::string s;
            s += std::to_string( c_env->get_base_name().size() );
            s += c_env->get_base_name();

            if ( template_signature ) {
                s += "TA[";
                s += std::to_string( template_signature->get().size() );
                s += "]";
                s += template_signature->get();
            }

            return s;
        }


        auto make_mangled_name(
            const_class_symbol_environment_ptr const& c_env,
            attribute::type_attributes const& attr,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature
            )
            -> std::string
        {
            assert( c_env != nullptr );
            rill_dout << "base_name :: " << c_env->get_base_name() << std::endl
                      << "mangling  :: " << c_env->get_qualified_name() << std::endl
                      << attr << std::endl
                      << "==========" << std::endl;

            assert( c_env->is_checked() );

            std::string s = c_env->get_qualified_name();

            s += [&]() {
                switch( attr.quality )
                {
                case attribute::holder_kind::k_suggest:
                    return "LET";
                case attribute::holder_kind::k_val:
                    return "VAL";
                case attribute::holder_kind::k_ref:
                    return "REF";
                default:
                    assert( false );
                    return "";
                }
            }();

            s += [&]() {
                switch( attr.modifiability )
                {
                case attribute::modifiability_kind::k_mutable:
                    return "MUT";
                case attribute::modifiability_kind::k_const:
                    return "CST";
                case attribute::modifiability_kind::k_immutable:
                case attribute::modifiability_kind::k_none: // none == immutable
                    return "IMM";
                default:
                    assert( false );
                    return "";
                }
            }();

            return s;
        }


        auto make_mangled_name(
            const_global_environment_ptr const& global_env,
            const_function_symbol_environment_ptr const& f_env,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature
            )
            -> std::string
        {
            assert( f_env != nullptr );
            assert( f_env->is_checked() );

            std::string s = "_R";

            s += std::to_string( f_env->get_base_name().size() );
            s += f_env->get_base_name();

            for( auto const& type_id : f_env->get_parameter_type_ids() ) {
                auto const& param_type = global_env->get_type_at( type_id );
                s += make_mangled_name(
                    global_env->get_env_at_as_strong_ref<class_symbol_environment const>(
                        param_type.class_env_id
                        ),
                    param_type.attributes
                    );
            }

            return s;
        }

    } // namespace semantic_analysis
} // namespace rill

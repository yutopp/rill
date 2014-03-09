//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once


#include <cassert>
#include <memory>
#include <unordered_map>
#include <bitset>
#include <vector>
#include <utility>
#include <boost/range/adaptor/transformed.hpp>

#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "environment_base.hpp"


namespace rill
{
    //
    //
    enum class environment_process_progress_t
    {
        constructed,
        checked,
        completed
    };


    //
    //
    class single_identifier_environment_base
        : public environment_base
    {
        typedef single_identifier_environment_base      self_type;
        typedef std::shared_ptr<self_type>              self_pointer;
        typedef std::shared_ptr<self_type>              self_const_pointer;

    public:
        single_identifier_environment_base( root_initialize_tag )
            : environment_base( root_initialize_tag() )
            , progress_( environment_process_progress_t::constructed )
        {}

        single_identifier_environment_base( environment_parameter_t&& pp )
            : environment_base( std::move( pp ) )
            , progress_( environment_process_progress_t::constructed )
        {}

        virtual ~single_identifier_environment_base() {};

    public:
        virtual auto lookup( ast::const_identifier_value_base_ptr const& )
            -> env_base_pointer RILL_CXX11_OVERRIDE;
        virtual auto lookup( ast::const_identifier_value_base_ptr const& ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE;

        //
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& )
            -> env_base_pointer RILL_CXX11_OVERRIDE;
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE;

        //
        //
        //
        auto is_incomplete() const
            -> bool
        {
            return progress_ == environment_process_progress_t::constructed;
        }

        auto is_checked() const
            -> bool
        {
            return progress_ >= environment_process_progress_t::checked;
        }

        auto is_complete() const
            -> bool
        {
            return progress_ >= environment_process_progress_t::completed;
        }



        //
        auto change_progress_to_checked()
            -> void
        {
            progress_ = environment_process_progress_t::checked;
        }

        auto change_progress_to_completed()
            -> void
        {
            progress_ = environment_process_progress_t::completed;
        }


    /*
        auto is_exist_in_instanced( native_string_type const& name ) const
            -> boost::optional<env_base_pointer>
        {
            auto const& it = instanced_env_.find( name );
            if ( it != instanced_env_.cend() )
                return it->second;

            return boost::none;
        }*/

        /*
        auto is_same_pre_declared_type( identifier_value_ptr const& name, symbol_kind const& kind ) const
            -> bool
        {

        }*/

        //
        // incomplete_construct
        //
        virtual auto incomplete_construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const&
            ) -> std::pair<
                     std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                     function_symbol_environment_ptr
                 >;

        virtual auto incomplete_construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&
            ) -> variable_symbol_environment_ptr;

        virtual auto incomplete_construct(
            kind::class_tag,
            ast::identifier_value_base_ptr const&
            ) -> class_symbol_environment_ptr;

        virtual auto incomplete_construct(
            kind::template_tag,
            ast::identifier_value_base_ptr const&
            ) -> std::pair<
                     template_set_environment_ptr,
                     template_environment_ptr
                 >;

        //
        // construct
        //
        typedef std::function<function_symbol_environment_ptr (function_symbol_environment_ptr const&)> function_env_generator_scope_type;
        virtual auto construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            function_env_generator_scope_type const&,
            class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> function_symbol_environment_ptr;


        virtual auto construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            const_class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> variable_symbol_environment_ptr;


        virtual auto construct(
            kind::class_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> class_symbol_environment_ptr;



        //
        //
        auto construct_template_parameter_var(
            ast::identifier_value_base_ptr const& param_name,
            ast::statement_ptr const& s,
            const_class_symbol_environment_ptr const& c_env,
            attribute::type_attributes const& t_attr
            ) -> variable_symbol_environment_ptr
        {
            auto const& v_env = construct( kind::k_variable, param_name, s, c_env, t_attr );
            template_parameter_var_decl_ids_.push_back(
                std::static_pointer_cast<environment_base const>( v_env )->get_id()
                );

            return v_env;
        }

        auto get_template_parameter_var_decl_ids() const
            -> environment_id_list_t const&
        {
            return template_parameter_var_decl_ids_;
        }


        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "single_identifier_environment_base" << std::endl;
            return dump_include_env( os, indent );
        }

        auto dump_include_env( std::ostream& os, std::string const& indent ) const
            -> std::ostream&
        {
            os << indent << "== NON template ==" << std::endl;
            for( auto const& ins : nontemplate_env_ ) {
                auto const& key = ins.first;
                auto const& env = ins.second;

                auto const& ast = env->get_related_ast();

                os << indent
                   << "-> symbol_name: " << key
                   << " / id: " << env->get_id()
                   << " / linked_astptr: " << ast.get()
                   << " / symbol kind: " << static_cast<int>( env->get_symbol_kind() ) << std::endl;
            }

            return os;
        }

        // TODO: fixit
        // logic is wrong
        auto is_instanced( native_string_type const& name ) const
            -> bool
        {
            return nontemplate_env_.find( name ) != nontemplate_env_.end();
        }

        auto is_exist_at_template( native_string_type const& name ) const
            -> bool
        {
            return template_env_.find( name ) != template_env_.end();
        }

        template<typename EnvPtr>
        auto insert( EnvPtr const& e )
            -> void
        {
            // TODO: duplicate check
            nontemplate_env_[e->get_qualified_name()] = e;
        }

    private:
        std::unordered_map<native_string_type, env_base_pointer> nontemplate_env_;
        std::unordered_map<native_string_type, template_set_environment_ptr> template_env_;

        //
        environment_process_progress_t progress_;

        //
        environment_id_list_t template_parameter_var_decl_ids_;
    };

} // namespace rill

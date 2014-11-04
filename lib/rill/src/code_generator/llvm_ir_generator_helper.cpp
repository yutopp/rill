//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/behavior/intrinsic_action_holder.hpp>

#include <rill/environment/environment.hpp>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Verifier.h>


namespace rill
{
    namespace code_generator
    {
        auto llvm_ir_generator::is_heavy_object( type const& ty ) const
            -> bool
        {
            auto const& c_env
                = root_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    ty.class_env_id
                    );

            return ( c_env->has_attribute( attribute::decl::k_structured ) || c_env->is_array() )
                && ty.attributes.lifetime == attribute::lifetime_kind::k_scoped
                ;
        }

        auto llvm_ir_generator::type_id_to_llvm_type_ptr( type_id_t const& type_id )
            -> llvm::Type*
        {
            std::cout << "T ID: "  << type_id << std::endl;
            auto const& ty = root_env_->get_type_at( type_id );
            auto const& type_class_env_id = ty.class_env_id;
            auto const& type_attr = ty.attributes;

            auto const& c_env
                = cast_to<class_symbol_environment const>(
                    root_env_->get_env_at_as_strong_ref( type_class_env_id )
                    );
            assert( c_env != nullptr );
            if ( !context_->env_conversion_table.is_defined( type_class_env_id ) ) {
                dispatch( c_env->get_related_ast(), c_env );
            }

            std::cout << "class env id: " << type_class_env_id<< " / " << c_env->get_base_name() << std::endl
                      << "is_heavy" << is_heavy_object( ty ) << std::endl;;
            llvm::Type* llvm_ty
                = context_->env_conversion_table.ref_type( type_class_env_id );

            if ( is_heavy_object( ty ) ) {
                // if heavy type, argument is always passed by pointer
                return llvm_ty->getPointerTo();

            } else {
                switch( type_attr.modifiability )
                {
                case attribute::modifiability_kind::k_mutable:
                    switch( type_attr.quality ) {
                    case attribute::holder_kind::k_val:
                        return llvm_ty;

                    case attribute::holder_kind::k_ref:
                        return llvm_ty->getPointerTo();

                    default:
                        assert( false && "[ice]" );
                        return nullptr;
                    }

                case attribute::modifiability_kind::k_const:
                case attribute::modifiability_kind::k_immutable:
                    return llvm_ty;

                default:
                    assert( false && "[ice]" );
                    return nullptr;
                }
            }
        }

        // convert llvm::Value type to
        // in LLVM, the structure type is treated as pointer type
        // conversion for pass value to function
        auto llvm_ir_generator::convert_value_by_attr(
            type const& target_type,
            type const& source_type,
            llvm::Value* const source_value
            ) -> llvm::Value*
        {
            auto const& source_c_env
                = cast_to<class_symbol_environment const>(
                    root_env_->get_env_strong_at( source_type.class_env_id )
                    );

            // if the target type has not been instanced, do instantiation.
            auto const& target_c_env
                = cast_to<class_symbol_environment const>(
                    root_env_->get_env_strong_at( target_type.class_env_id )
                    );
            if ( !context_->env_conversion_table.is_defined( target_type.class_env_id ) ) {
                dispatch( target_c_env->get_related_ast(), target_c_env );
            }

            bool const is_heavy_object_source = is_heavy_object( source_type );

            // represented as pointer
            bool const is_loadable
                = is_heavy_object_source
                || ( context_->represented_as_pointer_set.count( source_value ) == 1 )
                || ( source_type.attributes.quality == attribute::holder_kind::k_ref && is_heavy_object_source )
                || ( source_type.attributes.modifiability == attribute::modifiability_kind::k_mutable )
                ;

            bool const is_heavy_object_target = is_heavy_object( target_type );

            // represented as pointer
            bool const arg_is_pointer
                = is_heavy_object_target
                || ( target_type.attributes.quality == attribute::holder_kind::k_ref
                     && target_type.attributes.modifiability == attribute::modifiability_kind::k_mutable )
                ;

            // param pointer is represented by value
            bool const load_required
                = !arg_is_pointer;

            std::cout << "===="
                      << "from: " << source_c_env->get_base_name() << std::endl
                      << source_type.attributes
                      << "  h?: " << is_heavy_object_source << std::endl
                      << "to  : " << target_c_env->get_base_name() << std::endl
                      << target_type.attributes
                      << "  h?: " << is_heavy_object_target << std::endl
                      << "arg?: " << arg_is_pointer << std::endl;

            std::cout << "is_loadable   : " << is_loadable << std::endl
                      << "load_required : " << load_required << std::endl;

            if ( load_required && is_loadable ) {
                return context_->ir_builder.CreateLoad( source_value );
            } else {
                return source_value;
            }
        }

    } // namespace code_generator
} // namespace rill

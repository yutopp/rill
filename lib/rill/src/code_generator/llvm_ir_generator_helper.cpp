//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/behavior/intrinsic_function_holder.hpp>

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
                = root_env_->get_env_at_as_strong_ref<class_symbol_environment const>( ty.class_env_id );

            return c_env->has_metatype( class_metatype::structured )
                && ty.attributes.lifetime == attribute::lifetime_kind::k_scoped;
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

            std::cout << "class env id: " << type_class_env_id<< " / " << c_env->get_base_name() << std::endl;
            llvm::Type* llvm_ty
                = context_->env_conversion_table.ref_type( type_class_env_id );

            if ( c_env->has_metatype( class_metatype::structured ) ) {
                // if type is "structed", argument is always passed by pointer
                return llvm_ty->getPointerTo();

            } else {
                //
                switch( type_attr.quality )
                {
                case attribute::holder_kind::k_val:
                    return llvm_ty;

                case attribute::holder_kind::k_ref:
                    return llvm_ty->getPointerTo();

                default:
                    assert( false && "[ice]" );
                    return nullptr;
                }
            }
        }

        // convert llvm::Value type to
        // in LLVM, the structure type is treated as pointer type
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

            //
            auto const is_loadable
                = source_c_env->has_metatype( class_metatype::structured )
                || ( context_->represented_as_pointer_set.count( source_value ) == 1 )
                || ( source_type.attributes.quality == attribute::holder_kind::k_ref )
                || ( source_type.attributes.modifiability == attribute::modifiability_kind::k_mutable )
                ;

            auto const load_required
                = ( target_type.attributes.quality == attribute::holder_kind::k_val && !target_c_env->has_metatype( class_metatype::structured ) )
                ;

            if ( load_required && is_loadable ) {
                return context_->ir_builder.CreateLoad( source_value );
            } else {
                return source_value;
            }
        }

    } // namespace code_generator
} // namespace rill

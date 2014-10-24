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

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
#include <llvm/Analysis/Verifier.h>


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
            ) const -> llvm::Value*
        {
            // if the target type has not been instanced, do instantiation.
            if ( !context_->env_conversion_table.is_defined( target_type.class_env_id ) ) {
                auto const& c_env = root_env_->get_env_strong_at( target_type.class_env_id );
                dispatch( c_env->get_related_ast(), c_env );
            }


            // swiched with TARGET type
            switch( target_type.attributes.quality )
            {
            // convert to the VAL attribute from...
            case attribute::quality_kind::k_val:
            {
                switch( source_type.attributes.quality ) {
                // from VAL
                case attribute::quality_kind::k_val:
                    return source_value;

                // from ref
                case attribute::quality_kind::k_ref:
                    return context_->ir_builder.CreateLoad( source_value );

                default:
                    assert( false && "[[ice]]" );
                }
            }

            // convert to the REF attribute from...
            case attribute::quality_kind::k_ref:
                switch( source_type.attributes.quality ) {
                // from VAL
                case attribute::quality_kind::k_val:
                    //assert( false && "[[ice]] not implemented" );
                    // FIXME: maybe wrong..., use GEP
                    return source_value;

                // from ref
                case attribute::quality_kind::k_ref:
                    assert( false && "[[ice]] not implemented" );

                default:
                    assert( false && "[[ice]]" );
                }

            // convert to the unknown attribute...
            default:
                assert( false && "[ice]" );
                break;
            }

            return nullptr;
        }


    } // namespace code_generator
} // namespace rill

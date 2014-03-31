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
#if ( LLVM_MAJOR_VERSION == 3 && LLVM_MINOR_VERSION >= 5 )
# include <llvm/IR/Verifier.h>
#else
# include <llvm/Analysis/Verifier.h>
#endif


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
            auto const& target_c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    root_env_->get_env_strong_at( target_type.class_env_id )
                    );
            if ( !context_->env_conversion_table.is_defined( target_type.class_env_id ) ) {
                dispatch( target_c_env->get_related_ast(), target_c_env );
            }

            auto const& f_attr
                = attribute::flat_attributes_conbination(
                    source_type.attributes,
                    target_type.attributes
                    );


#define ababa( nn ) \
            case attribute::flatten_attribute::nn:          \
                std::cout << "--> " << #nn << std::endl;    \
                break;

            switch( f_attr )
            {
                //
                ababa( from_val_immutable_to_val_immutable )
                ababa( from_val_immutable_to_val_const )
                ababa( from_val_immutable_to_val_mutable )
                ababa( from_val_immutable_to_ref_immutable )
                ababa( from_val_immutable_to_ref_const )
                ababa( from_val_immutable_to_ref_mutable )

                ababa( from_val_const_to_val_immutable )
                ababa( from_val_const_to_val_const )
                ababa( from_val_const_to_val_mutable )
                ababa( from_val_const_to_ref_immutable )
                ababa( from_val_const_to_ref_const )
                ababa( from_val_const_to_ref_mutable )

                ababa( from_val_mutable_to_val_immutable )
                ababa( from_val_mutable_to_val_const )
                ababa( from_val_mutable_to_val_mutable )
                ababa( from_val_mutable_to_ref_immutable )
                ababa( from_val_mutable_to_ref_const )
                ababa( from_val_mutable_to_ref_mutable )


                ababa( from_ref_immutable_to_val_immutable )
                ababa( from_ref_immutable_to_val_const )
                ababa( from_ref_immutable_to_val_mutable )
                ababa( from_ref_immutable_to_ref_immutable )
                ababa( from_ref_immutable_to_ref_const )
                ababa( from_ref_immutable_to_ref_mutable )

                ababa( from_ref_const_to_val_immutable )
                ababa( from_ref_const_to_val_const )
                ababa( from_ref_const_to_val_mutable )
                ababa( from_ref_const_to_ref_immutable )
                ababa( from_ref_const_to_ref_const )
                ababa( from_ref_const_to_ref_mutable )

                ababa( from_ref_mutable_to_val_immutable )
                ababa( from_ref_mutable_to_val_const )
                ababa( from_ref_mutable_to_val_mutable )
                ababa( from_ref_mutable_to_ref_immutable )
                ababa( from_ref_mutable_to_ref_const )
                ababa( from_ref_mutable_to_ref_mutable )

                ababa( unknown )
            }


            switch( f_attr )
            {
            case attribute::flatten_attribute::from_val_immutable_to_val_immutable:
            case attribute::flatten_attribute::from_val_immutable_to_val_const:
            case attribute::flatten_attribute::from_val_immutable_to_val_mutable:
                if ( context_->represented_as_pointer_set.count( source_value ) == 1 ) {
                    // TODO: copy constructor
                    return context_->ir_builder.CreateLoad( source_value );
                } else {
                    return source_value;
                }

            case attribute::flatten_attribute::from_val_immutable_to_ref_immutable:
            case attribute::flatten_attribute::from_val_immutable_to_ref_const:
            case attribute::flatten_attribute::from_val_immutable_to_ref_mutable:
                return source_value;

            case attribute::flatten_attribute::from_val_const_to_val_immutable:
            case attribute::flatten_attribute::from_val_const_to_val_const:
            case attribute::flatten_attribute::from_val_const_to_val_mutable:
            case attribute::flatten_attribute::from_val_const_to_ref_immutable:
            case attribute::flatten_attribute::from_val_const_to_ref_const:
            case attribute::flatten_attribute::from_val_const_to_ref_mutable:
                assert( false );


            case attribute::flatten_attribute::from_val_mutable_to_val_immutable:
                // struct is always represented as pointer
                if ( target_c_env->has( class_attribute::structed ) ) {
                    // TODO: impl copy constructor
                    return source_value;

                } else {
                    // TODO: copy constructor
                    if ( context_->represented_as_pointer_set.count( source_value ) == 1 ) {
                        return source_value;
                    } else {
                        return context_->ir_builder.CreateLoad( source_value );
                    }
                }

            case attribute::flatten_attribute::from_val_mutable_to_val_const:
            case attribute::flatten_attribute::from_val_mutable_to_val_mutable:
                source_value->dump();
                // struct is always represented as pointer
                if ( target_c_env->has( class_attribute::structed ) ) {
                    return source_value;

                    return context_->ir_builder.CreateLoad( source_value );

                } else {
                    if ( context_->represented_as_pointer_set.count( source_value ) == 1 ) {
                        // TODO: copy constructor
                        //return source_value;
                        return context_->ir_builder.CreateLoad( source_value );
                    } else {
                        return context_->ir_builder.CreateLoad( source_value );
                    }
                }


            case attribute::flatten_attribute::from_val_mutable_to_ref_immutable:
            case attribute::flatten_attribute::from_val_mutable_to_ref_const:
            case attribute::flatten_attribute::from_val_mutable_to_ref_mutable:
                return source_value;


            //
            case attribute::flatten_attribute::from_ref_immutable_to_val_immutable:
            case attribute::flatten_attribute::from_ref_immutable_to_val_const:
            case attribute::flatten_attribute::from_ref_immutable_to_val_mutable:
                assert( false && "not supoprted" );

            case attribute::flatten_attribute::from_ref_immutable_to_ref_immutable:
            case attribute::flatten_attribute::from_ref_immutable_to_ref_const:
            case attribute::flatten_attribute::from_ref_immutable_to_ref_mutable:
                return source_value;

            case attribute::flatten_attribute::from_ref_const_to_val_immutable:
            case attribute::flatten_attribute::from_ref_const_to_val_const:
            case attribute::flatten_attribute::from_ref_const_to_val_mutable:
                assert( false && "not supoprted" );

            case attribute::flatten_attribute::from_ref_const_to_ref_immutable:
            case attribute::flatten_attribute::from_ref_const_to_ref_const:
            case attribute::flatten_attribute::from_ref_const_to_ref_mutable:
                assert( false );

            case attribute::flatten_attribute::from_ref_mutable_to_val_immutable:
            case attribute::flatten_attribute::from_ref_mutable_to_val_const:
            case attribute::flatten_attribute::from_ref_mutable_to_val_mutable:
                assert( false );

            case attribute::flatten_attribute::from_ref_mutable_to_ref_immutable:
            case attribute::flatten_attribute::from_ref_mutable_to_ref_const:
            case attribute::flatten_attribute::from_ref_mutable_to_ref_mutable:
                return source_value;
            }

            // failed...
            assert( false && "[[ice]]" );
            return nullptr;
        }


    } // namespace code_generator
} // namespace rill

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/behavior/intrinsic_function_holder.hpp>
#include <rill/semantic_analysis/analyzer.hpp>

#include <rill/environment/environment.hpp>

#include <iterator>
#include <cstdint>

#include <boost/scope_exit.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/join.hpp>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Analysis/Verifier.h>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


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



            switch( target_type.attributes.quality )
            {
            case attribute::quality_kind::k_val:
            {
                // TODO: implement
                if ( source_value->getType()->isPointerTy() ) {
                    return context_->ir_builder.CreateLoad( source_value );
                } else {
                    return source_value;
                }
            }

            case attribute::quality_kind::k_ref:
                // TODO: implement
                if ( source_value->getType()->isPointerTy() ) {
                    return source_value;
                } else {
                    assert( false && "[ice]" );
                    return source_value;
                }

            default:
                assert( false && "[ice]" );
                break;
            }

            return nullptr;
        }


    } // namespace code_generator
} // namespace rill

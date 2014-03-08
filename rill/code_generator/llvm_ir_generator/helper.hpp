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
        template<typename Px, typename EnvPtr>
        auto llvm_ir_generator::eval_args(
            Px const& parameter_type_ids,
            ast::expression_list const& arguments,
            EnvPtr const& parent_env
            ) const
            -> std::vector<llvm::Value*>
        {
            std::vector<llvm::Value*> args( arguments.size() );

            // evaluate argument from last to front(but ordering of vector is from front to last)
            for( std::size_t i=0; i<arguments.size(); ++i ) {
                auto const reversed_i = arguments.size()-i-1;

                auto const& parameter_type
                    = root_env_->get_type_at( parameter_type_ids[reversed_i] );
                auto const arg_type
                    = root_env_->get_type_at(
                        root_env_->get_related_type_id_by_ast_ptr(
                            arguments[reversed_i]
                            )
                        );
                auto const arg_value
                    = dispatch( arguments[reversed_i], parent_env );
                assert( arg_value != nullptr );

                auto const& result_value = convert_value_by_attr(
                    parameter_type,
                    arg_type,
                    arg_value
                    );

                args[reversed_i] = result_value;
            }

            return args;
        }


        // create new Value holder
        template<typename EnvPtr>
        auto llvm_ir_generator::store_value(
            type const& value_ty,
            llvm::Value* const value,
            EnvPtr const& v_env
            ) const
            -> void
        {
            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    root_env_->get_env_strong_at( value_ty.class_env_id )
                    );
            auto const& variable_attr = value_ty.attributes;

            auto const& variable_llvm_type = value->getType();

            if ( c_env->has( class_attribute::structed ) ) {
                llvm::AllocaInst* const allca_inst
                    = context_->ir_builder.CreateAlloca(
                        variable_llvm_type,
                        0/*length of array*/
                        );
                if ( value ) {
                    context_->ir_builder.CreateStore(
                        value,
                        allca_inst /*, is_volatile */
                        );
                }
                context_->env_conversion_table.bind_value(
                    v_env->get_id(),
                    allca_inst
                    );

            } else {
                //

                switch( variable_attr.quality )
                {
                    // VAL
                case attribute::quality_kind::k_val:
                    switch( variable_attr.modifiability )
                    {
                    case attribute::modifiability_kind::k_immutable:
                    {
                        context_->env_conversion_table.bind_value(
                            v_env->get_id(),
                            value
                            );
                    }
                    break;

                    case attribute::modifiability_kind::k_const:
                        assert( false && "[ice] notsuported" );
                        break;

                    case attribute::modifiability_kind::k_mutable:
                    {
                        // FIXME:
                        llvm::AllocaInst* const allca_inst
                            = context_->ir_builder.CreateAlloca(
                                variable_llvm_type,
                                0/*length*/
                                );
                        if ( value ) {
                            context_->ir_builder.CreateStore(
                                value,
                                allca_inst /*, is_volatile */
                                );
                        }

                        context_->env_conversion_table.bind_value(
                            v_env->get_id(),
                            allca_inst
                            );
                    }
                    break;
                    }

                    break;

                    // REF
                case attribute::quality_kind::k_ref:
                    assert( false && "not implemented..." );
                    break;

                default:
                    assert( false && "[ice]" );
                    break;
                }
            }
        }

    } // namespace code_generator
} // namespace rill

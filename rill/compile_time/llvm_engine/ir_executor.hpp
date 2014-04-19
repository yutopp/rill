//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_LLVM_ENGINE_IR_EXECUTOR_HPP
#define RILL_COMPILE_TIME_LLVM_ENGINE_IR_EXECUTOR_HPP

#include <memory>
#include <cstdint>

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>

#include "../../ast/visitor.hpp"

#include "../../code_generator/llvm_ir_generator.hpp"
#include "../../code_generator/llvm_engine/support.hpp"
#include "../../semantic_analysis/type_detail.hpp"

#include "../../behavior/intrinsic_function_holder.hpp"

#include "engine_value_holder.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            // this class holds llvm_ir_generator and llvm_ir_execute_engine
            class ir_executor RILL_CXX11_FINAL
                : public ast::readonly_ast_visitor<ir_executor, void*>
            {
                friend code_generator::llvm_ir_generator;

            public:
                ir_executor(
                    const_environment_base_ptr const&,
                    std::shared_ptr<code_generator::llvm_ir_generator> const&,
                    std::shared_ptr<llvm::ExecutionEngine> const&,
                    std::shared_ptr<semantic_analysis::type_detail_pool_t> const&
                    );

            public:
                RILL_VISITOR_READONLY_OP_DECL( ast::binary_operator_expression );
                RILL_VISITOR_READONLY_OP_DECL( ast::type_expression );
                //RILL_VISITOR_READONLY_OP_DECL( ast::term_expression );

                RILL_VISITOR_READONLY_OP_DECL( ast::identifier_value_base );

                RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::int32_value );
                RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::boolean_value );
                RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::string_value );
                RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::array_value );

                RILL_VISITOR_OP_FAIL

            public:
                // TODO: fix this...
                auto value_holder() const
                    -> std::shared_ptr<engine_value_holder>
                {
                    return value_holder_;
                }

            private:
                auto make_storage( std::size_t const& size, std::size_t const& align ) const
                    -> void*;

                template<typename T,typename... Args>
                auto make_object( Args&&... args ) const
                    -> void*
                {
                    auto storage = make_storage( sizeof( T ), alignof( T ) );
                    new( storage ) T( std::forward<Args>( args )... );

                    return storage;
                }

            private:
                const_environment_base_ptr root_env_;
                std::shared_ptr<code_generator::llvm_ir_generator> ir_generator_;
                std::shared_ptr<llvm::ExecutionEngine> execution_engine_;

                std::shared_ptr<engine_value_holder> value_holder_;
                std::shared_ptr<semantic_analysis::type_detail_pool_t> type_detail_pool_;
            };

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_IR_EXECUTOR_HPP*/

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_CTFE_ENGINE_HPP
#define RILL_COMPILE_TIME_CTFE_ENGINE_HPP

#include <memory>
#include <cstdint>

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include "../ast/detail/tree_visitor_base.hpp"

#include "../behavior/intrinsic_function_holder.hpp"
#include "../code_generator/llvm_ir_generator.hpp"
#include "../semantic_analysis/type_detail.hpp"

#include "engine_value_holder.hpp"


namespace rill
{
    namespace compile_time
    {
        // this class holds llvm_ir_generator and llvm_ir_execute_engine
        class ctfe_engine RILL_CXX11_FINAL
            : public ast::detail::const_tree_visitor<ctfe_engine, void*>
        {
        public:
            ctfe_engine(
                std::shared_ptr<code_generator::llvm_ir_generator const> const&,
                std::shared_ptr<llvm::ExecutionEngine> const&,
                std::shared_ptr<semantic_analysis::type_detail_pool_t> const&
                );

        public:
            RILL_TV_OP_DECL_CONST( ast::binary_operator_expression )
            RILL_TV_OP_DECL_CONST( ast::type_expression )
            RILL_TV_OP_DECL_CONST( ast::term_expression )

            RILL_TV_OP_FAIL

        public:
            // TODO: fix this...
            auto value_holder() const
                -> std::shared_ptr<engine_value_holder>
            {
                return value_holder_;
            }


            //
            // friend
            //
            friend class code_generator::llvm_ir_generator;

        private:
            std::shared_ptr<code_generator::llvm_ir_generator const> ir_generator_;
            std::shared_ptr<llvm::ExecutionEngine> execution_engine_;

            std::shared_ptr<engine_value_holder> value_holder_;
            std::shared_ptr<semantic_analysis::type_detail_pool_t> type_detail_pool_;
        };


        //
        template<
            typename AnalyzerVisitorPtr,
            typename EnvironmentPtr,
            typename ActionHolderPtr,
            typename TypeDetailPoolPtr
            >
        auto make_ctfe_engine(
            AnalyzerVisitorPtr const& analyzer_visitor,
            EnvironmentPtr const& env,
            ActionHolderPtr const& action_holder,
            TypeDetailPoolPtr const& type_detail_pool
            )
            -> std::shared_ptr<ctfe_engine const>
        {
            // first, create llvm_ir_generator
            auto const& context
                = std::make_shared<code_generator::llvm_ir_generator_context>();

            action_holder->invoke_initialize_action(
                processing_context::k_llvm_ir_generator,
                context,
                env
                );

            auto const& ir_generator
                = std::make_shared<code_generator::llvm_ir_generator>(
                    env,
                    action_holder,
                    context,
                    analyzer_visitor
                    )
                ;

            // second, create llvm JIT Engine
            // the ownership of context.llvm_module is moved to engine.
            // FIXME: the destructor of llvm::ExecutionEngine will execute "delete ptr to module", so this "&context->llvm_module" can be dangerous. MUST BE CHANGED.(cause Undefined behaviour)
            auto const engine
                = std::shared_ptr<llvm::ExecutionEngine>(
                    llvm::EngineBuilder( &context->llvm_module ).create() \
                    );

            return std::make_shared<ctfe_engine const>( ir_generator, engine, type_detail_pool );
        }


    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_CTFE_ENGINE_HPP*/

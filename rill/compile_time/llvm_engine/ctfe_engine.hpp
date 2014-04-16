//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_LLVM_ENGINE_CTFE_ENGINE_HPP
#define RILL_COMPILE_TIME_LLVM_ENGINE_CTFE_ENGINE_HPP

#include "ir_executor.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            // this class holds llvm_ir_generator and llvm_ir_execute_engine
            class ctfe_engine
            {
            public:
                ctfe_engine(
                    const_environment_base_ptr const& root_env,
                    std::shared_ptr<code_generator::llvm_ir_generator const> const& gen,
                    std::shared_ptr<llvm::ExecutionEngine> const& llvm_engine,
                    std::shared_ptr<semantic_analysis::type_detail_pool_t> const& type_pool
                    )
                    : executor_( root_env, gen, llvm_engine, type_pool )
                {}

            public:
                template<typename... Args>
                auto execute( Args&&... args )
                    -> decltype( std::declval<ir_executor>().dispatch( std::forward<Args>( args )... ) )
                {
                    return executor_.dispatch( std::forward<Args>( args )... );
                }

            public:
                // TODO: fix this...
                auto value_holder() const
                    -> std::shared_ptr<engine_value_holder const>
                {
                    return executor_.value_holder();
                }

            private:
                ir_executor executor_;
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
                -> std::shared_ptr<ctfe_engine>
            {
                // make target machine
                // the function "make_default_target_machine" initializes LLVM environment. so, call first
                std::string tm_error_log;
                auto target_machine = code_generator::llvm_engine::make_default_target_machine( tm_error_log );
                if ( target_machine == nullptr ) {
                    std::cerr << "" << tm_error_log << std::endl;
                    assert( false );
                }

                // create llvm_ir_generator
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
                        );

                // second, create llvm JIT Engine
                // the ownership of context.llvm_module is moved to engine.
                // FIXME: the destructor of llvm::ExecutionEngine will execute "delete ptr to module", so this "&context->llvm_module" can be dangerous. MUST BE CHANGED.(cause Undefined behaviour)
                std::string jit_engine_error_log;
                auto const engine
                    = std::shared_ptr<llvm::ExecutionEngine>(
                        llvm::EngineBuilder( &context->llvm_module ).setErrorStr( &jit_engine_error_log ).create()
                        );
                if ( engine == nullptr ) {
                    std::cerr << "failed to create JIT engine. " << jit_engine_error_log << std::endl;
                    assert( false );
                }

                return std::make_shared<ctfe_engine>( env, ir_generator, engine, type_detail_pool );
            }

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_CTFE_ENGINE_HPP*/

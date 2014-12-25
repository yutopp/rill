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
#include "value_converter.hpp"
#include "bridge.hpp"

#include "../../semantic_analysis/analyzer_fwd.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            // this class holds llvm_ir_generator and llvm_ir_execute_engine
            class ctfe_engine
            {
                friend semantic_analysis::analyzer;

            public:
                ctfe_engine(
                    global_environment_ptr const& g_env,
                    std::shared_ptr<code_generator::llvm_ir_generator> const& gen,
                    std::shared_ptr<llvm::ExecutionEngine> const& llvm_engine,
                    std::shared_ptr<type_detail_factory> const& type_factory,
                    semantic_analysis::analyzer* const sa
                    )
                    : executor_( g_env, gen, llvm_engine, type_factory )
                {
                    jit_execution_environmant const je = {
                        sa
                    };

                    set_global_jit_execution_environment( je );
                }

                ~ctfe_engine()
                {
                    clear_global_jit_execution_environment();
                }

            public:
                template<typename... Args>
                auto execute_as_raw_storage( Args&&... args )
                    -> decltype( std::declval<ir_executor>().dispatch( std::forward<Args>( args )... ) )
                {
                    return executor_.dispatch( std::forward<Args>( args )... );
                }

                template<typename... Args>
                auto execute_as_llvm_value( Args&&... args )
                    -> llvm::Value*
                {
                    return execute_as_raw_storage( std::forward<Args>( args )... );
                }

            public:
                // TODO: fix this...
                auto value_holder() const
                    -> std::shared_ptr<engine_value_holder const>
                {
                    return executor_.value_holder();
                }

            private:
                auto value_holder()
                    -> std::shared_ptr<engine_value_holder>
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

                // TODO: fix it...
                // create module
                // it has no owner ship..., because the custom deleter does NOT delete resource.
                std::shared_ptr<llvm::Module> module( new llvm::Module( "rill_ctfe", llvm::getGlobalContext() ), []( llvm::Module* ){} );

                // create llvm_ir_generator
                auto const& context
                    = std::make_shared<code_generator::llvm_ir_generator_context>( module );

                auto const& ir_generator
                    = std::make_shared<code_generator::llvm_ir_generator>(
                        env,
                        action_holder,
                        context,
                        analyzer_visitor
                        );

                // second, create llvm JIT Engine
                // delegate owner ship of module to Engine
                std::string jit_engine_error_log;
                auto const engine
                    = std::shared_ptr<llvm::ExecutionEngine>(
                        llvm::EngineBuilder( module.get() ).setErrorStr( &jit_engine_error_log ).create()
                        );
                if ( engine == nullptr ) {
                    std::cerr << "failed to create JIT engine. " << jit_engine_error_log << std::endl;
                    assert( false );
                }

                //
                engine->DisableSymbolSearching();

                return std::make_shared<ctfe_engine>(
                    env,
                    ir_generator,
                    engine,
                    type_detail_pool,
                    analyzer_visitor
                    );
            }

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_CTFE_ENGINE_HPP*/

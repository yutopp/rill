#pragma once

#include <memory>


namespace rill
{
    namespace processing_context
    {
        class debug_interpreter_tag {};
        auto const debug_interpreter_k = debug_interpreter_tag();

        class llvm_ir_generator_tag {};
        auto const llvm_ir_generator_k = llvm_ir_generator_tag();
    };

    class embedded_function_action_base;
    typedef std::shared_ptr<embedded_function_action_base>   embedded_function_action_base_ptr;
    typedef std::shared_ptr<embedded_function_action_base const>   const_embedded_function_action_base_ptr;

    typedef std::size_t embedded_function_action_id_t;

    class embedded_function_holder;
    typedef std::shared_ptr<embedded_function_holder>   embedded_function_holder_ptr;
};

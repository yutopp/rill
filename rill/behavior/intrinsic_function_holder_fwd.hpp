//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_FWD_HPP
#define RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_FWD_HPP

#include <memory>


namespace rill
{
    namespace processing_context
    {
        class debug_interpreter_tag {};
        auto const k_debug_interpreter = debug_interpreter_tag();

        class llvm_ir_generator_tag {};
        auto const k_llvm_ir_generator = llvm_ir_generator_tag();
    } // namespace processing_context

    class intrinsic_function_action_base;
    typedef std::shared_ptr<intrinsic_function_action_base>         intrinsic_function_action_base_ptr;
    typedef std::shared_ptr<intrinsic_function_action_base const>   const_intrinsic_function_action_base_ptr;


    typedef std::size_t intrinsic_function_action_id_t;


    class intrinsic_function_action_holder;
    typedef std::shared_ptr<intrinsic_function_action_holder>       intrinsic_function_action_holder_ptr;

} // namespace rill

#endif /*RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_FWD_HPP*/


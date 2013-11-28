//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_BINARY_GENERATOR_FROM_LLVM_IR_HPP
#define RILL_CODE_GENERATOR_BINARY_GENERATOR_FROM_LLVM_IR_HPP

#include "llvm_ir_generator.hpp"


namespace rill
{
    namespace code_generator
    {
        class binary_generator_from_llvm_ir
        {
        public:
            binary_generator_from_llvm_ir( llvm_ir_generator_context_ptr const& context )
                : context_( context )
            {}

        public:
            void test() const;

        private:
            llvm_ir_generator_context_ptr const& context_;
        };
    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_BINARY_GENERATOR_FROM_LLVM_IR_HPP*/

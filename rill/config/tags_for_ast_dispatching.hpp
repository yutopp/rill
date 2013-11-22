//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include "../environment_fwd.hpp"
#include "../ast/value_fwd.hpp"

#include <llvm/IR/Value.h>


// * IMPORTANT *
// used by tag dispatching of AST
// if you would like to visitor pattern, append ( TagName, ReturnType ) to this.
#define RILL_DISPATCH_TYPES_SEQ \
    (( dispatch_as_environment_tag,     environment_ptr )) \
    (( dispatch_as_value_tag,           ast::value_ptr )) \
    (( dispatch_as_type_tag,            ast::intrinsic::identifier_value_ptr )) \
    (( dispatch_as_llvm_ir_value_tag,   llvm::Value* ))

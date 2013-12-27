//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include "../environment/environment_fwd.hpp"
#include "../environment/type_registry_fwd.hpp"
#include "../ast/value_fwd.hpp"
#include "../semantic_analysis/type_info.hpp"
#include <llvm/IR/Value.h>
#include <llvm/ExecutionEngine/GenericValue.h>


// * IMPORTANT *
// used by tag dispatching of AST
// if you would like to visitor pattern, append ( TagName, ReturnType ) to this.
#define RILL_DISPATCH_TYPES_SEQ \
    (( dispatch_as_environment_tag,         environment_base_ptr )) \
    (( dispatch_as_value_tag,               ast::value_ptr )) \
    (( dispatch_as_type_value_tag,          semantic_analysis::type_info )) \
    (( dispatch_as_type_id_with_env_tag,    semantic_analysis::type_id_with_env )) \
    (( dispatch_as_llvm_ir_value_tag,       llvm::Value* )) \
    (( dispatch_as_llvm_generic_value_tag,  llvm::GenericValue )) \
    (( dispatch_as_void_tag,                void ))

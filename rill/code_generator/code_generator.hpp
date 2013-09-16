//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_HPP
#define RILL_CODE_GENERATOR_HPP

#include <memory>

#include "llvm_ir_generator.hpp"


namespace rill
{
    namespace code_generator
    {
        template<typename EnvironmentPtr, typename ActionHolderPtr, typename Node>
        auto generate_llvm_ir( EnvironmentPtr const& env, ActionHolderPtr const& holder, std::shared_ptr<Node> const& node )
            -> void
        {
            llvm_ir_generator const visitor( env );

            visitor.dispatch( node, env );
            visitor.debug();
        }
    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_HPP*/
//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//


//
// Compile time interpreter
// runner
//


#include <rill/semantic_analysis/compiletime_interpreter/interpreter.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>

#include <rill/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            //
            //
            RILL_TV_OP( type_evaluator, ast::type_identifier_expression_ptr, e, env )
            {
                return e->value_;
            }

            RILL_TV_OP( type_evaluator, ast::compiletime_return_type_expression_ptr, e, env )
            {
                // !!! Unimplemented !!!
                // TODO: evaluate by value as constant and cast to type identifiern
                throw -1;
                return nullptr;
            }
        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill
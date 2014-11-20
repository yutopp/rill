//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment/global_environment.hpp>
#include <rill/environment/root_environment.hpp>
#include <rill/environment/environment.hpp>


#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    auto environment_unit::connect_from_ast( ast::const_ast_base_ptr const& ast )
        -> void
    {
        rill_dout << "connect_from ast_id: " << ast->get_id()
                  << " -> env_id: " << get_id() << std::endl;

        b_.lock()->connect_from_ast( ast, shared_from_this() );
    }

    auto environment_unit::connect_to_ast( ast::statement_ptr const& ast )
        -> void
    {
        rill_dout << "connect_to env_id: " << get_id()
                  << " -> ast_id: " << ast->get_id() << std::endl;

        b_.lock()->connect_to_ast( get_id(), ast );
    }

    auto environment_unit::get_related_ast()
        -> ast::statement_ptr
    {
        return b_.lock()->get_related_ast( get_id() );
    }

    auto environment_unit::get_related_ast() const
        -> ast::const_statement_ptr
    {
        return b_.lock()->get_related_ast( get_id() );
    }

} // namespace rill

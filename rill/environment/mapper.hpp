//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_MAPPER
#define RILL_ENVIRONMENT_MAPPER

#include <unordered_map>
#include <memory>

#include "../environment/environment_fwd.hpp"

#include "../ast/statement_fwd.hpp"
#include "../ast/detail/dispatch_assets.hpp"


namespace rill
{
    // table[ast_ptr -> env_id]
    class ast_to_environment_mapper
    {
    public:
        typedef std::shared_ptr<void>   key_type;
        typedef environment_id_t        value_type;

    private:
        std::unordered_map<key_type, environment_id_t> map_;
    };


    // table[env_id -> {env_id, ast_ptr}]
    class environment_to_asts_mapper
    {
    public:
        typedef environment_id_t        key_type;
        struct value_type
        {
            environment_id_t env_id_on_time;
            std::shared_ptr<boost::type_erasure::any<ast::detail::dispatcher_concept<ast::statement>>> statement_env_;
        };

    private:
        std::unordered_multimap<key_type, environment_id_t> map_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_MAPPER*/

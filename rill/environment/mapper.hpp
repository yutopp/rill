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

#include <boost/type_erasure/any.hpp>

#include "../environment_fwd.hpp"
#include "../ast/detail/macros.hpp"


namespace rill
{
    class ast_to_environment_mapper
    {
    public:
        typedef environment_id_t value_type;
    };

    class environment_to_asts_mapper
    {
    public:
        struct value_type
        {
            environment_id_t env_id_on_time;
            std::shared_ptr<boost::type_erasure::any<ast::detail::dispatcher_concept<void>>> statement_env_;
        };
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_MAPPER*/

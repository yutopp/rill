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

#include "../../environment_fwd.hpp"

#include "../../ast/statement.hpp"
#include "../../ast/detail/dispatch_assets.hpp"


namespace rill
{
    // table[ast_ptr -> env_id]
    class ast_to_environment_mapper
    {
    public:
        typedef std::shared_ptr<void>   key_type;
        typedef environment_id_t        value_type;

    public:
        template<typename Ptr, typename Id>
        auto add( Ptr const& ast_ptr, Id const& env_id )
            -> void
        {
            map_.emplace( ast_ptr, env_id );
        }

    private:
        std::unordered_map<key_type, environment_id_t> map_;
    };


    struct _os : boost::type_erasure::placeholder {};
template<class T, class U = boost::type_erasure::_self>
struct base_and_derived
{
    static std::shared_ptr<T> apply(U const& arg) { return arg; }
};
typedef boost::mpl::vector<
        base_and_derived<ast::statement, _os>
    > requirements;




    // table[env_id -> {env_id, ast_ptr}]
    class environment_to_asts_mapper
    {
    public:
        typedef environment_id_t        key_type;
        struct value_type
        {
            environment_id_t env_id_on_time;
            ast::statement_ptr statement_ast_;
        };
        typedef std::unordered_multimap<key_type, value_type> container_type;
        typedef container_type::const_iterator const_iterator_type;
        typedef std::pair<container_type::const_iterator, container_type::const_iterator> const_iterator_pair_type;
    public:
        template<typename Id, typename AstPtr>
        auto add( Id const& env_id, Id const& env_id_on_time, AstPtr const& ast_ptr )
            -> void
        {
            value_type vv = { env_id_on_time, ast_ptr };
            map_.emplace( env_id, vv );
        }

        template<typename Id>
        auto get( Id const& env_id ) const
            -> const_iterator_pair_type
        {
            return map_.equal_range( env_id );
        }

    private:
        container_type map_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_MAPPER*/

//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_INTRINSIC_ACTION_HOLDER_HPP
#define RILL_BEHAVIOR_INTRINSIC_ACTION_HOLDER_HPP

#include <vector>
#include <string>
#include <unordered_map>

#include <boost/optional.hpp>

#include "intrinsic_action_holder_fwd.hpp"
#include "intrinsic_action_base.hpp"


namespace rill
{
    class intrinsic_action_holder
    {
    public:
        template<typename Action>
        auto append()
            -> intrinsic_action_id_t
        {
            auto const next_id = actions_.size();
            actions_.push_back( std::make_shared<Action>() );

            return next_id;
        }

        template<typename Action>
        auto append(
            std::string const& tag_name,
            std::shared_ptr<Action> const& action
            )
            -> intrinsic_action_id_t
        {
            auto const next_id = actions_.size();
            actions_.push_back( action );

            tag_map_[tag_name] = next_id;

            return next_id;
        }

        auto is_registered( std::string const& tag_name )
            -> boost::optional<intrinsic_action_id_t>
        {
            auto const it = tag_map_.find( tag_name );
            if ( it == tag_map_.cend() ) {
                return boost::none;
            }

            return it->second;
        }

        auto at( intrinsic_action_id_t const& id )
            -> intrinsic_action_base_ptr
        {
            return actions_.at( id );
        }

        auto at( intrinsic_action_id_t const& id ) const
            -> const_intrinsic_action_base_ptr
        {
            return actions_.at( id );
        }

    private:
        std::vector<intrinsic_action_base_ptr> actions_;
        std::unordered_map<std::string, intrinsic_action_id_t> tag_map_;
    };

} // namespace rill

#endif /*RILL_BEHAVIOR_INTRINSIC_ACTION_HOLDER_HPP*/

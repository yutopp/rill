//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_INTRINSIC_ACTION_HOLDER_FWD_HPP
#define RILL_BEHAVIOR_INTRINSIC_ACTION_HOLDER_FWD_HPP

#include <memory>


namespace rill
{
    class intrinsic_action_base;
    using intrinsic_action_base_ptr = std::shared_ptr<intrinsic_action_base>;
    using const_intrinsic_action_base_ptr = std::shared_ptr<intrinsic_action_base const>;

    typedef std::size_t intrinsic_action_id_t;

    class intrinsic_action_holder;
    using intrinsic_action_holder_ptr = std::shared_ptr<intrinsic_action_holder>;

} // namespace rill

#endif /*RILL_BEHAVIOR_INTRINSIC_ACTION_HOLDER_FWD_HPP*/

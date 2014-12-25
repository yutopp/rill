//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_TYPE_TYPE_DETAIL_POOL_HPP
#define RILL_TYPE_TYPE_DETAIL_POOL_HPP

//#include <boost/pool/object_pool.hpp>
#include "work_around_object_pool.hpp"


namespace rill
{
    struct type_detail;

    using type_detail_pool_t = boost::object_pool_workarounded<type_detail>;

} // namespace rill

#endif /*RILL_TYPE_TYPE_DETAIL_POOL_HPP*/

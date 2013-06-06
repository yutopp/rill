#pragma once

#include <memory>
#include <boost/shared_ptr.hpp>

class environment;

typedef boost::shared_ptr<environment>            environment_ptr;
typedef boost::shared_ptr<environment const>      const_environment_ptr;
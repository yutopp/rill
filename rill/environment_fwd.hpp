#pragma once

#include <memory>

// forward decleration
class environment;

typedef std::shared_ptr<environment>            environment_ptr;
typedef std::shared_ptr<environment const>      const_environment_ptr;
#pragma once

#include <memory>
#include <vector>

typedef std::size_t    environment_id_t;
auto const environment_id_limit = std::numeric_limits<environment_id_t>::max();
auto const envitonment_id_undefined = environment_id_limit;

typedef std::vector<environment_id_t> environment_id_list;

// forward decleration
class environment;

typedef std::shared_ptr<environment>            environment_ptr;
typedef std::shared_ptr<environment const>      const_environment_ptr;
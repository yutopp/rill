//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_ENGINE_VALUE_HOLDER_HPP
#define RILL_COMPILE_TIME_ENGINE_VALUE_HOLDER_HPP

#include <memory>
#include <unordered_map>
#include <vector>

#include <llvm/IR/DerivedTypes.h>


namespace rill
{
    namespace compile_time
    {
        class engine_value_holder
        {
        public:
            typedef std::shared_ptr<void>   value_type;

        public:
            //
            // Values
            //
            auto bind_value( environment_id_t const& env_id, llvm::Value* const value )
                -> void
            {
                // TODO: dup check
                //value_table_.emplace( env_id, value );
            }


            //
            //
            //
            auto is_defined( environment_id_t const& env_id ) const
                -> bool
            {
                return value_table_.find( env_id ) != value_table_.cend();
            }

        private:
            std::unordered_map<environment_id_t, value_type> value_table_;
        };

    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_ENGINE_VALUE_HOLDER_HPP*/

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
#include <unordered_set>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "../../semantic_analysis/type_detail.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            class engine_value_holder
            {
            public:
                //
                // Values
                //
                auto bind_value( environment_id_t const& env_id, semantic_analysis::type_detail_ptr const& value )
                    -> void
                {
                    //
                    type_detail_table_[env_id] = value;
                }

                auto bind_value( environment_id_t const& env_id, std::shared_ptr<char> const& value )
                    -> void
                {
                    //
                    storage_table_[env_id] = value;
                }


                //
                auto is_defined_in_type_detail( environment_id_t const& env_id ) const
                    -> bool
                {
                    return type_detail_table_.find( env_id ) != type_detail_table_.cend();
                }
                auto is_defined_in_storage( environment_id_t const& env_id ) const
                    -> bool
                {
                    return storage_table_.find( env_id ) != storage_table_.cend();
                }

                auto is_defined( environment_id_t const& env_id ) const
                    -> bool
                {
                    return is_defined_in_type_detail( env_id ) || is_defined_in_storage( env_id );
                }


                //
                auto ref_value( environment_id_t const& env_id ) const
                    -> void*
                {
                    return is_defined_in_type_detail( env_id )
                            ? static_cast<void*>( type_detail_table_.at( env_id ) )
                            : is_defined_in_storage( env_id )
                                ? static_cast<void*>( storage_table_.at( env_id ).get() )
                                : nullptr/*TODO: throw exception*/;
                }


                //
                auto bind_as_temporary( std::shared_ptr<char> const& value )
                    -> void
                {
                    temporary_storages_.insert( value );
                }

            private:
                std::unordered_map<environment_id_t, semantic_analysis::type_detail_ptr> type_detail_table_;
                std::unordered_map<environment_id_t, std::shared_ptr<char>> storage_table_;

                std::unordered_set<std::shared_ptr<char>> temporary_storages_;
            };

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_ENGINE_VALUE_HOLDER_HPP*/

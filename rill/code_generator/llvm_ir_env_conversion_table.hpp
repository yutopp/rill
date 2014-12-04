//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_LLVM_ENV_CONVERSION_TABLE_HPP
#define RILL_CODE_GENERATOR_LLVM_ENV_CONVERSION_TABLE_HPP

#include <memory>
#include <unordered_map>
#include <vector>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>

#include "../environment/class_symbol_environment.hpp"


namespace rill
{
    namespace code_generator
    {
        // consists env_id and llvm_object.
        class env_id_llvm_table
        {
        public:
            //
            // Values
            //
            auto bind_value( environment_id_t const& env_id, llvm::Value* const value, bool const is_alloca_inst = false )
                -> void
            {
                // TODO: dup check
                value_table_.emplace( env_id, value );
                require_load_inst_[env_id] = is_alloca_inst;
            }

            //
            auto bind_value( environment_id_t const& env_id, llvm::AllocaInst* const alloca_inst )
                -> void
            {
                bind_value( env_id, static_cast<llvm::Value*>( alloca_inst ), true );
//                    require_load_inst_[env_id] = true;
            }


            auto ref_value( environment_id_t const& env_id ) const
                -> llvm::Value*
            {
                return value_table_.at( env_id );
            }

            auto is_alloca_inst( environment_id_t const& env_id ) const
                -> bool
            {
                return require_load_inst_.at( env_id );
            }

            //
            // Types
            //
            auto bind_type( const_class_symbol_environment_ptr const& class_env, llvm::Type* const type )
                -> void
            {
                // TODO: dup check
                type_table_.emplace( class_env->get_id(), type );
            }

            auto ref_type( environment_id_t const& env_id ) const
                -> llvm::Type*
            {
                return type_table_.at( env_id );
            }


            //
            // Functions
            //
            auto bind_function_type( environment_id_t const& env_id, llvm::FunctionType* const f_type )
                -> void
            {
                // TODO: dup check
                function_type_table_.emplace( env_id, f_type );
            }

            auto ref_function_type( environment_id_t const& env_id ) const
                -> llvm::FunctionType*
            {
                return function_type_table_.at( env_id );
            }


            // Classes
            auto create_class_variable_type_holder(
                environment_id_t const& class_env_id
                )
                -> void
            {
                // TODO: dup check
                class_variable_type_table_[class_env_id];
            }

            auto bind_class_variable_type(
                environment_id_t const& class_env_id,
                environment_id_t const& variable_env_id,
                llvm::Type* const type
                )
                -> void
            {
                auto& table = class_variable_type_table_.at( class_env_id );
                table.type_index[variable_env_id] = table.type_list.size();
                table.type_list.push_back( type );
            }

            auto bind_class_variable_type(
                environment_id_t const& class_env_id,
                llvm::Type* const type
                )
                -> void
            {
                auto& table = class_variable_type_table_.at( class_env_id );
                table.type_list.push_back( type );
            }

            auto ref_class_variable_type_list(
                environment_id_t const& class_env_id
                ) const
                -> std::vector<llvm::Type*> const&
            {
                return class_variable_type_table_.at( class_env_id ).type_list;
            }

            auto get_class_variable_index(
                environment_id_t const& class_env_id,
                environment_id_t const& variable_env_id
                ) const
                -> std::size_t
            {
                auto& table = class_variable_type_table_.at( class_env_id );
                return table.type_index.at( variable_env_id );
            }
/*
  auto ref_function_type( environment_id_t const& env_id ) const
  -> llvm::FunctionType*
  {
  return function_type_table_.at( env_id );
  }
*/

            //
            //
            //
            auto is_defined( environment_id_t const& env_id ) const
                -> bool
            {
                return ( value_table_.find( env_id ) != value_table_.cend() )
                    || ( type_table_.find( env_id ) != type_table_.cend() )
                    || ( function_type_table_.find( env_id ) != function_type_table_.cend() )
                    ;
            }

            auto is_defined(
                environment_id_t const& class_env_id,
                environment_id_t const& variable_env_id
                ) const
                -> bool
            {
                auto const& it = class_variable_type_table_.find( class_env_id );
                if ( it == class_variable_type_table_.cend() )
                    return false;

                auto const& ti = it->second.type_index;
                return ti.find( variable_env_id ) != ti.cend();
            }

        private:
            std::unordered_map<environment_id_t, llvm::Value*> value_table_;
            std::unordered_map<environment_id_t, llvm::Type*> type_table_;
            std::unordered_map<environment_id_t, llvm::FunctionType*> function_type_table_;

            struct class_variable_type_holder
            {
                std::vector<llvm::Type*> type_list;
                std::unordered_map<environment_id_t, std::size_t> type_index;
            };
            std::unordered_map<environment_id_t, class_variable_type_holder> class_variable_type_table_;

            std::unordered_map<environment_id_t, bool> require_load_inst_;
        };

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_ENV_CONVERSION_TABLE_HPP*/

#pragma once

#include <type_traits>


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename=void>
            struct ast_base_type_specifier {};

            template<typename T>
            using ast_base_type = typename ast_base_type_specifier<T>::type;

            template<typename T>
            using raw_ast_base_type = ast_base_type<std::decay_t<T>>;

        } // namesapce detail
    } // namespace ast
} // namespace rill

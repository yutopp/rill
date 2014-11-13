//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_MESSAGE_CODE_HPP
#define RILL_SEMANTIC_ANALYSIS_MESSAGE_CODE_HPP


namespace rill
{
    namespace semantic_analysis
    {
        enum class message_code
        {
            e_id_not_found,
            e_overload_nomatch,
            e_overload_anbigous,
                e_module_not_found,
                e_failed_to_parse_module,
                e_variable_is_already_defined,
                e_different_kind_symbol,
                e_file_not_found,

                e_reference
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_MESSAGE_CODE_HPP*/

#pragma once

#include <memory>

struct value;
typedef std::shared_ptr<value> value_ptr;


//
class literal_value;






namespace literal
{
    struct symbol_value;
    typedef std::shared_ptr<symbol_value> symbol_value_ptr;
















    // 
    struct identifier_value;
    typedef std::shared_ptr<identifier_value> identifier_value_ptr;








    //
    struct simple_identifier_value;
    typedef std::shared_ptr<simple_identifier_value> simple_identifier_value_ptr;







    struct int32_value;
    typedef std::shared_ptr<int32_value> int32_value_ptr;
}



#pragma once

#include <vector>
#include <string>

#include "config/macros.hpp"

#include "value_fwd.hpp"

#include "environment_fwd.hpp"
#include "tree_visitor_base.hpp"

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// values
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
enum struct value_spec
{
    constatnt = 0
};



typedef std::string     native_string_t;



struct value
{
public:
    // basic constructor
    value()
    {}

    // specify value's type name
    value( native_string_t const& simple_typename );

    virtual ~value() {}

public:
    bool is_intrinsic_type() const
    {
        return intrinsic_typed_identifier_.use_count() != 0;
    }

public:
    const_environment_ptr dispatch( tree_visitor_base const& visitor, environment_ptr const& env ) const
    {
        return visitor( *this, env );
    }

public:
    std::shared_ptr<literal::single_identifier_value const> intrinsic_typed_identifier_;
};








//
class literal_value
    : public value
{
public:
    literal_value()
    {}

    literal_value( std::string const& name )
        : value( name )
    {}

    virtual ~literal_value()
    {};
};






namespace literal
{
    struct symbol_value
        : public literal_value
    {
    public:
        typedef std::string     native_string_type;

    public:
        explicit symbol_value( native_string_t const& name )
            : value_( name )
        {}

    public:
        // deplicated
        auto get_native_symbol_string() const
            -> native_string_type
        {
            return value_;
        }

        auto get_native_string() const
            -> native_string_t const&
        {
            return value_;
        }

    private:
        native_string_t value_;
    };

    inline auto make_symbol( native_string_t const& native_symbol_name )
        -> symbol_value_ptr
    {
        return std::make_shared<symbol_value>( native_symbol_name );
    }










    struct single_identifier_value_base
        : literal_value
    {
    public:
        virtual ~single_identifier_value_base() {}

    public:
        virtual bool is_template() const =0;
        virtual auto get_base_symbol() const
            -> symbol_value_ptr =0;
        virtual auto template_argument() const
            -> template_argument_list_ptr =0;
    };



    // 
    struct identifier_value RILL_CXX11_FINAL
        : public literal_value
    {
    public:
        explicit identifier_value( std::vector<single_identifier_value_base_ptr> const& nests )
            : nest_( nests )
        {}

    public:
        auto get_last_identifier() const
            -> single_identifier_value_base_ptr
        {
            return nest_.back();
        }

        auto nest_size() const
            -> std::size_t
        {
            return nest_.size();
        }

    public:
        std::vector<single_identifier_value_base_ptr> const nest_;
    };


    // TODO: add support for namespaces
    inline auto make_identifier( single_identifier_value_base_ptr const& p )
        -> identifier_value_ptr
    {
        std::vector<single_identifier_value_base_ptr> const l( 1, p );
        return std::make_shared<identifier_value>( l );
    }



    class single_identifier_value
        : public single_identifier_value_base
    {
    public:
        single_identifier_value( native_string_t const& single_identifier_string )
            : base_name_( make_symbol( single_identifier_string ) )
        {}

        single_identifier_value( symbol_value_ptr const& single_identifier_symbol )
            : base_name_( single_identifier_symbol )
        {}

    public:
        bool is_template() const RILL_CXX11_OVERRIDE
        {
            return false;
        }

        auto get_base_symbol() const RILL_CXX11_OVERRIDE
            -> symbol_value_ptr
        {
            return base_name_;
        }

        auto template_argument() const RILL_CXX11_OVERRIDE
            -> template_argument_list_ptr
        {
            return nullptr;
        }

    private:
        symbol_value_ptr base_name_;
    };

    /*
    //
    struct simple_identifier_value
        : public identifier_value
    {
    public:
        // TODO: support namespace and so on
        simple_identifier_value( native_string_t const& simple_typename );
    public:


    private:
    };
    typedef std::shared_ptr<simple_identifier_value> simple_identifier_value_ptr;
    */

    inline auto make_single_identifier( native_string_t const& simple_typename )
        -> single_identifier_value_ptr
    {
        return std::make_shared<single_identifier_value>( simple_typename );
    }


    inline auto make_binary_operator_identifier(
        native_string_t const& symbol_name
        )
        -> single_identifier_value_ptr
    {
        return make_single_identifier( "%binary%operator_" + symbol_name );
    }
    inline auto make_binary_operator_identifier(
        symbol_value_ptr const& symbol_name
        )
        -> single_identifier_value_ptr
    {
        return make_binary_operator_identifier( symbol_name->get_native_symbol_string() );
    }
    // TODO: add overload function that implement template specified operator

    inline auto make_binary_operator_symbol(
        symbol_value_ptr const& symbol_name
        )
        -> symbol_value_ptr
    {
        return make_symbol( "%binary%operator_" + symbol_name->get_native_symbol_string() );
    }




/*

    //
    class template_identifier_value
        : public identifier_value
    {
    public:
        template_identifier_value( symbol_value::native_string_type const& simple_typename );

    public:


    private:
    };*/



    struct int32_value
        : public literal_value
    {
    public:
        int32_value( int const v );

    public:
        int get_value() const;

    public:
        int const value_;
    };
    typedef std::shared_ptr<int32_value> int32_value_ptr;
}


struct parameter_pair
{
    literal::identifier_value_ptr name;
    literal::identifier_value_ptr type;
    value_ptr default_value; // TODO: change to expresison
};

#include <boost/fusion/include/adapt_struct.hpp>
BOOST_FUSION_ADAPT_STRUCT(
    parameter_pair,
    (literal::identifier_value_ptr, name)
    (literal::identifier_value_ptr, type)
    (value_ptr,                     default_value)
)


inline auto make_parameter_pair(
    literal::identifier_value_ptr const& name,
    literal::identifier_value_ptr const& type,
    value_ptr const& default_value = nullptr
    )
    -> parameter_pair
{
    parameter_pair ap = { name, type, default_value };

    return ap;
}

inline auto make_parameter_pair(
    literal::identifier_value_ptr const& type,
    value_ptr const& default_value = nullptr
    )
    -> parameter_pair
{
    parameter_pair ap = { nullptr, type, default_value };

    return ap;
}


typedef std::vector<parameter_pair> parameter_list;

// test imprementation
inline auto make_parameter_list(
    parameter_pair const& pp
    )
    -> parameter_list
{
    parameter_list pl;
    pl.push_back( pp ); // test code

    return pl;
}







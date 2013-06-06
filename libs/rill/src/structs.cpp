#include <rill/structs.hpp>
#include <rill/environment.hpp>



    value::value()
    {}

    value::value( std::string const& simple_typename )
        : type_labal_( boost::make_shared<literal::simple_type_value>( simple_typename ) )
    {}

    value::~value() {}

    bool value::is_typed() const
    {
        return !type_labal_.use_count() != 0;
    }

    auto value::type() const -> typed_label_type
    {
        return type_labal_;
    }




//
literal_value::~literal_value() {}






namespace literal
{
        symbol_value::symbol_value( native_string_type const& )
        {
        }



    // 
        type_value::type_value( bool const is_template_type, symbol_value::native_string_type const& simple_typename )
            : is_template_( is_template_type )
            , simple_typename_( boost::make_shared<symbol_value>( simple_typename ) )
        {}

        type_value::~type_value() {}

        bool type_value::is_template() const
        {
            return is_template_;
        }


        auto type_value::template_parameters() const -> template_parameters_pointer
        {
            return nullptr;
        }




    //
        simple_type_value::simple_type_value( symbol_value::native_string_type const& simple_typename )
            : type_value( false, simple_typename )
        {}




    //
        template_type_value::template_type_value( symbol_value::native_string_type const& simple_typename )
            : type_value( true, simple_typename )
        {}




        int32_value::int32_value( int const v )
            : literal_value( "int" )
            , value_( v )
        {}

        int int32_value::get_value() const
        {
            return value_;
        }
}



// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// expressions
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

    expression::~expression() {}



    term_expression::term_expression( value_ptr const& v )
        : value_( v )
    {}

    value_ptr term_expression::eval( const_environment_ptr const& env )
    {
        return value_;
    }



    binary_expression::binary_expression( expression_ptr const& lhs, std::string const& op, expression_ptr const& rhs )
        : lhs_( lhs )
        , op_( op )
        , rhs_( rhs )
    {}

    value_ptr binary_expression::eval( const_environment_ptr const& env )
    {
        auto const& evaled_lhs = lhs_->eval( env );
        auto const& evaled_rhs = rhs_->eval( env );

        auto const& ee = env->lookup_env( "+"/*evaled_lhs->type()*/ );
        if ( !ee ) {
            // throw
            exit( -2 );
        }

        std::vector<value_ptr> v;
        v.push_back( evaled_lhs );
        v.push_back( evaled_rhs );
        ee->get_stmt();
        boost::dynamic_pointer_cast<native_function_definition_statement>( ee->get_stmt() )->call( v );


        return boost::make_shared<value>( /**lhs_->eval() + *rhs_->eval()*/ );
    }



// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// statements
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

    statement::~statement() {}







    expression_statement::expression_statement( expression_ptr const& expr )
        : expression_( expr )
    {}


    void expression_statement::setup_environment( environment_ptr const& ) const
    {
        // nothing to do
    }

    void expression_statement::eval( const_environment_ptr const& env ) const
    {
        expression_->eval( env );
    }

    void expression_statement::instantiation( environment_ptr const& root_env ) const {}
    void expression_statement::semantic_analysis( environment_ptr const& root_env ) const {}





    function_definition_statement::function_definition_statement( expression_ptr const& expr )
        : expression_( expr )
    {}


    void function_definition_statement::setup_environment( environment_ptr const& ) const
    {
        // nothing to do
    }

    void function_definition_statement::eval( const_environment_ptr const& env ) const
    {
        expression_->eval( env );
    }

    void function_definition_statement::instantiation( environment_ptr const& root_env ) const {}
    void function_definition_statement::semantic_analysis( environment_ptr const& root_env ) const {}



    native_function_definition_statement::native_function_definition_statement( std::string const& name, native_function_t const& callee )
        : name_( name )
        , callee_( callee )
    {}

    void native_function_definition_statement::setup_environment( environment_ptr const& ) const
    {
        // nothing to do
    }

    void native_function_definition_statement::call( std::vector<value_ptr> const& args ) const
    {
        std::cout << boost::dynamic_pointer_cast<literal::int32_value>( callee_( args ) )->get_value() << std::endl;;
        //expression_->eval( env );
    }

    void native_function_definition_statement::eval( const_environment_ptr const& env ) const
    {
        
        //expression_->eval( env );
    }

    void native_function_definition_statement::instantiation( environment_ptr const& root_env ) const {}
    void native_function_definition_statement::semantic_analysis( environment_ptr const& root_env ) const {}

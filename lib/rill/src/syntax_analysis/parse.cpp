#include <rill/syntax_analysis/parse.hpp>
#include <rill/syntax_analysis/code_grammar.hpp>
#include <rill/syntax_analysis/skip_grammar.hpp>
#include <rill/syntax_analysis/error.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        auto parse( ast::native_string_t const& source )
            -> ast::statements_ptr
        {
            iterator_t it = iterator_t( source.cbegin() );
            iterator_t const end = iterator_t( source.cend() );

            return parse( it, end );
        }

        auto parse(
            iterator_t& it,
            iterator_t const& end
            ) -> ast::statements_ptr
        {
            namespace x3 = boost::spirit::x3;

            rill::ast::statements_ptr stmts;
            error_container error_holder;

            auto const code_g = x3::with<error_container_tag>(
                std::ref( error_holder )
                )[code_grammar::rules::entrypoint()];
            auto const skip_g = skip_grammer::rules::entrypoint();

            bool const success
                = boost::spirit::x3::phrase_parse( it, end, code_g, skip_g, stmts );

            std::cout << "finish parsing: " << success << std::endl;

            if ( success ) {
                // reaches if *parsing* is succeeded with skipping
                if ( error_holder.size() != 0 ) {
                    // there are some syntax error
                    return nullptr;

                } else {
                    if ( it != end ) {
                        // not reached to eof -> error
                        return nullptr;
                    }
                }

            } else {
                return nullptr;
            }

            return stmts;
        }

    } // namespace syntax_analysis
} // namespace rill

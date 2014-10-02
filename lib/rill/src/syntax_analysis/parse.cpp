#include <rill/syntax_analysis/parse.hpp>
#include <rill/syntax_analysis/code_grammar.hpp>
#include <rill/syntax_analysis/skip_grammar.hpp>


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
            rill::ast::statements_ptr stmts;

            auto const code_g = code_grammar::rules::entrypoint();
            auto const skip_g = skip_grammer::rules::entrypoint();

            bool const success
                = boost::spirit::x3::phrase_parse( it, end, code_g, skip_g, stmts );

            if ( success ) {
                std::cout << "succ! " << std::endl;
                std::cout << std::string( it, end ) << stmts << std::endl;
                if ( stmts == nullptr ) {
                    std::cout << "stmts is null" << std::endl;
                } else {
                    stmts->dump( std::cout );
                }

            } else {
                std::cout << "failed" << std::endl;
            }

            return stmts;
        }

    } // namespace syntax_analysis
} // namespace rill

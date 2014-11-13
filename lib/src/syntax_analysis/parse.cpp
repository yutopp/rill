//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/syntax_analysis/parse.hpp>
#include <rill/syntax_analysis/code_grammar.hpp>
#include <rill/syntax_analysis/skip_grammar.hpp>
#include <rill/syntax_analysis/tag.hpp>
#include <rill/syntax_analysis/error.hpp>

#include <fstream>
#include <iterator>
#include <boost/filesystem/path.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        auto parse( boost::filesystem::path const& source_fullpath )
            -> ast::module_ptr
        {
            // TODO: change to mapped file
            std::ifstream ifs( source_fullpath.string() );
            if ( !ifs ) {
                std::cerr << source_fullpath << " was not found..." << std::endl;
                return nullptr;
            }

            std::istreambuf_iterator<char> const begin = ifs, end;
            rill::ast::native_string_t const input_source_code( begin, end );
            rill_dout << "inputs are:" << std::endl
                      << input_source_code << std::endl;

            return parse( input_source_code, source_fullpath );
        }

        auto parse(
            ast::native_string_t const& source,
            boost::optional<boost::filesystem::path> const& source_fullpath
            )
            -> ast::module_ptr
        {
            iterator_t it = iterator_t( source.cbegin() );
            string_iterator_t const orig_begin = source.cbegin();
            iterator_t const end = iterator_t( source.cend() );

            return parse( it, orig_begin, end, source_fullpath );
        }

        auto parse(
            iterator_t& it,
            string_iterator_t const& orig_begin,
            iterator_t const& end,
            boost::optional<boost::filesystem::path> const& source_fullpath
            ) -> ast::module_ptr
        {
            namespace x3 = boost::spirit::x3;

            rill::ast::module_ptr mod;
            error_container error_holder;

            std::string const filepath
                = source_fullpath
                ? source_fullpath->string()
                : "noname";

            auto const code_g
                = x3::with<error_container_tag>(
                    std::ref( error_holder )
                    )[x3::with<iterator_orig_begin_tag>(
                        orig_begin
                        )[x3::with<filepath_tag>(
                            filepath
                            )[code_grammar::rules::entrypoint()]]];
            auto const skip_g = skip_grammer::rules::entrypoint();

            bool const success
                = boost::spirit::x3::phrase_parse( it, end, code_g, skip_g, mod );

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

            // set path
            mod->fullpath
                = source_fullpath
                ? *source_fullpath
                : boost::filesystem::path();

            return mod;
        }

    } // namespace syntax_analysis
} // namespace rill

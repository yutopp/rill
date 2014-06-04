//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_UTILITY_COLORIZE_HPP
#define RILL_UTILITY_COLORIZE_HPP

#include <cstdlib>
#include <ostream>


namespace rill
{
    namespace colorize
    {
        namespace esc
        {
            namespace detail
            {
                template<std::size_t N>
                struct sgr_list
                {
                    template<std::size_t NN>
                    friend auto put( std::ostream&, sgr_list<NN> const& ) -> std::ostream&;

                public:
                    explicit sgr_list( sgr_list<N-1> const& prev, std::uint8_t const p )
                        : prev_( prev )
                        , param_( p )
                    {}

                private:
                    sgr_list<N-1> prev_;
                    std::uint8_t param_;
                };

                template<>
                struct sgr_list<1>
                {
                    template<std::size_t NN>
                    friend auto put( std::ostream&, sgr_list<NN> const& ) -> std::ostream&;

                public:
                    explicit sgr_list( std::uint8_t const p )
                        : param_( p )
                    {}

                private:
                    std::uint8_t param_;
                };

                template<>
                struct sgr_list<0>;

                template<std::size_t N>
                auto put( std::ostream& os, sgr_list<N> const& sgrl )
                    -> std::ostream&
                {
                    os << static_cast<int>( sgrl.param_ ) << ";";
                    return put( os, sgrl.prev_ );
                }

                template<>
                auto put<1>( std::ostream& os, sgr_list<1> const& sgrl )
                -> std::ostream&
                {
                    os << static_cast<int>( sgrl.param_ );
                    return os;
                }

                template<std::size_t N>
                auto operator<<( std::ostream& os, detail::sgr_list<N> const& sgrl )
                    -> std::ostream&
                {
                    //
                    os << "\x1b[";

                    //
                    put( os, sgrl );

                    //
                    os << 'm';

                    return os;
                }
            } // namespace detail


            enum class forground_color : std::uint8_t
            {
                black = 30,
                red,
                green,
                yellow,
                blue,
                magenta,
                cyan,
                white
            };
            using fg = forground_color;

            enum status
            {
                reset = 0,
                bold = 1
            };


            // gen
            auto operator|( forground_color const& f, status const& s )
                -> detail::sgr_list<2>
            {
                return detail::sgr_list<2>(
                    detail::sgr_list<1>( static_cast<std::uint8_t>( f ) ),
                    static_cast<std::uint8_t>( s )
                    );
            }
            auto operator|( status const& s, forground_color const& f )
                -> detail::sgr_list<2>
            {
                return f | s;
            }


            inline auto operator<<( std::ostream& os, status const& s )
                -> std::ostream&
            {
                return os << detail::sgr_list<1>( static_cast<std::uint8_t>( s ) );
            }

            inline auto operator<<( std::ostream& os, forground_color const& f )
                -> std::ostream&
            {
                return os << detail::sgr_list<1>( static_cast<std::uint8_t>( f ) );
            }

        } // namespace esc

        // TODO: switch this per environments
        namespace standard = esc;

    } // namespace colorize
} // namespace rill

#endif /*RILL_UTILITY_COLORIZE_HPP*/

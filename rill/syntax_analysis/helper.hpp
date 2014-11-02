//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_HELPER_HPP
#define RILL_SYNTAX_ANALYSIS_HELPER_HPP

#include <string>
#include <memory>

#include "../ast.hpp"
#include "on_success.hpp"
#include "on_error.hpp"


#define RILL_RULES_BEGIN( struct_name, entrypoint_name )    \
    struct struct_name                                      \
    {                                                       \
    using self_type = struct_name;                          \
                                                            \
    static auto instance()                                  \
        -> struct_name const&                               \
    {                                                       \
        static struct_name self;                            \
        return self;                                        \
    }                                                       \
                                                            \
    static auto entrypoint()                                \
    {                                                       \
        return instance().entrypoint_name;                  \
    }                                                       \

#define RILL_RULES_END                          \
    };

#define RILL_ANNOTAROR_BASE_SPEC( r, _unused, elem )    \
    , public elem

#define RILL_RULE( name, type, ... )                    \
    RILL_RULE_WITH_ANNOTATOR( name, type, BOOST_PP_SEQ_NIL, __VA_ARGS__ )

#define RILL_RULE_WITH_ANNOTATOR( name, type, bases, ... )              \
    class name                                                          \
        : public rill::syntax_analysis::on_success_annotator_base       \
          BOOST_PP_SEQ_FOR_EACH( RILL_ANNOTAROR_BASE_SPEC, _, bases )   \
    {};                                                                 \
    struct PP_ ## name {                                                \
        using rule_type = x3::rule<name, type>;                         \
        static auto def( self_type const& t )  {                        \
            return __VA_ARGS__;                                         \
        }                                                               \
        auto operator()() -> rule_type {                                \
            return rule_type( #name );                                  \
        }                                                               \
    };                                                                  \
    decltype(( std::declval<PP_ ## name>()) ()) const name = PP_ ## name()(); \
    template <typename Iterator, typename Context, typename Attribute>  \
    friend inline bool parse_rule(                                      \
        decltype(( std::declval<PP_ ## name>()) ()) rule_,              \
        Iterator& first, Iterator const& last,                          \
        Context const& context, Attribute& attr                         \
        )                                                               \
    {                                                                   \
        using boost::spirit::x3::unused;                                \
        auto const& ri = self_type::instance();                         \
        auto const& raw_def = PP_ ## name :: def( ri );                 \
        auto const& def_ = ( ri.name = raw_def );                       \
        return def_.parse(first, last, context, unused, attr);          \
    }                                                                   \


namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        namespace placeholders
        {
            namespace detail
            {
                template<std::size_t I>
                struct placeholder_t
                {
                    static_assert( I > 0, "" );
                    static constexpr std::size_t value = I;
                };
            } // namespace detail

            constexpr auto _1 = detail::placeholder_t<1>();
            constexpr auto _2 = detail::placeholder_t<2>();
            constexpr auto _3 = detail::placeholder_t<3>();
            constexpr auto _4 = detail::placeholder_t<4>();
            constexpr auto _5 = detail::placeholder_t<5>();
            constexpr auto _6 = detail::placeholder_t<6>();
            constexpr auto _7 = detail::placeholder_t<7>();
        } // namespace placeholder


        namespace helper
        {
            template<typename T>
            struct is_placeholder
            {
                static constexpr std::size_t value = 0;
            };

            template<std::size_t I>
            struct is_placeholder<placeholders::detail::placeholder_t<I>>
            {
                static constexpr std::size_t value = I;
            };


            template<
                std::size_t I,
                typename Attr,
                std::enable_if_t<
                    boost::fusion::traits::is_sequence<Attr>::value
                    >* = nullptr
                >
            auto extract( Attr& attr )
            {
                using boost::fusion::at_c;
                return at_c<I>( attr );
            }

            template<
                std::size_t I,
                typename Attr,
                std::enable_if_t<
                    ! boost::fusion::traits::is_sequence<Attr>::value
                    >* = nullptr
                >
            auto extract( Attr& attr )
            {
                return attr;
            }


            // for place holder
            template<
                typename Context,
                typename T,
                std::enable_if_t<
                    is_placeholder<std::decay_t<T>>::value != 0
                    >* = nullptr
                >
            auto action_value( Context& ctx, T&& )
            {
                constexpr auto index = is_placeholder<std::decay_t<T>>::value - 1;
                return extract<index>( x3::_attr( ctx ) );
            }

            // for normal value
            template<
                typename Context,
                typename T,
                std::enable_if_t<
                    is_placeholder<std::decay_t<T>>::value == 0
                    >* = nullptr
                >
            auto action_value( Context&, T&& v )
            {
                return std::forward<T>( v );
            }

            template<typename T, typename... Args>
            auto make_node_ptr( Args&&... args )
            {
                return std::bind(
                    []( auto& ctx, auto&&... args ) {
                        x3::_val( ctx ) = std::make_shared<T>(
                            action_value<decltype(ctx)>(
                                ctx,
                                std::forward<decltype(args)>( args )
                                )...
                            );
                    },
                    std::placeholders::_1,  // ctx
                    std::forward<Args>( args )...
                    );
            }

            template<typename F, typename... Args>
            auto fun( F&& f, Args&&... args )
            {
                return std::bind(
                    []( auto& ctx, auto const& f, auto&&... args ) {
                        x3::_val( ctx ) = f(
                            action_value<decltype(ctx)>(
                                ctx,
                                std::forward<decltype(args)>( args )
                                )...
                            );
                    },
                    std::placeholders::_1,  // ctx
                    std::forward<F>( f ),
                    std::forward<Args>( args )...
                    );
            }

            inline auto assign()
            {
                return []( auto& ctx ) {
                    x3::_val( ctx ) = x3::_attr( ctx );
                };
            }

            template<typename T>
            auto assign( T&& t )
            {
                return std::bind(
                    []( auto& ctx, auto&& arg ) {
                        x3::_val( ctx ) = std::forward<decltype(arg)>( arg );
                    },
                    std::placeholders::_1,  // ctx
                    std::forward<T>( t )
                    );
            }

            template<typename T, typename... Args>
            auto construct( Args&&... args )
            {
                return std::bind(
                    []( auto& ctx, auto&&... args ) {
                        x3::_val( ctx ) = T{
                            action_value<decltype(ctx)>(
                                ctx,
                                std::forward<decltype(args)>( args )
                                )...
                            };
                    },
                    std::placeholders::_1,  // ctx
                    std::forward<Args>( args )...
                    );
            }

            template<typename T, typename... Args>
            auto make_assoc_node_ptr(
                Args&&... args
                )
            {
                return std::bind(
                    []( auto& ctx, auto&&... args ) {
                        x3::_val( ctx ) = std::make_shared<T>(
                            x3::_val( ctx ),    // lhs
                            action_value<decltype(ctx)>(
                                ctx,
                                std::forward<decltype(args)>( args )
                                )...
                            );
                    },
                    std::placeholders::_1,  // ctx
                    std::forward<Args>( args )...
                    );
            }

            template<typename T1>
            auto make_left_assoc_binary_op_node_ptr(
                ast::native_string_t const& op,
                T1&& rhs
                )
            {
                return make_assoc_node_ptr<ast::binary_operator_expression>(
                    ast::make_binary_operator_identifier( op ), // op
                    std::forward<T1>( rhs )
                    );
            }

            template<typename T1>
            auto make_merged_bitflag( T1&& rhs )
            {
                return std::bind(
                    []( auto& ctx, auto&& args ) {
                        x3::_val( ctx )
                            = x3::_val( ctx ) | action_value<decltype(ctx)>(
                                ctx,
                                std::forward<decltype(args)>( args )
                                );
                    },
                    std::placeholders::_1,  // ctx
                    std::forward<T1>( rhs )
                    );
            }

        } // namespace helper
    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_HELPER_HPP*/

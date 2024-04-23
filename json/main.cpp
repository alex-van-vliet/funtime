#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <expected>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <string_view>
#include <utility>

using namespace std::string_view_literals;

// @formatter:off
constexpr char json_example[] =
#include "json.json"
;
// @formatter:on

namespace meta {
    template<size_t N>
    struct TString {
        // Must be public to allow NTTP.
        std::array<char, N + 1> data;

        // Not marked explicit to allow for NTTP conversion.
        constexpr TString(const char (&s)[N + 1]) {
            std::copy_n(s, N + 1, data.data());
        }

        constexpr explicit TString(std::string_view sv) {
            std::copy_n(sv.data(), N, data.data());
            data[N] = 0;
        }

        [[nodiscard]] constexpr char operator[](std::size_t i) const {
            return data[i];
        }

        [[nodiscard]] constexpr std::string_view sv() const {
            return std::string_view{data.data(), N};
        }

        [[nodiscard]] constexpr std::size_t size() const {
            return N;
        }
    };

    template<size_t N>
    TString(const char (&s)[N]) -> TString<N - 1>;

    namespace list {
        struct End {
        };

        template<typename Value, typename Next = End>
        struct Node {
            using value = Value;
            using next = Next;
        };

        template<typename... Values>
        struct Make {
            using type = End;
        };

        template<typename Value, typename... Values>
        struct Make<Value, Values...> {
            using type = Node<Value, typename Make<Values...>::type>;
        };

        template<typename... Values>
        using make_t = typename Make<Values...>::type;

        template<typename List, template<typename...> typename Result, typename... Values>
        struct ToVariadic {
            using type = Result<Values...>;
        };

        template<typename Value, typename Next, template<typename...> typename Result, typename... Values>
        struct ToVariadic<Node<Value, Next>, Result, Values...> {
            using type = typename ToVariadic<Next, Result, Values..., Value>::type;
        };

        template<typename List, template<typename...> typename Result>
        using to_variadic_t = typename ToVariadic<List, Result>::type;

        template<typename List>
        using to_tuple_t = to_variadic_t<List, std::tuple>;
    }// namespace list
}// namespace meta

namespace json {
    enum class Type {
        // Cannot put "NULL" unfortunately.
        JSON_NULL,
        JSON_BOOLEAN,
        JSON_INTEGER,
        JSON_NUMBER,
        JSON_STRING,
        JSON_LIST,
        JSON_OBJECT,
    };

    // A bunch of concepts for each of the json types v

    template<typename T>
    concept JsonObject = requires(T) {
        { T::type } -> std::convertible_to<Type>;
    };

    template<typename T>
    concept IsNull = JsonObject<T> && (T::type == Type::JSON_NULL);

    template<typename T>
    concept IsBoolean = JsonObject<T> && (T::type == Type::JSON_BOOLEAN) &&
                            requires(T)
    {
        { T::value } -> std::convertible_to<bool>;
    };

    template<typename T>
    concept IsInteger = JsonObject<T> && (T::type == Type::JSON_INTEGER) &&
                            requires(T)
    {
        { T::value } -> std::convertible_to<std::int64_t>;
    };

    template<typename T>
    concept IsNumber = JsonObject<T> && (T::type == Type::JSON_NUMBER) &&
                           requires(T)
    {
        { T::value } -> std::convertible_to<double>;
    };

    template<typename T>
    concept IsString = JsonObject<T> && (T::type == Type::JSON_STRING) &&
                           requires(T)
    {
        { T::value } -> std::convertible_to<std::string_view>;
    };

    template<typename T>
    concept IsList = JsonObject<T> && (T::type == Type::JSON_LIST) &&
                         requires(T)
    {
        {T::forEach([]<std::size_t Index, typename Entry>() {})};
    };

    template<typename T>
    concept IsObject = JsonObject<T> && (T::type == Type::JSON_OBJECT) &&
                           requires(T)
    {
        {T::forEach([]<std::size_t Index, meta::TString Key, typename Value>() {})};
    };

    struct Null {
        constexpr static Type type = Type::JSON_NULL;
    };

    template<bool Value>
    struct Boolean {
        constexpr static Type type = Type::JSON_BOOLEAN;
        constexpr static bool value = Value;
    };

    template<std::int64_t Value>
    struct Integer {
        constexpr static Type type = Type::JSON_INTEGER;
        constexpr static std::int64_t value = Value;
    };

    template<double Value>
    struct Number {
        constexpr static Type type = Type::JSON_NUMBER;
        constexpr static double value = Value;
    };

    template<meta::TString Value>
    struct String {
        constexpr static Type type = Type::JSON_STRING;
        constexpr static std::string_view value = Value.sv();
    };

    template<typename... Values>
    struct List {
        constexpr static Type type = Type::JSON_LIST;

        template<typename Fun>
        constexpr static void forEach(Fun fun) {
            ([&fun]<std::size_t... Indices>(std::index_sequence<Indices...>) {
                ((void) fun.template operator()<Indices, Values>(), ...);
            })(std::make_index_sequence<std::tuple_size_v<std::tuple<Values...>>>{});
        }
    };

    template<meta::TString Key, typename Value>
    struct ObjectEntry {
        constexpr static meta::TString key = Key;
        using value = Value;
    };

    template<typename... Entries>
    struct Object {
        constexpr static Type type = Type::JSON_OBJECT;

        template<typename Fun>
        constexpr static void forEach(Fun fun) {
            ([&fun]<std::size_t... Indices>(std::index_sequence<Indices...>) {
                ((void) fun.template operator()<Indices, Entries::key, typename Entries::value>(), ...);
            })(std::make_index_sequence<std::tuple_size_v<std::tuple<Entries...>>>{});
        }
    };

    namespace lexer {
        enum class TokenType {
            TOKEN_ERROR = 0,
            TOKEN_END,

            TOKEN_NULL,
            TOKEN_TRUE,
            TOKEN_FALSE,

            TOKEN_STRING,
            TOKEN_NUMBER,

            TOKEN_LBRACE,
            TOKEN_RBRACE,

            TOKEN_LBRACKET,
            TOKEN_RBRACKET,

            TOKEN_COMMA,
            TOKEN_COLON,
        };

        template<TokenType Type, std::size_t Start, size_t End>
        struct Token {
            constexpr static TokenType type = Type;
            constexpr static std::size_t start = Start;
            constexpr static std::size_t end = End;

            constexpr static std::string_view sv() {
                switch (type) {
                    case TokenType::TOKEN_LBRACE:
                        return "{";
                    case TokenType::TOKEN_RBRACE:
                        return "}";
                    case TokenType::TOKEN_LBRACKET:
                        return "[";
                    case TokenType::TOKEN_RBRACKET:
                        return "]";
                    case TokenType::TOKEN_COMMA:
                        return ",";
                    case TokenType::TOKEN_COLON:
                        return ":";
                    default:
                        return "unprintable";
                }
            }
        };

        template<std::size_t I, meta::TString Message>
        struct ErrorToken : Token<TokenType::TOKEN_ERROR, I, I + 1> {
            constexpr static meta::TString message = Message;

            constexpr static std::string_view sv() {
                return "error";
            }
        };

        template<std::size_t I>
        struct EndToken : Token<TokenType::TOKEN_END, I, I> {
            constexpr static std::string_view sv() {
                return "_END_";
            }
        };

        template<std::size_t I>
        struct NullToken : Token<TokenType::TOKEN_NULL, I, I + 4> {
            constexpr static std::string_view sv() {
                return "null";
            }
        };

        template<std::size_t I, bool Value>
        struct BooleanToken : Token<Value ? TokenType::TOKEN_TRUE : TokenType::TOKEN_FALSE, I,
                                    I + 4 + (Value ? 0 : 1)> {
            constexpr static bool value = Value;

            constexpr static std::string_view sv() {
                return Value ? "true" : "false";
            }
        };

        template<std::size_t Start, std::size_t End, meta::TString Value>
        struct NumberToken : Token<TokenType::TOKEN_NUMBER, Start, End> {
            constexpr static meta::TString value = Value;

            constexpr static std::string_view sv() {
                return value.sv();
            }

            constexpr static bool isInteger() {
                return !value.sv().contains('.') && !value.sv().contains('e') && !value.sv().contains('E');
            }

            template<typename T, bool asFractional>
            constexpr static std::tuple<bool, T, std::string_view> parseDigits(std::string_view integer) {
                // If asDecimals, the integer is the fractional part of a floating point number.
                static_assert(!asFractional || std::is_floating_point_v<T>);

                bool negative{false};
                switch (integer.front()) {
                    case '-':
                        negative = true;
                        [[fallthrough]];
                    case '+':
                        integer = integer.substr(1);
                };

                T result{};
                size_t i = 0;

                if constexpr (asFractional) {
                    // Probably better float parsing
                    T div{10.};
                    for (char digit: integer) {
                        if (!('0' <= digit && digit <= '9')) {
                            break;
                        }
                        result = result + (digit - '0') * 1. / div;

                        div *= 10;
                        i += 1;
                    }
                } else {
                    for (char digit: integer) {
                        if (!('0' <= digit && digit <= '9')) {
                            break;
                        }
                        result = result * 10 + digit - '0';

                        i += 1;
                    }
                }

                return {negative, result, integer.substr(i)};
            }

            constexpr static std::int64_t asInteger() {
                // Cannot have a remainder.
                auto [negative, result, _] = parseDigits<std::int64_t, false>(value.sv());
                return (negative ? -result : result);
            }

            constexpr static double asDouble() {
                auto str = value.sv();

                auto [negative, result, after_integer] = parseDigits<double, false>(str);
                str = after_integer;

                if (str.starts_with('.')) {
                    // Cannot be negative.
                    auto [_, decimal, after_decimal] = parseDigits<double, true>(str.substr(1));
                    result += decimal;
                    str = after_decimal;
                }

                if (str.starts_with('e') || str.starts_with('E')) {
                    // Cannot have a remainder.
                    auto [negativeExponent, exponent, _] = parseDigits<double, false>(str.substr(1));
                    result *= std::pow(10., negativeExponent ? -exponent : exponent);
                }

                return negative ? -result : result;
            }
        };

        template<std::size_t Start, std::size_t End, meta::TString Value>
        struct StringToken : Token<TokenType::TOKEN_STRING, Start, End> {
            constexpr static meta::TString value = Value;

            constexpr static std::string_view sv() {
                return value.sv();
            }
        };

        constexpr TokenType getSimpleType(unsigned char c) {
            TokenType types[256]{TokenType::TOKEN_ERROR};
            types['['] = TokenType::TOKEN_LBRACKET;
            types[']'] = TokenType::TOKEN_RBRACKET;
            types['{'] = TokenType::TOKEN_LBRACE;
            types['}'] = TokenType::TOKEN_RBRACE;
            types[','] = TokenType::TOKEN_COMMA;
            types[':'] = TokenType::TOKEN_COLON;
            return types[c];
        }

        enum class NumberParsingError {
            MISSING_FRACTION,
            MISSING_EXPONENT,
        };

        constexpr std::expected<std::size_t, NumberParsingError> getNumberLength(std::string_view sv, std::size_t first) {
            sv = sv.substr(first);

            // First char should always be a number.
            // This allows leading zeros.

            size_t length = 0;
            // 1. Check integer,
            while (length < sv.size() && '0' <= sv[length] && sv[length] <= '9') {
                length += 1;
            }

            // 2. Check fraction,
            if (length < sv.size() && sv[length] == '.') {
                length += 1;

                if (length == sv.size() || !('0' <= sv[length] && sv[length] <= '9')) {
                    return std::unexpected(NumberParsingError::MISSING_FRACTION);
                }
                length += 1;

                while (length < sv.size() && '0' <= sv[length] && sv[length] <= '9') {
                    length += 1;
                }
            }

            // 3. Check exponent.
            if (length < sv.size() && (sv[length] == 'e' || sv[length] == 'E')) {
                length += 1;

                if (length == sv.size() || !(sv[length] == '+' || sv[length] == '-')) {
                    return std::unexpected(NumberParsingError::MISSING_EXPONENT);
                }
                length += 1;
                if (length == sv.size() || !('0' <= sv[length] && sv[length] <= '9')) {
                    return std::unexpected(NumberParsingError::MISSING_EXPONENT);
                }
                length += 1;

                while (length < sv.size() && '0' <= sv[length] && sv[length] <= '9') {
                    length += 1;
                }
            }
            return length;
        }

        template<meta::TString Json, std::size_t I, bool Negative>
        auto parsePositiveNumber() {
            constexpr std::size_t start = I - (Negative ? 1 : 0);
            if constexpr (constexpr auto absoluteLength = getNumberLength(Json.sv(), I); absoluteLength.has_value()) {
                constexpr std::size_t length = absoluteLength.value() + (Negative ? 1 : 0);
                return NumberToken<start, start + length, meta::TString<length>{Json.sv().substr(start, length)}>{};
            } else {
                return ErrorToken<start, "Invalid number">{};
            }
        }

        enum class StringParsingError {
            MISSING_END_QUOTES,
        };

        constexpr std::expected<std::size_t, StringParsingError> getStringLength(std::string_view sv, std::size_t first) {
            sv = sv.substr(first);

            // Called when already in a string
            std::size_t length = 0;

            while (length < sv.size()) {
                if (sv[length] == '"') {
                    return length + 1;// Include "
                }

                length += 1;
            }
            return std::unexpected(StringParsingError::MISSING_END_QUOTES);
        }

        template<meta::TString Json, std::size_t I>
        auto parseString() {
            if constexpr (constexpr auto length = getStringLength(Json.sv(), I); length.has_value()) {
                return StringToken<
                        I - 1, I + length.value(), meta::TString<length.value() - 1>{Json.sv().substr(I, length.value() - 1)}>{};
            } else {
                return ErrorToken<I - 1, "Invalid string">{};
            }
        }

        template<meta::TString Json, std::size_t I>
        struct LexerImpl {
            using type = decltype([]() {
                if constexpr (I == Json.size()) {
                    return EndToken<I>{};
                } else if constexpr (" \t\r\n"sv.contains(Json[I])) {
                    return typename LexerImpl<Json, I + 1>::type{};
                } else if constexpr ("[]{},:"sv.contains(Json[I])) {
                    return Token<getSimpleType(Json[I]), I, I + 1>{};
                } else if constexpr (Json.sv().substr(I, 4) == "null") {
                    return NullToken<I>{};
                } else if constexpr (Json.sv().substr(I, 4) == "true") {
                    return BooleanToken<I, true>{};
                } else if constexpr (Json.sv().substr(I, 5) == "false") {
                    return BooleanToken<I, false>{};
                } else if constexpr ('0' <= Json[I] && Json[I] <= '9') {// Simplification: allow leading zeros
                    return parsePositiveNumber<Json, I, false>();
                } else if constexpr (Json[I] == '-' && I + 1 < Json.size() &&
                                     '0' <= Json[I + 1] && Json[I + 1] <= '9') {// Simplification: allow leading zeros
                    return parsePositiveNumber<Json, I + 1, true>();
                } else if constexpr (Json[I] == '"') {
                    return parseString<Json, I + 1>();
                } else {
                    return ErrorToken<I, "Unexpected character">{};
                }
            }());
        };
    }// namespace lexer

    template<meta::TString Json, std::size_t I = 0>
    struct Lexer {
        using type = decltype([]() {
            using current = typename lexer::LexerImpl<Json, I>::type;

            if constexpr (current::type == lexer::TokenType::TOKEN_END || current::type == lexer::TokenType::TOKEN_ERROR) {
                return meta::list::Node<current>{};
            } else {
                return meta::list::Node<current, typename Lexer<Json, current::end>::type>{};
            }
        }());
    };

    /* Lexer tests */
    static_assert(std::is_same_v<lexer::LexerImpl<"null", 0>::type, lexer::NullToken<0>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"  null", 0>::type, lexer::NullToken<2>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"true", 0>::type, lexer::BooleanToken<0, true>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"false", 0>::type, lexer::BooleanToken<0, false>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"[", 0>::type, lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 0, 1>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"]", 0>::type, lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 0, 1>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"{", 0>::type, lexer::Token<lexer::TokenType::TOKEN_LBRACE, 0, 1>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"}", 0>::type, lexer::Token<lexer::TokenType::TOKEN_RBRACE, 0, 1>>);
    static_assert(std::is_same_v<lexer::LexerImpl<",", 0>::type, lexer::Token<lexer::TokenType::TOKEN_COMMA, 0, 1>>);
    static_assert(std::is_same_v<lexer::LexerImpl<":", 0>::type, lexer::Token<lexer::TokenType::TOKEN_COLON, 0, 1>>);
    static_assert(std::is_same_v<lexer::LexerImpl<"0", 0>::type, lexer::NumberToken<0, 1, "0">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"1", 0>::type, lexer::NumberToken<0, 1, "1">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"2", 0>::type, lexer::NumberToken<0, 1, "2">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"3", 0>::type, lexer::NumberToken<0, 1, "3">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"4", 0>::type, lexer::NumberToken<0, 1, "4">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"5", 0>::type, lexer::NumberToken<0, 1, "5">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"6", 0>::type, lexer::NumberToken<0, 1, "6">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"7", 0>::type, lexer::NumberToken<0, 1, "7">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"8", 0>::type, lexer::NumberToken<0, 1, "8">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"9", 0>::type, lexer::NumberToken<0, 1, "9">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"0123456789", 0>::type, lexer::NumberToken<0, 10, "0123456789">>);
    static_assert(
            std::is_same_v<lexer::LexerImpl<"0123456789.", 0>::type, lexer::ErrorToken<0, "Invalid number">>);
    static_assert(
            std::is_same_v<lexer::LexerImpl<"0123456789e", 0>::type, lexer::ErrorToken<0, "Invalid number">>);
    static_assert(
            std::is_same_v<lexer::LexerImpl<"0123456789E", 0>::type, lexer::ErrorToken<0, "Invalid number">>);
    static_assert(std::is_same_v<lexer::LexerImpl<"1.23e+4", 0>::type, lexer::NumberToken<0, 7, "1.23e+4">>);
    static_assert(
            std::is_same_v<lexer::LexerImpl<"\"Hello, World!\"", 0>::type, lexer::StringToken<0, 15, "Hello, World!">>);
    static_assert(
            std::is_same_v<lexer::LexerImpl<"\"Hello, World!", 0>::type, lexer::ErrorToken<0, "Invalid string">>);


    static_assert(
            std::is_same_v<Lexer<"">::type, meta::list::Node<lexer::EndToken<0>>>);
    static_assert(
            std::is_same_v<Lexer<".">::type, meta::list::Node<lexer::ErrorToken<0, "Unexpected character">>>);


    constexpr meta::TString lexingTest{"null true false []{},: 123 \"test\""};
    static_assert(std::is_same_v<Lexer<lexingTest>::type,
                                 meta::list::make_t<
                                         lexer::NullToken<0>,
                                         lexer::BooleanToken<5, true>,
                                         lexer::BooleanToken<10, false>,
                                         lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 16, 17>,
                                         lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 17, 18>,
                                         lexer::Token<lexer::TokenType::TOKEN_LBRACE, 18, 19>,
                                         lexer::Token<lexer::TokenType::TOKEN_RBRACE, 19, 20>,
                                         lexer::Token<lexer::TokenType::TOKEN_COMMA, 20, 21>,
                                         lexer::Token<lexer::TokenType::TOKEN_COLON, 21, 22>,
                                         lexer::NumberToken<23, 26, "123">,
                                         lexer::StringToken<27, 33, "test">,
                                         lexer::EndToken<lexingTest.size()>>>);

    namespace parser {
        template<typename TokenList>
        struct ParserImpl_ParseElement;

        template<typename TokenList, bool First = true>
        struct ParserImpl_ParseList {
            static_assert(!std::is_same_v<TokenList, meta::list::End>, "Expected more tokens");

            using result = decltype([]() {
                if constexpr (First && TokenList::value::type == lexer::TokenType::TOKEN_RBRACKET) {
                    return std::tuple<meta::list::End, typename TokenList::next>{};
                } else {
                    using current = ParserImpl_ParseElement<TokenList>;
                    using comma_or_end = typename current::next;
                    static_assert(!std::is_same_v<comma_or_end, meta::list::End>, "Expected more tokens");
                    if constexpr (comma_or_end::value::type == lexer::TokenType::TOKEN_COMMA) {
                        using next = ParserImpl_ParseList<typename comma_or_end::next, false>;
                        return std::tuple<meta::list::Node<typename current::type, typename next::type>, typename next::next>{};
                    } else if constexpr (comma_or_end::value::type == lexer::TokenType::TOKEN_RBRACKET) {
                        return std::tuple<meta::list::Node<typename current::type>, typename comma_or_end::next>{};
                    } else {
                        // Could be static_assert(false), but not allowed
                        static_assert(comma_or_end::value::type == lexer::TokenType::TOKEN_RBRACKET, "Expected comma or right bracket");
                    }
                }
            }());

            using type = std::tuple_element_t<0, result>;
            using next = std::tuple_element_t<1, result>;
        };

        template<typename TokenList, bool First = true>
        struct ParserImpl_ParseObject {
            static_assert(!std::is_same_v<TokenList, meta::list::End>, "Expected more tokens");

            using result = decltype([]() {
                if constexpr (First && TokenList::value::type == lexer::TokenType::TOKEN_RBRACE) {
                    return std::tuple<meta::list::End, typename TokenList::next>{};
                } else {
                    using key = TokenList;
                    static_assert(key::value::type == lexer::TokenType::TOKEN_STRING, "Expected string for object key");
                    using colon = key::next;
                    static_assert(!std::is_same_v<colon, meta::list::End>, "Expected more tokens");
                    static_assert(colon::value::type == lexer::TokenType::TOKEN_COLON, "Expected colon after object key");
                    using value = ParserImpl_ParseElement<typename colon::next>;
                    using comma_or_end = typename value::next;
                    static_assert(!std::is_same_v<comma_or_end, meta::list::End>, "Expected more tokens");
                    if constexpr (comma_or_end::value::type == lexer::TokenType::TOKEN_COMMA) {
                        using next = ParserImpl_ParseObject<typename comma_or_end::next, false>;
                        return std::tuple<meta::list::Node<ObjectEntry<key::value::value, typename value::type>, typename next::type>, typename next::next>{};
                    } else if constexpr (comma_or_end::value::type == lexer::TokenType::TOKEN_RBRACE) {
                        return std::tuple<meta::list::Node<ObjectEntry<key::value::value, typename value::type>>, typename comma_or_end::next>{};
                    } else {
                        // Could be static_assert(false), but not allowed at the moment.
                        static_assert(comma_or_end::value::type == lexer::TokenType::TOKEN_RBRACE, "Expected comma or right brace");
                    }
                }
            }());

            using type = std::tuple_element_t<0, result>;
            using next = std::tuple_element_t<1, result>;
        };

        template<typename TokenList>
        struct ParserImpl_ParseElement {
            static_assert(!std::is_same_v<TokenList, meta::list::End>, "Expected more tokens");

            using result = decltype([]() {
                if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_NULL) {
                    return std::tuple<Null, typename TokenList::next>{};
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_TRUE) {
                    return std::tuple<Boolean<true>, typename TokenList::next>{};
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_FALSE) {
                    return std::tuple<Boolean<false>, typename TokenList::next>{};
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_NUMBER) {
                    if constexpr (TokenList::value::isInteger()) {
                        return std::tuple<Integer<TokenList::value::asInteger()>, typename TokenList::next>{};
                    } else {
                        return std::tuple<Number<TokenList::value::asDouble()>, typename TokenList::next>{};
                    }
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_STRING) {
                    return std::tuple<String<TokenList::value::value>, typename TokenList::next>{};
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_LBRACE) {
                    using nested = ParserImpl_ParseObject<typename TokenList::next>;
                    return std::tuple<meta::list::to_variadic_t<typename nested::type, Object>, typename nested::next>{};
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_RBRACE) {
                    static_assert(TokenList::value::type != lexer::TokenType::TOKEN_RBRACE, "Unexpected right brace");
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_COLON) {
                    static_assert(TokenList::value::type != lexer::TokenType::TOKEN_COLON, "Unexpected colon");
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_LBRACKET) {
                    using nested = ParserImpl_ParseList<typename TokenList::next>;
                    return std::tuple<meta::list::to_variadic_t<typename nested::type, List>, typename nested::next>{};
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_RBRACKET) {
                    static_assert(TokenList::value::type != lexer::TokenType::TOKEN_RBRACKET, "Unexpected right bracket");
                } else if constexpr (TokenList::value::type == lexer::TokenType::TOKEN_COMMA) {
                    static_assert(TokenList::value::type != lexer::TokenType::TOKEN_COMMA, "Unexpected comma");
                }
            }());

            using type = std::tuple_element_t<0, result>;
            using next = std::tuple_element_t<1, result>;
        };
    }// namespace parser

    template<typename TokenList>
    struct Parser {
        using parser = parser::ParserImpl_ParseElement<TokenList>;

        static_assert(!std::is_same_v<typename parser::next, meta::list::End>, "Expected more tokens");
        static_assert(parser::next::value::type == lexer::TokenType::TOKEN_END, "Expected end token");

        using type = typename parser::type;
    };

    template<JsonObject T>
    struct Printer {
        static std::ostream &call(std::ostream &os) {
            if constexpr (IsNull<T>) {
                return os << "null";
            } else if constexpr (IsBoolean<T>) {
                return os << (T::value ? "true" : "false");
            } else if constexpr (IsInteger<T> || IsNumber<T>) {
                return os << T::value;
            } else if constexpr (IsString<T>) {
                return os << std::quoted(T::value);
            } else if constexpr (IsList<T>) {
                os << "[";
                T::forEach([&os]<std::size_t Index, typename Entry>() {
                    if constexpr (Index > 0) {
                        os << ",";
                    }
                    Printer<Entry>::call(os);
                });
                os << "]";
                return os;
            } else if constexpr (IsObject<T>) {
                os << "{";
                T::forEach([&os]<std::size_t Index, meta::TString Key, typename Value>() {
                    if constexpr (Index > 0) {
                        os << ",";
                    }
                    os << std::quoted(Key.sv()) << ":";
                    Printer<Value>::call(os);
                });
                os << "}";
                return os;
            } else {
                static_assert(false, "Type not supported.");
            }
        }
    };

    /* Parser tests */
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NullToken<0>, lexer::EndToken<4>>>::type, Null>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::BooleanToken<0, true>, lexer::EndToken<4>>>::type, Boolean<true>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::BooleanToken<0, false>, lexer::EndToken<5>>>::type, Boolean<false>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NumberToken<0, 10, "1234567890">, lexer::EndToken<10>>>::type, Integer<1234567890>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NumberToken<0, 11, "-1234567890">, lexer::EndToken<11>>>::type, Integer<-1234567890>>);
    // Note: needs to be a perfectly representable float
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NumberToken<0, 8, "123.4e+5">, lexer::EndToken<8>>>::type, Number<12340000.>>);
    // Note: needs to be a perfectly representable float
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NumberToken<0, 9, "-123.4e+5">, lexer::EndToken<9>>>::type, Number<-12340000.>>);
    // Note: needs to be a perfectly representable float
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NumberToken<0, 8, "2375e-3">, lexer::EndToken<8>>>::type, Number<2.375>>);
    // Note: needs to be a perfectly representable float
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::NumberToken<0, 9, "-2375e-3">, lexer::EndToken<9>>>::type, Number<-2.375>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::StringToken<0, 7, "hello">, lexer::EndToken<7>>>::type, String<"hello">>);

    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 0, 1>,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 1, 2>,
                                                           lexer::EndToken<2>>>::type,
                                 List<>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 0, 1>,
                                                           lexer::NumberToken<1, 2, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 2, 3>, lexer::EndToken<3>>>::type,
                                 List<Integer<1>>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 0, 1>,
                                                           lexer::NumberToken<1, 2, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 2, 3>,
                                                           lexer::NumberToken<3, 4, "2">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 4, 5>,
                                                           lexer::EndToken<5>>>::type,
                                 List<Integer<1>, Integer<2>>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 0, 1>,
                                                           lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 0, 1>,
                                                           lexer::NumberToken<1, 2, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 2, 3>,
                                                           lexer::NumberToken<3, 5, "-2">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 5, 6>,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 6, 7>,
                                                           lexer::Token<lexer::TokenType::TOKEN_LBRACKET, 7, 8>,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 8, 9>,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 9, 10>,
                                                           lexer::StringToken<10, 17, "Hello">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 17, 18>,
                                                           lexer::NumberToken<18, 22, "3e+1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACKET, 22, 23>,
                                                           lexer::EndToken<23>>>::type,
                                 List<List<Integer<1>, Integer<-2>>, List<>, String<"Hello">, Number<30.>>>);

    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACE, 0, 1>,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACE, 1, 2>,
                                                           lexer::EndToken<2>>>::type,
                                 Object<>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACE, 0, 1>,
                                                           lexer::StringToken<1, 6, "key">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 6, 7>,
                                                           lexer::NumberToken<7, 8, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACE, 8, 9>, lexer::EndToken<9>>>::type,
                                 Object<ObjectEntry<"key", Integer<1>>>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACE, 0, 1>,
                                                           lexer::StringToken<1, 6, "key">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 6, 7>,
                                                           lexer::NumberToken<7, 8, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 8, 9>,
                                                           lexer::StringToken<9, 15, "test">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 15, 16>,
                                                           lexer::NumberToken<16, 17, "2">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACE, 17, 18>,
                                                           lexer::EndToken<18>>>::type,
                                 Object<ObjectEntry<"key", Integer<1>>, ObjectEntry<"test", Integer<2>>>>);
    static_assert(std::is_same_v<Parser<meta::list::make_t<lexer::Token<lexer::TokenType::TOKEN_LBRACE, 0, 1>,
                                                           lexer::StringToken<1, 6, "key">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 6, 7>,
                                                           lexer::NumberToken<7, 8, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 8, 9>,
                                                           lexer::StringToken<9, 23, "nested_empty">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 23, 24>,
                                                           lexer::Token<lexer::TokenType::TOKEN_LBRACE, 24, 25>,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACE, 25, 26>,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 26, 27>,
                                                           lexer::StringToken<27, 35, "nested">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 35, 36>,
                                                           lexer::Token<lexer::TokenType::TOKEN_LBRACE, 36, 37>,
                                                           lexer::StringToken<37, 42, "key">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 42, 43>,
                                                           lexer::NumberToken<43, 44, "1">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACE, 44, 45>,
                                                           lexer::Token<lexer::TokenType::TOKEN_COMMA, 45, 46>,
                                                           lexer::StringToken<46, 52, "test">,
                                                           lexer::Token<lexer::TokenType::TOKEN_COLON, 52, 53>,
                                                           lexer::NumberToken<53, 54, "2">,
                                                           lexer::Token<lexer::TokenType::TOKEN_RBRACE, 54, 55>,
                                                           lexer::EndToken<55>>>::type,
                                 Object<ObjectEntry<"key", Integer<1>>, ObjectEntry<"nested_empty", Object<>>,
                                        ObjectEntry<"nested", Object<ObjectEntry<"key", Integer<1>>>>, ObjectEntry<"test", Integer<2>>>>);

    /* End to end tests */
    static_assert(std::is_same_v<Parser<typename Lexer<R"([1, "Hello 2", 3])">::type>::type,
                                 List<Integer<1>, String<"Hello 2">, Integer<3>>>);
    static_assert(std::is_same_v<Parser<typename Lexer<R"({"1": 2, "3": 4})">::type>::type,
                                 Object<ObjectEntry<"1", Integer<2>>, ObjectEntry<"3", Integer<4>>>>);
    static_assert(std::is_same_v<Parser<typename Lexer<R"({"nested": {"key": ["wow", {"a": "bit more"}, "complex"]},
                                                           "floats and everything": [0, -10, 111, 123456789, 2500.0, -2375e-3]})">::type>::type,
                                 Object<ObjectEntry<"nested",
                                                    Object<ObjectEntry<"key",
                                                                       List<String<"wow">,
                                                                            Object<ObjectEntry<"a", String<"bit more">>>,
                                                                            String<"complex">>>>>,
                                        ObjectEntry<"floats and everything",
                                                    List<Integer<0>, Integer<-10>, Integer<111>, Integer<123456789>, Number<2500.>, Number<-2.375>>>>>);
}// namespace json

int main() {
    std::cout << "==== Printing ====\n";

    json::Printer<json::Null>::call(std::cout) << '\n';
    json::Printer<json::Boolean<true>>::call(std::cout) << '\n';
    json::Printer<json::Boolean<false>>::call(std::cout) << '\n';
    json::Printer<json::Integer<10>>::call(std::cout) << '\n';
    json::Printer<json::Number<3.14>>::call(std::cout) << '\n';
    json::Printer<json::String<"Test">>::call(std::cout) << '\n';
    json::Printer<json::List<json::Integer<1>, json::Integer<2>>>::call(std::cout) << '\n';
    json::Printer<json::Object<json::ObjectEntry<"1", json::Integer<2>>, json::ObjectEntry<"3", json::Integer<4>>>>::call(
            std::cout)
            << '\n';
    json::Printer<json::Object<json::ObjectEntry<"1", json::List<json::Null, json::Boolean<true>, json::Boolean<false>, json::Integer<9>, json::Number<2.6>, json::String<"Hi!!">, json::List<json::String<"Looks like">, json::Object<json::ObjectEntry<" a ", json::String<"good day!">>>>>>, json::ObjectEntry<"3", json::Integer<4>>>>::call(
            std::cout)
            << '\n';

    std::cout << "==== Tokenizing ====\n";

    []<typename... Tokens>(std::tuple<Tokens...>) {
        ((void) ([]() {
             if constexpr (Tokens::type == json::lexer::TokenType::TOKEN_ERROR) {
                 std::cout << "Error: " << Tokens::message.sv() << ": " << Tokens::json[Tokens::start] << std::endl;
             } else {
                 std::cout << static_cast<int>(Tokens::type) << ": " << Tokens::sv() << std::endl;
             }
         })(),
         ...);
    }(meta::list::to_tuple_t<typename json::Lexer<json_example>::type>{});

    std::cout << "==== Parsing ====\n";

    json::Printer<json::Parser<typename json::Lexer<json_example>::type>::type>::call(std::cout) << "\n";

    return 0;
}

#ifndef _MQCC_TOKEN_H__
#define _MQCC_TOKEN_H__

#include "srcpos.h"
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <variant>

enum TokenType {
    TK_NOT     = '!',  // !
    TK_DQUOTE  = '"',  // "
    TK_HASH    = '#',  // #
    TK_DOLLAR  = '$',  // $
    TK_MOD     = '%',  // %
    TK_BAND    = '&',  // &
    TK_SQUOTE  = '\'', // '
    TK_LRB     = '(',  // (
    TK_RRB     = ')',  // )
    TK_STAR    = '*',  // *
    TK_PLUS    = '+',  // +
    TK_COMMA   = ',',  // ,
    TK_MINUS   = '-',  // -
    TK_DOT     = '.',  // .
    TK_SLASH   = '/',  // /
    TK_COLON   = ':',  // :
    TK_SEMI    = ';',  // ;
    TK_LESS    = '<',  // <
    TK_ASSIGN  = '=',  // =
    TK_GREATER = '>',  // >
    TK_COND    = '?',  // ?
    TK_LSB     = '[',  // [
    TK_BSLASH  = '\\', //
    TK_RSB     = ']',  // ]
    TK_XOR     = '^',  // ^
    TK_LCB     = '{',  // {
    TK_BOR     = '|',  // |
    TK_RCB     = '}',  // }
    TK_BNOT    = '~',  // ~

    TK_ARROW = 128,   // ->
    TK_INC,           // ++
    TK_DEC,           // --
    TK_LSHIFT,        // <<
    TK_RSHIFT,        // >>
    TK_LEQUAL,        // <=
    TK_GEQUAL,        // >=
    TK_EQUAL,         // ==
    TK_NEQUAL,        // !=
    TK_LAND,          // &&
    TK_LOR,           // ||
    TK_ELLIPSIS,      // ...
    TK_MUL_ASSIGN,    // *=
    TK_DIV_ASSIGN,    // /=
    TK_MOD_ASSIGN,    // %=
    TK_ADD_ASSIGN,    // +=
    TK_MINUS_ASSIGN,  // -=
    TK_LSHIFT_ASSIGN, // <<=
    TK_RSHIFT_ASSIGN, // >>=
    TK_BAND_ASSIGN,   // &=
    TK_XOR_ASSIGN,    // ^=
    TK_BOR_ASSIGN,    // |=
    TK_DHASH,         // ##
    TK_ALSB,          // <:   alternative spelling of [
    TK_ARSB,          // :>   alternative spelling of ]
    TK_ALCB,          // <%   alternative spelling of {
    TK_ARCB,          // %>   alternative spelling of }
    TK_AHASH,         // %:   alternative spelling of #
    TK_ADHASH,        // %:%: alternative spelling of ##

    TK_ALIGNOF,        // alignof
    TK_AUTO,           // auto
    TK_BREAK,          // break
    TK_CASE,           // case
    TK_CHAR,           // char
    TK_CONST,          // const
    TK_CONTINUE,       // continue
    TK_DEFAULT,        // default
    TK_DO,             // do
    TK_DOUBLE,         // double
    TK_ELSE,           // else
    TK_ENUM,           // enum
    TK_EXTERN,         // extern
    TK_FLOAT,          // float
    TK_FOR,            // for
    TK_GOTO,           // goto
    TK_IF,             // if
    TK_INLINE,         // inline
    TK_INT,            // int
    TK_LONG,           // long
    TK_REGISTER,       // register
    TK_RESTRICT,       // restrict
    TK_RETURN,         // return
    TK_SHORT,          // short
    TK_SIGNED,         // signed
    TK_SIZEOF,         // sizeof
    TK_STATIC,         // static
    TK_STRUCT,         // struct
    TK_SWITCH,         // switch
    TK_TYPEDEF,        // typedef
    TK_UNION,          // union
    TK_UNSIGNED,       // unsigned
    TK_VOID,           // void
    TK_VOLATILE,       // volatile
    TK_WHILE,          // while
    TK__ALIGNAS,       // _Alignas
    TK__ATOMIC,        // _Atomic
    TK__BOOL,          // _Bool
    TK__COMPLEX,       // _Complex
    TK__GENERIC,       // _Generic
    TK__IMAGINARY,     // _Imaginary
    TK__NORETURN,      // _Noreturn
    TK__STATIC_ASSERT, // _Static_assert
    TK__THREAD_LOCAL,  // _Thread_local

    TK_STRING = 300,   // string literal
    TK_INT_LITERAL,    // integer number
    TK_UINT_LITERAL,   // integer number
    TK_LONG_LITERAL,   // integer number
    TK_ULONG_LITERAL,  // integer number
    TK_CHARACTER,      // char literal
    TK_FLOAT_LITERAL,  // float number
    TK_DOUBLE_LITERAL, // double number
    TK_NAME,           // identifier

    TK_EOF = 404, // end of file
};

class Token {
  public:
    int get_type() const noexcept { return _type; }
    // single character token and keywords
    explicit Token(int tp, const SourcePosition *);
    // multi characters token
    explicit Token(int tp, const char *literal, const SourcePosition *);
    // token with lexer value
    template <typename T>
    explicit Token(int tp, const SourcePosition *sp, const char *literal, T v)
        : _type(tp), _lexeme(literal), _sp(sp), _value(v) {}

    const char *get_lexeme() const noexcept { return _lexeme; }
    template <typename T> T value() const noexcept { return std::get<T>(_value); }
    const SourcePosition *get_position() const noexcept { return _sp; }
    bool is_ignored() const noexcept;
    bool is_type_token() const noexcept;
    bool is_storage_class() const noexcept;
    bool is_builtin_type() const noexcept;
    bool is_assign_operator() const noexcept;
    bool is_unary_operator() const noexcept;
    bool is_decl_start() const noexcept;
    bool is_comparator() const noexcept;

    static bool is_line_terminater(int);
    static int find_keyword(std::string_view);

    static const Token *make_token(int tp, const SourcePosition *sp);

    static const Token *make_token(int tp, const char *lexeme, const SourcePosition *sp) {
        return new Token(tp, lexeme, sp);
    }
    template <typename T>
    static const Token *make_token(int tp, const char *lexeme, const SourcePosition *sp, T v) {
        return new Token(tp, sp, lexeme, v);
    }

    static const Token *fake_name_token(const SourcePosition *);
    ~Token();

  private:
    int _type;
    const char *_lexeme;
    const SourcePosition *_sp;
    std::variant<std::int64_t, double, float, const char *> _value;
};

#endif

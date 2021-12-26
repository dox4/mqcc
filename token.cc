#include "token.h"
#include "error.h"
#include <array>
#include <cstring>
using namespace std;

static constexpr auto _keyword_start = TK_ALIGNOF;
static constexpr auto _keyword_end   = TK__THREAD_LOCAL + 1;

// punctuators
static constexpr array<const char *, 128> _ascii = {
    "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "",  "",   "",   "",  "",
    "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "!", "\"", "#",  "$", "%",
    "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "",  "",  "",  "", "",  "",   "",   "",  "",
    "",  ":", ";", "<", "=", ">", "?", "@", "",  "",  "",  "",  "",  "", "",  "",   "",   "",  "",
    "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "",  "[",  "\\", "]", "^",
    "_", "`", "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "",  "",   "",   "",  "",
    "",  "",  "",  "",  "",  "",  "",  "",  "",  "{", "|", "}", "~", "",
};

static constexpr array<const char *, _keyword_end - _keyword_start> _keywords = {
    "alignof",
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Generic",
    "_Imaginary",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
};

/// token groups

// "void", "_Bool", "char", "short", "int", "long", "struct", "union",
// "typedef", "enum", "static", "extern", "_Alignas", "signed", "unsigned",
// "const", "volatile", "auto", "register", "restrict", "__restrict",
// "__restrict__", "_Noreturn", "float", "double", "typeof", "inline",
// "_Thread_local", "__thread", "_Atomic",
#define IGNORED() TK_INLINE, TK_VOLATILE, TK_AUTO, TK_CONST, TK_REGISTER, TK_RESTRICT, TK__NORETURN

#define TYPE_TOKENS()                                                                              \
    TK_VOID, TK__BOOL, TK_CHAR, TK_SHORT, TK_INT, TK_LONG, TK_STRUCT, TK_UNION, TK_TYPEDEF,        \
        TK_ENUM, TK_STATIC, TK_EXTERN, TK__ALIGNAS, TK_SIGNED, TK_UNSIGNED, TK_CONST, TK_VOLATILE, \
        TK_AUTO, TK_REGISTER, TK_RESTRICT, /*TK___RESTRICT, TK___RESTRICT__,*/ TK__NORETURN,       \
        TK_FLOAT, TK_DOUBLE, /*TK_TYPEOF,*/ TK_INLINE, TK__THREAD_LOCAL,                           \
        /*TK___THREAD,*/ TK__ATOMIC

#define STORAGE_CLASS() TK_EXTERN, TK_STATIC, TK_TYPEDEF, TK_INLINE, TK__THREAD_LOCAL

#define BUILTIN_TYPE()                                                                             \
    TK_VOID, TK__BOOL, TK_CHAR, TK_SIGNED, TK_UNSIGNED, TK_INT, TK_SHORT, TK_LONG, TK_FLOAT,       \
        TK_DOUBLE

#define ASSIGN_OP()                                                                                \
    TK_ASSIGN, TK_MUL_ASSIGN, TK_DIV_ASSIGN, TK_MOD_ASSIGN, TK_ADD_ASSIGN, TK_MINUS_ASSIGN,        \
        TK_LSHIFT_ASSIGN, TK_RSHIFT_ASSIGN, TK_BAND_ASSIGN, TK_XOR_ASSIGN, TK_BOR_ASSIGN
#define UNARY_OP() '&', '*', '+', '-', '~', '!'
#define COMP() '>', '<', TK_EQUAL, TK_NEQUAL, TK_GEQUAL, TK_LEQUAL
/// token groups end

/// helper functions

static inline constexpr bool eq(int x, int y) { return x == y; }

template <int... N> static bool one_of(int x) { return (eq(N, x) || ...); }

/// helper functions end

static bool is_keyword(int tk) { return _keyword_start <= tk && tk < _keyword_end; }

static constexpr auto _literal_start = TK_STRING;
static constexpr auto _literal_end   = TK_NAME + 1;

static bool is_literal(int tk) { return _literal_start <= tk && tk <= _literal_end; }

bool Token::is_ignored() const noexcept { return one_of<IGNORED()>(get_type()); }

bool Token::is_type_token() const noexcept { return one_of<TYPE_TOKENS()>(get_type()); }

bool Token::is_storage_class() const noexcept { return one_of<STORAGE_CLASS()>(get_type()); }

bool Token::is_builtin_type() const noexcept { return one_of<BUILTIN_TYPE()>(get_type()); }

bool Token::is_assign_operator() const noexcept { return one_of<ASSIGN_OP()>(get_type()); }

bool Token::is_unary_operator() const noexcept { return one_of<UNARY_OP()>(get_type()); }

bool Token::is_decl_start() const noexcept { return is_type_token(); }

bool Token::is_comparator() const noexcept { return one_of<COMP()>(get_type());}

int Token::find_keyword(string_view word) {
    for (size_t i = 0; i < _keywords.size(); i++) {
        auto w = _keywords.at(i);
        if (strlen(w) == word.size() && strncmp(w, word.data(), word.size()) == 0)
            return i;
    }
    return -1;
}

bool Token::is_line_terminater(int ch) { return ch == '\n'; }

Token::Token(int tp, const char *literal, const SourcePosition *sp)
    : _type(tp), _lexeme(literal), _sp(sp) {}

Token::Token(int tp, const SourcePosition *sp) : _type(tp), _sp(sp) {
    if (tp < 128) {
        _lexeme = _ascii[tp];
    } else if (is_keyword(tp)) {
        _lexeme = _keywords[tp - _keyword_start];
    } else if (tp == TK_EOF) {
        _lexeme = "<eof>";
    }
}

Token::~Token() {
    if (is_literal(_type)) {
        delete[] _lexeme;
    }
}

const Token *Token::make_token(int tp, const SourcePosition *sp) { return new Token(tp, sp); }

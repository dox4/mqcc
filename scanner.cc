#include "scanner.h"
#include "error.h"
#include "parser.h"
#include "token.h"

#include <cctype>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

/// Scanner

Scanner::Scanner(const char *file_name) { _sp = new SourcePosition(file_name); }
Scanner::Scanner(const char *file_name, std::string src) {
    _sp = new SourcePosition(file_name, src);
}
const Token *Scanner::make_token(int tp) { return Token::make_token(tp, _sp->copy()); }
const Token *Scanner::make_token(int tp, const char *literal) {
    return Token::make_token(tp, literal, _sp->copy());
}

void Scanner::next() { _sp->next(); }

bool Scanner::try_next(const char *pattern) { return _sp->try_next(pattern); }
bool Scanner::try_next(int ch) { return _sp->try_next(ch); }
int Scanner::peek() { return _sp->peek(); }
void Scanner::expect(int ch) {
    if (peek() != ch) {
        error_unexpected(static_cast<char>(ch), static_cast<char>(peek()));
    }
    next();
}

bool Scanner::test(int ch) { return (peek() != ch); }

void Scanner::advance(int step) {}

void Scanner::skip_spaces() {
    while (isspace(peek()))
        next();
}

const Token *Scanner::get_token() {
    skip_spaces();
    int ch = peek();
    switch (ch) {
    case '\0':
        return make_token(TK_EOF);
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case ';':
    case ':':
    case '?':
    case ',':
        next();
        return make_token(ch);
    case '+':
        next();
        if (try_next('='))
            return make_token(TK_ADD_ASSIGN, "+=");
        else if (try_next('+'))
            return make_token(TK_INC, "++");
        return make_token(TK_PLUS);
    case '-':
        next();
        if (try_next('='))
            return make_token(TK_MINUS_ASSIGN, "-=");
        if (try_next('-'))
            return make_token(TK_DEC, "--");
        return make_token(TK_MINUS);
    case '%':
        next();
        if (try_next('='))
            return make_token(TK_MOD_ASSIGN, "%=");
        return make_token(TK_MOD);
    case '*':
        next();
        if (try_next('='))
            return make_token(TK_MUL_ASSIGN, "*=");
        return make_token(TK_STAR);
    case '/':
        next();
        if (try_next('='))
            return make_token(TK_DIV_ASSIGN, "/=");
        return make_token(TK_SLASH);
    case '<':
        next();
        if (try_next('<')) {
            if (try_next('='))
                return make_token(TK_LSHIFT_ASSIGN, "<<=");
            return make_token(TK_LSHIFT, "<<");
        } else if (try_next('='))
            return make_token(TK_LEQUAL, "<=");
        return make_token(TK_LESS);
    case '>':
        next();
        if (try_next('>')) {
            if (try_next('='))
                return make_token(TK_RSHIFT_ASSIGN, ">>=");
            return make_token(TK_RSHIFT, ">>");
        } else if (try_next('='))
            return make_token(TK_GEQUAL, ">=");
        return make_token(TK_GREATER);
    case '&':
        next();
        if (try_next('&'))
            return make_token(TK_LAND, "&&");
        if (try_next('='))
            return make_token(TK_BAND_ASSIGN, "&=");
        return make_token(TK_BAND);
    case '|':
        next();
        if (try_next('|'))
            return make_token(TK_LOR, "||");
        if (try_next('='))
            return make_token(TK_BOR_ASSIGN, "|=");
        return make_token(TK_BOR);
    case '^':
        next();
        if (try_next('='))
            return make_token(TK_XOR_ASSIGN, "^=");
        return make_token(TK_XOR);
    case '!':
        next();
        if (try_next('='))
            return make_token(TK_NEQUAL, "!=");
        return make_token(TK_NOT);
    case '=':
        next();
        if (try_next('='))
            return make_token(TK_EQUAL, "==");
        return make_token(TK_ASSIGN);
    case '0' ... '9':
        return get_number();
    case '\"':
        return get_string();
    default: {
        auto tk = get_name_or_keyword();
        if (tk->is_ignored())
            return get_token();
        return tk;
    }
    }
}

const Token *Scanner::get_number() {
    auto start = _sp->current();
    while (isdigit(peek()))
        next();

    // floating number
    if (try_next('.')) {
        while (isdigit(peek()))
            next();
        try_next('f');
        auto end      = _sp->current();
        auto size     = end - start;
        char *literal = new char[size + 1];
        strncpy(literal, start, size);
        literal[size] = '\0';
        return make_token(TK_FNUMBER, literal);
    }
    // integer number postfix
    try_next('u') || try_next('U');
    try_next('l') || try_next('L');
    try_next('l') || try_next('L');

    auto end      = _sp->current();
    auto size     = end - start;
    char *literal = new char[size + 1];
    strncpy(literal, start, size);
    literal[size] = '\0';
    return make_token(TK_INUMBER, literal);
}

const Token *Scanner::get_name_or_keyword() {
    auto start = _sp->current();
    while (isalnum(peek()) || peek() == '_') {
        next();
    }
    auto end  = _sp->current();
    auto size = end - start;
    auto word = std::string_view(start, size);
    int index;
    if ((index = Token::find_keyword(word)) != -1)
        return make_token(index + TK_ALIGNOF);

    char *literal = new char[size + 1];
    strncpy(literal, word.data(), size);
    literal[size] = '\0';
    return make_token(TK_NAME, literal);
}

#define isoctal(c) ((c) >= '0' && (c) < '8')
int hex_to_int(int x) {
    if ('0' <= x && '9' >= x)
        return x - '0';
    return tolower(x) - 'a' + 10;
}

int Scanner::read_escape() {
    expect('\\');
    // read octal char
    // \NNN, max 3 octal numbers
    if (isoctal(peek())) {
        int o = peek() - '0';
        next();
        if (isoctal(peek())) {
            o = (o << 3) + (peek() - '0');
            next();
            if (isoctal(peek())) {
                o = (o << 3) + (peek() - '0');
                next();
            }
        }
        if (o > 256)
            error_at(make_token(peek()), "escape sequence out of range.");
        return o;
    }
    // read hex char
    if (peek() == 'x') {
        next();
        if (!isxdigit(peek()))
            error_at(make_token(peek()), "expect hex number sequence.");

        int x = 0;
        while (isxdigit(peek())) {
            x = (x << 4) + hex_to_int(peek());
            next();
        }
        if (x > 256)
            error_at(make_token(peek()), "escape sequence out of range.");
        return x;
    }
    // read escape char
    switch (peek()) {
    case 'a':
        next();
        return '\a';
    case 'b':
        next();
        return '\b';
    case 't':
        next();
        return '\t';
    case 'n':
        next();
        return '\n';
    case 'v':
        next();
        return '\v';
    case 'f':
        next();
        return '\f';
    case 'r':
        next();
        return '\r';
    // from chibicc
    // [GNU] \e for the ASCII escape character is a GNU C extension.
    case 'e':
        next();
        return 27;
    case '\'':
        next();
        return '\'';
    case '"':
        next();
        return '"';
    }

    auto c = peek();
    next();
    return c;
}
const Token *Scanner::get_string() {
    expect('\"');
    string buffer;
    int ch;
    while ((ch = peek()) != '\"') {
        if (ch == '\\') {
            buffer.push_back(static_cast<char>(read_escape()));
        } else {
            buffer.push_back(ch);
            next();
        }
    }
    expect('\"');
    char *literal = new char[buffer.size() + 1];
    strcpy(literal, buffer.c_str());
    return make_token(TK_STRING, literal);
}

/// Scanner

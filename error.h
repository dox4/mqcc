#ifndef _MQCC_ERROR_H__
#define _MQCC_ERROR_H__
#include <cstdio>
#include <stdarg.h>

// forward declaration
class Type;
class Token;
class SourcePosition;
// forward declaration end

inline void error_unexpected(char expected, char actual) {
    std::fprintf(stderr, "expected %c, but got %c.\n", expected, actual);
}

#ifdef DEBUG
#define debug(...)                                                                                 \
    do {                                                                                           \
        fprintf(stderr, "%s:%d:", __FILE__, __LINE__);                                             \
        fprintf(stderr, __VA_ARGS__);                                                              \
        fprintf(stderr, "\n");                                                                     \
    } while (false)

#define mqassert(expr, ...)                                                                        \
    do {                                                                                           \
        if (!(expr)) {                                                                             \
            fprintf(stderr, "%s:%d: ", __FILE__, __LINE__);                                        \
            fprintf(stderr, __VA_ARGS__);                                                          \
            fprintf(stderr, "\n");                                                                 \
            exit(1);                                                                               \
        }                                                                                          \
    } while (false)
#define set_mark() debug("set a mark at %s:%d", __FILE__, __LINE__)
#else
#define debug(...) (void)0
#define mqassert(expr, ...) (void)0
#endif

#define debug_enter_func() debug("enter function: %s", __func__)

#define debug_token(tk)                                                                            \
    do {                                                                                           \
        debug("%s:", __func__);                                                                    \
        if (tk == nullptr)                                                                         \
            debug("got nullptr.");                                                                 \
        else                                                                                       \
            warn_token(tk, "got token: (%s, %d)", tk->get_lexeme(), tk->get_type());               \
    } while (false)

void error(const char *fmt, ...);
void error_at(const Token *, const char *fmt, ...);
void error_invalid_oprands(const Token *tok, const Type *t1, const Type *t2);
void warn_at(const Token *, const char *fmt, ...);
void warn_token(const Token *, const char *, ...);
#define unreachable() (error("%s:%d: unreachable code.", __FILE__, __LINE__))
#define unimplement()                                                                              \
    (error("%s:%d: umimplemented function invoked: %s", __FILE__, __LINE__, __func__))

#endif

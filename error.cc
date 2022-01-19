#include "error.h"
#include "token.h"
#include "type.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
using namespace std;

// Reports an error and exit.
void error(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void error_invalid_oprands(const Token *tok, const Type *t1, const Type *t2) {
    error_at(tok, "invalid oprands for binary oprator (%s): `%s` and `%s`.", tok->get_lexeme(),
             t1->normalize().c_str(), t2->normalize().c_str());
}

void error_at(const Token *tok, const char *fmt, ...) {
    auto sp = tok->get_position();
    int indent =
        fprintf(stderr, "%s:%d:%d:", sp->get_file_name(), sp->get_line(), sp->get_column());
    fprintf(stderr, "%s\n", sp->current_line());
    fprintf(stderr, "%*s", indent + sp->get_column() - 1, "");
    fprintf(stderr, "^ ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void warn_at(const Token *tok, const char *fmt, ...) {
    auto sp = tok->get_position();
    int indent =
        fprintf(stderr, "%s:%d:%d:", sp->get_file_name(), sp->get_line(), sp->get_column());
    fprintf(stderr, "%s\n", sp->current_line());
    fprintf(stderr, "%*s", indent + sp->get_column() - 1, "");
    fprintf(stderr, "^ ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}

void warn_token(const Token *tk, const char *fmt, ...) {
    do {
        auto sp = tk->get_position();
        int indent =
            fprintf(stderr, "%s:%d:%d:", sp->get_file_name(), sp->get_line(), sp->get_column());
        fprintf(stderr, "%s\n", sp->current_line());
        fprintf(stderr, "%*s", indent + sp->get_column() - 1, "");
        fprintf(stderr, "^ ");
        va_list ap;
        va_start(ap, fmt);
        vfprintf(stderr, fmt, ap);
        fprintf(stderr, "\n");
    } while (false);
}

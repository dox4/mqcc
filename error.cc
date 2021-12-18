#include "error.h"
#include "token.h"

#include <cstdio>
#include <cstdlib>
using namespace std;

// Reports an error and exit.
void error(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void error_at(const SourcePosition *sp, const char *fmt, ...) {
    int indent =
        fprintf(stderr, "%s:%d:%d:", sp->get_file_name(), sp->get_line(), sp->get_column());
    fprintf(stderr, "%s", sp->current_line());
    fprintf(stderr, "%*s", indent + sp->get_column() - 1, "");
    fprintf(stderr, "^ ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void warn_at(const SourcePosition *sp, const char *fmt, ...) {
    int indent =
        fprintf(stderr, "%s:%d:%d:", sp->get_file_name(), sp->get_line(), sp->get_column());
    fprintf(stderr, "%s", sp->current_line());
    fprintf(stderr, "%*s", indent + sp->get_column() - 1, "");
    fprintf(stderr, "^ ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}

void warn_token(const Token *tk, const char *fmt, ...) {
    do {
        auto sp = tk->get_postion();
        int len = strlen(tk->get_lexeme());
        int indent =
            fprintf(stderr, "%s:%d:%d:", sp->get_file_name(), sp->get_line(), sp->get_column() - len);
        fprintf(stderr, "%s", sp->current_line());
        fprintf(stderr, "%*s", indent + sp->get_column() - len - 1, "");
        fprintf(stderr, "^ ");
        va_list ap;
        va_start(ap, fmt);
        vfprintf(stderr, fmt, ap);
        fprintf(stderr, "\n");
    } while (false);
}

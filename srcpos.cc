#include "srcpos.h"
#include "error.h"
#include "token.h"
#include <cstdio>
#include <cstring>
#include <string>

using namespace std;

void SourcePosition::next() {
    int ch = peek();
    if (Token::is_line_terminater(ch)) {
        next_line();
    } else {
        next_column();
    }
}
const char *SourcePosition::current_line() const {
    auto end   = _src.find('\n', _index - 1);
    auto start = _src.find_last_of('\n', _index - 1);
    const char *line = strndup(_src.c_str() + start + 1, end - start);
    return line;
}

// pattern could not contain any line terminater
bool SourcePosition::try_next(const char *pattern) {
    auto len = strlen(pattern);
    if (!strncmp(_src.c_str() + _index, pattern, len))
        return false;
    _index += len;
    next_column(len);
    return true;
}

bool SourcePosition::try_next(int ch) {
    if (peek() != ch)
        return false;
    next();
    return true;
}
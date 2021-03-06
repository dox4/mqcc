#include "srcpos.h"
#include "error.h"
#include "token.h"
#include <cstdio>
#include <cstring>
#include <string>

using namespace std;

const char *last_of(const char *start, int index, int dest) {
    while (index != 0) {
        if (start[index--] == dest)
            return start + index + 1;
    }
    return start;
}

void SourcePosition::next() {
    int ch = peek();
    if (Token::is_line_terminater(ch)) {
        next_line();
    } else {
        next_column();
    }
}
const char *SourcePosition::current_line() const {
    // auto end   = _src.find('\n', _index - 1);
    // auto start = _src.find_last_of('\n', _index - 1);
    auto end         = strchr(_src + _index, '\n');
    auto start       = last_of(_src, _index, '\n');
    const char *line = strndup(start + 1, end - start - 1);
    return line;
}

// pattern could not contain any line terminater
bool SourcePosition::try_next(const char *pattern) {
    auto len = strlen(pattern);
    if (!strncmp(_src + _index, pattern, len))
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

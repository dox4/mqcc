#ifndef _MQCC_SCANNER_H__
#define _MQCC_SCANNER_H__
#include <string>

/// forward declaration

// token.h
class Token;

// srcpos.h
class SourcePosition;

/// forward declaration end

class Scanner {
  public:
    explicit Scanner(const char *file_name);
    explicit Scanner(const char *file_name, std::string src);
    const Token *get_token();

  private:
    SourcePosition *_sp;
    void next();
    int peek();
    void expect(int ch);
    bool test(int ch);
    bool try_next(const char *pattern);
    bool try_next(int ch);
    void advance(int step);
    void skip_spaces();
    void skip_line_comment();
    void skip_block_comment();
    const Token *get_number();
    int read_escape();
    const Token *get_string();
    const Token *get_name_or_keyword();
    const Token *make_token(int tp);
    const Token *make_token(int tp, const char *literal);
};

#endif

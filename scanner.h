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
    // explicit Scanner(const char *file_name);
    explicit Scanner(const char *file_name, const char *src);
    const Token *get_token();

  private:
    SourcePosition *_sp;
    const SourcePosition *_token_start;
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
    void set_token_start();
    const char *dup_lexeme();
    const Token *get_number();
    const Token *create_int_token(int, int);
    int read_escape();
    const Token *get_string();
    const Token *get_char();
    const Token *get_name_or_keyword();
    const Token *make_token(int tp);
    const Token *make_token(int tp, const char *literal);
    template <typename T> const Token *make_token(int tp, const char *literal, T v);
};

#endif

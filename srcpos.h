#ifndef _MQCC_SRC_POS_H__
#define _MQCC_SRC_POS_H__
#include <cstring>
#include <string>
extern std::string read_file(const char *file_name);

class SourcePosition {
  private:
    unsigned _line, _column;
    unsigned long _index;
    const char *_file_name;
    const std::string _src;

  public:
    SourcePosition(const char *file_name)
        : _line(1), _column(1), _index(0), _file_name(file_name), _src(read_file(file_name)){};
    SourcePosition(const char *filename, std::string src)
        : _line(1), _column(1), _index(0), _file_name(filename), _src(src) {}
    SourcePosition(const SourcePosition *other)
        : _line(other->_line), _column(other->_column), _index(other->_index),
          _file_name(other->_file_name), _src(other->_src) {}

    const char *current() const noexcept { return _src.c_str() + _index; }
    const char *get_file_name() const noexcept { return _file_name; }

    int peek() const noexcept { return _index == _src.size() ? '\0' : _src.at(_index); }
    void next();

    bool try_next(int ch);
    bool try_next(const char *);

    void next_column() noexcept { next_column(1); }
    void next_column(int step) noexcept {
        _column += step;
        _index += step;
    }
    void next_line() noexcept {
        _column = 1;
        _index++;
        _line++;
    }
    const char *current_line() const;
    const char *get_line_begin() const noexcept { return _src.c_str() + (_index - _column); }
    const char *get_line_end() const noexcept {
        int idx = _index;
        while (_src[idx] != '\n' && _src[idx] != '\0')
            idx++;
        return this->_src.c_str() + idx;
    }
    unsigned get_line() const noexcept { return _line; }
    unsigned get_column() const noexcept { return _column; }

    const SourcePosition *copy_and_backward(int step) const noexcept {
        auto newsp = new SourcePosition(this);
        newsp->backward(step - 1);
        return newsp;
    }
    void backward(int step) { _column -= step; }
};

#endif // _MQCC_SRC_POS_H__

#ifndef _MQCC_SCOPE_H__
#define _MQCC_SCOPE_H__

#include <cstddef>
#include <map>
#include <string>
#include <string_view>
#include <unordered_map>

/// forward declaration

/// type.h
class Object;
class Type;

/// forward declaration end

class Scope {
  public:
    explicit Scope() = default;
    explicit Scope(Scope *parent) : _parent(parent), _tags(), _vars() {}

//     Object *resolve_name(const std::string_view &);
//     Object *resolve_name_in_local(const std::string_view &);
// 
//     const Type *find_typedef(std::string_view) const;
//     void push_typedef(std::string_view, const Type *);

    const Type *find_tag_in_local(const std::string_view &) const;
    const Type *find_tag(const std::string_view &) const;
    Type *find_mut_tag(const std::string_view &);
    Type *find_mut_tag_in_local(const std::string_view &);

    Object *find_var(const std::string_view &) const;
    Object *find_var_in_local(const std::string_view &) const;
    size_t size() const noexcept { return _vars.size(); }
    void push_tag(std::string_view, Type *);
    void push_var(std::string_view, Object *);
    Scope *drill_down();
    Scope *float_up();
    int offset() const noexcept { return _offset; }

    std::map<std::string_view, Object *> local_vars() const noexcept { return _vars; }

    std::string obj_to_string() const;

  private:
    Scope *_parent;
    int _offset = 0;
    // struts or unions
    std::unordered_map<std::string_view, Type *> _tags;
    // typedefs
    // std::unordered_map<std::string_view, const Type *> _typedefs;
    // global variables, local variables or functions
    std::map<std::string_view, Object *> _vars;
};

#endif

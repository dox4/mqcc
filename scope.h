#ifndef _MQCC_SCOPE_H__
#define _MQCC_SCOPE_H__

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
    explicit Scope(Scope *parent) : _parent(parent), _types(), _vars() {}
    // bool has_name(std::string_view);
    // Object *find_variable(std::string_view);
    // Type *find_typedef(std::string_view );
    // Type *find_struct_or_union(std::string_view);
    // void push_name(std::string_view, Object *);
    // void push_typedef(std::string_view, Type *);
    // void push_func(std::string_view, Object *);
    // void push_struct_or_union(std::string_view, Type *);

    Object *resolve_name(const std::string_view &);
    Object *resolve_name_in_local(const std::string_view &);

    const Type *find_type_in_local(const std::string_view &) const;
    const Type *find_type(const std::string_view &) const;
    Type *find_mut_type(const std::string_view &);
    Type *find_mut_type_in_local(const std::string_view &);

    Object *find_var(const std::string_view &) const;
    Object *find_var_in_local(const std::string_view &) const;
    size_t size() const noexcept { return _vars.size(); }
    void push_type(std::string_view, Type *);
    void push_var(std::string_view, Object *);
    Scope *drill_down();
    Scope *float_up();

    std::string obj_to_string() const;

  private:
    Scope *_parent;
    int _offset = 0;
    // struts or unions
    std::unordered_map<std::string_view, Type *> _types;
    // typedefs
    std::unordered_map<std::string_view, Type *> _typedefs;
    // global variables, local variables or functions
    std::unordered_map<std::string_view, Object *> _vars;
};

#endif

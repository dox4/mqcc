#include "scope.h"
#include "type.h"
#include <sstream>
using namespace std;

// bool Scope::has_name(std::string_view name) {
//     return this->_symbols.find(name) != this->_symbols.end() ||
//            (_parent != nullptr && _parent->has_name(name));
// }
//
// Type *Scope::find_typedef(std::string_view) { return nullptr; }
// Object *Scope::find_variable(const std::string_view key) {
//     if (_symbols.find(key) != _symbols.end())
//         return _symbols[key];
//     if (_parent == nullptr)
//         return nullptr;
//     return _parent->find_variable(key);
// }
//
// void Scope::push_name(std::string_view key, Object *obj) { _symbols.insert({key, obj}); }
// void Scope::push_func(std::string_view key, Object *obj) {}

Object *Scope::resolve_name(const std::string_view &name) {
    if (_vars.find(name) != _vars.end())
        return _vars.at(name);
    return nullptr;
}

Object *Scope::resolve_name_in_local(const std::string_view &name) { return nullptr; }

const Type *Scope::find_type_in_local(const std::string_view &name) const {
    if (_types.find(name) != _types.end())
        return _types.at(name);
    return nullptr;
}

Object *Scope::find_var_in_local(const std::string_view &name) const {
    if (_vars.find(name) != _vars.end())
        return _vars.at(name);
    return nullptr;
}

const Type *Scope::find_type(const std::string_view &name) const {
    auto in_local = find_type_in_local(name);
    return in_local ? in_local : _parent == nullptr ? nullptr : _parent->find_type(name);
}

Type *Scope::find_mut_type(const std::string_view &name) {
    auto in_local = find_mut_type_in_local(name);
    return in_local ? in_local : _parent == nullptr ? nullptr : _parent->find_mut_type(name);
}

Type *Scope::find_mut_type_in_local(const std::string_view &name) {
    if (_types.find(name) != _types.end())
        return _types.at(name);
    return nullptr;
}

Object *Scope::find_var(const std::string_view &name) {
    auto in_local = find_var_in_local(name);
    return in_local ? in_local : _parent == nullptr ? nullptr : _parent->find_var(name);
}

void Scope::push_var(std::string_view ident, Object *obj) {
    auto size = obj->type()->size();
    if (obj->offset() == INT32_MIN)
        obj->set_offset(_offset -= size);
    _vars.insert({ident, obj}); 
}

Scope *Scope::drill_down() { return new Scope(this); }

Scope *Scope::float_up() { return _parent; }

string Scope::obj_to_string() const {
    stringstream ss;
    for (auto key : this->_vars) {
        ss << key.first << ": " << key.second->type()->normalize() << endl;
    }
    return ss.str();
}

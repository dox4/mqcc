#include "scope.h"
#include "type.h"
#include <cstdint>
#include <sstream>
using namespace std;

// Object *Scope::resolve_name(const std::string_view &name) {
//     if (_vars.find(name) != _vars.end())
//         return _vars.at(name);
//     return nullptr;
// }
// 
// Object *Scope::resolve_name_in_local(const std::string_view &name) { return nullptr; }
// 
// const Type *Scope::find_typedef(std::string_view name) const {
//     auto nt = _typedefs.find(name);
//     if (nt == _typedefs.end())
//         return _parent == nullptr ? nullptr : _parent->find_typedef(name);
//     return nt->second;
// }
// void Scope::push_typedef(std::string_view name, const Type *type) {
//     _typedefs.insert({name, type});
// }
const Type *Scope::find_tag_in_local(const std::string_view &name) const {
    if (_tags.find(name) != _tags.end())
        return _tags.at(name);
    return nullptr;
}

Object *Scope::find_var_in_local(const std::string_view &name) const {
    if (_vars.find(name) != _vars.end())
        return _vars.at(name);
    return nullptr;
}

const Type *Scope::find_tag(const std::string_view &name) const {
    auto in_local = find_tag_in_local(name);
    return in_local ? in_local : _parent == nullptr ? nullptr : _parent->find_tag(name);
}

Type *Scope::find_mut_tag(const std::string_view &name) {
    auto in_local = find_mut_tag_in_local(name);
    return in_local ? in_local : _parent == nullptr ? nullptr : _parent->find_mut_tag(name);
}

Type *Scope::find_mut_tag_in_local(const std::string_view &name) {
    if (_tags.find(name) != _tags.end())
        return _tags.at(name);
    return nullptr;
}

Object *Scope::find_var(const std::string_view &name) const {
    auto in_local = find_var_in_local(name);
    return in_local ? in_local : _parent == nullptr ? nullptr : _parent->find_var(name);
}

void Scope::push_tag(std::string_view tag, Type *type) { _tags.insert({tag, type}); }
void Scope::push_var(std::string_view ident, Object *obj) {
    auto size = obj->type()->size();
    _offset += size;
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

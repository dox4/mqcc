#include "type.h"
#include "error.h"

using namespace std;

// const Type *Type::Dummy = new Type(TY_DUMMY, 0);

const BuiltinType BuiltinType::Void    = BuiltinType(TY_VOID, "void", 1, false);
const BuiltinType BuiltinType::Char    = BuiltinType(TY_CHAR, "char", 1, true);
const BuiltinType BuiltinType::UChar   = BuiltinType(TY_CHAR, "unsigned char", 1, false);
const BuiltinType BuiltinType::Short   = BuiltinType(TY_SHORT, "short", 2, true);
const BuiltinType BuiltinType::UShort  = BuiltinType(TY_SHORT, "unsigned short", 2, false);
const BuiltinType BuiltinType::Int     = BuiltinType(TY_INT, "int", 4, true);
const BuiltinType BuiltinType::UInt    = BuiltinType(TY_INT, "unsigned int", 4, false);
const BuiltinType BuiltinType::Long    = BuiltinType(TY_LONG, "long", 8, true);
const BuiltinType BuiltinType::ULong   = BuiltinType(TY_LONG, "unsigned long", 8, false);
const BuiltinType BuiltinType::Float   = BuiltinType(TY_FLOAT, "float", 4, true);
const BuiltinType BuiltinType::Double  = BuiltinType(TY_DOUBLE, "double", 8, true);
const BuiltinType BuiltinType::LDouble = BuiltinType(TY_LDOUBLE, "long double", 16, true);

const Type *uac(const Type *l, const Type *r) {
    if (!l->is_arithmetic() || !r->is_arithmetic())
        error("usual arithmetic conversions could only apply on arithmetic types");
    // debug("left type: %s, right type: %s", l->normalize().c_str(), r->normalize().c_str());
    // the same
    if (l->equals_to(r))
        return l;
    // long double
    if (l == &BuiltinType::LDouble || r == &BuiltinType::LDouble)
        return &BuiltinType::LDouble;
    // double
    if (l == &BuiltinType::Double || r == &BuiltinType::Double)
        return &BuiltinType::Double;
    // float
    if (l == &BuiltinType::Float || r == &BuiltinType::Float)
        return &BuiltinType::Float;
    // integer promotion
    // to the type with greater size
    if (l->size() > r->size())
        return l;
    if (l->size() < r->size())
        return r;
    // both types have the same size
    auto lb = static_cast<const BuiltinType *>(l);
    // if l is signed, return r
    // r must be unsigned, or l and r will have the same type
    if (lb->is_signed())
        return r;
    return l;
}

const std::string FuncType::normalize() const {
    std::string result = _ret->normalize() + "(";
    for (size_t idx = 0; idx < _params.size(); idx++) {
        result += _params.at(idx)->type()->normalize();
        if (idx != _params.size() - 1)
            result += ", ";
    }
    return result + ")";
}
bool FuncType::equals_to(const Type *other) const {
    if (other->kind() != TY_FUNC)
        return false;
    auto ft = static_cast<const FuncType *>(other);
    if (_ret->equals_to(ft->_ret))
        return false;
    if (_params.size() != ft->_params.size())
        return false;
    for (size_t i = 0; i < _params.size(); i++)
        if (!_params[i]->type()->equals_to(ft->_params[i]->type()))
            return false;
    return true;
}

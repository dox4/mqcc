#ifndef _MQCC_TYPE_H__
#define _MQCC_TYPE_H__
#include "error.h"
#include <stdint.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

/// forward declaration
class Token;
/// forward declaration end

enum TypeKind {
    TY_VOID,    // void
    TY_BOOL,    // _Bool
    TY_CHAR,    // char
    TY_SHORT,   // short
    TY_INT,     // int
    TY_LONG,    // long
    TY_FLOAT,   // float
    TY_DOUBLE,  // double
    TY_LDOUBLE, // long double
    TY_ENUM,    // enum
    TY_PTR,     // *TYPE
    TY_FUNC,    // function
    TY_ARRAY,   // array
    TY_VLA,     // variable-length array
    TY_STRUCT,  // struct
    TY_UNION,   // union
};

class Type {
  protected:
    TypeKind _kind;
    int _size;
    std::string_view _name;
    explicit Type(TypeKind kind, int size, std::string_view name)
        : _kind(kind), _size(size), _name(name) {}

  public:
    // static const Type *Dummy;
    TypeKind kind() const noexcept { return _kind; }
    virtual const std::string normalize() const = 0;
    virtual const int size() const noexcept { return _size; };
    virtual std::string_view name() const { return _name; }
    virtual bool is_void() const noexcept { return false; }
    virtual bool is_arithmetic() const noexcept { return false; }
    virtual bool is_float() const noexcept { return false; }
    virtual bool is_integer() const noexcept { return false; }
    virtual bool is_signed() const noexcept { return false; }
    virtual bool is_pointer() const noexcept { return false; }
    virtual bool equals_to(const Type *other) const { return this == other; }
    virtual bool is_compitable_with(const Type *other) const { return this == other; }
    // virtual const int align() const = 0;
    // virtual bool is_atomic() const  = 0;
};

// storage class and other attributes of variables or functions
struct Attribute {
    bool is_typedef      = false;
    bool is_static       = false;
    bool is_extern       = false;
    bool is_inline       = false;
    bool is_thread_local = false;
    int align            = 0;

    bool equals_to(const Attribute *other) {
        return is_typedef == other->is_typedef && is_static == other->is_static &&
               is_extern == other->is_extern && is_inline == other->is_inline &&
               is_thread_local == other->is_thread_local && align == other->align;
    }
};

class HalfType {
  public:
    explicit HalfType(const Token *token, const Type *type) : _token(token), _type(type) {}
    const Token *token() const noexcept { return _token; }
    const Type *type() const noexcept { return _type; }
    // Type *mut_type() const { return _type; }

  private:
    const Token *_token;
    const Type *_type;
};

// class to represent variables or functions
// - global variables
// - functions
// - function parameters
// - local variables
class Object {
  public:
    Object(const Token *ident, const Type *type, Attribute *attr)
        : _ident(ident), _type(type), _attr(attr) {}
    const Type *type() const noexcept { return _type; }
    const Attribute *attr() const noexcept { return _attr; }
    Attribute *mut_attr() { return _attr; }
    const Token *ident() const noexcept { return _ident; }
    bool is_function() const noexcept { return _type->kind() == TY_FUNC; }
    bool is_global() const noexcept { return !is_local(); }
    bool is_local() const noexcept { return _attr->is_static; }
    bool is_defined() const noexcept { return _is_defined; }
    void set_defined(bool is_defined) { _is_defined = is_defined; }
    void set_offset(int offset) { _offset = offset; }
    int offset() const noexcept { return _offset; }

  private:
    const Token *_ident;
    const Type *_type;
    Attribute *_attr;
    bool _is_defined = true;
    int _offset      = INT32_MIN;
};

class BuiltinType : public Type {
  private:
    BuiltinType(TypeKind kind, const char *s, int size, bool is_signed = true)
        : Type(kind, size, s), _normalize(s), _is_signed(is_signed) {}
    const char *_normalize;
    bool _is_signed = true;

  public:
    static const BuiltinType Void;
    static const BuiltinType Char;
    static const BuiltinType UChar;
    static const BuiltinType Short;
    static const BuiltinType UShort;
    static const BuiltinType Int;
    static const BuiltinType UInt;
    static const BuiltinType Long;
    static const BuiltinType ULong;
    static const BuiltinType Float;
    static const BuiltinType Double;
    static const BuiltinType LDouble;
    virtual const std::string normalize() const { return _normalize; }
    virtual const int size() const noexcept { return _size; }
    virtual bool is_signed() const noexcept { return _is_signed; }
    virtual bool is_arithmetic() const noexcept { return this != &Void; }
    virtual bool is_void() const noexcept { return this == &Void; }
    virtual bool is_float() const noexcept {
        return this == &Double || this == &Float || this == &LDouble;
    }
    virtual bool is_integer() const noexcept { return is_arithmetic() && !is_float(); }
    virtual bool is_compitable_with(const Type *that) const {
        if (Type::is_compitable_with(that))
            return true;
        mqassert(this != &Void && that != &Void, "void is uncompitable with any types.");
        if (that->is_pointer()) {
            // TODO show a warning when true
            return is_integer();
        }
        return that->is_arithmetic();
    }
};

class PointerType : public Type {
  public:
    explicit PointerType(const Type *point_to) : Type(TY_PTR, 8, ""), _point_to(point_to) {}
    const Type *point_to() const { return _point_to; }
    virtual const std::string normalize() const { return _point_to->normalize() + "*"; }
    virtual const int size() const noexcept { return 8; };
    virtual bool is_pointer() const noexcept { return true; }
    virtual bool is_compitable_with(const Type *that) const {
        if (Type::is_compitable_with(that)) {
            return true;
        }
        if (that->is_pointer()) {
            return true;
        }
        // long
        if (that->is_integer()) {
            if (that->size() < size()) {
                // TODO: warn that cast from smaller integer to pointer
            }
            return true;
        }
        return false;
    }

  private:
    const Type *_point_to;
};

class StructType : public Type {
  private:
    std::vector<Object *> _members;
};
class UnionType : public StructType {};
class ArrayType : public Type {};
class FuncType : public Type {
  public:
    explicit FuncType(const Type *ret, std::string_view name)
        : Type(TY_FUNC, 1, name), _ret(ret), _params() {}
    explicit FuncType(const Type *ret, std::string_view name, std::vector<const HalfType *> params)
        : Type(TY_FUNC, 1, name), _ret(ret), _params(params) {}
    const std::vector<const HalfType *> parameters() const noexcept { return _params; }
    const Type *return_type() const noexcept { return _ret; }
    virtual const std::string normalize() const;
    virtual const int size() const noexcept { return 1; };
    virtual bool equals_to(const Type *other) const;

  private:
    const Type *_ret;
    std::vector<const HalfType *> _params;
};

// usual arithmetic conversions
const Type *uac(const Type *, const Type *);

#endif

#ifndef _MQCC_TYPE_H__
#define _MQCC_TYPE_H__
#include "error.h"
#include <cstddef>
#include <cstdint>
#include <list>
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

// calculate align
int align_to(int offset, int align);
class StructType;
class FuncType;
class Derefed;
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
    virtual const std::uint64_t size() const noexcept { return _size; };
    virtual std::string_view name() const { return _name; }
    virtual bool is_void() const noexcept { return false; }
    virtual bool is_arithmetic() const noexcept { return false; }
    virtual bool is_scalar() const noexcept { return is_arithmetic() || is_pointer(); }
    virtual bool is_float() const noexcept { return false; }
    virtual bool is_integer() const noexcept { return false; }
    virtual bool is_signed() const noexcept { return false; }
    virtual bool is_pointer() const noexcept { return false; }
    virtual bool is_derefed() const noexcept { return false; }
    virtual const Type *derefed() const noexcept {
        unimplement();
        return nullptr;
    }
    virtual bool equals_to(const Type *other) const { return this == other; }
    virtual bool is_compitable_with(const Type *other) const { return this == other; }
    virtual const Type *point_to() const {
        unimplement();
        return nullptr;
    }
    virtual bool is_function() const noexcept { return false; }
    virtual const FuncType *as_function() const noexcept {
        unimplement();
        return nullptr;
    }
    virtual bool is_array() const noexcept { return false; }
    virtual const int align() const {
        unimplement();
        return -1;
    };
    virtual bool is_struct() const noexcept { return false; }
    virtual bool is_complete() const noexcept { return true; }
    virtual Derefed *as_derefed() noexcept {
        unimplement();
        return nullptr;
    }
    virtual const StructType *as_struct() const noexcept {
        unimplement();
        return nullptr;
    }
    virtual StructType *as_struct() noexcept {
        unimplement();
        return nullptr;
    }
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
    bool is_static() const noexcept { return _attr->is_static; }
    bool is_global() const noexcept { return _is_global; }
    void set_global() noexcept { _is_global = true; }
    bool is_defined() const noexcept { return _is_defined; }
    void set_defined(bool is_defined) { _is_defined = is_defined; }
    void set_offset(int offset) { _offset = offset; }
    int offset() const noexcept { return _offset; }

  private:
    const Token *_ident;
    const Type *_type;
    Attribute *_attr;
    bool _is_defined = true;
    bool _is_global  = false; // for global variables
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
    static const BuiltinType Bool;
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
    virtual const std::uint64_t size() const noexcept { return _size; }
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
    virtual const int align() const { return _size; }
};

class Derefed : public Type {
  public:
    explicit Derefed(const Type *derefed, TypeKind kind, std::uint64_t size)
        : Type(kind, size, ""), _derefed(derefed) {}
    virtual bool is_derefed() const noexcept { return true; }
    virtual const Type *derefed() const noexcept { return _derefed; }
    virtual Derefed *as_derefed() noexcept { return this; }
    virtual const int align() const { return 8; }
    virtual void set_derefed(const Type *derefed) { _derefed = derefed; }

  private:
    const Type *_derefed;
};

class PointerType : public Derefed {
  public:
    explicit PointerType(const Type *point_to) : Derefed(point_to, TY_PTR, 8) {}
    static const PointerType *point_to(const Type *type);
    virtual const Type *point_to() const { return derefed(); }
    virtual const std::string normalize() const { return derefed()->normalize() + "*"; }
    virtual const std::uint64_t size() const noexcept { return 8; };
    virtual bool is_pointer() const noexcept { return true; }
    virtual bool is_compitable_with(const Type *that) const {
        if (Type::is_compitable_with(that)) {
            return true;
        }
        if (that->is_derefed()) {
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
};

class Member {
  public:
    explicit Member(const Type *type, const Token *name) : _type(type), _name(name) {}
    const Type *type() const noexcept { return _type; }
    const Token *name() const noexcept { return _name; }
    void set_offset(int offset) { _offset = offset; }
    int offset() const noexcept { return _offset; }

  private:
    int _offset = 0;
    const Type *_type;
    const Token *_name;
};

class StructType : public Type {
  public:
    explicit StructType(const Token *name, std::list<Member *> members)
        : Type(TY_STRUCT, 0, ""), _tag(name) {
        set_members(members);
    }
    const Token *tag() const noexcept { return _tag; }
    virtual const std::string normalize() const;
    virtual const std::uint64_t size() const noexcept { return _size; };
    virtual const int align() const { return _align; }
    virtual bool is_complete() const noexcept { return _is_complete; }
    virtual void set_complete(bool is_complete) noexcept { _is_complete = is_complete; }
    virtual void set_members(std::list<Member *>);

    virtual bool is_struct() const noexcept { return true; }
    virtual bool is_union() const noexcept { return false; }
    virtual const StructType *as_struct() const noexcept { return this; }
    virtual StructType *as_struct() noexcept { return this; }
    Member *find_member(const char *name) const;
    const std::list<Member *> &members() const noexcept { return _members; }

  protected:
    void set_align(int align) { _align = align; }
    std::list<Member *> _members;

  private:
    const Token *_tag;
    int _align        = 1;
    bool _is_complete = false;
};
class UnionType : public StructType {
  public:
    virtual bool is_union() const noexcept { return true; }
    explicit UnionType(const Token *name, std::list<Member *> members) : StructType(name, members) {
        set_members(members);
    }
    virtual void set_members(std::list<Member *>);

  private:
};
class ArrayType : public Derefed {
  public:
    explicit ArrayType(const Type *base, size_t size)
        : Derefed(base, TY_ARRAY, base->size() * size), _cap(size) {}
    virtual const Type *elem_type() const noexcept { return derefed(); }
    size_t cap() const noexcept { return _cap; }

    virtual const std::string normalize() const {
        return elem_type()->normalize() + "[" + std::to_string(_cap) + "]";
    }
    virtual bool is_array() const noexcept { return true; }
    virtual const std::uint64_t size() const noexcept { return _size; };
    virtual const int align() const { return elem_type()->align(); }

  private:
    size_t _cap;
};
class FuncType : public Type {
  public:
    explicit FuncType(const Type *ret, std::string_view name)
        : Type(TY_FUNC, 1, name), _ret(ret), _params() {}
    explicit FuncType(const Type *ret, std::string_view name, std::vector<const HalfType *> params)
        : Type(TY_FUNC, 1, name), _ret(ret), _params(params) {}
    const std::vector<const HalfType *> parameters() const noexcept { return _params; }
    const Type *return_type() const noexcept { return _ret; }
    virtual const std::string normalize() const;
    virtual const std::uint64_t size() const noexcept { return 1; };
    virtual bool equals_to(const Type *other) const;
    virtual bool is_function() const noexcept { return true; }
    virtual const FuncType *as_function() const noexcept { return this; }

  private:
    const Type *_ret;
    std::vector<const HalfType *> _params;
};

// usual arithmetic conversions
const Type *uac(const Type *, const Type *);
#endif

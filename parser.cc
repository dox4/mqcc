#include "parser.h"
#include "ast.h"
#include "error.h"
#include "scanner.h"
#include "scope.h"
#include "token.h"
#include "type.h"

#include <cstdint>
#include <cstdlib>
#include <iterator>
#include <list>
#include <sstream>
#include <utility>
#include <vector>

using namespace std;

/// static functions

// clang-format off
// static ExprKind get_expr_kind(const Token* tok) {
//     switch (tok->get_type()) {
//         case TK_STAR   : return EXPR_MUL;     // *
//         case TK_SLASH  : return EXPR_DIV;     // /
//         case TK_MOD    : return EXPR_MOD;     // %
//         case TK_PLUS   : return EXPR_ADD;     // +
//         case TK_MINUS  : return EXPR_SUB;     // -
//         case TK_LSHIFT : return EXPR_BLS;     // <<
//         case TK_RSHIFT : return EXPR_BRS;     // >>
//         case TK_LESS   : return EXPR_LESS;    // <
//         case TK_GREATER: return EXPR_GREATER; // >
//         case TK_LEQUAL : return EXPR_LEQUAL;  // <=
//         case TK_GEQUAL : return EXPR_GEQUAL;  // >=
//         case TK_EQUAL  : return EXPR_EQUAL;   // ==
//         case TK_NEQUAL : return EXPR_NEQUAL;  // !=
//         case TK_BAND    : return EXPR_BAND;    // &
//         case TK_XOR    : return EXPR_BXOR;    // ^
//         case TK_BOR     : return EXPR_BOR;     // |
//         case TK_LAND   : return EXPR_LAND;    // &&
//         case TK_LOR    : return EXPR_LOR;     // ||
//         default:
//             error_at(tok, "%s is not a binary token",  tok->get_lexeme());
//     }
//     unreachable();
//     // no warning
//     return EXPR_INT;
// }
// clang-format on

// binary operator priority
enum BOP {
    OP_LOR,
    OP_LAND,
    OP_BOR,
    OP_BXOR,
    OP_BAND,
    OP_EQUAL,
    OP_RELATION,
    OP_SHIFT,
    OP_ADD,
    OP_MUL,
    OP_NOT_VALID,
};

static Expr *conv(const Type *type, Expr *expr) {
    if (expr->kind() == EXPR_FUNC_CALL)
        return expr;
    if (expr->type()->equals_to(type))
        return expr;
    if (!type->is_compitable_with(expr->type())) {
        error_at(expr->token(), "uncompitable type with implicit conversion: from %s to %s",
                 expr->type()->normalize().c_str(), type->normalize().c_str());
    }
    return new Conv(type, expr);
}

static void check_scalar(const Type *type, const Token *tok) {
    if (!type->is_scalar())
        error_at(tok, "expression requires scalar type here.");
}

static void check_type_integers(Expr *e) {
    Binary *b = static_cast<Binary *>(e);
    if (!(b->lhs()->type()->is_integer() && b->rhs()->type()->is_integer()))
        error_invalid_oprands(b->token(), b->lhs()->type(), b->rhs()->type());
}

static void check_type_arithmetics(Expr *e) {
    Binary *b = static_cast<Binary *>(e);
    if (!(b->lhs()->type()->is_arithmetic() && b->rhs()->type()->is_arithmetic()))
        error_invalid_oprands(b->token(), b->lhs()->type(), b->rhs()->type());
}

static void check_type_for_relational(Expr *e) {
    Relational *b = static_cast<Relational *>(e);
    if ((b->lhs()->type()->is_arithmetic() && b->rhs()->type()->is_arithmetic()))
        return;
    if (b->lhs()->type()->is_pointer() && b->rhs()->type()->is_pointer()) {
        if (!b->lhs()->type()->derefed()->is_compitable_with(b->rhs()->type()->derefed()))
            warn_at(b->token(), "comparison of distinct pointer types ('%s' and '%s').",
                    b->lhs()->type()->normalize().c_str(), b->rhs()->type()->normalize().c_str());
        return;
    }
    error_invalid_oprands(b->token(), b->lhs()->type(), b->rhs()->type());
}

static void swap_binary_oprands(Binary *b) {
    auto rhs = b->rhs();
    b->set_rhs(b->lhs());
    b->set_lhs(rhs);
}

static void apply_uac_on_binary(Expr *e) {
    Binary *b     = static_cast<Binary *>(e);
    auto uac_type = uac(b->lhs()->type(), b->rhs()->type());
    b->set_lhs(conv(uac_type, b->lhs()));
    b->set_rhs(conv(uac_type, b->rhs()));
}

static void check_type_for_additive(Expr *e) {
    Add *a = static_cast<Add *>(e);
    // when two oprands are both arithmetic types
    if (a->lhs()->type()->is_arithmetic() && a->rhs()->type()->is_arithmetic()) {
        apply_uac_on_binary(a);
        return;
    }
    // or when left oprand is a pointer and right is an integer
    if (a->lhs()->type()->is_derefed()) {
        if (a->rhs()->type()->is_integer())
            return;
    }
    // otherwise, if one of the oprands is derefed.
    if (a->token()->get_type() == '-') {
        if (a->lhs()->type()->is_derefed() && a->rhs()->type()->is_derefed()) {
            if (!a->lhs()->type()->derefed()->equals_to(a->rhs()->type()->derefed()))
                error_invalid_oprands(a->token(), a->lhs()->type(), a->rhs()->type());
            return;
        }
    } else {
        if (a->rhs()->type()->is_derefed()) {
            if (a->lhs()->type()->is_integer()) {
                // make sure derefed oprand is placed at lhs when an integer is added on a pointer.
                swap_binary_oprands(a);
                return;
            }
        }
    }
    error_invalid_oprands(a->token(), a->lhs()->type(), a->rhs()->type());
}

static void check_type_scalars(Expr *e) {
    Binary *b = static_cast<Binary *>(e);
    if (!(b->lhs()->type()->is_scalar() && b->rhs()->type()->is_scalar()))
        error_invalid_oprands(b->token(), b->lhs()->type(), b->rhs()->type());
}

// static void check_type_for_binary(Expr *lhs, Expr *rhs, const Token *tok) {
// #define isarith(t) (t->is_arithmetic())
// #define isint(t) (t->is_integer())
// #define isptr(t) (t->is_pointer())
// #define bothint(t1, t2) (isint(t1) && (isint(t2)))
// #define botharith(t1, t2) ((isarith(t1)) && (isarith(t2)))
// #define bothptr(t1, t2) ((isptr(t1) && isptr(t2)))
//     auto ltype = lhs->type(), rtype = rhs->type();
//     switch (tok->get_type()) {
//     case TK_STAR:  // *
//     case TK_SLASH: // /
//         if (!botharith(ltype, rtype))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         break;
//     case TK_MOD:    // %
//     case TK_LSHIFT: // <<
//     case TK_RSHIFT: // >>
//     case TK_BAND:   // &
//     case TK_XOR:    // ^
//     case TK_BOR:    // |
//         if (!bothint(ltype, rtype))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         break;
//     case TK_PLUS: // +
//         if (!(botharith(ltype, rtype) || (isint(ltype) && (isptr(rtype) || rtype->is_array())) ||
//               ((isptr(ltype) || ltype->is_array()) && isint(rtype))))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         break;
//     case TK_MINUS: { // -
//         if (!(botharith(ltype, rtype) || (isptr(ltype) && isint(rtype)) ||
//               (isptr(ltype) && isptr(rtype))))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         if (isptr(ltype) && isptr(rtype))
//             if (!ltype->point_to()->equals_to(rtype->point_to()))
//                 error_invalid_oprands(tok, lhs->type(), rhs->type());
//         break;
//     }
//     case TK_LESS:    // <
//     case TK_GREATER: // >
//     case TK_LEQUAL:  // <=
//     case TK_GEQUAL:  // >=
//         if (!(botharith(ltype, rtype) || bothptr(ltype, rtype)))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         if (bothptr(ltype, rtype) && ltype->point_to()->is_compitable_with(rtype->point_to()))
//             warn_at(tok, "comparison of distinct pointer types ('%s' and '%s'.",
//                     ltype->normalize().c_str(), rtype->normalize().c_str());
//         break;
//     case TK_EQUAL:  // ==
//     case TK_NEQUAL: // !=
//         if (!((botharith(ltype, rtype)) || bothptr(ltype, rtype)))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         if (bothptr(ltype, rtype)) {
//             if (!ltype->point_to()->equals_to(ltype->point_to()))
//                 warn_at(tok, "comparison of distinct pointer types ('%s' and '%s'.",
//                         ltype->normalize().c_str(), rtype->normalize().c_str());
//         }
//         break;
//     case TK_LAND: // &&
//     case TK_LOR:  // ||
//         if (!(ltype->is_scalar() && rtype->is_scalar()))
//             error_invalid_oprands(tok, lhs->type(), rhs->type());
//         break;
//     default:
//         unreachable();
//     }
// #undef isarith
// #undef isint
// #undef isptr
// #undef bothint
// #undef botharith
// #undef bothptr
// }

// static int get_priority(int tok_type) {
//     switch (tok_type) {
//     case TK_STAR:  // *
//     case TK_SLASH: // /
//     case TK_MOD:   // %
//         return OP_MUL;
//     case TK_PLUS:  // +
//     case TK_MINUS: // -
//         return OP_ADD;
//     case TK_LSHIFT: // <<
//     case TK_RSHIFT: // >>
//         return OP_SHIFT;
//     case TK_LESS:    // <
//     case TK_GREATER: // >
//         return OP_RELATION;
//     case TK_LEQUAL: // <=
//     case TK_GEQUAL: // >=
//     case TK_EQUAL:  // ==
//     case TK_NEQUAL: // !=
//         return OP_EQUAL;
//     case TK_BAND: // &
//         return OP_BAND;
//     case TK_XOR: // ^
//         return OP_BXOR;
//     case TK_BOR: // |
//         return OP_BOR;
//     case TK_LAND: // &&
//         return OP_LAND;
//     case TK_LOR: // ||
//         return OP_LOR;
//     default:
//         return OP_NOT_VALID;
//     }
// }

// static Expr *wrap_array(Expr *expr) {
//     auto type = expr->type();
//     if (!type->is_array())
//         return expr;
//     while (type->is_array()) {
//         debug("current type: %s", type->normalize().c_str());
//         type = static_cast<const ArrayType *>(type)->elem_type();
//     }
//     return new Conv(new PointerType(type), expr);
// }

// static Expr *wrap_pointer(Expr *expr, const Type *type) {
//     if (type->is_pointer()) {
//         return new Binary(EXPR_MUL, nullptr, conv(&BuiltinType::ULong, expr),
//                           new IntConst(nullptr, type->point_to()->size()));
//     }
//     if (type->is_array()) {
//         return new Binary(
//             EXPR_MUL, nullptr, conv(&BuiltinType::ULong, expr),
//             new IntConst(nullptr, static_cast<const ArrayType *>(type)->elem_type()->size()));
//     }
//     return expr;
// }

/// static functions end

Parser::Parser(Scanner *scanner) : _scanner(scanner), _lookups(), _consumed() {
    _scope = new Scope();
}

// function-definitions and declarations both start with
// `declaration-specifiers` and `declarator`
// so we can parse them first and dispatch parsing actions
// by next token or if the parsed declarator indicate a function
TransUnit *Parser::parse() {
    list<ExtDecl *> decls;
    while (!test(TK_EOF)) {
        auto attr      = new Attribute;
        auto base_type = parse_declaration_specifiers(attr);
        // typedef
        if (attr->is_typedef) {
            // auto type = parse_typedef(base_type);
        } else {
            // global variable or function declaration/definition
            // both start with declarator
            ExtDecl *current;
            auto prefix = parse_declarator(base_type);
            auto var    = _scope->find_var_in_local(prefix->token()->get_lexeme());
            if (var == nullptr) {
                auto gdecl = new Object(prefix->token(), prefix->type(), attr);
                if (test('{')) {
                    current = parse_func_def(prefix);
                } else if (try_next(';')) {
                    gdecl->set_defined(false);
                } else
                    current = parse_global_variable(prefix->type());
                _scope->push_var(prefix->token()->get_lexeme(), gdecl);
            } else {
                auto vt = var->type();
                if (!vt->equals_to(prefix->type())) {
                    error_at(prefix->token(), "type redefined conflict: '%s' vs '%s'",
                             vt->normalize().c_str(), prefix->type()->normalize().c_str());
                }
                if (var->is_function()) {
                    if (test('{')) {
                        if (var->is_defined())
                            error_at(prefix->token(), "function redefined.");
                        current = parse_func_def(prefix);
                    } else if (try_next(';')) {
                        // _scope->push_type(prefix->token()->get_lexeme(), prefix->type());
                    }
                } else
                    current = parse_global_variable(prefix->type());
            }
            decls.push_back(current);
        }
    }
    return new TransUnit(decls);
}

Block *Parser::parse_global_variable(const Type *base) { return nullptr; }

const Type *Parser::parse_typedef(const Type *base) { return nullptr; }

void Parser::process_storage_class(Attribute *attr) {
    auto tk = peek();
#define error_dup_token(tk)                                                                        \
    error_at(tk, "storage class %s has already been specified.", tk->get_lexeme())
    switch (tk->get_type()) {
    case TK_EXTERN: {
        if (attr->is_extern)
            error_dup_token(tk);
        attr->is_extern = true;
        break;
    }
    case TK_STATIC: {
        if (attr->is_static)
            error_dup_token(tk);
        attr->is_static = true;
        break;
    }
    case TK_INLINE: {
        if (attr->is_inline)
            error_dup_token(tk);
        attr->is_inline = true;
        break;
    }
    case TK_TYPEDEF: {
        if (attr->is_typedef)
            error_dup_token(tk);
        attr->is_typedef = true;
        break;
    }
    default: {
        if (attr->is_thread_local)
            error_dup_token(tk);
        attr->is_thread_local = true;
        break;
    }
    }
    if (attr->is_typedef &&
        (attr->is_extern || attr->is_inline || attr->is_static || attr->is_thread_local))
        error_at(tk, "typedef may not work with extern, inline, "
                     "static, or some storage class else.");
    next();
#undef error_dup_toke
}

struct BuiltinTypeEnum {
    // single keyword
    static constexpr type_counter_t VOID     = 1 << 0;
    static constexpr type_counter_t CHAR     = 1 << 2;
    static constexpr type_counter_t SHORT    = 1 << 4;
    static constexpr type_counter_t INT      = 1 << 6;
    static constexpr type_counter_t LONG     = 1 << 8;
    static constexpr type_counter_t FLOAT    = 1 << 12;
    static constexpr type_counter_t DOUBLE   = 1 << 14;
    static constexpr type_counter_t SIGNED   = 1 << 16;
    static constexpr type_counter_t UNSIGNED = 1 << 17;

    // all builtin types
    // — char
    // — signed char
    static constexpr type_counter_t SCHAR = SIGNED | CHAR;
    // — unsigned char
    static constexpr type_counter_t UCHAR = UNSIGNED | CHAR;
    // — short, signed short, short int, or signed short int
    static constexpr type_counter_t SSHORT     = SIGNED | SHORT;
    static constexpr type_counter_t SHORT_INT  = SHORT | INT;
    static constexpr type_counter_t SSHORT_INT = SIGNED | SHORT | INT;
    // — unsigned short, or unsigned short int
    static constexpr type_counter_t USHORT     = UNSIGNED | SHORT;
    static constexpr type_counter_t USHORT_INT = UNSIGNED | SHORT | INT;
    // — int, signed, or signed int
    static constexpr type_counter_t SINT = SIGNED | INT;
    // — unsigned, or unsigned int
    static constexpr type_counter_t UINT = UNSIGNED | INT;
    // — long, signed long, long int, or signed long int
    static constexpr type_counter_t SLONG     = SIGNED | LONG;
    static constexpr type_counter_t LONG_INT  = LONG | INT;
    static constexpr type_counter_t SLONG_INT = SIGNED | LONG | INT;
    // — unsigned long, or unsigned long int
    static constexpr type_counter_t ULONG     = UNSIGNED | LONG;
    static constexpr type_counter_t ULONG_INT = UNSIGNED | LONG | INT;
    // — long long, signed long long, long long int, or
    //   signed long long int
    static constexpr type_counter_t LLONG      = LONG + LONG;
    static constexpr type_counter_t SLLONG     = LLONG | SIGNED;
    static constexpr type_counter_t LLONG_INT  = LLONG | INT;
    static constexpr type_counter_t SLLONG_INT = LLONG | INT | SIGNED;
    // — unsigned long long, or unsigned long long int
    static constexpr type_counter_t ULLONG     = LLONG | UNSIGNED;
    static constexpr type_counter_t ULLONG_INT = LLONG | INT | UNSIGNED;
    // — float
    // — double
    // — long double
    static constexpr type_counter_t LDOUBLE = LONG | DOUBLE;
    // — _Bool
    // — float _Complex
    // — double _Complex
    // — long double _Complex
};

type_counter_t Parser::process_builtin(type_counter_t type_counter) {
    auto tk = peek();
    // process type couter
    switch (tk->get_type()) {
    case TK_VOID:
        type_counter += BuiltinTypeEnum::VOID;
        break;
    case TK_CHAR:
        type_counter += BuiltinTypeEnum::CHAR;
        break;
    case TK_SHORT:
        type_counter += BuiltinTypeEnum::SHORT;
        break;
    case TK_INT:
        type_counter += BuiltinTypeEnum::INT;
        break;
    case TK_LONG:
        type_counter += BuiltinTypeEnum::LONG;
        break;
    case TK_FLOAT:
        type_counter += BuiltinTypeEnum::FLOAT;
        break;
    case TK_DOUBLE:
        type_counter += BuiltinTypeEnum::DOUBLE;
        break;
    // times of `signed` and `unsigned` does not matter
    case TK_SIGNED:
        type_counter |= BuiltinTypeEnum::SIGNED;
        break;
    case TK_UNSIGNED:
        type_counter |= BuiltinTypeEnum::UNSIGNED;
        break;
    default:
        unreachable();
    }
    // all candidate builtin types:
    switch (type_counter) {
    case BuiltinTypeEnum::CHAR:
    case BuiltinTypeEnum::SCHAR:
    case BuiltinTypeEnum::UCHAR:
    case BuiltinTypeEnum::SHORT:
    case BuiltinTypeEnum::SSHORT:
    case BuiltinTypeEnum::SHORT_INT:
    case BuiltinTypeEnum::SSHORT_INT:
    case BuiltinTypeEnum::USHORT:
    case BuiltinTypeEnum::USHORT_INT:
    case BuiltinTypeEnum::INT:
    case BuiltinTypeEnum::SIGNED:
    case BuiltinTypeEnum::SINT:
    case BuiltinTypeEnum::UNSIGNED:
    case BuiltinTypeEnum::UINT:
    case BuiltinTypeEnum::LONG:
    case BuiltinTypeEnum::SLONG:
    case BuiltinTypeEnum::LONG_INT:
    case BuiltinTypeEnum::SLONG_INT:
    case BuiltinTypeEnum::ULONG:
    case BuiltinTypeEnum::ULONG_INT:
    case BuiltinTypeEnum::LLONG:
    case BuiltinTypeEnum::SLLONG:
    case BuiltinTypeEnum::LLONG_INT:
    case BuiltinTypeEnum::SLLONG_INT:
    case BuiltinTypeEnum::ULLONG:
    case BuiltinTypeEnum::ULLONG_INT:
    case BuiltinTypeEnum::FLOAT:
    case BuiltinTypeEnum::DOUBLE:
    case BuiltinTypeEnum::LDOUBLE: {
        next();
        return type_counter;
    }

    default:
        error_at(tk, "malformed type.");
    }
    unreachable();
    // no warning
    return -1;
}

static const Type *get_builtin_type(type_counter_t type_counter) {
    switch (type_counter) {
    case BuiltinTypeEnum::CHAR:
    case BuiltinTypeEnum::SCHAR:
        return &BuiltinType::Char;
    case BuiltinTypeEnum::UCHAR:
        return &BuiltinType::UChar;
    case BuiltinTypeEnum::SHORT:
    case BuiltinTypeEnum::SSHORT:
    case BuiltinTypeEnum::SHORT_INT:
    case BuiltinTypeEnum::SSHORT_INT:
        return &BuiltinType::Short;
    case BuiltinTypeEnum::USHORT:
    case BuiltinTypeEnum::USHORT_INT:
        return &BuiltinType::UShort;
    case BuiltinTypeEnum::INT:
    case BuiltinTypeEnum::SIGNED:
    case BuiltinTypeEnum::SINT:
        return &BuiltinType::Int;
    case BuiltinTypeEnum::UNSIGNED:
    case BuiltinTypeEnum::UINT:
        return &BuiltinType::UInt;
    case BuiltinTypeEnum::LONG:
    case BuiltinTypeEnum::SLONG:
    case BuiltinTypeEnum::LONG_INT:
    case BuiltinTypeEnum::SLONG_INT:
    case BuiltinTypeEnum::LLONG:
    case BuiltinTypeEnum::SLLONG:
    case BuiltinTypeEnum::LLONG_INT:
    case BuiltinTypeEnum::SLLONG_INT:
        return &BuiltinType::Long;
    case BuiltinTypeEnum::ULONG:
    case BuiltinTypeEnum::ULONG_INT:
    case BuiltinTypeEnum::ULLONG:
    case BuiltinTypeEnum::ULLONG_INT:
        return &BuiltinType::ULong;
    case BuiltinTypeEnum::FLOAT:
        return &BuiltinType::Float;
    case BuiltinTypeEnum::DOUBLE:
        return &BuiltinType::Double;
    case BuiltinTypeEnum::LDOUBLE:
        return &BuiltinType::LDouble;

    default:
        unreachable();
    }
    // no warning
    return nullptr;
}
// function definition and delaration both start with
// these two syntax elements;
// declaration-specifiers indicate a type without pointer or a typedef'ed type name
// int a -> `int` is a declaration-specifier
const Type *Parser::parse_declaration_specifiers(Attribute *attr) {
    type_counter_t type_counter = 0;
    // const Type *type;
    while (true) {
        auto tk = peek();
        if (tk->is_builtin_type()) {
            type_counter = process_builtin(type_counter);
        } else if (tk->is_storage_class()) {
            // if (attr == nullptr)
            //     error_at(tk, "storage class is not allowed here.");
            process_storage_class(attr);
        } else if (tk->get_type() == TK_NAME) {
            // process user-define types or variable name
            auto name = tk->get_lexeme();
            auto t    = _scope->resolve_name(name);
            // find type
            if (t != nullptr) {
                if (type_counter != 0) {
                    error_at(tk, "user-defined type should not follow up builtin types.");
                }
                return t->type();
            }
            return get_builtin_type(type_counter);

        } else if (tk->get_type() == TK_STRUCT || tk->get_type() == TK_UNION) {
            // process struct or union
        } else
            return get_builtin_type(type_counter);
    }
}

// declarator:
//   pointer(opt) direct-declarator
const HalfType *Parser::parse_declarator(const Type *base) {
    base        = parse_pointer(base);
    auto direct = parse_direct_declarator(base);
    return direct;
}

// init-declarator needs initializer
Initializer *Parser::parse_initializer(const Type *lhstype) {
    auto init = parse_assignment();
    if (lhstype->is_arithmetic() && init->type()->is_arithmetic()) {
        auto ntype = uac(lhstype, init->type());
        init       = conv(ntype, init);
    }
    return new Initializer(init);
}

InitDeclarator *Parser::parse_init_declarator(const Type *base) {
    auto declarator = parse_declarator(base);
    if (try_next('=')) {
        auto initializer = parse_initializer(declarator->type());
        return new InitDeclarator(declarator, initializer);
    } else {
        return new InitDeclarator(declarator, nullptr);
    }
}

// chibicc treats an init initializer list as a compound statement
// but an init initializer list does not have a nested scope
Block *Parser::parse_init_declarators(const Type *base) {
    vector<BlockItem *> block{};
    do {
        auto declarator = parse_init_declarator(base);
        block.push_back(declarator);
    } while (try_next(','));
    return new Block(_scope, block);
}

// direct declarator needs pointer
const Type *Parser::parse_pointer(const Type *base) {
    while (test(TK_STAR)) {
        base = new PointerType(base);
        next();
    }
    return base;
}
// declarator needs direct declarator
// direct-declarator = ( identifier | "(" declarator ")" ) [ type-suffix ]
// type-suffix = "(" ... |
//               "[" ...
const HalfType *Parser::parse_direct_declarator(const Type *base) {
    auto tk = peek();
    const Token *name;
    // identifier
    if (tk->get_type() == TK_NAME) {
        name = tk;
    }
    // ( declarator )
    else if (tk->get_type() == '(') {
        next();
        auto prefix = parse_declarator(base);
        base        = prefix->type();
        expect(')');
        name = prefix->token();
    } else {
        error_at(tk, "unexpected token while parsing direct declarator");
    }
    next();
    if (test('[') || test('(')) {
        auto type = parse_func_or_array_decl(base, name->get_lexeme());
        return new HalfType(name, type);
    }
    return new HalfType(name, base);
}

const HalfType *Parser::parse_parameter() {
    auto param_base_type = parse_declaration_specifiers(nullptr);
    return parse_declarator(param_base_type);
}

vector<const HalfType *> Parser::parse_parameters() {
    expect('(');
    vector<const HalfType *> result;
    // ()
    if (try_next(')')) {
        return result;
    }
    // (void)
    else if (try_next(TK_VOID)) {
        expect(')');
        return result;
    }
    // (type name)
    auto first = parse_parameter();
    result.push_back(first);
    while (try_next(',')) {
        auto param = parse_parameter();
        result.push_back(param);
    }
    expect(')');
    return result;
}

Expr *Parser::parse_array_dimen() {
    expect('[');
    auto expr = parse_expr();
    if (!expr->is_int_const())
        error_at(expr->token(), "array dimen requires int constant.");
    expect(']');
    return expr;
}
// O ( parameter-type-list )
// X ( identifier-list(opt) )
// X [ type-qualifier-list(opt) assignment-expression(opt) ]
// X [ static type-qualifier-list(opt) assignment-expression ]
// X [ type-qualifier-list static assignment-expression ]
// X [ type-qualifier-list(opt) * ]
Type *Parser::parse_func_or_array_decl(const Type *fake_base, const char *name) {
    if (test('(')) {
        auto ret_params = parse_parameters();
        if (test('[') || test('(')) {
            fake_base = parse_func_or_array_decl(fake_base, name);
        }
        if (fake_base->is_array())
            error_at(peek(), "cannot declare a function that return an array type.");
        return new FuncType(fake_base, "", ret_params);
    }
    if (test('[')) {
        auto dimen = parse_array_dimen();
        if (test('[') || test('(')) {
            fake_base = parse_func_or_array_decl(fake_base, name);
        }
        if (fake_base->is_function())
            error_at(peek(), "cannot declare an arrya of functions.");
        return new ArrayType(fake_base, strtoul(dimen->token()->get_lexeme(), nullptr, 10));
    }
    return nullptr;
}

// declarations
//   declaration-specifiers init-declarator-list(opt)
//   DS = declaration-specifiers
//   ptr = pointer
//   ? = (opt)
// +------------+------------------------------------------------+-----+
// |  DS        | init-declarator-list?                          | ";" |
// +------------+-----------------------------+------------------+-----+
// |            | declarator                  | "=" initializer? |     |
// +------------+------+----------------------+------------------+-----+
// |            | ptr? | direct-declarator    |                  |     |
// +------------+------+----------------------+------------------+-----+
// | struct x                                                      ;   |
// | static int  *       func_name(int param)                      ;   |
// | int                  var1                    = 10             ;   |
// | int         *        var2                    = &var1          ;   |
// +-------------------------------------------------------------------+
Block *Parser::parse_declaration() {
    Attribute *attr = new Attribute;
    auto type       = parse_declaration_specifiers(attr);
    auto block      = parse_init_declarators(type);
    for (auto item : block->items()) {
        mqassert(item->is_init_declarator(), "all items in a declaration must be init declarator.");
        InitDeclarator *id = static_cast<InitDeclarator *>(item);
        check_name(id, attr);
    }
    return block;
}

Decl *Parser::parse_decl(type_counter_t spec, Declarator *declarator) { return nullptr; }

Expr *Parser::process_const() {
    auto tk = peek();
    if (tk->get_type() == TK_FNUMBER) {
        next();
        return new FloatConst(tk);
    }
    if (tk->get_type() == TK_INUMBER) {
        next();
        return new IntConst(tk);
    }
    unreachable();
    return nullptr;
}

// expressions
//  primary-expression:
//   O  identifier
//   X  constant
//   O  string-literal
//   O  ( expression )
//   X  generic-selection
Expr *Parser::parse_primary() {
    auto tk = peek();
    switch (tk->get_type()) {
    case TK_INUMBER:
    case TK_FNUMBER:
        return process_const();
    case TK_NAME:
        return parse_ident();
    case TK_STRING: {
        auto stok = tk;
        next();
        return new StringLiteral(stok);
    }
    case '(': {
        next();
        auto expr = parse_expr();
        expect(')');
        return expr;
    }
    case TK__GENERIC:
        return parse_generic();
    default:
        error_at(tk, "unexpected token: (%d:'%s')", tk->get_type(), tk->get_lexeme());
    }
    // no warning
    return nullptr;
}
Expr *Parser::parse_ident() {
    auto tk = peek();
    next();
    return new Identifier(tk, _scope);
}
Expr *Parser::parse_constant() { return nullptr; }
Expr *Parser::parse_generic() { return nullptr; }

// postfix-expression:
//  O  primary-expression
//  O  postfix-expression [ expression ]
//  O  postfix-expression ( argument-expression-list(opt) )
//  X  postfix-expression . identifier
//  X  postfix-expression -> identifier
//  O  postfix-expression ++
//  O  postfix-expression --
//  X  ( type-name ) { initializer-list }
//  X  ( type-name ) { initializer-list , }
Expr *Parser::parse_postfix() {
    Expr *ret = parse_primary();
    while (true) {
        // parse arguments
        if (test('(')) {
            vector<Expr *> args = parse_args();
            ret                 = new FuncCall(ret, args);
            continue;
        }
        // subscription
        // x[y] is short for *(x+y)
        if (test('[')) {
            auto op = peek();
            next();
            auto sub = parse_expr();
            auto add = new Add(op, ret, sub);
            check_type_for_additive(add);
            ret = new TypedUnaryExpr<'*'>(add);
            expect(']');
            continue;
        }
        // postfix inc/dec
        if (try_next(TK_INC)) {
            ret = new PostInc(ret);
            continue;
        }
        if (try_next(TK_DEC)) {
            ret = new PostDec(ret);
            continue;
        }
        break;
    }
    return ret;
}

// argument-expression-list:
//   assignment-expression
//   argument-expression-list , assignment-expression
vector<Expr *> Parser::parse_args() {
    vector<Expr *> ret;
    expect('(');
    if (try_next(')'))
        return ret;
    auto arg = parse_assignment();
    ret.push_back(arg);

    while (try_next(',')) {
        arg = parse_assignment();
        ret.push_back(arg);
    }
    expect(')');
    return ret;
}
// (6.5.3) unary-expression:
//      postfix-expression
//      ++ unary-expression
//      -- unary-expression
//      unary-operator cast-expression
//      sizeof unary-expression
//      sizeof ( type-name )
//      alignof ( type-name )
Expr *Parser::parse_unary() {
    if (try_next(TK_INC))
        return new TypedUnaryExpr<TK_INC>(parse_unary());
    if (try_next(TK_DEC))
        return new TypedUnaryExpr<TK_DEC>(parse_unary());
    // unary operator
    if (try_next('&'))
        return new TypedUnaryExpr<'&'>(parse_cast());
    if (try_next('*'))
        return new TypedUnaryExpr<'*'>(parse_cast());
    if (try_next('+'))
        return new TypedUnaryExpr<'+'>(parse_cast());
    if (try_next('-'))
        return new TypedUnaryExpr<'-'>(parse_cast());
    if (try_next('!'))
        return new TypedUnaryExpr<'!'>(parse_cast());
    if (try_next('~'))
        return new TypedUnaryExpr<'~'>(parse_cast());
    if (test(TK_SIZEOF)) {
        auto tok = peek();
        next();
        if (try_next('(')) {
            auto type = parse_type_name();
            if (type) {
                expect(')');
                return new IntConst(tok, type->size());
            }
            unget();
        }
        auto expr = parse_unary();
        return new IntConst(tok, expr->type()->size());
    }
    return parse_postfix();
}
// (6.5.4) cast-expression:
//              unary-expression
//              ( type-name ) cast-expression
Expr *Parser::parse_cast() {
    if (try_next('(')) {
        // TODO: parse_type_name
        auto name = peek();
        auto type = _scope->find_type(name->get_lexeme());
        if (type) {
            expect(')');
            auto expr = parse_cast();
            return new Cast(type, expr);
        } else {
            while (!test('('))
                unget();
        }
    }
    return parse_unary();
}

// @bop binary operator priority
// Expr *Parser::parse_binary(int bop) {
//     // end cond
//     if (bop == OP_NOT_VALID) {
//         return parse_cast();
//     }
//     // parse lhs
//     auto left     = parse_binary(bop + 1);
//     auto op       = peek();
//     auto priority = get_priority(op->get_type());
//     // parse exprs with the same priority
//     while (priority == bop) {
//         next();
//         // rhs should have higher priority
//         auto right = parse_binary(bop + 1);
//         // combine lhs and rhs, the result will be new lhs within the binary expr
//         auto ek = get_expr_kind(op); // ExprKind
//         check_type_for_binary(left, right, op);
//         if (op->get_type() == TK_LAND || op->get_type() == TK_LOR) {
//             left = new Binary(ek, op, left, right);
//         } else if (left->type()->is_arithmetic() && right->type()->is_arithmetic()) {
//             auto ntype = uac(left->type(), right->type());
//             left       = new Binary(ek, op, conv(ntype, left), conv(ntype, right));
//         } else if (ek == EXPR_SUB && left->type()->is_pointer() && right->type()->is_pointer()) {
//             left = new Binary(EXPR_DIV, nullptr, new Binary(ek, op, left, right),
//                               new IntConst(nullptr, left->type()->point_to()->size()));
//         } else {
//             // left = new Binary(ek, op, wrap_pointer(left, right->type()),
//             //                  wrap_pointer(right, left->type()));
//             left = new Binary(ek, op, left, right);
//         }
//         op       = peek();
//         priority = get_priority(op->get_type());
//     }
//     return left;
// }

// (6.5.5) multiplicative-expression:
//      cast-expression
//      multiplicative-expression * cast-expression
//      multiplicative-expression / cast-expression
//      multiplicative-expression % cast-expression
Expr *Parser::parse_mult() {
    auto expr = parse_cast();
    while (test('*') || test('/') || test('%')) {
        auto op = peek();
        next();
        expr = new Multi(op, expr, parse_cast());
        if (op->get_type() == '%')
            check_type_integers(expr);
        else
            check_type_arithmetics(expr);
        apply_uac_on_binary(expr);
    }
    return expr;
}
// (6.5.6) additive-expression:
//      multiplicative-expression
//      additive-expression + multiplicative-expression
//      additive-expression - multiplicative-expression
Expr *Parser::parse_add() {
    auto expr = parse_mult();
    while (test('+') || test('-')) {
        auto op = peek();
        next();
        expr = new Add(op, expr, parse_mult());
        check_type_for_additive(expr);
    }
    return expr;
}
// (6.5.7) shift-expression:
//      additive-expression
//      shift-expression << additive-expression
//      shift-expression >> additive-expression
Expr *Parser::parse_shift() {
    auto expr = parse_add();
    while (test(TK_LSHIFT) || test(TK_RSHIFT)) {
        auto op = peek();
        next();
        expr = new Shift(op, expr, parse_add());
        check_type_integers(expr);
    }
    return expr;
}
// (6.5.8) relational-expression:
//      shift-expression
//      relational-expression < shift-expression
//      relational-expression > shift-expression
//      relational-expression <= shift-expression
//      relational-expression >= shift-expression
Expr *Parser::parse_relational() {
    auto expr = parse_shift();
    while (test('<') || test('>') || test(TK_LEQUAL) || test(TK_GEQUAL)) {
        auto op = peek();
        next();
        expr = new Relational(op, expr, parse_shift());
        check_type_for_relational(expr);
    }
    return expr;
}
// (6.5.9) equality-expression:
//      relational-expression
//      equality-expression == relational-expression
//      equality-expression != relational-expression
Expr *Parser::parse_equality() {
    auto expr = parse_relational();
    while (test(TK_EQUAL) || test(TK_NEQUAL)) {
        auto op = peek();
        next();
        expr = new Equality(op, expr, parse_relational());
        check_type_for_relational(expr);
    }
    return expr;
}
// (6.5.10) AND-expression:
//      equality-expression
//      AND-expression & equality-expression
Expr *Parser::parse_bit_and() {
    auto expr = parse_equality();
    while (test('&')) {
        auto op = peek();
        next();
        expr = new BitAnd(op, expr, parse_equality());
        check_type_integers(expr);
    }
    return expr;
}
// (6.5.11) exclusive-OR-expression:
//      AND-expression
//      exclusive-OR-expression ^ AND-expression
Expr *Parser::parse_xor() {
    auto expr = parse_bit_and();
    while (test('^')) {
        auto op = peek();
        next();
        expr = new BitXor(op, expr, parse_bit_and());
        check_type_integers(expr);
    }
    return expr;
}
// (6.5.12)inclusive-OR-expression:
//      exclusive-OR-expression
//      inclusive-OR-expression | exclusive-OR-expression
Expr *Parser::parse_bit_or() {
    auto expr = parse_xor();
    while (test('|')) {
        auto op = peek();
        next();
        expr = new BitOr(op, expr, parse_xor());
        check_type_integers(expr);
    }
    return expr;
}
// (6.5.13) logical-AND-expression:
//      inclusive-OR-expression
//      logical-AND-expression && inclusive-OR-expression
Expr *Parser::parse_log_and() {
    auto expr = parse_bit_or();
    while (test(TK_LAND)) {
        auto op = peek();
        next();
        expr = new LogAnd(op, expr, parse_bit_or());
        check_type_scalars(expr);
    }
    return expr;
}
// (6.5.14) logical-OR-expression:
//      logical-AND-expression
//      logical-OR-expression || logical-AND-expression
Expr *Parser::parse_log_or() {
    auto expr = parse_log_and();
    while (test(TK_LOR)) {
        auto op = peek();
        next();
        expr = new LogOr(op, expr, parse_log_and());
        check_type_scalars(expr);
    }
    return expr;
}

Expr *Parser::parse_conditional() {
    // auto cond = parse_binary(OP_LOR);
    auto cond = parse_log_or();
    if (try_next(TK_COND)) {
        auto branch_true = parse_expr();
        expect(TK_COLON);
        auto branch_false = parse_conditional();
        return new Cond(cond, branch_true, branch_false);
    }
    return cond;
}
// assignment-expression:
//   conditional-expression
//   unary-expression assignment-operator assignment-expression
Expr *Parser::parse_assignment() {
    auto expr = parse_conditional();
    if (expr->kind() <= EXPR_UNARY) {
        if (expr->kind() == EXPR_IDENT) {
            auto obj = _scope->find_var(expr->token()->get_lexeme());
            if (obj == nullptr) {
                warn_at(expr->token(), "use undeclared variable, assume as int.");
                _scope->push_var(expr->token()->get_lexeme(),
                                 new Object(expr->token(), &BuiltinType::Int, nullptr));
            }
        }
        if (peek()->is_assign_operator()) {
            auto tk_type = peek()->get_type();
            next();
            auto rhs = parse_assignment();
            return new Assignment(expr, rhs, tk_type);
        }
    }
    return expr;
}

// statement needs expression
Expr *Parser::parse_expr() { return parse_assignment(); }
// make labels while parsing case and default statements
static string caselabelmaker() {
    static int label_conter = 0;
    stringstream s;
    s << ".LCD_" << label_conter++;
    return s.str();
}
// (6.8.1) labeled-statement:
//      identifier : statement
//      case constant-expression : statement
//      default : statement
Stmt *Parser::parse_labeled() {
    if (try_next(TK_CASE)) {
        auto expr = parse_expr();
        if (!expr->is_int_const())
            error_at(expr->token(), "case requires an integer constant");
        IntConst *expr2 = static_cast<IntConst *>(expr);
        // gcc limits values of case statements as int type.
        // now i did not make a difference between signed and unsigned int
        if (expr2->type()->size() != 4) {
            auto ival = expr2->value();
            if (ival > INT32_MAX) {
                warn_at(expr2->token(), "value of out range, will be truncated.");
            }
            // int val = static_cast<int>(ival);
            // delete expr2;
            // expr2 = new IntConst(val);
        }
        for (auto c : _switch->labels) {
            if (c.first != nullptr && c.first->value() == expr2->value())
                error_at(expr2->token(), "duplcated value in case statement.");
        }
        expect(':');
        auto stmt  = parse_stmt();
        auto label = caselabelmaker();
        auto ret   = new Labeled(label, stmt);
        _switch->labels.emplace_back(expr2, ret->label());
        return ret;
    } else if (test(TK_DEFAULT)) {
        auto tk = peek();
        next();
        expect(':');
        auto stmt  = parse_stmt();
        auto label = caselabelmaker();
        auto ret   = new Labeled(label, stmt);
        for (auto c : _switch->labels)
            if (c.first == nullptr)
                error_at(tk, "duplcated default in switch");
        _switch->labels.emplace_back(nullptr, ret->label());
        return ret;
    }
    unreachable();
    return nullptr;
}
// (6.8.4) selection-statement:
//      if ( expression ) statement
//      if ( expression ) statement else statement
//      switch ( expression ) statement
Stmt *Parser::parse_if() {
    expect(TK_IF);
    expect('(');
    auto cond = parse_expr();
    check_scalar(cond->type(), nullptr);
    if (!cond->type()->is_integer()) {
        cond = conv(&BuiltinType::Int, cond);
    }
    expect(')');
    auto then       = parse_stmt();
    Stmt *otherwise = Empty::instance();
    if (try_next(TK_ELSE))
        otherwise = parse_stmt();
    return new IfElse(cond, then, otherwise);
}

Stmt *Parser::parse_switch() {
    auto backup         = _switch;
    auto current_switch = SwitchStatus{};
    _switch             = &current_switch;
    expect(TK_SWITCH);
    expect('(');
    auto cond = parse_expr();
    if (!cond->type()->is_integer()) {
        error_at(cond->token(), "switch requires an integer expression, but found: %s",
                 cond->type()->normalize().c_str());
    }
    if (cond->type()->size() < 4) {
        if (cond->type()->is_signed())
            cond = conv(&BuiltinType::Int, cond);
        else
            cond = conv(&BuiltinType::UInt, cond);
    }
    expect(')');
    auto body = parse_stmt();
    _switch   = backup;
    return new Switch(cond, body, current_switch.labels);
}

// iteration
// "while" "(" expression ")" statement
Stmt *Parser::parse_while() {
    expect(TK_WHILE);
    expect('(');
    auto cond = parse_expr();
    if (!cond->type()->is_integer()) {
        cond = conv(&BuiltinType::Int, cond);
    }
    expect(')');
    auto body = parse_stmt();
    return new While(cond, body);
}
// "do" statement "while" "(" expression ")" ";"
Stmt *Parser::parse_do_while() {
    expect(TK_DO);
    auto body = parse_stmt();
    expect(TK_WHILE);
    expect('(');
    auto cond = parse_expr();
    if (!cond->type()->is_integer()) {
        cond = conv(&BuiltinType::Int, cond);
    }
    expect(')');
    expect(';');
    return new DoWhile(body, cond);
}
// "for" "(" expression(opt) ";" expression(opt) ";" expression(opt) ")" statement
// "for" "(" declaration         expression(opt) ";" expression(opt) ")" statement
Stmt *Parser::parse_for() {
    expect(TK_FOR);
    expect('(');
    // parse init part
    Stmt *init;
    if (try_next(';')) {
        init = Empty::instance();
    } else if (maybe_decl()) {
        init = parse_declaration();
    } else {
        init = new ExprStmt(parse_expr());
        expect(';');
    }
    // parse cond part;
    Expr *cond;
    if (try_next(';')) {
        cond = nullptr;
    } else {
        cond = parse_expr();
        if (!cond->type()->is_integer()) {
            cond = conv(&BuiltinType::Int, cond);
        }
        expect(';');
    }
    // accumulator part
    Expr *accumulator;
    if (try_next(')')) {
        accumulator = nullptr;
    } else {
        accumulator = parse_expr();
        expect(')');
    }
    auto body = parse_stmt();
    return new For(init, cond, accumulator, body);
}

// jump-statement:
//    goto identifier ;
//    continue ;
//    break ;
//    return expressionopt ;
Stmt *Parser::parse_jump() {
    switch (peek()->get_type()) {
    case TK_GOTO:
        next();
        if (test(TK_NAME)) {
            auto ident = peek()->get_lexeme();
            next();
            expect(';');
            return new Goto(ident);
        }
        // report an error
        expect(TK_NAME);
        return nullptr;
    case TK_CONTINUE:
        next();
        expect(';');
        return new Continue();
    case TK_BREAK:
        next();
        expect(';');
        return new Break();
    case TK_RETURN: {
        next();
        if (try_next(';')) {
            return new Return(nullptr);
        }
        auto ret = parse_expr();
        expect(';');
        return new Return(conv(_cft->return_type(), ret));
    }
    default:
        unreachable();
    }
    return nullptr;
}
// block needs statement
// (6.8) statement:
//   O  labeled-statement
//   O  compound-statement
//   O  expression-statement
//   O  selection-statement
//   O  iteration-statement
//   O  jump-statement
Stmt *Parser::parse_stmt() {
    if (maybe_decl()) {
        return parse_declaration();
    }
    switch (peek()->get_type()) {
    case ';':
        next();
        return Empty::instance();
    // iteration
    case TK_DO:
        return parse_do_while();
    case TK_WHILE:
        return parse_while();
    case TK_FOR:
        return parse_for();
    // jump
    case TK_GOTO:
    case TK_CONTINUE:
    case TK_BREAK:
    case TK_RETURN:
        return parse_jump();
    // selection
    case TK_IF:
        return parse_if();
    case TK_SWITCH:
        return parse_switch();
    case '{': {
        auto block = parse_block();
        return block;
    }
    // labeled
    case TK_CASE:
    case TK_DEFAULT:
        return parse_labeled();
    case TK_NAME: {
        auto tk = peek();
        next();
        if (try_next(':')) {
            auto stmt = parse_stmt();
            return new Labeled(tk->get_lexeme(), stmt);
        }
        // if cannot parse a labeled statement, it must be an expression statement
        unget();
    }
    default: {
        auto expr = parse_expr();
        expect(';');
        return new ExprStmt(expr);
    }
    }
    return nullptr;
}

// function definition needs compound statements aka. block
Block *Parser::parse_block() {
    expect('{');
    vector<BlockItem *> items;
    while (!try_next('}')) {
        auto item = parse_stmt();
        if (item->is_return_stmt() && _cft == nullptr)
            error_at(peek(), "return statement found out of a function body.");
        items.push_back(item);
    }
    auto result = new Block(_scope, items);
    return result;
}

FuncDef *Parser::parse_func_def(const HalfType *base) {
    mqassert(base->type()->kind() == TY_FUNC, "expect a function type.");
    auto func_type = static_cast<const FuncType *>(base->type());
    // registry func name to scope
    _scope      = _scope->drill_down();
    auto params = func_type->parameters();
    for (auto param : params) {
        auto ptype  = param->type();
        auto ptoken = param->token();
        auto obj    = new Object(ptoken, ptype, nullptr);
        _scope->push_var(param->token()->get_lexeme(), obj);
    }
    // set current funtion type
    _cft      = func_type;
    auto body = parse_block();
    // clear current function type
    _cft   = nullptr;
    _scope = _scope->float_up();
    // assert _scope == _global_scope
    return new FuncDef(base->token(), func_type, body);
}

const Type *Parser::parse_type_name() { return nullptr; }
void Parser::check_name(const InitDeclarator *id, Attribute *attr) {
    auto token = id->halftype()->token();
    auto obj   = _scope->find_var_in_local(token->get_lexeme());
    auto type  = id->halftype()->type();
    // first defined here
    if (!obj) {
        _scope->push_var(token->get_lexeme(), new Object(token, type, attr));
        return;
    }
    // check if already defined
    if (obj->is_defined() && id->is_initialized()) {
        error_at(token, "redefined variable.");
    }
    // check if the same type
    if (!obj->type()->equals_to(id->halftype()->type())) {
        error_at(token, "redeclared name with different type.");
    }
    // check linkage
    if (!attr->equals_to(obj->attr()) || !attr->is_extern) {
        error_at(token, "redeclared with non-extern.");
    }
    // check extern
    if (attr->is_extern && id->is_initialized()) {
        error_at(token, "declared with both extern and initializer.");
    }
}

bool Parser::maybe_decl() {
    auto tk = peek();
    if (tk->get_type() == TK_NAME) {
        auto name = tk->get_lexeme();
        return _scope->resolve_name(name) && _scope->resolve_name(name)->type()->is_float();
    }
    return tk->is_decl_start();
}

void Parser::next() {
    mqassert(!_lookups.empty(), "lookup tokens is empty.");
    _consumed.push(_lookups.top());
    _lookups.pop();
}
const Token *Parser::peek() {
    if (_lookups.empty()) {
        _lookups.push(_scanner->get_token());
    }
    return _lookups.top();
}

bool Parser::test(int expected) { return peek()->get_type() == expected; }

void Parser::expect(int expected) {
    if (!test(expected)) {
        error_at(peek(), "expect '%s', but got '%s'", Token(expected, nullptr).get_lexeme(),
                 peek()->get_lexeme());
    }
    next();
}
bool Parser::try_next(int expected) {
    if (!test(expected))
        return false;
    next();
    return true;
}

void Parser::unget() {
    _lookups.push(_consumed.top());
    _consumed.pop();
}

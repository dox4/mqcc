#include "parser.h"
#include "ast.h"
#include "error.h"
#include "scanner.h"
#include "scope.h"
#include "token.h"
#include "type.h"

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iterator>
#include <list>
#include <sstream>
#include <string_view>
#include <utility>
#include <vector>

using namespace std;

/// static functions

static Expr *eval(const Type *type, Expr *cast);
static Expr *conv(const Type *type, Expr *expr) {
    if (expr->kind() == EXPR_FUNC_CALL)
        return expr;
    if (expr->type()->equals_to(type))
        return expr;
    if (type == &BuiltinType::Void)
        return new Conv(type, expr);
    if (!type->is_compitable_with(expr->type())) {
        error_at(expr->token(), "uncompitable type with implicit conversion: from %s to %s",
                 expr->type()->normalize().c_str(), type->normalize().c_str());
    }
    return eval(type, expr);
}

static void apply_uac_on_binary(Expr *e) {
    Binary *b     = e->as_binary();
    auto uac_type = uac(b->lhs()->type(), b->rhs()->type());
    b->set_lhs(conv(uac_type, b->lhs()));
    b->set_rhs(conv(uac_type, b->rhs()));
}

static void check_scalar(const Type *type, const Token *tok) {
    if (!type->is_scalar())
        error_at(tok, "expression requires scalar type here.");
}

static void check_type_integers(Expr *e) {
    if (!e->is_binary())
        return;
    Binary *b = e->as_binary();
    if (!(b->lhs()->type()->is_integer() && b->rhs()->type()->is_integer()))
        error_invalid_oprands(b->token(), b->lhs()->type(), b->rhs()->type());
}

static void check_type_arithmetics(Expr *e) {
    if (!e->is_binary())
        return;
    Binary *b = e->as_binary();
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

static void check_type_for_func_call(FuncCall *fc) {
    mqassert(fc->left()->type()->is_function(), "lhs of function call must be function: %s",
             fc->token()->get_lexeme());
    auto ft         = fc->left()->type()->as_function();
    auto parameters = ft->parameters();
    auto &argv      = fc->args();
    if (parameters.size() != argv.size())
        error_at(fc->left()->token(), "count of arguments does not match the function.");
    for (std::size_t idx = 0; idx < argv.size(); idx++) {
        auto pt = parameters.at(idx)->type();
        auto at = argv.at(idx)->type();
        if (!pt->is_compitable_with(at))
            error_at(argv.at(idx)->token(), "type not compitable.");
        if (!pt->equals_to(at)) {
            argv[idx] = new Conv(pt, argv.at(idx));
        }
    }
}

static void swap_binary_oprands(Binary *b) {
    auto rhs = b->rhs();
    b->set_rhs(b->lhs());
    b->set_lhs(rhs);
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
    if (!e->is_binary())
        return;
    Binary *b = e->as_binary();
    if (!(b->lhs()->type()->is_scalar() && b->rhs()->type()->is_scalar()))
        error_invalid_oprands(b->token(), b->lhs()->type(), b->rhs()->type());
}

static Expr *eval(const Type *type, Expr *cast) {
#define cast_int(var, typeref, type_to)                                                            \
    do {                                                                                           \
        if (type == typeref) {                                                                     \
            var->set_type(type);                                                                   \
            var->set_value((type_to)(var->value()));                                               \
            return var;                                                                            \
        }                                                                                          \
    } while (false)
#define cast_float(fexpr, typeref, type_to)                                                        \
    do {                                                                                           \
        if (type == typeref) {                                                                     \
            auto value = (type_to)fexpr->value();                                                  \
            auto *var  = new IntConst(fexpr->token(), value, typeref);                             \
            return var;                                                                            \
        }                                                                                          \
    } while (false)
    if (type == &BuiltinType::Void)
        return new Conv(type, cast);
    if (!type->is_compitable_with(cast->type()))
        error_at(cast->token(), "cast with uncompitable type.");
    if (cast->is_int_const()) {
        auto *intconst = cast->as_int_const();
        if (type->is_pointer()) {
            intconst->set_type(type);
            return intconst;
        }
        if (type->is_integer()) {
            if (type->size() < intconst->type()->size()) {
                cast_int(intconst, &BuiltinType::Bool, bool);
                cast_int(intconst, &BuiltinType::Char, char);
                cast_int(intconst, &BuiltinType::UChar, unsigned char);
                cast_int(intconst, &BuiltinType::Short, short);
                cast_int(intconst, &BuiltinType::UShort, unsigned short);
                cast_int(intconst, &BuiltinType::Int, int);
                cast_int(intconst, &BuiltinType::UInt, unsigned int);
            }
            intconst->set_type(type);
            return cast;
        } else if (type->is_float()) {
            cast_int(intconst, &BuiltinType::Float, float);
            cast_int(intconst, &BuiltinType::Double, double);
        }
    } else if (cast->is_float_const()) {
        auto *floatconst = cast->as_float_const();
        if (type->is_integer()) {
            cast_float(floatconst, &BuiltinType::Bool, bool);
            cast_float(floatconst, &BuiltinType::Char, char);
            cast_float(floatconst, &BuiltinType::UChar, unsigned char);
            cast_float(floatconst, &BuiltinType::Short, short);
            cast_float(floatconst, &BuiltinType::UShort, unsigned short);
            cast_float(floatconst, &BuiltinType::Int, int);
            cast_float(floatconst, &BuiltinType::UInt, unsigned int);
            cast_float(floatconst, &BuiltinType::Long, long);
            cast_float(floatconst, &BuiltinType::ULong, unsigned long);
            unreachable();
        } else if (type->is_float()) {
            if (type == &BuiltinType::Float) {
                floatconst->set_value((float)floatconst->value());
            }
            floatconst->set_type(type);
            return cast;
        }
    }
#undef cast_int
#undef cast_float
    if (type->equals_to(cast->type()))
        return cast;
    return new Cast(type, cast);
}

// get from chibicc
static int64_t eval_int(int64_t v, int size) {
    switch (size) {
    case 1:
        return (uint8_t)v;
    case 2:
        return (uint16_t)v;
    case 4:
        return (uint32_t)v;
    }
    return v;
}

Expr *eval(Binary *binary) {
    auto *lhs = binary->lhs(), *rhs = binary->rhs();
    if (!(lhs->is_int_const() || lhs->is_float_const()))
        return binary;
    if (!(rhs->is_int_const() || rhs->is_float_const()))
        return binary;
    // 0 -> both float, 1 -> one of the two is float, 2 -> both integer
    int branch = lhs->is_int_const() + rhs->is_int_const();
#define case0(op)                                                                                  \
    case 0: {                                                                                      \
        auto *lhsfloat = lhs->as_float_const();                                                    \
        lhsfloat->set_value(lhsfloat->value() op rhs->as_float_const()->value());                  \
        return lhsfloat;                                                                           \
    } break
#define case1(op)                                                                                  \
    case 1: {                                                                                      \
        if (lhs->is_float_const()) {                                                               \
            auto *lvar = lhs->as_float_const();                                                    \
            lvar->set_value(lvar->value() op rhs->as_int_const()->value());                        \
            return lvar;                                                                           \
        } else {                                                                                   \
            auto *lvar = rhs->as_float_const();                                                    \
            lvar->set_value(lvar->value() op lhs->as_int_const()->value());                        \
            return lvar;                                                                           \
        }                                                                                          \
    } break
#define case2(op)                                                                                  \
    case 2: {                                                                                      \
        auto *lvar = lhs->as_int_const();                                                          \
        auto *type = uac(lhs->type(), rhs->type());                                                \
        int64_t v  = lvar->value() op rhs->as_int_const()->value();                                \
        lvar->set_value(eval_int(v, type->size()));                                                \
        return eval(type, lvar);                                                                   \
    } break
#define default0()                                                                                 \
    default:                                                                                       \
        unreachable()
    auto test_div0 = [](Expr *expr, const char *errmsg) {
        if (expr->is_int_const() && expr->as_int_const()->value() == 0)
            warn_at(expr->token(), errmsg);
        //        if (expr->is_float_const() && expr->as_float_const()->value() == 0.0)
        //            warn_at(expr->token(), errmsg);
    };
    switch (binary->token()->get_type()) {
    // multiplicative
    case '*':
        switch (branch) {
            case0(*);
            case1(*);
            case2(*);
            default0();
        }
    case '/':
        test_div0(rhs, "divided by zero is undefined.");
        switch (branch) {
            case0(/);
            case1(/);
            case2(/);
            default0();
        }
    case '%':;
        switch (branch) {
            case2(%);
            default0();
        }
    // additive
    case '+':
        if (lhs->type()->is_pointer()) {
            auto *lvar = lhs->as_int_const();
            lvar->set_value(lvar->value() +
                            rhs->as_int_const()->value() * lhs->type()->derefed()->size());
            return lvar;
        }
        switch (branch) {
            case0(+);
            case1(+);
            case2(+);
            default0();
        }
    case '-':
        if (lhs->type()->is_pointer()) {
            auto *lvar = lhs->as_int_const();
            if (rhs->type()->is_pointer())
                lvar->set_value((lvar->value() - rhs->as_int_const()->value()) /
                                lvar->type()->derefed()->size());
            else
                lvar->set_value(lvar->value() -
                                rhs->as_int_const()->value() * lhs->type()->derefed()->size());
            return lvar;
        }
        switch (branch) {
            case0(-);
            case1(-);
            case2(-);
            default0();
        }
    // shift
    case TK_LSHIFT:
        switch (branch) {
            case2(<<);
            default0();
        }
    case TK_RSHIFT:
        switch (branch) {
            case2(>>);
            default0();
        }
    // relational
    case '>':
        switch (branch) {
            case0(>);
            case1(>);
            case2(>);
            default0();
        }
    case '<':
        switch (branch) {
            case0(<);
            case1(<);
            case2(<);
            default0();
        }
    case TK_GEQUAL:
        switch (branch) {
            case0(>=);
            case1(>=);
            case2(>=);
            default0();
        }
    case TK_LEQUAL:
        switch (branch) {
            case0(<=);
            case1(<=);
            case2(<=);
            default0();
        }
    // equality
    case TK_EQUAL:
        switch (branch) {
            case0(==);
            case1(==);
            case2(==);
            default0();
        }
    case TK_NEQUAL:
        switch (branch) {
            case0(!=);
            case1(!=);
            case2(!=);
            default0();
        }
    // bit and
    case '&':
        switch (branch) {
            case2(&);
            default0();
        }
    // bit xor
    case '^':
        switch (branch) {
            case2(^);
            default0();
        }
    // bit or
    case '|':
        switch (branch) {
            case2(|);
            default0();
        }
    // logical and
    case TK_LAND:
        switch (branch) {
            case0(&&);
            case1(&&);
            case2(&&);
            default0();
        }
    // logical or
    case TK_LOR:
        switch (branch) {
            case0(||);
            case1(||);
            case2(||);
            default0();
        }
    default:
        unreachable();
    }
    return binary;
#undef case0
#undef case1
#undef case2
#undef default0
}
/// static functions end

Parser::Parser(Scanner *scanner) : _scanner(scanner), /*_lookups(), _consumed()*/ _tokens() {
    _scope = new Scope();
}

// function-definitions and declarations both start with
// `declaration-specifiers` and `declarator`
// so we can parse them first and dispatch parsing actions
// by next token or if the parsed declarator indicate a function
TransUnit *Parser::parse() {
    list<FuncDef *> funcs;
    list<InitDeclarator *> gvars;
    while (!test(TK_EOF)) {
        auto attr      = new Attribute;
        auto base_type = parse_declaration_specifiers(attr);
        // typedef
        if (attr->is_typedef) {
            parse_typedef(base_type, attr);
            continue;
        }
        // forward declaration
        if (base_type->is_struct() && try_next(';'))
            continue;
        // global variable or function declaration/definition
        // both start with declarator
        auto prefix = parse_declarator(base_type);
        // function declaration
        if (prefix->type()->is_function()) {
            check_func_declaration(prefix, attr);
            if (test('{')) {
                auto func = parse_func_def(prefix);
                funcs.push_back(func);
            } else if (!try_next(';'))
                error_at(peek(), "expect ';' or '{'.");
            continue;
        }
        // global variable declaration
        auto vars = parse_global_variables(prefix, attr);
        gvars.splice(gvars.end(), vars);
    }
    return new TransUnit(funcs, gvars);
}

list<InitDeclarator *> Parser::parse_global_variables(const HalfType *base, Attribute *attr) {
    list<InitDeclarator *> ret;
    // first variable
    if (try_next('=')) {
        auto init = parse_initializer(base->type());
        ret.emplace_back(new InitDeclarator(base, init));
    } else {
        ret.emplace_back(new InitDeclarator(base, nullptr));
    }
    check_init_declarator(*ret.begin(), attr);
    if (try_next(';')) {
        _scope->find_var_in_local((*ret.begin())->halftype()->token()->get_lexeme())->set_global();
        return ret;
    }
    // declarations separated by ','
    if (!try_next(','))
        error_at(peek(), "unexpected token while parsing global variable, expect '=', ';' or ','.");

    auto inits = parse_init_declarators(base->type(), attr);
    for (auto init : inits->items())
        ret.emplace_back(init->as_init_declarator());
    for (auto init : ret) {
        // set all global variables `is_global`
        auto name = init->halftype()->token()->get_lexeme();
        _scope->find_var_in_local(name)->set_global();
        if (init->is_initialized()) {
            if (!init->initializer()->assignment()->is_const())
                error_at(init->initializer()->assignment()->token(),
                         "not a compile time constant.");
        }
    }
    expect(';');
    return ret;
}

void Parser::parse_typedef(const Type *base, Attribute *attr) {
    do {
        auto declarator = parse_declarator(base);
        if (declarator->token() != nullptr) {
            auto *obj = new Object(declarator->token(), declarator->type(), attr);
            _scope->push_var(declarator->token()->get_lexeme(), obj);
        }
    } while (try_next(','));
    expect(';');
}

list<Member *> Parser::parse_member_decl(Attribute *attr, const Type *base) {
    list<Member *> ret{};
    do {
        auto d = parse_declarator(base);
        ret.push_back(new Member(d->type(), d->token()));
    } while (try_next(','));
    expect(';');
    return ret;
}

const Type *Parser::parse_struct_union_decl() {
    // struct or union
    auto is_union = try_next(TK_UNION);
    if (!is_union)
        expect(TK_STRUCT);
    // tag (opt)
    auto tag = test(TK_NAME) ? peek() : nullptr;
    if (tag != nullptr)
        next();
    // forward declaration
    Type *incomplete;
    if (tag != nullptr) {
        incomplete = _scope->find_mut_tag_in_local(tag->get_lexeme());
        if (incomplete == nullptr) {
            incomplete = new StructType(tag, list<Member *>{});
            _scope->push_tag(tag->get_lexeme(), incomplete);
        }
    }
    if (!try_next('{')) {
        if (tag == nullptr)
            error_at(peek(), "unexpected token while parsing struct or union.");
        return incomplete;
    }
    if (tag != nullptr) {
        if (incomplete->is_complete())
            error_at(tag, "redefined struct or union.");
        if (incomplete->as_struct()->is_union() != is_union || incomplete->is_enum())
            error_at(tag, "type conflicted with previous declaration.");
    }
    list<Member *> members;
    while (!try_next('}')) {
        auto attr = new Attribute;
        auto spec = parse_declaration_specifiers(attr);
        auto mems = parse_member_decl(attr, spec);
        members.splice(members.end(), mems);
    }
    if (tag != nullptr) {
        auto stype = _scope->find_mut_tag_in_local(tag->get_lexeme());
        stype->as_struct()->set_members(members);
        stype->as_struct()->set_complete(true);
        return stype;
    }
    StructType *ret;
    if (is_union)
        ret = new UnionType(tag, members);
    else
        ret = new StructType(tag, members);
    ret->set_complete(true);
    return ret;
}
const Type *Parser::parse_enum_decl() {
    expect(TK_ENUM);
    const Token *tag = nullptr;
    if (test(TK_NAME)) {
        tag = peek();
        next();
    }
    EnumType *ret = new EnumType(tag, list<const Enumerator *>{});
    if (tag != nullptr) {
        auto etype = _scope->find_mut_tag_in_local(tag->get_lexeme());
        if (etype != nullptr)
            return etype;
        _scope->push_tag(tag->get_lexeme(), ret);
    }
    if (!try_next('{')) {
        if (tag == nullptr)
            error_at(peek(), "unexpected token while parsing enum");
        return _scope->find_mut_tag_in_local(tag->get_lexeme());
    }
    if (tag != nullptr) {
        auto stype = _scope->find_mut_tag_in_local(tag->get_lexeme());
        if (stype->is_complete())
            error_at(tag, "redefined enum.");
        if (!stype->is_enum())
            error_at(tag, "type conflicted with previous declaration.");
    }
    int value = 0;
    do {
        if (test('}'))
            break;
        auto *name = parse_ident();
        if (try_next('=')) {
            auto *const_expr = parse_assignment();
            if (!const_expr->is_int_const())
                error_at(const_expr->token(), "enum must be integer.");
            value = const_expr->as_int_const()->value();
        }
        auto *obj = new Object(name->token(), ret, nullptr);
        _scope->push_var(name->get_value(), obj);
        ret->append_enumerator(new Enumerator(name->token(), value++));
    } while (try_next(','));
    expect('}');
    if (ret->enumrators().empty())
        error_at(peek(), "declare an empty enum.");
    return ret;
}

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
    static constexpr type_counter_t BOOL     = 1 << 2;
    static constexpr type_counter_t CHAR     = 1 << 4;
    static constexpr type_counter_t SHORT    = 1 << 6;
    static constexpr type_counter_t INT      = 1 << 8;
    static constexpr type_counter_t LONG     = 1 << 10;
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
    case TK__BOOL:
        type_counter += BuiltinTypeEnum::BOOL;
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
    case BuiltinTypeEnum::VOID:
    case BuiltinTypeEnum::BOOL:
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
    case BuiltinTypeEnum::VOID:
        return &BuiltinType::Void;
    case BuiltinTypeEnum::BOOL:
        return &BuiltinType::Bool;
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
    }
    unreachable();
    // no warning
    return nullptr;
}
// function definition and delaration both start with
// these two syntax elements;
// declaration-specifiers indicate a type without pointer or a typedef'ed type name
// int a -> `int` is a declaration-specifier
const Type *Parser::parse_declaration_specifiers(Attribute *attr) {
    type_counter_t type_counter = 0;
    while (maybe_decl()) {
        auto tk = peek();
        if (tk->is_builtin_type())
            type_counter = process_builtin(type_counter);
        else if (tk->is_storage_class())
            process_storage_class(attr);
        else if (test(TK_UNION) || test(TK_STRUCT))
            return parse_struct_union_decl();
        else if (tk->get_type() == TK_ENUM)
            return parse_enum_decl();
        else if (tk->get_type() == TK_NAME) {
            // process user-define types
            auto name = tk->get_lexeme();
            auto obj  = _scope->find_var_in_local(name);
            // check if the type defined in local scope
            if (obj != nullptr) {
                // name could not be redefined in the same scope
                // actually, chibicc supports name redefined in the same scope
                // while nether gcc nor clang supports
                if (type_counter != 0) {
                    error_at(tk, "user-defined type should not follow up builtin types.");
                }
                next();
                return obj->type();
            } else {
                // type must defined in parent scope
                // so it's name could be redefined
                if (type_counter != 0)
                    return get_builtin_type(type_counter);
                auto o2 = _scope->find_var(name);
                next();
                return o2->type();
            }
        }
    }
    if (type_counter != 0) {
        return get_builtin_type(type_counter);
    }
    warn_at(peek(), "declaration specifiers not found, assuming as `int`.");
    return &BuiltinType::Int;
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
Block *Parser::parse_init_declarators(const Type *base, Attribute *attr) {
    vector<BlockItem *> block{};
    do {
        auto declarator = parse_init_declarator(base);
        check_init_declarator(declarator, attr);
        block.push_back(declarator);
    } while (try_next(','));
    return new Block(_scope, block);
}

// direct declarator needs pointer
const Type *Parser::parse_pointer(const Type *base) {
    while (try_next(TK_STAR)) {
        base = new PointerType(base);
    }
    return base;
}
// declarator needs direct declarator
// direct-declarator = ( identifier | "(" declarator ")" ) [ type-suffix ]
// type-suffix = "(" ... |
//               "[" ...
const HalfType *Parser::parse_direct_declarator(const Type *base) {
    auto tk = peek();
    // identifier
    if (tk->get_type() == TK_NAME) {
        auto name = tk;
        next();
        if (test('[') || test('(')) {
            auto type = parse_func_or_array_decl(base);
            return new HalfType(name, type);
        }
        return new HalfType(name, base);
    }
    // ( declarator )
    else if (try_next('(')) {
        // type here may be not right.
        // the declarator like int (*x)[3] will be parsed as "array[3] of pointer to `int`"
        // actually, it should be "pointer to array[3] of `int`".
        // this mistake only occurs when the inner declarator has a derefed type
        // and the outer declarator leads to a function or an array.
        // if the inner declarator does not have a derefed type, its type must be the same as the
        // outer one. if the outer declarator does not lead to an array or a function, the result
        // needs no correctness.
        // so we igonre this result and record `_index`, when we get the right derefed type, we'll
        // reparse the declarator
        // this function is similar with the function that chibicc used.
        auto anchor = index();
        parse_declarator(base);
        expect(')');
        // we get the right derefed type here
        auto type    = parse_func_or_array_decl(base);
        auto anchor2 = index();
        // backward and reparse
        set_index(anchor);
        // re-parse the declarator within "(" and ")"
        auto ret = parse_declarator(type);
        // set the `_index` to the end of type suffix which we record in `anchor2`
        set_index(anchor2);
        return ret;
    }
    // with no identifier, here's an abstract declarator
    base = parse_func_or_array_decl(base);
    return new HalfType(nullptr, base);
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
    if (first->type()->is_array())
        first = new HalfType(first->token(), new PointerType(first->type()->derefed()));
    result.push_back(first);
    while (try_next(',')) {
        auto param = parse_parameter();
        if (param->type()->is_array())
            param = new HalfType(param->token(), new PointerType(param->type()->derefed()));
        result.push_back(param);
    }
    expect(')');
    return result;
}

Expr *Parser::parse_array_dimen() {
    expect('[');
    if (test(']')) {
        auto *expr = new IntConst(peek(), -1);
        next();
        return expr;
    }
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
const Type *Parser::parse_func_or_array_decl(const Type *fake_base) {
    if (test('(')) {
        auto ret_params = parse_parameters();
        if (test('[') || test('(')) {
            fake_base = parse_func_or_array_decl(fake_base);
        }
        if (fake_base->is_array())
            error_at(peek(), "cannot declare a function that return an array type.");
        return new FuncType(fake_base, "", ret_params);
    }
    if (test('[')) {
        auto dimen = parse_array_dimen();
        if (test('[') || test('(')) {
            fake_base = parse_func_or_array_decl(fake_base);
        }
        if (fake_base->is_function())
            error_at(peek(), "cannot declare an array of functions.");
        return new ArrayType(fake_base, dimen->as_int_const()->value());
    }
    return fake_base;
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
    if (attr->is_typedef) {
        parse_typedef(type, attr);
        return nullptr;
    }
    if (try_next(';') && (type->is_struct() || type->is_enum()))
        return nullptr;
    return parse_init_declarators(type, attr);
}

Decl *Parser::parse_decl(type_counter_t spec, Declarator *declarator) { return nullptr; }

Expr *Parser::process_const() {
    auto tk = peek();
    if (tk->get_type() == TK_FLOAT_LITERAL || tk->get_type() == TK_DOUBLE_LITERAL) {
        next();
        return new FloatConst(tk);
    }
    if (try_next(TK_INT_LITERAL) || try_next(TK_UINT_LITERAL) || try_next(TK_LONG_LITERAL) ||
        try_next(TK_ULONG_LITERAL)) {
        return new IntConst(tk);
    }
    if (tk->get_type() == TK_CHARACTER) {
        next();
        return new IntConst(tk, char(tk->value<int64_t>()));
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
//   O  ( block-stmt )
//   X  generic-selection
Expr *Parser::parse_primary() {
    auto tk = peek();
    switch (tk->get_type()) {
    case TK_INT_LITERAL:
    case TK_FLOAT_LITERAL:
    case TK_CHARACTER:
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
        Expr *expr;
        if (test('{')) {
            expr = new StmtExpr(parse_block());
        } else {
            expr = parse_expr();
        }
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
Identifier *Parser::parse_ident() {
    auto tk = peek();
    if (tk->get_type() != TK_NAME)
        error_at(tk, "expect identifier, but get '%s'.", tk->get_lexeme());
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
            auto fc             = new FuncCall(ret, args);
            check_type_for_func_call(fc);
            ret = fc;
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
        if (test(TK_INC) || test(TK_DEC)) {
            ret = make_post(peek(), ret);
            next();
            continue;
        }
        auto pk = peek();
        if (pk->get_type() == '.') {
            if (!ret->type()->is_struct())
                error_at(ret->token(), "expect struct type, but got type %s.",
                         ret->type()->normalize().c_str());
            next();
            auto ident = parse_ident();
            if (ret->type()->as_struct()->find_member(ident->get_value()) == nullptr)
                error_at(ident->token(), "not an member of struct.");
            ret = new MemberAccess(ret, ident, pk);
            continue;
        }
        if (pk->get_type() == TK_ARROW) {
            if (!ret->type()->is_pointer() || !ret->type()->derefed()->is_struct())
                error_at(ret->token(), "'->' operator could only apply on a pointer to a struct.");
            next();
            auto ident = parse_ident();
            if (ret->type()->derefed()->as_struct()->find_member(ident->get_value()) == nullptr)
                error_at(ident->token(), "not an member of struct.");
            ret = new MemberAccess(ret, ident, pk);
            continue;
        }
        break;
    }
    return ret;
}

// expr ++ => ($var1 = &expr, $var2 = *$var1, *$var1 = $var2 + 1, $var2)
// expr -- => ($var1 = &expr, $var2 = *$var1, *$var1 = $var2 - 1, $var2)
// chibicc converts A++ to `(typeof A)((A += 1) - 1)`
Expr *Parser::make_post(const Token *op, Expr *expr) {
    auto ftok1 = Token::fake_name_token(op->get_position());
    create_local_variable(ftok1, new PointerType(expr->type()));
    auto *var1 = new Identifier(ftok1, _scope);
    auto ftok2 = Token::fake_name_token(op->get_position());
    create_local_variable(ftok2, expr->type());
    auto *var2  = new Identifier(ftok2, _scope);
    auto *expr1 = new Assignment(op, var1, new TypedUnaryExpr<'&'>(expr));
    auto *expr2 = new Assignment(op, var2, new TypedUnaryExpr<'*'>(var1));
    auto *tok   = Token::make_token(op->get_type() == TK_INC ? '+' : '-', op->get_position());
    Expr *val   = nullptr;
    if (expr->type()->is_float())
        val = new FloatConst(op, 1.0);
    else if (expr->type()->is_integer() || expr->type()->is_derefed())
        val = new IntConst(op, 1);
    else
        error_at(op, "++/-- could not be apply on type %s", expr->type()->normalize().c_str());
    auto *expr3 = new Assignment(op, new TypedUnaryExpr<'*'>(var1), new Add(tok, var2, val));
    return new Comma(op, new Comma(op, new Comma(op, expr1, expr2), expr3), var2);
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
    if (try_next('*')) {
        auto *cast = parse_cast();
        if (!cast->type()->is_derefed())
            error_at(cast->token(), "unary '*' could only apply on pointer or array type.");
        return new TypedUnaryExpr<'*'>(cast);
    }
    if (try_next('+')) {
        auto *cast = parse_cast();
        if (!cast->type()->is_arithmetic())
            error_at(cast->token(), "unary `+` could only apply on arithmetic type.");
        return cast;
    }
    if (try_next('-')) {
        auto *cast = parse_cast();
        if (!cast->type()->is_arithmetic())
            error_at(cast->token(), "unary `-` could only apply on arithmetic type.");
        if (cast->is_int_const())
            return cast->as_int_const()->neg();
        if (cast->is_float_const())
            return cast->as_float_const()->neg();
        return new TypedUnaryExpr<'-'>(cast);
    }
    if (try_next('!')) {
        auto *cast = parse_cast();
        if (cast->is_int_const()) {
            return cast->as_int_const()->value() == 0 ? IntConst::one(cast->token())
                                                      : IntConst::zero(cast->token());
        }
        if (cast->is_float_const()) {
            return cast->as_float_const()->value() == 0 ? IntConst::zero(cast->token())
                                                        : IntConst::one(cast->token());
        }
        return new TypedUnaryExpr<'!'>(cast);
    }
    if (try_next('~')) {
        auto *cast = parse_cast();
        if (!cast->type()->is_integer())
            error_at(cast->token(), "unary '~' could only apply on integer type.");
        if (cast->is_int_const()) {
            auto *ic = cast->as_int_const();
            ic->set_value(~ic->value());
            return ic;
        }
        return new TypedUnaryExpr<'~'>(cast);
    }
    if (test(TK_SIZEOF)) {
        auto tok = peek();
        next();
        if (try_next('(')) {
            auto tok = peek();
            if (tok->get_type() == TK_NAME) {
                auto defined = _scope->find_var(tok->get_lexeme());
                if (defined != nullptr && defined->attr()->is_typedef) {
                    next();
                    expect(')');
                    if (defined->type()->is_complete()) {
                        return new IntConst(tok, defined->type()->size());
                    } else {
                        // type is not complete, it must be struct or union with a tag
                        auto tag  = defined->type()->as_struct()->tag();
                        auto type = _scope->find_tag(tag->get_lexeme());
                        if (type == nullptr)
                            error_at(tok, "sizeof an uncomplete type.");
                        return new IntConst(tok, type->size());
                    }
                }
            } else if (maybe_decl()) {
                auto type = parse_type_name();
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
        if (maybe_decl()) {
            auto type = parse_type_name();
            expect(')');
            auto *cast = parse_cast();
            return eval(type, cast);
        } else {
            unget();
        }
    }
    return parse_unary();
}

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
        expr = eval(expr->as_binary());
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
        expr = eval(expr->as_binary());
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
        expr = eval(expr->as_binary());
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
        apply_uac_on_binary(expr);
        expr = eval(expr->as_binary());
        expr = eval(&BuiltinType::Int, expr);
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
        apply_uac_on_binary(expr);
        expr = eval(expr->as_binary());
        expr = eval(&BuiltinType::Int, expr);
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
        apply_uac_on_binary(expr);
        expr = eval(expr->as_binary());
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
        apply_uac_on_binary(expr);
        expr = eval(expr->as_binary());
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
        apply_uac_on_binary(expr);
        expr = eval(expr->as_binary());
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
        expr = eval(expr->as_binary());
        expr = eval(&BuiltinType::Int, expr);
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
        expr = eval(expr->as_binary());
        expr = eval(&BuiltinType::Int, expr);
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
        if (cond->is_int_const())
            return cond->as_int_const()->value() == 0 ? branch_false : branch_true;
        if (cond->is_float_const())
            return cond->as_float_const()->value() == 0.0 ? branch_false : branch_true;
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
            check_identifier(expr->token());
        }
        if (peek()->is_assign_operator()) {
            auto token = peek();
            next();
            auto rhs = parse_assignment();
            if (token->get_type() != TK_ASSIGN) {
                return complex_assignment(token, expr, rhs);
            }
            auto a = new Assignment(token, expr, rhs);
            check_assignment(a);
            return a;
        }
    }
    // for comma operator
    if (expr->token() != nullptr && expr->token()->get_type() == ',') {
        auto comma = static_cast<Comma *>(expr);
        if (comma->rhs()->kind() <= EXPR_UNARY) {
            if (comma->rhs()->kind() == EXPR_IDENT) {
                check_identifier(comma->rhs()->token());
            }
        }
        if (peek()->is_assign_operator()) {
            auto token = peek();
            next();
            auto rhs = parse_assignment();
            auto a   = new Assignment(token, expr, rhs);
            check_assignment(a);
            return a;
        }
    }
    return expr;
}

void Parser::create_local_variable(const Token *tok, const Type *type) {
    // save local variable
    auto obj = new Object(tok, type, nullptr);
    _cfd->append_local_variable(obj);
    _scope->push_var(tok->get_lexeme(), obj);
}
Expr *Parser::complex_assignment(const Token *tok, Expr *dest, Expr *rhs) {
    auto *fname = Token::fake_name_token(tok->get_position());
    auto *ident = new Identifier(fname, _scope);
    // first, create a local variable to store the address of lhs's result
    auto *ref = new TypedUnaryExpr<'&'>(dest);
    create_local_variable(fname, ref->type());
    auto *expr1 = new Assignment(tok, ident, ref);
    // second, apply binary operation on the value and assign to the result
    auto deref = new TypedUnaryExpr<'*'>(ident);
    const Token *op;
    Binary *bin;
    switch (tok->get_type()) {
    case TK_MUL_ASSIGN: // *=
        op  = Token::make_token('*', tok->get_position());
        bin = new Multi(op, deref, rhs);
        break;
    case TK_DIV_ASSIGN: // /=
        op  = Token::make_token('/', tok->get_position());
        bin = new Multi(op, deref, rhs);
        break;
    case TK_MOD_ASSIGN: // %=
        op  = Token::make_token('%', tok->get_position());
        bin = new Multi(op, deref, rhs);
        break;
    case TK_ADD_ASSIGN: // +=
        op  = Token::make_token('+', tok->get_position());
        bin = new Add(op, deref, rhs);
        break;
    case TK_MINUS_ASSIGN: // -=
        op  = Token::make_token('-', tok->get_position());
        bin = new Add(op, deref, rhs);
        break;
    case TK_LSHIFT_ASSIGN: // <<=
        op  = Token::make_token(TK_LSHIFT, "<<", tok->get_position());
        bin = new Shift(op, deref, rhs);
        break;
    case TK_RSHIFT_ASSIGN: // >>=
        op  = Token::make_token(TK_RSHIFT, ">>", tok->get_position());
        bin = new Shift(op, deref, rhs);
        break;
    case TK_BAND_ASSIGN: // &=
        op  = Token::make_token('&', tok->get_position());
        bin = new BitAnd(op, deref, rhs);
        break;
    case TK_XOR_ASSIGN: // ^=
        op  = Token::make_token('^', tok->get_position());
        bin = new BitXor(op, deref, rhs);
        break;
    case TK_BOR_ASSIGN: // |=
        op  = Token::make_token('|', tok->get_position());
        bin = new BitOr(op, deref, rhs);
        break;
    default:
        error_at(tok, "not a valid assignment operator.");
        bin = nullptr;
    }
    auto *expr2 = new Assignment(tok, deref, bin);

    // finally, make a comma expr
    return new Comma(tok, expr1, expr2);
}

Expr *Parser::parse_comma() {
    auto expr = parse_assignment();
    while (test(',')) {
        auto op = peek();
        next();
        if (expr->is_const())
            expr = parse_assignment();
        else
            expr = new Comma(op, expr, parse_assignment());
    }
    return expr;
}

// statement needs expression
Expr *Parser::parse_expr() { return parse_comma(); }
// make labels while parsing case and default statements
static string caselabelmaker() {
    static int label_conter = 0;
    stringstream s;
    s << ".LCD_" << label_conter++;
    return s.str();
}

static string userdefinedlabel(const char *label) {
    stringstream ss;
    ss << ".Lgoto_" << label;
    return ss.str();
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
    _scope = _scope->drill_down();
    expect('(');
    // parse init part
    Stmt *init;
    if (test(';')) {
        init = Empty::instance();
    } else if (maybe_decl()) {
        auto decl = parse_declaration();
        if (decl == nullptr)
            init = Empty::instance();
        else
            init = decl;
    } else {
        init = new ExprStmt(parse_expr());
    }
    expect(';');
    // parse cond part;
    Expr *cond;
    if (test(';')) {
        cond = nullptr;
    } else {
        cond = parse_expr();
        if (!cond->type()->is_integer()) {
            cond = conv(&BuiltinType::Int, cond);
        }
    }
    expect(';');
    // accumulator part
    Expr *accumulator;
    if (test(')')) {
        accumulator = nullptr;
    } else {
        accumulator = parse_expr();
    }
    expect(')');
    auto body   = parse_stmt();
    auto newfor = new For(init, cond, accumulator, body, _scope);
    _scope      = _scope->float_up();
    return newfor;
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
            auto ident = peek();
            next();
            expect(';');
            return new Goto(ident, userdefinedlabel(ident->get_lexeme()));
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
        return new Return(conv(_cfd->signature()->return_type(), ret));
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
    // typedef'ed name may be conflicted with label, so check if it be a labeled statement
    if (test(TK_NAME)) {
        auto tk = peek();
        next();
        if (try_next(':')) {
            auto stmt = parse_stmt();
            return new Labeled(userdefinedlabel(tk->get_lexeme()), stmt);
        }
        // if cannot parse a labeled statement, it maybe leads declaration or expression
        unget();
    }
    if (maybe_decl()) {
        auto decl = parse_declaration();
        if (decl == nullptr)
            return Empty::instance();
        return decl;
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
    default: {
        auto expr = parse_expr();
        expect(';');
        return new ExprStmt(expr);
    }
    }
    return nullptr;
}

// function definition needs compound statements aka. block
Block *Parser::parse_func_body() {
    expect('{');
    vector<BlockItem *> items;
    while (!try_next('}')) {
        auto item = parse_stmt();
        items.push_back(item);
    }
    auto result = new Block(_scope, items);
    return result;
}

Block *Parser::parse_block() {
    _scope = _scope->drill_down();
    expect('{');
    vector<BlockItem *> items;
    while (!try_next('}')) {
        auto item = parse_stmt();
        if (item->is_return_stmt() && _cfd == nullptr)
            error_at(peek(), "return statement found out of a function body.");
        items.push_back(item);
    }
    auto result = new Block(_scope, items);
    _scope      = _scope->float_up();
    return result;
}

FuncDef *Parser::parse_func_def(const HalfType *base) {
    // check redefined and extern
    auto obj = _scope->find_var(base->token()->get_lexeme());
    if (obj->is_defined())
        error_at(base->token(), "function redefined.");
    if (obj->attr()->is_extern)
        error_at(base->token(), "defined extern function.");
    auto func_type = static_cast<const FuncType *>(base->type());

    // registry func name to scope
    _scope      = _scope->drill_down();
    auto params = func_type->parameters();
    // set current funtion type
    _cfd = new FuncDef(base->token(), func_type, nullptr);
    for (auto param : params) {
        auto ptype  = param->type();
        auto ptoken = param->token();
        auto obj    = new Object(ptoken, ptype, nullptr);
        _cfd->append_local_variable(obj);
        _scope->push_var(param->token()->get_lexeme(), obj);
    }
    auto body = parse_func_body();
    auto ret  = _cfd;
    ret->set_body(body);
    _cfd   = nullptr;
    _scope = _scope->float_up();
    return ret;
}

const Type *Parser::parse_type_name() {
    Attribute attr;
    auto type = parse_declaration_specifiers(&attr);
    auto ad   = parse_declarator(type);
    if (ad->token() != nullptr)
        error_at(ad->token(), "abstract declarator can not declared with an identifier.");
    return ad->type();
}
void Parser::check_init_declarator(const InitDeclarator *id, Attribute *attr) {
    auto token = id->halftype()->token();
    auto obj   = _scope->find_var_in_local(token->get_lexeme());
    auto type  = id->halftype()->type();
    // first defined here
    if (obj == nullptr) {
        obj = new Object(token, type, attr);
        if (_cfd != nullptr && !attr->is_static)
            _cfd->append_local_variable(obj);
        _scope->push_var(token->get_lexeme(), obj);
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
void Parser::check_func_declaration(const HalfType *prefix, Attribute *attr) {
    auto var = _scope->find_var_in_local(prefix->token()->get_lexeme());
    if (var == nullptr) {
        var = new Object(prefix->token(), prefix->type(), attr);
        var->set_defined(false);
        _scope->push_var(prefix->token()->get_lexeme(), var);
        return;
    }
    auto vartype = var->type();
    auto varattr = var->attr();
    // check type
    if (!vartype->equals_to(prefix->type()))
        error_at(prefix->token(), "name redeclared with different type.");
    // check linkage
    if (!attr->equals_to(varattr) || !attr->is_extern)
        error_at(prefix->token(), "redeclared with non-extern.");
}
void Parser::check_identifier(const Token *token) {
    auto obj = _scope->find_var(token->get_lexeme());
    if (obj == nullptr) {
        warn_at(token, "use undeclared variable, assume as int.");
        auto o = new Object(token, &BuiltinType::Int, nullptr);
        if (_cfd != nullptr)
            _cfd->append_local_variable(o);
        _scope->push_var(token->get_lexeme(), o);
    }
}
void Parser::check_assignment(Assignment *a) {
    auto *lefttype = a->lhs()->type();
    if (a->rhs()->is_cond()) {
        auto *cond = a->rhs()->as_cond();
        if (!lefttype->is_compitable_with(cond->iftrue()->type()))
            error_at(cond->iftrue()->token(), "assign %s with uncompitable type %s.",
                     lefttype->normalize().c_str(), cond->iftrue()->type()->normalize().c_str());
        if (!lefttype->is_compitable_with(cond->iffalse()->type()))
            error_at(cond->iffalse()->token(), "assign %s with uncompitable type %s.",
                     lefttype->normalize().c_str(), cond->iffalse()->type()->normalize().c_str());
        cond->set_iftrue(conv(lefttype, cond->iftrue()));
        cond->set_iffalse(conv(lefttype, cond->iffalse()));
        return;
    }
    if (lefttype->is_compitable_with(a->rhs()->type())) {
        a->set_rhs(conv(a->lhs()->type(), a->rhs()));
        return;
    }
    error_at(a->token(), "assign %s with uncompitable type %s.",
             a->lhs()->type()->normalize().c_str(), a->rhs()->type()->normalize().c_str());
}

bool Parser::maybe_decl() {
    auto tk = peek();
    if (tk->get_type() == TK_NAME) {
        auto name = tk->get_lexeme();
        auto obj  = _scope->find_var(name);
        return obj != nullptr && obj->attr() != nullptr && obj->attr()->is_typedef;
    }
    return tk->is_decl_start();
}

void Parser::next() {
    mqassert(_index < _tokens.size(), "lookup tokens is empty.");
    // _consumed.push(_lookups.top());
    // _lookups.pop();
    _index++;
}
const Token *Parser::peek() {
    //    if (_lookups.empty()) {
    //        _lookups.push(_scanner->get_token());
    //    }
    //    return _lookups.top();
    if (_index == _tokens.size())
        _tokens.emplace_back(_scanner->get_token());
    return _tokens.at(_index);
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
    _index--;
    // _lookups.push(_consumed.top());
    // _consumed.pop();
}

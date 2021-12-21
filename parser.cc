#include "parser.h"
#include "ast.h"
#include "error.h"
#include "scanner.h"
#include "scope.h"
#include "token.h"
#include "type.h"

#include <cstdlib>
#include <list>
#include <vector>

using namespace std;

/// static functions

// clang-format off
static ExprKind get_expr_kind(const Token* tok) {
    switch (tok->get_type()) {
        case TK_STAR   : return EXPR_MUL;     // *
        case TK_SLASH  : return EXPR_DIV;     // /
        case TK_MOD    : return EXPR_MOD;     // %
        case TK_PLUS   : return EXPR_ADD;     // +
        case TK_MINUS  : return EXPR_SUB;     // -
        case TK_LSHIFT : return EXPR_BLS;     // <<
        case TK_RSHIFT : return EXPR_BRS;     // >>
        case TK_LESS   : return EXPR_LESS;    // <
        case TK_GREATER: return EXPR_GREATER; // >
        case TK_LEQUAL : return EXPR_LEQUAL;  // <=
        case TK_GEQUAL : return EXPR_GEQUAL;  // >=
        case TK_EQUAL  : return EXPR_EQUAL;   // ==
        case TK_NEQUAL : return EXPR_NEQUAL;  // !=
        case TK_BAND    : return EXPR_BAND;    // &
        case TK_XOR    : return EXPR_BXOR;    // ^
        case TK_BOR     : return EXPR_BOR;     // |
        case TK_LAND   : return EXPR_LAND;    // &&
        case TK_LOR    : return EXPR_LOR;     // ||
        default:
            error_at(tok->get_postion(), "%s is not a binary token",  tok->get_lexeme());
    }
    unreachable();
    // no warning
    return EXPR_INT;
}
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

static int get_priority(int tok_type) {
    switch (tok_type) {
    case TK_STAR:  // *
    case TK_SLASH: // /
    case TK_MOD:   // %
        return OP_MUL;
    case TK_PLUS:  // +
    case TK_MINUS: // -
        return OP_ADD;
    case TK_LSHIFT: // <<
    case TK_RSHIFT: // >>
        return OP_SHIFT;
    case TK_LESS:    // <
    case TK_GREATER: // >
        return OP_RELATION;
    case TK_LEQUAL: // <=
    case TK_GEQUAL: // >=
    case TK_EQUAL:  // ==
    case TK_NEQUAL: // !=
        return OP_EQUAL;
    case TK_BAND: // &
        return OP_BAND;
    case TK_XOR: // ^
        return OP_BXOR;
    case TK_BOR: // |
        return OP_BOR;
    case TK_LAND: // &&
        return OP_LAND;
    case TK_LOR: // ||
        return OP_LOR;
    default:
        return OP_NOT_VALID;
    }
}

static Expr *conv(const Type *type, Expr *expr) {
    if (expr->type()->equals_to(type))
        return expr;
    if (!type->is_compitable_with(expr->type())) {
        error("uncompitable type with implicit conversion: from %s to %s",
              expr->type()->normalize().c_str(), type->normalize().c_str());
    }
    return new ConvExpr(type, expr);
}

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
                auto funcdecl = new Object(prefix->token(), prefix->type(), attr);
                if (test('{')) {
                    current = parse_func_def(prefix);
                } else if (try_next(';')) {
                    funcdecl->set_defined(false);
                } else
                    current = parse_global_variable(prefix->type());
                _scope->push_var(prefix->token()->get_lexeme(), funcdecl);
            } else {
                auto vt = var->type();
                if (!vt->equals_to(prefix->type())) {
                    error_at(prefix->token()->get_postion(),
                             "type redefined conflict: '%s' vs '%s'", vt->normalize().c_str(),
                             prefix->type()->normalize().c_str());
                }
                if (var->is_function()) {
                    if (test('{')) {
                        if (var->is_defined())
                            error_at(prefix->token()->get_postion(), "function redefined.");
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
    // debug("start function: %s", __func__);
    auto tk = peek();
#define error_dup_token(tk)                                                                        \
    error_at(tk->get_postion(), "storage class %s has already been specified.", tk->get_lexeme())
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
        error_at(tk->get_postion(), "typedef may not work with extern, inline, "
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
        error_at(tk->get_postion(), "malformed type.");
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
        debug_token(tk);
        if (tk->is_builtin_type()) {
            type_counter = process_builtin(type_counter);
        } else if (tk->is_storage_class()) {
            // if (attr == nullptr)
            //     error_at(tk->get_postion(), "storage class is not allowed here.");
            process_storage_class(attr);
        } else if (tk->get_type() == TK_NAME) {
            // process user-define types or variable name
            auto name = tk->get_lexeme();
            auto t    = _scope->resolve_name(name);
            // find type
            if (t != nullptr) {
                if (type_counter != 0) {
                    error_at(tk->get_postion(),
                             "user-defined type should not follow up builtin types.");
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
        expect(';');
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
    debug_token(tk);
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
        error_at(tk->get_postion(), "unexpected token while parsing direct declarator");
    }
    next();
    tk = peek();
    debug_token(tk);
    if (tk->get_type() == '(' || tk->get_type() == '[') {
        auto type = parse_array_or_func_decl(base, name->get_lexeme());
        return new HalfType(name, type);
    }
    return new HalfType(name, base);
}

const HalfType *Parser::parse_parameter() {
    auto param_base_type = parse_declaration_specifiers(nullptr);
    debug_token(peek());
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
    else if (test(TK_VOID)) {
        next();
        expect(')');
        return result;
    }
    // (type name)
    auto first = parse_parameter();
    debug("first param: %s", first->token()->get_lexeme());
    result.push_back(first);
    auto tk = peek();
    debug_token(tk);
    while (try_next(',')) {
        auto param = parse_parameter();
        debug("parsing parameter: %s", param->type()->normalize().c_str());
        result.push_back(param);
    }
    expect(')');
    return result;
}

Type *Parser::parse_array_or_func_decl(const Type *fake_base, const char *name) {
    if (test('(')) {
        auto params = parse_parameters();
        return new FuncType(fake_base, string_view(name), params);
    } else if (try_next('[')) {
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
        char *end;
        double dval = std::strtod(tk->get_lexeme(), &end);
        next();
        return new FloatConst(dval, *end == 'f');
    }
    if (tk->get_type() == TK_INUMBER) {
        char *end;
        auto nval = std::strtoull(tk->get_lexeme(), &end, 10);
        if (errno == ERANGE)
            warn_at(tk->get_postion(), "integer number overflow.");
        next();

        bool is_unsigned = false, is_long = false, is_long_long = false;

        // integer literal without postfix
        // will be stored as `int` value
        if (*end == '\0') {
            int ival = nval;
            return new IntConst(ival);
        }
        end++;
        if (*end == 'u' || *end == 'L') {
            is_unsigned = true;
            end++;
        }
        if (*end == 'l' || *end == 'L') {
            end++;
            if (*end == 'l' || *end == 'L') {
                is_long_long = true;
                end++;
            } else
                is_long = true;
        }
        mqassert(*end == '\0', "number literal has trailing character(s).");
        if (is_unsigned) {
            if (is_long_long) {
                return new IntConst(nval);
            } else if (is_long) {
                return new IntConst(static_cast<unsigned long>(nval));
            } else {
                return new IntConst(static_cast<unsigned>(nval));
            }
        } else {
            if (is_long_long) {
                return new IntConst(static_cast<long long>(nval));
            } else if (is_long) {
                return new IntConst(static_cast<long>(nval));
            } else {
                return new IntConst(static_cast<int>(nval));
            }
        }
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
        return new StringLiteral(stok->get_lexeme());
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
        error_at(tk->get_postion(), "unexpected token");
    }
    // no warning
    return nullptr;
}
Expr *Parser::parse_ident() {
    auto tk = peek();
    next();
    return new Identifier(tk->get_lexeme(), _scope);
}
Expr *Parser::parse_constant() { return nullptr; }
Expr *Parser::parse_generic() { return nullptr; }

// postfix-expression:
//  X  primary-expression
//  X  postfix-expression [ expression ]
//  O  postfix-expression ( argument-expression-list(opt) )
//  X  postfix-expression . identifier
//  X  postfix-expression -> identifier
//  X  postfix-expression ++
//  X  postfix-expression --
//  X  ( type-name ) { initializer-list }
//  X  ( type-name ) { initializer-list , }
Expr *Parser::parse_postfix() {
    Expr *primary = parse_primary();
    // parse arguments
    if (test('(')) {
        vector<Expr *> args = parse_args();
        return new FuncCallExpr(primary, args);
    }
    return primary;
}

// argument-expression-list:
//   assignment-expression
//   argument-expression-list , assignment-expression
vector<Expr *> Parser::parse_args() {
    vector<Expr *> ret;
    expect('(');
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
        return new TypedUnaryExpr<'~'>(parse_cast());
    if (try_next('!'))
        return new TypedUnaryExpr<'!'>(parse_cast());
    if (try_next('&'))
        return new TypedUnaryExpr<'&'>(parse_cast());
    if (try_next(TK_SIZEOF)) {
        if (try_next('(')) {
            auto type = parse_type_name();
            if (type) {
                expect(')');
                return new IntConst(type->size());
            }
            unget();
        }
        auto expr = parse_unary();
        return new IntConst(expr->type()->size());
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
            return new CastExpr(type, expr);
        } else {
            while (!test('('))
                unget();
        }
    }
    return parse_unary();
}
// Expr *Parser::parse_mult() { return parse_cast(); }
// Expr *Parser::parse_add() { return parse_mult(); }
// Expr *Parser::parse_shift() { return parse_add(); }
// Expr *Parser::parse_relational() { return parse_shift(); }
// Expr *Parser::parse_equality() { return parse_relational(); }
// Expr *Parser::parse_bit_and() { return parse_equality(); }
// Expr *Parser::parse_bit_xor() { return parse_bit_and(); }
// Expr *Parser::parse_bit_or() { return parse_bit_xor(); }
// Expr *Parser::parse_log_and() { return parse_bit_or(); }
// Expr *Parser::parse_log_or() { return parse_log_and(); }

// @bop binary operator priority
Expr *Parser::parse_binary(int bop) {
    // end cond
    if (bop == OP_NOT_VALID) {
        return parse_postfix();
    }
    // parse lhs
    auto left     = parse_binary(bop + 1);
    auto op       = peek();
    auto priority = get_priority(op->get_type());
    // parse exprs with the same priority
    while (priority == bop) {
        next();
        // rhs should have higher priority
        auto right = parse_binary(bop + 1);
        // combine lhs and rhs, the result will be new lhs within the binary expr
        auto ek = get_expr_kind(op); // ExprKind
        if (left->type()->is_arithmetic() && right->type()->is_arithmetic()) {
            auto ntype = uac(left->type(), right->type());
            left       = new BinaryExpr(ek, conv(ntype, left), conv(ntype, right));
        } else {
            left = new BinaryExpr(ek, left, right);
        }
        op       = peek();
        priority = get_priority(op->get_type());
    }
    return left;
}

Expr *Parser::parse_conditional() {
    auto cond = parse_binary(OP_LOR);
    if (try_next(TK_COND)) {
        auto branch_true = parse_expr();
        expect(TK_COLON);
        auto branch_false = parse_conditional();
        return new CondExpr(cond, branch_true, branch_false);
    }
    return cond;
}
// assignment-expression:
//   conditional-expression
//   unary-expression assignment-operator assignment-expression
Expr *Parser::parse_assignment() {
    auto expr = parse_conditional();
    if (expr->kind() <= EXPR_UNARY) {
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
// block needs statement
Stmt *Parser::parse_stmt() {
    if (maybe_decl()) {
        return parse_declaration();
    }
    auto tk = peek();
    // debug_token(tk);
    switch (tk->get_type()) {
    case TK_RETURN: {
        next();
        if (try_next(';')) {
            return new ReturnStmt(nullptr);
        }
        auto ret = parse_expr();
        expect(';');
        return new ReturnStmt(conv(_cft->return_type(), ret));
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
Block *Parser::parse_block(bool in_func) {
    expect('{');
    vector<BlockItem *> items;
    while (!try_next('}')) {
        auto item = maybe_decl() ? parse_declaration() : parse_stmt();
        if (item->is_return_stmt() && !in_func)
            error_at(peek()->get_postion(), "return statement found out of a function body.");
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
    int idx_int = 0, idx_float = 0;
    for (auto param : params) {
        auto ptype  = param->type();
        auto ptoken = param->token();
        auto obj    = new Object(ptoken, ptype, nullptr);
        // deal with floating number
        if (ptype->is_float()) {
            obj->set_offset(idx_float++);
        }
        // deal with integer and pointer
        // TODO: struct/union
        else {
            obj->set_offset(idx_int++);
        }
        _scope->push_var(param->token()->get_lexeme(), obj);
    }
    // set current funtion type
    _cft      = func_type;
    auto body = parse_block(true);
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
        error_at(token->get_postion(), "redefined variable.");
    }
    // check if the same type
    if (!obj->type()->equals_to(id->halftype()->type())) {
        error_at(token->get_postion(), "redeclared name with different type.");
    }
    // check linkage
    if (!attr->equals_to(obj->attr()) || !attr->is_extern) {
        error_at(token->get_postion(), "redeclared with non-extern.");
    }
    // check extern
    if (attr->is_extern && id->is_initialized()) {
        error_at(token->get_postion(), "declared with both extern and initializer.");
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
        error_at(peek()->get_postion(), "expect `%s', but got `%s'",
                 Token(expected, nullptr).get_lexeme(), peek()->get_lexeme());
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

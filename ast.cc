#include "ast.h"
#include "error.h"
#include "generator.h"
#include "token.h"
#include "type.h"
#include <cctype>
#include <cstddef>
#include <cstdlib>
#include <cstring>
using namespace std;

static const Type *infer_type_by_suffix(const char *end) {
    if (*end == '\0') {
        return &BuiltinType::Int;
    }
    size_t len;
    if ((len = strlen(end)) > 3)
        return nullptr;
    char *dup = strndup(end, len);
    for (size_t idx = 0; dup[idx]; idx++)
        dup[idx] = tolower(dup[idx]);

    if (strcmp(dup, "ull") || strcmp(dup, "ul"))
        return &BuiltinType::ULong;
    if (strcmp(dup, "ll") || strcmp(dup, "l"))
        return &BuiltinType::Long;
    if (strcmp(dup, "u"))
        return &BuiltinType::UInt;
    return nullptr;
}

/// AstNode

void AstNode::accept(Visitor *) { unimplement(); }

/// AstNode end

Empty *Empty::_instance = new Empty();
Empty *Empty::instance() { return _instance; }

/// ExprStmt

void ExprStmt::accept(Visitor *visitor) { _expr->accept(visitor); }

/// ExprStmt end

// labeled

void Labeled::accept(Visitor *v) { v->visit_labeled(this); }

// selection
void IfElse::accept(Visitor *v) { v->visit_ifelse(this); }
void Switch::accept(Visitor *v) { v->visit_switch(this); }

// iteration
void While::accept(Visitor *v) { v->visit_while(this); }
void DoWhile::accept(Visitor *v) { v->visit_do_while(this); }
void For::accept(Visitor *v) { v->visit_for(this); }
// jump
void Goto::accept(Visitor *v) { v->visit_goto(this); }
void Continue::accept(Visitor *v) { v->visit_continue(this); }
void Break::accept(Visitor *v) { v->visit_break(this); }

void Return::accept(Visitor *v) { v->visit_return(this); }

void Multi::accept(Visitor *v) { v->visit_mult(this); }

void Add::accept(Visitor *v) { v->visit_additive(this); }
const Type *Add::type() const noexcept {
    // 1. subtraction result of two pointers is the count of object between the two pointers,
    //    while the object is refed by the pointers.
    // 2. if one of the oprands is a pointer, it should be placed at lhs, this is ensured by
    //    type-checking
    return lhs()->type()->is_derefed() && rhs()->type()->is_derefed() ? &BuiltinType::Long
                                                                      : lhs()->type();
}

void Shift::accept(Visitor *v) { v->visit_shift(this); }
void Relational::accept(Visitor *v) { v->visit_relational(this); }
void Bitwise::accept(Visitor *v) { v->visit_bitwise(this); }
void Logical::accept(Visitor *v) { v->visit_logical(this); }

void FloatConst::accept(Visitor *v) { v->visit_float_const(this); }

/// IntConst

IntConst::IntConst(const Token *token) : Const(EXPR_INT), _token(token) {
    char *end;
    // got the value and assign
    auto nval = std::strtoull(_token->get_lexeme(), &end, 10);
    if (errno == ERANGE)
        warn_at(_token, "integer number overflow.");
    _value = nval;
    if ((_type = infer_type_by_suffix(end)) == nullptr)
        error_at(token, "invalid postfix for integer constant.");
}
void IntConst::accept(Visitor *v) { v->visit_int_const(this); }

/// IntConst end
/// Identifier
void Identifier::accept(Visitor *v) { v->visit_identifier(this); }
const Type *Identifier::type() const noexcept {
    if (_scope->find_var(_token->get_lexeme()) == nullptr)
        return nullptr;
    return _scope->find_var(_token->get_lexeme())->type();
}

/// Identifier end

const Type *Subscript::type() const noexcept {
    return static_cast<const ArrayType *>(_array->type())->elem_type();
}

/// FuncCallExpr

void FuncCall::accept(Visitor *visitor) { visitor->visit_func_call(this); }

const Type *FuncCall::type() const noexcept {
    auto functype = _left->type();
    if (functype == nullptr)
        return &BuiltinType::Int;
    mqassert(functype->is_function(),
             "left hand of a function call expression must has a function type.");
    return static_cast<const FuncType *>(functype)->return_type();
}

/// FuncCallExpr end
void StringLiteral::accept(Visitor *v) { v->visit_string_literal(this); }
const Type *StringLiteral::type() const noexcept {
    return new ArrayType(&BuiltinType::Char, strlen(_token->get_lexeme()) + 1);
}

void Assignment::accept(Visitor *v) { v->visit_assignment(this); }
void Conv::accept(Visitor *v) { v->visit_conv(this); }
void Cast::accept(Visitor *v) { v->visit_cast(this); }
void Unary::accept(Visitor *v) { v->visit_unary(this); }
const Type *Unary::type() const noexcept {
    if (unary_type() == '*') {
        mqassert(_oprand->type()->is_derefed(),
                 "unary '*' could only apply on pointer or array types.");
        return _oprand->type()->derefed();
    }
    return unary_type() == '&' ? new PointerType(_oprand->type()) : _oprand->type();
}
/// FuncDef

void FuncDef::accept(Visitor *visitor) { visitor->visit_func_def(this); }
const string_view FuncDef::func_name() const noexcept { return _func_name->get_lexeme(); }
int FuncDef::stack_size() const noexcept { return _body->scope()->offset(); }

/// FuncDef end

void InitDeclarator::accept(Visitor *v) { v->visit_init_declarator(this); }

void Block::accept(Visitor *v) { v->visit_block(this); }

void Initializer::accept(Visitor *v) { v->visit_initializer(this); }

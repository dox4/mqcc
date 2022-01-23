#include "ast.h"
#include "error.h"
#include "generator.h"
#include "token.h"
#include "type.h"
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
using namespace std;

// static const Type *infer_type_by_suffix(const char *end) {
//     if (*end == '\0') {
//         return &BuiltinType::Int;
//     }
//     size_t len;
//     if ((len = strlen(end)) > 3)
//         return nullptr;
//     char *dup = strndup(end, len);
//     for (size_t idx = 0; dup[idx]; idx++)
//         dup[idx] = tolower(dup[idx]);
//
//     if (strcmp(dup, "ull") || strcmp(dup, "ul"))
//         return &BuiltinType::ULong;
//     if (strcmp(dup, "ll") || strcmp(dup, "l"))
//         return &BuiltinType::Long;
//     if (strcmp(dup, "u"))
//         return &BuiltinType::UInt;
//     return nullptr;
// }

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

void Cond::accept(Visitor *v) { v->visit_cond(this); }

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
void Comma::accept(Visitor *v) { v->visit_comma(this); }

void FloatConst::accept(Visitor *v) { v->visit_float_const(this); }

/// IntConst

IntConst::IntConst(const Token *token)
    : Const(EXPR_INT), _value(token->value<int64_t>()), _token(token) {
    switch (token->get_type()) {
    case TK_INT_LITERAL:
        _type = &BuiltinType::Int;
        break;
    case TK_UINT_LITERAL:
        _type = &BuiltinType::UInt;
        break;
    case TK_LONG_LITERAL:
        _type = &BuiltinType::Long;
        break;
    case TK_ULONG_LITERAL:
        _type = &BuiltinType::ULong;
        break;
    default:
        unreachable();
    }
}
void IntConst::accept(Visitor *v) { v->visit_int_const(this); }

IntConst *IntConst::one(const Token *tok) { return new IntConst(tok, 1); }
IntConst *IntConst::zero(const Token *tok) { return new IntConst(tok, 0); }
/// IntConst end
/// Identifier
void Identifier::accept(Visitor *v) { v->visit_identifier(this); }
const Type *Identifier::type() const noexcept {
    if (_scope->find_var(_token->get_lexeme()) == nullptr) {
        warn_at(_token, "use undeclared name, assuming as `int`.");
        return &BuiltinType::Int;
    }
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
void PostInc::accept(Visitor *v) { v->visit_postfix_inc(this); }
void PostDec::accept(Visitor *v) { v->visit_postfix_dec(this); }
void MemberAccess::accept(Visitor *v) { v->visit_member_access(this); }

/// FuncCallExpr end
void StringLiteral::accept(Visitor *v) { v->visit_string_literal(this); }
// const Type *StringLiteral::type() const noexcept {
//     return new ArrayType(&BuiltinType::Char, strlen(_token->get_lexeme()) + 1);
// }
void StmtExpr::accept(Visitor *v) { v->visit_block(_block); }
const Type *StmtExpr::type() const noexcept {
    return (*_block->items().rbegin())->as_expr_statement()->expr()->type();
}

void Assignment::accept(Visitor *v) { v->visit_assignment(this); }
void Conv::accept(Visitor *v) { v->visit_conv(this); }
void Cast::accept(Visitor *v) { v->visit_cast(this); }
void Unary::accept(Visitor *v) { v->visit_unary(this); }
const Type *Unary::type() const noexcept {
    static const PointerType *_p = nullptr;
    if (unary_type() == '*') {
        return _oprand->type()->derefed();
    }
    if (unary_type() == '&') {
        if (_p == nullptr)
            _p = new PointerType(_oprand->type());
        return _p;
    }
    return unary_type() == '!' ? &BuiltinType::Int : _oprand->type();
}
const Type *MemberAccess::type() const noexcept {
    return (token()->get_type() == TK_ARROW)
               ? _lhs->type()->derefed()->as_struct()->find_member(_member->get_value())->type()
               : _lhs->type()->as_struct()->find_member(_member->get_value())->type();
}
/// FuncDef

void FuncDef::accept(Visitor *visitor) { visitor->visit_func_def(this); }
const string_view FuncDef::func_name() const noexcept { return _func_name->get_lexeme(); }
// int FuncDef::stack_size() const noexcept { return _body->scope()->offset(); }

void FuncDef::set_offset_for_local_vars() {
    int offset = 0;
    for (auto ro = _locals.rbegin(); ro != _locals.rend(); ro++) {
        auto &o = *ro;
        offset += o->type()->size();
        offset = align_to(offset, o->type()->align());
        o->set_offset(-offset);
    }
    _stack_size = align_to(offset, 16);
}
/// FuncDef end

void InitDeclarator::accept(Visitor *v) { v->visit_init_declarator(this); }

void Block::accept(Visitor *v) { v->visit_block(this); }

void Initializer::accept(Visitor *v) { v->visit_initializer(this); }

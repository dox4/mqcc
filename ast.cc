#include "ast.h"
#include "error.h"
#include "generator.h"
#include "token.h"
#include "type.h"
using namespace std;

/// AstNode

void AstNode::accept(Visitor *) { unimplement(); }

/// AstNode end

/// ExprStmt

void ExprStmt::accept(Visitor *visitor) { _expr->accept(visitor); }

/// ExprStmt end

/// ReturnStmt

void ReturnStmt::accept(Visitor *v) { v->visit_return(this); }

/// ReturnStmt end

/// BinaryExpr

void BinaryExpr::accept(Visitor *visitor) { visitor->visit_binary(this); }

const Type *BinaryExpr::type() const noexcept {
    auto lt = _lhs->type();
    auto rt = _rhs->type();
    return uac(lt, rt);
}

/// BinaryExpr end

void FloatConst::accept(Visitor *v) { v->visit_float_const(this); }

/// IntConst

void IntConst::accept(Visitor *v) { v->visit_int_const(this); }

/// IntConst end
/// Identifier
void Identifier::accept(Visitor *v) { v->visit_identifier(this); }
const Type *Identifier::type() const noexcept { return _scope->find_var_in_local(_name)->type(); }

/// Identifier end

/// FuncCallExpr

void FuncCallExpr::accept(Visitor *visitor) { visitor->visit_func_call(this); }

const Type *FuncCallExpr::type() const noexcept { return _left->type(); }

/// FuncCallExpr end

void ConvExpr::accept(Visitor *v) { v->visit_conv(this); }
void CastExpr::accept(Visitor *v) { v->visit_cast(this); }
void UnaryExpr::accept(Visitor *v) { v->visit_unary(this); }
/// FuncDef

void FuncDef::accept(Visitor *visitor) { visitor->visit_func_def(this); }
const string_view FuncDef::func_name() const noexcept { return _func_name->get_lexeme(); }

/// FuncDef end

void InitDeclarator::accept(Visitor *v) { v->visit_init_declarator(this); }

void Block::accept(Visitor *v) { v->visit_block(this); }

void Initializer::accept(Visitor *v) { v->visit_initializer(this); }

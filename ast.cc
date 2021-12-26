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

static const Type *infer_type_by_postfix(const char *end) {
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

// iteration

// iteration
void While::accept(Visitor *v) { v->visit_while(this); }
void DoWhile::accept(Visitor *v) { v->visit_do_while(this); }
void For::accept(Visitor *v) { v->visit_for(this); }
// jump
void Goto::accept(Visitor *v) { v->visit_goto(this); }
void Continue::accept(Visitor *v) { v->visit_continue(this); }
void Break::accept(Visitor *v) { v->visit_break(this); }

/// ReturnStmt

void Return::accept(Visitor *v) { v->visit_return(this); }

/// ReturnStmt end

/// BinaryExpr

void BinaryExpr::accept(Visitor *visitor) { visitor->visit_binary(this); }

const Type *BinaryExpr::type() const noexcept {
    if (_token->is_comparator() || _token->get_type() == TK_LAND || _token->get_type() == TK_LOR)
        return &BuiltinType::Int;
    return _lhs->type();
}

/// BinaryExpr end

void FloatConst::accept(Visitor *v) { v->visit_float_const(this); }

/// IntConst

IntConst::IntConst(const Token *token) : PrimaryExpr(EXPR_INT), _token(token) {
    char *end;
    // got the value and assign
    auto nval = std::strtoull(_token->get_lexeme(), &end, 10);
    if (errno == ERANGE)
        warn_at(_token, "integer number overflow.");
    _value = nval;
    if ((_type = infer_type_by_postfix(end)) == nullptr)
        error_at(token, "invalid postfix for integer constant.");
}
void IntConst::accept(Visitor *v) { v->visit_int_const(this); }

/// IntConst end
/// Identifier
void Identifier::accept(Visitor *v) { v->visit_identifier(this); }
const Type *Identifier::type() const noexcept {
    return _scope->find_var_in_local(_token->get_lexeme())->type();
}

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

#ifndef _MQCC_AST_H__
#define _MQCC_AST_H__

#include "error.h"
#include "scope.h"
#include "token.h"
#include "type.h"
#include "typedefs.h"
#include <cstdint>
#include <cstring>
#include <list>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

/// forward declaration

class FuncType;
class HalfType;
class Token;

/// forward declaration end

enum AstType {};

class Visitor;
class InitDeclarator;
class ExprStmt;
class Identifier;
class AstNode {
  public:
    // every ast node should implement its own accept function
    // to invoke corresponding visit_xxx function
    virtual void accept(Visitor *visitor);
    virtual ~AstNode() = default;
    virtual bool is_return_stmt() const noexcept { return false; }
    // virtual bool is_func_def() const noexcept { return false; }
    virtual bool is_block() const noexcept { return false; }
};

// external declaration:
//   declaration
//   fuction definition

class ExtDecl : public AstNode {};

// declaration-specifiers:
//   storage-class-specifier declaration-specifiers(opt)
//   type-specifier declaration-specifiers(opt)
//   type-qualifier declaration-specifiers(opt)
//   function-specifier declaration-specifiers(opt)
//   alignment-specifier declaration-specifiers(opt)

enum ExprKind {
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_FUNC_CALL,
    EXPR_SUBSCRIPT, // array subscription
    EXPR_PINC,      // postfix increment
    EXPR_PDEC,      // postfix decrement
    EXPR_MEMBER,    // postfix member access
    EXPR_IDENT,
    EXPR_STR,
    EXPR_STMT,

    // unary expression
    EXPR_UNARY,
    // cast
    EXPR_CAST,

    EXPR_BINARY,

    EXPR_COND, // :?

    EXPR_CONV, // implicit conversion

    //
    EXPR_ASSIGNMENT, // assignment expression

};

class Expr : public AstNode {
  public:
    Expr(ExprKind kind) : _kind(kind) {}
    const ExprKind kind() const noexcept { return _kind; }
    // TODO: should be pure virtual
    virtual const Type *type() const noexcept {
        unimplement();
        return nullptr;
    };
    virtual bool is_unary() const noexcept { return false; }
    virtual bool is_int_const() const noexcept { return false; }
    virtual bool is_const() const noexcept { return false; }
    virtual const Token *token() const { return nullptr; }

  private:
    const ExprKind _kind;
};

class Cond : public Expr {
  public:
    explicit Cond(Expr *cond, Expr *iftrue, Expr *iffalse)
        : Expr(EXPR_COND), _cond(cond), _iftrue(iftrue), _iffalse(iffalse) {}
    // virtual void accept(Visitor *);
    const Expr *cond() const noexcept { return _cond; }
    const Expr *iftrue() const noexcept { return _iftrue; }
    const Expr *iffalse() const noexcept { return _iffalse; }
    virtual const Token *token() const { return _cond->token(); }

  private:
    const Expr *_cond, *_iftrue, *_iffalse;
};

class Binary : public Expr {
  public:
    Binary(const Token *token, Expr *lhs, Expr *rhs)
        : Expr(EXPR_BINARY), _token(token), _lhs(lhs), _rhs(rhs) {}
    Expr *lhs() const noexcept { return _lhs; }
    Expr *rhs() const noexcept { return _rhs; }
    void set_lhs(Expr *expr) { _lhs = expr; }
    void set_rhs(Expr *expr) { _rhs = expr; }
    // virtual void accept(Visitor *);
    virtual const Type *type() const noexcept { return _lhs->type(); }
    virtual const Token *token() const { return _token; }

  private:
    const Token *_token;
    Expr *_lhs, *_rhs;
};

class Multi : public Binary {
  public:
    explicit Multi(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *);
};

class Add : public Binary {
  public:
    explicit Add(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept;
};

class Shift : public Binary {
  public:
    explicit Shift(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *);
};

class Relational : public Binary {
  public:
    explicit Relational(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept { return &BuiltinType::Int; }
};

class Equality : public Relational {
  public:
    explicit Equality(const Token *token, Expr *lhs, Expr *rhs) : Relational(token, lhs, rhs) {}
};

class Bitwise : public Binary {
  public:
    explicit Bitwise(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *);
};

class BitAnd : public Bitwise {
  public:
    explicit BitAnd(const Token *token, Expr *lhs, Expr *rhs) : Bitwise(token, lhs, rhs) {}
};

class BitXor : public Bitwise {
  public:
    explicit BitXor(const Token *token, Expr *lhs, Expr *rhs) : Bitwise(token, lhs, rhs) {}
};

class BitOr : public Bitwise {
  public:
    explicit BitOr(const Token *token, Expr *lhs, Expr *rhs) : Bitwise(token, lhs, rhs) {}
};

class Logical : public Binary {
  public:
    explicit Logical(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept { return &BuiltinType::Int; }
};

class LogAnd : public Logical {
  public:
    explicit LogAnd(const Token *token, Expr *lhs, Expr *rhs) : Logical(token, lhs, rhs) {}
};

class LogOr : public Logical {
  public:
    explicit LogOr(const Token *token, Expr *lhs, Expr *rhs) : Logical(token, lhs, rhs) {}
};

class Comma : public Binary {
  public:
    explicit Comma(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual const Type *type() const noexcept { return rhs()->type(); }
    virtual void accept(Visitor *);
};

class Postfix : public Expr {
  public:
    Postfix(ExprKind kind) : Expr(kind) {}
};
class Subscript : public Postfix {
  public:
    explicit Subscript(Expr *array, Expr *sub)
        : Postfix(EXPR_SUBSCRIPT), _array(array), _sub(sub) {}
    Expr *array() const noexcept { return _array; }
    Expr *sub() const noexcept { return _sub; }
    virtual const Token *token() const { return _array->token(); }
    virtual const Type *type() const noexcept;

  private:
    Expr *_array;
    Expr *_sub;
};
class FuncCall : public Postfix {
  public:
    explicit FuncCall(Expr *left, std::vector<Expr *> args)
        : Postfix(EXPR_FUNC_CALL), _left(left), _args(args) {}

    virtual void accept(Visitor *);
    Expr *left() const noexcept { return _left; }
    std::vector<Expr *> &args() noexcept { return _args; }
    virtual const Type *type() const noexcept;
    virtual const Token *token() const { return _left->token(); }

  private:
    Expr *_left;
    std::vector<Expr *> _args;
};

class PostInc : public Postfix {
  public:
    explicit PostInc(Expr *base) : Postfix(EXPR_PINC), _base(base) {}
    virtual const Token *token() const { return _base->token(); }
    virtual const Type *type() const noexcept { return _base->type(); }
    virtual void accept(Visitor *);

  private:
    Expr *_base;
};

class PostDec : public Postfix {
  public:
    explicit PostDec(Expr *base) : Postfix(EXPR_PDEC), _base(base) {}
    virtual const Token *token() const { return _base->token(); }
    virtual const Type *type() const noexcept { return _base->type(); }
    virtual void accept(Visitor *);

  private:
    Expr *_base;
};

class MemberAccess : public Postfix {
  public:
    explicit MemberAccess(Expr *expr, Identifier *ident, const Token *op)
        : Postfix(EXPR_MEMBER), _lhs(expr), _member(ident), _op(op) {}
    virtual const Token *token() const { return _op; }
    virtual const Type *type() const noexcept;
    virtual void accept(Visitor *);
    Expr *lhs() const noexcept { return _lhs; }
    Identifier *member() const noexcept { return _member; }

  private:
    Expr *_lhs;
    Identifier *_member;
    const Token *_op;
};

// primary-expression:
//   identifier
//   constant
//   string-literal
//   ( expression )
//   ( block-stmt )
//   generic-selection

class PrimaryExpr : public Expr {
  public:
    PrimaryExpr(ExprKind kind) : Expr(kind) {}
    // TODO: should be pure virtual
    virtual const Type *type() const noexcept { return nullptr; };
};

class Identifier : public PrimaryExpr {
  public:
    explicit Identifier(const Token *token, const Scope *scope)
        : PrimaryExpr(EXPR_IDENT), _token(token), _scope(scope) {}
    const char *get_value() const noexcept { return _token->get_lexeme(); }
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept;
    virtual const Token *token() const { return _token; }

  private:
    const Token *_token;
    const Scope *_scope;
};

// constant:
//   integer-constant
//   floating-constant
//   enumeration-constant
//   character-constant
class Const : public PrimaryExpr {
  protected:
    explicit Const(ExprKind kind) : PrimaryExpr(kind){};

  public:
    virtual bool is_const() const noexcept { return true; }
};
class FloatConst : public Const {
  public:
    explicit FloatConst(const Token *token) : Const(EXPR_FLOAT), _token(token) {
        bool isfloat = _token->get_type() == TK_FLOAT_LITERAL;
        _type        = isfloat ? &BuiltinType::Float : &BuiltinType::Double;
        _value       = isfloat ? _token->value<float>() : _token->value<double>();
    }
    const double value() const noexcept { return _value; }
    virtual const Type *type() const noexcept { return _type; }
    virtual const Token *token() const { return _token; }
    virtual void accept(Visitor *);

  private:
    double _value;
    const Token *_token;
    const Type *_type;
};
class IntConst : public Const {
  public:
    explicit IntConst(const Token *token);
    explicit IntConst(const Token *token, int ival)
        : Const(EXPR_INT), _value(ival), _token(token), _type(&BuiltinType::Int) {}
    explicit IntConst(const Token *token, std::uint64_t ival)
        : Const(EXPR_INT), _value(ival), _token(token), _type(&BuiltinType::ULong) {}
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept { return _type; }
    virtual bool is_int_const() const noexcept { return true; }
    virtual const Token *token() const noexcept { return _token; }
    const unsigned long value() const noexcept { return _value; }
    void neg() { _value = -_value; }

  private:
    std::uint64_t _value;
    const Token *_token;
    const Type *_type;
};

class StringLiteral : public Const {
  public:
    explicit StringLiteral(const Token *tok)
        : Const(EXPR_STR), _token(tok),
          _type(new ArrayType(&BuiltinType::Char, strlen(_token->value<const char *>()) + 1)){};
    const char *get_value() const noexcept { return _token->value<const char *>(); }
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept { return _type; }
    virtual const Token *token() const noexcept { return _token; }

  private:
    const Token *_token;
    const Type *_type;
};
class Block;
class StmtExpr : public PrimaryExpr {
  public:
    explicit StmtExpr(Block *block) : PrimaryExpr(EXPR_STMT), _block(block){};
    Block *block() const noexcept { return _block; }
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept;

  private:
    Block *_block;
};

class Unary : public Expr {
  protected:
    explicit Unary(Expr *oprand) : Expr(EXPR_UNARY), _oprand(oprand) {}

  public:
    virtual bool is_unary() const noexcept { return true; }
    virtual int unary_type() const noexcept = 0;
    virtual void accept(Visitor *);
    Expr *oprand() const noexcept { return _oprand; }
    virtual const Type *type() const noexcept;

  private:
    Expr *_oprand;
};

template <int Type> class TypedUnaryExpr : public Unary {
  public:
    explicit TypedUnaryExpr(Expr *oprand) : Unary(oprand) {}
    virtual int unary_type() const noexcept { return Type; };
};

class Cast : public Expr {
  public:
    explicit Cast(const Type *to, Expr *expr) : Expr(EXPR_CAST), _to(to), _expr(expr) {}
    const Type *to_type() const noexcept { return _to; }
    Expr *from_expr() const noexcept { return _expr; }
    virtual const Type *type() const noexcept override { return to_type(); }
    virtual void accept(Visitor *) override;

  private:
    const Type *_to;
    Expr *_expr;
};

class Assignment : public Binary {
  public:
    explicit Assignment(const Token *token, Expr *lhs, Expr *rhs) : Binary(token, lhs, rhs) {}
    virtual void accept(Visitor *) override;
    virtual const Type *type() const noexcept override { return lhs()->type(); }
};

// implicit conversion expression
class Conv : public Expr {
  public:
    explicit Conv(const Type *type, Expr *expr) : Expr(EXPR_CONV), _type(type), _expr(expr) {}
    virtual void accept(Visitor *);
    const Type *type() const noexcept { return _type; }
    Expr *expr() const { return _expr; }

  private:
    const Type *_type;
    Expr *_expr;
};

// statement:
//   labeled-statement
//   compound-statement
//   expression-statement
//   selection-statement
//   iteration-statement
//   jump-statement

// compound-statement:
//   { block-item-listopt }
// block-item-list:
//   block-item
//   block-item-list block-item
// block-item:
//   declaration
//   statement

class BlockItem : public ExtDecl {
  public:
    virtual bool is_expr_statement() const noexcept { return false; }
    virtual ExprStmt *as_expr_statement() noexcept {
        unimplement();
        return nullptr;
    }
    virtual bool is_init_declarator() const noexcept { return false; }
    virtual InitDeclarator *as_init_declarator() noexcept {
        unimplement();
        return nullptr;
    }
};

class Stmt : public BlockItem {};

// labeled
class Labeled : public Stmt {
  public:
    explicit Labeled(const char *label, Stmt *stmt) : _label(label), _stmt(stmt) {}
    explicit Labeled(const std::string label, Stmt *stmt) : _label(label), _stmt(stmt) {}
    const std::string &label() const noexcept { return _label; }
    Stmt *stmt() const noexcept { return _stmt; }
    virtual void accept(Visitor *);

  private:
    const std::string _label;
    Stmt *_stmt;
};

class Block : public Stmt {
  public:
    explicit Block(const Scope *scope, const std::vector<BlockItem *> items)
        : _scope(scope), _block_items(items) {}
    const Scope *scope() const noexcept { return _scope; }
    const std::vector<BlockItem *> &items() const noexcept { return _block_items; }
    virtual void accept(Visitor *);

  private:
    const Scope *_scope;
    std::vector<BlockItem *> _block_items;
};

class Empty : public Stmt {
  private:
    explicit Empty() {}
    virtual void accept(Visitor *) {}
    static Empty *_instance;

  public:
    static Empty *instance();
};

class ExprStmt : public Stmt {
  public:
    explicit ExprStmt(Expr *expr) : _expr(expr) {}
    Expr *expr() const noexcept { return _expr; }
    virtual bool is_expr_statement() const noexcept { return true; }
    virtual ExprStmt *as_expr_statement() noexcept { return this; }
    virtual void accept(Visitor *);

  private:
    Expr *_expr;
};
// selection
class IfElse : public Stmt {
  public:
    explicit IfElse(Expr *cond, Stmt *then, Stmt *otherwise)
        : _cond(cond), _then(then), _otherwise(otherwise) {}
    Expr *cond() const noexcept { return _cond; }
    Stmt *then() const noexcept { return _then; }
    Stmt *otherwise() const noexcept { return _otherwise; }
    virtual void accept(Visitor *);

  private:
    Expr *_cond;
    Stmt *_then, *_otherwise;
};

using CaseDefaultList = std::vector<std::pair<IntConst *, const std::string &>>;
class Switch : public Stmt {
  public:
    explicit Switch(Expr *expr, Stmt *body, CaseDefaultList labels)
        : _expr(expr), _body(body), _labels(labels) {}
    Expr *expr() const noexcept { return _expr; }
    Stmt *body() const noexcept { return _body; }
    const CaseDefaultList &labels() const noexcept { return _labels; }
    virtual void accept(Visitor *);

  private:
    Expr *_expr;
    Stmt *_body;
    CaseDefaultList _labels;
};

// iteration
class Iteration : public Stmt {};
class While : public Iteration {
  public:
    explicit While(Expr *cond, Stmt *body) : _cond(cond), _body(body) {}
    Expr *cond() const noexcept { return _cond; }
    Stmt *body() const noexcept { return _body; }
    virtual void accept(Visitor *);

  private:
    Expr *_cond;
    Stmt *_body;
};
class DoWhile : public While {
  public:
    explicit DoWhile(Stmt *body, Expr *cond) : While(cond, body) {}
    virtual void accept(Visitor *v);
};

class For : public While {
  public:
    explicit For(Stmt *init, Expr *cond, Expr *acc, Stmt *body, const Scope *scope)
        : While(cond, body), _init(init), _accumulator(acc), _scope(scope) {}
    Stmt *init() const noexcept { return _init; }
    Expr *accumulator() const noexcept { return _accumulator; }
    const Scope *scope() const noexcept { return _scope; }
    virtual void accept(Visitor *v);

  private:
    Stmt *_init;
    Expr *_accumulator;
    const Scope *_scope;
};

class Jump : public Stmt {};
class Goto : public Jump {
  public:
    explicit Goto(const char *label) : _label(label) {}
    const char *label() const noexcept { return _label; }
    virtual void accept(Visitor *);

  private:
    const char *_label;
};
class Continue : public Jump {
  public:
    virtual void accept(Visitor *);
};
class Break : public Jump {
  public:
    virtual void accept(Visitor *);
};
class Return : public Jump {
  public:
    explicit Return(Expr *expr) : _expr(expr) {}
    Expr *expr() const noexcept { return _expr; }
    virtual void accept(Visitor *);
    virtual bool is_return_stmt() const noexcept { return true; }

  private:
    Expr *_expr;
};

// direct-declarator:
//   identifier
//   ( declarator )
//   direct-declarator [ type-qualifier-list(opt) assignment-expression(opt) ]
//   direct-declarator [ static type-qualifier-list(opt) assignment-expression ]
//   direct-declarator [ type-qualifier-list static assignment-expression ]
//   direct-declarator [ type-qualifier-list(opt) * ]
//   direct-declarator ( parameter-type-list )
//   direct-declarator ( identifier-list(opt) )
class Pointer {
  public:
    Pointer() = default;
};
class DirectDeclarator : public AstNode {
  public:
    DirectDeclarator(Identifier *identifier) : _identifier(identifier) {}
    std::string_view get_name() const { return std::string_view(_identifier->get_value()); }
    virtual void accept(Visitor *);

  private:
    Identifier *_identifier;
};

class Declarator : public AstNode {
  public:
    explicit Declarator(Pointer *pointer, DirectDeclarator *direct)
        : _pointer(pointer), _direct_declarator(direct) {}
    std::string_view get_name() const { return _direct_declarator->get_name(); }
    virtual void accept(Visitor *);

  private:
    Pointer *_pointer;
    DirectDeclarator *_direct_declarator;
};

// init-declarator:
//   declarator
//   declarator = initializer
class Initializer : public AstNode {
  public:
    explicit Initializer(Expr *assignment) : _element(assignment) {}
    Expr *assignment() const noexcept { return std::get<Expr *>(_element); }
    const bool is_assignment() const noexcept { return _is_assignment; }
    void set_expr(Expr *expr) { _element.emplace<Expr *>(expr); }
    virtual void accept(Visitor *);

  private:
    const bool _is_assignment = true;
    std::variant<Expr *, Initializer *> _element;
};
class InitDeclarator : public BlockItem {
  public:
    explicit InitDeclarator(const HalfType *prefix, Initializer *initializer)
        : _declarator(prefix), _initializer(initializer){};
    bool is_initialized() const noexcept { return _initializer != nullptr; }
    const HalfType *halftype() const noexcept { return _declarator; }
    Initializer *initializer() { return _initializer; }
    virtual bool is_init_declarator() const noexcept { return true; }
    virtual InitDeclarator *as_init_declarator() noexcept { return this; }
    virtual void accept(Visitor *);

  private:
    const HalfType *_declarator;
    Initializer *_initializer;
};

class Declaration : public ExtDecl {
  public:
    explicit Declaration();

  private:
};

// function-definition:
//   declaration-specifiers declarator declaration-list(opt) compound-statement
// declaration-list:
//   declaration
//   declaration-list declaration
class FuncDef : public ExtDecl {
  public:
    explicit FuncDef(const Token *name, const FuncType *sig, Block *block)
        : _func_name(name), _singnature(sig), _body(block) {}
    void accept(Visitor *);
    const std::string_view func_name() const noexcept;
    Block *body() const noexcept { return _body; }
    void set_body(Block *body) { _body = body; }
    int stack_size() const noexcept { return _stack_size; }
    void append_local_variable(Object *o) { _locals.push_back(o); }
    void set_offset_for_local_vars();
    std::list<Object *> local_vars() const noexcept { return _locals; }
    const FuncType *signature() const noexcept { return _singnature; }
    // virtual bool is_func_def() const noexcept { return true; }

  private:
    const Token *_func_name;
    const FuncType *_singnature;
    int _stack_size = 0;
    std::list<Object *> _locals;
    Block *_body;
};

// translation unit ::= external-declaration +
class TransUnit {
  public:
    explicit TransUnit(std::list<FuncDef *> funcs, std::list<InitDeclarator *> gvars)
        : _func_defs(funcs), _global_variables(gvars) {}
    std::list<FuncDef *> func_defs() const { return _func_defs; }
    std::list<InitDeclarator *> global_vars() const { return _global_variables; }

  private:
    std::list<FuncDef *> _func_defs;
    std::list<InitDeclarator *> _global_variables;
};
#endif

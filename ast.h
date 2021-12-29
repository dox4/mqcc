#ifndef _MQCC_AST_H__
#define _MQCC_AST_H__

#include "error.h"
#include "scope.h"
#include "token.h"
#include "type.h"
#include "typedefs.h"
#include <cstdint>
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
class AstNode {
  public:
    // every ast node should implement its own accept function
    // to invoke corresponding visit_xxx function
    virtual void accept(Visitor *visitor);
    virtual ~AstNode() = default;
    virtual bool is_return_stmt() const noexcept { return false; }
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
    EXPR_SUBSCRIPT,
    EXPR_IDENT,
    EXPR_STR,

    // unary expression
    EXPR_UNARY,
    // cast
    EXPR_CAST,

    // binary expression
    EXPR_MUL,     // *
    EXPR_DIV,     // /
    EXPR_MOD,     // %
    EXPR_ADD,     // +
    EXPR_SUB,     // -
    EXPR_BLS,     // <<
    EXPR_BRS,     // >>
    EXPR_LESS,    // <
    EXPR_GREATER, // >
    EXPR_LEQUAL,  // <=
    EXPR_GEQUAL,  // >=
    EXPR_EQUAL,   // ==
    EXPR_NEQUAL,  // !=
    EXPR_BAND,    // &
    EXPR_BXOR,    // ^
    EXPR_BOR,     // |
    EXPR_LAND,    // &&
    EXPR_LOR,     // ||

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
    virtual const Type *type() const noexcept { return nullptr; };
    virtual bool is_unary() const noexcept { return false; }
    virtual bool is_int_const() const noexcept { return false; }
    virtual const Token *token() const { return nullptr; }

  private:
    const ExprKind _kind;
};

class CondExpr : public Expr {
  public:
    explicit CondExpr(Expr *cond, Expr *iftrue, Expr *iffalse)
        : Expr(EXPR_COND), _cond(cond), _iftrue(iftrue), _iffalse(iffalse) {}
    // virtual void accept(Visitor *);
    const Expr *cond() const noexcept { return _cond; }
    const Expr *iftrue() const noexcept { return _iftrue; }
    const Expr *iffalse() const noexcept { return _iffalse; }
    virtual const Token *token() const { return _cond->token(); }

  private:
    const Expr *_cond, *_iftrue, *_iffalse;
};

class BinaryExpr : public Expr {
  public:
    BinaryExpr(ExprKind kind, const Token *token, Expr *lhs, Expr *rhs)
        : Expr(kind), _token(token), _lhs(lhs), _rhs(rhs) {}
    Expr *lhs() const noexcept { return _lhs; }
    Expr *rhs() const noexcept { return _rhs; }
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept;
    virtual const Token *token() const { return _token; }

  private:
    const Token *_token;
    Expr *_lhs, *_rhs;
};

class PostfixExpr : public Expr {
  public:
    PostfixExpr(ExprKind kind) : Expr(kind) {}
};
class SubscriptExpr : public PostfixExpr {
  public:
    SubscriptExpr() : PostfixExpr(EXPR_SUBSCRIPT) {}
};
class FuncCallExpr : public PostfixExpr {
  public:
    explicit FuncCallExpr(Expr *left, std::vector<Expr *> args)
        : PostfixExpr(EXPR_FUNC_CALL), _left(left), _args(args) {}

    void accept(Visitor *);
    Expr *left() const noexcept { return _left; }
    std::vector<Expr *> args() const noexcept { return _args; }
    virtual const Type *type() const noexcept;
    virtual const Token *token() const { return _left->token(); }

  private:
    Expr *_left;
    std::vector<Expr *> _args;
};

// primary-expression:
//   identifier
//   constant
//   string-literal
//   ( expression )
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
class FloatConst : public PrimaryExpr {
  public:
    //     explicit FloatConst(double val, bool isfloat)
    //         : PrimaryExpr(EXPR_FLOAT), _value(val),
    //           _type(isfloat ? &BuiltinType::Float : &BuiltinType::Double) {}
    explicit FloatConst(const Token *token) : PrimaryExpr(EXPR_FLOAT), _token(token) {
        char *end;
        double dval = std::strtod(token->get_lexeme(), &end);
        _value      = dval;
        _type       = *end == 'f' ? &BuiltinType::Float : &BuiltinType::Double;
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
class IntConst : public PrimaryExpr {
  public:
    // explicit IntConst(int val) : PrimaryExpr(EXPR_INT), _value(val), _type(&BuiltinType::Int) {}
    // explicit IntConst(unsigned int val)
    //     : PrimaryExpr(EXPR_INT), _value(val), _type(&BuiltinType::UInt) {}
    // explicit IntConst(long val) : PrimaryExpr(EXPR_INT), _value(val), _type(&BuiltinType::Long)
    // {} explicit IntConst(unsigned long val)
    //     : PrimaryExpr(EXPR_INT), _value(val), _type(&BuiltinType::ULong) {}
    // explicit IntConst(long long val)
    //     : PrimaryExpr(EXPR_INT), _value(val), _type(&BuiltinType::Long) {}
    // explicit IntConst(unsigned long long val)
    //     : PrimaryExpr(EXPR_INT), _value(val), _type(&BuiltinType::ULong) {}
    explicit IntConst(const Token *token);
    explicit IntConst(const Token *token, int ival)
        : PrimaryExpr(EXPR_INT), _value(ival), _token(token), _type(&BuiltinType::Int) {}
    virtual void accept(Visitor *);
    virtual const Type *type() const noexcept { return _type; }
    virtual bool is_int_const() const noexcept { return true; }
    const unsigned long value() const noexcept { return _value; }
    void neg() { _value = -_value; }

  private:
    std::uint64_t _value;
    const Token *_token;
    const Type *_type;
};

class StringLiteral : public PrimaryExpr {
  public:
    explicit StringLiteral(const Token *literal) : PrimaryExpr(EXPR_STR), _token(literal){};
    const char *get_value() const noexcept { return _token->get_lexeme(); }
    virtual void accept(Visitor *) {}
    // virtual const Type *type() const noexcept;

  private:
    const Token *_token;
};

class UnaryExpr : public Expr {
  protected:
    explicit UnaryExpr() : Expr(EXPR_UNARY) {}

  public:
    virtual bool is_unary() const noexcept { return true; }
    virtual int unary_type() const noexcept = 0;
    virtual void accept(Visitor *);
};

template <int Type> class TypedUnaryExpr : public UnaryExpr {
  public:
    explicit TypedUnaryExpr(Expr *oprand) : UnaryExpr(), _type(Type) {}
    virtual int unary_type() const noexcept { return Type; };

  private:
    int _type;
};

class CastExpr : public Expr {
  public:
    explicit CastExpr(const Type *to, Expr *expr) : Expr(EXPR_CAST), _to(to), _expr(expr) {}
    const Type *to_type() const noexcept { return _to; }
    Expr *from_expr() const noexcept { return _expr; }
    virtual void accept(Visitor *);

  private:
    const Type *_to;
    Expr *_expr;
};

class Assignment : public Expr {
  public:
    explicit Assignment(Expr *lhs, Expr *rhs) : Expr(EXPR_ASSIGNMENT), _lhs(lhs), _rhs(rhs) {
        mqassert(lhs->kind() <= EXPR_UNARY,
                 "the left of an assignment expression must be an unary expression.");
    }
    explicit Assignment(Expr *lhs, Expr *rhs, int type)
        : Expr(EXPR_ASSIGNMENT), _lhs(lhs), _rhs(rhs), _type(type) {
        mqassert(lhs->kind() <= EXPR_UNARY,
                 "the left of an assignment expression must be an unary expression.");
    }
    Expr *lhs() const noexcept { return _lhs; }
    Expr *rhs() const noexcept { return _rhs; }
    int assign_type() const noexcept { return _type; }

  private:
    Expr *_lhs, *_rhs;
    int _type = TK_ASSIGN;
};

// implicit conversion expression
class ConvExpr : public Expr {
  public:
    explicit ConvExpr(const Type *type, Expr *expr) : Expr(EXPR_CONV), _type(type), _expr(expr) {}
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
    virtual bool is_init_declarator() const noexcept { return false; }
};

class Stmt : public BlockItem {};

// labeled
class Labeled : public Stmt {
  public:
    explicit Labeled(const char *label, Stmt *stmt) : _label(label), _stmt(stmt) {}
    explicit Labeled(const std::string label, Stmt *stmt) : _label(label), _stmt(stmt) {}
    const std::string &label() const noexcept { return _label; }
    Stmt *stmt() const noexcept { return _stmt; }

  private:
    const std::string _label;
    Stmt *_stmt;
};

// class Case : public Labeled {
//   public:
//     explicit Case(const std::string label, IntConst *expr, Stmt *stmt)
//         : Labeled(label, stmt), _expr(expr) {}
//     const std::string label() const noexcept { return _label; }
//     IntConst *expr() const noexcept { return _expr; }
//
//   private:
//     const std::string _label;
//     IntConst *_expr;
// };
// class Default : public Stmt {
//   public:
//     explicit Default(Stmt *stmt) : _stmt(stmt) {}
//     Stmt *_stmt;
// };
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
    explicit For(Stmt *init, Expr *cond, Expr *acc, Stmt *body)
        : While(cond, body), _init(init), _accumulator(acc) {}
    Stmt *init() const noexcept { return _init; }
    Expr *accumulator() const noexcept { return _accumulator; }
    virtual void accept(Visitor *v);

  private:
    Stmt *_init;
    Expr *_accumulator;
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

  private:
    const Token *_func_name;
    const FuncType *_singnature;
    Block *_body;
};

// translation unit ::= external-declaration +
class TransUnit {
  public:
    explicit TransUnit(std::list<ExtDecl *> ext_decls) : _ext_decls(ext_decls){};
    std::list<ExtDecl *> get_ext_decls() const { return _ext_decls; }

  private:
    std::list<ExtDecl *> _ext_decls;
};
#endif

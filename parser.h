#ifndef _MQCC_PARSER_H__
#define _MQCC_PARSER_H__
#include "type.h"
#include "typedefs.h"

#include <memory>
#include <stack>
#include <string_view>
#include <vector>

/// forward declaration

/// ast.h
class TransUnit;
class FuncDef;
class Declarator;
class Initializer;
class InitDeclarator;
class Decl;
class Declarattion;
class Expr;
class BlockItem;
class Block;
class Stmt;

/// scanner.h
class Scanner;

/// scope.h
class Scope;

/// type.h
struct Attribute;
class Type;
class HalfType;

/// token.h
class Token;

/// forward declaration end

class Parser {
  public:
    explicit Parser(Scanner *scanner);
    TransUnit *parse();

  private:
    Scanner *_scanner;
    Scope *_scope;
    const FuncType *_cft = nullptr; // current function type, nullptr while not parsing func def
    struct SwitchStatus {
        bool hasdefault = false;
        std::vector<Expr *> cases{};
    } *_switch = nullptr;
    std::stack<const Token *> _lookups;
    std::stack<const Token *> _consumed;
    void process_storage_class(Attribute *base);
    const Type *parse_typedef(const Type *base);
    Type *parse_struct_decl();
    Type *parse_union_decl();
    Type *parse_struct_union_decl();
    type_counter_t process_builtin(type_counter_t);
    // function definition and declaration both start with
    // these two syntax elements;
    const Type *parse_declaration_specifiers(Attribute *);
    const HalfType *parse_declarator(const Type *);

    // init-declarator needs initializer
    Initializer *parse_initializer(const Type *);
    Block *parse_init_declarators(const Type *);
    InitDeclarator *parse_init_declarator(const Type *);

    // direct declarator needs pointer
    const Type *parse_pointer(const Type *base);
    // declarator needs direct declarator
    const HalfType *parse_direct_declarator(const Type *);
    std::vector<const HalfType *> parse_parameters();
    const HalfType *parse_parameter();
    Type *parse_array_or_func_decl(const Type *base, const char *name);

    Block *parse_declaration();
    Decl *parse_decl(type_counter_t, Declarator *);

    Expr *parse_primary();
    Expr *process_const();
    Expr *parse_ident();
    Expr *parse_constant();
    Expr *parse_generic();
    Expr *parse_postfix();
    Expr *parse_unary();
    Expr *parse_cast();

    Expr *parse_binary(int);

    Expr *parse_conditional();
    Expr *parse_assignment();

    // argument-expression-list:
    //   assignment-expression
    //   argument-expression-list , assignment-expression
    std::vector<Expr *> parse_args();
    // statement needs expression
    Expr *parse_expr();
    // block needs statement
    Stmt *parse_stmt();
    // labeled
    Stmt *parse_labeled();
    // jump statements
    Stmt *parse_jump();
    // selection
    Stmt *parse_if();
    Stmt *parse_switch();
    // iteration
    Stmt *parse_while();
    Stmt *parse_do_while();
    Stmt *parse_for();
    // function definition needs compound statements aka. block
    Block *parse_block(bool);
    FuncDef *parse_func_def(const HalfType *);
    FuncDef *parse_func_def(type_counter_t, Declarator *);
    Block *parse_global_variable(const Type *);

    // parse type name
    const Type *parse_type_name();

    // check name
    void check_name(const InitDeclarator *, Attribute *);

    // test if next token could lead a declaration
    bool maybe_decl();

    // base functions
    void next();
    const Token *peek();
    bool test(int expected);
    void expect(int expected);
    bool try_next(int expected);
    void unget();
};

#endif

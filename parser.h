#ifndef _MQCC_PARSER_H__
#define _MQCC_PARSER_H__
#include "ast.h"
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
    const Scope *scope() const noexcept { return _scope; }

  private:
    Scanner *_scanner;
    Scope *_scope;
    FuncDef *_cfd = nullptr; // current function type, nullptr while not parsing func def
    struct SwitchStatus {
        CaseDefaultList labels;
    } *_switch = nullptr;
    // std::stack<const Token *> _lookups;
    // std::stack<const Token *> _consumed;
    std::vector<const Token *> _tokens;
    unsigned _index = 0;
    void process_storage_class(Attribute *base);
    void parse_typedef(const Type *base, Attribute *attr);
    Type *parse_struct_decl();
    Type *parse_union_decl();
    const Type *parse_struct_union_decl();
    const Type *parse_enum_decl();
    std::list<Member *> parse_member_decl(Attribute *attr, const Type *base);
    type_counter_t process_builtin(type_counter_t);
    // function definition and declaration both start with
    // these two syntax elements;
    const Type *parse_declaration_specifiers(Attribute *);
    const HalfType *parse_declarator(const Type *);

    // init-declarator needs initializer
    Initializer *parse_initializer(const Type *);
    Block *parse_init_declarators(const Type *, Attribute *);
    InitDeclarator *parse_init_declarator(const Type *);

    // direct declarator needs pointer
    const Type *parse_pointer(const Type *base);
    // declarator needs direct declarator
    const HalfType *parse_direct_declarator(const Type *);
    void fix_type(Type *fake_type, const Type *fake_base, const Type *real_base);
    std::vector<const HalfType *> parse_parameters();
    const HalfType *parse_parameter();
    Expr *parse_array_dimen();
    const Type *parse_func_or_array_decl(const Type *base);

    Block *parse_declaration();
    Decl *parse_decl(type_counter_t, Declarator *);

    Expr *parse_primary();
    Expr *process_const();
    Identifier *parse_ident();
    Expr *parse_constant();
    Expr *parse_generic();
    Expr *parse_postfix();
    Expr *parse_unary();
    Expr *parse_cast();

    Expr *parse_binary(int);
    Expr *parse_mult();
    Expr *parse_add();
    Expr *parse_shift();
    Expr *parse_relational();
    Expr *parse_equality();
    Expr *parse_bit_and();
    Expr *parse_xor();
    Expr *parse_bit_or();
    Expr *parse_log_and();
    Expr *parse_log_or();

    Expr *parse_conditional();
    Expr *parse_assignment();
    Expr *parse_comma();

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
    Block *parse_block();
    Block *parse_func_body();
    FuncDef *parse_func_def(const HalfType *);
    std::list<InitDeclarator *> parse_global_variables(const HalfType *, Attribute *);

    // parse type name
    const Type *parse_type_name();

    // helper functions
    Expr *complex_assignment(const Token *, Expr *, Expr *);
    Expr *make_post(const Token*, Expr*);
    void create_local_variable(const Token*, const Type*);

    // check name
    void check_init_declarator(const InitDeclarator *, Attribute *);
    void check_func_declaration(const HalfType *, Attribute *);
    void check_identifier(const Token *);
    void check_assignment(Assignment *);

    // test if next token could lead a declaration
    bool maybe_decl();

    // base functions
    void next();
    const Token *peek();
    bool test(int expected);
    void expect(int expected);
    bool try_next(int expected);
    void unget();
    unsigned int index() { return _index; }
    void set_index(unsigned int index) { _index = index; }
};

#endif

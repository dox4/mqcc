#ifndef _MQCC_GENERATOR_H__
#define _MQCC_GENERATOR_H__

#include "ast.h"
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

/// forward declaration

// ast.h
// class AstNode;
// class TransUnit;
// class FuncDef;
// class FuncCallExpr;
//
// class ReturnStmt;

// generator.cc
class RoData;

/// forward declaration end

/// class declarations

class Visitor {
  public:
    Visitor() = default;
    virtual ~Visitor();
    // visit functions
    virtual void visit(AstNode *);
    // expressions
    virtual void visit_int_const(IntConst *)           = 0;
    virtual void visit_float_const(FloatConst *)       = 0;
    virtual void visit_string_literal(StringLiteral *) = 0;
    virtual void visit_binary(BinaryExpr *)            = 0;
    virtual void visit_func_call(FuncCallExpr *fd)     = 0;
    virtual void visit_assignment(Assignment *)        = 0;
    virtual void visit_identifier(Identifier *)        = 0;
    virtual void visit_conv(ConvExpr *)                = 0;
    // statements
    virtual void visit_func_def(FuncDef *fd)             = 0;
    virtual void visit_return(ReturnStmt *rs)            = 0;
    virtual void visit_block(Block *)                    = 0;
    virtual void visit_init_declarator(InitDeclarator *) = 0;
    virtual void visit_initializer(Initializer *)        = 0;
};

class Generator : public Visitor {
  protected:
    TransUnit *_unit;
    struct {
        FuncDef *def;
        std::string ret_label;
    } _current_fn;
    const Scope *_current_scope;

    std::stringstream _buffer;
    int _offset = 0;
    std::vector<const RoData *> _rodata;

  public:
    Generator(TransUnit *unit);
    virtual ~Generator();
    void gen();
    std::string code() const;
    // expressions
    virtual void visit_int_const(IntConst *);
    virtual void visit_float_const(FloatConst *);
    virtual void visit_string_literal(StringLiteral *);
    virtual void visit_binary(BinaryExpr *);
    virtual void visit_func_call(FuncCallExpr *fd);
    virtual void visit_assignment(Assignment *);
    virtual void visit_identifier(Identifier *);
    virtual void visit_conv(ConvExpr *);
    // statements
    virtual void visit_func_def(FuncDef *fd);
    virtual void visit_return(ReturnStmt *rs);
    virtual void visit_block(Block *);
    virtual void visit_init_declarator(InitDeclarator *);
    virtual void visit_initializer(Initializer *);

  private:
    std::ostream &indent() { return _buffer << "    "; }
    void emit(const std::string_view &inst) { indent() << inst << '\n'; }
    void emit(const std::string_view &inst, const std::string_view &oprand) {
        indent() << inst << " " << oprand << '\n';
    }

    void emit(const std::string_view &inst, const unsigned long val, const std::string_view &dest) {
        indent() << inst << " $" << std::to_string(val) << ", " << dest << '\n';
    }
    void emit(const std::string_view &inst, const std::string_view &src,
              const std::string_view &dest) {
        indent() << inst << " " << src << ", " << dest << '\n';
    }
    void emit_noindent(const std::string &label) { _buffer << label << '\n'; }
    void emit_label(const std::string_view &label) { _buffer << label << ":\n"; }

    // implement in .cc
    void push(const std::string_view &reg, int, bool);
    void pop(const std::string_view &reg, int, bool);
    void restore();

    void emit_div(const Type *type);
    void emit_cmp(const char *setcc, int width);
    void emit_cvt(const Type *from, const Type *to);
    void emit_cvt_to_int(const Type *from, const Type *to);
    void emit_promot_int(const Type *from);
    void emit_cvt_to_float(const Type *from, const Type *to);
    void emit_data();
    void emit_text();

    const std::string add_rodata(const std::string str);
    const std::string add_rodata(const double dval);
    const std::string add_rodata(const float fval);
};

// class Address;
// class LValueVisitor : Visitor {
//   private:
//     Address *_addr;
//
//   public:
//     virtual void visit_identifier(Identifier *);
//     const std::string code() const;
// };

#endif

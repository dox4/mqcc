#ifndef _MQCC_GENERATOR_H__
#define _MQCC_GENERATOR_H__

#include "ast.h"
#include <cstdint>
#include <iostream>
#include <optional>
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
    virtual void visit_cast(CastExpr *)                = 0;
    virtual void visit_unary(UnaryExpr *)              = 0;
    // statements
    virtual void visit_func_def(FuncDef *fd) = 0;
    // labeled
    virtual void visit_labeled(Labeled *) = 0;
    // virtual void visit_case(Case *)       = 0;
    // virtual void visit_default(Default *) = 0;
    // selection
    virtual void visit_ifelse(IfElse *) = 0;
    virtual void visit_switch(Switch *) = 0;
    // iteration
    virtual void visit_while(While *)      = 0;
    virtual void visit_do_while(DoWhile *) = 0;
    virtual void visit_for(For *)          = 0;
    // jump
    virtual void visit_goto(Goto *)                      = 0;
    virtual void visit_continue(Continue *)              = 0;
    virtual void visit_break(Break *)                    = 0;
    virtual void visit_return(Return *)                  = 0;
    virtual void visit_block(Block *)                    = 0;
    virtual void visit_init_declarator(InitDeclarator *) = 0;
    virtual void visit_initializer(Initializer *)        = 0;
};

class Generator;

struct ObjAddr {
    // addr = offset (base, index, scale)
    // addr = offset + (base + index * scale)
    // offset can be number or label
    static const ObjAddr direct(std::uint64_t addr) {
        return ObjAddr{std::to_string(addr), "", "", 0};
    }
    static const ObjAddr indirect(const std::string_view &base) { return ObjAddr{"", base, "", 0}; }
    static const ObjAddr index_addr(const std::string &label, const std::string_view &index,
                                    int scale) {
        return ObjAddr{label, "", index, scale};
    }
    static const ObjAddr base_addr(int offset, const std::string_view &base) {
        return ObjAddr{std::to_string(offset), base, "", 0};
    }
    static const ObjAddr base_addr(const std::string &label, const std::string_view &base) {
        return ObjAddr{label, base, "", 0};
    }
    const std::string offset;
    const std::string_view &base;
    const std::string_view &index;
    const int scale;
    const std::string to_string() const;
};

class LValueGenerator : public Visitor {
  public:
    explicit LValueGenerator(Generator *g) : _gtr(g) {}

    // expressions
    virtual void visit_int_const(IntConst *) {}
    virtual void visit_float_const(FloatConst *) {}
    virtual void visit_string_literal(StringLiteral *) {}
    virtual void visit_binary(BinaryExpr *) {}
    virtual void visit_func_call(FuncCallExpr *fd) {}
    virtual void visit_assignment(Assignment *) {}
    virtual void visit_identifier(Identifier *);
    virtual void visit_conv(ConvExpr *) {}
    virtual void visit_cast(CastExpr *) {}
    virtual void visit_unary(UnaryExpr *) {}
    // statements
    virtual void visit_func_def(FuncDef *fd) {}
    // labeled
    virtual void visit_labeled(Labeled *) {}
    // selection
    virtual void visit_ifelse(IfElse *) {}
    virtual void visit_switch(Switch *) {}
    // iteration
    virtual void visit_while(While *) {}
    virtual void visit_do_while(DoWhile *) {}
    virtual void visit_for(For *) {}
    // jump
    virtual void visit_goto(Goto *) {}
    virtual void visit_continue(Continue *) {}
    virtual void visit_break(Break *) {}
    virtual void visit_return(Return *) {}
    virtual void visit_block(Block *) {}
    virtual void visit_init_declarator(InitDeclarator *) {}
    virtual void visit_initializer(Initializer *) {}

  private:
    Generator *_gtr;
    std::optional<const std::string> _addr = std::nullopt;

  public:
    const std::string &addr() const { return _addr.value(); }
};

class Generator : public Visitor {
  protected:
    TransUnit *_unit;
    struct {
        FuncDef *def;
        std::string ret_label;
    } _current_fn;
    struct BreakTo {
        const std::string &label;
    } *_break_to = nullptr;
    struct ContinueTo {
        const std::string &label;
    } *_cont_to = nullptr;
    const Scope *_current_scope;

    std::stringstream _buffer;
    int _offset = 0;
    std::vector<const RoData *> _rodata;
    LValueGenerator *_lvgtr = new LValueGenerator(this);

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
    virtual void visit_cast(CastExpr *);
    virtual void visit_unary(UnaryExpr *);
    // statements
    virtual void visit_func_def(FuncDef *fd);
    // labeled
    virtual void visit_labeled(Labeled *);
    // virtual void visit_case(Case *);
    // virtual void visit_default(Default *);
    // selection
    virtual void visit_ifelse(IfElse *);
    virtual void visit_switch(Switch *);
    // iteration
    virtual void visit_while(While *);
    virtual void visit_do_while(DoWhile *);
    virtual void visit_for(For *);
    // jump
    virtual void visit_goto(Goto *);
    virtual void visit_continue(Continue *);
    virtual void visit_break(Break *);
    virtual void visit_return(Return *rs);
    virtual void visit_block(Block *);
    virtual void visit_init_declarator(InitDeclarator *);
    virtual void visit_initializer(Initializer *);

  private:
    friend class LValueGenerator;
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

    void emit_logic(BinaryExpr *, bool isand);
    void emit_iset0(const std::string_view &);
    void emit_fset0(const std::string_view &);
    void emit_ibin(BinaryExpr *);
    void emit_fbin(BinaryExpr *);
    void emit_fcmp(const char *setcc, int width);
    void emit_idiv(const Type *type);
    void emit_fdiv(const Type *type);
    void emit_icmp(const char *setcc, int width);
    void emit_cvt(const Type *from, const Type *to);
    void emit_cvt_to_int(const Type *from, const Type *to);
    void emit_promot_int(const Type *from);
    void emit_cvt_to_float(const Type *from, const Type *to);
    void emit_data();
    void emit_text();

    const std::string add_rodata(const std::string str);
    const std::string add_rodata(const double dval);
    const std::string add_rodata(const float fval);

    inline void backup_loop(ContinueTo **ib, BreakTo **bt) {
        *ib = _cont_to;
        *bt = _break_to;
    }
    inline void restore_loop(ContinueTo *ib, BreakTo *bt) {
        _cont_to  = ib;
        _break_to = bt;
    }
};

#endif

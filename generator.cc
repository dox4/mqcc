#include "generator.h"
#include "ast.h"
#include "error.h"
#include "token.h"
#include "type.h"
#include <cstddef>
#include <cstdint>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
using namespace std;

/// static variables

// clang-format off

// registers
//                          |                                 bits                               |
//                          |63              31                15                 7             0|
static constexpr string_view rax = "%rax"sv, eax  =  "%eax"sv, ax   =   "%ax"sv,  al  =   "%al"sv;  // acc, return value
static constexpr string_view rbx = "%rbx"sv, ebx  =  "%ebx"sv, bx   =   "%bx"sv,  bl  =   "%bl"sv;  // base, callee saved, 
static constexpr string_view rcx = "%rcx"sv, ecx  =  "%ecx"sv, cx   =   "%cx"sv,  cl  =   "%cl"sv;  // counter, 4th arg
static constexpr string_view rdx = "%rdx"sv, edx  =  "%edx"sv, dx   =   "%dx"sv,  dl  =   "%dl"sv;  // io, 3th arg
static constexpr string_view rsi = "%rsi"sv, esi  =  "%esi"sv, si   =   "%si"sv,  sil =  "%sil"sv;  // src idx, with rds, 2nd arg
static constexpr string_view rdi = "%rdi"sv, edi  =  "%edi"sv, di   =   "%di"sv,  dil =  "%dil"sv;  // dest idx, with res, 1st arg
static constexpr string_view rbp = "%rbp"sv, ebp  =  "%ebp"sv, bp   =   "%bp"sv,  bpl =  "%bpl"sv;  // base ptr, with rss, callee saved
static constexpr string_view rsp = "%rsp"sv, esp  =  "%esp"sv, sp   =   "%sp"sv,  spl =  "%spl"sv;  // stack ptr, with rss,
static constexpr string_view r8  =  "%r8"sv, r8d  =  "%r8d"sv, r8w  =  "%r8w"sv,  r8b =  "%r8b"sv;  // 5th arg
static constexpr string_view r9  =  "%r9"sv, r9d  =  "%r9d"sv, r9w  =  "%r9w"sv,  r9b =  "%r9b"sv;  // 6th arg
static constexpr string_view r10 = "%r10"sv, r10d = "%r10d"sv, r10w = "%r10w"sv, r10b = "%r10b"sv;  // caller saved
static constexpr string_view r11 = "%r11"sv, r11d = "%r11d"sv, r11w = "%r11w"sv, r11b = "%r11b"sv;  // caller saved
static constexpr string_view r12 = "%r12"sv, r12d = "%r12d"sv, r12w = "%r12w"sv, r12b = "%r12b"sv;  // callee saved
static constexpr string_view r13 = "%r13"sv, r13d = "%r13d"sv, r13w = "%r13w"sv, r13b = "%r13b"sv;  // callee saved
static constexpr string_view r14 = "%r14"sv, r14d = "%r14d"sv, r14w = "%r14w"sv, r14b = "%r14b"sv;  // callee saved
static constexpr string_view r15 = "%r15"sv, r15d = "%r15d"sv, r15w = "%r15w"sv, r15b = "%r15b"sv;  // callee saved

static const unordered_map<string_view, int> reg_bytes{
    { rax, 8 }, { eax , 4 }, { ax  , 2 }, { al  , 1 },
    { rbx, 8 }, { ebx , 4 }, { bx  , 2 }, { bl  , 1 },
    { rcx, 8 }, { ecx , 4 }, { cx  , 2 }, { cl  , 1 },
    { rdx, 8 }, { edx , 4 }, { dx  , 2 }, { dl  , 1 },
    { rsi, 8 }, { esi , 4 }, { si  , 2 }, { sil , 1 },
    { rdi, 8 }, { edi , 4 }, { di  , 2 }, { dil , 1 },
    { rbp, 8 }, { ebp , 4 }, { bp  , 2 }, { bpl , 1 },
    { rsp, 8 }, { esp , 4 }, { sp  , 2 }, { spl , 1 },
    { r8 , 8 }, { r8d , 4 }, { r8w , 2 }, { r8b , 1 },
    { r9 , 8 }, { r9d , 4 }, { r9w , 2 }, { r9b , 1 },
    { r10, 8 }, { r10d, 4 }, { r10w, 2 }, { r10b, 1 },
    { r11, 8 }, { r11d, 4 }, { r11w, 2 }, { r11b, 1 },
    { r12, 8 }, { r12d, 4 }, { r12w, 2 }, { r12b, 1 },
    { r13, 8 }, { r13d, 4 }, { r13w, 2 }, { r13b, 1 },
    { r14, 8 }, { r14d, 4 }, { r14w, 2 }, { r14b, 1 },
    { r15, 8 }, { r15d, 4 }, { r15w, 2 }, { r15b, 1 },
};

#define isxmm(reg) (reg[1] == 'x')

// 128-bit SSE registers (SSE: Streaming SIMD Extensions)
static constexpr string_view xmm[] = {
    // passing arguments
    "%xmm0", "%xmm1", "%xmm2",  "%xmm3",  "%xmm4",  "%xmm5",  "%xmm6",  "%xmm7",
    // temporary registers
    "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15",
};


// registers to passing arguments
// first to 6th argument
const string_view reg_args[][8] = {
    /* 8 - width       0    1   2   3   4    5   6    7 */
    /* 1st arg   */  { rdi, "", "", "", edi, "", di,  dil, },
    /* 2nd arg   */  { rsi, "", "", "", esi, "", si,  sil, },
    /* 3rd arg   */  { rdx, "", "", "", edx, "", dx,  dl,  },
    /* 4th arg   */  { rcx, "", "", "", ecx, "", cx,  cl,  },
    /* 5th arg   */  { r8,  "", "", "", r8d, "", r8w, r8b, },
    /* 6th arg   */  { r9,  "", "", "", r9d, "", r9w, r9b, },
};

// integer promotion instructions
constexpr string_view to_long[][2] { 
    // zero-extend byte to long  sign-extend byte to long
    {  "movzbl",                 "movsbl" },
    // zero-extend word to long  sign-extend word to long
    {  "movzwl",                 "movswl" }
};

constexpr string_view to_quad[][2] {
    // zero-extend byte to quad  sign-extend byte to quad
    {  "movzbq",                 "movsbq"},
    // zero-extend word to quad  sign-extend word to quad
    {  "movzwq",                 "movswq"},
};

// ConVerT with Truncation Scalar Single/Double-precision floating-point value to Signed Integer
constexpr string_view f2i[][2] {
    // scalar single to doubleword,  scalar double to doubleword
    {  "cvttss2si",                  "cvttsd2si"  },
    // scalar single to quadword,  scalar double to quadword
    {  "cvttss2siq",                 "cvttsd2siq" },
};

constexpr string_view i2f[][2] {
    // covert doubleword integer to scalar single, to scalar double
    {  "cvtsi2ss",                                 "cvtsi2sd"   },
    // covert quadword integer to scalar single,   to scalar double
    {  "cvtsi2ssq",                                "cvtsi2sdq"  },
};
// clang-format on

class LabelMaker {
    const char *_flag;
    int _cnt = 0;

  public:
    explicit LabelMaker(const char *flag) : _flag(flag) {}
    const string operator()() {
        stringstream s;
        s << ".L" << _flag << _cnt++;
        return s.str();
    }
};

static LabelMaker const_label = LabelMaker("C");
// floating number as unsigned integer number
static LabelMaker float_label    = LabelMaker("FAU");
static LabelMaker func_end_label = LabelMaker("FE");
static LabelMaker branch_label   = LabelMaker("B");
/// static variables end

/// static funtions

// dest register for integer operation
static const string_view &idest(int width) {
    switch (width) {
    case 1:
        return al;
    case 2:
        return ax;
    case 4:
        return eax;
    case 8:
        return rax;
    default:
        unreachable();
    }
    // no compiler warning
    return rax;
}

// source register for integer operation
static const string_view &isrc(int width) {
    switch (width) {
    case 1:
        return r11b;
    case 2:
        return r11w;
    case 4:
        return r11d;
    case 8:
        return r11;
    default:
        unreachable();
    }
    // no compiler warning
    return r11;
}

// dest register for floating number
#define fdest() (xmm[0])
// source register for floating number
#define fsrc() (xmm[8])

static const string_view rdest(int width, bool isfloat) { return isfloat ? fdest() : idest(width); }

static const string_view rsrc(int width, bool isfloat) { return isfloat ? fsrc() : isrc(width); }

static const string addr(const string_view &reg) {
    stringstream s;
    s << '(' << reg << ')';
    return s.str();
}

static const string onto_stack(int offset) {
    stringstream s;
    s << offset << '(' << rbp << ')';
    return s.str();
}

static const string simple_addr(const Object *obj) {
    if (obj->offset() < 0) {
        return onto_stack(obj->offset());
    }
    return obj->type()->is_float() ? string(xmm[obj->offset()])
                                   : string(reg_args[obj->offset()][8 - obj->type()->size()]);
}

static const string iinst(const string &ins, int width) {
    switch (width) {
    case 1:
        return ins + "b";
    case 2:
        return ins + "w";
    case 4:
        return ins + "l";
    case 8:
        return ins + "q";
    }
    unreachable();
    return "";
}
// instructions for integer
static const string iinst(const string &ins, int width, bool is_singed) {
    return is_singed ? iinst("i" + ins, width) : iinst(ins, width);
}
// instructions for floating number
static const string finst(const string &inst, int width) {
    return width == 4 ? inst + "ss" : inst + "sd";
}

static const string mov(int width) { return iinst("mov", width); }
static const string fmov(int width) { return finst("mov", width); }

static const string_view dreg(int width) {
    switch (width) {
    case 1:
        return dl;
    case 2:
        return dx;
    case 4:
        return edx;
    case 8:
        return rdx;
    }
    unreachable();
    return "";
}

/// static functions end

// Visitor

void Visitor::visit(AstNode *node) { node->accept(this); }
Visitor::~Visitor() {}

// Visitor end

// RoData
enum Alignment {
    Alignment1 = 1,
    Alignment2 = 2,
    Alignment4 = 4,
    Alignment8 = 8,
};

class RoData {
  public:
    explicit RoData(const string label, Alignment align, const string &type)
        : _label(label), _align(std::to_string(align)), _type(type) {}
    explicit RoData(const string label, Alignment align)
        : _label(label), _align(std::to_string(align)), _type(align_type(align)) {}

    const string &label() const noexcept { return _label; }
    const string &align() const noexcept { return _align; }
    const string &type() const noexcept { return _type; }
    virtual const string data() const noexcept = 0;

  private:
    const string _label, _align, _type;
    static const string align_type(Alignment align) {
        switch (align) {
        case Alignment1:
            return ".byte";
        case Alignment2:
            return ".word";
        case Alignment4:
            return ".long";
        case Alignment8:
            return ".quad";
        }
        error("invalied alignment: %d", align);
        return "";
    }
};

class StrRoData : public RoData {
  public:
    explicit StrRoData(const string &label, const string val)
        : RoData(label, Alignment1, ".string"), _str("\"" + val + "\"") {}
    virtual const string data() const noexcept { return _str; }

  private:
    const string _label, _str;
};

template <typename UintType> class NumRoData : public RoData {
  public:
    explicit NumRoData(const string &label, const UintType value)
        : RoData(label, static_cast<Alignment>(sizeof(UintType))), _value(value) {}

    virtual const string data() const noexcept { return std::to_string(_value); }

  private:
    const UintType _value;
};

// RoData end

/// Generator

Generator::Generator(TransUnit *unit) : _unit(unit) {}

Generator::~Generator() {}

void Generator::push(const std::string_view &reg, int width, bool isfloat) {
    if (isfloat) {
        _offset -= width;
        emit(fmov(width), reg, onto_stack(_offset));
        return;
    }
    _offset -= width;
    emit(mov(width), reg, onto_stack(_offset));
}
void Generator::pop(const std::string_view &reg, int width, bool isfloat) {
    if (isfloat) {
        emit(fmov(width), onto_stack(_offset), reg);
        _offset += width;
        return;
    }
    emit(mov(width), onto_stack(_offset), reg);
    _offset += width;
}
void Generator::restore() {}

void Generator::emit_div(const Type *type) {
    if (type->is_float()) {
        // TODO float division
    } else {
        auto width    = type->size();
        int is_singed = type->is_signed();
        if (width < 4) {
            int is_word = width == 2;
            auto inst   = to_long[is_word][is_singed];
            // left in dest and right in src
            // extend them to long
            emit(inst, isrc(width), r11d);
            emit(inst, idest(width), idest(4));
            // it seems that gcc emits `idivl` instruction for both
            // signed and unsigned integers if their width is under 4
            // maybe 'cause that `signed int` is big enough to hold
            // all values of integers with width under 4
            emit("cltd");
            emit("idivl", r11d);
            // if (is_singed) {
            //     emit("cltd");
            //     emit(get_inst("idiv", width), r11d);
            // } else {
            //     emit("xor", edx, edx);
            //     emit(get_inst("div", width), r11d);
            // }
        } else {
            // emit instructions for long and quad
            if (is_singed) {
                if (width == 4) {
                    emit("cltd");
                    emit("idivl", r11d);
                } else {
                    emit("cqto");
                    emit("idivq", r11);
                }
            } else {
                emit("xorl", edx, edx);
                emit(iinst("div", width), isrc(width));
            }
        }
    }
}

// when two unsigned operands are compared, the Zero and Carry flags indicate the
// following relations between operands:
// +----------------------+----+----+
// | CMP Results          | ZF | CF |
// +----------------------+----+----+
// | Destination < source | 0  | 1  |
// | Destination > source | 0  | 0  |
// | Destination = source | 1  | 0  |
// +----------------------+----+----+
//
// when two signed operands are compared, the Sign, Zero, and Overflow flags indicate
// the following relations between operands:
// +----------------------+----+----+
// | CMP Results          | Flags   |
// +----------------------+----+----+
// | Destination < source | SF ≠ OF |
// | Destination > source | SF = OF |
// | Destination = source | ZF = 1  |
// +----------------------+----+----+
//  --  Assembly Language for x86 Processors 6th edition 6.2.8
//
// +------------+----------+----------+----------------------------------------------+
// | is signed? | operator | mnemonic | description                                  |
// +------------+----------+----------+----------------------------------------------+
// | unsigned   | >        | SETA     | Set byte if above (CF=0 and ZF=0)            |
// | unsigned   | >=       | SETAE    | Set byte if above or equal (CF=0)            |
// | unsigned   | <        | SETB     | Set byte if below (CF=1)                     |
// | unsigned   | <=       | SETBE    | Set byte if below or equal (CF=1 or ZF=1)    |
// | signed     | >        | SETG     | Set byte if greater (ZF=0 and SF=OF)         |
// | signed     | >=       | SETGE    | Set byte if greater or equal (SF=OF)         |
// | signed     | <        | SETL     | Set byte if less (SF != OF)                  |
// | signed     | <=       | SETLE    | Set byte if less or equal (ZF=1 or SF != OF) |
// | -          | ==       | SETE     | Set byte if equal (ZF=1)                     |
// | -          | !=       | SETNE    | Set byte if not equal (ZF=0)                 |
// +------------+----------+----------+----------------------------------------------+

void Generator::emit_cmp(const char *setcc, int width) {
    emit(iinst("cmp", width), isrc(width), idest(width));
    emit(setcc, al);
    emit("movzbq", al, rax);
}

void Generator::emit_data() {
    if (_rodata.size() > 0) {
        emit(".section", ".rodata");
        for (auto rodata : _rodata) {
            emit(".align", rodata->align());
            emit_label(rodata->label());
            emit(rodata->type(), rodata->data());
        }
    }
}
void Generator::emit_text() {}

void Generator::emit_cvt(const Type *from, const Type *to) {
    debug("from type %s to type %s", from->normalize().c_str(), to->normalize().c_str());
    if (to->is_float()) {
        emit_cvt_to_float(from, to);
    } else {
        emit_cvt_to_int(from, to);
    }
}
void Generator::emit_cvt_to_int(const Type *from, const Type *to) {
    if (from->is_float()) {
        emit(f2i[to->size() == 8][from->size() == 8], fdest(), idest(to->size()));
    } else {
        emit_promot_int(from);
    }
}

// promote integer to quad
void Generator::emit_promot_int(const Type *from) {
    switch (from->kind()) {
    case TY_BOOL:
    case TY_CHAR:
        emit(to_quad[0][from->is_signed()], al, rax);
        return;
    case TY_SHORT:
        emit(to_quad[1][from->is_signed()], ax, rax);
        return;
    case TY_INT:
        if (from->is_signed()) {
            emit("cltq");
        }
        return;
    case TY_LONG:
    case TY_PTR:
        return;
    default:
        error("type %s could not be converted to integer.", from->normalize().c_str());
    }
}

// ps and pd mean `packed single precision` and `packed double precision`
// while ss and sd mead `scalar single` and `scalar double`
// in the instructions below
// 8cc and gcc pick different instructions while generating code for the same code
// e.g.
// assume variable a has type `float` while b has type `double`
// a = b; b = a;
// 8cc:
//     movsd -16(%rbp), %xmm0
//     cvtpd2ps %xmm0, %xmm0
//     movss %xmm0, -8(%rbp)
//     movss -8(%rbp), %xmm0
//     cvtps2pd %xmm0, %xmm0
//     movsd %xmm0, -16(%rbp)
// gcc:
//     cvtsd2ss    -16(%rbp), %xmm1
//     movss   %xmm1, -4(%rbp)
//     cvtss2sd    -4(%rbp), %xmm0
//     movsd   %xmm0, -16(%rbp)
void Generator::emit_cvt_to_float(const Type *from, const Type *to) {
#define isdouble(ty) (ty->size() == 8)
    if (from->is_float()) {
        // convert float to double
        if (!isdouble(from) && isdouble(to)) {
            // emit("cvtss2sd", xmm[0], xmm[0]);
            emit("cvtps2pd", xmm[0], xmm[0]);
        }
        // convert double to float
        else if (isdouble(from) && !isdouble(to)) {
            // emit("cvtsd2ss", xmm[0], xmm[0]);
            emit("cvtpd2ps", xmm[0], xmm[0]);
        }
    } else {
        // from int
        if (from->size() == 4) {
            emit(i2f[0][isdouble(to)], eax, xmm[0]);
        }
        // from long
        else {
            emit(i2f[1][isdouble(to)], rax, xmm[0]);
        }
    }
#undef isdouble
}

const string Generator::add_rodata(const string str) {
    auto str_label = const_label();
    _rodata.push_back(new StrRoData(str_label, str));
    return str_label;
}
const string Generator::add_rodata(const double dval) {
    auto fau_label     = float_label();
    const uint64_t val = *reinterpret_cast<const uint64_t *>(&dval);
    // debug("fau_label: %s, with value: %lf", fau_label.c_str(), dval);
    _rodata.push_back(new NumRoData<uint64_t>(fau_label, val));
    return fau_label;
}
const string Generator::add_rodata(const float fval) {
    auto fau_label     = float_label();
    const uint32_t val = *reinterpret_cast<const uint32_t *>(&fval);
    _rodata.push_back(new NumRoData<uint32_t>(fau_label, val));
    return fau_label;
}

const string static_addr(const string &label, int offset) {
    return offset == 0 ? label + "(%rip)" : label + std::to_string(offset) + "(%rip)";
}

void Generator::gen() {
    for (auto decl : _unit->get_ext_decls()) {
        if (decl != nullptr)
            visit(decl);
    }
    emit_data();
}

void Generator::visit_func_call(FuncCallExpr *fc) {
    // TODO float numbers and arguments with size larger than 8
    // process args
    auto args = fc->args();
    for (int idx = args.size() - 1; idx >= 0; idx--) {
        auto arg = args.at(idx);
        visit(arg);
        auto width   = arg->type()->size();
        auto isfloat = arg->type()->is_float();
        auto reg     = isfloat ? fdest() : idest(width);
        push(reg, width, isfloat);
    }
    for (size_t idx = 0; idx < args.size() && idx < 6; idx++) {
        auto arg     = args.at(idx);
        auto size    = arg->type()->size();
        auto isfloat = arg->type()->is_float();
        auto reg     = isfloat ? xmm[idx] : reg_args[idx][8 - size];
        pop(reg, size, isfloat);
    }
    // process function address
    auto left = fc->left();
    if (left->kind() == EXPR_IDENT) {
        auto ident = static_cast<const Identifier *>(left);
        emit("call", ident->get_value());
    } else {
        visit(left);
        emit("call", rax);
    }
}

void Generator::visit_func_def(FuncDef *fd) {
    // record current function and generate return label
    _current_fn.def       = fd;
    _current_fn.ret_label = func_end_label();

    // emit function info
    emit(".text");
    auto name = fd->func_name();
    emit(".globl", name);
    emit(".type", name, "@function");
    emit_label(name);

    // enter function
    // cache %rbp
    emit("pushq", rbp);
    emit("movq", rsp, rbp);

    // function body
    visit_block(fd->body());

    // return
    emit_label(_current_fn.ret_label);
    emit("popq", rbp);
    emit("retq");
}

void Generator::visit_int_const(IntConst *ic) {
    auto width = ic->type()->size();
    auto r     = idest(width);
    emit(mov(width), ic->value(), r);
}

void Generator::visit_float_const(FloatConst *fc) {
    auto width = fc->type()->size();
    auto r     = fdest();
    auto dval  = fc->value();
    if (width == 4) {
        float fval = dval;
        auto label = add_rodata(fval);
        emit(fmov(width), static_addr(label, 0), r);
    } else {
        auto label = add_rodata(dval);
        emit(fmov(width), static_addr(label, 0), r);
    }
}
void Generator::visit_string_literal(StringLiteral *str) {
    auto label = add_rodata(str->get_value());
    emit("lea", static_addr(label, 0), rax);
}

void Generator::visit_binary(BinaryExpr *e) {
    // type info
    auto type    = e->type();
    auto width   = type->size();
    auto isfloat = type->is_float();
    // generate code for lhs
    visit(e->lhs());
    // cache the result of lhs
    auto dest = idest(width);
    push(dest, width, isfloat);
    // generate code for rhs
    // result of rhs is stored at dest reg
    visit(e->rhs());
    auto src = isrc(width);
    auto m   = isfloat ? fmov(width) : mov(width);
    // mov rhs to src reg and pop lhs to dest reg
    // to keep the oprands order in assembly as the same as in the C code
    // it's necessary for div and sub, but unnecessary for add and mul
    emit(m, dest, src);
    pop(dest, width, isfloat);
    auto is_signed = type->is_signed();
    switch (e->kind()) {
    case EXPR_MUL: { // *
        auto inst = iinst("mul", width, is_signed);
        emit(inst, src);
        return;
    }
    case EXPR_DIV: { // /
        emit_div(type);
        return;
    }
    case EXPR_MOD: { // %
        emit_div(type);
        emit(m, dreg(width), dest);
        return;
    case EXPR_ADD: { // +
        auto inst = iinst("add", width);
        emit(inst, src, dest);
        return;
    }
    case EXPR_SUB: { // -
        auto inst = iinst("sub", width);
        emit(inst, src, dest);
        return;
    }
    case EXPR_BLS:   // <<
    case EXPR_BRS: { // >>
        auto inst = e->kind() == EXPR_BLS ? "shl" : is_signed ? "sar" : "shr";
        emit("movq", r11, rcx);
        emit(iinst(inst, width), cl, idest(width));
        return;
    }
    case EXPR_LESS: // <
        emit_cmp(is_signed ? "setl" : "setb", width);
        return;
    case EXPR_LEQUAL: // <=
        emit_cmp(is_signed ? "setle" : "setbe", width);
        return;
    case EXPR_GREATER: // >
        emit_cmp(is_signed ? "setg" : "seta", width);
        return;
    case EXPR_GEQUAL: // >=
        emit_cmp(is_signed ? "setge" : "setae", width);
        return;
    case EXPR_EQUAL: // ==
        emit_cmp("sete", width);
        return;
    case EXPR_NEQUAL: // !=
        emit_cmp("setne", width);
        return;
    case EXPR_BAND: // &
    case EXPR_BXOR: // ^
    case EXPR_BOR:  // |
    case EXPR_LAND: // &&
    case EXPR_LOR:  // ||
    default:
        error("not a valid binary operator.");
    }
    }
}

void Generator::visit_goto(Goto *) {}
void Generator::visit_continue(Continue *) {}
void Generator::visit_break(Break *) {}
void Generator::visit_return(Return *rs) {
    visit(rs->expr());
    emit("jmp", _current_fn.ret_label);
}

void Generator::visit_assignment(Assignment *assignment) {
    auto rhs     = assignment->rhs();
    auto lhs     = assignment->lhs();
    auto size    = rhs->type()->size();
    auto isfloat = rhs->type()->is_float();
    visit(rhs);
    auto dest = rdest(size, isfloat);
    push(dest, size, isfloat);
    visit(lhs);
    auto src = rsrc(size, isfloat);
    pop(src, size, isfloat);
    emit(mov(size), src, addr(dest));
}
void Generator::visit_block(Block *block) {
    auto bak_scope = _current_scope;
    // debug("visiting block, scope size: %zu", _current_scope->size());
    // debug("obj in scope:\n%s", _current_scope->obj_to_string().c_str());
    _current_scope = block->scope();
    for (auto item : block->items())
        visit(item);
    _current_scope = bak_scope;
}

void Generator::visit_init_declarator(InitDeclarator *id) {
    if (id->is_initialized()) {
        visit(id->initializer());
        auto type  = id->halftype()->type();
        auto token = id->halftype()->token();
        auto name  = token->get_lexeme();
        auto obj   = _current_scope->find_var_in_local(name);
        // deal with floating number
        if (type->is_float()) {
            emit(fmov(type->size()), fdest(), simple_addr(obj));
            return;
        }
        auto m = mov(type->size());
        emit(m, idest(type->size()), simple_addr(obj));
    }
}

void Generator::visit_initializer(Initializer *init) {
    // debug("visit initializer");
    visit(init->assignment());
}

void Generator::visit_identifier(Identifier *ident) {
    auto obj = _current_scope->find_var_in_local(ident->get_value());
    // debug("obj is null? %d", obj == nullptr);
    auto type = obj->type();
    auto size = type->size();
    // debug("obj: { name: '%s', type: '%s', size: %d' }", obj->ident()->get_lexeme(),
    //       type->normalize().c_str(), size);
    if (type->is_float()) {
        emit(fmov(size), simple_addr(obj), fdest());
    } else {
        emit(mov(size), simple_addr(obj), idest(size));
    }
}

void Generator::visit_conv(ConvExpr *conv) {
    visit(conv->expr());
    if (conv->type()->equals_to(conv->expr()->type()))
        return;
    emit_cvt(conv->expr()->type(), conv->type());
}

void Generator::visit_cast(CastExpr *) {}
void Generator::visit_unary(UnaryExpr *ue) {
    // TODO
    switch (ue->unary_type()) {
    case TK_INC:
    case TK_DEC:
    case '*':
    case '+':
    case '-':
    case '!':
    case '&':
    case '~':
    default:
        unreachable();
    }
}

string Generator::code() const { return _buffer.str(); }

// Generator end
// LValueVisitor

// void LValueVisitor::visit_identifier(Identifier *){}
// const string LValueVisitor::code() const {}
// LValueVisitor end

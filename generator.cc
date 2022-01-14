#include "generator.h"
#include "ast.h"
#include "error.h"
#include "token.h"
#include "type.h"
#include <array>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
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

static constexpr string_view rip = "%rip";

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
    // zero-extend long to quad  sign-extend long to quad
    {  "movzx",                  "movsx"},
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
static LabelMaker float_label      = LabelMaker("FAU");
static LabelMaker func_end_label   = LabelMaker("FE");
static LabelMaker branch_label     = LabelMaker("B");
static LabelMaker iter_begin_label = LabelMaker("IB");
static LabelMaker iter_end_label   = LabelMaker("IE");
static LabelMaker test_label       = LabelMaker("T");
static LabelMaker switch_end_label = LabelMaker("SE");

static const string userlabel(const string label) {
    // self defined
    return ".LSD" + label + "0";
}

static const string gvar_label(const string name) { return "GVAR_" + name; }
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

// static const string_view rsrc(int width, bool isfloat) { return isfloat ? fsrc() : isrc(width); }

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

static const string imov(int width) { return iinst("mov", width); }
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

const string align_type(Alignment align) {
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
};

static const string tohex(char c) {
    stringstream ss;
    ss << "\\x"
       << "0123456789abcdef"[c / 16] << "0123456789abcdef"[c % 16];
    return ss.str();
}

class StrRoData : public RoData {
  public:
    explicit StrRoData(const string &label, const string val)
        : RoData(label, Alignment1, ".string"), _str(val) {}
    virtual const string data() const noexcept {
        stringstream ss;
        ss << "\"";
        for (char c : _str) {
            if (c == '"')
                ss << "\\\"";
            else if (isprint(c))
                ss << c;
            else
                ss << tohex(c);
        }
        ss << "\"";
        return ss.str();
    }

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

Generator::Generator(TransUnit *unit, const Scope *scope, const char *file_name)
    : _unit(unit), _current_scope(scope), _src_file_name(file_name) {}

Generator::~Generator() {}

void Generator::push(const std::string_view &reg, int width, bool isfloat) {
    // if (width == 8) {
    //     emit("pushq", reg);
    //     _offset -= 8;
    //     return;
    // }
    if (isfloat) {
        _offset -= width;
        emit(fmov(width), reg, onto_stack(_offset));
        emit("subq", width, rsp);
        return;
    }
    emit("pushq", reg);
    _offset -= 8;

    // _offset -= width;
    // emit(mov(width), reg, onto_stack(_offset));
    // emit("subq", width, rsp);
}
void Generator::pop(const std::string_view &reg, int width, bool isfloat) {
    // if (width == 8) {
    //     emit("pop", reg);
    //     _offset += 8;
    //     return;
    // }
    if (isfloat) {
        emit(fmov(width), onto_stack(_offset), reg);
        _offset += width;
        emit("addq", width, rsp);
        return;
    }
    emit("popq", reg);
    _offset += 8;
    // emit(mov(width), onto_stack(_offset), reg);
    // _offset += width;
    // emit("addq", width, rsp);
}
void Generator::push_addr(const ObjAddr *obj) {
    if (obj->base == r10)
        push(r10, 8, false);
}
void Generator::pop_addr(const ObjAddr *obj) {
    if (obj->base == r10)
        pop(r10, 8, false);
}
void Generator::store_value(ObjAddr *obj, const Type *type) {
    if (type->is_struct()) {
        emit("movq", rax, r11);
        int offset = 0;
        offset     = copy_bytes(obj, offset, type->size(), 8);
        offset     = copy_bytes(obj, offset, type->size(), 4);
        offset     = copy_bytes(obj, offset, type->size(), 2);
        offset     = copy_bytes(obj, offset, type->size(), 1);
        return;
    }
    auto addr    = obj->to_string();
    auto isfloat = type->is_float();
    auto size    = type->size();
    emit(isfloat ? fmov(size) : imov(size), rdest(size, isfloat), addr);
}

void Generator::copy_struct(const Type *) {}

int Generator::copy_bytes(ObjAddr *obj, int offset, int max, int step) {
    auto mov  = imov(step);
    auto dest = idest(step);
    while (max - offset >= step) {
        emit(mov, ObjAddr::base_addr(offset, r11)->to_string(), dest);
        emit(mov, dest, obj->to_string());
        obj->offset += step;
        offset += step;
    }
    return offset;
}

void Generator::emit_idiv(const Type *type) {
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

void Generator::emit_fdiv(const Type *type) { emit(finst("div", type->size()), fsrc(), fdest()); }
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

void Generator::emit_icmp(const char *setcc, int width) {
    emit(iinst("cmp", width), isrc(width), idest(width));
    emit(setcc, al);
    emit("movzbq", al, rax);
}

// ucomiss: Unordered Compare Scalar Single-Precision Floating-Point Values and Set EFLAGS
// ucomisd: Unordered Compare Scalar Double-Precision Floating-Point Values and Set EFLAGS
void Generator::emit_fcmp(const char *setcc, int width) {
    emit(finst("ucomi", width), fsrc(), fdest());
    emit(setcc, al);
    emit("movzbq", al, rax);
}

void Generator::emit_global_variable(InitDeclarator *id) {
    auto name      = id->halftype()->token()->get_lexeme();
    auto obj       = _current_scope->find_var_in_local(name);
    auto gvarlabel = gvar_label(id->halftype()->token()->get_lexeme());
    emit(".globl", gvarlabel);
    emit_label(gvarlabel);
    if (id->is_initialized()) {
        auto e          = id->initializer()->assignment();
        auto type       = obj->type();
        auto align_size = type->size() >= 8 ? 8 : type->size();
        auto align      = align_type(static_cast<Alignment>(align_size));
        switch (e->kind()) {
        case EXPR_INT: {
            auto i = static_cast<const IntConst *>(e);
            emit(align, to_string(i->value()));
            break;
        }
        case EXPR_FLOAT: {
            auto f = static_cast<const FloatConst *>(e);
            emit(align, to_string(static_cast<uint64_t>(f->value())));
            break;
        }
        case EXPR_STR: {
            auto s = static_cast<const StringLiteral *>(e);
            emit(".string", s->get_value());
            break;
        }
        case EXPR_FUNC_CALL:
        case EXPR_SUBSCRIPT:
        case EXPR_PINC:
        case EXPR_PDEC:
        case EXPR_IDENT:
        case EXPR_UNARY:
        case EXPR_CAST:
        case EXPR_BINARY:
        case EXPR_COND:
        case EXPR_CONV:
        case EXPR_ASSIGNMENT:
        case EXPR_STMT:
        case EXPR_MEMBER:
            unreachable();
            break;
        }
    } else {
        emit(".zero", to_string(obj->type()->size()));
    }
}

void Generator::emit_loc(int lineno) { emit(".loc 1", to_string(lineno)); }
void Generator::emit_load_value(const Type *type) {
    auto size = type->size();
    auto addr = _lvgtr->addr();
    if (type->is_scalar()) {
        if (type->is_float()) {
            emit(fmov(size), addr, fdest());
        } else {
            switch (type->size()) {
            case 1:
                emit(to_quad[0][type->is_signed()], addr, rax);
                break;
            case 2:
                emit(to_quad[1][type->is_signed()], addr, rax);
                break;
            case 4:
                emit(to_quad[2][type->is_signed()], addr, rax);
                break;
            case 8:
                emit("movq", addr, rax);
                break;
            default:
                unreachable();
            }
        }
    } else {
        emit("leaq", addr, rax);
    }
}

void Generator::emit_data() {
    if (!_unit->global_vars().empty()) {
        emit(".section", ".data");
        for (auto gvar : _unit->global_vars())
            emit_global_variable(gvar);
    }

    if (!_rodata.empty()) {
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
    case TY_ARRAY:
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
    _rodata.push_back(new NumRoData<uint64_t>(fau_label, val));
    return fau_label;
}
const string Generator::add_rodata(const float fval) {
    auto fau_label     = float_label();
    const uint32_t val = *reinterpret_cast<const uint32_t *>(&fval);
    _rodata.push_back(new NumRoData<uint32_t>(fau_label, val));
    return fau_label;
}

const string addr_based_rip(const string &label, int offset) {
    return ObjAddr::base_addr(offset, label, rip)->to_string();
    // return offset == 0 ? label + "(%rip)" : label + std::to_string(offset) + "(%rip)";
}

void Generator::gen() {
    stringstream ss;
    ss << 1 << " \"" << _src_file_name << "\"";
    emit(".file", ss.str());
    for (auto decl : _unit->func_defs()) {
        visit(decl);
    }
    emit_data();
}

void Generator::visit_func_call(FuncCall *fc) {
    // TODO float numbers and arguments with size larger than 8
    // process args
    auto args = fc->args();
    for (int idx = args.size() - 1; idx >= 0; idx--) {
        auto arg = args.at(idx);
        visit(arg);
        if (arg->type()->is_derefed())
            push(rax, 8, false);
        else {
            auto width   = arg->type()->size();
            auto isfloat = arg->type()->is_float();
            auto reg     = isfloat ? fdest() : idest(8);
            push(reg, width, isfloat);
        }
    }
    for (size_t idx = 0; idx < args.size() && idx < 6; idx++) {
        auto arg = args.at(idx);
        if (arg->type()->is_derefed())
            pop(reg_args[idx][0], 8, false);
        else {
            auto size    = arg->type()->size();
            auto isfloat = arg->type()->is_float();
            auto reg     = isfloat ? xmm[idx] : reg_args[idx][0];
            pop(reg, size, isfloat);
        }
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

void Generator::vist_subscription(Subscript *sub) { _lvgtr->visit(sub->array()); }

void Generator::visit_postfix_inc(PostInc *inc) {}

void Generator::visit_postfix_dec(PostDec *dec) {}
void Generator::visit_member_access(MemberAccess *ma) {
    _lvgtr->visit(ma);
    emit_load_value(ma->type());
}

void Generator::visit_func_def(FuncDef *fd) {
    // record current function and generate return label
    fd->set_offset_for_local_vars();
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
    // allocate stack frame and set offset of local variables
    auto scope = fd->body()->scope();
    int offset = _current_fn.def->stack_size();
    if (offset > 0)
        emit("subq", offset, rsp);
    // move arguments onto stack
    auto sig      = fd->signature();
    int float_idx = 0, integer_idx = 0;
    for (auto arg : sig->parameters()) {
        auto type = arg->type();
        auto name = arg->token()->get_lexeme();
        if (type->is_arithmetic() || type->is_pointer()) {
            auto addr =
                ObjAddr::base_addr(scope->find_var_in_local(name)->offset(), rbp)->to_string();
            if (type->is_integer())
                emit(imov(type->size()), reg_args[integer_idx++][8 - type->size()], addr);
            else
                emit(fmov(type->size()), xmm[float_idx++], addr);
        }
    }
    // function body
    visit_block(fd->body());

    // return
    emit_label(_current_fn.ret_label);
    emit("movq", rbp, rsp);
    emit("popq", rbp);
    emit("retq");
}

void Generator::visit_int_const(IntConst *ic) {
    auto width = ic->type()->size();
    auto r     = idest(width);
    emit(imov(width), ic->value(), r);
}

void Generator::visit_float_const(FloatConst *fc) {
    auto width = fc->type()->size();
    auto r     = fdest();
    auto dval  = fc->value();
    if (width == 4) {
        float fval = dval;
        auto label = add_rodata(fval);
        emit(fmov(width), addr_based_rip(label, 0), r);
    } else {
        auto label = add_rodata(dval);
        emit(fmov(width), addr_based_rip(label, 0), r);
    }
}
void Generator::visit_string_literal(StringLiteral *str) {
    auto label = add_rodata(str->get_value());
    emit("leaq", addr_based_rip(label, 0), rax);
}

void Generator::emit_iset0(const string_view &reg) { emit("xorq", reg, reg); }
void Generator::emit_fset0(const string_view &freg) { emit("pxor", freg, freg); }
void Generator::emit_arithmetic_integer_additive(Add *a) {
    emit_oprands_for_integer_binary(a);
    auto width = a->lhs()->type()->size();
    auto src = isrc(width), dest = idest(width);
    emit(iinst(a->token()->get_type() == '+' ? "add" : "sub", width), src, dest);
}
void Generator::emit_derefed_additive(Add *a) {
    // lhs must be derefed
    auto lhs  = a->lhs();
    auto rhs  = a->rhs();
    auto type = lhs->type();
    if (rhs->type()->is_derefed()) {
        visit(rhs);
        push(rax, 8, false);
        visit(lhs);
        pop(r11, 8, false);
        emit("subq", r11, rax);
        emit("movq", type->derefed()->size(), r11);
        emit("cqto");
        emit("idivq", r11);
    } else {
        const Type *base = type->derefed();
        // if (rhs->is_int_const()) {
        //     auto val = static_cast<IntConst *>(rhs);
        //     if (val->value() == 0) {
        //         visit(lhs);
        //         return;
        //     }
        // }
        visit(rhs);
        emit_promot_int(rhs->type());
        emit("movq", base->size(), r11);
        emit("mulq", r11);
        push(rax, 8, false);
        visit(lhs);
        pop(r11, 8, false);
        // both '+' and '[' emit "addq"
        emit(a->token()->get_type() == '-' ? "subq" : "addq", r11, rax);
    }
}
void Generator::emit_additive(Binary *b) {}

void Generator::emit_float_binary(Binary *e) {
    // type info
    auto type  = e->lhs()->type();
    auto width = type->size();
    // generate code for lhs
    visit(e->lhs());
    // cache the result of lhs
    auto dest = fdest();
    push(dest, width, true);
    // generate code for rhs
    // result of rhs is stored at dest reg
    visit(e->rhs());
    auto src = fsrc();
    auto m   = fmov(width);
    emit(m, dest, src);
    pop(dest, width, true);
    switch (e->token()->get_type()) {
    case '*': { // *
        auto inst = finst("mul", width);
        emit(inst, src, dest);
        return;
    }
    case '/': { // /
        emit_fdiv(type);
        return;
    }
    case '+': { // +
        auto inst = finst("add", width);
        emit(inst, src, dest);
        return;
    }
    case '-': { // -
        auto inst = finst("sub", width);
        emit(inst, src, dest);
        return;
    }
    case '<': // <
        emit_fcmp("setb", width);
        return;
    case TK_LEQUAL: // <=
        emit_fcmp("setbe", width);
        return;
    case '>': // >
        emit_fcmp("seta", width);
        return;
    case TK_GEQUAL: // >=
        emit_fcmp("setae", width);
        return;
    case TK_EQUAL: // ==
        emit_fcmp("sete", width);
        return;
    case TK_NEQUAL: // !=
        emit_fcmp("setne", width);
        return;
    default:
        error("not a valid binary operator.");
    }
}

// emit LAND
//      code for lhs...
//
//      code for rhs...
// .LNOT

void Generator::emit_logic(Binary *e, bool isand) {
    auto cmpf_set = [this](Expr *e) {
        emit_fset0(fsrc());
        emit(finst("ucomi", e->type()->size()), fdest(), fsrc());
    };
    auto cmpi_set = [this](Expr *e) {
        emit_iset0(isrc(8));
        emit(iinst("com", e->type()->size()), idest(e->type()->size()), isrc(e->type()->size()));
    };
    auto isfloat = e->type()->is_float();
    auto lsetres = branch_label();
    auto lend    = branch_label();
    // emit code for lhs
    visit(e);
    // compare result with 0
    if (isfloat)
        cmpf_set(e->lhs());
    else
        cmpi_set(e->lhs());
    // short out jmp
    auto jmp = isand ? "je" : "jne";
    emit(jmp, lsetres);
    // emit code for rhs
    visit(e);
    // compare result with 0
    if (isfloat)
        cmpf_set(e->lhs());
    else
        cmpi_set(e->rhs());
    emit(jmp, lsetres);
    emit("movq", 1, rax);
    emit("jmp", lend);
    emit_label(lsetres);
    if (isand)
        emit_iset0(rax);
    else
        emit("movq", 1, rax);
    emit_label(lend);
}

void Generator::visit_mult(Multi *e) {
    if (e->lhs()->type()->is_float()) {
        emit_float_binary(e);
        return;
    }
    // emit code for integer multi or divide
    emit_oprands_for_integer_binary(e);
    auto lhs_type = e->lhs()->type();
    auto width    = lhs_type->size();
    auto dest     = idest(width);
    auto src      = isrc(width);
    switch (e->token()->get_type()) {
    case '*': { // *
        auto is_signed = lhs_type->is_signed();
        auto inst      = iinst("mul", width, is_signed);
        emit(inst, src);
        return;
    }
    case '/': { // /
        emit_idiv(lhs_type);
        return;
    }
    case '%': { // %
        emit_idiv(lhs_type);
        emit(imov(width), dreg(width), dest);
        return;
    }
    }
}

void Generator::visit_additive(Add *e) {
    if (e->lhs()->type()->is_float()) {
        emit_float_binary(e);
    } else {
        // parser ensures that if one of the oprands is derefed, it will be placed at lhs
        if (e->lhs()->type()->is_derefed())
            emit_derefed_additive(e);
        else
            emit_arithmetic_integer_additive(e);
    }
}

void Generator::visit_shift(Shift *e) {
    emit_oprands_for_integer_binary(e);
    auto width = e->lhs()->type()->size();
    const char *inst;
    switch (e->token()->get_type()) {
    case TK_LSHIFT: // <<
        inst = "shl";
        break;
    case TK_RSHIFT: { // >>
        auto is_signed = e->lhs()->type()->is_signed();
        inst           = is_signed ? "sar" : "shr";
        break;
    }
    default:
        unreachable();
        inst = "";
    }
    emit("movq", r11, rcx);
    emit(iinst(inst, width), cl, idest(width));
}

void Generator::visit_relational(Relational *e) {
    if (e->lhs()->type()->is_float()) {
        emit_float_binary(e);
    } else {
        emit_oprands_for_integer_binary(e);
        auto is_signed = e->lhs()->type()->is_signed();
        auto width     = e->lhs()->type()->size();
        switch (e->token()->get_type()) {
        case '<':
            emit_icmp(is_signed ? "setl" : "setb", width);
            return;
        case TK_LEQUAL: // <=
            emit_icmp(is_signed ? "setle" : "setbe", width);
            return;
        case '>':
            emit_icmp(is_signed ? "setg" : "seta", width);
            return;
        case TK_GEQUAL: // >=
            emit_icmp(is_signed ? "setge" : "setae", width);
            return;
        case TK_EQUAL: // ==
            emit_icmp("sete", width);
            return;
        case TK_NEQUAL: // !=
            emit_icmp("setne", width);
            return;
        }
    }
}

void Generator::visit_bitwise(Bitwise *e) {
    string inst;
    emit_oprands_for_integer_binary(e);
    auto width = e->lhs()->type()->size();
    switch (e->token()->get_type()) {
    case '&': // &
        inst = iinst("and", width);
        break;
    case '^': // ^
        inst = iinst("xor", width);
        break;
    case '|': // |
        inst = iinst("or", width);
        break;
    default:
        unreachable();
    }
    emit(inst, isrc(width), idest(width));
}
void Generator::visit_logical(Logical *l) { emit_logic(l, l->token()->get_type() == TK_LAND); }

void Generator::emit_oprands_for_integer_binary(Binary *e) {
    // type info
    auto lhs_type = e->lhs()->type();
    auto width    = lhs_type->size();
    // generate code for lhs
    visit(e->lhs());
    // cache the result of lhs
    auto dest = idest(width);
    push(rax, width, false);
    // generate code for rhs
    // result of rhs is stored at dest reg
    visit(e->rhs());
    auto src = isrc(width);
    auto m   = imov(width);
    // mov rhs to src reg and pop lhs to dest reg
    // to keep the oprands order in assembly as the same as in the C code
    // it's necessary for div and sub, but unnecessary for add and mul
    emit(m, dest, src);
    pop(rax, width, false);
}

// labeled
void Generator::visit_labeled(Labeled *l) {
    emit_label(l->label());
    visit(l->stmt());
}

// selection
void Generator::visit_ifelse(IfElse *ifelse) {
    visit(ifelse->cond());
    auto else_label = branch_label();
    auto end_label  = branch_label();
    emit("cmpl", 0, eax);
    emit("je", else_label);
    visit(ifelse->then());
    emit("jmp", end_label);
    emit_label(else_label);
    visit(ifelse->otherwise());
    emit_label(end_label);
}
void Generator::visit_switch(Switch *s) {
    auto backup   = _break_to;
    auto se_label = switch_end_label();
    auto breakto  = BreakTo{se_label};
    _break_to     = &breakto;
    // emit code
    visit(s->expr());
    for (auto cd : s->labels()) {
        // default label
        if (cd.first == nullptr) {
            emit("jmp", cd.second);
        }
        // case label
        else {
            emit("movl", cd.first->value(), r11d);
            emit("cmpl", eax, r11d);
            emit("je", cd.second);
        }
    }
    emit("jmp", se_label);
    visit(s->body());
    emit_label(se_label);
    _break_to = backup;
}
// iteration
void Generator::visit_while(While *w) {
    // backup current iteration state
    ContinueTo *pib;
    BreakTo *pbt;
    backup_loop(&pib, &pbt);

    // create new labels used in this scope
    auto bl = iter_begin_label();
    auto el = iter_end_label();
    BreakTo bt{el};
    ContinueTo ib{bl};

    // assign new temporary labels
    restore_loop(&ib, &bt);

    // emit code
    emit_label(bl);
    visit(w->cond());
    emit("cmpq", 0, rax);
    emit("je", el);
    visit(w->body());
    emit("jmp", bl);
    emit_label(el);

    // restore iteration state
    restore_loop(pib, pbt);
}
void Generator::visit_do_while(DoWhile *dw) {
    // backup current iteration state
    ContinueTo *pib;
    BreakTo *pbt;
    backup_loop(&pib, &pbt);

    // create new labels used in this scope
    auto bl = iter_begin_label();
    auto el = iter_end_label();
    BreakTo bt{el};
    ContinueTo ib{bl};

    // assign new temporary labels
    restore_loop(&ib, &bt);

    emit_label(bl);
    visit(dw->body());
    visit(dw->cond());
    emit("jnz", bl);
    emit_label(el);

    // restore iteration state
    restore_loop(pib, pbt);
}
void Generator::visit_for(For *f) {
    // backup current iteration state
    ContinueTo *pib;
    BreakTo *pbt;
    backup_loop(&pib, &pbt);

    // create new labels used in this scope
    auto bl = iter_begin_label();
    auto el = iter_end_label();
    BreakTo bt{el};
    ContinueTo ib{bl};

    // assign new temporary labels
    restore_loop(&ib, &bt);

    // emit code
    visit(f->init());
    emit_label(bl);
    if (f->cond() != nullptr) {
        visit(f->cond());
        emit("cmpl", 0, eax);
        emit("je", el);
    }
    visit(f->body());
    if (f->accumulator() != nullptr)
        visit(f->accumulator());
    emit("jmp", bl);
    emit_label(el);

    // restore loop status
    restore_loop(pib, pbt);
}
void Generator::visit_goto(Goto *g) { emit("jmp", userlabel(g->label())); }
void Generator::visit_continue(Continue *c) { emit("jmp", _cont_to->label); }
void Generator::visit_break(Break *b) { emit("jmp", _break_to->label); }
void Generator::visit_return(Return *rs) {
    visit(rs->expr());
    emit("jmp", _current_fn.ret_label);
}

void Generator::visit_assignment(Assignment *assignment) {
    auto rhs = assignment->rhs();
    auto lhs = assignment->lhs();
    _lvgtr->visit(lhs);
    auto addr = _lvgtr->mut_obj();
    push_addr(addr);
    visit(rhs);
    pop_addr(addr);
    store_value(addr, lhs->type());
}

void Generator::visit_comma(Comma *c) {
    visit(c->lhs());
    visit(c->rhs());
}
void Generator::visit_block(Block *block) {
    auto bak_scope = _current_scope;
    _current_scope = block->scope();
    for (auto item : block->items()) {
        visit(item);
    }
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
        auto m = imov(type->size());
        emit(m, idest(type->size()), simple_addr(obj));
    }
}

void Generator::visit_initializer(Initializer *init) { visit(init->assignment()); }

void Generator::visit_identifier(Identifier *ident) {
    emit_loc(ident->token()->get_position()->get_line());
    _lvgtr->visit(ident);
    auto obj = _current_scope->find_var(ident->get_value());
    emit_load_value(obj->type());
}

void Generator::visit_conv(Conv *conv) {
    visit(conv->expr());
    debug("from %s to %s", conv->expr()->type()->normalize().c_str(),
          conv->type()->normalize().c_str());
    if (conv->type()->equals_to(conv->expr()->type()))
        return;
    emit_cvt(conv->expr()->type(), conv->type());
}

void Generator::visit_cast(Cast *) {}
void Generator::visit_unary(Unary *ue) {
    switch (ue->unary_type()) {
    case TK_INC: {
        // ++ expr => expr = expr + 1
        _lvgtr->visit(ue->oprand());
        auto dest = _lvgtr->addr();
        auto add  = ue->type()->is_float() ? finst("add", ue->type()->size())
                                           : iinst("add", ue->type()->size());
        emit(add, 1, dest);
        visit(ue->oprand());
        return;
    }
    case TK_DEC: {
        // -- expr => expr = expr - 1
        _lvgtr->visit(ue->oprand());
        auto dest = _lvgtr->addr();
        auto sub  = ue->type()->is_float() ? finst("sub", ue->type()->size())
                                           : iinst("sub", ue->type()->size());
        emit(sub, 1, dest);
        visit(ue->oprand());
        return;
    }
    case '*': {
        visit(ue->oprand());
        auto valtype = ue->type();
        if (valtype->is_scalar()) {
            if (valtype->is_float()) {
                emit(fmov(valtype->size()), addr(rax), fdest());
            } else {
                emit(imov(valtype->size()), addr(rax), idest(valtype->size()));
            }
        }
        return;
    }
    case '+': {
        visit(ue->oprand());
        return;
    }
    case '-': {
        visit(ue->oprand());
        if (ue->type()->is_integer()) {
            emit(iinst("neg", ue->type()->size()), idest(ue->type()->size()));
        }
        return;
    }
    case '!':
        unimplement();
    case '&': {
        _lvgtr->visit(ue->oprand());
        emit("leaq", _lvgtr->addr(), rax);
        return;
    }
    case '~':
        unimplement();
    default:
        unreachable();
    }
}

string Generator::code() const { return _buffer.str(); }

// Generator end

const string ObjAddr::to_string() const {
    stringstream ss;
    if (offset != 0) {
        ss << offset;
        if (!label.empty()) {
            ss << "+";
        }
    }
    ss << label;
    if (!(base.empty() && index.empty())) {
        ss << "(";
        if (!base.empty()) {
            ss << base;
        }
        if (!index.empty())
            ss << "," << index << "," << scale;
        ss << ")";
    }
    return ss.str();
}

// LValueGenerator

void LValueGenerator::visit_unary(Unary *ue) {
    switch (ue->unary_type()) {
    case TK_INC: {
    }
    case TK_DEC: {
    }
    case '*': {
        _gtr->visit(ue->oprand());
        _gtr->emit("movq", rax, r10);
        _obj_addr.emplace(ObjAddr::indirect(r10));
        return;
    }
    case '+': {
    }
    case '&': {
        visit(ue->oprand());
        return;
    }
    case '-':
    case '!':
    case '~':
    default:
        unreachable();
    }
}
void LValueGenerator::visit_identifier(Identifier *ident) {
    auto name = ident->get_value();
    auto obj  = _gtr->_current_scope->find_var(ident->get_value());
    auto sp   = ident->token()->get_position();
    _gtr->emit("#", sp->get_file_name(), to_string(sp->get_line()));
    if (obj->is_global())
        _obj_addr.emplace(ObjAddr::base_addr(gvar_label(name), rip));
    else {
        auto stack_offset = obj->offset();
        _obj_addr.emplace(ObjAddr::base_addr(stack_offset, rbp));
        debug("var name: %s, offset: %d", ident->get_value(), stack_offset);
        _gtr->emit("#", name, _obj_addr.value()->to_string());
    }
}
void LValueGenerator::vist_subscription(Subscript *s) {
    visit(s->array());
    _gtr->emit("leaq", _obj_addr.value()->to_string(), r10);
    _gtr->visit(s->sub());
    _gtr->emit("addq", rax, r10);
}
void LValueGenerator::visit_member_access(MemberAccess *ma) {
    _gtr->visit(ma->lhs());
    _gtr->emit("movq", rax, r10);
    auto ident  = ma->member();
    auto member = ma->token()->get_type() == TK_ARROW
                      ? ma->lhs()->type()->derefed()->as_struct()->find_member(ident->get_value())
                      : ma->lhs()->type()->as_struct()->find_member(ident->get_value());
    _obj_addr.emplace(ObjAddr::base_addr(member->offset(), r10));
}
void LValueGenerator::visit_comma(Comma *c) {
    _gtr->visit(c->lhs());
    visit(c->rhs());
}
// LValueGenerator end

/* File generated from libasmir.idl */

#ifndef _CAMLIDL_LIBASMIR_H
#define _CAMLIDL_LIBASMIR_H

#ifdef __cplusplus
#define _CAMLIDL_EXTERN_C extern "C"
#else
#define _CAMLIDL_EXTERN_C extern
#endif

#ifdef _WIN32
#pragma pack(push,8) /* necessary for COM interfaces */
#endif

typedef void *Exp;

typedef void *Stmt;

typedef void *asm_function_t;

typedef void *asm_program_t;

typedef void *vine_block_t;

typedef void *vine_blocks_t;

typedef void *dyn_function_t;

typedef void *dyn_functions_t;

typedef void *memory_cell_data_t;

typedef void *memory_data_t;

typedef void *instmap_t;

typedef unsigned long long address_t;

struct _raw_inst_t {
  address_t address;
  int length;
  unsigned char *bytes;
};

typedef struct _raw_inst_t raw_inst_t;

enum bfd_architecture {
bfd_arch_unknown,
bfd_arch_obscure,
bfd_arch_m68k,
bfd_arch_vax,
bfd_arch_i960,
bfd_arch_or32,
bfd_arch_sparc,
bfd_arch_spu,
bfd_arch_mips,
bfd_arch_i386,
bfd_arch_we32k,
bfd_arch_tahoe,
bfd_arch_i860,
bfd_arch_i370,
bfd_arch_romp,
bfd_arch_convex,
bfd_arch_m88k,
bfd_arch_m98k,
bfd_arch_pyramid,
bfd_arch_h8300,
bfd_arch_pdp11,
bfd_arch_powerpc,
bfd_arch_rs6000,
bfd_arch_hppa,
bfd_arch_d10v,
bfd_arch_d30v,
bfd_arch_dlx,
bfd_arch_m68hc11,
bfd_arch_m68hc12,
bfd_arch_z8k,
bfd_arch_h8500,
bfd_arch_sh,
bfd_arch_alpha,
bfd_arch_arm,
bfd_arch_ns32k,
bfd_arch_w65,
bfd_arch_tic30,
bfd_arch_tic4x,
bfd_arch_tic54x,
bfd_arch_tic80,
bfd_arch_v850,
bfd_arch_arc,
bfd_arch_m32c,
bfd_arch_m32r,
bfd_arch_mn10200,
bfd_arch_mn10300,
bfd_arch_fr30,
bfd_arch_frv,
bfd_arch_mcore,
bfd_arch_mep,
bfd_arch_ia64,
bfd_arch_ip2k,
bfd_arch_iq2000,
bfd_arch_mt,
bfd_arch_pj,
bfd_arch_avr,
bfd_arch_bfin,
bfd_arch_cr16,
bfd_arch_cr16c,
bfd_arch_crx,
bfd_arch_cris,
bfd_arch_s390,
bfd_arch_score,
bfd_arch_openrisc,
bfd_arch_mmix,
bfd_arch_xstormy16,
bfd_arch_msp430,
bfd_arch_xc16x,
bfd_arch_xtensa,
bfd_arch_maxq,
bfd_arch_z80,
bfd_arch_last,
};

struct _vine_symbol_t {
  char *name;
  address_t addr;
  int is_function;
  int is_dynamic;
};

typedef struct _vine_symbol_t vine_symbol_t;

struct _cflow_t {
  int ctype;
  address_t target;
};

typedef struct _cflow_t cflow_t;

typedef void *vine_symbols_t;

enum exp_type_t {
BINOP,
UNOP,
CONSTANT,
MEM,
TEMP,
PHI,
CAST,
NAME,
UNKNOWN,
LET,
EXTENSION,
};

enum reg_t {
REG_1,
REG_8,
REG_16,
REG_32,
REG_64,
};

enum binop_type_t {
PLUS = 0,
MINUS,
TIMES,
DIVIDE,
MOD,
LSHIFT,
RSHIFT,
ARSHIFT,
LROTATE,
RROTATE,
LOGICAND,
LOGICOR,
BITAND,
BITOR,
XOR,
EQ,
NEQ,
GT,
LT,
GE,
LE,
SDIVIDE,
SMOD,
};

enum unop_type_t {
NEG,
NOT,
};

typedef long long const_val_t;

enum cast_t {
CAST_UNSIGNED,
CAST_SIGNED,
CAST_HIGH,
CAST_LOW,
CAST_FLOAT,
CAST_INTEGER,
CAST_RFLOAT,
CAST_RINTEGER,
};

enum stmt_type_t {
JMP,
CJMP,
SPECIAL,
MOVE,
COMMENT,
LABEL,
EXPSTMT,
VARDECL,
CALL,
RETURN,
FUNCTION,
ASSERT,
};

_CAMLIDL_EXTERN_C int exp_type(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int binop_type(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp binop_lhs(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp binop_rhs(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int unop_type(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp unop_subexp(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp mem_addr(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int mem_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C const_val_t constant_val(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int constant_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *phi_phiname(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int phi_numnodes(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp phi_nodeat(/*in*/ Exp e, /*in*/ int i);

_CAMLIDL_EXTERN_C int temp_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *temp_name(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *unknown_str(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int cast_width(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int cast_casttype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp cast_subexp(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *name_string(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp let_var(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp let_exp(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp let_in(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int stmt_type(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp move_lhs(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp move_rhs(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *label_string(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *special_string(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *comment_string(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp jmp_target(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp cjmp_cond(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp cjmp_ttarget(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp cjmp_ftarget(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp expstmt_exp(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *vardecl_name(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int vardecl_type(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int call_has_lval(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp call_lval_opt(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp call_fnname(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp *call_params(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int ret_has_exp(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp ret_exp(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char const *func_name(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int func_has_rv(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int func_rt(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Stmt *func_params(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int func_is_external(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Stmt *func_body(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp assert_cond(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Stmt *gen_eflags_helpers_c(void);

_CAMLIDL_EXTERN_C asm_program_t disassemble_program(/*in*/ char *filename);

_CAMLIDL_EXTERN_C asm_program_t instmap_to_asm_program(/*in*/ instmap_t instmap);

_CAMLIDL_EXTERN_C instmap_t filename_to_instmap(/*in*/ char *filename);

_CAMLIDL_EXTERN_C vine_blocks_t instmap_translate_address_range(/*in*/ instmap_t instmap, /*in*/ address_t start_addr, /*in*/ address_t end_addr);

_CAMLIDL_EXTERN_C vine_block_t instmap_translate_address(/*in*/ instmap_t instmap, /*in*/ address_t addr);

_CAMLIDL_EXTERN_C int instmap_has_address(/*in*/ instmap_t instmap, /*in*/ address_t addr);

_CAMLIDL_EXTERN_C address_t get_last_segment_address(/*in*/ char *filename, /*in*/ address_t addr);

_CAMLIDL_EXTERN_C vine_blocks_t asmir_asmprogram_to_vine(/*in*/ asm_program_t prog);

_CAMLIDL_EXTERN_C int asmir_vine_blocks_size(/*in*/ vine_blocks_t bs);

_CAMLIDL_EXTERN_C vine_block_t asmir_vine_blocks_get(/*in*/ vine_blocks_t bs, /*in*/ int i);

_CAMLIDL_EXTERN_C void destroy_vine_block(/*in*/ vine_block_t bs);

_CAMLIDL_EXTERN_C void destroy_vine_blocks(/*in*/ vine_blocks_t bs);

_CAMLIDL_EXTERN_C int asmir_vine_block_size(/*in*/ vine_block_t b);

_CAMLIDL_EXTERN_C address_t asmir_vine_block_address(/*in*/ vine_block_t b);

_CAMLIDL_EXTERN_C Stmt asmir_vine_block_get(/*in*/ vine_block_t b, /*in*/ int i);

_CAMLIDL_EXTERN_C void free_asm_function(/*in*/ asm_function_t f);

_CAMLIDL_EXTERN_C void free_asm_program(/*in*/ asm_program_t p);

_CAMLIDL_EXTERN_C vine_block_t asm_addr_to_ir(/*in*/ asm_program_t p, /*in*/ address_t addr);

_CAMLIDL_EXTERN_C char *string_blockinsn(/*in*/ asm_program_t prog, /*in*/ vine_block_t block);

_CAMLIDL_EXTERN_C dyn_functions_t get_synthetic_symbols(/*in*/ char *filename);

_CAMLIDL_EXTERN_C int dyn_functions_size(/*in*/ dyn_functions_t ds);

_CAMLIDL_EXTERN_C dyn_function_t dyn_functions_get(/*in*/ dyn_functions_t ds, /*in*/ int i);

_CAMLIDL_EXTERN_C char *dyn_functions_name(/*in*/ dyn_function_t d);

_CAMLIDL_EXTERN_C address_t dyn_functions_addr(/*in*/ dyn_function_t d);

_CAMLIDL_EXTERN_C void destroy_dyn_functions(/*in*/ dyn_functions_t ds);

_CAMLIDL_EXTERN_C vine_symbols_t get_symbols_of_file(/*in*/ char *filename);

_CAMLIDL_EXTERN_C int symbols_size(/*in*/ vine_symbols_t ds);

_CAMLIDL_EXTERN_C vine_symbol_t *symbols_get(/*in*/ vine_symbols_t ds, /*in*/ int i);

_CAMLIDL_EXTERN_C void destroy_symbols(/*in*/ vine_symbols_t ds);

_CAMLIDL_EXTERN_C address_t memory_cell_data_address(/*in*/ memory_cell_data_t md);

_CAMLIDL_EXTERN_C int memory_cell_data_value(/*in*/ memory_cell_data_t md);

_CAMLIDL_EXTERN_C memory_data_t get_rodata(/*in*/ asm_program_t prog);

_CAMLIDL_EXTERN_C memory_data_t get_bssdata(/*in*/ asm_program_t prog);

_CAMLIDL_EXTERN_C int memory_data_size(/*in*/ memory_data_t md);

_CAMLIDL_EXTERN_C memory_cell_data_t memory_data_get(/*in*/ memory_data_t md, /*in*/ int i);

_CAMLIDL_EXTERN_C void destroy_memory_data(/*in*/ memory_data_t md);

_CAMLIDL_EXTERN_C void set_use_eflags_thunks(/*in*/ int v);

_CAMLIDL_EXTERN_C int get_use_eflags_thunks(void);

_CAMLIDL_EXTERN_C void set_call_return_translation(/*in*/ int v);

_CAMLIDL_EXTERN_C int asmprogram_arch(/*in*/ asm_program_t prog);

_CAMLIDL_EXTERN_C void print_disasm_rawbytes(/*in*/ int arch, /*in*/ address_t addr, /*in*/ char *buf, /*in*/ int size);

_CAMLIDL_EXTERN_C asm_program_t byte_insn_to_asmp(/*in*/ int arch, /*in*/ address_t addr, /*in*/ char *bb_bytes, /*in*/ int len);

#ifdef _WIN32
#pragma pack(pop)
#endif


#endif /* !_CAMLIDL_LIBASMIR_H */

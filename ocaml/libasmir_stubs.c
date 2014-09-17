/* File generated from libasmir.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>


#include "libasmir.h"

void camlidl_ml2c_libasmir_Exp(value _v1, Exp * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Exp *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_Exp(Exp * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Exp) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Exp *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_Stmt(value _v1, Stmt * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Stmt *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_Stmt(Stmt * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Stmt) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Stmt *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_asm_function_t(value _v1, asm_function_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((asm_function_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_asm_function_t(asm_function_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(asm_function_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((asm_function_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_asm_program_t(value _v1, asm_program_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((asm_program_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_asm_program_t(asm_program_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(asm_program_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((asm_program_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_vine_block_t(value _v1, vine_block_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((vine_block_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_vine_block_t(vine_block_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(vine_block_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((vine_block_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_vine_blocks_t(value _v1, vine_blocks_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((vine_blocks_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_vine_blocks_t(vine_blocks_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(vine_blocks_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((vine_blocks_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_dyn_function_t(value _v1, dyn_function_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((dyn_function_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_dyn_function_t(dyn_function_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(dyn_function_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((dyn_function_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_dyn_functions_t(value _v1, dyn_functions_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((dyn_functions_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_dyn_functions_t(dyn_functions_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(dyn_functions_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((dyn_functions_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_memory_cell_data_t(value _v1, memory_cell_data_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((memory_cell_data_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_memory_cell_data_t(memory_cell_data_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(memory_cell_data_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((memory_cell_data_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_memory_data_t(value _v1, memory_data_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((memory_data_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_memory_data_t(memory_data_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(memory_data_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((memory_data_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_instmap_t(value _v1, instmap_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((instmap_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_instmap_t(instmap_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(instmap_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((instmap_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_address_t(value _v1, address_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int64_val(_v1);
}

value camlidl_c2ml_libasmir_address_t(address_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int64((*_c2));
  return _v1;
}

void camlidl_ml2c_libasmir_struct__raw_inst_t(value _v1, struct _raw_inst_t * _c2, camlidl_ctx _ctx)
{
  value _v3;
  value _v4;
  mlsize_t _c5;
  mlsize_t _c6;
  value _v7;
  _v3 = Field(_v1, 0);
  camlidl_ml2c_libasmir_address_t(_v3, &(*_c2).address, _ctx);
  _v4 = Field(_v1, 1);
  _c5 = Wosize_val(_v4);
  (*_c2).bytes = camlidl_malloc(_c5 * sizeof(unsigned char ), _ctx);
  for (_c6 = 0; _c6 < _c5; _c6++) {
    _v7 = Field(_v4, _c6);
    (*_c2).bytes[_c6] = Int_val(_v7);
  }
  (*_c2).length = _c5;
}

value camlidl_c2ml_libasmir_struct__raw_inst_t(struct _raw_inst_t * _c1, camlidl_ctx _ctx)
{
  value _v2;
  value _v3[2];
  mlsize_t _c4;
  value _v5;
  _v3[0] = _v3[1] = 0;
  Begin_roots_block(_v3, 2)
    _v3[0] = camlidl_c2ml_libasmir_address_t(&(*_c1).address, _ctx);
    _v3[1] = camlidl_alloc((*_c1).length, 0);
    for (_c4 = 0; _c4 < (*_c1).length; _c4++) {
      _v5 = Val_int((*_c1).bytes[_c4]);
      modify(&Field(_v3[1], _c4), _v5);
    }
    _v2 = camlidl_alloc_small(2, 0);
    Field(_v2, 0) = _v3[0];
    Field(_v2, 1) = _v3[1];
  End_roots()
  return _v2;
}

void camlidl_ml2c_libasmir_raw_inst_t(value _v1, raw_inst_t * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_libasmir_struct__raw_inst_t(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_libasmir_raw_inst_t(raw_inst_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_libasmir_struct__raw_inst_t(&(*_c2), _ctx);
  return _v1;
}

int camlidl_transl_table_libasmir_enum_2[72] = {
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

int camlidl_ml2c_libasmir_enum_bfd_architecture(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_2[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_bfd_architecture(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_2, 72, "enum bfd_architecture: bad enum bfd_architecture value");
  return _v2;
}

void camlidl_ml2c_libasmir_struct__vine_symbol_t(value _v1, struct _vine_symbol_t * _c2, camlidl_ctx _ctx)
{
  value _v3;
  value _v4;
  value _v5;
  value _v6;
  _v3 = Field(_v1, 0);
  (*_c2).name = camlidl_malloc_string(_v3, _ctx);
  _v4 = Field(_v1, 1);
  camlidl_ml2c_libasmir_address_t(_v4, &(*_c2).addr, _ctx);
  _v5 = Field(_v1, 2);
  (*_c2).is_function = Int_val(_v5);
  _v6 = Field(_v1, 3);
  (*_c2).is_dynamic = Int_val(_v6);
}

value camlidl_c2ml_libasmir_struct__vine_symbol_t(struct _vine_symbol_t * _c1, camlidl_ctx _ctx)
{
  value _v2;
  value _v3[4];
  _v3[0] = _v3[1] = _v3[2] = _v3[3] = 0;
  Begin_roots_block(_v3, 4)
    _v3[0] = copy_string((*_c1).name);
    _v3[1] = camlidl_c2ml_libasmir_address_t(&(*_c1).addr, _ctx);
    _v3[2] = Val_int((*_c1).is_function);
    _v3[3] = Val_int((*_c1).is_dynamic);
    _v2 = camlidl_alloc_small(4, 0);
    Field(_v2, 0) = _v3[0];
    Field(_v2, 1) = _v3[1];
    Field(_v2, 2) = _v3[2];
    Field(_v2, 3) = _v3[3];
  End_roots()
  return _v2;
}

void camlidl_ml2c_libasmir_vine_symbol_t(value _v1, vine_symbol_t * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_libasmir_struct__vine_symbol_t(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_libasmir_vine_symbol_t(vine_symbol_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_libasmir_struct__vine_symbol_t(&(*_c2), _ctx);
  return _v1;
}

void camlidl_ml2c_libasmir_struct__cflow_t(value _v1, struct _cflow_t * _c2, camlidl_ctx _ctx)
{
  value _v3;
  value _v4;
  _v3 = Field(_v1, 0);
  (*_c2).ctype = Int_val(_v3);
  _v4 = Field(_v1, 1);
  camlidl_ml2c_libasmir_address_t(_v4, &(*_c2).target, _ctx);
}

value camlidl_c2ml_libasmir_struct__cflow_t(struct _cflow_t * _c1, camlidl_ctx _ctx)
{
  value _v2;
  value _v3[2];
  _v3[0] = _v3[1] = 0;
  Begin_roots_block(_v3, 2)
    _v3[0] = Val_int((*_c1).ctype);
    _v3[1] = camlidl_c2ml_libasmir_address_t(&(*_c1).target, _ctx);
    _v2 = camlidl_alloc_small(2, 0);
    Field(_v2, 0) = _v3[0];
    Field(_v2, 1) = _v3[1];
  End_roots()
  return _v2;
}

void camlidl_ml2c_libasmir_cflow_t(value _v1, cflow_t * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_libasmir_struct__cflow_t(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_libasmir_cflow_t(cflow_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_libasmir_struct__cflow_t(&(*_c2), _ctx);
  return _v1;
}

void camlidl_ml2c_libasmir_vine_symbols_t(value _v1, vine_symbols_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((vine_symbols_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_vine_symbols_t(vine_symbols_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(vine_symbols_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((vine_symbols_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

int camlidl_transl_table_libasmir_enum_5[11] = {
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

int camlidl_ml2c_libasmir_enum_exp_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_5[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_exp_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_5, 11, "enum exp_type_t: bad enum exp_type_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_6[5] = {
  REG_1,
  REG_8,
  REG_16,
  REG_32,
  REG_64,
};

int camlidl_ml2c_libasmir_enum_reg_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_6[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_reg_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_6, 5, "enum reg_t: bad enum reg_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_7[23] = {
  PLUS,
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

int camlidl_ml2c_libasmir_enum_binop_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_7[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_binop_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_7, 23, "enum binop_type_t: bad enum binop_type_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_8[2] = {
  NEG,
  NOT,
};

int camlidl_ml2c_libasmir_enum_unop_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_8[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_unop_type_t(int _c1)
{
  value _v2;
  switch(_c1) {
  case NEG: _v2 = Val_int(0); break;
  case NOT: _v2 = Val_int(1); break;
  default: invalid_argument("enum unop_type_t: bad enum unop_type_t value");
  }
  return _v2;
}

void camlidl_ml2c_libasmir_const_val_t(value _v1, const_val_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int64_val(_v1);
}

value camlidl_c2ml_libasmir_const_val_t(const_val_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int64((*_c2));
  return _v1;
}

int camlidl_transl_table_libasmir_enum_9[8] = {
  CAST_UNSIGNED,
  CAST_SIGNED,
  CAST_HIGH,
  CAST_LOW,
  CAST_FLOAT,
  CAST_INTEGER,
  CAST_RFLOAT,
  CAST_RINTEGER,
};

int camlidl_ml2c_libasmir_enum_cast_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_9[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_cast_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_9, 8, "enum cast_t: bad enum cast_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_10[12] = {
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

int camlidl_ml2c_libasmir_enum_stmt_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_10[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_stmt_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_10, 12, "enum stmt_type_t: bad enum stmt_type_t value");
  return _v2;
}

value camlidl_libasmir_exp_type(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = exp_type(e);
  _vres = camlidl_c2ml_libasmir_enum_exp_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_binop_type(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = binop_type(e);
  _vres = camlidl_c2ml_libasmir_enum_binop_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_binop_lhs(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = binop_lhs(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_binop_rhs(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = binop_rhs(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unop_type(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unop_type(e);
  _vres = camlidl_c2ml_libasmir_enum_unop_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unop_subexp(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unop_subexp(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_mem_addr(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = mem_addr(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_mem_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = mem_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_constant_val(
	value _v_e)
{
  Exp e; /*in*/
  const_val_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = constant_val(e);
  _vres = camlidl_c2ml_libasmir_const_val_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_constant_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = constant_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_phi_phiname(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = phi_phiname(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_phi_numnodes(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = phi_numnodes(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_phi_nodeat(
	value _v_e,
	value _v_i)
{
  Exp e; /*in*/
  int i; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  i = Int_val(_v_i);
  _res = phi_nodeat(e, i);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_temp_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = temp_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_temp_name(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = temp_name(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unknown_str(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unknown_str(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cast_width(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = cast_width(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cast_casttype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = cast_casttype(e);
  _vres = camlidl_c2ml_libasmir_enum_cast_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cast_subexp(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = cast_subexp(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_name_string(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = name_string(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_let_var(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = let_var(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_let_exp(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = let_exp(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_let_in(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = let_in(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_stmt_type(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = stmt_type(s);
  _vres = camlidl_c2ml_libasmir_enum_stmt_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_move_lhs(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = move_lhs(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_move_rhs(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = move_rhs(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_label_string(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = label_string(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_special_string(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = special_string(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_comment_string(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = comment_string(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_jmp_target(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = jmp_target(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cjmp_cond(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = cjmp_cond(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cjmp_ttarget(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = cjmp_ttarget(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cjmp_ftarget(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = cjmp_ftarget(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_expstmt_exp(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = expstmt_exp(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_vardecl_name(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = vardecl_name(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_vardecl_type(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = vardecl_type(s);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_has_lval(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_has_lval(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_lval_opt(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_lval_opt(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_fnname(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_fnname(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_params(
	value _v_s)
{
  Stmt s; /*in*/
  Exp *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_params(s);
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Exp(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_ret_has_exp(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = ret_has_exp(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_ret_exp(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = ret_exp(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_name(
	value _v_s)
{
  Stmt s; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_name(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_has_rv(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_has_rv(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_rt(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_rt(s);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_params(
	value _v_s)
{
  Stmt s; /*in*/
  Stmt *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_params(s);
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Stmt(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_is_external(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_is_external(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_body(
	value _v_s)
{
  Stmt s; /*in*/
  Stmt *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_body(s);
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Stmt(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_assert_cond(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = assert_cond(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_gen_eflags_helpers_c(value _unit)
{
  Stmt *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _res = gen_eflags_helpers_c();
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Stmt(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_disassemble_program(
	value _v_filename)
{
  char *filename; /*in*/
  asm_program_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  _res = disassemble_program(filename);
  _vres = camlidl_c2ml_libasmir_asm_program_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_instmap_to_asm_program(
	value _v_instmap)
{
  instmap_t instmap; /*in*/
  asm_program_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_instmap_t(_v_instmap, &instmap, _ctx);
  _res = instmap_to_asm_program(instmap);
  _vres = camlidl_c2ml_libasmir_asm_program_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_filename_to_instmap(
	value _v_filename)
{
  char *filename; /*in*/
  instmap_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  _res = filename_to_instmap(filename);
  _vres = camlidl_c2ml_libasmir_instmap_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_instmap_translate_address_range(
	value _v_instmap,
	value _v_start_addr,
	value _v_end_addr)
{
  instmap_t instmap; /*in*/
  address_t start_addr; /*in*/
  address_t end_addr; /*in*/
  vine_blocks_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_instmap_t(_v_instmap, &instmap, _ctx);
  camlidl_ml2c_libasmir_address_t(_v_start_addr, &start_addr, _ctx);
  camlidl_ml2c_libasmir_address_t(_v_end_addr, &end_addr, _ctx);
  _res = instmap_translate_address_range(instmap, start_addr, end_addr);
  _vres = camlidl_c2ml_libasmir_vine_blocks_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_instmap_translate_address(
	value _v_instmap,
	value _v_addr)
{
  instmap_t instmap; /*in*/
  address_t addr; /*in*/
  vine_block_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_instmap_t(_v_instmap, &instmap, _ctx);
  camlidl_ml2c_libasmir_address_t(_v_addr, &addr, _ctx);
  _res = instmap_translate_address(instmap, addr);
  _vres = camlidl_c2ml_libasmir_vine_block_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_instmap_has_address(
	value _v_instmap,
	value _v_addr)
{
  instmap_t instmap; /*in*/
  address_t addr; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_instmap_t(_v_instmap, &instmap, _ctx);
  camlidl_ml2c_libasmir_address_t(_v_addr, &addr, _ctx);
  _res = instmap_has_address(instmap, addr);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_get_last_segment_address(
	value _v_filename,
	value _v_addr)
{
  char *filename; /*in*/
  address_t addr; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  camlidl_ml2c_libasmir_address_t(_v_addr, &addr, _ctx);
  _res = get_last_segment_address(filename, addr);
  _vres = camlidl_c2ml_libasmir_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_asmprogram_to_vine(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  vine_blocks_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  _res = asmir_asmprogram_to_vine(prog);
  _vres = camlidl_c2ml_libasmir_vine_blocks_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_vine_blocks_size(
	value _v_bs)
{
  vine_blocks_t bs; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_blocks_t(_v_bs, &bs, _ctx);
  _res = asmir_vine_blocks_size(bs);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_vine_blocks_get(
	value _v_bs,
	value _v_i)
{
  vine_blocks_t bs; /*in*/
  int i; /*in*/
  vine_block_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_blocks_t(_v_bs, &bs, _ctx);
  i = Int_val(_v_i);
  _res = asmir_vine_blocks_get(bs, i);
  _vres = camlidl_c2ml_libasmir_vine_block_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_destroy_vine_block(
	value _v_bs)
{
  vine_block_t bs; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_block_t(_v_bs, &bs, _ctx);
  destroy_vine_block(bs);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_destroy_vine_blocks(
	value _v_bs)
{
  vine_blocks_t bs; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_blocks_t(_v_bs, &bs, _ctx);
  destroy_vine_blocks(bs);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asmir_vine_block_size(
	value _v_b)
{
  vine_block_t b; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_block_t(_v_b, &b, _ctx);
  _res = asmir_vine_block_size(b);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_vine_block_address(
	value _v_b)
{
  vine_block_t b; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_block_t(_v_b, &b, _ctx);
  _res = asmir_vine_block_address(b);
  _vres = camlidl_c2ml_libasmir_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_vine_block_get(
	value _v_b,
	value _v_i)
{
  vine_block_t b; /*in*/
  int i; /*in*/
  Stmt _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_block_t(_v_b, &b, _ctx);
  i = Int_val(_v_i);
  _res = asmir_vine_block_get(b, i);
  _vres = camlidl_c2ml_libasmir_Stmt(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_free_asm_function(
	value _v_f)
{
  asm_function_t f; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_function_t(_v_f, &f, _ctx);
  free_asm_function(f);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_free_asm_program(
	value _v_p)
{
  asm_program_t p; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  free_asm_program(p);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asm_addr_to_ir(
	value _v_p,
	value _v_addr)
{
  asm_program_t p; /*in*/
  address_t addr; /*in*/
  vine_block_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  camlidl_ml2c_libasmir_address_t(_v_addr, &addr, _ctx);
  _res = asm_addr_to_ir(p, addr);
  _vres = camlidl_c2ml_libasmir_vine_block_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_string_blockinsn(
	value _v_prog,
	value _v_block)
{
  asm_program_t prog; /*in*/
  vine_block_t block; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  camlidl_ml2c_libasmir_vine_block_t(_v_block, &block, _ctx);
  _res = string_blockinsn(prog, block);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_get_synthetic_symbols(
	value _v_filename)
{
  char *filename; /*in*/
  dyn_functions_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  _res = get_synthetic_symbols(filename);
  _vres = camlidl_c2ml_libasmir_dyn_functions_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_dyn_functions_size(
	value _v_ds)
{
  dyn_functions_t ds; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_dyn_functions_t(_v_ds, &ds, _ctx);
  _res = dyn_functions_size(ds);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_dyn_functions_get(
	value _v_ds,
	value _v_i)
{
  dyn_functions_t ds; /*in*/
  int i; /*in*/
  dyn_function_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_dyn_functions_t(_v_ds, &ds, _ctx);
  i = Int_val(_v_i);
  _res = dyn_functions_get(ds, i);
  _vres = camlidl_c2ml_libasmir_dyn_function_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_dyn_functions_name(
	value _v_d)
{
  dyn_function_t d; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_dyn_function_t(_v_d, &d, _ctx);
  _res = dyn_functions_name(d);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  /* begin user-supplied deallocation sequence */
free(_res);
  /* end user-supplied deallocation sequence */
  return _vres;
}

value camlidl_libasmir_dyn_functions_addr(
	value _v_d)
{
  dyn_function_t d; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_dyn_function_t(_v_d, &d, _ctx);
  _res = dyn_functions_addr(d);
  _vres = camlidl_c2ml_libasmir_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_destroy_dyn_functions(
	value _v_ds)
{
  dyn_functions_t ds; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_dyn_functions_t(_v_ds, &ds, _ctx);
  destroy_dyn_functions(ds);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_get_symbols_of_file(
	value _v_filename)
{
  char *filename; /*in*/
  vine_symbols_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  _res = get_symbols_of_file(filename);
  _vres = camlidl_c2ml_libasmir_vine_symbols_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_symbols_size(
	value _v_ds)
{
  vine_symbols_t ds; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_symbols_t(_v_ds, &ds, _ctx);
  _res = symbols_size(ds);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_symbols_get(
	value _v_ds,
	value _v_i)
{
  vine_symbols_t ds; /*in*/
  int i; /*in*/
  vine_symbol_t *_res;
  value _v1;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_symbols_t(_v_ds, &ds, _ctx);
  i = Int_val(_v_i);
  _res = symbols_get(ds, i);
  if (_res == NULL) {
    _vres = Val_int(0);
  } else {
    _v1 = camlidl_c2ml_libasmir_vine_symbol_t(&*_res, _ctx);
    Begin_root(_v1)
      _vres = camlidl_alloc_small(1, 0);
      Field(_vres, 0) = _v1;
    End_roots();
  }
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_destroy_symbols(
	value _v_ds)
{
  vine_symbols_t ds; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_vine_symbols_t(_v_ds, &ds, _ctx);
  destroy_symbols(ds);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_memory_cell_data_address(
	value _v_md)
{
  memory_cell_data_t md; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_cell_data_t(_v_md, &md, _ctx);
  _res = memory_cell_data_address(md);
  _vres = camlidl_c2ml_libasmir_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_cell_data_value(
	value _v_md)
{
  memory_cell_data_t md; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_cell_data_t(_v_md, &md, _ctx);
  _res = memory_cell_data_value(md);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_get_rodata(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  memory_data_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  _res = get_rodata(prog);
  _vres = camlidl_c2ml_libasmir_memory_data_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_get_bssdata(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  memory_data_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  _res = get_bssdata(prog);
  _vres = camlidl_c2ml_libasmir_memory_data_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_data_size(
	value _v_md)
{
  memory_data_t md; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_data_t(_v_md, &md, _ctx);
  _res = memory_data_size(md);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_data_get(
	value _v_md,
	value _v_i)
{
  memory_data_t md; /*in*/
  int i; /*in*/
  memory_cell_data_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_data_t(_v_md, &md, _ctx);
  i = Int_val(_v_i);
  _res = memory_data_get(md, i);
  _vres = camlidl_c2ml_libasmir_memory_cell_data_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_destroy_memory_data(
	value _v_md)
{
  memory_data_t md; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_data_t(_v_md, &md, _ctx);
  destroy_memory_data(md);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_set_use_eflags_thunks(
	value _v_v)
{
  int v; /*in*/
  v = Int_val(_v_v);
  set_use_eflags_thunks(v);
  return Val_unit;
}

value camlidl_libasmir_get_use_eflags_thunks(value _unit)
{
  int _res;
  value _vres;

  _res = get_use_eflags_thunks();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_libasmir_set_call_return_translation(
	value _v_v)
{
  int v; /*in*/
  v = Int_val(_v_v);
  set_call_return_translation(v);
  return Val_unit;
}

value camlidl_libasmir_asmprogram_arch(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  _res = asmprogram_arch(prog);
  _vres = camlidl_c2ml_libasmir_enum_bfd_architecture(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_print_disasm_rawbytes(
	value _v_arch,
	value _v_addr,
	value _v_buf)
{
  int arch; /*in*/
  address_t addr; /*in*/
  char *buf; /*in*/
  int size; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  arch = camlidl_ml2c_libasmir_enum_bfd_architecture(_v_arch);
  camlidl_ml2c_libasmir_address_t(_v_addr, &addr, _ctx);
  _c1 = Wosize_val(_v_buf);
  buf = camlidl_malloc(_c1 * sizeof(char ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_buf, _c2);
    buf[_c2] = Int_val(_v3);
  }
  size = _c1;
  print_disasm_rawbytes(arch, addr, buf, size);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_byte_insn_to_asmp(
	value _v_arch,
	value _v_addr,
	value _v_bb_bytes)
{
  int arch; /*in*/
  address_t addr; /*in*/
  char *bb_bytes; /*in*/
  int len; /*in*/
  asm_program_t _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  arch = camlidl_ml2c_libasmir_enum_bfd_architecture(_v_arch);
  camlidl_ml2c_libasmir_address_t(_v_addr, &addr, _ctx);
  _c1 = Wosize_val(_v_bb_bytes);
  bb_bytes = camlidl_malloc(_c1 * sizeof(char ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_bb_bytes, _c2);
    bb_bytes[_c2] = Int_val(_v3);
  }
  len = _c1;
  _res = byte_insn_to_asmp(arch, addr, bb_bytes, len);
  _vres = camlidl_c2ml_libasmir_asm_program_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}


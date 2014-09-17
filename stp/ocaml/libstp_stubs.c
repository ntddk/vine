/* File generated from libstp.idl */

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


#include "libstp.h"

void camlidl_ml2c_libstp_Vc(value _v1, Vc * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Vc *) Bp_val(_v1));
}

value camlidl_c2ml_libstp_Vc(Vc * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Vc) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Vc *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libstp_Expr(value _v1, Expr * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Expr *) Bp_val(_v1));
}

value camlidl_c2ml_libstp_Expr(Expr * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Expr) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Expr *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libstp_ExprList(value _v1, ExprList * _c2, camlidl_ctx _ctx)
{
mlsize_t _c3;
mlsize_t _c4;
value _v5;
  _c3 = Wosize_val(_v1);
  (*_c2) = camlidl_malloc((_c3 + 1) * sizeof(Expr ), _ctx);
  for (_c4 = 0; _c4 < _c3; _c4++) {
    _v5 = Field(_v1, _c4);
    camlidl_ml2c_libstp_Expr(_v5, &(*_c2)[_c4], _ctx);
  }
  (*_c2)[_c3] = 0;
}

value camlidl_c2ml_libstp_ExprList(ExprList * _c2, camlidl_ctx _ctx)
{
value _v1;
mlsize_t _c3;
mlsize_t _c4;
value _v5;
  _c3 = camlidl_ptrarray_size((void **) (*_c2));
  _v1 = camlidl_alloc(_c3, 0);
  Begin_root(_v1)
    for (_c4 = 0; _c4 < _c3; _c4++) {
      _v5 = camlidl_c2ml_libstp_Expr(&(*_c2)[_c4], _ctx);
      modify(&Field(_v1, _c4), _v5);
    }
  End_roots()
  return _v1;
}

void camlidl_ml2c_libstp_Typ(value _v1, Typ * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Typ *) Bp_val(_v1));
}

value camlidl_c2ml_libstp_Typ(Typ * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Typ) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Typ *) Bp_val(_v1)) = *_c2;
  return _v1;
}

int camlidl_transl_table_libstp_enum_1[54] = {
  UNDEFINED,
  SYMBOL,
  BVCONST,
  BVNEG,
  BVCONCAT,
  BVOR,
  BVAND,
  BVXOR,
  BVNAND,
  BVNOR,
  BVXNOR,
  BVEXTRACT,
  BVLEFTSHIFT,
  BVRIGHTSHIFT,
  BVSRSHIFT,
  BVVARSHIFT,
  BVPLUS,
  BVSUB,
  BVUMINUS,
  BVMULTINVERSE,
  BVMULT,
  BVDIV,
  BVMOD,
  SBVDIV,
  SBVMOD,
  BVSX,
  BOOLVEC,
  ITE,
  BVGETBIT,
  BVLT,
  BVLE,
  BVGT,
  BVGE,
  BVSLT,
  BVSLE,
  BVSGT,
  BVSGE,
  EQ,
  NEQ,
  FALSE,
  TRUE,
  NOT,
  AND,
  OR,
  NAND,
  NOR,
  XOR,
  IFF,
  IMPLIES,
  READ,
  WRITE,
  ARRAY,
  BITVECTOR,
  BOOLEAN,
};

int camlidl_ml2c_libstp_enum_exprkind_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libstp_enum_1[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libstp_enum_exprkind_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libstp_enum_1, 54, "enum exprkind_t: bad enum exprkind_t value");
  return _v2;
}

int camlidl_transl_table_libstp_enum_2[4] = {
  BOOLEAN_TYPE,
  BITVECTOR_TYPE,
  ARRAY_TYPE,
  UNKNOWN_TYPE,
};

int camlidl_ml2c_libstp_enum_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libstp_enum_2[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libstp_enum_type_t(int _c1)
{
  value _v2;
  switch(_c1) {
  case BOOLEAN_TYPE: _v2 = Val_int(0); break;
  case BITVECTOR_TYPE: _v2 = Val_int(1); break;
  case ARRAY_TYPE: _v2 = Val_int(2); break;
  case UNKNOWN_TYPE: _v2 = Val_int(3); break;
  default: invalid_argument("enum type_t: bad enum type_t value");
  }
  return _v2;
}

value camlidl_libstp_vc_setFlags(
	value _v_c)
{
  char c; /*in*/
  c = Int_val(_v_c);
  vc_setFlags(c);
  return Val_unit;
}

value camlidl_libstp_vc_createValidityChecker(value _unit)
{
  Vc _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _res = vc_createValidityChecker();
  _vres = camlidl_c2ml_libstp_Vc(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_Destroy(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_Destroy(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_DeleteExpr(
	value _v_e)
{
  Expr e; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  vc_DeleteExpr(e);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_boolType(
	value _v_vc)
{
  Vc vc; /*in*/
  Typ _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = vc_boolType(vc);
  _vres = camlidl_c2ml_libstp_Typ(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_arrayType(
	value _v_vc,
	value _v_typeIndex,
	value _v_typeData)
{
  Vc vc; /*in*/
  Typ typeIndex; /*in*/
  Typ typeData; /*in*/
  Typ _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Typ(_v_typeIndex, &typeIndex, _ctx);
  camlidl_ml2c_libstp_Typ(_v_typeData, &typeData, _ctx);
  _res = vc_arrayType(vc, typeIndex, typeData);
  _vres = camlidl_c2ml_libstp_Typ(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_varExpr(
	value _v_vc,
	value _v_name,
	value _v_type)
{
  Vc vc; /*in*/
  char *name; /*in*/
  Typ type; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  name = String_val(_v_name);
  camlidl_ml2c_libstp_Typ(_v_type, &type, _ctx);
  _res = vc_varExpr(vc, name, type);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_varExpr1(
	value _v_vc,
	value _v_name,
	value _v_indexwidth,
	value _v_valuewidth)
{
  Vc vc; /*in*/
  char *name; /*in*/
  int indexwidth; /*in*/
  int valuewidth; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  name = String_val(_v_name);
  indexwidth = Int_val(_v_indexwidth);
  valuewidth = Int_val(_v_valuewidth);
  _res = vc_varExpr1(vc, name, indexwidth, valuewidth);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_eqExpr(
	value _v_vc,
	value _v_child0,
	value _v_child1)
{
  Vc vc; /*in*/
  Expr child0; /*in*/
  Expr child1; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child0, &child0, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child1, &child1, _ctx);
  _res = vc_eqExpr(vc, child0, child1);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_trueExpr(
	value _v_vc)
{
  Vc vc; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = vc_trueExpr(vc);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_falseExpr(
	value _v_vc)
{
  Vc vc; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = vc_falseExpr(vc);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_notExpr(
	value _v_vc,
	value _v_child)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_notExpr(vc, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_andExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_andExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_andExprN(
	value _v_vc,
	value _v_children,
	value _v_numOfChildNodes)
{
  Vc vc; /*in*/
  Expr *children; /*in*/
  int numOfChildNodes; /*in*/
  Expr _res;
  value _v1;
  Expr _c2;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  if (_v_children == Val_int(0)) {
    children = NULL;
  } else {
    _v1 = Field(_v_children, 0);
    children = &_c2;
    camlidl_ml2c_libstp_Expr(_v1, &_c2, _ctx);
  }
  numOfChildNodes = Int_val(_v_numOfChildNodes);
  _res = vc_andExprN(vc, children, numOfChildNodes);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_orExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_orExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_orExprN(
	value _v_vc,
	value _v_children,
	value _v_numOfChildNodes)
{
  Vc vc; /*in*/
  Expr *children; /*in*/
  int numOfChildNodes; /*in*/
  Expr _res;
  value _v1;
  Expr _c2;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  if (_v_children == Val_int(0)) {
    children = NULL;
  } else {
    _v1 = Field(_v_children, 0);
    children = &_c2;
    camlidl_ml2c_libstp_Expr(_v1, &_c2, _ctx);
  }
  numOfChildNodes = Int_val(_v_numOfChildNodes);
  _res = vc_orExprN(vc, children, numOfChildNodes);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_impliesExpr(
	value _v_vc,
	value _v_hyp,
	value _v_conc)
{
  Vc vc; /*in*/
  Expr hyp; /*in*/
  Expr conc; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_hyp, &hyp, _ctx);
  camlidl_ml2c_libstp_Expr(_v_conc, &conc, _ctx);
  _res = vc_impliesExpr(vc, hyp, conc);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_iffExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_iffExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_iteExpr(
	value _v_vc,
	value _v_ifpart,
	value _v_thenpart,
	value _v_elsepart)
{
  Vc vc; /*in*/
  Expr ifpart; /*in*/
  Expr thenpart; /*in*/
  Expr elsepart; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_ifpart, &ifpart, _ctx);
  camlidl_ml2c_libstp_Expr(_v_thenpart, &thenpart, _ctx);
  camlidl_ml2c_libstp_Expr(_v_elsepart, &elsepart, _ctx);
  _res = vc_iteExpr(vc, ifpart, thenpart, elsepart);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_boolToBVExpr(
	value _v_vc,
	value _v_form)
{
  Vc vc; /*in*/
  Expr form; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_form, &form, _ctx);
  _res = vc_boolToBVExpr(vc, form);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_readExpr(
	value _v_vc,
	value _v_array,
	value _v_index)
{
  Vc vc; /*in*/
  Expr array; /*in*/
  Expr index; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_array, &array, _ctx);
  camlidl_ml2c_libstp_Expr(_v_index, &index, _ctx);
  _res = vc_readExpr(vc, array, index);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_writeExpr(
	value _v_vc,
	value _v_array,
	value _v_index,
	value _v_newValue)
{
  Vc vc; /*in*/
  Expr array; /*in*/
  Expr index; /*in*/
  Expr newValue; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_array, &array, _ctx);
  camlidl_ml2c_libstp_Expr(_v_index, &index, _ctx);
  camlidl_ml2c_libstp_Expr(_v_newValue, &newValue, _ctx);
  _res = vc_writeExpr(vc, array, index, newValue);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_printExpr(
	value _v_vc,
	value _v_e)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  vc_printExpr(vc, e);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printExprFile(
	value _v_vc,
	value _v_e,
	value _v_fd)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  int fd; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  fd = Int_val(_v_fd);
  vc_printExprFile(vc, e, fd);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printExprToBuffer(
	value _v_vc,
	value _v_e,
	value _v_buf,
	value _v_len)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  char **buf; /*in*/
  unsigned long *len; /*in*/
  value _v1;
  char *_c2;
  value _v3;
  char _c4;
  value _v5;
  unsigned long _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  if (_v_buf == Val_int(0)) {
    buf = NULL;
  } else {
    _v1 = Field(_v_buf, 0);
    buf = &_c2;
    if (_v1 == Val_int(0)) {
      _c2 = NULL;
    } else {
      _v3 = Field(_v1, 0);
      _c2 = &_c4;
      _c4 = Int_val(_v3);
    }
  }
  if (_v_len == Val_int(0)) {
    len = NULL;
  } else {
    _v5 = Field(_v_len, 0);
    len = &_c6;
    _c6 = Long_val(_v5);
  }
  vc_printExprToBuffer(vc, e, buf, len);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printCounterExample(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_printCounterExample(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printVarDecls(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_printVarDecls(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printAsserts(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_printAsserts(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printQueryStateToBuffer(
	value _v_vc,
	value _v_e,
	value _v_buf,
	value _v_len)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  char **buf; /*in*/
  unsigned long *len; /*in*/
  value _v1;
  char *_c2;
  value _v3;
  char _c4;
  value _v5;
  unsigned long _c6;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  if (_v_buf == Val_int(0)) {
    buf = NULL;
  } else {
    _v1 = Field(_v_buf, 0);
    buf = &_c2;
    if (_v1 == Val_int(0)) {
      _c2 = NULL;
    } else {
      _v3 = Field(_v1, 0);
      _c2 = &_c4;
      _c4 = Int_val(_v3);
    }
  }
  if (_v_len == Val_int(0)) {
    len = NULL;
  } else {
    _v5 = Field(_v_len, 0);
    len = &_c6;
    _c6 = Long_val(_v5);
  }
  vc_printQueryStateToBuffer(vc, e, buf, len);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_printQuery(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_printQuery(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_assertFormula(
	value _v_vc,
	value _v_e)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  vc_assertFormula(vc, e);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_simplify(
	value _v_vc,
	value _v_e)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = vc_simplify(vc, e);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_query(
	value _v_vc,
	value _v_e)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = vc_query(vc, e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_getCounterExample(
	value _v_vc,
	value _v_e)
{
  Vc vc; /*in*/
  Expr e; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = vc_getCounterExample(vc, e);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_counterexample_size(
	value _v_vc)
{
  Vc vc; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = vc_counterexample_size(vc);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_push(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_push(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_vc_pop(
	value _v_vc)
{
  Vc vc; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  vc_pop(vc);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_getBVInt(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getBVInt(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getBVUnsigned(
	value _v_e)
{
  Expr e; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getBVUnsigned(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getBVUnsignedLongLong(
	value _v_e)
{
  Expr e; /*in*/
  unsigned long long _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getBVUnsignedLongLong(e);
  _vres = copy_int64(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvType(
	value _v_vc,
	value _v_no_bits)
{
  Vc vc; /*in*/
  int no_bits; /*in*/
  Typ _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  no_bits = Int_val(_v_no_bits);
  _res = vc_bvType(vc, no_bits);
  _vres = camlidl_c2ml_libstp_Typ(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32Type(
	value _v_vc)
{
  Vc vc; /*in*/
  Typ _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = vc_bv32Type(vc);
  _vres = camlidl_c2ml_libstp_Typ(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvConstExprFromStr(
	value _v_vc,
	value _v_binary_repr)
{
  Vc vc; /*in*/
  char *binary_repr; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  binary_repr = String_val(_v_binary_repr);
  _res = vc_bvConstExprFromStr(vc, binary_repr);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvConstExprFromInt(
	value _v_vc,
	value _v_n_bits,
	value _v_val)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  unsigned int val; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  val = Int_val(_v_val);
  _res = vc_bvConstExprFromInt(vc, n_bits, val);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32ConstExprFromInt(
	value _v_vc,
	value _v_val)
{
  Vc vc; /*in*/
  unsigned int val; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  val = Int32_val(_v_val);
  _res = vc_bv32ConstExprFromInt(vc, val);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvConstExprFromLL(
	value _v_vc,
	value _v_n_bits,
	value _v_val)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  unsigned long long val; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  val = Int64_val(_v_val);
  _res = vc_bvConstExprFromLL(vc, n_bits, val);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvConcatExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvConcatExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvPlusExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvPlusExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32PlusExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bv32PlusExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvMinusExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvMinusExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32MinusExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bv32MinusExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvMultExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvMultExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32MultExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bv32MultExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvDivExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvDivExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvModExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvModExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_sbvDivExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_sbvDivExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_sbvModExpr(
	value _v_vc,
	value _v_n_bits,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  int n_bits; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  n_bits = Int_val(_v_n_bits);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_sbvModExpr(vc, n_bits, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvLtExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvLtExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvLeExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvLeExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvGtExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvGtExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvGeExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvGeExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_sbvLtExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_sbvLtExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_sbvLeExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_sbvLeExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_sbvGtExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_sbvGtExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_sbvGeExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_sbvGeExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvUMinusExpr(
	value _v_vc,
	value _v_child)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bvUMinusExpr(vc, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvAndExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvAndExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvOrExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvOrExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvXorExpr(
	value _v_vc,
	value _v_left,
	value _v_right)
{
  Vc vc; /*in*/
  Expr left; /*in*/
  Expr right; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_left, &left, _ctx);
  camlidl_ml2c_libstp_Expr(_v_right, &right, _ctx);
  _res = vc_bvXorExpr(vc, left, right);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvNotExpr(
	value _v_vc,
	value _v_child)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bvNotExpr(vc, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvLeftShiftExpr(
	value _v_vc,
	value _v_sh_amt,
	value _v_child)
{
  Vc vc; /*in*/
  int sh_amt; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  sh_amt = Int_val(_v_sh_amt);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bvLeftShiftExpr(vc, sh_amt, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvRightShiftExpr(
	value _v_vc,
	value _v_sh_amt,
	value _v_child)
{
  Vc vc; /*in*/
  int sh_amt; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  sh_amt = Int_val(_v_sh_amt);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bvRightShiftExpr(vc, sh_amt, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32LeftShiftExpr(
	value _v_vc,
	value _v_sh_amt,
	value _v_child)
{
  Vc vc; /*in*/
  int sh_amt; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  sh_amt = Int_val(_v_sh_amt);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bv32LeftShiftExpr(vc, sh_amt, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bv32RightShiftExpr(
	value _v_vc,
	value _v_sh_amt,
	value _v_child)
{
  Vc vc; /*in*/
  int sh_amt; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  sh_amt = Int_val(_v_sh_amt);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bv32RightShiftExpr(vc, sh_amt, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvVar32LeftShiftExpr(
	value _v_vc,
	value _v_sh_amt,
	value _v_child)
{
  Vc vc; /*in*/
  Expr sh_amt; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_sh_amt, &sh_amt, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bvVar32LeftShiftExpr(vc, sh_amt, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvVar32RightShiftExpr(
	value _v_vc,
	value _v_sh_amt,
	value _v_child)
{
  Vc vc; /*in*/
  Expr sh_amt; /*in*/
  Expr child; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_sh_amt, &sh_amt, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  _res = vc_bvVar32RightShiftExpr(vc, sh_amt, child);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvVar32DivByPowOfTwoExpr(
	value _v_vc,
	value _v_child,
	value _v_rhs)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  Expr rhs; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  camlidl_ml2c_libstp_Expr(_v_rhs, &rhs, _ctx);
  _res = vc_bvVar32DivByPowOfTwoExpr(vc, child, rhs);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvExtract(
	value _v_vc,
	value _v_child,
	value _v_high_bit_no,
	value _v_low_bit_no)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  int high_bit_no; /*in*/
  int low_bit_no; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  high_bit_no = Int_val(_v_high_bit_no);
  low_bit_no = Int_val(_v_low_bit_no);
  _res = vc_bvExtract(vc, child, high_bit_no, low_bit_no);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvBoolExtract(
	value _v_vc,
	value _v_child,
	value _v_bit_no)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  int bit_no; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  bit_no = Int_val(_v_bit_no);
  _res = vc_bvBoolExtract(vc, child, bit_no);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvSignExtend(
	value _v_vc,
	value _v_child,
	value _v_nbits)
{
  Vc vc; /*in*/
  Expr child; /*in*/
  int nbits; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_child, &child, _ctx);
  nbits = Int_val(_v_nbits);
  _res = vc_bvSignExtend(vc, child, nbits);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvCreateMemoryArray(
	value _v_vc,
	value _v_arrayName)
{
  Vc vc; /*in*/
  char *arrayName; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  arrayName = String_val(_v_arrayName);
  _res = vc_bvCreateMemoryArray(vc, arrayName);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvReadMemoryArray(
	value _v_vc,
	value _v_array,
	value _v_byteIndex,
	value _v_numOfBytes)
{
  Vc vc; /*in*/
  Expr array; /*in*/
  Expr byteIndex; /*in*/
  int numOfBytes; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_array, &array, _ctx);
  camlidl_ml2c_libstp_Expr(_v_byteIndex, &byteIndex, _ctx);
  numOfBytes = Int_val(_v_numOfBytes);
  _res = vc_bvReadMemoryArray(vc, array, byteIndex, numOfBytes);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_bvWriteToMemoryArray(
	value _v_vc,
	value _v_array,
	value _v_byteIndex,
	value _v_element,
	value _v_numOfBytes)
{
  Vc vc; /*in*/
  Expr array; /*in*/
  Expr byteIndex; /*in*/
  Expr element; /*in*/
  int numOfBytes; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  camlidl_ml2c_libstp_Expr(_v_array, &array, _ctx);
  camlidl_ml2c_libstp_Expr(_v_byteIndex, &byteIndex, _ctx);
  camlidl_ml2c_libstp_Expr(_v_element, &element, _ctx);
  numOfBytes = Int_val(_v_numOfBytes);
  _res = vc_bvWriteToMemoryArray(vc, array, byteIndex, element, numOfBytes);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_exprString(
	value _v_e)
{
  Expr e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = exprString(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_typeString(
	value _v_t)
{
  Typ t; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Typ(_v_t, &t, _ctx);
  _res = typeString(t);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getChild(
	value _v_e,
	value _v_i)
{
  Expr e; /*in*/
  int i; /*in*/
  Expr _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  i = Int_val(_v_i);
  _res = getChild(e, i);
  _vres = camlidl_c2ml_libstp_Expr(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_libstp_regerrorhandler(value _unit)
{
  libstp_regerrorhandler();
  return Val_unit;
}

value camlidl_libstp_getExprKind(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getExprKind(e);
  _vres = camlidl_c2ml_libstp_enum_exprkind_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getDegree(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getDegree(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getBVLength(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getBVLength(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getType(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getType(e);
  _vres = camlidl_c2ml_libstp_enum_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getVWidth(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getVWidth(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getIWidth(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getIWidth(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_vc_printCounterExampleFile(
	value _v_vc,
	value _v_fd)
{
  Vc vc; /*in*/
  int fd; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  fd = Int_val(_v_fd);
  vc_printCounterExampleFile(vc, fd);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libstp_exprName(
	value _v_e)
{
  Expr e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = exprName(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_getExprID(
	value _v_e)
{
  Expr e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Expr(_v_e, &e, _ctx);
  _res = getExprID(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libstp_GetTrueCounterExampleFst(
	value _v_vc)
{
  Vc vc; /*in*/
  ExprList _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = GetTrueCounterExampleFst(vc);
  _vres = camlidl_c2ml_libstp_ExprList(&_res, _ctx);
  camlidl_free(_ctx);
  /* begin user-supplied deallocation sequence */
free(_res);
  /* end user-supplied deallocation sequence */
  return _vres;
}

value camlidl_libstp_GetTrueCounterExampleSnd(
	value _v_vc)
{
  Vc vc; /*in*/
  ExprList _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libstp_Vc(_v_vc, &vc, _ctx);
  _res = GetTrueCounterExampleSnd(vc);
  _vres = camlidl_c2ml_libstp_ExprList(&_res, _ctx);
  camlidl_free(_ctx);
  /* begin user-supplied deallocation sequence */
free(_res);
  /* end user-supplied deallocation sequence */
  return _vres;
}


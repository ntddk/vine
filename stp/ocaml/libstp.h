/* File generated from libstp.idl */

#ifndef _CAMLIDL_LIBSTP_H
#define _CAMLIDL_LIBSTP_H

#ifdef __cplusplus
#define _CAMLIDL_EXTERN_C extern "C"
#else
#define _CAMLIDL_EXTERN_C extern
#endif

#ifdef _WIN32
#pragma pack(push,8) /* necessary for COM interfaces */
#endif

typedef void *Vc;

typedef void *Expr;

typedef Expr *ExprList;

typedef void *Typ;

enum exprkind_t {
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

enum type_t {
BOOLEAN_TYPE = 0,
BITVECTOR_TYPE,
ARRAY_TYPE,
UNKNOWN_TYPE,
};

_CAMLIDL_EXTERN_C void vc_setFlags(/*in*/ char c);

_CAMLIDL_EXTERN_C Vc vc_createValidityChecker(void);

_CAMLIDL_EXTERN_C void vc_Destroy(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_DeleteExpr(/*in*/ Expr e);

_CAMLIDL_EXTERN_C Typ vc_boolType(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C Typ vc_arrayType(/*in*/ Vc vc, /*in*/ Typ typeIndex, /*in*/ Typ typeData);

_CAMLIDL_EXTERN_C Expr vc_varExpr(/*in*/ Vc vc, /*in*/ char *name, /*in*/ Typ type);

_CAMLIDL_EXTERN_C Expr vc_varExpr1(/*in*/ Vc vc, /*in*/ char *name, /*in*/ int indexwidth, /*in*/ int valuewidth);

_CAMLIDL_EXTERN_C Expr vc_eqExpr(/*in*/ Vc vc, /*in*/ Expr child0, /*in*/ Expr child1);

_CAMLIDL_EXTERN_C Expr vc_trueExpr(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C Expr vc_falseExpr(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C Expr vc_notExpr(/*in*/ Vc vc, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_andExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_andExprN(/*in*/ Vc vc, /*in*/ Expr *children, /*in*/ int numOfChildNodes);

_CAMLIDL_EXTERN_C Expr vc_orExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_orExprN(/*in*/ Vc vc, /*in*/ Expr *children, /*in*/ int numOfChildNodes);

_CAMLIDL_EXTERN_C Expr vc_impliesExpr(/*in*/ Vc vc, /*in*/ Expr hyp, /*in*/ Expr conc);

_CAMLIDL_EXTERN_C Expr vc_iffExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_iteExpr(/*in*/ Vc vc, /*in*/ Expr ifpart, /*in*/ Expr thenpart, /*in*/ Expr elsepart);

_CAMLIDL_EXTERN_C Expr vc_boolToBVExpr(/*in*/ Vc vc, /*in*/ Expr form);

_CAMLIDL_EXTERN_C Expr vc_readExpr(/*in*/ Vc vc, /*in*/ Expr array, /*in*/ Expr index);

_CAMLIDL_EXTERN_C Expr vc_writeExpr(/*in*/ Vc vc, /*in*/ Expr array, /*in*/ Expr index, /*in*/ Expr newValue);

_CAMLIDL_EXTERN_C void vc_printExpr(/*in*/ Vc vc, /*in*/ Expr e);

_CAMLIDL_EXTERN_C void vc_printExprFile(/*in*/ Vc vc, /*in*/ Expr e, /*in*/ int fd);

_CAMLIDL_EXTERN_C void vc_printExprToBuffer(/*in*/ Vc vc, /*in*/ Expr e, /*in*/ char **buf, /*in*/ unsigned long *len);

_CAMLIDL_EXTERN_C void vc_printCounterExample(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_printVarDecls(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_printAsserts(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_printQueryStateToBuffer(/*in*/ Vc vc, /*in*/ Expr e, /*in*/ char **buf, /*in*/ unsigned long *len);

_CAMLIDL_EXTERN_C void vc_printQuery(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_assertFormula(/*in*/ Vc vc, /*in*/ Expr e);

_CAMLIDL_EXTERN_C Expr vc_simplify(/*in*/ Vc vc, /*in*/ Expr e);

_CAMLIDL_EXTERN_C int vc_query(/*in*/ Vc vc, /*in*/ Expr e);

_CAMLIDL_EXTERN_C Expr vc_getCounterExample(/*in*/ Vc vc, /*in*/ Expr e);

_CAMLIDL_EXTERN_C int vc_counterexample_size(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_push(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C void vc_pop(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C int getBVInt(/*in*/ Expr e);

_CAMLIDL_EXTERN_C unsigned int getBVUnsigned(/*in*/ Expr e);

_CAMLIDL_EXTERN_C unsigned long long getBVUnsignedLongLong(/*in*/ Expr e);

_CAMLIDL_EXTERN_C Typ vc_bvType(/*in*/ Vc vc, /*in*/ int no_bits);

_CAMLIDL_EXTERN_C Typ vc_bv32Type(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C Expr vc_bvConstExprFromStr(/*in*/ Vc vc, /*in*/ char *binary_repr);

_CAMLIDL_EXTERN_C Expr vc_bvConstExprFromInt(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ unsigned int val);

_CAMLIDL_EXTERN_C Expr vc_bv32ConstExprFromInt(/*in*/ Vc vc, /*in*/ unsigned int val);

_CAMLIDL_EXTERN_C Expr vc_bvConstExprFromLL(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ unsigned long long val);

_CAMLIDL_EXTERN_C Expr vc_bvConcatExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvPlusExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bv32PlusExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvMinusExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bv32MinusExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvMultExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bv32MultExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvDivExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvModExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_sbvDivExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_sbvModExpr(/*in*/ Vc vc, /*in*/ int n_bits, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvLtExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvLeExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvGtExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvGeExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_sbvLtExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_sbvLeExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_sbvGtExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_sbvGeExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvUMinusExpr(/*in*/ Vc vc, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bvAndExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvOrExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvXorExpr(/*in*/ Vc vc, /*in*/ Expr left, /*in*/ Expr right);

_CAMLIDL_EXTERN_C Expr vc_bvNotExpr(/*in*/ Vc vc, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bvLeftShiftExpr(/*in*/ Vc vc, /*in*/ int sh_amt, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bvRightShiftExpr(/*in*/ Vc vc, /*in*/ int sh_amt, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bv32LeftShiftExpr(/*in*/ Vc vc, /*in*/ int sh_amt, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bv32RightShiftExpr(/*in*/ Vc vc, /*in*/ int sh_amt, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bvVar32LeftShiftExpr(/*in*/ Vc vc, /*in*/ Expr sh_amt, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bvVar32RightShiftExpr(/*in*/ Vc vc, /*in*/ Expr sh_amt, /*in*/ Expr child);

_CAMLIDL_EXTERN_C Expr vc_bvVar32DivByPowOfTwoExpr(/*in*/ Vc vc, /*in*/ Expr child, /*in*/ Expr rhs);

_CAMLIDL_EXTERN_C Expr vc_bvExtract(/*in*/ Vc vc, /*in*/ Expr child, /*in*/ int high_bit_no, /*in*/ int low_bit_no);

_CAMLIDL_EXTERN_C Expr vc_bvBoolExtract(/*in*/ Vc vc, /*in*/ Expr child, /*in*/ int bit_no);

_CAMLIDL_EXTERN_C Expr vc_bvSignExtend(/*in*/ Vc vc, /*in*/ Expr child, /*in*/ int nbits);

_CAMLIDL_EXTERN_C Expr vc_bvCreateMemoryArray(/*in*/ Vc vc, /*in*/ char *arrayName);

_CAMLIDL_EXTERN_C Expr vc_bvReadMemoryArray(/*in*/ Vc vc, /*in*/ Expr array, /*in*/ Expr byteIndex, /*in*/ int numOfBytes);

_CAMLIDL_EXTERN_C Expr vc_bvWriteToMemoryArray(/*in*/ Vc vc, /*in*/ Expr array, /*in*/ Expr byteIndex, /*in*/ Expr element, /*in*/ int numOfBytes);

_CAMLIDL_EXTERN_C char *exprString(/*in*/ Expr e);

_CAMLIDL_EXTERN_C char *typeString(/*in*/ Typ t);

_CAMLIDL_EXTERN_C Expr getChild(/*in*/ Expr e, /*in*/ int i);

_CAMLIDL_EXTERN_C void libstp_regerrorhandler(void);

_CAMLIDL_EXTERN_C int getExprKind(/*in*/ Expr e);

_CAMLIDL_EXTERN_C int getDegree(/*in*/ Expr e);

_CAMLIDL_EXTERN_C int getBVLength(/*in*/ Expr e);

_CAMLIDL_EXTERN_C int getType(/*in*/ Expr e);

_CAMLIDL_EXTERN_C int getVWidth(/*in*/ Expr e);

_CAMLIDL_EXTERN_C int getIWidth(/*in*/ Expr e);

_CAMLIDL_EXTERN_C void vc_printCounterExampleFile(/*in*/ Vc vc, /*in*/ int fd);

_CAMLIDL_EXTERN_C char *exprName(/*in*/ Expr e);

_CAMLIDL_EXTERN_C int getExprID(/*in*/ Expr e);

_CAMLIDL_EXTERN_C ExprList GetTrueCounterExampleFst(/*in*/ Vc vc);

_CAMLIDL_EXTERN_C ExprList GetTrueCounterExampleSnd(/*in*/ Vc vc);

#ifdef _WIN32
#pragma pack(pop)
#endif


#endif /* !_CAMLIDL_LIBSTP_H */

(* File generated from libstp.idl *)

type vc
and expr
and exprList = expr array
and typ
and exprkind_t =
  | UNDEFINED
  | SYMBOL
  | BVCONST
  | BVNEG
  | BVCONCAT
  | BVOR
  | BVAND
  | BVXOR
  | BVNAND
  | BVNOR
  | BVXNOR
  | BVEXTRACT
  | BVLEFTSHIFT
  | BVRIGHTSHIFT
  | BVSRSHIFT
  | BVVARSHIFT
  | BVPLUS
  | BVSUB
  | BVUMINUS
  | BVMULTINVERSE
  | BVMULT
  | BVDIV
  | BVMOD
  | SBVDIV
  | SBVMOD
  | BVSX
  | BOOLVEC
  | ITE
  | BVGETBIT
  | BVLT
  | BVLE
  | BVGT
  | BVGE
  | BVSLT
  | BVSLE
  | BVSGT
  | BVSGE
  | EQ
  | NEQ
  | FALSE
  | TRUE
  | NOT
  | AND
  | OR
  | NAND
  | NOR
  | XOR
  | IFF
  | IMPLIES
  | READ
  | WRITE
  | ARRAY
  | BITVECTOR
  | BOOLEAN
and type_t =
  | BOOLEAN_TYPE
  | BITVECTOR_TYPE
  | ARRAY_TYPE
  | UNKNOWN_TYPE

external vc_setFlags : char -> unit
	= "camlidl_libstp_vc_setFlags"

external vc_createValidityChecker : unit -> vc
	= "camlidl_libstp_vc_createValidityChecker"

external vc_Destroy : vc -> unit
	= "camlidl_libstp_vc_Destroy"

external vc_DeleteExpr : expr -> unit
	= "camlidl_libstp_vc_DeleteExpr"

external vc_boolType : vc -> typ
	= "camlidl_libstp_vc_boolType"

external vc_arrayType : vc -> typ -> typ -> typ
	= "camlidl_libstp_vc_arrayType"

external vc_varExpr : vc -> string -> typ -> expr
	= "camlidl_libstp_vc_varExpr"

external vc_varExpr1 : vc -> string -> int -> int -> expr
	= "camlidl_libstp_vc_varExpr1"

external vc_eqExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_eqExpr"

external vc_trueExpr : vc -> expr
	= "camlidl_libstp_vc_trueExpr"

external vc_falseExpr : vc -> expr
	= "camlidl_libstp_vc_falseExpr"

external vc_notExpr : vc -> expr -> expr
	= "camlidl_libstp_vc_notExpr"

external vc_andExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_andExpr"

external vc_andExprN : vc -> expr option -> int -> expr
	= "camlidl_libstp_vc_andExprN"

external vc_orExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_orExpr"

external vc_orExprN : vc -> expr option -> int -> expr
	= "camlidl_libstp_vc_orExprN"

external vc_impliesExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_impliesExpr"

external vc_iffExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_iffExpr"

external vc_iteExpr : vc -> expr -> expr -> expr -> expr
	= "camlidl_libstp_vc_iteExpr"

external vc_boolToBVExpr : vc -> expr -> expr
	= "camlidl_libstp_vc_boolToBVExpr"

external vc_readExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_readExpr"

external vc_writeExpr : vc -> expr -> expr -> expr -> expr
	= "camlidl_libstp_vc_writeExpr"

external vc_printExpr : vc -> expr -> unit
	= "camlidl_libstp_vc_printExpr"

external vc_printExprFile : vc -> expr -> int -> unit
	= "camlidl_libstp_vc_printExprFile"

external vc_printExprToBuffer : vc -> expr -> char option option -> int option -> unit
	= "camlidl_libstp_vc_printExprToBuffer"

external vc_printCounterExample : vc -> unit
	= "camlidl_libstp_vc_printCounterExample"

external vc_printVarDecls : vc -> unit
	= "camlidl_libstp_vc_printVarDecls"

external vc_printAsserts : vc -> unit
	= "camlidl_libstp_vc_printAsserts"

external vc_printQueryStateToBuffer : vc -> expr -> char option option -> int option -> unit
	= "camlidl_libstp_vc_printQueryStateToBuffer"

external vc_printQuery : vc -> unit
	= "camlidl_libstp_vc_printQuery"

external vc_assertFormula : vc -> expr -> unit
	= "camlidl_libstp_vc_assertFormula"

external vc_simplify : vc -> expr -> expr
	= "camlidl_libstp_vc_simplify"

external vc_query : vc -> expr -> int
	= "camlidl_libstp_vc_query"

external vc_getCounterExample : vc -> expr -> expr
	= "camlidl_libstp_vc_getCounterExample"

external vc_counterexample_size : vc -> int
	= "camlidl_libstp_vc_counterexample_size"

external vc_push : vc -> unit
	= "camlidl_libstp_vc_push"

external vc_pop : vc -> unit
	= "camlidl_libstp_vc_pop"

external getBVInt : expr -> int
	= "camlidl_libstp_getBVInt"

external getBVUnsigned : expr -> int
	= "camlidl_libstp_getBVUnsigned"

external getBVUnsignedLongLong : expr -> int64
	= "camlidl_libstp_getBVUnsignedLongLong"

external vc_bvType : vc -> int -> typ
	= "camlidl_libstp_vc_bvType"

external vc_bv32Type : vc -> typ
	= "camlidl_libstp_vc_bv32Type"

external vc_bvConstExprFromStr : vc -> string -> expr
	= "camlidl_libstp_vc_bvConstExprFromStr"

external vc_bvConstExprFromInt : vc -> int -> int -> expr
	= "camlidl_libstp_vc_bvConstExprFromInt"

external vc_bv32ConstExprFromInt : vc -> int32 -> expr
	= "camlidl_libstp_vc_bv32ConstExprFromInt"

external vc_bvConstExprFromLL : vc -> int -> int64 -> expr
	= "camlidl_libstp_vc_bvConstExprFromLL"

external vc_bvConcatExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvConcatExpr"

external vc_bvPlusExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvPlusExpr"

external vc_bv32PlusExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bv32PlusExpr"

external vc_bvMinusExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvMinusExpr"

external vc_bv32MinusExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bv32MinusExpr"

external vc_bvMultExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvMultExpr"

external vc_bv32MultExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bv32MultExpr"

external vc_bvDivExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvDivExpr"

external vc_bvModExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvModExpr"

external vc_sbvDivExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_sbvDivExpr"

external vc_sbvModExpr : vc -> int -> expr -> expr -> expr
	= "camlidl_libstp_vc_sbvModExpr"

external vc_bvLtExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvLtExpr"

external vc_bvLeExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvLeExpr"

external vc_bvGtExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvGtExpr"

external vc_bvGeExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvGeExpr"

external vc_sbvLtExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_sbvLtExpr"

external vc_sbvLeExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_sbvLeExpr"

external vc_sbvGtExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_sbvGtExpr"

external vc_sbvGeExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_sbvGeExpr"

external vc_bvUMinusExpr : vc -> expr -> expr
	= "camlidl_libstp_vc_bvUMinusExpr"

external vc_bvAndExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvAndExpr"

external vc_bvOrExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvOrExpr"

external vc_bvXorExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvXorExpr"

external vc_bvNotExpr : vc -> expr -> expr
	= "camlidl_libstp_vc_bvNotExpr"

external vc_bvLeftShiftExpr : vc -> int -> expr -> expr
	= "camlidl_libstp_vc_bvLeftShiftExpr"

external vc_bvRightShiftExpr : vc -> int -> expr -> expr
	= "camlidl_libstp_vc_bvRightShiftExpr"

external vc_bv32LeftShiftExpr : vc -> int -> expr -> expr
	= "camlidl_libstp_vc_bv32LeftShiftExpr"

external vc_bv32RightShiftExpr : vc -> int -> expr -> expr
	= "camlidl_libstp_vc_bv32RightShiftExpr"

external vc_bvVar32LeftShiftExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvVar32LeftShiftExpr"

external vc_bvVar32RightShiftExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvVar32RightShiftExpr"

external vc_bvVar32DivByPowOfTwoExpr : vc -> expr -> expr -> expr
	= "camlidl_libstp_vc_bvVar32DivByPowOfTwoExpr"

external vc_bvExtract : vc -> expr -> int -> int -> expr
	= "camlidl_libstp_vc_bvExtract"

external vc_bvBoolExtract : vc -> expr -> int -> expr
	= "camlidl_libstp_vc_bvBoolExtract"

external vc_bvSignExtend : vc -> expr -> int -> expr
	= "camlidl_libstp_vc_bvSignExtend"

external vc_bvCreateMemoryArray : vc -> string -> expr
	= "camlidl_libstp_vc_bvCreateMemoryArray"

external vc_bvReadMemoryArray : vc -> expr -> expr -> int -> expr
	= "camlidl_libstp_vc_bvReadMemoryArray"

external vc_bvWriteToMemoryArray : vc -> expr -> expr -> expr -> int -> expr
	= "camlidl_libstp_vc_bvWriteToMemoryArray"

external exprString : expr -> string
	= "camlidl_libstp_exprString"

external typeString : typ -> string
	= "camlidl_libstp_typeString"

external getChild : expr -> int -> expr
	= "camlidl_libstp_getChild"

external libstp_regerrorhandler : unit -> unit
	= "camlidl_libstp_libstp_regerrorhandler"

external getExprKind : expr -> exprkind_t
	= "camlidl_libstp_getExprKind"

external getDegree : expr -> int
	= "camlidl_libstp_getDegree"

external getBVLength : expr -> int
	= "camlidl_libstp_getBVLength"

external getType : expr -> type_t
	= "camlidl_libstp_getType"

external getVWidth : expr -> int
	= "camlidl_libstp_getVWidth"

external getIWidth : expr -> int
	= "camlidl_libstp_getIWidth"

external vc_printCounterExampleFile : vc -> int -> unit
	= "camlidl_libstp_vc_printCounterExampleFile"

external exprName : expr -> string
	= "camlidl_libstp_exprName"

external getExprID : expr -> int
	= "camlidl_libstp_getExprID"

external getTrueCounterExampleFst : vc -> exprList
	= "camlidl_libstp_GetTrueCounterExampleFst"

external getTrueCounterExampleSnd : vc -> exprList
	= "camlidl_libstp_GetTrueCounterExampleSnd"


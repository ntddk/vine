/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/


#ifndef __IRTOIR_INTERNAL_H
#define __IRTOIR_INTERNAL_H

#include "irtoir.h"

// functions internal to irtoir.cpp and irtoir-*.cpp

void panic( string msg );

reg_t IRType_to_reg_type( IRType type );

reg_t get_exp_type( Exp *exp );

inline int get_type_size(reg_t typ) {
  return Exp::reg_to_bits(typ);
}


Temp *mk_temp( string name, IRType ty );
Temp *mk_temp( reg_t type, vector<Stmt *> *stmts );
Temp *mk_temp( IRType ty, vector<Stmt *> *stmts );


Exp *translate_expr( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );

string get_op_str(asm_program_t *prog, Instruction *inst );

int match_mux0x(vector<Stmt*> *ir, unsigned int i,
		Exp **cond, Exp **exp0,	Exp **expx, Exp **res);

extern bool use_eflags_thunks;
extern Exp * count_opnd;


//
// arch specific functions used in irtoir.cpp
//

// defined in irtoir-i386.cpp
vector<VarDecl *> i386_get_reg_decls();
Exp  *i386_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *i386_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
Exp  *i386_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
void  i386_modify_flags( asm_program_t *prog, vine_block_t *block );

// defined in irtoir-arm.cpp
vector<VarDecl *> arm_get_reg_decls();
Exp  *arm_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *arm_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
Exp  *arm_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
void  arm_modify_flags( asm_program_t *prog, vine_block_t *block );


#endif

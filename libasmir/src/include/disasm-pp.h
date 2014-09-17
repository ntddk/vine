/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

#warning "disasm-pp.h is deprecated"
#define _DISASM_PP_H
#ifndef _DISASM_PP_H
#define _DISASM_PP_H

#include <disasm.h>
#include <stdint.h>
#include <libiberty.h>
#include <irtoir.h>

#ifdef __cplusplus

#include <iostream>
#include <sstream>

using namespace std;

void ostream_i386_register(int regnum, ostream &out);

void
ostream_i386_mnemonic(Instruction *inst, ostream &out);

void ostream_i386_insn(Instruction *inst, ostream &out);

extern "C" {
#endif

  extern char* string_i386_blockinsn(vine_block_t *block);

#ifdef __cplusplus
}
#endif

#endif

/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

#ifndef _IR_PRINTER_H
#define _IR_PRINTER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <set>

#include "asm_program.h"
#include "disasm-pp.h"

#include "ir_printer.h"

extern "C" 
{
#include "libvex.h"
}

#include "irtoir.h"

using namespace std;

// Uncomment to enable typechecking.
//#define NOTYPECHECKING
void print_globals();
void print_vine_ir(asm_program_t *prog, vector<vine_block_t *> vblocks );
#endif

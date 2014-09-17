/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

/*
** translate.h
*/

#ifndef   	TRANSLATE_H_
#define   	TRANSLATE_H_

#ifdef __cplusplus
#include <vector>
#include "asm_program.h"
#include "stmt.h"
#include "exp.h"
#include "common.h"
#include "irtoir.h"

using namespace std;

typedef struct instmap_s {
  asm_program_t *prog;
  map<address_t, Instruction *> imap;
} instmap_t;

#else
typedef struct _instmap_t instmap_t;

#endif // __cplusplus

typedef struct _cflow_t { int ctype; address_t target; } cflow_t;


#ifdef __cplusplus
extern "C" {
#endif

  extern asm_program_t *instmap_to_asm_program(instmap_t *map);

  extern instmap_t * filename_to_instmap(const char *filename);

  extern vine_blocks_t *instmap_translate_address_range(instmap_t *insts,
							address_t start,
							address_t end);

  extern vine_block_t *instmap_translate_address(instmap_t *insts,
						 address_t addr);

  extern int instmap_has_address(instmap_t *insts,
				 address_t addr);

  extern cflow_t *instmap_control_flow_type(instmap_t *insts,
					    address_t addr);
  extern address_t get_last_segment_address(const char *filename, 
					    address_t addr);


#ifdef __cplusplus
}
#endif

#endif 	    /* !TRANSLATE_H_ */

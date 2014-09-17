/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

#ifndef _COMMON_H
#define _COMMON_H
#include <stdint.h>

#ifdef __cplusplus
#include "debug.h"
extern "C" {
#endif


#include <bfd.h>

#define DEFAULT_IRMAPFILE  "./asm2re.map"
#define DEFAULT_TRACEFILE "proctrace.out"
#define DEFAULT_UNROLL 2
#define LIBASMIR_VERSION "0.1"
typedef bfd_vma address_t;


#ifdef __cplusplus
}
#endif

#endif

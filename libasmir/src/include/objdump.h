/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

// objdump.cpp contains functions ripped off from objdump.c from
// the binutils distribution. 

#ifndef _OBJDUMP_H
#define _OBJDUMP_H
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C" {
#include <bfd.h>
}

int compare_symbols(const void *ap, const void *bp);
long
remove_useless_symbols (asymbol **symbols, long count);

#endif

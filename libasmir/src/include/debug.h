/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

#ifndef _DEBUG_H
#define _DEBUG_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

void debug_on(const char *key);
bool is_debug_on(const char *key);
void _print_debug(const char *key, const char *fmt, ...);

#ifndef NODEBUG
#define print_debug(args...) _print_debug(args) 
#else 
#define print_debug(args...) while(0)
#endif

void fatal(const char *funcName, const char *fmt, ...);
#endif

/*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*/

#include "debug.h"
#include <set>
#include <string>
#include <iostream>
using namespace std;

static set<string> debugKeys;

bool is_debug_on(const char *key)
{
  return (debugKeys.find(string(key)) != debugKeys.end());
}

void debug_on(const char *key)
{
  debugKeys.insert(string(key));
}

void _print_debug(const char *key, const char *fmt, ...)
{
  va_list args;
  char buf[8192];
  if(!is_debug_on(key))
	return;
  
  va_start(args, fmt);
  vsnprintf(buf, sizeof(buf)-1, fmt, args);
  va_end(args);
  buf[8191] = 0;

  cerr << "++ " << string(key) << ": " << string(buf) << endl;
  cerr.flush();
}

void fatal(const char *funcname, const char *fmt, ...)
{
  va_list args;
  char buf[1024];

  va_start(args, fmt);
  vsnprintf(buf, sizeof(buf)-1, fmt, args);
  va_end(args);
  buf[1023] = 0;
  cerr << "fatal err in " << string(funcname) << ": ";
  cerr << string(buf) << endl;
  cerr.flush();
  exit(-1);
}


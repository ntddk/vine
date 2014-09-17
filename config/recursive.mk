# got it from 
# http://uucode.com/blog/2005/08/10/adding-recursive-targets-to-automake/
# sojeong

doc-recursive:

ifneq ($(RECURSIVE_TARGETS),doc-recursive)
doc-recursive:
	$(MAKE) $(AM_MAKEFLAGS) RECURSIVE_TARGETS=doc-recursive doc-recursive
endif

doc: doc-recursive doc-am

doc-am: doc-local

doc-local:
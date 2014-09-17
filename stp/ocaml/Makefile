export OCAMLMAKEFILE=OCamlMakefile

SOURCES=libstp_regerrorhandler.c libstp.idl stpvc.ml stpvc.mli 
RESULT=stpvc

LIBDIRS = ..
CLIBS = stp stdc++

all: dcl ncl top

distdir:
	cp --parents -t $(distdir) $(SOURCES) \
		$(OCAMLMAKEFILE) \
		Makefile

include $(OCAMLMAKEFILE)

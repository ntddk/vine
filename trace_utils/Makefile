export OCAMLMAKEFILE=../ocaml/OCamlMakefile
export VINE_OCAMLDIR=../ocaml
export VINE_TRACEDIR=../trace

# ocamlfind packages required
export PACKS = gdome2 str ocamlgraph extlib unix
export LIBS = stpvc vine trace 
export INCDIRS = $(VINE_OCAMLDIR) $(VINE_TRACEDIR)

# Include sources from ../ocaml and ../trace in the automatically
# generated dependencies.
# It seems like there should be a more elegant way to do this.
export OCAMLDEP = ocamldep -I $(VINE_OCAMLDIR) $(VINE_TRACEDIR)

# default subprojects to build
ifndef SUBPROJS
	export SUBPROJS = appreplay state_reader trace_reader
endif

EXTRA_DIST = appreplay.ml state_reader.ml trace_reader.ml Makefile

define PROJ_appreplay
        RESULT=appreplay
        SOURCES=appreplay.ml
        INCDIRS += ../stp/ocaml ../stp 
        EXTLIBDIRS += ../stp ../stp/ocaml
        DOC_FILES=$(SOURCES)
endef
export PROJ_appreplay

define PROJ_state_reader
        RESULT=state_reader
        SOURCES=state_reader.ml
        DOC_FILES=$(SOURCES)
endef
export PROJ_state_reader

define PROJ_trace_reader
	RESULT=trace_reader
	SOURCES=trace_reader.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_trace_reader


# suffix for bytecode (which when we make all is debug code)
export BCSUFFIX = .dbg

all: nc dc

distdir:
	cp --parents -t $(distdir) $(EXTRA_DIST)

# You may also tell "make" at the command-line what kind of target to
# produce (e.g. "make nc").  Here all the possibilities with shortcuts
# between parenthesis:
# 
#    * byte-code                     (bc)
#    * byte-code-nolink              (bcnl)   - no linking stage
#    * byte-code-library             (bcl)
#    * native-code                   (nc)
#    * native-code-nolink            (ncnl)   - no linking stage
#    * native-code-library           (ncl)
#    * debug-code                    (dc)
#    * debug-code-nolink             (dcnl)   - no linking stage
#    * debug-code-library            (dcl)
#    * profiling-byte-code           (pbc)
#    * profiling-byte-code-library   (pbcl)
#    * profiling-native-code         (pnc)
#    * profiling-native-code-library (pncl)
#    * byte-code-dll                 (bcd)
#    * native-code-dll               (ncd)
#    * pack-byte-code              (pabc)
#    * pack-native-code            (panc)
#    * toplevel interpreter          (top)
#    * subprjs



%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@


#include $(OCAMLMAKEFILE)

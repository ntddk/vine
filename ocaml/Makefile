export OCAMLMAKEFILE=OCamlMakefile
export LIBASMIR=../libasmir
include $(LIBASMIR)/makefile.inc
export INCDIRS += ../stp ../stp/ocaml
export EXTLIBDIRS += ../stp ../stp/ocaml
export LIBS += stpvc


# common sources
export SOURCES = \
	debug.mli debug.ml \
	symbols.mli symbols.ml \
	vine_util.mli vine_util.ml \
	vine.mli vine.ml  \
	vine_alphavary.mli vine_alphavary.ml \
	vine_typecheck.mli vine_typecheck.ml \
	vine_opt.ml \
	stp.ml smt_lib.ml \
	vine_absyn.mli vine_absyn.ml  \
	vine_grammar.mly \
	vine_lexer.mll \
	libasmir.idl \
	asmir.mli asmir.ml \
	vine_memory2array.ml \
	vine_parser.mli vine_parser.ml \
	dataflow.mli dataflow.ml \
	dominator.ml \
	vine_cfg.mli vine_cfg.ml \
	ssa.mli ssa.ml \
	vine_eval.mli vine_eval.ml \
	vine_ceval.mli vine_ceval.ml \
	vine_loop.mli vine_loop.ml \
	vine_dataflow.mli vine_dataflow.ml \
	vine_indepclauses.ml \
	vine_callgraph.mli vine_callgraph.ml \
	vine_stpvc.mli vine_stpvc.ml \
	gcl.ml \
	wp.ml wp.mli \
	vine_graphviz.ml \
	vine_pdg.ml \
	vsa.ml \
	vine_alias.mli vine_alias.ml \
	vine_inline.ml \
	to_c.ml \
	vine_slice.ml \


RESULT = vine
# DOC_FILES=$(SOURCES)


# ocamlfind packages required
export PACKS = str ocamlgraph extlib unix
export YFLAGS = -v


export CLIBS = asmir vex opcodes bfd iberty stdc++ 
export CFLAGS = -g
export LDFLAGS = $(LIBASMIR_LDFLAGS) 
export CLIBFLAGS= $(LDFLAGS)
export LIBDIRS = $(LIBASMIR)/src # ../appreplay ../stp ../stp/ocaml

OCAMLFLAGS = -warn-error Aelz  #-w A
# merge everything but descriptions.
OCAMLDOCFLAGS = -m avlsoper

all: dcl ncl top

vine_lib_doc: all htdoc

distdir:
	cp --parents -t $(distdir) $(SOURCES) \
		$(OCAMLMAKEFILE) \
		Makefile

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



include $(OCAMLMAKEFILE)

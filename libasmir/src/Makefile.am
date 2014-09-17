RANLIB = @RANLIB@
lib_LIBRARIES = libasmir.a
#lib_LTLIBRARIES = libasmir.la
AM_CFLAGS = -I@VEX_DIR@/pub -I./include -fPIC
AM_CXXFLAGS = $(AM_CFLAGS)


libasmir_a_SOURCES = \
	./include/asm_program.h \
	./include/common.h \
	./include/debug.h \
	./include/disasm-pp.h \
	./include/exp.h \
	./include/info.h \
	./include/ir_program.h \
	./include/irtoir.h \
	./include/irtoir-internal.h \
	./include/irvisitor.h \
	./include/objdump.h \
	./include/stmt.h \
	./include/translate.h \
	./include/vexmem.h \
	./util/debug.cpp \
	./disasm/objdump.cpp \
	./disasm/asm_program.cpp \
	./vex/vexmem.cpp \
	./vex/irtoir.cpp \
	./vex/irtoir-i386.cpp \
	./vex/irtoir-arm.cpp \
	./vex/irtoir_c.cpp \
	./vex/vexir.cpp \
	./vex/gen_thunks.cpp \
	./ir/stmt.cpp \
	./ir/stmt_c.cpp \
	./ir/exp.cpp \
	./ir/exp_c.cpp \
	./ir/translate.cpp

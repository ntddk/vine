bin_PROGRAMS = \
	disassembler \
	print-ir

disassembler_SOURCES = disassembler.cpp
print_ir_SOURCES = print-ir.cpp ir_printer.h ir_printer.cpp
LDADD =  -lasmir -lvex -lbfd -liberty -lopcodes
AM_LDFLAGS = -L../src -L@VEX_DIR@
AM_CXXFLAGS = -I../src/include  -I@VEX_DIR@/pub

# these executables should depend on libasmir, since they link it in
# statically.
$(bin_PROGRAMS): ../src/libasmir.a

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(vine, 1.0, bitblaze-users@googlegroups.com)
AC_CONFIG_SRCDIR([libasmir/src/vex/vexir.cpp])
AC_CONFIG_HEADER([libasmir/config.h])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

m4_include([./m4/ac_ocaml.m4])
m4_include([./m4/ac_prog_latex.m4])
#m4_include([./m4/acltx_prog_kpsewhich.m4])
#m4_include([./m4/acltx_package_location.m4])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_OCAML
AC_PROG_OCAMLFIND
AC_PROG_CAMLIDL
AC_PROG_LATEX
#AC_PROG_LIBTOOL

# Checks for ocaml libraries
ocamlfind query gdome2
if test $? != 0; then
    echo "vine needs libgdome2-ocaml-dev"
    exit -1
fi
ocamlfind query ocamlgraph
if test $? != 0; then
    echo "vine needs libocamlgraph-ocaml-dev"
    exit -1
fi
ocamlfind query extlib
if test $? != 0; then
    echo "vine needs libextlib-ocaml-dev"
    exit -1
fi

# Where is VEX ?
# stolen from valgrind's configure.in
# Nb: For the 2nd arg, the help string, AS_HELP_STRING is the proper way, but
#     older autoconfs don't support it... here's what it would say:
#
#    AS_HELP_STRING([--with-vex], [Vex directory]),
#
AC_ARG_WITH(vex, 
    [  --with-vex=/path/to/vex/dir    Vex directory],
[
    AC_CHECK_FILE($withval/pub/libvex.h,
        [VEX_DIR=$withval],
        [AC_MSG_ERROR([Directory '$withval' does not exist, or does not contain Vex])])
],
[
    VEX_DIR=`pwd`/'VEX/'
])

if test "${VEX_DIR}" = ""; then
	echo " ** ERROR: "
	echo " ** You must supply --with-vex, which should"
	echo " ** point to the directory containing VEX"
	exit -1
fi

AC_ARG_WITH(vex-revision, 
    [  --with-vex-revision=<num>    Vex version],
[
        VEX_VERSION=$withval,
],
[
        VEX_VERSION=`cat ${VEX_DIR}/priv/main/vex_svnversion.h | awk -F\" '{print $2}'`
])
if test "${VEX_VERSION}" = ""; then
    echo "Could not determine VEX version (override with --with-vex-revision=<num>)"
    exit -1
fi

# Pick an appropriate set of STP binaries.

case `uname -m` in
    i?86) stp_arch=i386;;
    x86_64) stp_arch=x86_64;;
    *) stp_arch=unknown;;
esac;

if test "${stp_arch}" = "unknown"; then
    echo "We don't appear to have an STP binary for your architecture"
    exit -1
fi

echo "Using $stp_arch version of STP"

cp -f stp/$stp_arch/stp stp/stp
cp -f stp/$stp_arch/libstp.a stp/libstp.a

#AC_ARG_WITH(binutils, [  --with-binutils=/full/path/to/bintutils])

#if test "${with_binutils}" = ""; then
#	echo " ** ERROR: "
#	echo " ** You must supply --with-binutils, which should"
#	echo " ** point to the directory containing binutils"
#	exit -1
#else
#	CFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CFLAGS"
#	CXXFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CXXFLAGS"
#	LDFLAGS="-L$with_binutils/lib -L$with_binutils/bfd -L$with_binutils/libiberty $LDFLAGS"
#fi


# Checks for libraries.
#AC_LDFLAGS(${VEX_DIR}, ${BINUTILS_DIR})
#echo "HERE: ${AC_LDFLAGS}"
#AC_LIBS([-lasmir], [-lvex], [-lbfd], [-liberty])
#AC_CHECK_LIB([asmir], [libasmir_version])
AC_CHECK_LIB([bfd], [bfd_init])
AC_CHECK_LIB([iberty], [xmalloc])
AC_CHECK_LIB([vex], [LibVEX_Version])

AC_SUBST(VEX_DIR)


cp /dev/null libasmir/makefile.inc
echo "VEX_DIR=${VEX_DIR}" >> libasmir/makefile.inc
echo "LIBASMIR_CXXFLAGS=-I${VEX_DIR}/pub -I`pwd`/src/include $CXXFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LDFLAGS=-L${VEX_DIR} -L`pwd`/src $LDFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LIBS=-lasmir -lvex -lbfd -liberty" >> libasmir/makefile.inc


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h unistd.h])

AC_LANG_PUSH(C++)
#AC_CHECK_HEADERS([boost/shared_ptr.hpp], , 
#    AC_MSG_ERROR([You need the boost headers package (e.g. libboost-dev)]))
#AC_CHECK_HEADER([boost/weak_ptr.hpp], , 
#    AC_MSG_ERROR([You need the boost headers package (e.g. libboost-dev)]))
#AC_CHECK_HEADER([boost/graph/graph_traits.hpp], , 
#    AC_MSG_ERROR([You need the boost graph headers package (e.g. libboost-graph-dev)]))
#AC_CHECK_HEADER([/usr/include/bfd.h], ,
#   AC_MSG_ERROR([You need BFD headers (e.g., libbfd-dev)]))
#AC_CHECK_HEADER([/usr/include/sqlite3.h], ,
#   AC_MSG_ERROR([You need sqlite3 headers (e.g., libsqlite3-dev, and sqlite3 is also recommended but not required)]))

AC_LANG_POP(C++)

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_SIZE_T
AC_DEFINE_UNQUOTED([VEX_VERSION], [${VEX_VERSION}], [The VEX SVN Version])

AC_CHECK_SIZEOF(bfd_vma, [], [#include <bfd.h>])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_CHECK_FUNCS([bzero memset strstr strtoul])


# Checks for latex packages -- to do (without tetex-extra)

# Check for library function versions

AC_CONFIG_FILES([Makefile
                 libasmir/Makefile
                 libasmir/apps/Makefile
                 libasmir/src/Makefile])
AC_OUTPUT

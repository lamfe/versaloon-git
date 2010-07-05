SUBDIRS = driver programmer target tools

bin_PROGRAMS = vsprog

MAINFILE = vsprog.c
vsprog_SOURCES = $(MAINFILE) file_parser.c

INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/tools/hex \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/tools/list \
    -I$(top_srcdir)/src/tools/memlist \
    -I$(top_srcdir)/src/tools/filelist \
    -I$(top_srcdir)/src/target \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/programmer

vsprog_CPPFLAGS = @CPPFLAGS@
vsprog_CPPFLAGS += -DPKGBLDDATE=\"`date +%F-%R`\"

if RELEASE
  vsprog_CPPFLAGS += -DRELSTR=\"Release\" -DPKGBLDREV=\"\"
else
  vsprog_CPPFLAGS += -DRELSTR=\"svn:\" -DPKGBLDREV=\"`$(top_srcdir)/guess-rev.sh $(top_srcdir)`\"
endif

vsprog_LDFLAGS = $(all_libraries)

vsprog_LDADD = \
    $(top_builddir)/src/programmer/libprog.a \
    $(top_builddir)/src/target/libtgt.a \
    $(top_builddir)/src/tools/libtool.a \
    $(top_builddir)/src/driver/libdrv.a \
    $(MINGWLDADD) -lusb $(XML_LIBS)

MAINTAINERCLEANFILES = Makefile.in

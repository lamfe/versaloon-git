bin_PROGRAMS = vsprog

vsprog_SOURCES = vsprog.c

INCLUDES = $(all_includes)
INCLUDES+= -I$(top_srcdir)/src
INCLUDES+= -I$(top_srcdir)/src/tools/hex -I$(top_srcdir)/src/tools/pgbar -I$(top_srcdir)/src/tools/memlist
INCLUDES+= -I$(top_srcdir)/src/target
INCLUDES+= -I$(top_srcdir)/src/driver
INCLUDES+= -I$(top_srcdir)/src/programmer

vsprog_CPPFLAGS = @CPPFLAGS@
vsprog_CPPFLAGS += -DPKGBLDDATE=\"`date +%F-%R`\"

if RELEASE
vsprog_CPPFLAGS += -DRELSTR=\"Release\" -DPKGBLDREV=\"\"
else
vsprog_CPPFLAGS += -DRELSTR=\"svn:\" -DPKGBLDREV=\"`$(top_srcdir)/guess-rev.sh $(top_srcdir)`\"
endif

vsprog_LDFLAGS = $(all_libraries) 
SUBDIRS = driver programmer target tools

vsprog_LDADD =  $(top_builddir)/src/programmer/libprog.a \
				$(top_builddir)/src/target/libtgt.a $(top_builddir)/src/tools/libtool.a \
				$(top_builddir)/src/driver/libdrv.a $(MINGWLDADD) -lusb

localedir = $(datadir)/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

MAINTAINERCLEANFILES = Makefile.in

INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/driver/timer \
    -I$(top_srcdir)/src/tools/list \
    -I$(top_srcdir)/src/tools/filelist

METASOURCES = AUTO
noinst_LIBRARIES = libtool.a

libtool_a_SOURCES = \
    hex/hex.c \
    memlist/memlist.c \
    pgbar/pgbar.c \
    filelist/filelist.c

#noinst_HEADERS = \
#    hex/hex.h \
#    memlist/memlist.h \
#    pgbar/pgbar.h \
#    filelist/filelist.h \
#    list/list.h

MAINTAINERCLEANFILES = Makefile.in

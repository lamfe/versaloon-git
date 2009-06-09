INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/driver/timer

METASOURCES = AUTO
noinst_LIBRARIES = libtool.a

libtool_a_SOURCES = \
    hex/hex.c \
    memlist/memlist.c \
    pgbar/pgbar.c

#noinst_HEADERS = \
#    hex/hex.h \
#    memlist/memlist.h \
#    pgbar/pgbar.h

MAINTAINERCLEANFILES = Makefile.in

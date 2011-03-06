INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/scripts 

METASOURCES = AUTO
noinst_LIBRARIES = libdal.a

libdal_a_SOURCES = \
    dal.c \
    mic2826/mic2826.c \
    mic2826/mic2826_script.c

MAINTAINERCLEANFILES = Makefile.in

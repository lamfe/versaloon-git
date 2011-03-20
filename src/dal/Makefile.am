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
    mal/mal.c \
    mic2826/mic2826_drv.c \
    mic2826/mic2826_script.c \
    ee93cx6/ee93cx6_drv.c \
    ee24cxx/ee24cxx_drv.c \
    df25xx/df25xx_drv.c \
    df45xx/df45xx_drv.c

MAINTAINERCLEANFILES = Makefile.in

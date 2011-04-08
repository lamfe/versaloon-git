INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/programmer/interfaces

METASOURCES = AUTO
noinst_LIBRARIES = libvsf.a

libvsf_a_SOURCES = \
    dal/dal.c \
    dal/mal/mal.c \
    dal/mic2826/mic2826_drv.c \
    dal/mic2826/mic2826_script.c \
    dal/ee93cx6/ee93cx6_drv.c \
    dal/ee24cxx/ee24cxx_drv.c \
    dal/df25xx/df25xx_drv.c \
    dal/df45xx/df45xx_drv.c \
    dal/sd/sd_spi_drv.c \
    dal/sd/sd_common.c

MAINTAINERCLEANFILES = Makefile.in

INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/driver/usb \
    -I$(top_srcdir)/src/driver/timer \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/tools/strparser

METASOURCES = AUTO
noinst_LIBRARIES = libprog.a

libprog_a_SOURCES = \
    programmer.c \
    versaloon/versaloon.c \
    versaloon/usbtoxxx/usbtoadc.c \
    versaloon/usbtoxxx/usbtoc2.c \
    versaloon/usbtoxxx/usbtogpio.c \
    versaloon/usbtoxxx/usbtoi2c.c \
    versaloon/usbtoxxx/usbtoissp.c \
    versaloon/usbtoxxx/usbtomsp430jtag.c \
    versaloon/usbtoxxx/usbtomsp430sbw.c \
    versaloon/usbtoxxx/usbtospi.c \
    versaloon/usbtoxxx/usbtousart.c \
    versaloon/usbtoxxx/usbtoxxx.c \
    versaloon/usbtoxxx/usbtolpcicp.c \
    versaloon/usbtoxxx/usbtopwr.c \
    versaloon/usbtoxxx/usbtojtaghl.c \
    versaloon/usbtoxxx/usbtojtagll.c \
    versaloon/usbtoxxx/usbtoswd.c \
    versaloon/usbtoxxx/usbtoswim.c \
    versaloon/usbtoxxx/usbtojtagraw.c \
    versaloon/usbtoxxx/usbtobdm.c

#noinst_HEADERS = \
    programmer.h \
    versaloon/usbtoxxx/usbtoxxx.h

MAINTAINERCLEANFILES = Makefile.in

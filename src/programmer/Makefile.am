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
    interfaces/interfaces.c \
    interfaces/versaloon/versaloon.c \
    interfaces/versaloon/usbtoxxx/usbtoadc.c \
    interfaces/versaloon/usbtoxxx/usbtoc2.c \
    interfaces/versaloon/usbtoxxx/usbtogpio.c \
    interfaces/versaloon/usbtoxxx/usbtoi2c.c \
    interfaces/versaloon/usbtoxxx/usbtoissp.c \
    interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c \
    interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c \
    interfaces/versaloon/usbtoxxx/usbtospi.c \
    interfaces/versaloon/usbtoxxx/usbtousart.c \
    interfaces/versaloon/usbtoxxx/usbtoxxx.c \
    interfaces/versaloon/usbtoxxx/usbtolpcicp.c \
    interfaces/versaloon/usbtoxxx/usbtopwr.c \
    interfaces/versaloon/usbtoxxx/usbtojtaghl.c \
    interfaces/versaloon/usbtoxxx/usbtojtagll.c \
    interfaces/versaloon/usbtoxxx/usbtoswd.c \
    interfaces/versaloon/usbtoxxx/usbtoswim.c \
    interfaces/versaloon/usbtoxxx/usbtojtagraw.c \
    interfaces/versaloon/usbtoxxx/usbtobdm.c \
    interfaces/versaloon/usbtoxxx/usbtodusi.c \
    interfaces/versaloon/usbtoxxx/usbtomicrowire.c \
    interfaces/versaloon/usbtoxxx/usbtopwm.c

#noinst_HEADERS = \
    programmer.h \
    versaloon/usbtoxxx/usbtoxxx.h

MAINTAINERCLEANFILES = Makefile.in

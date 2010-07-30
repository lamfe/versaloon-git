INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/driver/timer \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/tools/strparser

METASOURCES = AUTO
noinst_LIBRARIES = libdrv.a
libdrv_a_SOURCES = \
    comport/comport.c \
    usb/usbapi.c \
    timer/timer.c
noinst_HEADERS = \
    comport/comport.h \
    usb/usbapi.h \
    timer/timer.h

MAINTAINERCLEANFILES = Makefile.in

INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/programmer/interfaces \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/tools/strparser

METASOURCES = AUTO
noinst_LIBRARIES = libdrv.a
libdrv_a_SOURCES = \
    comport/comport.c \
    usb/usbapi.c
noinst_HEADERS = \
    comport/comport.h \
    usb/usbapi.h

MAINTAINERCLEANFILES = Makefile.in

INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src/vsf_cfg \
    -I$(top_srcdir)/vsf \
    -I$(top_srcdir)/vsf/interfaces/cpu/pc \
    -I$(top_srcdir)/vsf/tool/list \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/driver/usb \
    -I$(top_srcdir)/src/driver/timer \
    -I$(top_srcdir)/src/programmer/interfaces \
    -I$(top_srcdir)/src/scripts \
    -I$(top_srcdir)/src/tools/strparser \
    -I$(top_srcdir)/src/target \
    -I$(top_srcdir)/src/target/core/arm_adi \
    -I$(top_srcdir)/src/target/cortex-m

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
    interfaces/versaloon/usbtoxxx/usbtopwm.c \
    interfaces/versaloon/usbtoxxx/usbtoebi.c \
    interfaces/virtualinterface/vi_stm32/vi_stm32.c

#noinst_HEADERS = \
    programmer.h \
    versaloon/usbtoxxx/usbtoxxx.h

MAINTAINERCLEANFILES = Makefile.in

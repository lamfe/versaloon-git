INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/tools/memlist \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/driver/comport

METASOURCES = AUTO
noinst_LIBRARIES = libtgt.a

libtgt_a_SOURCES = \
    target.c \
    at89s5x/at89s5x.c \
    avr8/avr8.c \
    avr8/avr8_hvpp.c \
    avr8/avr8_hvsp.c \
    avr8/avr8_isp.c \
    avr8/avr8_jtag.c \
    c8051f/c8051f.c \
    c8051f/c8051f_c2.c \
    c8051f/c8051f_jtag.c \
    comisp/comisp.c \
    comisp/lpcarmisp.c \
    comisp/stm32isp.c \
    msp430/JTAGfunc.c \
    msp430/msp430.c \
    msp430/msp430_jtagsbw.c \
    msp430/msp430_bsl.c \
    psoc/psoc.c \
    stm8/stm8.c \
    svf_player/svf_player.c \
    svf_player/byte_tap.c \
    svf_player/svf_parser.c

#noinst_HEADERS = \
#    target.h \
#    at89s5x/at89s5x.h \
#    avr8/avr8.h \
#    c8051f/c8051f_c2.h \
#    c8051f/c8051f_jtag.h \
#    comisp/comisp.h 
#    comisp/lpcarmisp.h \
#    comisp/stm32isp.h \
#    msp430/JTAGfunc.h \
#    msp430/msp430.h \
#    msp430/msp430_const.h \
#    psoc/psoc.h \
#    stm8/stm8.h \
#    svf_player/byte_tap.h \
#    svf_player/svf.h \
#    svf_player/svf_parser.h \
#    svf_player/svf_player.h

MAINTAINERCLEANFILES = Makefile.in

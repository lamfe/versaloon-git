INCLUDES = \
    $(all_includes) \
    -I$(top_srcdir)/src \
    -I$(top_srcdir)/src/programmer \
    -I$(top_srcdir)/src/tools/list \
    -I$(top_srcdir)/src/tools/memlist \
    -I$(top_srcdir)/src/tools/filelist \
    -I$(top_srcdir)/src/tools/fileparser \
    -I$(top_srcdir)/src/tools/pgbar \
    -I$(top_srcdir)/src/tools/strparser \
    -I$(top_srcdir)/src/tools/bufffunc \
    -I$(top_srcdir)/src/driver \
    -I$(top_srcdir)/src/driver/comport \
    -I$(top_srcdir)/src/driver/timer \
    -I$(top_srcdir)/src/target/core/arm_adi \
    -I$(top_srcdir)/src/target/comisp \
    -I$(top_srcdir)/src/target/cortex-m3 \
    -I$(top_srcdir)/src/target/stm32 \
    -I$(top_srcdir)/src/target/lpc1000 \
    -I$(top_srcdir)/src/target/at91sam3 \
    -I$(top_srcdir)/src/target/avr32 \
    -I$(top_srcdir)/src/target/avrxmega \
    -I$(top_srcdir)/src/target/lm3s \
    -I$(top_srcdir)/src/scripts

METASOURCES = AUTO
noinst_LIBRARIES = libtgt.a

libtgt_a_SOURCES = \
    target.c \
    core/arm_adi/adi_v5p1.c \
    cortex-m3/cm3.c \
    cortex-m3/cm3_stm32.c \
    cortex-m3/cm3_lpc1000.c \
    cortex-m3/cm3_at91sam3.c \
    cortex-m3/cm3_lm3s.c \
    cortex-m3/cm3_common.c \
    at89s5x/at89s5x.c \
    avr8/avr8.c \
    avr8/avr8_hvpp.c \
    avr8/avr8_hvsp.c \
    avr8/avr8_isp.c \
    avr8/avr8_jtag.c \
    lpc900/lpc900.c \
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
    psoc1/psoc1.c \
    stm8/stm8.c \
    svf_player/svf_player.c \
    svf_player/byte_tap.c \
    svf_player/svf_parser.c \
    stm32/stm32.c \
    lpc1000/lpc1000.c \
    at91sam3/at91sam3.c \
    avr32/avr32.c \
    avrxmega/avrxmega.c \
    lm3s/lm3s.c \
    hcs08/hcs08.c \
    hcs12/hcs12.c

MAINTAINERCLEANFILES = Makefile.in

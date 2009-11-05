OPT	= s
TARGET	= Versaloon
PRJDIR	= ..
SRCDIR  = ../../..
OBJ_DIR = .

# ASM
ASM_SRCS+= $(SRCDIR)/HW/lib/CMSIS/Core/CM3/startup/gcc/startup_stm32f10x_md.s

# HW
USR_SRCS+= $(SRCDIR)/HW/HW.c
USR_SRCS+= $(SRCDIR)/main.c $(SRCDIR)/HW/stm32f10x_it.c
USR_INCS+= -I $(SRCDIR)/HW -I $(SRCDIR)/HW/lib/CMSIS/Core/CM3

# myString
USR_SRCS+= $(SRCDIR)/HW/Port/GCC/myString.c
USR_INCS+= -I $(SRCDIR)/HW/Port/GCC

# Delay
USR_SRCS+= $(SRCDIR)/HW/Port/GCC/Delay/delay.c
USR_INCS+= -I $(SRCDIR)/HW/Port/GCC/Delay

# VCOM
USR_SRCS+= $(SRCDIR)/HW/VCOM/usb_desc.c $(SRCDIR)/HW/VCOM/usb_endp.c $(SRCDIR)/HW/VCOM/usb_istr.c
USR_SRCS+= $(SRCDIR)/HW/VCOM/usb_prop.c $(SRCDIR)/HW/VCOM/usb_pwr.c
USR_INCS+= -I $(SRCDIR)/HW/VCOM

# Mass
USR_SRCS+= $(SRCDIR)/HW/Mass/mass_mal.c $(SRCDIR)/HW/Mass/memory.c $(SRCDIR)/HW/Mass/scsi_data.c
USR_SRCS+= $(SRCDIR)/HW/Mass/usb_scsi.c $(SRCDIR)/HW/Mass/usb_bot.c
USR_INCS+= -I $(SRCDIR)/HW/Mass

# Tools
USR_SRCS+= $(SRCDIR)/Tools/fifo/fifo.c
USR_INCS+= -I $(SRCDIR)/Tools/fifo

#SPI
USR_SRCS+= $(SRCDIR)/Interfaces/SPI/SPI.c
USR_INCS+= -I $(SRCDIR)/Interfaces/SPI

# JTAG
USR_SRCS+= $(SRCDIR)/Interfaces/JTAG/JTAG_TAP.c
USR_INCS+= -I $(SRCDIR)/Interfaces/JTAG

# SWJ
USR_SRCS+= $(SRCDIR)/Interfaces/SWJ/SWJ.c
USR_INCS+= -I $(SRCDIR)/Interfaces/SWJ

# PowerExt
USR_SRCS+= $(SRCDIR)/Interfaces/PowerExt/PowerExt.c
USR_INCS+= -I $(SRCDIR)/Interfaces/PowerExt

# I2C
USR_SRCS+= $(SRCDIR)/Interfaces/I2C/I2C.c
USR_INCS+= -I $(SRCDIR)/Interfaces/I2C

# VSLLink
USR_SRCS+= $(SRCDIR)/VSLLink/VSLLink.c
USR_INCS+= -I $(SRCDIR)/VSLLink

# USB_TO_XXX
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_XXX.c $(SRCDIR)/USB_TO_XXX/USB_TO_USART.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_SPI.c $(SRCDIR)/USB_TO_XXX/USB_TO_I2C.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_GPIO.c $(SRCDIR)/USB_TO_XXX/USB_TO_PWM.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_ADC.c $(SRCDIR)/USB_TO_XXX/USB_TO_DAC.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_POWER.c 
USR_INCS+= -I $(SRCDIR)/USB_TO_XXX



LIBSTM32_SRCS = \
 $(SRCDIR)/HW/lib/src/misc.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_adc.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_bkp.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_dma.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_flash.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_gpio.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_pwr.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_rcc.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_spi.c \
 $(SRCDIR)/HW/lib/src/stm32f10x_usart.c \
 $(SRCDIR)/HW/lib/src/usb_core.c \
 $(SRCDIR)/HW/lib/src/usb_init.c \
 $(SRCDIR)/HW/lib/src/usb_int.c \
 $(SRCDIR)/HW/lib/src/usb_mem.c \
 $(SRCDIR)/HW/lib/src/usb_regs.c
LIBSTM32_INCS = $(SRCDIR)/HW/lib/inc

LIBSTM32_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(LIBSTM32_SRCS:.c=.o)))
USR_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(USR_SRCS:.c=.o)))
ASM_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(ASM_SRCS:.s=.o)))


TCHAIN = arm-none-eabi

INCLUDE_DIRS = -I $(PRJDIR) -I $(PRJDIR)/../ -I $(SRCDIR) -I $(LIBSTM32_INCS) $(USR_INCS)
COMPILE_OPTS = -mcpu=cortex-m3 -mthumb -mfpu=vfp -nostartfiles -fno-common -fomit-frame-pointer -ffunction-sections -Wall -g -O$(OPT)
#-mabi=apcs-gnu 
LIBRARY_DIRS = 
PREDEFINES   = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_MD -DNDEBUG

CC = $(TCHAIN)-gcc
CFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

CXX = $(TCHAIN)-g++
CXXFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

AS = $(TCHAIN)-gcc
ASFLAGS = $(COMPILE_OPTS) -c

LD = $(TCHAIN)-gcc
LDFLAGS = -Wl,--gc-sections,-Map=$(TARGET).map,-cref,-u,Reset_Handler $(INCLUDE_DIRS) $(LIBRARY_DIRS) -T ./stm32.ld

OBJCP = $(TCHAIN)-objcopy

AR = $(TCHAIN)-ar
ARFLAGS = cr

MAIN_OUT = $(TARGET)_GCC
MAIN_OUT_ELF = $(MAIN_OUT).elf
MAIN_OUT_BIN = $(MAIN_OUT).bin
MAIN_OUT_HEX = $(MAIN_OUT).hex

MAIN_OUT_PROG = $(MAIN_OUT_HEX)

STR_DIV	= ------------------------------------------------------------

# all

all: start gcc-info $(MAIN_OUT_ELF) $(MAIN_OUT_PROG) size flash end

# misc

start:
	@echo Start Compiling Target $(TARGET)
	@echo User Files:
	@echo $(USR_OBJS)
	@echo STM32 Library Files:
	@echo $(LIBSTM32_OBJS)

end:
	@echo $(STR_DIV)
	@echo bye!!

gcc-info:
	@echo $(STR_DIV)
	@echo gcc version is
	@$(CC) --version
	@echo $(STR_DIV)

# main

$(MAIN_OUT_ELF): $(ASM_OBJS) $(USR_OBJS) $(LIBSTM32_OBJS)
	$(LD) $(LDFLAGS) $(ASM_OBJS) $(LIBSTM32_OBJS) $(USR_OBJS) --output $@

$(MAIN_OUT_BIN): $(MAIN_OUT_ELF)
	$(OBJCP) -O binary $< $@

$(MAIN_OUT_HEX): $(MAIN_OUT_ELF)
	$(OBJCP) -O ihex $< $@

size: $(MAIN_OUT_ELF)
	@echo $(STR_DIV)
	size $(MAIN_OUT_ELF)

flash: $(MAIN_OUT_PROG)
	@echo $(STR_DIV)
	vsprog -ccomisp_stm32 -C "COM3" -i $(MAIN_OUT_HEX) -x0x08002000 -oe -owf

$(ASM_OBJS): $(ASM_SRCS)
	$(CC) -c $(CFLAGS) $^

$(LIBSTM32_OBJS): $(LIBSTM32_SRCS)
	$(CC) -c $(CFLAGS) $^

$(USR_OBJS): $(USR_SRCS)
	$(CC) -c $(CFLAGS) $^

clean:
	-rm $(USR_OBJS) $(TARGET).map $(ASM_OBJS) $(LIBSTM32_OBJS) $(MAIN_OUT_ELF) $(MAIN_OUT_PROG)

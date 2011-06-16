# options and settings
PROJECT	= Versaloon

# 
# To select your board , set HW_BOARD to one of the below boards
# Available boards:
# NanoRelease1
# MiniRelease1
# STBee_Mini
# STM8S_Discovery
# STM32VL_Discovery
#
HW_BOARD = MiniRelease1
#

#
include	../../board_defs.mk
#

#
# available OPT:
# 0 - 3: low optimization - high optimization
# s: size optimization
OPT	= s

# set to yes to include USB library
INCLUDE_USB_LIB = yes

# directory define
PRJDIR	= ..
SRCDIR  = ../../..
VSFDIR  = ../../../../../vsf
OBJ_DIR = .
COMPILER_DIR = .
APP_IFACES_DIR = $(SRCDIR)/Interfaces/APP
STM32_IFACES_DIR = $(VSFDIR)/interfaces/cpu/stm32
STM32_LIB_DIR = $(STM32_IFACES_DIR)/hw/STM32F10x_Lib

# User source files/includes/macros
# add .c source files to USR_SRCS
# add .h header files to USR_INCS
# add pre-defines to USR_DEFS
# add library directory to USR_LIBS
USR_SRCS+= $(SRCDIR)/main.c $(SRCDIR)/usb_protocol_Versaloon.c
USR_INCS+= -I $(PRJDIR)/..
USR_SRCS+= $(STM32_IFACES_DIR)/hw/HW.c $(STM32_IFACES_DIR)/hw/stm32f10x_it.c
USR_INCS+= -I $(STM32_IFACES_DIR)/hw
#USR_DEFS+= -DADD_USER_DEFINES_HERE
#USR_LIBS+= -L./

# compiler
USR_SRCS+= $(COMPILER_DIR)/compiler_string.c
USR_INCS+= -I $(COMPILER_DIR)

# Tools
USR_SRCS+= $(SRCDIR)/Tools/fifo/fifo.c
USR_INCS+= -I $(SRCDIR)/Tools/fifo

#APP interfaces
USR_SRCS+= $(SRCDIR)/Interfaces/app_interfaces.c
USR_INCS+= -I $(SRCDIR)/Interfaces

USR_SRCS+= $(APP_IFACES_DIR)/GPIO/GPIO.c $(APP_IFACES_DIR)/USART/USART.c \
  $(APP_IFACES_DIR)/SPI/SPI.c $(APP_IFACES_DIR)/JTAG/JTAG_TAP.c \
  $(APP_IFACES_DIR)/SWD/SWD.c $(APP_IFACES_DIR)/SWIM/SWIM.c \
  $(APP_IFACES_DIR)/DUSI/DUSI.c $(APP_IFACES_DIR)/MicroWire/MicroWire.c \
  $(APP_IFACES_DIR)/PowerExt/PowerExt.c $(APP_IFACES_DIR)/IIC/IIC.c \
  $(APP_IFACES_DIR)/C2/C2.c $(APP_IFACES_DIR)/ISSP/ISSP.c \
  $(APP_IFACES_DIR)/LPC_ICP/LPC_ICP.c $(APP_IFACES_DIR)/MSP430_JTAG/MSP430_JTAG.c \
  $(APP_IFACES_DIR)/BDM/BDM.c $(APP_IFACES_DIR)/PWM/PWM.c
USR_INCS+= -I $(APP_IFACES_DIR) -I $(APP_IFACES_DIR)/GPIO \
  -I $(APP_IFACES_DIR)/USART -I $(APP_IFACES_DIR)/SPI \
  -I $(APP_IFACES_DIR)/JTAG -I $(APP_IFACES_DIR)/SWD \
  -I $(APP_IFACES_DIR)/SWIM -I $(APP_IFACES_DIR)/DUSI \
  -I $(APP_IFACES_DIR)/MicroWire -I $(APP_IFACES_DIR)/PowerExt \
  -I $(APP_IFACES_DIR)/IIC -I $(APP_IFACES_DIR)/C2 \
  -I $(APP_IFACES_DIR)/ISSP -I $(APP_IFACES_DIR)/LPC_ICP \
  -I $(APP_IFACES_DIR)/MSP430_JTAG -I $(APP_IFACES_DIR)/BDM \
  -I $(APP_IFACES_DIR)/PWM

#VSF interfaces
USR_SRCS+= $(VSFDIR)/interfaces/interfaces.c
USR_INCS+= -I $(VSFDIR)/interfaces

USR_SRCS+= $(STM32_IFACES_DIR)/gpio/STM32_GPIO.c $(STM32_IFACES_DIR)/usbd/STM32_USBD.c
USR_INCS+= -I $(STM32_IFACES_DIR) -I $(STM32_IFACES_DIR)/gpio \
  -I $(STM32_IFACES_DIR)/usbd

#VSF dal
USR_SRCS+= $(VSFDIR)/dal/dal.c \
  $(VSFDIR)/dal/df25xx/df25xx_drv.c \
  $(VSFDIR)/dal/df45xx/df45xx_drv.c \
  $(VSFDIR)/dal/ee24cxx/ee24cxx_drv.c \
  $(VSFDIR)/dal/ee93cx6/ee93cx6_drv.c \
  $(VSFDIR)/dal/mal/mal.c \
  $(VSFDIR)/dal/mic2826/mic2826_drv.c \
  $(VSFDIR)/dal/nrf24l01/nrf24l01_drv.c

#VSF stack
USR_SRCS+= $(VSFDIR)/stack/usb_device/vsf_usbd.c \
  $(VSFDIR)/stack/usb_device/class/CDC/vsfusbd_CDC.c \
  $(VSFDIR)/stack/usb_device/class/HID/vsfusbd_HID.c \
  $(VSFDIR)/stack/usb_device/class/MSC/vsfusbd_MSC_BOT.c \
  $(VSFDIR)/stack/usb_device/class/MSC/SCSI.c
USR_INCS+= -I $(VSFDIR)

# USB_TO_XXX
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_XXX.c $(SRCDIR)/USB_TO_XXX/USB_TO_USART.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_SPI.c $(SRCDIR)/USB_TO_XXX/USB_TO_IIC.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_GPIO.c $(SRCDIR)/USB_TO_XXX/USB_TO_PWM.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_ADC.c $(SRCDIR)/USB_TO_XXX/USB_TO_DAC.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_POWER.c $(SRCDIR)/USB_TO_XXX/USB_TO_C2.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_ISSP.c $(SRCDIR)/USB_TO_XXX/USB_TO_LPCICP.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_MSP430_JTAG.c $(SRCDIR)/USB_TO_XXX/USB_TO_JTAG_HL.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_SWD.c $(SRCDIR)/USB_TO_XXX/USB_TO_JTAG_LL.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_SWIM.c $(SRCDIR)/USB_TO_XXX/USB_TO_JTAG_RAW.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_BDM.c $(SRCDIR)/USB_TO_XXX/USB_TO_DUSI.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/USB_TO_MICROWIRE.c
USR_SRCS+= $(SRCDIR)/USB_TO_XXX/CommandProcessor.c
USR_INCS+= -I $(SRCDIR)/USB_TO_XXX

# LIBSTM32_LIB
ifeq ($(TARGET_STM32),LowDensity)
  LIBSTM32_ASM_SRCS = \
    $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_ld.s
  LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_LD
else
  ifeq ($(TARGET_STM32),LowDensityValueLine)
    LIBSTM32_ASM_SRCS = \
      $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_ld_vl.s
    LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_LD_VL
  else
    ifeq ($(TARGET_STM32),MediumDensity)
	  LIBSTM32_ASM_SRCS = \
        $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_md.s
      LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_MD
	else
	  ifeq ($(TARGET_STM32),MediumDensityValueLine)
	    LIBSTM32_ASM_SRCS = \
          $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_md_vl.s
        LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_MD_VL
	  else
	    ifeq ($(TARGET_STM32),HighDensity)
		  LIBSTM32_ASM_SRCS = \
            $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_hd.s
          LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_HD
		else
		  ifeq ($(TARGET_STM32),ConnectivityLine)
		    LIBSTM32_ASM_SRCS = \
              $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/startup/gcc_ride7/startup_stm32f10x_cl.s
            LIBSTM32_DEFS = -DUSE_STDPERIPH_DRIVER -DSTM32F10X_CL
		  else
		    Invalid STM32 Target
		  endif
		endif
	  endif
	endif
  endif
endif
LIBSTM32_SRCS = \
  $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/CoreSupport/core_cm3.c \
  $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x/system_stm32f10x.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/misc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_adc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_bkp.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_can.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_cec.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_crc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_dac.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_dbgmcu.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_dma.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_exti.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_flash.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_fsmc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_gpio.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_i2c.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_iwdg.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_pwr.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_rcc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_rtc.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_sdio.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_spi.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_tim.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_usart.c \
  $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/src/stm32f10x_wwdg.c
LIBSTM32_INCS = \
  -I $(STM32_LIB_DIR)/Libraries/STM32F10x_StdPeriph_Driver/inc \
  -I $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x \
  -I $(STM32_LIB_DIR)/Libraries/CMSIS/CM3/CoreSupport

# LIBSTM32_USB_LIB
ifeq (${INCLUDE_USB_LIB},yes)
  LIBSTM32_SRCS+= \
	$(STM32_LIB_DIR)/Libraries/STM32_USB-FS-Device_Driver/src/usb_mem.c \
	$(STM32_LIB_DIR)/Libraries/STM32_USB-FS-Device_Driver/src/usb_regs.c
  LIBSTM32_INCS+= \
    -I $(STM32_LIB_DIR)/Libraries/STM32_USB-FS-Device_Driver/inc
endif


LIBSTM32_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(LIBSTM32_SRCS:.c=.o)))
USR_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(USR_SRCS:.c=.o)))
ASM_SRCS = $(LIBSTM32_ASM_SRCS)
ASM_OBJS = $(addprefix $(OBJ_DIR)/, $(notdir $(LIBSTM32_ASM_SRCS:.s=.o)))


TCHAIN = arm-none-eabi

INCLUDE_DIRS = -I $(PRJDIR) -I $(SRCDIR) $(LIBSTM32_INCS) $(USR_INCS)
COMPILE_OPTS = -mcpu=cortex-m3 -mthumb -mfpu=vfp 
COMPILE_OPTS+= -nostartfiles -fno-common -fomit-frame-pointer -Wall -g -O$(OPT)
LIBRARY_DIRS = $(USR_LIBS)
PREDEFINES   = $(LIBSTM32_DEFS) $(USR_DEFS)

CC = $(TCHAIN)-gcc
CFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

CXX = $(TCHAIN)-g++
CXXFLAGS = $(COMPILE_OPTS) $(INCLUDE_DIRS) $(PREDEFINES)

AS = $(TCHAIN)-gcc
ASFLAGS = $(COMPILE_OPTS) -c

LD = $(TCHAIN)-gcc
LDFLAGS = -Wl,--gc-sections,-Map=$(PROJECT).map,-cref,-u,Reset_Handler $(INCLUDE_DIRS) $(LIBRARY_DIRS) -T $(LD_FILE)

OBJCP = $(TCHAIN)-objcopy

AR = $(TCHAIN)-ar
ARFLAGS = cr

MAIN_OUT = $(PROJECT)_GCC
MAIN_OUT_ELF = $(MAIN_OUT).elf
MAIN_OUT_BIN = $(MAIN_OUT).bin
MAIN_OUT_HEX = $(MAIN_OUT).hex

MAIN_OUT_PROG = $(MAIN_OUT_HEX)

STR_DIV	= ------------------------------------------------------------

# all

all: start gcc-info $(MAIN_OUT_ELF) $(MAIN_OUT_PROG) size flash end

# misc

start:
	@echo Start Compiling Project $(PROJECT)
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
#	@echo $(STR_DIV)
#	vsprog -sstm32 -G -mj -I $(MAIN_OUT_HEX) -oe -owf -ovf

$(ASM_OBJS): $(ASM_SRCS)
	$(CC) -c $(CFLAGS) $^

$(LIBSTM32_OBJS): $(LIBSTM32_SRCS)
	$(CC) -c $(CFLAGS) $^

$(USR_OBJS): $(USR_SRCS)
	$(CC) -c $(CFLAGS) $^

clean:
	-rm $(USR_OBJS) $(PROJECT).map $(ASM_OBJS) $(LIBSTM32_OBJS) $(MAIN_OUT_ELF) $(MAIN_OUT_PROG)

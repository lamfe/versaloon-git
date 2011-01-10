#########################################################################################################################
#
# Here we define various hardware specific things about the board being built for
#
#########################################################################################################################

# Available boards:
# NanoRelease1
# MiniRC2
# MiniRC3
# MiniRC4
# MiniRelease1
# ProRC1
# STBee_Mini
# STM8S_Discovery
# STM32VL_Discovery

# Available targets:
# LowDensity
# LowDensityValueLine
# MediumDensity
# MediumDensityValueLine
# HighDensity
# ConnectivityLine
#
# This is selected per board by TARGET_STM32 = <target>
#

########################################################################
ifeq ($(HW_BOARD),NanoRelease1)
########################################################################
_HARDWARE_VER			= 0x01
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE 		= 12000000
LD_FILE 		= versaloon.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),MiniRC2)
########################################################################
_HARDWARE_VER			= 0x12
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE 		= 12000000
LD_FILE 		= versaloon.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),MiniRC3)
#########################################################################
_HARDWARE_VER			= 0x13
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE 		= 12000000
LD_FILE 		= versaloon.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),MiniRC4)
########################################################################
_HARDWARE_VER			= 0x14
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE 		= 12000000
LD_FILE 		= versaloon.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),MiniRelease1)
########################################################################
_HARDWARE_VER			= 0x15
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE 		= 12000000
LD_FILE 		= versaloon.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),ProRC1)
########################################################################
_HARDWARE_VER			= 0x21
FLASH_LOAD_OFFSET	= 0x2000
HSE_VALUE 		= 12000000
LD_FILE 		= versaloon.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),STBee_Mini)
########################################################################
_HARDWARE_VER			= 0x31
FLASH_LOAD_OFFSET	= 0x3000
HSE_VALUE 		= 12000000
LD_FILE 		= stbee.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),STM8S_Discovery)
########################################################################
_HARDWARE_VER			= 0x32
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE 		= 8000000
LD_FILE 		= sd-discovery.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
ifeq ($(HW_BOARD),STM32VL_Discovery)
########################################################################
_HARDWARE_VER			= 0x33
FLASH_LOAD_OFFSET	= 0x0000
HSE_VALUE 		= 8000000
LD_FILE 		= st-discovery.ld
TARGET_STM32 		= MediumDensity
else
########################################################################
# Unknown board error
########################################################################
$(error Missing or unknown HW_BOARD defined in makefile)
endif
endif
endif
endif
endif
endif
endif
endif
endif

#
USR_DEFS+= -DHSE_VALUE=$(HSE_VALUE)UL -DHW_BOARD=$(HW_BOARD) -D_HARDWARE_VER=$(_HARDWARE_VER)
USR_DEFS+= -DFLASH_LOAD_OFFSET=$(FLASH_LOAD_OFFSET)
USR_LIBS+= -L../../

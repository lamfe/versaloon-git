/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       app_cfg.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    configuration file                                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2011-01-09:     Add stmxx-discovery (by Bingo)                    *
 **************************************************************************/

/************************ What do U have ************************/
// For IAR, define _HARDWARE_VER and FLASH_LOAD_OFFSET in pre-defined symbols
// For GCC, edit HW_BOARD in makefile

//
// Note The below definitions must correspond to the definitions made in 
// board_defs.mk (same place where the makefile resides)
//
#define NanoRelease1					0x01
#define MiniRC2							0x12
#define MiniRC3							0x13
#define MiniRC4							0x14
#define MiniRelease1					0x15
#define ProRC1							0x21
#define STBee_Mini						0x31
#define STM8S_Discovery					0x32
#define STM32VL_Discovery				0x33

#if _HARDWARE_VER == NanoRelease1
#include "hw_cfg_NanoRelease1.h"
#elif _HARDWARE_VER == MiniRC2
#include "hw_cfg_MiniRC2.h"
#elif _HARDWARE_VER == MiniRC3
#include "hw_cfg_MiniRC3.h"
#elif _HARDWARE_VER == MiniRC4
#include "hw_cfg_MiniRC4.h"
#elif _HARDWARE_VER == MiniRelease1
#include "hw_cfg_MiniRelease1.h"
#elif _HARDWARE_VER == ProRC1
#include "hw_cfg_ProRC1.h"
#elif _HARDWARE_VER == STBee_Mini
#include "hw_cfg_STBee_Mini.h"
#elif _HARDWARE_VER == STM8S_Discovery
#include "hw_cfg_stm8s-discovery.h"
#elif _HARDWARE_VER == STM32VL_Discovery
#include "hw_cfg_stm32vl-discovery.h"
#else
#error "Unknown or missing HW_BOARD definition"
#endif


/*************************** Includes Library ***************************/
#include "stm32f10x_conf.h"
#include "app_type.h"
#include "port.h"
#include "myString.h"
#include "CommandProcessor.h"

/************************ What do U want ************************/
#define __VSF_DEBUG__					1

#define USB_TO_XXX_EN					1

#define POWER_OUT_EN					(1 && HW_HAS_POWERCONTROL)
#define POWER_SAMPLE_EN					(1 && HW_HAS_ADC)

#if USB_TO_XXX_EN
// page 0
#	define USB_TO_USART_EN				(1 && HW_HAS_USART)
#	define USB_TO_SPI_EN				(1 && HW_HAS_SPI)
#	define USB_TO_IIC_EN				(1 && HW_HAS_IIC)
#	define USB_TO_GPIO_EN				(1 && HW_HAS_GPIO)
#	define USB_TO_CAN_EN				(0 && HW_HAS_CAN)
#	define USB_TO_PWM_EN				(1 && HW_HAS_PWM)
#	define USB_TO_ADC_EN				(1 && HW_HAS_ADC)
#	define USB_TO_DAC_EN				(0 && HW_HAS_DAC)
#	define USB_TO_MICROWIRE_EN			(1 && HW_HAS_MICROWIRE)
#	define USB_TO_DUSI_EN				(1 && HW_HAS_DUSI)
// page 1
#	define USB_TO_JTAG_LL_EN			(1 && HW_HAS_JTAG)
#	define USB_TO_JTAG_HL_EN			(1 && HW_HAS_JTAG)
#	define USB_TO_JTAG_RAW_EN			(1 && HW_HAS_JTAG)
#	define USB_TO_ISSP_EN				(1 && HW_HAS_ISSP)
#	define USB_TO_C2_EN					(1 && HW_HAS_C2)
#	define USB_TO_MSP430_JTAG_EN		(1 && HW_HAS_MSP430_JTAG)
#	define USB_TO_MSP430_SBW_EN			(0 && HW_HAS_MSP430_SBW)
#	define USB_TO_LPCICP_EN				(1 && HW_HAS_LPCICP)
#	define USB_TO_SWD_EN				(1 && HW_HAS_SWD)
#	define USB_TO_SWIM_EN				(1 && HW_HAS_SWIM)
#	define USB_TO_BDM_EN				(1 && HW_HAS_BDM)
// page 2
#	define USB_TO_POWER_EN				(1 && HW_HAS_POWERCONTROL)
#endif

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				(12 * 1024)
#define ASYN_DATA_BUFF_SIZE				(4 * 1024)

/**************************** Checks ****************************/
#define _HARDWARE_VER_STR				make_ver(_HARDWARE_VER)
#define make_ver(v)						make_str(v)
#define make_str(s)						# s

#define INTERFACE_C2_EN					(USB_TO_XXX_EN && USB_TO_C2_EN)
#define INTERFACE_IIC_EN				(USB_TO_XXX_EN && USB_TO_IIC_EN)
#define INTERFACE_ISSP_EN				(USB_TO_XXX_EN && USB_TO_ISSP_EN)
#define INTERFACE_LPC_ICP_EN			(USB_TO_XXX_EN && USB_TO_LPCICP_EN)
#define INTERFACE_JTAG_EN				(USB_TO_XXX_EN && \
				(USB_TO_JTAG_HL_EN || USB_TO_JTAG_LL_EN || USB_TO_JTAG_RAW_EN))
#define INTERFACE_MSP430_JTAG_EN		(USB_TO_XXX_EN && USB_TO_MSP430_JTAG_EN)
#define INTERFACE_MSP430_SBW_EN			(USB_TO_XXX_EN && USB_TO_MSP430_SBW_EN)
#define INTERFACE_SPI_EN				(USB_TO_XXX_EN && USB_TO_SPI_EN)
#define INTERFACE_SWIM_EN				(USB_TO_XXX_EN && USB_TO_SWIM_EN)
#define INTERFACE_USART_EN				(USB_TO_XXX_EN && USB_TO_USART_EN)
#define INTERFACE_SWD_EN				(USB_TO_XXX_EN && USB_TO_SWD_EN)
#define INTERFACE_BDM_EN				(USB_TO_XXX_EN && USB_TO_BDM_EN)
#define INTERFACE_GPIO_EN				(USB_TO_XXX_EN && USB_TO_GPIO_EN)
#define INTERFACE_DUSI_EN				(USB_TO_XXX_EN && USB_TO_DUSI_EN)
#define INTERFACE_MICROWIRE_EN			(USB_TO_XXX_EN && USB_TO_MICROWIRE_EN)
#define INTERFACE_PWM_EN				(USB_TO_XXX_EN && USB_TO_PWM_EN)

/*************************** Includes Application ***************************/
#include "HW.h"

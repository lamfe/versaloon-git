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
 **************************************************************************/

/************************ What do U have ************************/
// enable ONLY one below according to your hardware
//#include "hw_cfg_NanoRelease1.h"
//#include "hw_cfg_MiniRC2.h"
//#include "hw_cfg_MiniRC3.h"
//#include "hw_cfg_MiniRC4.h"
#include "hw_cfg_MiniRelease1.h"
//#include "hw_cfg_ProRC1.h"
//#include "hw_cfg_STBee_Mini.h"

/*************************** Includes Library ***************************/
#include "stm32f10x_conf.h"
#include "app_type.h"
#include "port.h"
#include "myString.h"
#include "delay.h"
#include "CommandProcessor.h"

/************************ What do U want ************************/
#define USB_ST_VCOM						0x02
#define USB_PROTOCOL					USB_ST_VCOM

#define USB_OUT_EN						1

#define USB_WITH_NO_CDC					0				// 0 more USB Interface descriptor
#define USB_WITH_COMPOSITE_CDC			1				// 1 more USB Interface descriptor
#define USB_WITH_IAD_CDC				2				// 2 more USB Interface descriptors
#define USB_WITH_CDC					USB_WITH_IAD_CDC// only valid if not use ST_VCOM
#define USB_CDC_BY_POLLING				1

#define USB_WITH_MASSSTORAGE			0

#define USB_TO_XXX_EN					1

#define POWER_OUT_EN					1
#define POWER_SAMPLE_EN					1

#define VERSALOON_FW_UPDATE_EN			1

#if USB_TO_XXX_EN
// page 0
#	define USB_TO_USART_EN				1
#	define USB_TO_SPI_EN				1
#	define USB_TO_IIC_EN				1
#	define USB_TO_GPIO_EN				1
#	define USB_TO_CAN_EN				0
#	define USB_TO_PWM_EN				0
#	define USB_TO_ADC_EN				1
#	define USB_TO_DAC_EN				0
#	define USB_TO_MICROWIRE_EN			0
// page 1
#	define USB_TO_JTAG_LL_EN			1
#	define USB_TO_JTAG_HL_EN			1
#	define USB_TO_JTAG_RAW_EN			1
#	define USB_TO_ISSP_EN				1
#	define USB_TO_C2_EN					1
#	define USB_TO_MSP430_JTAG_EN		1
#	define USB_TO_MSP430_SBW_EN			0
#	define USB_TO_LPCICP_EN				1
#	define USB_TO_SWD_EN				1
#	define USB_TO_SWIM_EN				1
#	define USB_TO_BDM_EN				1
// page 2
#	define USB_TO_POWER_EN				1
#endif

/****************************** Mass ******************************/
#define MSD_MEMORY_START_ADDR			0x0800A000	// from 40K
#define MSD_MEMORY_BLOCK_SIZE			1024

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				(12 * 1024)
#define ASYN_DATA_BUFF_SIZE				(4 * 1024)

/**************************** Checks ****************************/
#define _HARDWARE_VER_STR				make_ver(_HARDWARE_VER)
#define make_ver(v)						make_str(v)
#define make_str(s)						# s

#if (USB_PROTOCOL == USB_ST_VCOM)
#	define CDC_IF_EN					1
#endif

#if !POWER_OUT_EN
#define PWREXT_Acquire()
#define PWREXT_Release()
#endif

#if (USB_PROTOCOL != USB_ST_VCOM) && (USB_WITH_CDC != USB_WITH_IAD_CDC) && USB_WITH_MASSSTORAGE
#	warning "MSD will not be enabled, if not using IAD for CDC"
#endif

#if USB_PROTOCOL == USB_ST_VCOM
#	define USB_RX_DOUBLEBUFFER_EN		1
#	define USB_TX_DOUBLEBUFFER_EN		1
#else
#	define USB_RX_DOUBLEBUFFER_EN		0
#	define USB_TX_DOUBLEBUFFER_EN		0
#endif

#if !USB_TO_XXX_EN
// page 0
#	define USB_TO_USART_EN				0
#	define USB_TO_SPI_EN				0
#	define USB_TO_IIC_EN				0
#	define USB_TO_GPIO_EN				0
#	define USB_TO_CAN_EN				0
#	define USB_TO_PWM_EN				0
#	define USB_TO_ADC_EN				0
#	define USB_TO_DAC_EN				0
#	define USB_TO_MICROWIRE_EN			0
// page 1
#	define USB_TO_JTAG_LL_EN			0
#	define USB_TO_JTAG_HL_EN			0
#	define USB_TO_JTAG_RAW_EN			0
#	define USB_TO_ISSP_EN				0
#	define USB_TO_C2_EN					0
#	define USB_TO_MSP430_JTAG_EN		0
#	define USB_TO_MSP430_SBW_EN			0
#	define USB_TO_LPCICP_EN				0
#	define USB_TO_SWD_EN				0
#	define USB_TO_SWIM_EN				0
#	define USB_TO_BDM_EN				0
// page 2
#	define USB_TO_POWER_EN				0
#endif

#if USB_TO_CAN_EN
#	warning "USB_TO_CAN_EN can not supported by this hardware"
#	undef USB_TO_CAN_EN
#	define USB_TO_CAN_EN				0
#endif

#define INTERFACE_C2_EN					(USB_TO_XXX_EN && USB_TO_C2_EN)
#define INTERFACE_IIC_EN				(USB_TO_XXX_EN && USB_TO_IIC_EN)
#define INTERFACE_ISSP_EN				(USB_TO_XXX_EN && USB_TO_ISSP_EN)
#define INTERFACE_LPC_ICP_EN			(USB_TO_XXX_EN && USB_TO_LPCICP_EN)
#define INTERFACE_JTAG_EN				(USB_TO_XXX_EN && (USB_TO_JTAG_HL_EN || USB_TO_JTAG_LL_EN || USB_TO_JTAG_RAW_EN))
#define INTERFACE_MSP430_JTAG_EN		(USB_TO_XXX_EN && USB_TO_MSP430_JTAG_EN)
#define INTERFACE_MSP430_SBW_EN			(USB_TO_XXX_EN && USB_TO_MSP430_SBW_EN)
#define INTERFACE_SPI_EN				(USB_TO_XXX_EN && USB_TO_SPI_EN)
#define INTERFACE_SWIM_EN				(USB_TO_XXX_EN && USB_TO_SWIM_EN)
#define INTERFACE_USART_EN				(USB_TO_XXX_EN && USB_TO_USART_EN)
#define INTERFACE_SWD_EN				(USB_TO_XXX_EN && USB_TO_SWD_EN)
#define INTERFACE_BDM_EN				(USB_TO_XXX_EN && USB_TO_BDM_EN)

/*************************** Includes Application ***************************/
#include "HW.h"

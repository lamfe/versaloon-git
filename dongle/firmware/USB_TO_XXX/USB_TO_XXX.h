/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_xxx.h                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header file for USB_TO_XXX                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "USB_proto.h"

// USB_TO_XXX USB Commands
// Page0
#define USB_TO_USART				(VERSALOON_USB_TO_XXX_CMD_START + 0x00)
#define USB_TO_SPI					(VERSALOON_USB_TO_XXX_CMD_START + 0x01)
#define USB_TO_I2C					(VERSALOON_USB_TO_XXX_CMD_START + 0x02)
#define USB_TO_GPIO					(VERSALOON_USB_TO_XXX_CMD_START + 0x03)
#define USB_TO_CAN					(VERSALOON_USB_TO_XXX_CMD_START + 0x04)
#define USB_TO_PWM					(VERSALOON_USB_TO_XXX_CMD_START + 0x05)
#define USB_TO_ADC					(VERSALOON_USB_TO_XXX_CMD_START + 0x06)
#define USB_TO_DAC					(VERSALOON_USB_TO_XXX_CMD_START + 0x07)
#define USB_TO_MICROWIRE			(VERSALOON_USB_TO_XXX_CMD_START + 0x08)
// Page1
#define USB_TO_JTAG_LL				(VERSALOON_USB_TO_XXX_CMD_START + 0x20)
#define USB_TO_JTAG_HL				(VERSALOON_USB_TO_XXX_CMD_START + 0x21)
#define USB_TO_ISSP					(VERSALOON_USB_TO_XXX_CMD_START + 0x22)
#define USB_TO_C2					(VERSALOON_USB_TO_XXX_CMD_START + 0x23)
#define USB_TO_SBW					(VERSALOON_USB_TO_XXX_CMD_START + 0x24)
#define USB_TO_LPCICP				(VERSALOON_USB_TO_XXX_CMD_START + 0x25)
#define USB_TO_SWJ					(VERSALOON_USB_TO_XXX_CMD_START + 0x26)
#define USB_TO_MSP430_JTAG			(VERSALOON_USB_TO_XXX_CMD_START + 0x38)
// Page2
#define USB_TO_POWER				(VERSALOON_USB_TO_XXX_CMD_START + 0x40)
#define USB_TO_DELAY				(VERSALOON_USB_TO_XXX_CMD_START + 0x41)
#define USB_TO_POLL					(VERSALOON_USB_TO_XXX_CMD_START + 0x42)
#define USB_TO_ALL					(VERSALOON_USB_TO_XXX_CMD_START + 0x5F)

// USB_TO_XXX USB Processors
extern uint8* buffer_reply;
void USB_TO_XXX_ProcessCmd(uint8* dat, uint16 len);
// Page0
void USB_TO_USART_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_SPI_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_I2C_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_GPIO_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_CAN_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_PWM_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_ADC_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_DAC_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_MICROWIRE_ProcessCmd(uint8* dat, uint16 len);
// Page1
void USB_TO_JTAG_LL_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_JTAG_HL_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_ISSP_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_C2_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_LPCICP_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_SWJ_ProcessCmd(uint8* dat, uint16 len);
void USB_TO_MSP430_JTAG_ProcessCmd(uint8* dat, uint16 len);
// Page2
void USB_TO_POWER_ProcessCmd(uint8* dat, uint16 len);
extern int8 USB_TO_POLL_Index;

// USB_TO_XXX Masks
#define USB_TO_XXX_CMDMASK			0xF8
#define USB_TO_XXX_CMDSHIFT			3
#define USB_TO_XXX_IDXMASK			0x07

// USB_TO_XXX Commands
// Common Commands
#define USB_TO_XXX_INIT				(0x00 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_FINI				(0x01 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_CONFIG			(0x02 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_GETHWINFO		(0x03 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_STATUS			(0X04 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_IN_OUT			(0x05 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_IN				(0x06 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_OUT				(0x07 << USB_TO_XXX_CMDSHIFT)
#define USB_T0_XXX_POLL				(0x08 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_SPECIAL			(0x09 << USB_TO_XXX_CMDSHIFT)
#define USB_TO_XXX_RESET			(0x0A << USB_TO_XXX_CMDSHIFT)
// USB_TO_JTAG_LL
#define USB_TO_JTAG_LL_SCAN			USB_TO_XXX_IN_OUT
#define USB_TO_JTAG_LL_TMS			USB_TO_XXX_OUT
// USB_TO_JTAG_HL
#define USB_TO_JTAG_HL_IR_DR		USB_TO_XXX_IN_OUT
#define USB_TO_JTAG_HL_TMS			USB_TO_XXX_OUT
// USB_TO_ISSP
#define USB_TO_ISSP_EnterProgMode	USB_TO_XXX_IN
#define USB_TO_ISSP_LeaveProgMode	USB_TO_XXX_OUT
#define USB_TO_ISSP_Vector			USB_TO_XXX_IN_OUT
#define USB_TO_ISSP_WaitAndPoll		USB_T0_XXX_POLL
// USB_TO_LPCICP
#define USB_TO_LPCICP_EnterProgMode	USB_TO_XXX_IN_OUT
#define USB_TO_LPCICP_In			USB_TO_XXX_IN
#define USB_TO_LPCICP_Out			USB_TO_XXX_OUT
#define USB_TO_LPCICP_PollRdy		USB_T0_XXX_POLL
// USB_TO_SWJ
#define USB_TO_SWJ_SEQOUT			USB_TO_XXX_OUT
#define USB_TO_SWJ_SEQIN			USB_TO_XXX_IN
#define USB_TO_SWJ_Transact			USB_TO_XXX_IN_OUT
// USB_TO_I2C
#define USB_TO_I2C_Read				USB_TO_XXX_IN
#define USB_TO_I2C_Write			USB_TO_XXX_OUT
// USB_TO_C2
#define USB_TO_C2_Data				USB_TO_XXX_IN_OUT
#define USB_TO_C2_Addr				USB_TO_XXX_OUT
#define USB_TO_C2_PollStats			USB_T0_XXX_POLL
// USB_TO_MSP430_JTAG
#define USB_TO_MSP430_JTAG_Reset	USB_TO_XXX_RESET
#define USB_TO_MSP430_JTAG_IRDR		USB_TO_XXX_IN_OUT
#define USB_TO_MSP430_JTAG_TCLK		USB_TO_XXX_OUT
#define USB_TO_MSP430_JTAG_TCLK_STROBE	USB_TO_XXX_SPECIAL
#define USB_TO_MSP430_JTAG_Poll		USB_T0_XXX_POLL
// USB_TO_MSP430_SBW
#define USB_TO_MSP430_SBW_Reset		USB_TO_XXX_RESET
#define USB_TO_MSP430_SBW_IRDR		USB_TO_XXX_IN_OUT
#define USB_TO_MSP430_SBW_TCLK		USB_TO_XXX_OUT
#define USB_TO_MSP430_SBW_TCLK_STROBE	USB_TO_XXX_SPECIAL
#define USB_TO_MSP430_SBW_Poll		USB_T0_XXX_POLL
// USB_TO_POLL
#define USB_TO_POLL_START			0x00
#define USB_TO_POLL_END				0x01
#define USB_TO_POLL_CHECKBYTE		0x02

// USB_TO_XXX Replys
#define USB_TO_XXX_OK				0x00
#define USB_TO_XXX_FAILED			0x01
#define USB_TO_XXX_TIME_OUT			0x02
#define USB_TO_XXX_INVALID_INDEX	0x03
#define USB_TO_XXX_INVALID_PARA		0x04
#define USB_TO_XXX_INVALID_CMD		0x05
#define USB_TO_XXX_CMD_NOT_SUPPORT	0x06


// USB_TO_POWER
#define USB_TO_POWER_5V				0x00
#define USB_TO_POWER_3D3V			0x01
#define USB_TO_POWER_BOOST			0x02


// USB_TO_SPI
#define USB_TO_SPI_CPOL_HIGH		0x20
#define USB_TO_SPI_CPOL_LOW			0x00
#define USB_TO_SPI_CPHA_2EDGE		0x40
#define USB_TO_SPI_CPHA_1EDGE		0x00
#define USB_TO_SPI_MSB_FIRST		0x80


// USB_TO_GPIO
#define USB_TO_GPIO_DIR_MSK			0x01
#define USB_TO_GPIO_SRST			(1 << 0)
#define USB_TO_GPIO_TRST			(1 << 1)
#if JTAG_HAS_USER_PIN
#	define USB_TO_GPIO_USR1			(1 << 2)
#	define USB_TO_GPIO_USR2			(1 << 3)
#else
#	define USB_TO_GPIO_USR1			0
#	define USB_TO_GPIO_USR2			0
#endif
#define USB_TO_GPIO_TCK				(1 << 4)
#define USB_TO_GPIO_TDO				(1 << 5)
#define USB_TO_GPIO_TDI				(1 << 6)
#define USB_TO_GPIO_RTCK			(1 << 7)
#define USB_TO_GPIO_TMS				(1 << 8)
#define USB_TO_GPIO_OUT_MSK			(USB_TO_GPIO_SRST | USB_TO_GPIO_TRST | USB_TO_GPIO_USR1 | USB_TO_GPIO_TCK | USB_TO_GPIO_TDI)
#define USB_TO_GPIO_IN_MSK			(USB_TO_GPIO_SRST | USB_TO_GPIO_USR2 | USB_TO_GPIO_TDO | USB_TO_GPIO_RTCK)
#define USB_TO_GPIO_MSK				(USB_TO_GPIO_OUT_MSK | USB_TO_GPIO_IN_MSK)


// USB_TO_ISSP
#define USB_TO_ISSP_ATTR_BANK		(1 << 0)
#define USB_TO_ISSP_ATTR_READ		(1 << 1)
#define USB_TO_ISSP_ATTR_APPENDBIT	(1 << 2)
#define USB_TO_ISSP_ATTR_0s			(1 << 3)


// Number of Interfaces
#define USB_TO_USART_NUM			1
#define USB_TO_SPI_NUM				1
#define USB_TO_I2C_NUM				1
#define USB_TO_GPIO_NUM				1
#define USB_TO_CAN_NUM				1
#define USB_TO_PWM_NUM				1
#define USB_TO_ADC_NUM				1
#define USB_TO_DAC_NUM				1
#define USB_TO_MICROWIRE_NUM		1
#define USB_TO_JTAG_LL_NUM			1
#define USB_TO_JTAG_HL_NUM			1
#define USB_TO_ISSP_NUM				1
#define USB_TO_LPCICP_NUM			1
#define USB_TO_SWJ_NUM				1
#define USB_TO_C2_NUM				1
#define USB_TO_MSP430_JTAG_NUM		1
#define USB_TO_SBW_NUM				1
#define USB_TO_POWER_NUM			1
#define USB_TO_POLL_NUM				3

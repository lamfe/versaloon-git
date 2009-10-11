/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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
 *      2008-11-22:     rewrite GPIO_Dir(by SimonQian)                    *
 **************************************************************************/

/*************************** Includes ***************************/
#include "stm32f10x_conf.h"
#include "stm32f10x.h"
#include "app_type.h"
#include "port.h"
#include "myString.h"
#include "delay.h"
#include "HW.h"

/************************ What do U have ************************/
#define STM32_NANO						0x03
#define STM32_MINI						0x13
#define STM32_PRO						0x23
#define STM32_FULL						0x33

#define _HARDWARE_VER					STM32_MINI

#define _SYS_FREQUENCY					72		// in MHz
#define _SYS_FLASH_VECTOR_TABLE_SHIFT	0x2000	// application will locate at 0x08002000

/************************ What do U want ************************/
#define USB_AT_JTAGICE_MKII				0x00
#define USB_AT_DRAGON					0x01
#define USB_ST_VCOM						0x02
#define USB_PROTOCOL					USB_ST_VCOM

#define USB_WITH_NO_CDC					0				// 0 more USB Interface descriptor
#define USB_WITH_COMPOSITE_CDC			1				// 1 more USB Interface descriptor
#define USB_WITH_IAD_CDC				2				// 2 more USB Interface descriptors
#define USB_WITH_CDC					USB_WITH_IAD_CDC// only valid if not use ST_VCOM
#define USB_CDC_BY_POLLING				1

#define USB_WITH_MASSSTORAGE			0

#define AVR_ISP_EN						0
#define AVR_JTAG_EN						0
#define AVR_HVPP_EN						0
#define AVR_HVSP_EN						0
#define AVR_DW_EN						0
#define AVRX_JTAG_EN					0
#define AVRX_PDI_EN						0
#define AVR32_JTAG_EN					0

#define VSLLINK_EN						0
#define VSLLINK_HL_EN					0
#define USB_TO_XXX_EN					0

#define POWER_OUT_EN					0
#define POWER_SAMPLE_EN					0

#define VERSALOON_FW_UPDATE_EN			0

// mass-product support
#define MP_EN							0
#if MP_EN
#	define MP_PSOC_EN					0
#	define MP_C8051F_C2_EN				0
#	define MP_C8051F_JTAG_EN			0
#	define MP_S5X_EN					0
#	define MP_STM8_EN					0
#	define MP_LPCICP_EN					0
#endif

#if USB_TO_XXX_EN
// page 0
#	define USB_TO_USART_EN				0
#	define USB_TO_SPI_EN				1
#	define USB_TO_I2C_EN				1
#	define USB_TO_GPIO_EN				1
#	define USB_TO_CAN_EN				0
#	define USB_TO_PWM_EN				0
#	define USB_TO_ADC_EN				1
#	define USB_TO_DAC_EN				0
#	define USB_TO_MICROWIRE_EN			0
// page 1
#	define USB_TO_JTAG_LL_EN			0
#	define USB_TO_JTAG_HL_EN			0
#	define USB_TO_ISSP_EN				0
#	define USB_TO_C2_EN					0
#	define USB_TO_MSP430_JTAG_EN		0
#	define USB_TO_MSP430_SBW_EN			0
#	define USB_TO_SBW_EN				0
#	define USB_TO_LPCICP_EN				0
// page 2
#	define USB_TO_POWER_EN				1
#endif

#if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
#	define AVR_EN						(AVR_ISP_EN | AVR_JTAG_EN | AVR_HVPP_EN | AVR_HVSP_EN)
#	define AVRX_EN						(AVRX_JTAG_EN | AVRX_PDI_EN)
#	define ATMEL_DEVICE_EN				(AVR_EN | AVRX_EN | AVR_DW_EN | AVR32_JTAG_EN)
#elif USB_PROTOCOL == USB_ST_VCOM
#	define AVR_EN						0
#	define AVRX_EN						0
#	define ATMEL_DEVICE_EN				0
#endif
#define MCU_PROG_EN						(ATMEL_DEVICE_EN)

/****************************** Mass ******************************/
#define MSD_MEMORY_START_ADDR			0x08005000	// from 20K
#define MSD_MEMORY_BLOCK_SIZE			1024

/****************************** Power ******************************/
#define PWREXT_EN_PORT					GPIOB
#define PWREXT_EN_PIN					GPIO_PIN_8

#define PWREXT_INIT()					PWREXT_DISABLE()
#define PWREXT_ENABLE()					do{\
											GPIO_ClrPins(PWREXT_EN_PORT, GPIO_PIN_GetMask(PWREXT_EN_PIN));\
											GPIO_Dir(PWREXT_EN_PORT, GPIO_MODE_OUT_PP, PWREXT_EN_PIN);\
										}while(0)
#define PWREXT_DISABLE()				GPIO_Dir(PWREXT_EN_PORT, GPIO_MODE_IN_FLOATING, PWREXT_EN_PIN)

/****************************** Global Output ******************************/
#define GLOBAL_OUTPUT_ENABLE_PORT		GPIOB
#define GLOBAL_OUTPUT_ENABLE_PIN		GPIO_PIN_1

#if (_HARDWARE_VER == STM32_PRO)
#	define GLOBAL_OUTPUT_INIT()			do{\
											GLOBAL_OUTPUT_DISABLE();\
											GPIO_Dir(GLOBAL_OUTPUT_ENABLE_PORT, GPIO_MODE_OUT_PP, GLOBAL_OUTPUT_ENABLE_PIN);\
										}while(0)
#	define GLOBAL_OUTPUT_ENABLE()		GPIO_ClrPins(GLOBAL_OUTPUT_ENABLE_PORT, GPIO_PIN_GetMask(GLOBAL_OUTPUT_ENABLE_PIN))
#	define GLOBAL_OUTPUT_DISABLE()		GPIO_SetPins(GLOBAL_OUTPUT_ENABLE_PORT, GPIO_PIN_GetMask(GLOBAL_OUTPUT_ENABLE_PIN))
#else
#	define GLOBAL_OUTPUT_INIT()			
#	define GLOBAL_OUTPUT_ENABLE()		
#	define GLOBAL_OUTPUT_DISABLE()		
#endif

/****************************** SW ******************************/
#if (_HARDWARE_VER == STM32_PRO)
#	define SW_PORT						GPIOB
#	define SW_PIN						GPIO_PIN_11
#	define SW_RST_PORT					GPIOB
#	define SW_RST_PIN					GPIO_PIN_10
#elif (_HARDWARE_VER == STM32_MINI)
#	define SW_PORT						GPIOB
#	define SW_PIN						GPIO_PIN_10
#	define SW_RST_PORT					GPIOB
#	define SW_RST_PIN					GPIO_PIN_11
#elif (_HARDWARE_VER == STM32_NANO)
#	define SW_PORT						GPIOA
#	define SW_PIN						GPIO_PIN_10
#	define SW_RST_PORT					GPIOA
#	define SW_RST_PIN					GPIO_PIN_9
#endif
#define SW_DIR_PORT						GPIOB
#define SW_DIR_PIN						GPIO_PIN_12

#define SW_DIR_INIT()					do{\
											SW_SETINPUT();\
											GPIO_Dir(SW_DIR_PORT, GPIO_MODE_OUT_PP, SW_DIR_PIN);\
										}while(0)
#define SW_SETINPUT()					do{\
											GPIO_Dir(SW_PORT, GPIO_MODE_IN_FLOATING, SW_PIN);\
											GPIO_ClrPins(SW_DIR_PORT, GPIO_PIN_GetMask(SW_DIR_PIN));\
										}while(0)
#define SW_SETOUTPUT()					do{\
											GPIO_SetPins(SW_DIR_PORT, GPIO_PIN_GetMask(SW_DIR_PIN));\
											GPIO_Dir(SW_PORT, GPIO_MODE_OUT_PP, SW_PIN);\
										}while(0)
#define SW_SET()						GPIO_SetPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))
#define SW_CLR()						GPIO_ClrPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))
#define SW_GET()						GPIO_GetInPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))

#define SW_RST_SETOUTPUT()				GPIO_Dir(SW_RST_PORT, GPIO_MODE_OUT_PP, SW_RST_PIN)
#define SW_RST_SETINPUT()				GPIO_Dir(SW_RST_PORT, GPIO_MODE_IN_FLOATING, SW_RST_PIN)
#define SW_RST_SET()					GPIO_SetPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))
#define SW_RST_CLR()					GPIO_ClrPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))
#define SW_RST_GET()					GPIO_GetOutPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))

/***************************** JTAG ******************************/
#define JTAG_TAP_PORT					GPIOB
#define JTAG_TAP_TCK_PIN				GPIO_PIN_13
#define JTAG_TAP_TDO_PIN				GPIO_PIN_14
#define JTAG_TAP_TDI_PIN				GPIO_PIN_15
#define JTAG_TAP_TMS_PORT				GPIOB
#define JTAG_TAP_TMS_PIN				GPIO_PIN_4
#define JTAG_TAP_RTCK_PORT				GPIOA
#define JTAG_TAP_RTCK_PIN				GPIO_PIN_8

#if (_HARDWARE_VER == STM32_PRO) || (_HARDWARE_VER == STM32_MINI)
#	define JTAG_HAS_USER_PIN			1
#elif (_HARDWARE_VER == STM32_NANO)
#	define JTAG_HAS_USER_PIN			0
#endif

#if JTAG_HAS_USER_PIN
#	define JTAG_TAP_USR_PORT			GPIOA
#	define JTAG_TAP_USR1_PIN			GPIO_PIN_9
#	define JTAG_TAP_USR2_PIN			GPIO_PIN_10

#	define JTAG_TAP_USR1_SETOUTPUT()	GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_OUT_PP, JTAG_TAP_USR1_PIN)
#	define JTAG_TAP_USR1_SETINPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_USR1_PIN)
#	define JTAG_TAP_USR1_SET()			GPIO_SetPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))
#	define JTAG_TAP_USR1_CLR()			GPIO_ClrPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))
#	define JTAG_TAP_USR1_GET()			GPIO_GetOutPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))

#	define JTAG_TAP_USR2_SETINPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_USR2_PIN)
#	define JTAG_TAP_USR2_GET()			GPIO_GetInPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR2_PIN))
#endif

#define JTAG_TAP_TMS_SETOUTPUT()		GPIO_Dir(JTAG_TAP_TMS_PORT, GPIO_MODE_OUT_PP, JTAG_TAP_TMS_PIN)
#define JTAG_TAP_TMS_SETINPUT()			GPIO_Dir(JTAG_TAP_TMS_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TMS_PIN)
#define JTAG_TAP_TMS_SET()				GPIO_SetPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TMS_PIN))
#define JTAG_TAP_TMS_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TMS_PIN))
#define JTAG_TAP_TMS_GET()				GPIO_GetOutPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TMS_PIN))

#define JTAG_TAP_TCK_SETOUTPUT()		GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_OUT_PP, JTAG_TAP_TCK_PIN)
#define JTAG_TAP_TCK_SETINPUT()			GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TCK_PIN)
#define JTAG_TAP_TCK_SET()				GPIO_SetPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TCK_PIN))
#define JTAG_TAP_TCK_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TCK_PIN))
#define JTAG_TAP_TCK_GET()				GPIO_GetOutPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TCK_PIN))

#define JTAG_TAP_TDI_SETOUTPUT()		GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_OUT_PP, JTAG_TAP_TDI_PIN)
#define JTAG_TAP_TDI_SETINPUT()			GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TDI_PIN)
#define JTAG_TAP_TDI_SET()				GPIO_SetPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TDI_PIN))
#define JTAG_TAP_TDI_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TDI_PIN))
#define JTAG_TAP_TDI_GET()				GPIO_GetOutPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TDI_PIN))

#define JTAG_TAP_TDO_SETINPUT()			GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TDO_PIN)
#define JTAG_TAP_TDO_SET()				GPIO_SetPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TDO_PIN))
#define JTAG_TAP_TDO_CLR()				GPIO_ClrPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TDO_PIN))
#define JTAG_TAP_TDO_GET()				GPIO_GetInPins(JTAG_TAP_PORT, GPIO_PIN_GetMask(JTAG_TAP_TDO_PIN))

#define JTAG_TAP_TRST_SETOUTPUT()		SW_RST_SETOUTPUT()
#define JTAG_TAP_TRST_SETINPUT()		SW_RST_SETINPUT()
#define JTAG_TAP_TRST_SET()				SW_RST_SET()
#define JTAG_TAP_TRST_CLR()				SW_RST_CLR()
#define JTAG_TAP_TRST_GET()				SW_RST_GET()

#define JTAG_TAP_RTCK_SETINPUT()		GPIO_Dir(JTAG_TAP_RTCK_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_RTCK_PIN)
#define JTAG_TAP_RTCK_GET()				GPIO_GetInPins(JTAG_TAP_RTCK_PORT, GPIO_PIN_GetMask(JTAG_TAP_RTCK_PIN))

#define JTAG_TAP_SRST_SETOUTPUT()		SW_SETOUTPUT()
#define JTAG_TAP_SRST_SETINPUT()		SW_SETINPUT()
#define JTAG_TAP_SRST_SET()				SW_SET()
#define JTAG_TAP_SRST_CLR()				SW_CLR()
#define JTAG_TAP_SRST_GET()				SW_GET()


#define JTAG_TAP_HS_SPI_M				SPI2
#define JTAG_TAP_HS_SPI_S				SPI1

// DMA
#define JTAG_TAP_HS_SPI_M_TX_DMA		DMA1_Channel5
#define JTAG_TAP_HS_SPI_M_RX_DMA		DMA1_Channel4
#define JTAG_TAP_HS_SPI_S_TX_DMA		DMA1_Channel3

#define JTAG_TAP_HS_SPI_EnableDMA()		do{\
											SPI_I2S_DMACmd(JTAG_TAP_HS_SPI_M, SPI_I2S_DMAReq_Rx, ENABLE);\
											SPI_I2S_DMACmd(JTAG_TAP_HS_SPI_S, SPI_I2S_DMAReq_Tx, ENABLE);\
										}while(0)

#define JTAG_TAP_HS_SPI_M_RX_DMA_LEN(l)	(JTAG_TAP_HS_SPI_M_RX_DMA->CNDTR = (uint32)(l))
#define JTAG_TAP_HS_SPI_M_RX_DMA_ADDR(a)(JTAG_TAP_HS_SPI_M_RX_DMA->CMAR = (uint32)(a))
#define JTAG_TAP_HS_SPI_M_RX_DMA_EN()	(JTAG_TAP_HS_SPI_M_RX_DMA->CCR |= (uint32)1)
#define JTAG_TAP_HS_SPI_M_RX_DMA_DIS()	(JTAG_TAP_HS_SPI_M_RX_DMA->CCR &= ~(uint32)1)
#define JTAG_TAP_HS_SPI_M_RX_DMA_WAIT()	do{while(!(DMA1->ISR & DMA1_FLAG_TC4)); DMA1->IFCR = DMA1_FLAG_TC4;}while(0)

#define JTAG_TAP_HS_SPI_S_TX_DMA_LEN(l)	(JTAG_TAP_HS_SPI_S_TX_DMA->CNDTR = (uint32)(l))
#define JTAG_TAP_HS_SPI_S_TX_DMA_ADDR(a)(JTAG_TAP_HS_SPI_S_TX_DMA->CMAR = (uint32)(a))
#define JTAG_TAP_HS_SPI_S_TX_DMA_EN()	(JTAG_TAP_HS_SPI_S_TX_DMA->CCR |= (uint32)1)
#define JTAG_TAP_HS_SPI_S_TX_DMA_DIS()	(JTAG_TAP_HS_SPI_S_TX_DMA->CCR &= ~(uint32)1)
#define JTAG_TAP_HS_SPI_S_TX_DMA_WAIT()	do{while(!(DMA1->ISR & DMA1_FLAG_TC3)); DMA1->IFCR = DMA1_FLAG_TC3;}while(0)

#define JTAG_TAP_HS_MPORT				GPIOB

#define JTAG_TAP_HS_SPORT				GPIOB
#define JTAG_TAP_TCK1_PIN				GPIO_PIN_3
#define JTAG_TAP_TMS_PIN				GPIO_PIN_4

#define JTAG_TAP_HS_PortInit()			do{\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_AF_PP, JTAG_TAP_TCK_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_AF_PP, JTAG_TAP_TDO_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_AF_PP, JTAG_TAP_TDI_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_AF_PP, JTAG_TAP_TCK1_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_AF_PP, JTAG_TAP_TMS_PIN);\
										}while(0)
#define JTAG_TAP_HS_PortFini()			do{\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TCK_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TDO_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TDI_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TCK1_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TMS_PIN);\
										}while(0)

#define JTAG_TAP_HS_SetSpeed(div)		SPI_Configuration(JTAG_TAP_HS_SPI_M,SPI_Mode_Master,(div),\
														SPI_FirstBit_LSB,SPI_CPOL_High,SPI_CPHA_2Edge)

#define JTAG_TAP_HS_TMS_Out(tms)		JTAG_TAP_HS_SPI_S->DR = (tms)
#define JTAG_TAP_HS_TDI_Out(tdi)		JTAG_TAP_HS_SPI_M->DR = (tdi)
#define JTAG_TAP_HS_Out(tms, tdi)		do{JTAG_TAP_HS_TMS_Out(tms); JTAG_TAP_HS_TDI_Out(tdi);}while(0)
#define JTAG_TAP_HS_In()				JTAG_TAP_HS_SPI_M->DR

#define JTAG_TAP_HS_WaitTxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_TXE))
#define JTAG_TAP_HS_WaitRxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_RXNE))
#define JTAG_TAP_HS_WaitReady()			while(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_BSY)

/****************************** USART ******************************/
// to change USART port below, modify the interrupt handler
#define USART_DEF_PORT					USART1
#define USART_IRQ						USART1_IRQn
#define USART_AUX_PORT_EN				0

#define USART_PORT						GPIOA
#define USART_TXD_PIN					GPIO_PIN_9
#define USART_RXD_PIN					GPIO_PIN_10

#define USART_AUX_PORT					GPIOB
#define USART_DTR_PIN					GPIO_PIN_10
#define USART_RTS_PIN					GPIO_PIN_11
#define USART_DSR_PIN					
#define USART_CTS_PIN					
#define USART_RI_PIN					

#define USART_DTR_SET()					GPIO_SetPins(USART_AUX_PORT, GPIO_PIN_GetMask(USART_DTR_PIN))
#define USART_DTR_CLR()					GPIO_ClrPins(USART_AUX_PORT, GPIO_PIN_GetMask(USART_DTR_PIN))
#define USART_RTS_SET()					GPIO_SetPins(USART_AUX_PORT, GPIO_PIN_GetMask(USART_RTS_PIN))
#define USART_RTS_CLR()					GPIO_ClrPins(USART_AUX_PORT, GPIO_PIN_GetMask(USART_RTS_PIN))

#define USART_AUX_Port_Init()			do{\
											GPIO_Dir(USART_AUX_PORT, GPIO_MODE_OUT_PP, USART_DTR_PIN);\
											GPIO_Dir(USART_AUX_PORT, GPIO_MODE_OUT_PP, USART_RTS_PIN);\
										}while(0)
#define USART_Port_Init()				do{\
											GPIO_Dir(USART_PORT, GPIO_MODE_AF_PP, USART_TXD_PIN);\
											GPIO_Dir(USART_PORT, GPIO_MODE_IN_FLOATING, USART_RXD_PIN);\
										}while(0)

/****************************** LED ******************************/
#if (_HARDWARE_VER == STM32_MINI) || (_HARDWARE_VER == STM32_PRO)
#	define LED_PORT						GPIOB
#	define LED_RED_PIN					GPIO_PIN_6
#	define LED_GREEN_PIN				GPIO_PIN_7
#	define LED_RED_ON()					GPIO_ClrPins(LED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#	define LED_RED_OFF()				GPIO_SetPins(LED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#	define LED_GREEN_ON()				GPIO_ClrPins(LED_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))
#	define LED_GREEN_OFF()				GPIO_SetPins(LED_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))
#elif (_HARDWARE_VER == STM32_NANO)
#	define LED_PORT						GPIOA
#	define LED_RED_PIN					GPIO_PIN_1
#	define LED_GREEN_PIN				GPIO_PIN_2
#	define LED_RED_ON()					GPIO_SetPins(LED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#	define LED_RED_OFF()				GPIO_ClrPins(LED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#	define LED_GREEN_ON()				GPIO_SetPins(LED_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))
#	define LED_GREEN_OFF()				GPIO_ClrPins(LED_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))
#endif
#define LED_Init()						do{\
											GPIO_Dir(LED_PORT, GPIO_MODE_OUT_PP, LED_RED_PIN);\
											GPIO_Dir(LED_PORT, GPIO_MODE_OUT_PP, LED_GREEN_PIN);\
										}while(0)
#define LED_Fini()						do{\
											GPIO_Dir(LED_PORT, GPIO_MODE_IN_FLOATING, LED_RED_PIN);\
											GPIO_Dir(LED_PORT, GPIO_MODE_IN_FLOATING, LED_GREEN_PIN);\
										}while(0)

// LED_RW
#if (_HARDWARE_VER == STM32_PRO)
#define LED_RW_PORT						GPIOA
#define LED_RW_PIN						GPIO_PIN_3
#define Led_RW_Init()					do{\
											GPIO_Dir(LED_RW_PORT, GPIO_MODE_OUT_PP, LED_RW_PIN);\
										}while(0)
#define Led_RW_ON()						GPIO_ClrPins(LED_RW_PORT, GPIO_PIN_GetMask(LED_RW_PIN))
#define Led_RW_OFF()					GPIO_SetPins(LED_RW_PORT, GPIO_PIN_GetMask(LED_RW_PIN))
#else
#define Led_RW_Init()					
#define Led_RW_ON()						LED_GREEN_OFF()
#define Led_RW_OFF()					LED_GREEN_ON()
#endif

/****************************** KEY ******************************/
#if (_HARDWARE_VER == STM32_MINI)
#	define KEY_PORT						GPIOC
#	define KEY_PIN						GPIO_PIN_13
#	define KEY_IsDown()					(!GPIO_GetInPins(KEY_PORT, GPIO_PIN_GetMask(KEY_PIN)) == 0)
#	define KEY_Init()					do{ BKP_TamperPinCmd(DISABLE); GPIO_Dir(KEY_PORT, GPIO_MODE_IPU, KEY_PIN); }while(0)
#	define KEY_Fini()					do{ GPIO_Dir(KEY_PORT, GPIO_MODE_IN_FLOATING, KEY_PIN); BKP_TamperPinCmd(ENABLE); }while(0)
#elif (_HARDWARE_VER == STM32_PRO)
#	define KEY_PORT						GPIOC
#	define KEY_PIN						GPIO_PIN_13
#	define KEY_IsDown()					(GPIO_GetInPins(KEY_PORT, GPIO_PIN_GetMask(KEY_PIN)) == 0)
#	define KEY_Init()					do{ BKP_TamperPinCmd(DISABLE); GPIO_Dir(KEY_PORT, GPIO_MODE_IPU, KEY_PIN); }while(0)
#	define KEY_Fini()					do{ GPIO_Dir(KEY_PORT, GPIO_MODE_IN_FLOATING, KEY_PIN); BKP_TamperPinCmd(ENABLE); }while(0)
#elif (_HARDWARE_VER == STM32_NANO)
#	define KEY_PORT						GPIOB
#	define KEY_PIN						GPIO_PIN_9
#	define KEY_IsDown()					(GPIO_GetInPins(KEY_PORT, GPIO_PIN_GetMask(KEY_PIN)))
#	define KEY_Init()					GPIO_Dir(KEY_PORT, GPIO_MODE_IPD, KEY_PIN)
#	define KEY_Fini()					GPIO_Dir(KEY_PORT, GPIO_MODE_IN_FLOATING, KEY_PIN)
#endif


/****************************** USB *****************************/
// For USB 2.0, use DP
// For USB 1.1, use DM
#define USB_DP_PORT						GPIOA
#define USB_DP_PIN						GPIO_PIN_12

#define USB_Disable()					PowerOff()
#define USB_D_SETOUTPUT()				GPIO_Dir(USB_DP_PORT, GPIO_MODE_OUT_PP, USB_DP_PIN)
#define USB_D_SET()						GPIO_SetPins(USB_DP_PORT, GPIO_PIN_GetMask(USB_DP_PIN))
#define USB_D_CLR()						GPIO_ClrPins(USB_DP_PORT, GPIO_PIN_GetMask(USB_DP_PIN))

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				(6 * 1024)
#define ASYN_DATA_BUFF_SIZE				(1 * 1024)

/***************************** EXTERN ****************************/
extern SPI_InitTypeDef   SPI_InitStructure;
void SPI_Configuration(SPI_TypeDef* SPIx, u16 mode, u16 brp, u16 fb, u16 cpol, u16 cpha);

extern __IO uint32_t rep_len, cmd_len;
extern uint8_t buffer_out[USB_DATA_BUFF_SIZE], *buffer_in;
extern __IO uint32_t count_out, usb_ovf;
extern __IO uint32_t usb_in_data_remain, usb_in_numofpackage;

extern uint8 asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
extern uint16 asyn_rx_idx, asyn_tx_idx;
extern uint16 Vtarget;

extern void System_Idle_Loop(void);
extern void GLOBAL_OUTPUT_Acquire(void);
extern void GLOBAL_OUTPUT_Release(void);

/**************************** Checks ****************************/
#define _HARDWARE_VER_STR				make_ver(_HARDWARE_VER)
#define make_ver(v)						make_str(v)
#define make_str(s)						# s



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

#if USB_TO_CAN_EN == 1
#	warning "USB_TO_CAN_EN can not supported by this hardware"
#	undef USB_TO_CAN_EN
#	define USB_TO_CAN_EN				0
#endif

#if USB_TO_XXX_EN == 0
#	define USB_TO_USART_EN				0
#	define USB_TO_SPI_EN				0
#	define USB_TO_I2C_EN				0
#	define USB_TO_GPIO_EN				0
#	define USB_TO_CAN_EN				0
#	define USB_TO_PWM_EN				0
#	define USB_TO_ADC_EN				0
#	define USB_TO_DAC_EN				0
#	define USB_TO_POWER_EN				0
#endif

#if MP_EN == 0
#	define MP_PSOC_EN					0
#	define MP_C8051F_C2_EN				0
#	define MP_C8051F_JTAG_EN			0
#	define MP_S5X_EN					0
#	define MP_STM8_EN					0
#endif

#define INTERFACE_C2_EN					((USB_TO_XXX_EN && USB_TO_C2_EN) || (MP_EN && MP_C8051F_C2_EN))
#define INTERFACE_I2C_EN				(USB_TO_XXX_EN && USB_TO_I2C_EN)
#define INTERFACE_ISSP_EN				((USB_TO_XXX_EN && USB_TO_ISSP_EN) || (MP_EN && MP_PSOC_EN))
#define INTERFACE_LPC_ICP_EN			((USB_TO_XXX_EN && USB_TO_LPCICP_EN) || (MP_EN && MP_LPCICP_EN))
#define INTERFACE_JTAG_EN				((USB_TO_XXX_EN && (USB_TO_JTAG_HL_EN || USB_TO_JTAG_LL_EN)) || AVR_JTAG_EN || AVR32_JTAG_EN || AVRX_JTAG_EN || VSLLINK_EN || (MP_EN && MP_C8051F_JTAG_EN))
#define INTERFACE_MSP430_JTAG_EN		USB_TO_MSP430_JTAG_EN
#define INTERFACE_SBW_EN				(USB_TO_XXX_EN && USB_TO_SBW_EN)
#define INTERFACE_SPI_EN				((USB_TO_XXX_EN && USB_TO_SPI_EN) || AVR_ISP_EN || (MP_EN && MP_S5X_EN))
#define INTERFACE_SWIM_EN				((USB_TO_XXX_EN && USB_TO_SWIM_EN) || (MP_EN && MP_STM8_EN))
#define INTERFACE_USART_EN				(USB_TO_XXX_EN && USB_TO_USART_EN)

/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       hw_cfg_VSFCore.h                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    hardware configuration file for VSFCore                   *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2008-11-22:     rewrite GPIO_Dir(by SimonQian)                    *
 **************************************************************************/

#ifndef HSE_VALUE
#define HSE_VALUE						((uint32_t)12000000)
#endif
#define OSC_HZ							HSE_VALUE

#define _SYS_FREQUENCY					72		// in MHz
#define _SYS_FLASH_VECTOR_TABLE_SHIFT	FLASH_LOAD_OFFSET // From board_defs.mk

/****************************** Abilities ******************************/
#define HW_HAS_USART					1
#define HW_HAS_SPI						1
#define HW_HAS_SDIO						1
#define HW_HAS_EBI						1
#define HW_HAS_IIC						1
#define HW_HAS_GPIO						1
#define HW_HAS_CAN						1
#define HW_HAS_PWM						1
#define HW_HAS_ADC						1
#define HW_HAS_DAC						1
#define HW_HAS_MICROWIRE				0
#define HW_HAS_DUSI						0
#define HW_HAS_JTAG						0
#define HW_HAS_ISSP						0
#define HW_HAS_C2						0
#define HW_HAS_MSP430_JTAG				0
#define HW_HAS_MSP430_SBW				0
#define HW_HAS_LPCICP					0
#define HW_HAS_SWD						0
#define HW_HAS_SWIM						0
#define HW_HAS_BDM						0
#define HW_HAS_POWERCONTROL				0

#define HW_HAS_BEEPER					0
#define HW_HAS_LEDARRAY					0
#define HW_HAS_7COLOR_LED				0

/****************************** SPI ******************************/
#define SPI_Interface_Idx				1

#define SPI_MOSI_SET()					JTAG_TAP_TDI_SET()
#define SPI_MOSI_CLR()					JTAG_TAP_TDI_CLR()
#define SPI_MOSI_GET()					JTAG_TAP_TDI_GET()
#define SPI_MOSI_SETOUTPUT()			JTAG_TAP_TDI_SETOUTPUT()
#define SPI_MOSI_SETINPUT()				JTAG_TAP_TDI_SETINPUT()

#define SPI_MISO_GET()					JTAG_TAP_TDO_GET()
#define SPI_MISO_SETINPUT()				JTAG_TAP_TDO_SETINPUT()
#define SPI_MISO_SETINPUT_PU()			GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN, GPIO_MODE_IPU)

#define SPI_SCK_SET()					JTAG_TAP_TCK_SET()
#define SPI_SCK_CLR()					JTAG_TAP_TCK_CLR()
#define SPI_SCK_GET()					JTAG_TAP_TCK_GET()
#define SPI_SCK_SETOUTPUT()				JTAG_TAP_TCK_SETOUTPUT()
#define SPI_SCK_SETINPUT()				JTAG_TAP_TCK_SETINPUT()

#define SPI_SS_SETOUTPUT()				SW_RST_SETOUTPUT()
#define SPI_SS_SETINPUT()				SW_RST_SETINPUT()
#define SPI_SS_SET()					SW_RST_SET()
#define SPI_SS_CLR()					SW_RST_CLR()

#define SPI_Disable()					SPI_I2S_DeInit(SPI_Interface)
#define SPI_SetData(d)					SPI_Interface->DR = (d)
#define SPI_GetData()					SPI_Interface->DR
#define SPI_WaitReady()					while((SPI_Interface->SR & SPI_I2S_FLAG_BSY))
#define SPI_WaitRxReady()				while(!(SPI_Interface->SR & SPI_I2S_FLAG_RXNE))
#define SPI_WaitTxReady()				while(!(SPI_Interface->SR & SPI_I2S_FLAG_TXE))

#define SPI_AllInput()					do{\
											SPI_SCK_SETINPUT();\
											SPI_MISO_SETINPUT();\
											SPI_MOSI_SETINPUT();\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, DISABLE);\
										}while(0)
#define SPI_AllSPIIO()					do{\
											SPI_SCK_SETOUTPUT();\
											SPI_MOSI_SETOUTPUT();\
											SPI_MISO_SETINPUT_PU();\
										}while(0)
#define SPI_AllSPIHW()					do{\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, ENABLE);\
											GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TCK_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDO_PIN, GPIO_MODE_AF_PP);\
											GPIO_SetMode(JTAG_TAP_PORT, JTAG_TAP_TDI_PIN, GPIO_MODE_AF_PP);\
										}while(0)

/****************************** IIC ******************************/
#define IIC_PORT						JTAG_TAP_PORT
#define IIC_SCL_PIN						JTAG_TAP_TMS_PIN
#define IIC_SDA_PIN						JTAG_TAP_TDO_PIN

#define IIC_PULL_INIT()					

#define IIC_SCL_INIT()					GPIO_SetMode(IIC_PORT, IIC_SCL_PIN, GPIO_MODE_OUT_OD)
#define IIC_SCL_FINI()					GPIO_SetMode(IIC_PORT, IIC_SCL_PIN, GPIO_MODE_IN_FLOATING)
#define IIC_SCL_SET()					GPIO_SetPins(IIC_PORT, IIC_SCL_PIN)
#define IIC_SCL_CLR()					GPIO_ClrPins(IIC_PORT, IIC_SCL_PIN)
#define IIC_SCL_GET()					GPIO_GetInPins(IIC_PORT, IIC_SCL_PIN)

#define IIC_SDA_INIT()					GPIO_SetMode(IIC_PORT, IIC_SDA_PIN, GPIO_MODE_OUT_OD)
#define IIC_SDA_FINI()					GPIO_SetMode(IIC_PORT, IIC_SDA_PIN, GPIO_MODE_IN_FLOATING)
#define IIC_SDA_SET()					GPIO_SetPins(IIC_PORT, IIC_SDA_PIN)
#define IIC_SDA_CLR()					GPIO_ClrPins(IIC_PORT, IIC_SDA_PIN)
#define IIC_SDA_GET()					GPIO_GetInPins(IIC_PORT, IIC_SDA_PIN)

/****************************** ADC ******************************/
#define TVCC_ADC_CHANNEL				8
#define TVCC_ADC_PORT					0

#define TVCC_SAMPLE_MIN_POWER			1800

#define TVCC_SAMPLE_DIV					2
#define TVCC_SAMPLE_VREF				3300
#define TVCC_SAMPLE_MAXVAL				4096

/****************************** USB *****************************/
// For USB 2.0, use DP
// For USB 1.1, use DM
#define USB_DP_PORT						0
#define USB_DP_PIN						12

#define USB_Pull_Init()					
#define USB_Connect()					
#define USB_Disconnect()				

#define USB_Disable()					PowerOff()
#define USB_D_SETOUTPUT()				do {\
											core_interfaces.gpio.init(USB_DP_PORT);\
											core_interfaces.gpio.config_pin(USB_DP_PORT, USB_DP_PIN, GPIO_OUTPP);\
										} while (0)
#define USB_D_SET()						core_interfaces.gpio.set(USB_DP_PORT, 1 << USB_DP_PIN)
#define USB_D_CLR()						core_interfaces.gpio.clear(USB_DP_PORT, 1 << USB_DP_PIN)

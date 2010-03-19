/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       hw_cfg_MiniRC3.h                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    hardware configuration file for Mini Version RC3          *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2008-11-22:     rewrite GPIO_Dir(by SimonQian)                    *
 **************************************************************************/

#define STM32_MINI_RC3					0x13
#define _HARDWARE_VER					STM32_MINI_RC3
#define HSE_Value 						((uint32_t)12000000)

/****************************** Power ******************************/
#define PWREXT_INIT()					
#define PWREXT_ENABLE()					
#define PWREXT_DISABLE()				

/****************************** Global Output ******************************/
#define GLOBAL_OUTPUT_INIT()			
#define GLOBAL_OUTPUT_ENABLE()			
#define GLOBAL_OUTPUT_DISABLE()			

/****************************** SW ******************************/
#define SW_PORT							GPIOB
#define SW_PIN							GPIO_PIN_10
#define SW_RST_PORT						GPIOB
#define SW_RST_PIN						GPIO_PIN_11
#define SYNCSW_PORT						GPIOB
#define SYNCSW_PIN						GPIO_PIN_4

#define SW_PULL_INIT()					
#define SW_RST_PULL_INIT()				

#define SW_DIR_INIT()					SW_SETINPUT_PU()
#define SW_SETINPUT_PU()				GPIO_Dir(SW_PORT, GPIO_MODE_IPU, SW_PIN)
#define SW_SETINPUT_PD()				GPIO_Dir(SW_PORT, GPIO_MODE_IPD, SW_PIN)
#define SW_SETOUTPUT()					GPIO_Dir(SW_PORT, GPIO_MODE_OUT_PP, SW_PIN)
#define SW_SET()						GPIO_SetPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))
#define SW_CLR()						GPIO_ClrPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))
#define SW_GET()						GPIO_GetInPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))

#define SW_RST_DIR_INIT()				SW_RST_SETINPUT_PU()
#define SW_RST_SETOUTPUT()				GPIO_Dir(SW_RST_PORT, GPIO_MODE_OUT_PP, SW_RST_PIN)
#define SW_RST_SETINPUT_PU()			GPIO_Dir(SW_RST_PORT, GPIO_MODE_IPU, SW_RST_PIN)
#define SW_RST_SETINPUT_PD()			GPIO_Dir(SW_RST_PORT, GPIO_MODE_IPD, SW_RST_PIN)
#define SW_RST_SET()					GPIO_SetPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))
#define SW_RST_CLR()					GPIO_ClrPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))
#define SW_RST_GET()					GPIO_GetOutPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))

#define SYNCSW_DIR_INIT()				SYNCSW_SETINPUT()
#define SYNCSW_SETINPUT()				GPIO_Dir(SYNCSW_PORT, GPIO_MODE_IPU, SYNCSW_PIN)
#define SYNCSW_SETOUTPUT()				GPIO_Dir(SYNCSW_PORT, GPIO_MODE_OUT_PP, SYNCSW_PIN)

#define SYNCSW_SET()					GPIO_SetPins(SYNCSW_PORT, GPIO_PIN_GetMask(SYNCSW_PIN))
#define SYNCSW_CLR()					GPIO_ClrPins(SYNCSW_PORT, GPIO_PIN_GetMask(SYNCSW_PIN))
#define SYNCSW_GET()					GPIO_GetInPins(SYNCSW_PORT, GPIO_PIN_GetMask(SYNCSW_PIN))

/***************************** SWJ ******************************/
#define SWJ_SWDIO_SETOUTPUT()			JTAG_TAP_TMS_SETOUTPUT()
#define SWJ_SWDIO_SETINPUT()			JTAG_TAP_TMS_SETINPUT()
#define SWJ_SWDIO_SET()					JTAG_TAP_TMS_SET()
#define SWJ_SWDIO_CLR()					JTAG_TAP_TMS_CLR()
#define SWJ_SWDIO_GET()					JTAG_TAP_TMS_GET()

#define SWJ_SWCLK_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define SWJ_SWCLK_SETINPUT()			JTAG_TAP_TCK_SETINPUT()
#define SWJ_SWCLK_SET()					JTAG_TAP_TCK_SET()
#define SWJ_SWCLK_CLR()					JTAG_TAP_TCK_CLR()

/***************************** JTAG ******************************/
#define JTAG_TAP_PORT					GPIOB
#define JTAG_TAP_TCK_PIN				GPIO_PIN_13
#define JTAG_TAP_TDO_PIN				GPIO_PIN_14
#define JTAG_TAP_TDI_PIN				GPIO_PIN_15
#define JTAG_TAP_RTCK_PORT				GPIOA
#define JTAG_TAP_RTCK_PIN				GPIO_PIN_8

#define JTAG_HAS_USER_PIN				1

#define JTAG_TAP_USR_PORT				GPIOA
#define JTAG_TAP_USR1_PIN				GPIO_PIN_9
#define JTAG_TAP_USR2_PIN				GPIO_PIN_10

#define JTAG_TAP_USR1_SETOUTPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_OUT_PP, JTAG_TAP_USR1_PIN)
#define JTAG_TAP_USR1_SETINPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_USR1_PIN)
#define JTAG_TAP_USR1_SET()				GPIO_SetPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))
#define JTAG_TAP_USR1_CLR()				GPIO_ClrPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))
#define JTAG_TAP_USR1_GET()				GPIO_GetOutPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))

#define JTAG_TAP_USR2_SETINPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_USR2_PIN)
#define JTAG_TAP_USR2_GET()				GPIO_GetInPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR2_PIN))

#define JTAG_TAP_TMS_SETOUTPUT()		SYNCSW_SETOUTPUT()
#define JTAG_TAP_TMS_SETINPUT()			SYNCSW_SETINPUT()
#define JTAG_TAP_TMS_SET()				SYNCSW_SET()
#define JTAG_TAP_TMS_CLR()				SYNCSW_CLR()
#define JTAG_TAP_TMS_GET()				SYNCSW_GET()

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
#define JTAG_TAP_TRST_SETINPUT()		SW_RST_SETINPUT_PU()
#define JTAG_TAP_TRST_SET()				SW_RST_SET()
#define JTAG_TAP_TRST_CLR()				SW_RST_CLR()
#define JTAG_TAP_TRST_GET()				SW_RST_GET()

#define JTAG_TAP_RTCK_SETINPUT()		GPIO_Dir(JTAG_TAP_RTCK_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_RTCK_PIN)
#define JTAG_TAP_RTCK_GET()				GPIO_GetInPins(JTAG_TAP_RTCK_PORT, GPIO_PIN_GetMask(JTAG_TAP_RTCK_PIN))

#define JTAG_TAP_SRST_SETOUTPUT()		SW_SETOUTPUT()
#define JTAG_TAP_SRST_SETINPUT()		SW_SETINPUT_PU()
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
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, ENABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, ENABLE);\
											GPIO_PinRemapConfig(GPIO_Remap_SPI1, ENABLE);\
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
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, DISABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, DISABLE);\
											GPIO_PinRemapConfig(GPIO_Remap_SPI1, DISABLE);\
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

/****************************** SPI ******************************/
#define SPI_Interface					SPI2

#define SPI_Conf(br)					SPI_Configuration(SPI_Interface,SPI_Mode_Master,\
														(br),SPI_FirstBit_MSB,\
														SPI_CPOL_Low,SPI_CPHA_1Edge)
#define SPI_SetClkHw(br)				do{SPI_InitStructure.SPI_BaudRatePrescaler = (br);\
										SPI_Init(SPI_Interface, &SPI_InitStructure);}while(0)

#define SPI_MOSI_SET()					JTAG_TAP_TDI_SET()
#define SPI_MOSI_CLR()					JTAG_TAP_TDI_CLR()
#define SPI_MOSI_GET()					JTAG_TAP_TDI_GET()
#define SPI_MOSI_SETOUTPUT()			JTAG_TAP_TDI_SETOUTPUT()
#define SPI_MOSI_SETINPUT()				JTAG_TAP_TDI_SETINPUT()

#define SPI_MISO_GET()					JTAG_TAP_TDO_GET()
#define SPI_MISO_SETINPUT()				JTAG_TAP_TDO_SETINPUT()
#define SPI_MISO_SETINPUT_PU()			GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_IPU, JTAG_TAP_TDO_PIN)

#define SPI_SCK_SET()					JTAG_TAP_TCK_SET()
#define SPI_SCK_CLR()					JTAG_TAP_TCK_CLR()
#define SPI_SCK_GET()					JTAG_TAP_TCK_GET()
#define SPI_SCK_SETOUTPUT()				JTAG_TAP_TCK_SETOUTPUT()
#define SPI_SCK_SETINPUT()				JTAG_TAP_TCK_SETINPUT()

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
											GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_AF_PP, JTAG_TAP_TCK_PIN);\
											GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_AF_PP, JTAG_TAP_TDO_PIN);\
											GPIO_Dir(JTAG_TAP_PORT, GPIO_MODE_AF_PP, JTAG_TAP_TDI_PIN);\
										}while(0)


/****************************** ISP ******************************/
#define ISP_SetRST()					JTAG_TAP_SRST_SET()
#define ISP_ClrRST()					JTAG_TAP_SRST_CLR()
#define ISP_GetRST()					JTAG_TAP_SRST_GET()

#define ISP_SetRSTOutput()				SW_SETOUTPUT()
#define ISP_SetRSTInput()				SW_SETINPUT_PU()

/************************** MSP430 JTAG ****************************/
#define MSP430_JTAG_TEST_SETOUTPUT()	JTAG_TAP_TRST_SETOUTPUT()
#define MSP430_JTAG_TEST_SETINPUT()		JTAG_TAP_TRST_SETINPUT()
#define MSP430_JTAG_TEST_SET()			JTAG_TAP_TRST_SET()
#define MSP430_JTAG_TEST_CLR()			JTAG_TAP_TRST_CLR()
#define MSP430_JTAG_TEST_GET()			JTAG_TAP_TRST_GET()

/************************** MSP430 SBW ****************************/
#define MSP430_SBWTDIO_SETOUTPUT()		SW_SETOUTPUT()
#define MSP430_SBWTDIO_SETINPUT()		SW_SETINPUT_PU()
#define MSP430_SBWTDIO_SET()			SW_SET()
#define MSP430_SBWTDIO_CLR()			SW_CLR()
#define MSP430_SBWTDIO_GET()			SW_GET()

#define MSP430_SBWTCK_SETOUTPUT()		MSP430_JTAG_TCK_SETOUTPUT()
#define MSP430_SBWTCK_SETINPUT()		MSP430_JTAG_TCK_SETINPUT()
#define MSP430_SBWTCK_SET()				MSP430_JTAG_TCK_SET()
#define MSP430_SBWTCK_CLR()				MSP430_JTAG_TCK_CLR()

/************************** PSoC1 ISSP ****************************/
#define ISSP_SDATA_SET()				SW_SET()
#define ISSP_SDATA_CLR()				SW_CLR()
#define ISSP_SDATA_GET()				SW_GET()
#define ISSP_SDATA_SETOUTPUT()			SW_SETOUTPUT()
#define ISSP_SDATA_SETINPUT()			SW_SETINPUT_PD()

#define ISSP_SCLK_SET()					JTAG_TAP_TCK_SET()
#define ISSP_SCLK_CLR()					JTAG_TAP_TCK_CLR()
#define ISSP_SCLK_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define ISSP_SCLK_SETINPUT()			JTAG_TAP_TCK_SETINPUT()

#define ISSP_XRES_SET()					SW_RST_SET()
#define ISSP_XRES_CLR()					SW_RST_CLR()
#define ISSP_XRES_SETOUTPUT()			SW_RST_SETOUTPUT()
#define ISSP_XRES_SETINPUT()			SW_RST_SETINPUT_PU()

#define ISSP_PowerOn()					PWREXT_ENABLE()
#define ISSP_PowerOff()					PWREXT_DISABLE()

/****************************** C2 ******************************/
#define C2_C2CK_SET()					JTAG_TAP_TCK_SET()
#define C2_C2CK_CLR()					JTAG_TAP_TCK_CLR()
#define C2_C2CK_SETOUTPUT()				JTAG_TAP_TCK_SETOUTPUT()
#define C2_C2CK_SETINPUT()				JTAG_TAP_TCK_SETINPUT()

#define C2_C2D_SET()					SW_SET()
#define C2_C2D_CLR()					SW_CLR()
#define C2_C2D_GET()					SW_GET()
#define C2_C2D_SETOUTPUT()				SW_SETOUTPUT()
#define C2_C2D_SETINPUT()				SW_SETINPUT_PU()

/****************************** LPCICP ******************************/
#define LPCICP_SDATA_SET()				SW_SET()
#define LPCICP_SDATA_CLR()				SW_CLR()
#define LPCICP_SDATA_GET()				SW_GET()
#define LPCICP_SDATA_SETOUTPUT()		SW_SETOUTPUT()
#define LPCICP_SDATA_SETINPUT()			SW_SETINPUT_PU()

#define LPCICP_SCLK_SET()				JTAG_TAP_TCK_SET()
#define LPCICP_SCLK_CLR()				JTAG_TAP_TCK_CLR()
#define LPCICP_SCLK_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define LPCICP_SCLK_SETINPUT()			JTAG_TAP_TCK_SETINPUT()

#define LPCICP_XRES_SET()				SW_RST_SET()
#define LPCICP_XRES_CLR()				SW_RST_CLR()
#define LPCICP_XRES_SETOUTPUT()			SW_RST_SETOUTPUT()
#define LPCICP_XRES_SETINPUT()			SW_RST_SETINPUT_PU()

/****************************** I2C ******************************/
#define I2C_PULL_INIT()					

#define I2C_SCL_SETOUTPUT()				SW_RST_SETOUTPUT()
#define I2C_SCL_SETINPUT()				SW_RST_SETINPUT_PU()
#define I2C_SCL_SET()					I2C_SCL_SETINPUT()
#define I2C_SCL_CLR()					SW_RST_CLR()
#define I2C_SCL_GET()					SW_RST_GET()

#define I2C_SDA_SETOUTPUT()				SW_SETOUTPUT()
#define I2C_SDA_SETINPUT()				SW_SETINPUT_PU()
#define I2C_SDA_SET()					I2C_SDA_SETINPUT()
#define I2C_SDA_CLR()					SW_CLR()
#define I2C_SDA_GET()					SW_GET()

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
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_USART1, ENABLE);\
											GPIO_Dir(USART_PORT, GPIO_MODE_AF_PP, USART_TXD_PIN);\
											GPIO_Dir(USART_PORT, GPIO_MODE_IN_FLOATING, USART_RXD_PIN);\
										}while(0)
#define USART_Port_Fini()				do{\
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_USART1, DISABLE);\
											GPIO_Dir(USART_PORT, GPIO_MODE_IN_FLOATING, USART_TXD_PIN);\
											GPIO_Dir(USART_PORT, GPIO_MODE_IN_FLOATING, USART_RXD_PIN);\
											GPIO_Dir(USART_AUX_PORT, GPIO_MODE_IN_FLOATING, USART_DTR_PIN);\
											GPIO_Dir(USART_AUX_PORT, GPIO_MODE_IN_FLOATING, USART_RTS_PIN);\
										}while(0)

/****************************** ADC ******************************/
#define TVCC_PORT						GPIOB
#define TVCC_PIN						GPIO_PIN_0
#define TVCC_ADC_CHANNEL				ADC_Channel_8
#define TVCC_ADC_PORT					ADC1

#define TVCC_SAMPLE_MIN_POWER			1800

#define TVCC_SAMPLE_DIV					6
#define TVCC_SAMPLE_VREF				3300
#define TVCC_SAMPLE_MAXVAL				4096

/****************************** LED ******************************/
#define LED_PORT						GPIOB
#define LED_RED_PIN						GPIO_PIN_6
#define LED_GREEN_PIN					GPIO_PIN_7
#define LED_RED_ON()					GPIO_ClrPins(LED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#define LED_RED_OFF()					GPIO_SetPins(LED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#define LED_GREEN_ON()					GPIO_ClrPins(LED_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))
#define LED_GREEN_OFF()					GPIO_SetPins(LED_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))

#define LED_Init()						do{\
											GPIO_Dir(LED_PORT, GPIO_MODE_OUT_PP, LED_RED_PIN);\
											GPIO_Dir(LED_PORT, GPIO_MODE_OUT_PP, LED_GREEN_PIN);\
										}while(0)
#define LED_Fini()						do{\
											GPIO_Dir(LED_PORT, GPIO_MODE_IN_FLOATING, LED_RED_PIN);\
											GPIO_Dir(LED_PORT, GPIO_MODE_IN_FLOATING, LED_GREEN_PIN);\
										}while(0)

// LED_RW
#define Led_RW_Init()					
#define Led_RW_ON()						LED_GREEN_OFF()
#define Led_RW_OFF()					LED_GREEN_ON()

// LED_USB
#define LED_USB_INIT()					
#define LED_USB_ON()					LED_GREEN_ON()
#define LED_USB_OFF()					LED_GREEN_OFF()

/****************************** KEY ******************************/
#define KEY_PORT						GPIOC
#define KEY_PIN							GPIO_PIN_13
#define KEY_IsDown()					!GPIO_GetInPins(KEY_PORT, GPIO_PIN_GetMask(KEY_PIN))
#define KEY_Init()						do{ BKP_TamperPinCmd(DISABLE); GPIO_Dir(KEY_PORT, GPIO_MODE_IPU, KEY_PIN); }while(0)
#define KEY_Fini()						do{ GPIO_Dir(KEY_PORT, GPIO_MODE_IN_FLOATING, KEY_PIN); BKP_TamperPinCmd(ENABLE); }while(0)

/****************************** USB *****************************/
// For USB 2.0, use DP
// For USB 1.1, use DM
#define USB_DP_PORT						GPIOA
#define USB_DP_PIN						GPIO_PIN_12

#define USB_Disable()					PowerOff()
#define USB_D_SETOUTPUT()				GPIO_Dir(USB_DP_PORT, GPIO_MODE_OUT_PP, USB_DP_PIN)
#define USB_D_SET()						GPIO_SetPins(USB_DP_PORT, GPIO_PIN_GetMask(USB_DP_PIN))
#define USB_D_CLR()						GPIO_ClrPins(USB_DP_PORT, GPIO_PIN_GetMask(USB_DP_PIN))

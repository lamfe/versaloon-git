/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       hw_cfg_MiniRelease1.h                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    hardware configuration file for Mini Version Release1     *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 *      2008-11-22:     rewrite GPIO_Dir(by SimonQian)                    *
 **************************************************************************/

#define STM32_MINI_Release1				0x15
#define _HARDWARE_VER					STM32_MINI_Release1
#ifdef HSE_VALUE
#undef HSE_VALUE
#endif
#define HSE_VALUE						((uint32_t)12000000)

#define _SYS_FREQUENCY					72		// in MHz
#define _SYS_FLASH_VECTOR_TABLE_SHIFT	0x2000	// application will locate at 0x08002000

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
#define GLOBAL_OUTPUT_INIT()			
#define GLOBAL_OUTPUT_ENABLE()			
#define GLOBAL_OUTPUT_DISABLE()			

/****************************** DelayTimer ******************************/
#define DELAYTIMER_MAXDELAY_US			200000

#define DELAYTIMER_INIT()				do{\
											SysTick->CTRL &= ~SysTick_CTRL_TICKINT_Msk;\
											SysTick->CTRL |= SysTick_CLKSource_HCLK;\
											SysTick->CTRL &= ~SysTick_CTRL_ENABLE_Msk;\
											SysTick->VAL = 0;\
										} while (0)

#define DELAYTIMER_DelayUS(us)			do{\
											SysTick->LOAD = (us) * _SYS_FREQUENCY;\
											SysTick->CTRL |= SysTick_CTRL_ENABLE_Msk;\
											while (!(SysTick->CTRL & SysTick_CTRL_COUNTFLAG_Msk));\
											SysTick->CTRL &= ~SysTick_CTRL_ENABLE_Msk;\
											SysTick->VAL = 0;\
										} while (0)

/****************************** SW ******************************/
#define SW_PORT							GPIOB
#define SW_PIN							GPIO_PIN_11
#define SW_RST_PORT						GPIOB
#define SW_RST_PIN						GPIO_PIN_10
#define SYNCSW_IN_PORT					GPIOB
#define SYNCSW_IN_PIN					GPIO_PIN_6
#define SYNCSW_OUT_PORT					GPIOB
#define SYNCSW_OUT_PIN					GPIO_PIN_4

#define SW_PULL_INIT()					
#define SW_DIR_INIT()					SW_SETINPUT_PD()
#define SW_SETINPUT()					GPIO_Dir(SW_PORT, GPIO_MODE_IN_FLOATING, SW_PIN)
#define SW_SETINPUT_PU()				GPIO_Dir(SW_PORT, GPIO_MODE_IPU, SW_PIN)
#define SW_SETINPUT_PD()				GPIO_Dir(SW_PORT, GPIO_MODE_IPD, SW_PIN)
#define SW_SETOUTPUT()					GPIO_Dir(SW_PORT, GPIO_MODE_OUT_PP, SW_PIN)
#define SW_SETOUTPUT_OD()				GPIO_Dir(SW_PORT, GPIO_MODE_OUT_OD, SW_PIN)
#define SW_SET()						GPIO_SetPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))
#define SW_CLR()						GPIO_ClrPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))
#define SW_GET()						GPIO_GetInPins(SW_PORT, GPIO_PIN_GetMask(SW_PIN))

#define SW_RST_PULL_INIT()				
#define SW_RST_DIR_INIT()				SW_RST_SETINPUT_PD()
#define SW_RST_SETINPUT_PU()			GPIO_Dir(SW_RST_PORT, GPIO_MODE_IPU, SW_RST_PIN)
#define SW_RST_SETINPUT_PD()			GPIO_Dir(SW_RST_PORT, GPIO_MODE_IPD, SW_RST_PIN)
#define SW_RST_SETOUTPUT()				GPIO_Dir(SW_RST_PORT, GPIO_MODE_OUT_PP, SW_RST_PIN)
#define SW_RST_SET()					GPIO_SetPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))
#define SW_RST_CLR()					GPIO_ClrPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))
#define SW_RST_GET()					GPIO_GetInPins(SW_RST_PORT, GPIO_PIN_GetMask(SW_RST_PIN))

#define SYNCSW_DIR_INIT()				do{\
											SYNCSW_SETINPUT();\
											GPIO_Dir(SYNCSW_IN_PORT, GPIO_MODE_IPU, SYNCSW_IN_PIN);\
										}while(0)
#define SYNCSW_SETINPUT()				GPIO_Dir(SYNCSW_OUT_PORT, GPIO_MODE_IN_FLOATING, SYNCSW_OUT_PIN)
#define SYNCSW_SETOUTPUT()				GPIO_Dir(SYNCSW_OUT_PORT, GPIO_MODE_OUT_PP, SYNCSW_OUT_PIN)
#define SYNCSW_SET()					GPIO_SetPins(SYNCSW_OUT_PORT, GPIO_PIN_GetMask(SYNCSW_OUT_PIN))
#define SYNCSW_CLR()					GPIO_ClrPins(SYNCSW_OUT_PORT, GPIO_PIN_GetMask(SYNCSW_OUT_PIN))
#define SYNCSW_GET()					GPIO_GetInPins(SYNCSW_IN_PORT, GPIO_PIN_GetMask(SYNCSW_IN_PIN))

// SYNCSW in PWM mode
#define SYNCSWPWM_GPIO_PORT				GPIOB
#define SYNCSWPWM_GPIO_PIN				GPIO_PIN_6

#define SYNCSWPWM_OUT_TIMER				TIM3
#define SYNCSWPWM_OUT_TIMER_DMA			DMA1_Channel6
#define SYNCSWPWM_IN_TIMER				TIM4
#define SYNCSWPWM_IN_TIMER_DMA			DMA1_Channel4

#define SYNCSWPWM_IN_TIMER_INIT()		do{\
											DMA_InitTypeDef DMA_InitStructure;\
											TIM_ICInitTypeDef TIM_ICInitStructure;\
											\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);\
											\
											DMA_DeInit(SYNCSWPWM_IN_TIMER_DMA);\
											DMA_StructInit(&DMA_InitStructure);\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&(SYNCSWPWM_IN_TIMER->CCR2);\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;\
											DMA_InitStructure.DMA_BufferSize = 0;\
											DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
											DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
											DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;\
											DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;\
											DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
											DMA_InitStructure.DMA_Priority = DMA_Priority_High;\
											DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
											DMA_Init(SYNCSWPWM_IN_TIMER_DMA, &DMA_InitStructure);\
											DMA_Cmd(SYNCSWPWM_IN_TIMER_DMA, ENABLE);\
											\
											TIM_ICStructInit(&TIM_ICInitStructure);\
											TIM_ICInitStructure.TIM_Channel = TIM_Channel_2;\
											TIM_ICInitStructure.TIM_ICPolarity = TIM_ICPolarity_Rising;\
											TIM_ICInitStructure.TIM_ICSelection = TIM_ICSelection_IndirectTI;\
											TIM_ICInitStructure.TIM_ICPrescaler = TIM_ICPSC_DIV1;\
											TIM_ICInitStructure.TIM_ICFilter = 0;\
											TIM_PWMIConfig(SYNCSWPWM_IN_TIMER, &TIM_ICInitStructure);\
											\
											TIM_SelectInputTrigger(SYNCSWPWM_IN_TIMER, TIM_TS_TI1FP1);\
											TIM_SelectSlaveMode(SYNCSWPWM_IN_TIMER, TIM_SlaveMode_Reset);\
											TIM_SelectMasterSlaveMode(SYNCSWPWM_IN_TIMER, TIM_MasterSlaveMode_Enable);\
											TIM_DMACmd(SYNCSWPWM_IN_TIMER, TIM_DMA_CC2, ENABLE);\
											TIM_Cmd(SYNCSWPWM_IN_TIMER, ENABLE);\
										}while(0)
#define SYNCSWPWM_IN_TIMER_FINI()		do{\
											TIM_DeInit(SYNCSWPWM_IN_TIMER);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, DISABLE);\
											DMA_DeInit(SYNCSWPWM_IN_TIMER_DMA);\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);\
										}while(0)
#define SYNCSWPWM_IN_TIMER_DMA_INIT(l, a)	do{\
												SYNCSWPWM_IN_TIMER_DMA->CCR &= ~1;\
												SYNCSWPWM_IN_TIMER_DMA->CNDTR = (l);\
												SYNCSWPWM_IN_TIMER_DMA->CMAR = (uint32_t)(a);\
												SYNCSWPWM_IN_TIMER_DMA->CCR |= 1;\
											}while(0)
#define SYNCSWPWM_IN_TIMER_DMA_WAIT(dly)	do{\
												while((!(DMA1->ISR & DMA1_FLAG_TC4)) && --dly);\
												DMA1->IFCR = DMA1_FLAG_TC4;\
											}while(0)

#define SYNCSWPWM_OUT_TIMER_INIT()		do{\
											DMA_InitTypeDef DMA_InitStructure;\
											TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;\
											TIM_OCInitTypeDef TIM_OCInitStructure;\
											\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);\
											\
											DMA_DeInit(SYNCSWPWM_OUT_TIMER_DMA);\
											DMA_StructInit(&DMA_InitStructure);\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&(SYNCSWPWM_OUT_TIMER->CCR1);\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;\
											DMA_InitStructure.DMA_BufferSize = 0;\
											DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
											DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
											DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;\
											DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;\
											DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
											DMA_InitStructure.DMA_Priority = DMA_Priority_Medium;\
											DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
											DMA_Init(SYNCSWPWM_OUT_TIMER_DMA, &DMA_InitStructure);\
											DMA_Cmd(SYNCSWPWM_OUT_TIMER_DMA, ENABLE);\
											\
											TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);\
											TIM_TimeBaseStructure.TIM_Prescaler = 0;\
											TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;\
											TIM_TimeBaseStructure.TIM_Period = 0;\
											TIM_TimeBaseStructure.TIM_ClockDivision = 0;\
											TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;\
											TIM_TimeBaseInit(SYNCSWPWM_OUT_TIMER, &TIM_TimeBaseStructure);\
											\
											TIM_OCStructInit(&TIM_OCInitStructure);\
											TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;\
											TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;\
											TIM_OCInitStructure.TIM_Pulse = 0;\
											TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_Low;\
											TIM_OC1Init(SYNCSWPWM_OUT_TIMER, &TIM_OCInitStructure);\
											\
											TIM_OC1PreloadConfig(SYNCSWPWM_OUT_TIMER, TIM_OCPreload_Enable);\
											TIM_ARRPreloadConfig(SYNCSWPWM_OUT_TIMER, ENABLE);\
											TIM_DMACmd(SYNCSWPWM_OUT_TIMER, TIM_DMA_CC1, ENABLE);\
											TIM_Cmd(SYNCSWPWM_OUT_TIMER, ENABLE);\
											TIM_CtrlPWMOutputs(SYNCSWPWM_OUT_TIMER, ENABLE);\
										}while(0)
#define SYNCSWPWM_OUT_TIMER_FINI()		do{\
											TIM_DeInit(SYNCSWPWM_OUT_TIMER);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, DISABLE);\
											DMA_DeInit(SYNCSWPWM_OUT_TIMER_DMA);\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);\
										}while(0)
#define SYNCSWPWM_OUT_TIMER_SetCycle(cycle)	do {\
												SYNCSWPWM_OUT_TIMER->ARR = (cycle);\
												SYNCSWPWM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;\
											} while (0)
#define SYNCSWPWM_OUT_TIMER_GetCycle(cycle)	SYNCSWPWM_OUT_TIMER->ARR
#define SYNCSWPWM_OUT_TIMER_DMA_INIT(l, a)	do{\
												SYNCSWPWM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;\
												SYNCSWPWM_OUT_TIMER_DMA->CCR &= ~1;\
												SYNCSWPWM_OUT_TIMER_DMA->CNDTR = (l);\
												SYNCSWPWM_OUT_TIMER_DMA->CMAR = (uint32_t)(a);\
												SYNCSWPWM_OUT_TIMER_DMA->CCR |= 1;\
											}while(0)
#define SYNCSWPWM_OUT_TIMER_DMA_WAIT()	do{\
											while(!(DMA1->ISR & DMA1_FLAG_TC6));\
											DMA1->IFCR = DMA1_FLAG_TC6;\
										}while(0)

#define SYNCSWPWM_PORT_INIT()			GPIO_PinRemapConfig(GPIO_PartialRemap_TIM3, ENABLE)
#define SYNCSWPWM_PORT_FINI()			GPIO_PinRemapConfig(GPIO_PartialRemap_TIM3, DISABLE)

#define SYNCSWPWM_PORT_OD_INIT()		do{\
											SYNCSWPWM_PORT_INIT();\
											GPIO_SetPins(SYNCSW_OUT_PORT, GPIO_PIN_GetMask(SYNCSW_OUT_PIN));\
											GPIO_Dir(SYNCSW_OUT_PORT, GPIO_MODE_AF_OD, SYNCSW_OUT_PIN);\
										}while(0)
#define SYNCSWPWM_PORT_OD_FINI()		do{\
											GPIO_Dir(SYNCSW_OUT_PORT, GPIO_MODE_IN_FLOATING, SYNCSW_OUT_PIN);\
											SYNCSWPWM_PORT_FINI();\
										}while(0)

#define SYNCSWPWM_PORT_PP_INIT()		do{\
											SYNCSWPWM_PORT_INIT();\
											GPIO_SetPins(SYNCSW_OUT_PORT, GPIO_PIN_GetMask(SYNCSW_OUT_PIN));\
											GPIO_Dir(SYNCSW_OUT_PORT, GPIO_MODE_AF_PP, SYNCSW_OUT_PIN);\
										}while(0)
#define SYNCSWPWM_PORT_PP_FINI()		do{\
											GPIO_Dir(SYNCSW_OUT_PORT, GPIO_MODE_IN_FLOATING, SYNCSW_OUT_PIN);\
											SYNCSWPWM_PORT_FINI();\
										}while(0)

/***************************** STM8_SWIM ******************************/
#define SWIM_SET()						GPIO_Dir(SYNCSWPWM_GPIO_PORT, GPIO_MODE_IPU, SYNCSWPWM_GPIO_PIN)
#define SWIM_CLR()						do{\
											GPIO_ClrPins(SYNCSWPWM_GPIO_PORT, GPIO_PIN_GetMask(SYNCSWPWM_GPIO_PIN));\
											GPIO_Dir(SYNCSWPWM_GPIO_PORT, GPIO_MODE_OUT_PP, SYNCSWPWM_GPIO_PIN);\
										} while (0)
#define SWIM_GET()						GPIO_GetInPins(SYNCSWPWM_GPIO_PORT, GPIO_PIN_GetMask(SYNCSWPWM_GPIO_PIN))

/***************************** BDM ******************************/
#define BDM_SET()						SWIM_SET()
#define BDM_CLR()						SWIM_CLR()
#define BDM_GET()						SWIM_GET()

/***************************** SWD ******************************/
#define SWD_SWDIO_SETOUTPUT()			JTAG_TAP_TMS_SETOUTPUT()
#define SWD_SWDIO_SETINPUT()			JTAG_TAP_TMS_SETINPUT()
#define SWD_SWDIO_SET()					JTAG_TAP_TMS_SET()
#define SWD_SWDIO_CLR()					JTAG_TAP_TMS_CLR()
#define SWD_SWDIO_GET()					JTAG_TAP_TMS_GET()

#define SWD_SWCLK_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define SWD_SWCLK_SETINPUT()			JTAG_TAP_TCK_SETINPUT()
#define SWD_SWCLK_SET()					JTAG_TAP_TCK_SET()
#define SWD_SWCLK_CLR()					JTAG_TAP_TCK_CLR()

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
#define JTAG_TAP_USR1_GET()				GPIO_GetInPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR1_PIN))

#define JTAG_TAP_USR2_SETOUTPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_OUT_PP, JTAG_TAP_USR2_PIN)
#define JTAG_TAP_USR2_SETINPUT()		GPIO_Dir(JTAG_TAP_USR_PORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_USR2_PIN)
#define JTAG_TAP_USR2_SET()				GPIO_SetPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR2_PIN))
#define JTAG_TAP_USR2_CLR()				GPIO_ClrPins(JTAG_TAP_USR_PORT, GPIO_PIN_GetMask(JTAG_TAP_USR2_PIN))
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

#define JTAG_TAP_HS_SPIS_Disable()		SPI_Cmd(JTAG_TAP_HS_SPI_S, DISABLE)
#define JTAG_TAP_HS_SPIS_Enable()		SPI_Cmd(JTAG_TAP_HS_SPI_S, ENABLE)

#define JTAG_TAP_HS_DMA_FINI()			do{\
											DMA_DeInit(JTAG_TAP_HS_SPI_M_RX_DMA);\
											DMA_DeInit(JTAG_TAP_HS_SPI_M_TX_DMA);\
											DMA_DeInit(JTAG_TAP_HS_SPI_S_TX_DMA);\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);\
										} while (0)
#define JTAG_TAP_HS_DMA_INIT()			do{\
											DMA_InitTypeDef  DMA_InitStructure;\
											\
											RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);\
											\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (u32)&JTAG_TAP_HS_SPI_M->DR;\
											DMA_InitStructure.DMA_MemoryBaseAddr = (uint32)0;\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;\
											DMA_InitStructure.DMA_BufferSize = 0;\
											DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;\
											DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;\
											DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;\
											DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_Byte;\
											DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;\
											DMA_InitStructure.DMA_Priority = DMA_Priority_VeryHigh;\
											DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;\
											DMA_Init(JTAG_TAP_HS_SPI_M_RX_DMA, &DMA_InitStructure);\
											\
											DMA_InitStructure.DMA_PeripheralBaseAddr = (u32)&JTAG_TAP_HS_SPI_S->DR;\
											DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;\
											DMA_Init(JTAG_TAP_HS_SPI_S_TX_DMA, &DMA_InitStructure);\
											\
											JTAG_TAP_HS_SPI_EnableDMA();\
										} while (0)

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

#define JTAG_TAP_HS_PortIOInit()		do{\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_AF_PP, JTAG_TAP_TCK_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_AF_PP, JTAG_TAP_TDO_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_AF_PP, JTAG_TAP_TDI_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_AF_PP, JTAG_TAP_TCK1_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_AF_PP, JTAG_TAP_TMS_PIN);\
										}while(0)

#define JTAG_TAP_HS_PortIOFini()		do{\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TCK_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TDO_PIN);\
											GPIO_Dir(JTAG_TAP_HS_MPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TDI_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TCK1_PIN);\
											GPIO_Dir(JTAG_TAP_HS_SPORT, GPIO_MODE_IN_FLOATING, JTAG_TAP_TMS_PIN);\
										}while(0)

#define JTAG_TAP_HS_PortInit()			do{\
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, ENABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, ENABLE);\
											GPIO_PinRemapConfig(GPIO_Remap_SPI1, ENABLE);\
											JTAG_TAP_HS_PortIOInit();\
										}while(0)
#define JTAG_TAP_HS_PortFini()			do{\
											JTAG_TAP_HS_PortIOFini();\
											RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, DISABLE);\
											RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2, DISABLE);\
											GPIO_PinRemapConfig(GPIO_Remap_SPI1, DISABLE);\
										}while(0)

#define JTAG_TAP_HS_SetSpeed(div)		do{\
											SPI_Configuration(JTAG_TAP_HS_SPI_M, SPI_Mode_Master, (div),\
																SPI_FirstBit_LSB, SPI_CPOL_High, SPI_CPHA_2Edge);\
											SPI_Configuration(JTAG_TAP_HS_SPI_S, SPI_Mode_Slave,SPI_BaudRatePrescaler_2, \
																SPI_FirstBit_LSB, SPI_CPOL_High, SPI_CPHA_2Edge);\
										}while(0)

#define JTAG_TAP_HS_TMS_Out(tms)		JTAG_TAP_HS_SPI_S->DR = (tms)
#define JTAG_TAP_HS_TDI_Out(tdi)		JTAG_TAP_HS_SPI_M->DR = (tdi)
#define JTAG_TAP_HS_Out(tms, tdi)		do{JTAG_TAP_HS_TMS_Out(tms); JTAG_TAP_HS_TDI_Out(tdi);}while(0)
#define JTAG_TAP_HS_In()				JTAG_TAP_HS_SPI_M->DR

#define JTAG_TAP_HS_WaitTxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_TXE))
#define JTAG_TAP_HS_WaitRxReady()		while(!(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_RXNE))
#define JTAG_TAP_HS_WaitReady()			while(JTAG_TAP_HS_SPI_M->SR & SPI_I2S_FLAG_BSY)

/****************************** SPI ******************************/
#define SPI_Interface					SPI2

#define SPI_Conf(br, fb, cpol, cpha)	SPI_Configuration(SPI_Interface,SPI_Mode_Master,\
															(br),(fb),(cpol),(cpha))

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


/****************************** Reset ******************************/
#define RST_SET()						SW_SET()
#define RST_CLR()						SW_CLR()
#define RST_GET()						SW_GET()

#define RST_SETOUTPUT()					SW_SETOUTPUT()
#define RST_SETINPUT()					SW_SETINPUT_PU()

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
#define LPCICP_PDA_SET()				SW_SET()
#define LPCICP_PDA_CLR()				SW_CLR()
#define LPCICP_PDA_GET()				SW_GET()
#define LPCICP_PDA_SETOUTPUT()			SW_SETOUTPUT()
#define LPCICP_PDA_SETINPUT()			SW_SETINPUT_PU()

#define LPCICP_PCL_SET()				JTAG_TAP_TCK_SET()
#define LPCICP_PCL_CLR()				JTAG_TAP_TCK_CLR()
#define LPCICP_PCL_SETOUTPUT()			JTAG_TAP_TCK_SETOUTPUT()
#define LPCICP_PCL_SETINPUT()			JTAG_TAP_TCK_SETINPUT()

#define LPCICP_RST_SET()				SW_RST_SET()
#define LPCICP_RST_CLR()				SW_RST_CLR()
#define LPCICP_RST_SETOUTPUT()			SW_RST_SETOUTPUT()
#define LPCICP_RST_SETINPUT()			SW_RST_SETINPUT_PU()

/****************************** IIC ******************************/
#define IIC_PORT						JTAG_TAP_PORT
#define IIC_SCL_PIN						JTAG_TAP_TCK_PIN
#define IIC_SDA_PIN						JTAG_TAP_TDI_PIN

#define IIC_PULL_INIT()					

#define IIC_SCL_SETOUTPUT()				GPIO_Dir(IIC_PORT, GPIO_MODE_OUT_PP, IIC_SCL_PIN)
#define IIC_SCL_SETINPUT()				GPIO_Dir(IIC_PORT, GPIO_MODE_IPU, IIC_SCL_PIN)
#define IIC_SCL_SET()					IIC_SCL_SETINPUT()
#define IIC_SCL_CLR()					do{\
											GPIO_ClrPins(IIC_PORT, GPIO_PIN_GetMask(IIC_SCL_PIN));\
											IIC_SCL_SETOUTPUT();\
										}while(0)
#define IIC_SCL_GET()					GPIO_GetInPins(IIC_PORT, GPIO_PIN_GetMask(IIC_SCL_PIN))

#define IIC_SDA_SETOUTPUT()				GPIO_Dir(IIC_PORT, GPIO_MODE_OUT_PP, IIC_SDA_PIN)
#define IIC_SDA_SETINPUT()				GPIO_Dir(IIC_PORT, GPIO_MODE_IPU, IIC_SDA_PIN)
#define IIC_SDA_SET()					IIC_SDA_SETINPUT()
#define IIC_SDA_CLR()					do{\
											GPIO_ClrPins(IIC_PORT, GPIO_PIN_GetMask(IIC_SDA_PIN));\
											IIC_SDA_SETOUTPUT();\
										}while(0)
#define IIC_SDA_GET()					GPIO_GetInPins(IIC_PORT, GPIO_PIN_GetMask(IIC_SDA_PIN))

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
#define USART_AUX_Port_Fini()			do{\
											GPIO_Dir(USART_AUX_PORT, GPIO_MODE_IN_FLOATING, USART_DTR_PIN);\
											GPIO_Dir(USART_AUX_PORT, GPIO_MODE_IN_FLOATING, USART_RTS_PIN);\
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

#define TVCC_SAMPLE_DIV					2
#define TVCC_SAMPLE_VREF				3300
#define TVCC_SAMPLE_MAXVAL				4096

/****************************** LED ******************************/
#define LED_RED_PORT					GPIOA
#define LED_RED_PIN						GPIO_PIN_15
#define LED_GREEN_PORT					GPIOB
#define LED_GREEN_PIN					GPIO_PIN_5
#define LED_RED_ON()					GPIO_ClrPins(LED_RED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#define LED_RED_OFF()					GPIO_SetPins(LED_RED_PORT, GPIO_PIN_GetMask(LED_RED_PIN))
#define LED_GREEN_ON()					GPIO_ClrPins(LED_GREEN_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))
#define LED_GREEN_OFF()					GPIO_SetPins(LED_GREEN_PORT, GPIO_PIN_GetMask(LED_GREEN_PIN))

#define LED_Init()						do{\
											GPIO_Dir(LED_RED_PORT, GPIO_MODE_OUT_PP, LED_RED_PIN);\
											GPIO_Dir(LED_GREEN_PORT, GPIO_MODE_OUT_PP, LED_GREEN_PIN);\
										}while(0)
#define LED_Fini()						do{\
											GPIO_Dir(LED_RED_PORT, GPIO_MODE_IN_FLOATING, LED_RED_PIN);\
											GPIO_Dir(LED_GREEN_PORT, GPIO_MODE_IN_FLOATING, LED_GREEN_PIN);\
										}while(0)

// LED_RW
#define LED_RW_PORT						GPIOA
#define LED_RW_PIN						GPIO_PIN_14
#define Led_RW_Init()					GPIO_Dir(LED_RW_PORT, GPIO_MODE_OUT_PP, LED_RW_PIN)
#define Led_RW_ON()						GPIO_ClrPins(LED_RW_PORT, GPIO_PIN_GetMask(LED_RW_PIN))
#define Led_RW_OFF()					GPIO_SetPins(LED_RW_PORT, GPIO_PIN_GetMask(LED_RW_PIN))

// LED_USB
#define LED_USB_INIT()					
#define LED_USB_ON()					Led_RW_OFF()
#define LED_USB_OFF()					Led_RW_ON()

/****************************** KEY ******************************/
#define KEY_PORT						GPIOB
#define KEY_PIN							GPIO_PIN_9
#define KEY_IsDown()					!GPIO_GetInPins(KEY_PORT, GPIO_PIN_GetMask(KEY_PIN))
#define KEY_Init()						GPIO_Dir(KEY_PORT, GPIO_MODE_IPU, KEY_PIN)
#define KEY_Fini()						GPIO_Dir(KEY_PORT, GPIO_MODE_IN_FLOATING, KEY_PIN)

/****************************** USB *****************************/
// For USB 2.0, use DP
// For USB 1.1, use DM
#define USB_DP_PORT						GPIOA
#define USB_DP_PIN						GPIO_PIN_12

#define USB_Pull_Init()					
#define USB_Connect()					
#define USB_Disconnect()				

#define USB_Disable()					PowerOff()
#define USB_D_SETOUTPUT()				GPIO_Dir(USB_DP_PORT, GPIO_MODE_OUT_PP, USB_DP_PIN)
#define USB_D_SET()						GPIO_SetPins(USB_DP_PORT, GPIO_PIN_GetMask(USB_DP_PIN))
#define USB_D_CLR()						GPIO_ClrPins(USB_DP_PORT, GPIO_PIN_GetMask(USB_DP_PIN))

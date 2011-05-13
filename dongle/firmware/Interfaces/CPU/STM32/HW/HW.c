#include "app_cfg.h"
#include "HW.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#include "usb_regs.h"
#include "usb_mem.h"
#define USB_DATA_SIZE		64

uint8_t buffer_out[USB_DATA_BUFF_SIZE], *buffer_in = NULL;
volatile uint32_t count_out = 0;
volatile uint32_t usb_in_data_remain = 0, usb_in_numofpackage = 0;
volatile uint32_t buffer_ptr = 0;
volatile uint32_t usb_ovf = 0;

volatile uint32_t cmd_len = 0;
volatile uint32_t rep_len = 0;

uint8_t asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
uint16_t Vtarget = 0;

void GPIO_SetMode(GPIO_TypeDef* GPIOx, uint8_t pin, uint8_t mode)
{
	uint32_t tmp_reg;

	if(pin < 8)
	{
		tmp_reg = GPIOx->CRL;
		tmp_reg &= ~(((uint32_t)0x0F) << ((pin - 0) * 4));
		tmp_reg |= (uint32_t)(mode & 0x0F) << ((pin - 0) * 4);
		GPIOx->CRL = tmp_reg;
	}
	else
	{
		tmp_reg = GPIOx->CRH;
		tmp_reg &= ~(((uint32_t)0x0F) << ((pin - 8) * 4));
		tmp_reg |= (uint32_t)(mode & 0x0F) << ((pin - 8) * 4);
		GPIOx->CRH = tmp_reg;
	}

	if(mode & 0x20)
	{
		if(mode & 0x10)
		{
			GPIOx->BSRR = (((uint32_t)0x01) << pin);
		}
		else
		{
			GPIOx->BRR = (((uint32_t)0x01) << pin);
		}
	}
}

/*******************************************************************************
* Function Name	: Sys_Init
* Description		: Initialize the system
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void Sys_Init(void)
{
	/* Configure the system clocks */
	RCC_Configuration();

	HW_INIT();

	/* NVIC Configuration */
	NVIC_Configuration();

	/* Configure the GPIO ports */
	GPIO_Configuration();

	/* Configure the ADC ports */
	ADC_Configuration();

	DELAYTIMER_INIT();
}

/*******************************************************************************
* Function Name	: RCC_Configuration
* Description		: Configures the different system clocks.
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void RCC_Configuration(void)
{
	/* RCC system reset(for debug purpose) */
	RCC_DeInit();

	/* Enable HSE */
	RCC_HSEConfig(RCC_HSE_ON);

	/* Wait till HSE is ready */
	if(RCC_WaitForHSEStartUp() == SUCCESS)
	{
		/* Enable Prefetch Buffer */
		FLASH_PrefetchBufferCmd(FLASH_PrefetchBuffer_Enable);

		/* Flash 2 wait state */
		FLASH_SetLatency(FLASH_Latency_2);
 	
		/* HCLK = SYSCLK */
		RCC_HCLKConfig(RCC_SYSCLK_Div1); 
	
		/* PCLK2 = HCLK */
		RCC_PCLK2Config(RCC_HCLK_Div1); 

		/* PCLK1 = HCLK/2 */
		RCC_PCLK1Config(RCC_HCLK_Div2);

		/* PLLCLK */
		RCC_PLLConfig(RCC_PLLSource_HSE_Div1, (_SYS_FREQUENCY * 1000000 / HSE_VALUE - 2) << 18);

		/* Enable PLL */ 
		RCC_PLLCmd(ENABLE);

		/* Wait till PLL is ready */
		while(RCC_GetFlagStatus(RCC_FLAG_PLLRDY) == RESET)
		{
		}

		/* Select PLL as system clock source */
		RCC_SYSCLKConfig(RCC_SYSCLKSource_PLLCLK);

		/* Wait till PLL is used as system clock source */
		while((RCC_GetSYSCLKSource() >> 2) != RCC_SYSCLKSource_PLLCLK)
		{
		}
	}

	/* USBCLK = PLLCLK / 1.5 */
#if _SYS_FREQUENCY == 72
	RCC_USBCLKConfig(RCC_USBCLKSource_PLLCLK_1Div5);
#elif _SYS_FREQUENCY == 48
	RCC_USBCLKConfig(RCC_USBCLKSource_PLLCLK_Div1);
#else
#	error _SYS_FREQUENCY not supported
#endif

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA | RCC_APB2Periph_GPIOB | RCC_APB2Periph_AFIO, ENABLE);

	RCC_APB1PeriphClockCmd(RCC_APB1Periph_USB, ENABLE);
}

/*******************************************************************************
* Function Name	: GPIO_Configuration
* Description		: Configures the different GPIO ports.
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void GPIO_Configuration(void)
{
	// Enable pin
	GLOBAL_OUTPUT_INIT();
	GLOBAL_OUTPUT_DISABLE();
	SW_PULL_INIT();
	SW_DIR_INIT();
	SW_RST_PULL_INIT();
	SW_RST_DIR_INIT();
	SYNCSW_DIR_INIT();

	// powerext
	PWREXT_INIT();

	// Key Init
	BKP_TamperPinCmd(DISABLE);
	KEY_Init();

	// LED Init
	Led_RW_OFF();
	Led_RW_Init();
	LED_RED_OFF();
	LED_GREEN_OFF();
	LED_Init();
	LED_USB_OFF();
	LED_USB_INIT();

#if POWER_SAMPLE_EN
	// VSample Init
	GPIO_SetMode(TVCC_PORT, TVCC_PIN, GPIO_MODE_AIN);
#endif
}

/*******************************************************************************
* Function Name	: NVIC_Configuration
* Description		: Configures Vector Table base location.
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void NVIC_Configuration(void)
{
	NVIC_InitTypeDef NVIC_InitStructure;

#ifdef	VECT_TAB_RAM	
	/* Set the Vector Table base location at 0x20000000 */ 
	NVIC_SetVectorTable(NVIC_VectTab_RAM, 0x0); 
#else	/* VECT_TAB_FLASH	*/
	NVIC_SetVectorTable(NVIC_VectTab_FLASH, _SYS_FLASH_VECTOR_TABLE_SHIFT);
#endif

	NVIC_PriorityGroupConfig(NVIC_PriorityGroup_2);

	/* Enable the USB Wake-up interrupt */
	NVIC_InitStructure.NVIC_IRQChannel = USBWakeUp_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	/* Enable the USB_LP Interrupt */
	NVIC_InitStructure.NVIC_IRQChannel = USB_LP_CAN1_RX0_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	/* Enable the USB_HP Interrupt */
	NVIC_InitStructure.NVIC_IRQChannel = USB_HP_CAN1_TX_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
}

SPI_InitTypeDef		SPI_InitStructure;
/*******************************************************************************
* Function Name	: SPI_Configuration
* Description		: Configures SPI Interface.
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void SPI_Configuration(SPI_TypeDef* SPIx, uint16_t mode, uint16_t brp, 
						uint16_t fb, uint16_t cpol, uint16_t cpha)
{
//	DeInit cannot be called here because this function will be called to set the speed of SPI
//	SPI_I2S_DeInit(SPIx);

	SPI_InitStructure.SPI_Direction = SPI_Direction_2Lines_FullDuplex;
	SPI_InitStructure.SPI_Mode = mode;
	SPI_InitStructure.SPI_DataSize = SPI_DataSize_8b;
	SPI_InitStructure.SPI_CPOL = cpol;
	SPI_InitStructure.SPI_CPHA = cpha;
	SPI_InitStructure.SPI_NSS = SPI_NSS_Soft;
	SPI_InitStructure.SPI_BaudRatePrescaler = brp;
	SPI_InitStructure.SPI_FirstBit = fb;
	SPI_InitStructure.SPI_CRCPolynomial = 1;
	SPI_Init(SPIx, &SPI_InitStructure);

	SPI_Cmd(SPIx, ENABLE);
}

/*******************************************************************************
* Function Name	: ADC_Configuration
* Description		: Configures SPI Interface.
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void ADC_Configuration(void)
{
#if POWER_SAMPLE_EN
	ADC_InitTypeDef ADC_InitStructure;

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1, ENABLE);

	ADC_DeInit(ADC1);

	ADC_InitStructure.ADC_Mode = ADC_Mode_Independent;
	ADC_InitStructure.ADC_ScanConvMode = ENABLE;
	ADC_InitStructure.ADC_ContinuousConvMode = ENABLE;
	ADC_InitStructure.ADC_ExternalTrigConv = ADC_ExternalTrigConv_None;
	ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
	ADC_InitStructure.ADC_NbrOfChannel = 1;
	ADC_Init(ADC1, &ADC_InitStructure);

	/* ADC1 regular channel7 configuration */ 
	ADC_RegularChannelConfig(ADC1, TVCC_ADC_CHANNEL, 1, ADC_SampleTime_55Cycles5);

	/* Enable ADC1 */
	ADC_Cmd(ADC1, ENABLE);

	/* Enable ADC1 reset calibaration register */   
	ADC_ResetCalibration(ADC1);
	/* Check the end of ADC1 reset calibration register */
	while(ADC_GetResetCalibrationStatus(ADC1));

	/* Start ADC1 calibaration */
	ADC_StartCalibration(ADC1);
	/* Check the end of ADC1 calibration */
	while(ADC_GetCalibrationStatus(ADC1));

	/* Start ADC1 Software Conversion */ 
	ADC_SoftwareStartConvCmd(ADC1, ENABLE);
#endif
}

// GLOBAL_OUTPUT
uint8_t GLOBAL_OUTPUT_Count = 0;
void GLOBAL_OUTPUT_Acquire(void)
{
	if(!GLOBAL_OUTPUT_Count)
	{
		GLOBAL_OUTPUT_ENABLE();
	}
	GLOBAL_OUTPUT_Count++;
}

void GLOBAL_OUTPUT_Release(void)
{
	if (GLOBAL_OUTPUT_Count)
	{
		GLOBAL_OUTPUT_Count--;
		if(!GLOBAL_OUTPUT_Count)
		{
			GLOBAL_OUTPUT_DISABLE();
		}
	}
}

uint16_t SampleVtarget(void)
{
#if POWER_SAMPLE_EN
	uint16_t tmp;

	tmp = ADC_GetConversionValue(TVCC_ADC_PORT);
	// convert target power to be in mV unit
	tmp = tmp * TVCC_SAMPLE_VREF * TVCC_SAMPLE_DIV / TVCC_SAMPLE_MAXVAL;
	return tmp;
#else
	return 3300;
#endif
}

void PWREXT_Check(uint8_t b_control_led)
{
	static uint32_t dly = 0;

	if(++dly > 0xFFFF)
	{
		dly = 0;
#if POWER_SAMPLE_EN
		Vtarget = SampleVtarget();
#if POWER_OUT_EN
		// if PowerExt is enabled and no power on the line
		if((Vtarget < TVCC_SAMPLE_MIN_POWER))
		{
			// release power
			PWREXT_ForceRelease();
		}
#endif
		if (b_control_led)
		{
			// Control LED
			if(Vtarget > TVCC_SAMPLE_MIN_POWER)	// Red LED indicate the target power
			{
				LED_RED_ON();
			}
			else
			{
				LED_RED_OFF();
			}
		}
#endif
	}
}

// Delay
void DelayUS(volatile uint32_t dly)
{
	uint32_t dly_tmp;

	while (dly)
	{
		if (dly > DELAYTIMER_MAXDELAY_US)
		{
			dly_tmp = DELAYTIMER_MAXDELAY_US;
		}
		else
		{
			dly_tmp = dly;
		}
		DELAYTIMER_DelayUS(dly_tmp);
		dly -= dly_tmp;
	}
}

void DelayMS(uint32_t dly)
{
	DelayUS(1000 * dly);
}

s32 USB_Out_IsReady(void)
{
	if (usb_in_numofpackage > 0)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

s32 USB_Out_PollReady(void)
{
	uint32_t retry;

	retry = 0;
	while(!USB_Out_IsReady())
	{
		if((retry++ > 0xFFFFF) || (usb_ovf))
		{
			// time out or
			// over flow(new command received, but old reply not sent)
			usb_ovf = 0;
			usb_in_numofpackage = 0;
			usb_in_data_remain = 0;
			SetEPTxStatus(ENDP2, EP_TX_NAK);
			return -1;
		}
	}
	return 0;
}

void USB_Out(uint8_t *data, uint32_t len)
{
	uint32_t ep_size;

	buffer_in = data;
	usb_in_numofpackage = (len + USB_DATA_SIZE - 1) / USB_DATA_SIZE;
	if(!(len % USB_DATA_SIZE))
	{
		// 1 more package for ZLP
		usb_in_numofpackage++;
	}
	usb_in_data_remain = len;

	// start to send the first package
	if(usb_in_data_remain > USB_DATA_SIZE)
	{
		ep_size = USB_DATA_SIZE;
	}
	else
	{
		ep_size = usb_in_data_remain;
	}
	UserToPMABufferCopy(buffer_in, GetEPTxAddr(ENDP2), ep_size);
	SetEPTxCount(ENDP2, ep_size);
	usb_in_data_remain -= ep_size;
	buffer_in += ep_size;
	SetEPTxValid(ENDP2);
}

/*******************************************************************************
* Function Name	: Enter_LowPowerMode
* Description		: Power-off system clocks and power while entering suspend mode
* Input					: None.
* Return				 : None.
*******************************************************************************/
void Enter_LowPowerMode(void)
{
}

/*******************************************************************************
* Function Name	: Leave_LowPowerMode
* Description		: Restores system clocks and power while exiting suspend mode
* Input					: None.
* Return				 : None.
*******************************************************************************/
void Leave_LowPowerMode(void)
{
}

#ifdef	DEBUG
/*******************************************************************************
* Function Name	: assert_failed
* Description		: Reports the name of the source file and the source line number
*									where the assert_param error has occurred.
* Input					: - file: pointer to the source file name
*									- line: assert_param error line source number
* Output				 : None
* Return				 : None
*******************************************************************************/
void assert_failed(uint8_t *file, uint32_t line)
{ 
	/* User can add his own implementation to report the file name and line number,
		 ex: printf("Wrong parameters value: file %s on line %d\r\n", file, line) */

	/* Infinite loop */
	while (1)
	{
	}
}
#endif

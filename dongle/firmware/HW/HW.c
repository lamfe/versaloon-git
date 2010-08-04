#include "app_cfg.h"
#include "HW.h"
#include "fifo.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#include "usb_lib.h"
#if USB_PROTOCOL == USB_ST_VCOM
#	include "hw_config.h"
#	include "usb_conf.h"
#	include "usb_desc.h"
#	include "usb_cdc.h"
#elif (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
#	include "hw_config_at.h"
#	include "usb_conf_at.h"
#	include "usb_desc_at.h"
#	include "usb_cdc_at.h"
#endif
#define USB_DATA_SIZE		BULK_MAX_PACKET_SIZE

#if (	((USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON))	\
		&& (USB_WITH_CDC == USB_WITH_IAD_CDC) && USB_WITH_MASSSTORAGE)				\
	|| ((USB_PROTOCOL == USB_ST_VCOM) && USB_WITH_MASSSTORAGE)
#	include "mass_mal.h"
#endif

void GPIO_Dir(GPIO_TypeDef* GPIOx, uint8 mode, uint8 pin)
{
	uint32 tmp_reg;

	if(pin < 8)
	{
		tmp_reg = GPIOx->CRL;
		tmp_reg &= ~(((u32)0x0F) << ((pin - 0) * 4));
		tmp_reg |= (u32)(mode & 0x0F) << ((pin - 0) * 4);
		GPIOx->CRL = tmp_reg;
	}
	else
	{
		tmp_reg = GPIOx->CRH;
		tmp_reg &= ~(((u32)0x0F) << ((pin - 8) * 4));
		tmp_reg |= (u32)(mode & 0x0F) << ((pin - 8) * 4);
		GPIOx->CRH = tmp_reg;
	}

	if(mode & 0x20)
	{
		if(mode & 0x10)
		{
			GPIOx->BSRR = (((u32)0x01) << pin);
		}
		else
		{
			GPIOx->BRR = (((u32)0x01) << pin);
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

	/* NVIC Configuration */
	NVIC_Configuration();

	/* Configure the GPIO ports */
	GPIO_Configuration();

	/* Configure the ADC ports */
	ADC_Configuration();

#if (	((USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON))	\
		&& (USB_WITH_CDC == USB_WITH_IAD_CDC) && USB_WITH_MASSSTORAGE)				\
	|| ((USB_PROTOCOL == USB_ST_VCOM) && USB_WITH_MASSSTORAGE)
	MAL_Init(0);
#endif

	USB_Init();
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
	// Disable JTAG
	GPIO_PinRemapConfig(GPIO_Remap_SWJ_Disable, ENABLE);

	// Enable pin
	GLOBAL_OUTPUT_DISABLE();
	GLOBAL_OUTPUT_INIT();
	SW_PULL_INIT();
	SW_DIR_INIT();
	SW_RST_PULL_INIT();
	SW_RST_DIR_INIT();
	SYNCSW_DIR_INIT();

	// powerext
	PWREXT_INIT();

#if MP_EN
	// Key Init
	BKP_TamperPinCmd(DISABLE);
	KEY_Init();
#endif

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
	GPIO_Dir(TVCC_PORT, GPIO_MODE_AIN, TVCC_PIN);
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

#if INTERFACE_SPI_EN
SPI_InitTypeDef		SPI_InitStructure;
/*******************************************************************************
* Function Name	: SPI_Configuration
* Description		: Configures SPI Interface.
* Input					: None
* Output				 : None
* Return				 : None
*******************************************************************************/
void SPI_Configuration(SPI_TypeDef* SPIx,u16 mode,u16 brp,u16 fb,u16 cpol,u16 cpha)
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
#endif

#if INTERFACE_JTAG_EN
void JTAG_DMA_Fini(void)
{
	DMA_DeInit(JTAG_TAP_HS_SPI_M_RX_DMA);
	DMA_DeInit(JTAG_TAP_HS_SPI_M_TX_DMA);
	DMA_DeInit(JTAG_TAP_HS_SPI_S_TX_DMA);
	RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, DISABLE);
}

void JTAG_DMA_Init(void)
{
	DMA_InitTypeDef  DMA_InitStructure;

	RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);
	
	DMA_InitStructure.DMA_PeripheralBaseAddr = (u32)&JTAG_TAP_HS_SPI_M->DR;
	DMA_InitStructure.DMA_MemoryBaseAddr = (uint32)0;
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;
	DMA_InitStructure.DMA_BufferSize = 0;
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;
	DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_Byte;
	DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;
	DMA_InitStructure.DMA_Priority = DMA_Priority_VeryHigh;
	DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;
	DMA_Init(JTAG_TAP_HS_SPI_M_RX_DMA, &DMA_InitStructure);

	DMA_InitStructure.DMA_PeripheralBaseAddr = (u32)&JTAG_TAP_HS_SPI_S->DR;
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;
	DMA_Init(JTAG_TAP_HS_SPI_S_TX_DMA, &DMA_InitStructure);

	JTAG_TAP_HS_SPI_EnableDMA();
}
#endif

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

void USB_Init_SerialString(uint8 *strSerial, uint16 len)
{
	uint8 *chip_serial = (uint8*)0x1FFFF7E8, tmp;
	uint16 i = 0;

	len /= 2;
	if (len > 24)
	{
		len = 24;
	}

	while(len)
	{
		tmp = (chip_serial[len - 1] & 0xF0) >> 4;
		if (tmp > 9)
		{
			strSerial[i] = tmp + 'A' - 10;
		}
		else
		{
			strSerial[i] = tmp + '0';
		}
		len--;
		if (!len)
		{
			break;
		}

		tmp = (chip_serial[len - 1] & 0x0F) >> 0;
		if (tmp > 9)
		{
			strSerial[i + 2] = tmp + 'A' - 10;
		}
		else
		{
			strSerial[i + 2] = tmp + '0';
		}
		len--;

		chip_serial++;
		i += 4;
	}
}

static uint8 __if_inited = 0;
void CDC_IF_Fini(void)
{
	NVIC_InitTypeDef NVIC_InitStructure;

	/* Disable the USART1 Interrupt */
	NVIC_InitStructure.NVIC_IRQChannel = USART_IRQ;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = DISABLE;
	NVIC_Init(&NVIC_InitStructure);

	USART_DeInit(USART_DEF_PORT);
#if USART_AUX_PORT_EN
	USART_AUX_Port_Fini();
#endif
	USART_Port_Fini();

	PWREXT_Release();
	GLOBAL_OUTPUT_Release();
	__if_inited = 0;
}

void CDC_IF_Setup(uint32 baudrate, uint8 datatype, uint8 paritytype, uint8 stopbittype)
{
	USART_InitTypeDef USART_InitStructure;
	NVIC_InitTypeDef NVIC_InitStructure;

	FIFO_Reset(&CDC_OUT_fifo);
	FIFO_Reset(&CDC_IN_fifo);

	if(!__if_inited)
	{
		__if_inited = 1;
		USART_DeInit(USART_DEF_PORT);
#if USART_AUX_PORT_EN
		USART_AUX_Port_Init();
#endif
		USART_Port_Init();
	}

	/* Enable the USART1 Interrupt */
	NVIC_InitStructure.NVIC_IRQChannel = USART_IRQ;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	USART_InitStructure.USART_BaudRate = baudrate;
	switch(stopbittype)
	{
	case 0:
		USART_InitStructure.USART_StopBits = USART_StopBits_1;
		break;
	case 1:
		USART_InitStructure.USART_StopBits = USART_StopBits_1_5;
		break;
	case 2:
		USART_InitStructure.USART_StopBits = USART_StopBits_2;
		break;
	default:
		USART_InitStructure.USART_StopBits = USART_StopBits_1;
		break;
	}
	switch(paritytype)
	{
	case 0:
		USART_InitStructure.USART_Parity = USART_Parity_No;
		USART_InitStructure.USART_WordLength = USART_WordLength_8b;
		break;
	case 1:
		USART_InitStructure.USART_Parity = USART_Parity_Odd;
		USART_InitStructure.USART_WordLength = USART_WordLength_9b;
		break;
	case 2:
		USART_InitStructure.USART_Parity = USART_Parity_Even;
		USART_InitStructure.USART_WordLength = USART_WordLength_9b;
		break;
	default:
		USART_InitStructure.USART_Parity = USART_Parity_No;
		USART_InitStructure.USART_WordLength = USART_WordLength_8b;
		break;
	}
	USART_InitStructure.USART_Mode = USART_Mode_Rx | USART_Mode_Tx;
	USART_InitStructure.USART_HardwareFlowControl = USART_HardwareFlowControl_None; 

	USART_Init(USART_DEF_PORT, &USART_InitStructure);
	USART_ITConfig(USART_DEF_PORT, USART_IT_RXNE, ENABLE);
#if !USB_CDC_BY_POLLING
	USART_ITConfig(USART_DEF_PORT, USART_IT_TC, ENABLE);
#endif
	USART_Cmd(USART_DEF_PORT, ENABLE);

	GLOBAL_OUTPUT_Acquire();
	PWREXT_Acquire();
}

extern __IO uint8 CDC_Out_En;
extern uint8 USBTOUSART_En;
void CDC_Process(void)
{
#if USB_CDC_BY_POLLING
	uint32 len;
	uint8 *buff;
	static uint32 usb_out_buff_len = 0;
	static uint32 usart_out_buff_len = 0;

	if (USART_GetFlagStatus(USART_DEF_PORT, USART_FLAG_TC) == SET)
	{
		len = FIFO_Get_Length(&CDC_OUT_fifo);
		if (len > 0)
		{
			USART_SendData(USART_DEF_PORT, FIFO_Get_Byte(&CDC_OUT_fifo));
			if (!USBTOUSART_En && !CDC_Out_En && (FIFO_Get_AvailableLength(&CDC_OUT_fifo) >= 6 * USB_DATA_SIZE))
			{
				CDC_Out_En = 1;
#if USB_RX_DOUBLEBUFFER_EN
				FreeUserBuffer(ENDP3, EP_DBUF_OUT);
#else
				SetEPRxValid(ENDP3);
#endif
			}
			usart_out_buff_len = 1;
			if (!USBTOUSART_En)
			{
				Led_RW_ON();
			}
		}
		else if(usart_out_buff_len)
		{
			usart_out_buff_len = 0;
			if (!USBTOUSART_En)
			{
				Led_RW_OFF();
			}
		}
	}

	if (!USBTOUSART_En && USB_Out_IsReady())
	{
		if (usb_out_buff_len > 0)
		{
			FIFO_Release_Consequent_Buffer(&CDC_IN_fifo, usb_out_buff_len);
		}

		len = FIFO_Get_Consequent_Buffer(&CDC_IN_fifo, &buff);
		if (len > 0)
		{
			USB_Out(buff, len);
			usb_out_buff_len = len;
			Led_RW_ON();
		}
		else if(usb_out_buff_len)
		{
			usb_out_buff_len = 0;
			USB_Out(NULL, 0);
			Led_RW_OFF();
		}
	}
#endif
}

void CDC_IF_Enable_Int(void)
{
	// CDC on USART
	// enable USART Int.
	NVIC_EnableIRQ(USART_IRQ);
}

void CDC_IF_Disable_Int(void)
{
	// CDC on USART
	// disable USART Int.
	NVIC_DisableIRQ(USART_IRQ);
}

void CDC_IF_RX_Int(uint8 dat)
{
#if !USB_CDC_BY_POLLING
	uint8 *buff;
	uint32 len;
#endif

	if(FIFO_Add_Byte(&CDC_IN_fifo, dat) != 1)
	{
		// OVERFLOW
		LED_RED_ON();
		return;
	}
#if !USB_CDC_BY_POLLING
	if(!CDC_USB_IsBusy)
	{
		len = FIFO_Get_Consequent_Buffer(&CDC_IN_fifo, &buff);
		if(len > USB_DATA_SIZE - 10)
		{
			len = USB_DATA_SIZE - 10;
		}
		CDC_USB_IsBusy = len;
		CDC_USB_Out(buff, CDC_USB_IsBusy);
		Led_RW_ON();
	}
#endif
}

void CDC_IF_TX_Int(void)
{
#if !USB_CDC_BY_POLLING
	if(FIFO_Get_Length(&CDC_OUT_fifo))
	{
		USART_SendData(USART_DEF_PORT, FIFO_Get_Byte(&CDC_OUT_fifo));
	}
	else
	{
		CDC_USART_IsBusy = 0;
		Led_RW_OFF();
	}
#endif
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
	u32 retry;

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
#if USB_TX_DOUBLEBUFFER_EN
			ToggleDTOG_RX(ENDP2);
#endif
			SetEPTxStatus(ENDP2, EP_TX_NAK);
			return -1;
		}
	}
	return 0;
}

void USB_Out(u8 *data, u32 len)
{
	u32 ep_size;

	buffer_in = data;
	usb_in_numofpackage = (len + USB_DATA_SIZE - 1) / USB_DATA_SIZE;
	if(!(len % USB_DATA_SIZE))
	{
		// 1 more package for ZLP
		usb_in_numofpackage++;
	}
	usb_in_data_remain = len;

#if USB_TX_DOUBLEBUFFER_EN
	if(GetENDPOINT(ENDP2) & EP_DTOG_RX)
	{
		if(usb_in_data_remain > USB_DATA_SIZE)
		{
			ep_size = USB_DATA_SIZE;
		}
		else
		{
			ep_size = usb_in_data_remain;
		}
		UserToPMABufferCopy(buffer_in, ENDP2_TXADDR1, ep_size);
		SetEPDblBuf1Count(ENDP2, EP_DBUF_IN, ep_size);
		FreeUserBuffer(ENDP2, EP_DBUF_IN);
		usb_in_data_remain -= ep_size;
		buffer_in += ep_size;

		if(usb_in_data_remain > 0)
		{
			if(usb_in_data_remain > USB_DATA_SIZE)
			{
				ep_size = USB_DATA_SIZE;
			}
			else
			{
				ep_size = usb_in_data_remain;
			}
			UserToPMABufferCopy(buffer_in, ENDP2_TXADDR0, ep_size);
			SetEPDblBuf0Count(ENDP2, EP_DBUF_IN, ep_size);
			usb_in_data_remain -= ep_size;
			buffer_in += ep_size;
		}
		else
		{
			SetEPDblBuf0Count(ENDP2, EP_DBUF_IN, 0);
		}
	}
	else
	{
		if(usb_in_data_remain > USB_DATA_SIZE)
		{
			ep_size = USB_DATA_SIZE;
		}
		else
		{
			ep_size = usb_in_data_remain;
		}
		UserToPMABufferCopy(buffer_in, ENDP2_TXADDR0, ep_size);
		SetEPDblBuf0Count(ENDP2, EP_DBUF_IN, ep_size);
		FreeUserBuffer(ENDP2, EP_DBUF_IN);
		usb_in_data_remain -= ep_size;
		buffer_in += ep_size;

		if(usb_in_data_remain > 0)
		{
			if(usb_in_data_remain > USB_DATA_SIZE)
			{
				ep_size = USB_DATA_SIZE;
			}
			else
			{
				ep_size = usb_in_data_remain;
			}
			UserToPMABufferCopy(buffer_in, ENDP2_TXADDR1, ep_size);
			SetEPDblBuf1Count(ENDP2, EP_DBUF_IN, ep_size);
			usb_in_data_remain -= ep_size;
			buffer_in += ep_size;
		}
		else
		{
			SetEPDblBuf1Count(ENDP2, EP_DBUF_IN, 0);
		}
	}
#else
	// start to send the first package
	if(usb_in_data_remain > USB_DATA_SIZE)
	{
		ep_size = USB_DATA_SIZE;
	}
	else
	{
		ep_size = usb_in_data_remain;
	}
	UserToPMABufferCopy(buffer_in, ENDP2_TXADDR, ep_size);
	SetEPTxCount(ENDP2, ep_size);
	usb_in_data_remain -= ep_size;
	buffer_in += ep_size;
#endif
	SetEPTxValid(ENDP2);
}

/*******************************************************************************
* Function Name	: USB_Cable_Config
* Description		: Software Connection/Disconnection of USB Cable
* Input					: None.
* Return				 : Status
*******************************************************************************/
void USB_Cable_Config (FunctionalState NewState)
{
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

void DOVR_Callback(void)
{
	LED_RED_ON();
}

void ERR_Callback(void)
{
	LED_RED_ON();
}

void WKUP_Callback(void)
{
//	LED_RED_ON();
}

void SUSP_Callback(void)
{
//	LED_RED_ON();
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
void assert_failed(u8* file, u32 line)
{ 
	/* User can add his own implementation to report the file name and line number,
		 ex: printf("Wrong parameters value: file %s on line %d\r\n", file, line) */

	/* Infinite loop */
	while (1)
	{
	}
}
#endif

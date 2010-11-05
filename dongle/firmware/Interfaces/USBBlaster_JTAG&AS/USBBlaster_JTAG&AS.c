/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian	                              *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USBBlaster_JTAG&AS.c                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    Driver for USBBlaster implementation file                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *	  YYYY-MM-DD:     What(by Who)                                        *
 *	  2010-10-6:      created(by SimonQian)                               *
 **************************************************************************/

#include "stm32f10x_it.h"
#include "usb_lib.h"
#include "usb_prop.h"
#include "usb_desc.h"
#include "hw_config.h"
#include "platform_config.h"
#include "USBBlaster_JTAG&AS.h"
#include "app_cfg.h"

uint8_t stat[2] = {0x31,0x60};
uint8_t ClockBytes,WriteOnly;
uint16_t CALL;

void ShiftOut(uint8_t shiftdata)
{
	//bit0
	if (shiftdata & bmBIT0)   
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit1
	if (shiftdata & bmBIT1)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit2
	if (shiftdata & bmBIT2)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit3
	if (shiftdata & bmBIT3)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit4
	if (shiftdata & bmBIT4)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit5
	if (shiftdata & bmBIT5)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit6
	if (shiftdata & bmBIT6)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit7
	if (shiftdata & bmBIT7)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
}

void OutputByes_ShiftOut(uint8_t outbotbyesdata)
{ 
	uint8_t e, g;
	
	g = 0;
	//bit0
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT0;
	}
	if (outbotbyesdata & bmBIT0)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	 
	//bit1
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT1;
	}
	if (outbotbyesdata & bmBIT1)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit2
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT2;
	}
	if (outbotbyesdata & bmBIT2)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit3
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT3;
	}
	if (outbotbyesdata & bmBIT3)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit4
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT4;
	}
	if (outbotbyesdata & bmBIT4)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit5
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT5;
	}
	if (outbotbyesdata & bmBIT5)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit6
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT6;
	}
	if (outbotbyesdata & bmBIT6)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	//bit7
	if (USBBlaster_NCS_GET())
	{
		e = USBBlaster_TDO_GET();
	}
	else
	{
		e = USBBlaster_ASDO_GET();
	}
	if (e)
	{
		g |= bmBIT7;
	}
	if (outbotbyesdata & bmBIT7)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	USBBlaster_TCK_SET();
	USBBlaster_TCK_CLR();
	
	buffer_out[usb_in_data_remain] = g;
	usb_in_data_remain++;
	if (usb_in_data_remain == USB_DATA_BUFF_SIZE)
	{
		usb_in_data_remain = 0;
 	}
}

void Set_Get_State(uint8_t setgetstatedata)
{
	if (setgetstatedata&bmBIT0)
	{
		USBBlaster_TCK_SET();
	}
	else
	{
		USBBlaster_TCK_CLR();
	}
	if (setgetstatedata&bmBIT4)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	if (setgetstatedata&bmBIT3)
	{
		USBBlaster_NCS_SET();
	}
	else
	{
		USBBlaster_NCS_CLR();
	}
	if (setgetstatedata&bmBIT2)
	{
		USBBlaster_NCE_SET();
	}
	else
	{
		USBBlaster_NCE_CLR();
	}
	if (setgetstatedata&bmBIT1)
	{
		USBBlaster_TMS_SET();
	}
	else
	{
		USBBlaster_TMS_CLR();
	}
}

void OutputByes_Set_Get_State(uint8_t outputsetgetdata)
{
	uint8_t h = 0;
	if (outputsetgetdata & bmBIT0)
	{
		USBBlaster_TCK_SET();
	}
	else
	{
		USBBlaster_TCK_CLR();
	}
	if (outputsetgetdata & bmBIT4)
	{
		USBBlaster_TDI_SET();
	}
	else
	{
		USBBlaster_TDI_CLR();
	}
	if (outputsetgetdata & bmBIT3)
	{
		USBBlaster_NCS_SET();
	}
	else
	{
		USBBlaster_NCS_CLR();
	}
	if (outputsetgetdata & bmBIT2)
	{
		USBBlaster_NCE_SET();
	}
	else
	{
		USBBlaster_NCE_CLR();
	}
	if (outputsetgetdata & bmBIT1)
	{
		USBBlaster_TMS_SET();
	}
	else
	{
		USBBlaster_TMS_CLR();
	}
	
	if (USBBlaster_TDO_GET())
	{
		h |= bmBIT0;
	}
	if (USBBlaster_ASDO_GET())
	{
		h |= bmBIT1;
	}
	
	buffer_out[usb_in_data_remain] = h;
	usb_in_data_remain++;
	if (usb_in_data_remain == USB_DATA_BUFF_SIZE)
	{
		usb_in_data_remain = 0;
	}
}

/*******************************************************************************
* Function Name  : USB_To_USART_Send_Data.
* Description	: send the received data from USB to the UART 0.
* Input		  : data_buffer: data address.
				   Nb_bytes: number of bytes to send.
* Return		 : none.
*******************************************************************************/
void USB_To_JTAG_Send_Data(uint8_t* data_buffer, uint8_t Nb_bytes)
{
	uint16_t i;
	
	for (i = 0; i < Nb_bytes;i++)
	{
		uint8_t data = *(data_buffer + i);
		
		if (ClockBytes > 0)
		{
			if (WriteOnly)
			{
				OutputByes_ShiftOut(data);
			}
			else
			{
				ShiftOut(data);
			}
			ClockBytes--;
		}
		else 
		{
			WriteOnly = (data&bmBIT6);
			if (data&bmBIT7)
			{
				ClockBytes = (data&0x3f);
			}
			else
			{
				if (WriteOnly)
				{
					OutputByes_Set_Get_State(data);
				}
				else
				{
					Set_Get_State(data);
				}
	 		}
		}
	}
}

/*******************************************************************************
* Function Name  : Handle_USBAsynchXfer.
* Description	: send data to USB.
* Input		  : None.
* Return		 : none.
*******************************************************************************/
void Handle_USBAsynchXfer (void)
{
	uint16_t USB_Tx_length;
	uint16_t USB_Tx_ptr;
	
	if (usb_ovf != 1)
	{
		if (usb_in_numofpackage == USB_DATA_BUFF_SIZE)
		{
			usb_in_numofpackage = 0;
		}
		
		if (usb_in_numofpackage == usb_in_data_remain)
		{
			usb_ovf = 0; 
			return;
		}
		
		usb_ovf = 1;
		if (usb_in_numofpackage > usb_in_data_remain)  /* rollback */
		{
			count_out = USB_DATA_BUFF_SIZE - usb_in_numofpackage;
		}
		else 
		{
			count_out = usb_in_data_remain - usb_in_numofpackage;
		}
		
		if (count_out > USB_BLASTER_DATA_SIZE - 2)
		{
			USB_Tx_ptr = usb_in_numofpackage;
			USB_Tx_length = USB_BLASTER_DATA_SIZE-2;
			usb_in_numofpackage += USB_BLASTER_DATA_SIZE-2;
			count_out -= USB_BLASTER_DATA_SIZE-2;
		}
		else
		{
			USB_Tx_ptr = usb_in_numofpackage;
			USB_Tx_length = count_out;
			usb_in_numofpackage += count_out;
			count_out = 0;
		}
		UserToPMABufferCopy(&stat[0], ENDP1_TXADDR, 2); 
		UserToPMABufferCopy(&buffer_out[USB_Tx_ptr], ENDP1_TXADDR + 2, USB_Tx_length);
		
		SetEPTxCount(ENDP1, USB_Tx_length + 2);
		SetEPTxValid(ENDP1);
	}
}

void STAT(void)
{
	UserToPMABufferCopy(&stat[0], ENDP1_TXADDR, 2);
	SetEPTxCount(ENDP1, 2);
	SetEPTxValid(ENDP1); 
}

/******************* (C) COPYRIGHT 2010 STMicroelectronics *****END OF FILE****/

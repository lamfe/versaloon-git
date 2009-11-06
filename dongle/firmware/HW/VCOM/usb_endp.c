/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_endp.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Endpoint routines
********************************************************************************
* THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
* WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE TIME.
* AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY DIRECT,
* INDIRECT OR CONSEQUENTIAL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING FROM THE
* CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE CODING
* INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
*******************************************************************************/

#include "app_cfg.h"
#if USB_PROTOCOL == USB_ST_VCOM

/* Includes ------------------------------------------------------------------*/
#if VSLLINK_EN
#	include "VSLLink.h"
#endif
#include "fifo.h"
#include "USB_proto.h"

#include "stm32f10x.h"
#include "usb_lib.h"
#include "usb_desc.h"

#include "usb_bot.h"
#include "usb_istr.h"
#include "usb_cdc.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
uint8_t buffer_out[USB_DATA_BUFF_SIZE], *buffer_in = NULL;
__IO uint32_t count_out = 0;
__IO uint32_t usb_in_data_remain = 0, usb_in_numofpackage = 0;
__IO uint32_t buffer_ptr = 0;
__IO uint32_t usb_ovf = 0;

__IO uint32_t cmd_len = 0;
__IO uint32_t rep_len = 0;

// OUT means PC OUT, Device input
// IN means PC IN, Device output
#define CDC_OUT_buff			asyn_rx_buf
#define CDC_IN_buff				buffer_out

FIFO CDC_OUT_fifo = {CDC_OUT_buff, 8 * VIRTUAL_COM_PORT_DATA_SIZE, 0, 0};
FIFO CDC_IN_fifo = FIFO_Init(CDC_IN_buff);

__IO uint8_t CDC_USART_IsBusy = 0;
__IO uint8_t CDC_USB_IsBusy = 0;

__IO uint8_t CDC_Out_En = 1;

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
/*******************************************************************************
* Function Name  : EP3_IN_Callback
* Description    :
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void EP3_OUT_Callback(void)
{
	uint32_t pkg_len, tmp_len;
	uint8_t buff[VIRTUAL_COM_PORT_DATA_SIZE];

	if(CDC_enable)
	{
#if USB_RX_DOUBLEBUFFER_EN
		if(GetENDPOINT(ENDP3) & EP_DTOG_TX)
		{
			if (CDC_Out_En)
			{
				FreeUserBuffer(ENDP3, EP_DBUF_OUT);
			}
			pkg_len = GetEPDblBuf0Count(ENDP3);
			PMAToUserBufferCopy(buff, ENDP3_RXADDR0, pkg_len);
		}
		else
		{
			if (CDC_Out_En)
			{
				FreeUserBuffer(ENDP3, EP_DBUF_OUT);
			}
			pkg_len = GetEPDblBuf1Count(ENDP3);
			PMAToUserBufferCopy(buff, ENDP3_RXADDR1, pkg_len);
		}
#else
		pkg_len = GetEPRxCount(ENDP3);
		PMAToUserBufferCopy(buff, ENDP3_RXADDR, pkg_len);
		if (CDC_Out_En)
		{
			SetEPRxValid(ENDP3);
		}
#endif
		if(!pkg_len)
		{
			return;
		}

		// check memory available
		if (FIFO_Get_AvailableLength(&CDC_OUT_fifo) < 4 * VIRTUAL_COM_PORT_DATA_SIZE)
		{
			CDC_Out_En = 0;
		}

		// copy to fifo
		tmp_len = FIFO_Add_Buffer(&CDC_OUT_fifo, buff, pkg_len);
		if(tmp_len < pkg_len)
		{
			// OVERFLOW
			LED_RED_ON();
		}
#if !USB_CDC_BY_POLLING
		if(0 == tmp_len)
		{
			return;
		}

		CDC_IF_Disable_Int();
		if(!CDC_USART_IsBusy)
		{
			CDC_USART_IsBusy = 1;
			CDC_IF_Enable_Int();
			// start to transmit first byte to USART
			CDC_USART_Out(FIFO_Get_Byte(&CDC_OUT_fifo));
			Led_RW_ON();
		}
		else
		{
			CDC_IF_Enable_Int();
		}
#endif
		return;
	}

	if(cmd_len & 0x80000000)
	{
		usb_ovf = 1;
		count_out = 0;
	}

#if USB_RX_DOUBLEBUFFER_EN
	FreeUserBuffer(ENDP3, EP_DBUF_OUT);
	if(GetENDPOINT(ENDP3) & EP_DTOG_TX)
	{
		pkg_len = GetEPDblBuf1Count(ENDP3);
		PMAToUserBufferCopy(buffer_out + count_out, ENDP3_RXADDR1, pkg_len);
	}
	else
	{
		pkg_len = GetEPDblBuf0Count(ENDP3);
		PMAToUserBufferCopy(buffer_out + count_out, ENDP3_RXADDR0, pkg_len);
	}
#else
	pkg_len = GetEPRxCount(ENDP3);
	PMAToUserBufferCopy(buffer_out + count_out, ENDP3_RXADDR, pkg_len);
	SetEPRxValid(ENDP3);
#endif

	if(!pkg_len)
	{
		return;
	}

	if(!count_out)
	{
		// first package
		if(buffer_out[0] <= VERSALOON_COMMON_CMD_END)
		{
			// Common Commands
			if(buffer_out[0] == VERSALOON_WRITE_OFFLINE_DATA)
			{
				cmd_len = buffer_out[1] + ((u16)buffer_out[2] << 8) + 7;
			}
			else
			{
				cmd_len = pkg_len;
			}
		}
#if USB_TO_XXX_EN
		else if(buffer_out[0] <= VERSALOON_USB_TO_XXX_CMD_END)
		{
			// USB_TO_XXX Support
			cmd_len = buffer_out[1] + ((u16)buffer_out[2] << 8);
		}
#endif
#if VSLLINK_EN
		else //if(buffer_out[0] <= VERSALOON_VSLLINK_CMD_END)
		{
			// JTAG Debugger Support
			if((buffer_out[0] == VSLLINK_CMD_HW_JTAGSEQCMD) 
				|| (buffer_out[0] == VSLLINK_CMD_HW_JTAGHLCMD) 
				|| (buffer_out[0] == VSLLINK_CMD_HW_JTAGRAWCMD) 
				|| (buffer_out[0] == VSLLINK_CMD_HW_SWJCMD))
			{
				cmd_len = buffer_out[1] + ((u16)buffer_out[2] << 8);
			}
			else
			{
				cmd_len = pkg_len;
			}
		}
#endif
	}
	count_out += pkg_len;

	// all data received?
	pkg_len = cmd_len;
	if(count_out >= pkg_len)
	{
		cmd_len |= 0x80000000;
	}
}

/*******************************************************************************
* Function Name  : EP2_IN_Callback
* Description    :
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void EP2_IN_Callback(void)
{
	uint32_t len;
#if !USB_CDC_BY_POLLING
	if(CDC_enable)
	{
		uint8_t *buff;

		FIFO_Release_Consequent_Buffer(&CDC_IN_fifo, CDC_USB_IsBusy);
		CDC_IF_Disable_Int();
		len = FIFO_Get_Consequent_Buffer(&CDC_IN_fifo, &buff);

		if(len)
		{
			CDC_IF_Enable_Int();
			if(len > VIRTUAL_COM_PORT_DATA_SIZE - 10)
			{
				len = VIRTUAL_COM_PORT_DATA_SIZE - 10;
			}
			CDC_USB_IsBusy = len;
			CDC_USB_Out(buff, CDC_USB_IsBusy);
		}
		else
		{
			CDC_USB_IsBusy = 0;
			CDC_IF_Enable_Int();
			Led_RW_OFF();
		}
		return;
	}
#endif

	usb_in_numofpackage--;
#if USB_TX_DOUBLEBUFFER_EN
	if(GetENDPOINT(ENDP2) & EP_DTOG_RX)
	{
		if(usb_in_numofpackage > 0)
		{
			// enable next package
			FreeUserBuffer(ENDP2, EP_DBUF_IN);

			if(usb_in_numofpackage > 1)
			{
				if(usb_in_data_remain > 0)
				{
					if(usb_in_data_remain > VIRTUAL_COM_PORT_DATA_SIZE)
					{
						len = VIRTUAL_COM_PORT_DATA_SIZE;
					}
					else
					{
						len = usb_in_data_remain;
					}
					UserToPMABufferCopy(buffer_in, ENDP2_TXADDR0, len);
					SetEPDblBuf0Count(ENDP2, EP_DBUF_IN, len);
					usb_in_data_remain -= len;
					buffer_in += len;
				}
				else
				{
					SetEPDblBuf0Count(ENDP2, EP_DBUF_IN, 0);
				}
			}
		}
	}
	else
	{
		if(usb_in_numofpackage > 0)
		{
			// enable next package
			FreeUserBuffer(ENDP2, EP_DBUF_IN);

			if(usb_in_numofpackage > 1)
			{
				if(usb_in_data_remain > 0)
				{
					if(usb_in_data_remain > VIRTUAL_COM_PORT_DATA_SIZE)
					{
						len = VIRTUAL_COM_PORT_DATA_SIZE;
					}
					else
					{
						len = usb_in_data_remain;
					}
					UserToPMABufferCopy(buffer_in, ENDP2_TXADDR1, len);
					SetEPDblBuf1Count(ENDP2, EP_DBUF_IN, len);
					usb_in_data_remain -= len;
					buffer_in += len;
				}
				else
				{
					SetEPDblBuf1Count(ENDP2, EP_DBUF_IN, 0);
				}
			}
		}
	}
#else
	if(usb_in_data_remain > 0)
	{
		if(usb_in_data_remain > VIRTUAL_COM_PORT_DATA_SIZE)
		{
			len = VIRTUAL_COM_PORT_DATA_SIZE;
		}
		else
		{
			len = usb_in_data_remain;
		}
		UserToPMABufferCopy(buffer_in, ENDP2_TXADDR, len);
		SetEPTxCount(ENDP2, len);
		buffer_in += len;
		usb_in_data_remain -= len;
	}
	else
	{
		SetEPTxCount(ENDP2, 0);
	}
	if(usb_in_numofpackage > 0)
	{
		SetEPTxValid(ENDP2);
	}
#endif
}




#if USB_WITH_MASSSTORAGE
void EP4_OUT_Callback(void)
{
	Mass_Storage_Out();
}

void EP4_IN_Callback(void)
{
	Mass_Storage_In();
}
#endif

#endif  // #if USB_PROTOCOL == USB_ST_VCOM
/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


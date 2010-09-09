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

/* Includes ------------------------------------------------------------------*/
#include "app_cfg.h"
#include "CommandProcessor.h"

#include "stm32f10x.h"
#include "usb_lib.h"
#include "usb_desc.h"
#include "usb_istr.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
/*******************************************************************************
* Function Name  : EP1_IN_Callback
* Description    :
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void EP1_OUT_Callback(void)
{
	uint32_t pkg_len;

	if(cmd_len & 0x80000000)
	{
		usb_ovf = 1;
		count_out = 0;
	}

#if USB_RX_DOUBLEBUFFER_EN
	FreeUserBuffer(ENDP1, EP_DBUF_OUT);
	if(GetENDPOINT(ENDP1) & EP_DTOG_TX)
	{
		pkg_len = GetEPDblBuf1Count(ENDP1);
		PMAToUserBufferCopy(buffer_out + count_out, ENDP3_RXADDR1, pkg_len);
	}
	else
	{
		pkg_len = GetEPDblBuf0Count(ENDP1);
		PMAToUserBufferCopy(buffer_out + count_out, ENDP3_RXADDR0, pkg_len);
	}
#else
	pkg_len = GetEPRxCount(ENDP1);
	PMAToUserBufferCopy(buffer_out + count_out, ENDP3_RXADDR, pkg_len);
	SetEPRxValid(ENDP1);
#endif

	if(!pkg_len)
	{
		return;
	}
	count_out += pkg_len;

	// all data received?
	if (count_out >= buffer_out[0])
	{
		cmd_len = 0x80000000 | buffer_out[0];
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
					if(usb_in_data_remain > USBDM_DATA_SIZE)
					{
						len = USBDM_DATA_SIZE;
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
					if(usb_in_data_remain > USBDM_DATA_SIZE)
					{
						len = USBDM_DATA_SIZE;
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
		if(usb_in_data_remain > USBDM_DATA_SIZE)
		{
			len = USBDM_DATA_SIZE;
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

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


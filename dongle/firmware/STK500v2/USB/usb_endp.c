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
#if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)

/* Includes ------------------------------------------------------------------*/
#include "stm32f10x.h"
#include "usb_lib.h"
#include "usb_desc.h"
#include "usb_istr.h"

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

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
/*******************************************************************************
* Function Name	: EP2_OUT_Callback
* Description		:
* Input					: None.
* Output				 : None.
* Return				 : None.
*******************************************************************************/
void EP2_OUT_Callback(void)
{
	uint32_t pkg_len;

	if(cmd_len & 0x80000000)
	{
		usb_ovf = 1;
		count_out = 0;
	}

	pkg_len = GetEPRxCount(ENDP2);
	PMAToUserBufferCopy(buffer_out + count_out, ENDP2_RXADDR, pkg_len);
	SetEPRxValid(ENDP2);
	if(!pkg_len)
	{
		return;
	}

	if(!count_out && (STK500V2_COMMAND_CHAR == buffer_out[0]))
	{
		memcpy((uint8_t*)&cmd_len, buffer_out + 3, 4);

		if((pkg_len < 11) || (buffer_out[7] != STK500V2_TOKEN_CHAR) || (!cmd_len))
		{
			count_out = 0;
			return;
		}
		cmd_len += 10;
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
* Function Name	: EP2_IN_Callback
* Description		:
* Input					: None.
* Output				 : None.
* Return				 : None.
*******************************************************************************/
void EP2_IN_Callback(void)
{
	uint32_t len;

	usb_in_numofpackage--;

	if(usb_in_data_remain > 0)
	{
		if(usb_in_data_remain > STK500V2_USB_DATA_SIZE)
		{
			len = STK500V2_USB_DATA_SIZE;
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
}

#endif  // #if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


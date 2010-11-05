/******************** (C) COPYRIGHT 2010 STMicroelectronics ********************
* File Name          : usb_endp.c
* Author             : MCD Application Team
* Version            : V3.2.1
* Date               : 07/05/2010
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
#include "usb_lib.h"
#include "usb_desc.h"
#include "app_cfg.h"
#include "usb_mem.h"
#include "hw_config.h"
#include "usb_istr.h"
#include "stm32f10x.h"
#include "USBBlaster_JTAG&AS.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/

/* Interval between sending IN packets in frame number (1 frame = 1ms) */
#define VCOMPORT_IN_FRAME_INTERVAL             5

/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
uint8_t USB_Rx_Buffer[USB_BLASTER_DATA_SIZE];
 
/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/

/*******************************************************************************
* Function Name  : EP1_IN_Callback
* Description    :
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/

 void EP1_IN_Callback (void)
{  uint16_t USB_Tx_ptr;
   uint16_t USB_Tx_length;
  if (usb_ovf == 1)
  {
    if (count_out == 0) 
    {
      usb_ovf = 0;
    }
    else 
    {           
         if (count_out > USB_BLASTER_DATA_SIZE-2)
    {
      USB_Tx_ptr= usb_in_numofpackage;
      USB_Tx_length = USB_BLASTER_DATA_SIZE-2;
      usb_in_numofpackage+=USB_BLASTER_DATA_SIZE-2;
      count_out -= USB_BLASTER_DATA_SIZE-2;           
    }
    else
    { 
      USB_Tx_ptr= usb_in_numofpackage;
      USB_Tx_length = count_out;
      usb_in_numofpackage+=count_out;
      count_out = 0;
      
    }
  UserToPMABufferCopy(&stat[0], ENDP1_TXADDR, 2); 
  UserToPMABufferCopy(&buffer_out[USB_Tx_ptr], ENDP1_TXADDR+2, USB_Tx_length);
   
   SetEPTxCount(ENDP1, USB_Tx_length+2);
   SetEPTxValid(ENDP1);  
    }
  }
 
}
 



 
 
/*******************************************************************************
* Function Name  : EP2_OUT_Callback
* Description    :
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void EP2_OUT_Callback(void)
{   uint8_t USB_Rx_Cnt=0; 
    CALL=3;
    USB_Rx_Cnt = USB_SIL_Read(EP2_OUT, USB_Rx_Buffer);
    USB_To_JTAG_Send_Data(USB_Rx_Buffer, USB_Rx_Cnt);
    SetEPRxValid(ENDP2);
}



/******************* (C) COPYRIGHT 2010 STMicroelectronics *****END OF FILE****/


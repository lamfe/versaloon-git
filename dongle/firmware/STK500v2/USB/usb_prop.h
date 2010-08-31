/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_prop.h
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : All processing related to Virtual COM Port Demo (Endpoint 0)
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

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __usb_prop_H
#define __usb_prop_H

/* Includes ------------------------------------------------------------------*/
/* Exported types ------------------------------------------------------------*/
/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported define -----------------------------------------------------------*/

#define STK500V2_USB_GetConfiguration              NOP_Process
//#define STK500V2_USB_SetConfiguration            NOP_Process
#define STK500V2_USB_GetInterface                  NOP_Process
#define STK500V2_USB_SetInterface                  NOP_Process
#define STK500V2_USB_GetStatus                     NOP_Process
//#define STK500V2_USB_ClearFeature                NOP_Process
#define STK500V2_USB_SetEndPointFeature            NOP_Process
#define STK500V2_USB_SetDeviceFeature              NOP_Process
//#define STK500V2_USB_SetDeviceAddress            NOP_Process

/* Exported functions ------------------------------------------------------- */
void STK500V2_USB_init(void);
void STK500V2_USB_Reset(void);
void STK500V2_USB_Status_In (void);
void STK500V2_USB_Status_Out (void);
RESULT STK500V2_USB_Data_Setup(u8);
RESULT STK500V2_USB_NoData_Setup(u8);
RESULT STK500V2_USB_Get_Interface_Setting(uint8_t Interface, uint8_t AlternateSetting);
uint8_t *STK500V2_USB_GetDeviceDescriptor(uint16_t);
uint8_t *STK500V2_USB_GetConfigDescriptor(uint16_t);
uint8_t *STK500V2_USB_GetStringDescriptor(uint16_t);
void STK500V2_USB_SetConfiguration(void);
void STK500V2_USB_ClearFeature(void);
void STK500V2_USB_SetDeviceAddress (void);

#endif /* __usb_prop_H */

#endif  // #if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


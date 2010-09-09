/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_desc.h
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Descriptor Header for USBDM
********************************************************************************
* THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
* WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE TIME.
* AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY DIRECT,
* INDIRECT OR CONSEQUENTIAL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING FROM THE
* CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE CODING
* INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
*******************************************************************************/

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __USB_DESC_H
#define __USB_DESC_H

/* Includes ------------------------------------------------------------------*/
#include "hw_config.h"
/* Exported types ------------------------------------------------------------*/
/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported define -----------------------------------------------------------*/
#define USB_DEVICE_DESCRIPTOR_TYPE              0x01
#define USB_CONFIGURATION_DESCRIPTOR_TYPE       0x02
#define USB_STRING_DESCRIPTOR_TYPE              0x03
#define USB_INTERFACE_DESCRIPTOR_TYPE           0x04
#define USB_ENDPOINT_DESCRIPTOR_TYPE            0x05
#define USB_IAD_DESCRIPTOR_TYPE                 0x0B

#define USBDM_DATA_SIZE                         BULK_MAX_PACKET_SIZE
#define USBDM_INT_SIZE                          INT_MAX_PACKET_SIZE

#define USBDM_SIZ_DEVICE_DESC                   18
#define USBDM_SIZ_CONFIG_DESC                   32
#define USBDM_SIZ_STRING_LANGID                 4
#define USBDM_SIZ_STRING_VENDOR                 20
#define USBDM_SIZ_STRING_PRODUCT                24
#define USBDM_SIZ_STRING_SERIAL                 50

#define STANDARD_ENDPOINT_DESC_SIZE             0x09

/* Exported functions ------------------------------------------------------- */
extern const uint8_t USBDM_DeviceDescriptor[USBDM_SIZ_DEVICE_DESC];
extern const uint8_t USBDM_ConfigDescriptor[USBDM_SIZ_CONFIG_DESC];

extern const uint8_t USBDM_StringLangID[USBDM_SIZ_STRING_LANGID];
extern const uint8_t USBDM_StringVendor[USBDM_SIZ_STRING_VENDOR];
extern const uint8_t USBDM_StringProduct[USBDM_SIZ_STRING_PRODUCT];
extern uint8_t USBDM_StringSerial[USBDM_SIZ_STRING_SERIAL];

#endif /* __USB_DESC_H */

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

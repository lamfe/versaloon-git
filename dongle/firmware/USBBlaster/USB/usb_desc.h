/******************** (C) COPYRIGHT 2010 STMicroelectronics ********************
* File Name          : usb_desc.h
* Author             : MCD Application Team
* Version            : V3.2.1
* Date               : 07/05/2010
* Description        : Descriptor Header for Virtual COM Port Device
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
/* Exported types ------------------------------------------------------------*/
/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported define -----------------------------------------------------------*/
#define USB_DEVICE_DESCRIPTOR_TYPE              0x01
#define USB_CONFIGURATION_DESCRIPTOR_TYPE       0x02
#define USB_STRING_DESCRIPTOR_TYPE              0x03
#define USB_INTERFACE_DESCRIPTOR_TYPE           0x04
#define USB_ENDPOINT_DESCRIPTOR_TYPE            0x05

#define USB_BLASTER_DATA_SIZE              64
#define USB_BLASTER_INT_SIZE               8

#define USB_BLASTER_DEVICE_DESC        18
#define USB_BLASTER_CONFIG_DESC        32
#define USB_BLASTER_STRING_LANGID      4
#define USB_BLASTER_STRING_VENDOR      14
#define USB_BLASTER_STRING_PRODUCT     24
#define USB_BLASTER_STRING_SERIAL      18

#define STANDARD_ENDPOINT_DESC_SIZE             0x09

/* Exported functions ------------------------------------------------------- */
extern const uint8_t USB_BLASTER_DeviceDescriptor[USB_BLASTER_DEVICE_DESC];
extern const uint8_t USB_BLASTER_ConfigDescriptor[USB_BLASTER_CONFIG_DESC];

extern const uint8_t USB_BLASTER_StringLangID[USB_BLASTER_STRING_LANGID];
extern const uint8_t USB_BLASTER_StringVendor[USB_BLASTER_STRING_VENDOR];
extern const uint8_t USB_BLASTER_StringProduct[USB_BLASTER_STRING_PRODUCT];
extern uint8_t USB_BLASTER_StringSerial[USB_BLASTER_STRING_SERIAL];

#endif /* __USB_DESC_H */
/******************* (C) COPYRIGHT 2010 STMicroelectronics *****END OF FILE****/

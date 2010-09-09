/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_desc.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Descriptors for USBDM
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

/* USB Standard Device Descriptor */
const uint8_t USBDM_DeviceDescriptor[] =
  {
	0x12,	// bLength
	USB_DEVICE_DESCRIPTOR_TYPE,	 // bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0xFF,	// bDeviceClass
	0xFF,	// bDeviceSubClass
	0xFF,	// bDeviceProtocol
	0x08,	// bMaxPacketSize0
	0xA2,
	0x15,	// idVendor = 0x15A2
	0x21,
	0x00,	// idProduct = 0x0021
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
  };

const uint8_t USBDM_ConfigDescriptor[] =
  {
	// Configuation Descriptor
	0x09,// bLength: Configuation Descriptor size
	USB_CONFIGURATION_DESCRIPTOR_TYPE,	// bDescriptorType: Configuration
	USBDM_SIZ_CONFIG_DESC,	// wTotalLength:no of returned bytes
	0x00,
	0x01,	// bNumInterfaces: 1 interface
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	150,	// MaxPower 300 mA

	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_INTERFACE_DESCRIPTOR_TYPE,  // bDescriptorType: Interface
	// Interface descriptor type
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: 2 endpoints used
	0xFF,	// bInterfaceClass
	0xFF,	// bInterfaceSubClass
	0xFF,	// bInterfaceProtocol
	0x00,	// iInterface:

	// Endpoint 3 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_ENDPOINT_DESCRIPTOR_TYPE,	// bDescriptorType: Endpoint
	0x01,	// bEndpointAddress: (OUT1)
	0x02,	// bmAttributes: Bulk
	USBDM_DATA_SIZE,		// wMaxPacketSize:
	0x00,
	0x01,	// bInterval: ignore for Bulk transfer

	// Endpoint 2 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_ENDPOINT_DESCRIPTOR_TYPE,	// bDescriptorType: Endpoint
	0x82,	// bEndpointAddress: (IN2)
	0x02,	// bmAttributes: Bulk
	USBDM_DATA_SIZE,		// wMaxPacketSize:
	0x00,
	0x01,	// bInterval
  };

/* USB String Descriptors */
const uint8_t USBDM_StringLangID[USBDM_SIZ_STRING_LANGID] =
  {
    USBDM_SIZ_STRING_LANGID,
    USB_STRING_DESCRIPTOR_TYPE,
    0x09,
    0x04 /* LangID = 0x0409: U.S. English */
  };

const uint8_t USBDM_StringVendor[USBDM_SIZ_STRING_VENDOR] =
  {
    USBDM_SIZ_STRING_VENDOR,     /* Size of Vendor string */
    USB_STRING_DESCRIPTOR_TYPE,             /* bDescriptorType*/
    'V', 0, 'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0
  };

const uint8_t USBDM_StringProduct[USBDM_SIZ_STRING_PRODUCT] =
  {
    USBDM_SIZ_STRING_PRODUCT,    /* bLength */
    USB_STRING_DESCRIPTOR_TYPE,             /* bDescriptorType */
    'U', 0, 'S', 0, 'B', 0, 'D', 0, 'M', 0, '(', 0, 'J', 0, 'M', 0, 'x', 0, 
	'x', 0, ')', 0
  };

uint8_t USBDM_StringSerial[USBDM_SIZ_STRING_SERIAL] =
  {
    USBDM_SIZ_STRING_SERIAL,           /* bLength */
    USB_STRING_DESCRIPTOR_TYPE,                   /* bDescriptorType */
  };

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

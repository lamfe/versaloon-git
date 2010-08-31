/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_desc.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Descriptors for Virtual Com Port Demo
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
#include "usb_lib.h"

#include "usb_desc.h"

// USB Standard Device Descriptor
const uint8_t STK500V2_USB_DeviceDescriptor[STK500V2_USB_SIZ_DEVICE_DESC] =
{
	0x12,	// bLength
	USB_DEVICE_DESCRIPTOR_TYPE,	// bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0xFF,	// bDeviceClass
	0x00,	// bDeviceSubClass
	0x00,	// bDeviceProtocol
	0x08,	// bMaxPacketSize0
	STK500V2_VID,
	STK500V2_PID,
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
};

const uint8_t STK500V2_USB_ConfigDescriptor[STK500V2_USB_SIZ_CONFIG_DESC] =
{
	// Configuation Descriptor
	0x09,	// bLength: Configuation Descriptor size
	USB_CONFIGURATION_DESCRIPTOR_TYPE,	// bDescriptorType: Configuration
	STK500V2_USB_SIZ_CONFIG_DESC,		// wTotalLength:no of returned bytes
	0x00,
	0x01,	// bNumInterfaces: 1 interface for JTAGICE mkII
	0x01,	// bConfigurationValue: Configuration value
	2,		// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA



	// JTAGICE mkII
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_INTERFACE_DESCRIPTOR_TYPE,	// bDescriptorType: Interface
	// Interface descriptor type
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: 2 endpoints used
	0xFF,	// bInterfaceClass
	0x00,	// bInterfaceSubClass
	0x00,	// bInterfaceProtocol
	2,		// iInterface:

	// Endpoint 2 OUT Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_ENDPOINT_DESCRIPTOR_TYPE,	// bDescriptorType: Endpoint
	0x02,	// bEndpointAddress: (OUT2)
	0x02,	// bmAttributes: Bulk
	STK500V2_USB_DATA_SIZE,		// wMaxPacketSize:
	0x00,
	0x0A,	// bInterval: ignore for Bulk transfer

	// Endpoint 2 IN Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_ENDPOINT_DESCRIPTOR_TYPE,	// bDescriptorType: Endpoint
	0x82,	// bEndpointAddress: (IN2)
	0x02,	// bmAttributes: Bulk
	STK500V2_USB_DATA_SIZE,		// wMaxPacketSize:
	0x00,
	0x0A,	// bInterval
};

// USB String Descriptors
const uint8_t STK500V2_USB_StringLangID[STK500V2_USB_SIZ_STRING_LANGID] =
{
	STK500V2_USB_SIZ_STRING_LANGID,
	USB_STRING_DESCRIPTOR_TYPE,
	0x09,
	0x04 // LangID = 0x0409: U.S. English
};

const uint8_t STK500V2_USB_StringVendor[STK500V2_USB_SIZ_STRING_VENDOR] =
{
	STK500V2_USB_SIZ_STRING_VENDOR,	 // Size of Vendor string
	USB_STRING_DESCRIPTOR_TYPE,		 // bDescriptorType
	STK500V2_USB_STRING_VENDOR
};

const uint8_t STK500V2_USB_StringProduct[STK500V2_USB_SIZ_STRING_PRODUCT] =
{
	STK500V2_USB_SIZ_STRING_PRODUCT,// bLength
	USB_STRING_DESCRIPTOR_TYPE,		// bDescriptorType
	STK500V2_USB_STRING_PRODUCT
};

uint8_t STK500V2_USB_StringSerial[STK500V2_USB_SIZ_STRING_SERIAL] =
{
	STK500V2_USB_SIZ_STRING_SERIAL,	// bLength
	USB_STRING_DESCRIPTOR_TYPE,		// bDescriptorType
	STK500V2_USB_STRING_SERIAL
};

#endif  // #if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

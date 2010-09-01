/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_desc.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Descriptors for MSC Demo
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
const uint8_t MSC_DeviceDescriptor[MSC_SIZ_DEVICE_DESC] =
  {
	0x12,	// bLength
	USB_DEVICE_DESCRIPTOR_TYPE,	 // bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0x08,	// bDeviceClass
	0x06,	// bDeviceSubClass
	0x50,	// bDeviceProtocol
	0x08,	// bMaxPacketSize0
	0x83,
	0x04,	// idVendor = 0x0483
	0x44,
	0x37,	// idProduct = 0x3744
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
  };

const uint8_t MSC_ConfigDescriptor[] =
  {
	// Configuation Descriptor
	0x09,// bLength: Configuation Descriptor size
	USB_CONFIGURATION_DESCRIPTOR_TYPE,	// bDescriptorType: Configuration
	MSC_SIZ_CONFIG_DESC,	// wTotalLength:no of returned bytes
	0x00,
	0x01,	// bNumInterfaces: 1 interface
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA

	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	0x04,	// bDescriptorType:
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints
	0x08,	// bInterfaceClass: MASS STORAGE Class
	0x06,	// bInterfaceSubClass : SCSI transparent
	0x50,	// nInterfaceProtocol
	0x00,	// iInterface:

	// Endpoint 4 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x84,	// Endpoint address (IN, address 4)
	0x02,	// Bulk endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds

	// Endpoint 4 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x04,	// Endpoint address (OUT, address 4)
	0x02,	// Bulk endpoint type
	0x40,	// Maximum packet size (64 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds
  };

/* USB String Descriptors */
const uint8_t MSC_StringLangID[MSC_SIZ_STRING_LANGID] =
  {
    MSC_SIZ_STRING_LANGID,
    USB_STRING_DESCRIPTOR_TYPE,
    0x09,
    0x04 /* LangID = 0x0409: U.S. English */
  };

const uint8_t MSC_StringVendor[MSC_SIZ_STRING_VENDOR] =
  {
    MSC_SIZ_STRING_VENDOR,     /* Size of Vendor string */
    USB_STRING_DESCRIPTOR_TYPE,             /* bDescriptorType*/
    /* Manufacturer: "STMicroelectronics" */
    'S', 0, 'T', 0, 'M', 0, 'i', 0, 'c', 0, 'r', 0, 'o', 0, 'e', 0,
    'l', 0, 'e', 0, 'c', 0, 't', 0, 'r', 0, 'o', 0, 'n', 0, 'i', 0,
    'c', 0, 's', 0
  };

const uint8_t MSC_StringProduct[MSC_SIZ_STRING_PRODUCT] =
  {
    MSC_SIZ_STRING_PRODUCT,          /* bLength */
    USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
    'S', 0, 'T', 0, 'L', 0, 'i', 0, 'n', 0, 'k', 0, 'o', 0, 'n', 0, 
	'V', 0, 'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0
  };

uint8_t MSC_StringSerial[MSC_SIZ_STRING_SERIAL] =
  {
    MSC_SIZ_STRING_SERIAL,           /* bLength */
    USB_STRING_DESCRIPTOR_TYPE,                   /* bDescriptorType */
  };

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

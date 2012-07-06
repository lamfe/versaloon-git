#include "app_cfg.h"

#include "app_interfaces.h"
#include "GPIO/GPIO.h"

#include "usb_protocol.h"
#include "USB_TO_XXX.h"

#include "dal/mal/mal.h"
#include "fakefat.h"

#include "dal/usart_stream/usart_stream.h"

static const uint8_t VersaloonSuper_DeviceDescriptor[] =
{
	0x12,	// bLength
	USB_DESC_TYPE_DEVICE,
			// bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0xEF,	// bDeviceClass: Composite
	0x02,	// bDeviceSubClass
	0x01,	// bDeviceProtocol: IAD
	0x08,	// bMaxPacketSize0
	0x83,
	0x04,	// idVendor = 0x0483
	0x39,
	0xA0,	// idProduct = 0xA038
	0x00,
	0x02,	// bcdDevice = 2.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
};

// 1st device: MSC with fakefat to hold drivers
// 2nd device: Versaloon
// 3rd device: CDCACM with RNDIS
// 4th device: CDCACM for VCOM
// 5th device: CDCACM for SHELL
#define CONFIGDESC_SIZE				(9 + 31 + 31 + 66 + 66)
#define CONFIGDESC_NUM_IFACE		6
const uint8_t VersaloonSuper_ConfigDescriptor[] =
{
	// Configuation Descriptor
	0x09,	// bLength: Configuation Descriptor size
	USB_DESC_TYPE_CONFIGURATION,
			// bDescriptorType: Configuration
	(CONFIGDESC_SIZE >> 0) & 0xFF,
	(CONFIGDESC_SIZE >> 8) & 0xFF,
	 		// wTotalLength:no of returned bytes
	CONFIGDESC_NUM_IFACE,
			// bNumInterfaces: 8 interfaces
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA
	
	// IAD for MSC
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	0,		// bFirstInterface
	1,		// bInterfaceCount
	0x08,	// bFunctionClass
	0x06,	// bFunctionSubClass
	0x50,	// bFunctionProtocol
	0x04,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType:
	0x00,	// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints
	0x08,	// bInterfaceClass: MASS STORAGE Class
	0x06,	// bInterfaceSubClass : SCSI transparent
	0x50,	// nInterfaceProtocol
	0x04,	// iInterface:
	
	// Endpoint 1 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x81,	// Endpoint address (IN1)
	0x02,	// Bulk endpoint type
	32,		// Maximum packet size (32 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds
	
	// Endpoint 1 Descriptor
	0x07,	// Endpoint descriptor length = 7
	0x05,	// Endpoint descriptor type
	0x01,	// Endpoint address (OUT1)
	0x02,	// Bulk endpoint type
	32,		// Maximum packet size (32 bytes)
	0x00,
	0x00,	// Polling interval in milliseconds
	
	// IAD for Versaloon
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	1,		// bFirstInterface
	1,		// bInterfaceCount
	0xFF,	// bFunctionClass
	0x00,	// bFunctionSubClass
	0x00,	// bFunctionProtocol
	0x05,	// iFunction
	
	// interface descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	1,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0xFF,	// bInterfaceClass:
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x05,	// iInterface:
	
	// Endpoint 2 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x02,	// bEndpointAddress: (OUT2)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer

	// Endpoint 2 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x82,	// bEndpointAddress: (IN2)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval
	
	// IAD for RNDIS
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	2,		// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0xFF,	// bFunctionProtocol
	0x06,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	2,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x01,	// bNumEndpoints: One endpoints used
	0x02,	// bInterfaceClass: Communication Interface Class
	0x02,	// bInterfaceSubClass: Abstract Control Model
	0xFF,	// bInterfaceProtocol: User defined
	0x06,	// iInterface:
	
	// Header Functional Descriptor
	0x05,	// bLength: Endpoint Descriptor size
	0x24,	// bDescriptorType: CS_INTERFACE
	0x00,	// bDescriptorSubtype: Header Func Desc
	0x10,	// bcdCDC: spec release number
	0x01,
	
	// Call Managment Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x01,	// bDescriptorSubtype: Call Management Func Desc
	0x00,	// bmCapabilities: 
	0x03,	// bDataInterface: 3
	
	// ACM Functional Descriptor
	0x04,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x02,	// bDescriptorSubtype: Abstract Control Management desc
	0x00,	// bmCapabilities
	
	// Union Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x06,	// bDescriptorSubtype: Union func desc
	2,		// bMasterInterface: Communication class interface
	3,		// bSlaveInterface0: Data Class Interface
	
	// Endpoint 3 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x83,	// bEndpointAddress: (IN3)
	0x03,	// bmAttributes: Interrupt
	8,		// wMaxPacketSize:
	0x00,
	0xFF,	// bInterval:
	
	// Data class interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	3,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0x0A,	// bInterfaceClass: CDCACM
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:
	
	// Endpoint 4 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x04,	// bEndpointAddress: (OUT4)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// Endpoint 4 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x84,	// bEndpointAddress: (IN4)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval
	
	// IAD for VCOM
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	4,		// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0x01,	// bFunctionProtocol
	0x07,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	4,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x01,	// bNumEndpoints: One endpoints used
	0x02,	// bInterfaceClass: Communication Interface Class
	0x02,	// bInterfaceSubClass: Abstract Control Model
	0x01,	// bInterfaceProtocol: AT command
	0x07,	// iInterface:
	
	// Header Functional Descriptor
	0x05,	// bLength: Endpoint Descriptor size
	0x24,	// bDescriptorType: CS_INTERFACE
	0x00,	// bDescriptorSubtype: Header Func Desc
	0x10,	// bcdCDC: spec release number
	0x01,
	
	// Call Managment Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x01,	// bDescriptorSubtype: Call Management Func Desc
	0x00,	// bmCapabilities: 
	0x05,	// bDataInterface: 5
	
	// ACM Functional Descriptor
	0x04,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x02,	// bDescriptorSubtype: Abstract Control Management desc
	0x02,	// bmCapabilities
	
	// Union Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x06,	// bDescriptorSubtype: Union func desc
	4,		// bMasterInterface: Communication class interface
	5,		// bSlaveInterface0: Data Class Interface
	
	// Endpoint 5 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x85,	// bEndpointAddress: (IN5)
	0x03,	// bmAttributes: Interrupt
	8,		// wMaxPacketSize:
	0x00,
	0xFF,	// bInterval:
	
	// Data class interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	5,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0x0A,	// bInterfaceClass: CDCACM
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:
	
	// Endpoint 6 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x06,	// bEndpointAddress: (OUT6)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// Endpoint 6 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x86,	// bEndpointAddress: (IN6)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval
/*	
	// IAD for SHELL
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	6,		// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0x01,	// bFunctionProtocol
	0x08,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	6,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x01,	// bNumEndpoints: One endpoints used
	0x02,	// bInterfaceClass: Communication Interface Class
	0x02,	// bInterfaceSubClass: Abstract Control Model
	0x01,	// bInterfaceProtocol: AT command
	0x08,	// iInterface:
	
	// Header Functional Descriptor
	0x05,	// bLength: Endpoint Descriptor size
	0x24,	// bDescriptorType: CS_INTERFACE
	0x00,	// bDescriptorSubtype: Header Func Desc
	0x10,	// bcdCDC: spec release number
	0x01,
	
	// Call Managment Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x01,	// bDescriptorSubtype: Call Management Func Desc
	0x00,	// bmCapabilities: 
	0x07,	// bDataInterface: 7
	
	// ACM Functional Descriptor
	0x04,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x02,	// bDescriptorSubtype: Abstract Control Management desc
	0x02,	// bmCapabilities
	
	// Union Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x06,	// bDescriptorSubtype: Union func desc
	6,		// bMasterInterface: Communication class interface
	7,		// bSlaveInterface0: Data Class Interface
	
	// Endpoint 7 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x87,	// bEndpointAddress: (IN7)
	0x03,	// bmAttributes: Interrupt
	8,		// wMaxPacketSize:
	0x00,
	0xFF,	// bInterval:
	
	// Data class interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	7,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0x0A,	// bInterfaceClass: CDCACM
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:
	
	// Endpoint 8 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x08,	// bEndpointAddress: (OUT8)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer
	
	// Endpoint 8 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x88,	// bEndpointAddress: (IN8)
	0x02,	// bmAttributes: Bulk
	32,		// wMaxPacketSize:
	0x00,
	0x00	// bInterval
*/
};

static const uint8_t VersaloonSuper_StringLangID[] =
{
	4,
	USB_DESC_TYPE_STRING,
	0x09,
	0x04
};

static const uint8_t VersaloonSuper_StringVendor[] =
{
	20,
	USB_DESC_TYPE_STRING,
	'S', 0, 'i', 0, 'm', 0, 'o', 0, 'n', 0, 'Q', 0, 'i', 0, 'a', 0,
	'n', 0
};

static const uint8_t VersaloonSuper_StringProduct[] =
{
	30,
	USB_DESC_TYPE_STRING,
	'V', 0, 'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0,
	'n', 0, 'S', 0, 'u', 0, 'p', 0, 'e', 0, 'r', 0
};

static const uint8_t VersaloonSuper_StringSerial[50] =
{
	50,
	USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0, 'C', 0, 'D', 0, 'E', 0, 'F', 0, 
	'0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, 
};

static const uint8_t MSC_StringProduct[] =
{
	40,
	USB_DESC_TYPE_STRING,
	'M', 0, 'S', 0, 'C', 0, 'o', 0, 'n', 0, 'V', 0, 'e', 0, 'r', 0,
	's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0, 'S', 0, 'u', 0,
	'p', 0, 'e', 0, 'r', 0
};

static const uint8_t Versaloon_StringProduct[] =
{
	20,
	USB_DESC_TYPE_STRING,
	'V', 0, 'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0,
	'n', 0
};

static const uint8_t RNDIS_StringProduct[] =
{
	44,
	USB_DESC_TYPE_STRING,
	'R', 0, 'N', 0, 'D', 0, 'I', 0, 'S', 0, 'o', 0, 'n', 0, 'V', 0,
	'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0,
	'S', 0, 'u', 0, 'p', 0, 'e', 0, 'r', 0
};

static const uint8_t VCOM_StringProduct[] =
{
	42,
	USB_DESC_TYPE_STRING,
	'V', 0, 'C', 0, 'O', 0, 'M', 0, 'o', 0, 'n', 0, 'V', 0, 'e', 0,
	'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0, 'S', 0,
	'u', 0, 'p', 0, 'e', 0, 'r', 0
};

static const uint8_t SHELL_StringProduct[] =
{
	44,
	USB_DESC_TYPE_STRING,
	'S', 0, 'H', 0, 'E', 0, 'L', 0, 'L', 0, 'o', 0, 'n', 0, 'V', 0,
	'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0, 'n', 0,
	'S', 0, 'u', 0, 'p', 0, 'e', 0, 'r', 0
};

static const struct vsfusbd_desc_filter_t descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, VersaloonSuper_DeviceDescriptor, sizeof(VersaloonSuper_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, VersaloonSuper_ConfigDescriptor, sizeof(VersaloonSuper_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, VersaloonSuper_StringLangID, sizeof(VersaloonSuper_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, VersaloonSuper_StringVendor, sizeof(VersaloonSuper_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, VersaloonSuper_StringProduct, sizeof(VersaloonSuper_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, VersaloonSuper_StringSerial, sizeof(VersaloonSuper_StringSerial), NULL),
	VSFUSBD_DESC_STRING(0x0409, 4, MSC_StringProduct, sizeof(MSC_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 5, Versaloon_StringProduct, sizeof(Versaloon_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 6, RNDIS_StringProduct, sizeof(RNDIS_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 7, VCOM_StringProduct, sizeof(VCOM_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 8, SHELL_StringProduct, sizeof(SHELL_StringProduct), NULL),
	VSFUSBD_DESC_NULL
};

// MSC
static struct fakefat_param_t fakefat_param = {0, 0};
static struct mal_info_t fakefat_mal_info = 
{
	{0, 0}, NULL, 0, 0, 0, &fakefat_drv
};
static struct dal_info_t fakefat_dal_info = 
{
	NULL,
	&fakefat_param,
	NULL,
	&fakefat_mal_info,
};

struct SCSI_LUN_info_t MSCBOT_LunInfo = 
{
	&fakefat_dal_info, 
	{
		true,
		{'S', 'i', 'm', 'o', 'n', ' ', ' ', ' '},
		{'S', 'i', 'm', 'o', 'n', ' ', ' ', ' ', 
		'S', 'i', 'm', 'o', 'n', ' ', ' ', ' '},
		{'1', '.', '0', '0'},
		SCSI_PDT_DIRECT_ACCESS_BLOCK
	}
};
uint8_t MSCBOT_Buffer0[512], MSCBOT_Buffer1[512];

struct vsfusbd_MSCBOT_param_t MSCBOT_param = 
{
	1,							// uint8_t ep_out;
	1,							// uint8_t ep_in;
	
	0,							// uint8_t max_lun;
	&MSCBOT_LunInfo,			// struct SCSI_LUN_info_t *lun_info;
	NULL, 						// struct SCSI_handler_t *user_handlers;
	
	{
		{MSCBOT_Buffer0, sizeof(MSCBOT_Buffer0)},
		{MSCBOT_Buffer1, sizeof(MSCBOT_Buffer1)}
	},							// struct vsf_buffer_t page_buffer[2];
};

// Versaloon
struct vsfusbd_Versaloon_param_t Versaloon_param = 
{
	2,				// uint8_t ep_out;
	2,				// uint8_t ep_in;
	
	false,			// bool dbuffer_en;
};

// RNDIS
struct vsfusbd_CDCACM_param_t RNDIS_param = 
{
	4,			// ep_out
	4, 			// ep_in
	
/*	{
		
	},			// stream_tx;
	{
		
	},			// stream_rx;
	{
		
	}			// callback
*/
};

// VCOM
extern struct usart_stream_info_t usart_stream_p0;

vsf_err_t VOM_set_line_coding(struct vsfusbd_CDCACM_line_coding_t *line_coding)
{
	usart_stream_p0.usart_info.datalength = line_coding->datatype;
	usart_stream_p0.usart_info.baudrate = line_coding->bitrate;
	usart_stream_p0.usart_info.mode = 0;
	switch(line_coding->stopbittype)
	{
	default:
	case 0:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_1;
		break;
	case 1:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_1P5;
		break;
	case 2:
		usart_stream_p0.usart_info.mode |= USART_STOPBITS_2;
		break;
	}
	switch(line_coding->paritytype)
	{
	default:
	case 0:
		usart_stream_p0.usart_info.mode |= USART_PARITY_NONE;
		usart_stream_p0.usart_info.datalength = 8;
		break;
	case 1:
		usart_stream_p0.usart_info.mode |= USART_PARITY_ODD;
		usart_stream_p0.usart_info.datalength = 9;
		break;
	case 2:
		usart_stream_p0.usart_info.mode |= USART_PARITY_EVEN;
		usart_stream_p0.usart_info.datalength = 9;
		break;
	}
	return usart_stream_config(&usart_stream_p0);
}

struct vsfusbd_CDCACM_param_t CDCACM_param = 
{
	6,			// ep_out
	6, 			// ep_in
	
//	&usart_stream_p0.stream_tx,
//	&usart_stream_p0.stream_rx,
	NULL, NULL,
	
	{
		VOM_set_line_coding
	},			// callback
	
	{
		115200,	// bitrate
		0,		// stopbittype
		0,		// paritytype
		8		// datatype
	},
};

// SHELL
/*extern struct usart_stream_info_t shell_stream;
struct vsfusbd_CDCACM_param_t Versaloon_Shell_param = 
{
	&shell_stream,
				// usart_stream
	false,		// gpio_rts_enable
	0,			// gpio_rts_port
	0,			// gpio_rts_pin
	false,		// gpio_dtr_enable
	0,			// gpio_dtr_port
	0,			// gpio_dtr_pin
	
	7,			// ep_out
	7, 			// ep_in
	{
		115200,	// bitrate
		0,		// stopbittype
		0,		// paritytype
		8		// datatype
	},
};
*/

// Device
static struct vsfusbd_iface_t ifaces[] = 
{
	// MSC
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_MSCBOT_class, (void *)&MSCBOT_param},
	// Versaloon
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_Versaloon_class, (void *)&Versaloon_param},
	// RNDIS
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMMaster_class, (void *)&CDCACM_param},
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMData_class, (void *)&CDCACM_param},
	// VCOM
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMMaster_class, (void *)&CDCACM_param},
	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMData_class, (void *)&CDCACM_param},
	// SHELL
//	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMMaster_class, (void *)&Versaloon_Shell_param},
//	{(struct vsfusbd_class_protocol_t *)&vsfusbd_CDCACMData_class, (void *)&Versaloon_Shell_param}
};
static struct vsfusbd_config_t config0[] = 
{
	{
		NULL, NULL, dimof(ifaces), (struct vsfusbd_iface_t *)ifaces
	}
};
struct vsfusbd_device_t usb_device = 
{
	1, (struct vsfusbd_config_t *)config0, 
	(struct vsfusbd_desc_filter_t *)descriptors, 0, 
	(struct interface_usbd_t *)&core_interfaces.usbd
};

vsf_err_t usb_protocol_init(void)
{
	NVIC_InitTypeDef NVIC_InitStructure;
	
	NVIC_PriorityGroupConfig(NVIC_PriorityGroup_2);
	
	NVIC_InitStructure.NVIC_IRQChannel = USART1_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
	
	interfaces->gpio.init(0);
	interfaces->gpio.init(1);
	interfaces->gpio.init(2);
	
	LED_POWER_INIT();
	LED_STATE_INIT();
	LED_STATE_G_ON();
	LED_USB_INIT();
	
	mal.init(&fakefat_dal_info);
	CDCACM_param.stream_tx = &usart_stream_p0.stream_tx;
	CDCACM_param.stream_rx = &usart_stream_p0.stream_rx;
	usart_stream_init(&usart_stream_p0);
	
	USB_Pull_Init();
	USB_Connect();
	return vsfusbd_device_init(&usb_device);
}

vsf_err_t usb_protocol_poll(void)
{
	usart_stream_poll(&usart_stream_p0);
	return vsfusbd_device_poll(&usb_device);
}

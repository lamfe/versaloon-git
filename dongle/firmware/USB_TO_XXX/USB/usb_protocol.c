#include "app_cfg.h"

#include "interfaces.h"
#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "usb_protocol.h"

static RESULT Versaloon_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	uint32_t len;

	usb_in_numofpackage--;
	if(usb_in_data_remain > 0)
	{
		if(usb_in_data_remain > 64)
		{
			len = 64;
		}
		else
		{
			len = usb_in_data_remain;
		}
		device->drv->ep.write_IN_buffer(ep, buffer_in, len);
		device->drv->ep.set_IN_count(ep, len);
		buffer_in += len;
		usb_in_data_remain -= len;
	}
	else
	{
		device->drv->ep.set_IN_count(ep, 0);
	}
	if(usb_in_numofpackage > 0)
	{
		device->drv->ep.set_IN_state(ep, USB_EP_STAT_ACK);
	}
	
	return ERROR_OK;
}

static RESULT Versaloon_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	uint32_t pkg_len;

	if(cmd_len & 0x80000000)
	{
		usb_ovf = 1;
		count_out = 0;
	}
	
	pkg_len = device->drv->ep.get_OUT_count(ep);
	device->drv->ep.read_OUT_buffer(ep, buffer_out + count_out, pkg_len);
	device->drv->ep.set_OUT_state(ep, USB_EP_STAT_ACK);
	
	if(pkg_len)
	{
		if(!count_out)
		{
			// first package
			if(buffer_out[0] <= VERSALOON_COMMON_CMD_END)
			{
				// Common Commands
				if(buffer_out[0] == VERSALOON_WRITE_OFFLINE_DATA)
				{
					cmd_len = buffer_out[1] + ((u16)buffer_out[2] << 8) + 7;
				}
				else
				{
					cmd_len = pkg_len;
				}
			}
#if USB_TO_XXX_EN
			else if(buffer_out[0] <= VERSALOON_USB_TO_XXX_CMD_END)
			{
				// USB_TO_XXX Support
				cmd_len = buffer_out[1] + ((u16)buffer_out[2] << 8);
			}
#endif
		}
		count_out += pkg_len;
		
		// all data received?
		pkg_len = cmd_len;
		if(count_out >= pkg_len)
		{
			cmd_len |= 0x80000000;
		}
	}
	
	return ERROR_OK;
}

static RESULT versaloon_init(struct vsfusbd_device_t *device)
{
	if ((ERROR_OK != device->drv->ep.set_IN_dbuffer(2)) || 
		(ERROR_OK != device->drv->ep.set_OUT_dbuffer(3)) || 
		(ERROR_OK != device->drv->ep.set_IN_handler(2, Versaloon_IN_hanlder)) || 
		(ERROR_OK != device->drv->ep.set_OUT_handler(3, Versaloon_OUT_hanlder)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static const uint8_t Versaloon_DeviceDescriptor[] =
{
	0x12,	// bLength
	USB_DESC_TYPE_DEVICE,
			// bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0xFF,	// bDeviceClass:
	0x00,	// bDeviceSubClass
	0x00,	// bDeviceProtocol
	0x08,	// bMaxPacketSize0
	0x83,
	0x04,	// idVendor = 0x0483
	0x38,
	0xA0,	// idProduct = 0xA038
	0x00,
	0x01,	// bcdDevice = 1.00
	1,		// Index of string descriptor describing manufacturer
	2,		// Index of string descriptor describing product
	3,		// Index of string descriptor describing the device's serial number
	0x01	// bNumConfigurations
};

const uint8_t Versaloon_ConfigDescriptor[] =
{
	// Configuation Descriptor
	0x09,	// bLength: Configuation Descriptor size
	USB_DESC_TYPE_CONFIGURATION,
			// bDescriptorType: Configuration
	32,		// wTotalLength:no of returned bytes
	0x00,
	0x01,	// bNumInterfaces: 2 interface
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA
	
	// interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType:
	0,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0xFF,	// bInterfaceClass:
	0x00,	// bInterfaceSubClass:
	0x00,	// bInterfaceProtocol:
	0x00,	// iInterface:

	// Endpoint 3 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x03,	// bEndpointAddress: (OUT3)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00,	// bInterval: ignore for Bulk transfer

	// Endpoint 2 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x82,	// bEndpointAddress: (IN2)
	0x02,	// bmAttributes: Bulk
	64,		// wMaxPacketSize:
	0x00,
	0x00	// bInterval
};

static const uint8_t Versaloon_StringLangID[] =
{
	4,
	USB_DESC_TYPE_STRING,
	0x09,
	0x04
};

static const uint8_t Versaloon_StringVendor[] =
{
    38,
    USB_DESC_TYPE_STRING,
    'S', 0, 'T', 0, 'M', 0, 'i', 0, 'c', 0, 'r', 0, 'o', 0, 'e', 0,
    'l', 0, 'e', 0, 'c', 0, 't', 0, 'r', 0, 'o', 0, 'n', 0, 'i', 0,
    'c', 0, 's', 0
};

static const uint8_t Versaloon_StringProduct[] =
{
    20,
    USB_DESC_TYPE_STRING,
    'V', 0, 'e', 0, 'r', 0, 's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0,
    'n', 0
};

static const uint8_t Versaloon_StringSerial[50] =
{
    50,
    USB_DESC_TYPE_STRING,
	'0', 0, '1', 0, '2', 0, '3', 0, '4', 0, '5', 0, '6', 0, '7', 0, 
	'8', 0, '9', 0, 'A', 0, 'B', 0, 'C', 0, 'D', 0, 'E', 0, 'F', 0, 
	'0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, 
};

static const struct vsfusbd_desc_filter_t descriptors[] = 
{
	VSFUSBD_DESC_DEVICE(0, Versaloon_DeviceDescriptor, sizeof(Versaloon_DeviceDescriptor), NULL),
	VSFUSBD_DESC_CONFIG(0, 0, Versaloon_ConfigDescriptor, sizeof(Versaloon_ConfigDescriptor), NULL),
	VSFUSBD_DESC_STRING(0, 0, Versaloon_StringLangID, sizeof(Versaloon_StringLangID), NULL),
	VSFUSBD_DESC_STRING(0x0409, 1, Versaloon_StringVendor, sizeof(Versaloon_StringVendor), NULL),
	VSFUSBD_DESC_STRING(0x0409, 2, Versaloon_StringProduct, sizeof(Versaloon_StringProduct), NULL),
	VSFUSBD_DESC_STRING(0x0409, 3, Versaloon_StringSerial, sizeof(Versaloon_StringSerial), NULL),
	VSFUSBD_DESC_NULL
};
static const struct vsfusbd_class_protocol_t Versaloon_Protocol = 
{
	NULL, NULL, 
	versaloon_init,
	NULL, NULL
};
static const struct vsfusbd_iface_t ifaces[] = 
{
	{0, (struct vsfusbd_class_protocol_t *)&Versaloon_Protocol},
};
static const struct vsfusbd_config_t config0[] = 
{
	{NULL, NULL, 1, (struct vsfusbd_iface_t *)ifaces}
};
static struct vsfusbd_device_t usb_device = 
{
	1, 0, 0, (struct vsfusbd_config_t *)config0, 
	(struct vsfusbd_desc_filter_t *)descriptors, 
	(struct interface_usbd_t *)&core_interfaces.usbd
};

RESULT usb_protocol_init()
{
	return vsfusbd_device_init(&usb_device);
}

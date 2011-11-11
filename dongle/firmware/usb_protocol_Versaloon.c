#include "app_cfg.h"

#include "app_interfaces.h"
#include "GPIO/GPIO.h"

#include "usb_protocol.h"
#include "USB_TO_XXX.h"

uint8_t buffer_out[USB_DATA_BUFF_SIZE], asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
volatile uint32_t count_out = 0;
volatile uint32_t usb_ovf = 0;
volatile uint32_t cmd_len = 0;

volatile uint32_t rep_len = 0;

static vsf_err_t Versaloon_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	uint32_t pkg_len;

	if(cmd_len & 0x80000000)
	{
		usb_ovf = 1;
		count_out = 0;
	}
	
	device->drv->ep.switch_OUT_buffer(ep);
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
					cmd_len = buffer_out[1] + ((uint16_t)buffer_out[2] << 8) + 7;
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
				cmd_len = buffer_out[1] + ((uint16_t)buffer_out[2] << 8);
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
	
	return VSFERR_NONE;
}

static vsf_err_t versaloon_usb_init(uint8_t iface, struct vsfusbd_device_t *device)
{
	if (device->drv->ep.set_IN_dbuffer(2) || 
		device->drv->ep.set_OUT_dbuffer(3) || 
		device->drv->ep.set_OUT_handler(3, Versaloon_OUT_hanlder))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t versaloon_idle(uint8_t iface, struct vsfusbd_device_t *device)
{
	if(cmd_len & 0x80000000)
	{
		// A valid USB package has been received
		LED_USB_ON();
		
		ProcessCommand(&buffer_out[0], cmd_len & 0xFFFF);
		if(rep_len > 0)
		{
			// indicate reply data is valid
			rep_len |= 0x80000000;
		}
		else
		{
			// no data to send, set cmd_len to 0
			cmd_len = 0;
		}
		count_out = 0;				// set USB receive pointer to 0
		
		if(rep_len & 0x80000000)	// there is valid data to be sent to PC
		{
			struct vsf_buffer_t buffer;
			
			buffer.buffer = buffer_out;
			buffer.size = rep_len & 0xFFFF;
			vsfusbd_ep_out(&usb_device, 2, &buffer);
			
			// reset command length and reply length for next command
			cmd_len = 0;
			rep_len = 0;
		}
		
		LED_USB_OFF();
	}
	else
	{
#if POWER_OUT_EN
			app_interfaces.target_voltage.poll(0);
#endif
	}
	
	return VSFERR_NONE;
}

static const uint8_t Versaloon_DeviceDescriptor[] =
{
	0x12,	// bLength
	USB_DESC_TYPE_DEVICE,
			// bDescriptorType
	0x00,
	0x02,	// bcdUSB = 2.00
	0xEF,	// bDeviceClass: IAD
	0x02,	// bDeviceSubClass
	0x01,	// bDeviceProtocol
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
	98,		// wTotalLength:no of returned bytes
	0x00,
	0x03,	// bNumInterfaces: 3 interfaces
	0x01,	// bConfigurationValue: Configuration value
	0x00,	// iConfiguration: Index of string descriptor describing the configuration
	0x80,	// bmAttributes: bus powered
	0x64,	// MaxPower 200 mA
	
	// interface descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
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
	0x00,	// bInterval
	
	// IAD
	0x08,	// bLength: IAD Descriptor size
	USB_DESC_TYPE_IAD,
			// bDescriptorType: IAD
	1,		// bFirstInterface
	2,		// bInterfaceCount
	0x02,	// bFunctionClass
	0x02,	// bFunctionSubClass
	0x01,	// bFunctionProtocol
	0x04,	// iFunction
	
	// Interface Descriptor
	0x09,	// bLength: Interface Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	1,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x01,	// bNumEndpoints: One endpoints used
	0x02,	// bInterfaceClass: Communication Interface Class
	0x02,	// bInterfaceSubClass: Abstract Control Model
	0x01,	// bInterfaceProtocol: Common AT commands
	0x04,	// iInterface:
	
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
	0x00,	// bmCapabilities: D0+D1
	0x01,	// bDataInterface: 1
	
	// ACM Functional Descriptor
	0x04,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x02,	// bDescriptorSubtype: Abstract Control Management desc
	0x02,	// bmCapabilities
	
	// Union Functional Descriptor
	0x05,	// bFunctionLength
	0x24,	// bDescriptorType: CS_INTERFACE
	0x06,	// bDescriptorSubtype: Union func desc
	1,		// bMasterInterface: Communication class interface
	2,		// bSlaveInterface0: Data Class Interface
	
	// Endpoint 1 Descriptor
	0x07,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_ENDPOINT,
			// bDescriptorType: Endpoint
	0x81,	// bEndpointAddress: (IN1)
	0x03,	// bmAttributes: Interrupt
	8,		// wMaxPacketSize:
	0x00,
	0xFF,	// bInterval:
	
	// Data class interface descriptor
	0x09,	// bLength: Endpoint Descriptor size
	USB_DESC_TYPE_INTERFACE,
			// bDescriptorType: Interface
	2,		// bInterfaceNumber: Number of Interface
	0x00,	// bAlternateSetting: Alternate setting
	0x02,	// bNumEndpoints: Two endpoints used
	0x0A,	// bInterfaceClass: CDC
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

static const uint8_t CDC_StringProduct[] =
{
	30,
	USB_DESC_TYPE_STRING,
	'C', 0, 'O', 0, 'M', 0, 'o', 0, 'n', 0, 'V', 0, 'e', 0, 'r', 0,
	's', 0, 'a', 0, 'l', 0, 'o', 0, 'o', 0,'n', 0
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
	VSFUSBD_DESC_STRING(0x0409, 4, CDC_StringProduct, sizeof(CDC_StringProduct), NULL),
	VSFUSBD_DESC_NULL
};
static const struct vsfusbd_class_protocol_t Versaloon_Protocol = 
{
	NULL, NULL, NULL, 
	versaloon_usb_init,
	NULL, versaloon_idle
};
extern struct usart_stream_info_t usart_stream_p0;
struct vsfusbd_CDC_param_t Versaloon_CDC_param = 
{
	&usart_stream_p0,
				// usart_stream
	false,		// gpio_rts_enable
	0,			// gpio_rts_port
	GPIO_TDI,	// gpio_rts_pin
	false,		// gpio_dtr_enable
	0,			// gpio_dtr_port
	GPIO_TMS,	// gpio_dtr_pin
	
	1,			// iface_master
	2,			// iface_slave
	4,			// ep_out
	4, 			// ep_in
	{
		115200,	// bitrate
		0,		// stopbittype
		0,		// paritytype
		8		// datatype
	},
};
static const struct vsfusbd_iface_t ifaces[] = 
{
	{0, (struct vsfusbd_class_protocol_t *)&Versaloon_Protocol, NULL},
	{0, (struct vsfusbd_class_protocol_t *)&vsfusbd_CDCMaster_class, (void *)&Versaloon_CDC_param},
	{0, (struct vsfusbd_class_protocol_t *)&vsfusbd_CDCData_class, (void *)&Versaloon_CDC_param}
};
static struct vsfusbd_config_t config0[] = 
{
	{
		NULL, NULL, 3, (struct vsfusbd_iface_t *)ifaces,
	}
};
struct vsfusbd_device_t usb_device = 
{
	1, (struct vsfusbd_config_t *)config0, 
	(struct vsfusbd_desc_filter_t *)descriptors, 
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
	
	NVIC_InitStructure.NVIC_IRQChannel = USBWakeUp_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
	
	NVIC_InitStructure.NVIC_IRQChannel = USB_LP_CAN1_RX0_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
	
	NVIC_InitStructure.NVIC_IRQChannel = USB_HP_CAN1_TX_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
	
	interfaces->gpio.init(0);
	interfaces->gpio.init(1);
	interfaces->gpio.init(2);
	
	LED_RED_INIT();
	LED_RED_OFF();
	LED_GREEN_INIT();
	LED_GREEN_ON();
	LED_USB_INIT();
	LED_USB_OFF();
	
	app_interfaces.delay.init();
#if POWER_SAMPLE_EN
	interfaces->adc.init(TVCC_ADC_PORT);
	interfaces->adc.config(TVCC_ADC_PORT, CORE_APB2_FREQ_HZ / 8, ADC_ALIGNRIGHT);
	interfaces->adc.config_channel(TVCC_ADC_PORT, TVCC_ADC_CHANNEL, 0xFF);
	interfaces->adc.calibrate(TVCC_ADC_PORT, TVCC_ADC_CHANNEL);
#endif
	USB_TO_XXX_Init(asyn_rx_buf + 2048);
	
	USB_Pull_Init();
	USB_Connect();
	return vsfusbd_device_init(&usb_device);
}

vsf_err_t usb_protocol_idle(void)
{
	return vsfusbd_device_poll(&usb_device);
}

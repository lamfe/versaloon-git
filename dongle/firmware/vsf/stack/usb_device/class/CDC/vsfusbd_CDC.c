#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_CDC.h"

struct vsfusbd_CDC_param_t vsfusbd_CDC_param;
struct vsfusbd_CDC_line_coding_t vsfusbd_CDC_LineCoding = 
{
	115200, 0, 0, 8
};
uint8_t vsfusbd_CDC_LineCoding_buffer[7];
volatile bool vsfusbd_CDC_OutEn = true;

static RESULT vsfusbd_CDCData_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	uint16_t pkg_size;
	uint8_t buffer[64];
	struct usart_status_t status;
	
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if (pkg_size > 64)
	{
		return ERROR_FAIL;
	}
	device->drv->ep.read_OUT_buffer(ep, buffer, pkg_size);
	if (vsfusbd_CDC_OutEn)
	{
		device->drv->ep.set_OUT_state(ep, USB_EP_STAT_ACK);
	}
	
	interfaces->usart.status(vsfusbd_CDC_param.usart_port, &status);
	if (status.tx_buff_avail < 64)
	{
		vsfusbd_CDC_OutEn = false;
	}
	return interfaces->usart.send(vsfusbd_CDC_param.usart_port, buffer, pkg_size);
}

static RESULT vsfusbd_CDCData_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	uint16_t pkg_size;
	uint8_t buffer[64];
	struct usart_status_t status;
	
	interfaces->usart.status(vsfusbd_CDC_param.usart_port, &status);
	if (status.rx_buff_size)
	{
		pkg_size = (status.rx_buff_size > sizeof(buffer)) ? sizeof(buffer) : 
				status.rx_buff_size;
		interfaces->usart.receive(vsfusbd_CDC_param.usart_port, buffer, pkg_size);
		device->drv->ep.write_IN_buffer(ep, buffer, pkg_size);
		device->drv->ep.set_IN_count(ep, pkg_size);
		device->drv->ep.set_IN_state(ep, USB_EP_STAT_ACK);
	}
	else
	{
		device->drv->ep.set_IN_count(ep, 0);
		device->drv->ep.set_IN_state(ep, USB_EP_STAT_ACK);
	}
	
	return ERROR_OK;
}

static RESULT vsfusbd_CDCData_class_init(struct vsfusbd_device_t *device)
{
	uint8_t port;
	uint32_t pin;
	
	if ((ERROR_OK != device->drv->ep.set_IN_handler(vsfusbd_CDC_param.ep_in, vsfusbd_CDCData_IN_hanlder)) || 
		(ERROR_OK != device->drv->ep.set_OUT_handler(vsfusbd_CDC_param.ep_out, vsfusbd_CDCData_OUT_hanlder)))
	{
		return ERROR_FAIL;
	}
	
	port = vsfusbd_CDC_param.gpio_rts_port;
	pin = vsfusbd_CDC_param.gpio_rts_pin;
	interfaces->gpio.init(port);
	interfaces->gpio.config(port, pin, pin, 0, 0);
	port = vsfusbd_CDC_param.gpio_dtr_port;
	pin = vsfusbd_CDC_param.gpio_dtr_pin;
	interfaces->gpio.init(port);
	interfaces->gpio.config(port, pin, pin, 0, 0);
	port = vsfusbd_CDC_param.usart_port;
	interfaces->usart.init(port);
	return interfaces->usart.config(port, vsfusbd_CDC_LineCoding.bitrate, 
		vsfusbd_CDC_LineCoding.datatype, vsfusbd_CDC_LineCoding.paritytype, 
		vsfusbd_CDC_LineCoding.stopbittype, 0);
}

static RESULT vsfusbd_CDCData_class_poll(struct vsfusbd_device_t *device)
{
	if (!vsfusbd_CDC_OutEn)
	{
		struct usart_status_t status;
		interfaces->usart.status(vsfusbd_CDC_param.usart_port, &status);
		if (status.tx_buff_avail >= 64)
		{
			vsfusbd_CDC_OutEn = true;
			device->drv->ep.set_OUT_state(vsfusbd_CDC_param.ep_out, 
							USB_EP_STAT_ACK);
		}
	}
	return interfaces->usart.poll(vsfusbd_CDC_param.usart_port);
}

static RESULT vsfusbd_CDCMaster_GetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->length != 7) || (request->value != 0))
	{
		return ERROR_FAIL;
	}
	
	SET_LE_U32(&vsfusbd_CDC_LineCoding_buffer[0], 
				vsfusbd_CDC_LineCoding.bitrate);
	vsfusbd_CDC_LineCoding_buffer[4] = vsfusbd_CDC_LineCoding.stopbittype;
	vsfusbd_CDC_LineCoding_buffer[5] = vsfusbd_CDC_LineCoding.paritytype;
	vsfusbd_CDC_LineCoding_buffer[6] = vsfusbd_CDC_LineCoding.datatype;
	buffer->buffer = vsfusbd_CDC_LineCoding_buffer;
	buffer->size = 7;
	
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_GetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_CDCMaster_SetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->length != 7) || (request->value != 0))
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = vsfusbd_CDC_LineCoding_buffer;
	buffer->size = 7;
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_SetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	uint8_t port;
	
	vsfusbd_CDC_LineCoding.bitrate = GET_LE_U32(&buffer->buffer[0]);
	vsfusbd_CDC_LineCoding.stopbittype = buffer->buffer[4];
	vsfusbd_CDC_LineCoding.paritytype = buffer->buffer[5];
	vsfusbd_CDC_LineCoding.datatype = buffer->buffer[6];
	port = vsfusbd_CDC_param.usart_port;
	return interfaces->usart.config(port, vsfusbd_CDC_LineCoding.bitrate, 
		vsfusbd_CDC_LineCoding.datatype, vsfusbd_CDC_LineCoding.paritytype, 
		vsfusbd_CDC_LineCoding.stopbittype, 0);
}

static RESULT vsfusbd_CDCMaster_SetControlLineState_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t port;
	uint32_t pin;
	
	if ((request->length != 0) || (request->value & USBCDC_CONTROLLINE_MASK))
	{
		return ERROR_FAIL;
	}
	
	port = vsfusbd_CDC_param.gpio_dtr_port;
	pin = vsfusbd_CDC_param.gpio_dtr_pin;
	if (request->value & USBCDC_CONTROLLINE_DTR)
	{
		interfaces->gpio.out(port, pin, pin);
	}
	else
	{
		interfaces->gpio.out(port, pin, 0);
	}
	port = vsfusbd_CDC_param.gpio_rts_port;
	pin = vsfusbd_CDC_param.gpio_rts_pin;
	if (request->value & USBCDC_CONTROLLINE_RTS)
	{
		interfaces->gpio.out(port, pin, pin);
	}
	else
	{
		interfaces->gpio.out(port, pin, 0);
	}
	
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_SetControlLineState_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_CDCMaster_SendBreak_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (request->length != 0)
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_SendBreak_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static struct vsfusbd_setup_filter_t vsfusbd_CDCMaster_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_GET_LINE_CODING,
		vsfusbd_CDCMaster_GetLineCoding_prepare,
		vsfusbd_CDCMaster_GetLineCoding_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_SET_LINE_CODING,
		vsfusbd_CDCMaster_SetLineCoding_prepare,
		vsfusbd_CDCMaster_SetLineCoding_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_SET_CONTROL_LINE_STATE,
		vsfusbd_CDCMaster_SetControlLineState_prepare,
		vsfusbd_CDCMaster_SetControlLineState_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_SEND_BREAK,
		vsfusbd_CDCMaster_SendBreak_prepare,
		vsfusbd_CDCMaster_SendBreak_process
	}
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCMaster_class = 
{
	NULL,
	vsfusbd_CDCMaster_class_setup,
	
	NULL, NULL, NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCData_class = 
{
	NULL,
	NULL,
	
	vsfusbd_CDCData_class_init, NULL, vsfusbd_CDCData_class_poll
};

RESULT vsfusbd_CDC_set_param(struct vsfusbd_CDC_param_t *param)
{
	vsfusbd_CDC_param = *param;
	return ERROR_OK;
}

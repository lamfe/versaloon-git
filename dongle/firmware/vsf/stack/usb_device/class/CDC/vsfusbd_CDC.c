#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_CDC.h"

struct vsfusbd_CDC_param_t *vsfusbd_CDC_param_list = NULL;

static struct vsfusbd_CDC_param_t* vsfusbd_CDC_find_param(uint8_t iface)
{
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_param_list;
	
	while (tmp != NULL)
	{
		if ((tmp->master_iface == iface) || (tmp->slave_iface == iface))
		{
			break;
		}
		tmp = sllist_get_container(tmp->list.next, struct vsfusbd_CDC_param_t, 
									list);
	}
	return tmp;
}

static RESULT vsfusbd_CDCData_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	int8_t iface = device->ep_OUT_iface_map[ep];
	struct vsfusbd_CDC_param_t *tmp = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64];
	struct usart_status_t status;
	
	if (iface < 0)
	{
		return ERROR_FAIL;
	}
	tmp = vsfusbd_CDC_find_param(iface);
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if (pkg_size > 64)
	{
		return ERROR_FAIL;
	}
	device->drv->ep.read_OUT_buffer(ep, buffer, pkg_size);
	if (tmp->cdc_out_enable)
	{
		device->drv->ep.set_OUT_state(ep, USB_EP_STAT_ACK);
	}
	
	interfaces->usart.status(tmp->usart_port, &status);
	if (status.tx_buff_avail < 64)
	{
		tmp->cdc_out_enable = false;
	}
	return interfaces->usart.send(tmp->usart_port, buffer, pkg_size);
}

static RESULT vsfusbd_CDCData_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	int8_t iface = device->ep_OUT_iface_map[ep];
	struct vsfusbd_CDC_param_t *tmp = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64];
	struct usart_status_t status;
	
	if (iface < 0)
	{
		return ERROR_FAIL;
	}
	tmp = vsfusbd_CDC_find_param(iface);
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	interfaces->usart.status(tmp->usart_port, &status);
	if (status.rx_buff_size)
	{
		pkg_size = (status.rx_buff_size > sizeof(buffer)) ? sizeof(buffer) : 
				status.rx_buff_size;
		interfaces->usart.receive(tmp->usart_port, buffer, pkg_size);
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

static RESULT vsfusbd_CDCData_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_find_param(iface);
	uint8_t port;
	uint32_t pin;
	
	if ((NULL == tmp) || 
		(ERROR_OK != device->drv->ep.set_IN_handler(tmp->ep_in, 
												vsfusbd_CDCData_IN_hanlder)) || 
		(ERROR_OK != device->drv->ep.set_OUT_handler(tmp->ep_out, 
												vsfusbd_CDCData_OUT_hanlder)))
	{
		return ERROR_FAIL;
	}
	
	port = tmp->gpio_rts_port;
	pin = tmp->gpio_rts_pin;
	interfaces->gpio.init(port);
	interfaces->gpio.config(port, pin, pin, 0, 0);
	port = tmp->gpio_dtr_port;
	pin = tmp->gpio_dtr_pin;
	interfaces->gpio.init(port);
	interfaces->gpio.config(port, pin, pin, 0, 0);
	port = tmp->usart_port;
	interfaces->usart.init(port);
	return interfaces->usart.config(port, tmp->line_coding.bitrate, 
		tmp->line_coding.datatype, tmp->line_coding.paritytype, 
		tmp->line_coding.stopbittype, 0);
}

static RESULT vsfusbd_CDCData_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_find_param(iface);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	if (!tmp->cdc_out_enable)
	{
		struct usart_status_t status;
		interfaces->usart.status(tmp->usart_port, &status);
		if (status.tx_buff_avail >= 64)
		{
			tmp->cdc_out_enable = true;
			device->drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK);
		}
	}
	return interfaces->usart.poll(tmp->usart_port);
}

static RESULT vsfusbd_CDCMaster_GetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_find_param(request->index);
	
	if ((NULL == tmp) || (request->length != 7) || (request->value != 0))
	{
		return ERROR_FAIL;
	}
	
	SET_LE_U32(&tmp->line_coding_buffer[0], tmp->line_coding.bitrate);
	tmp->line_coding_buffer[4] = tmp->line_coding.stopbittype;
	tmp->line_coding_buffer[5] = tmp->line_coding.paritytype;
	tmp->line_coding_buffer[6] = tmp->line_coding.datatype;
	buffer->buffer = tmp->line_coding_buffer;
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
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_find_param(request->index);
	
	if ((NULL == tmp) || (request->length != 7) || (request->value != 0))
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = tmp->line_coding_buffer;
	buffer->size = 7;
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_SetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_find_param(request->index);
	
	tmp->line_coding.bitrate = GET_LE_U32(&buffer->buffer[0]);
	tmp->line_coding.stopbittype = buffer->buffer[4];
	tmp->line_coding.paritytype = buffer->buffer[5];
	tmp->line_coding.datatype = buffer->buffer[6];
	return interfaces->usart.config(tmp->usart_port, tmp->line_coding.bitrate, 
		tmp->line_coding.datatype, tmp->line_coding.paritytype, 
		tmp->line_coding.stopbittype, 0);
}

static RESULT vsfusbd_CDCMaster_SetControlLineState_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_CDC_param_t *tmp = vsfusbd_CDC_find_param(request->index);
	uint8_t port;
	uint32_t pin;
	
	if ((NULL == tmp) || (request->length != 0) || 
		(request->value & USBCDC_CONTROLLINE_MASK))
	{
		return ERROR_FAIL;
	}
	
	port = tmp->gpio_dtr_port;
	pin = tmp->gpio_dtr_pin;
	if (request->value & USBCDC_CONTROLLINE_DTR)
	{
		interfaces->gpio.out(port, pin, pin);
	}
	else
	{
		interfaces->gpio.out(port, pin, 0);
	}
	port = tmp->gpio_rts_port;
	pin = tmp->gpio_rts_pin;
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
	if ((vsfusbd_CDC_find_param(param->master_iface) != NULL) || 
		(vsfusbd_CDC_find_param(param->slave_iface) != NULL))
	{
		return ERROR_FAIL;
	}
	
	if (NULL == vsfusbd_CDC_param_list)
	{
		sllist_init_node(param->list);
		vsfusbd_CDC_param_list = param;
	}
	else
	{
		sllint_insert(param->list, vsfusbd_CDC_param_list->list);
	}
	
	return ERROR_OK;
}

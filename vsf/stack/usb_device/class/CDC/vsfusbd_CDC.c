#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"
#include "tool/buffer/buffer.h"

#include "vsfusbd_CDC.h"

static vsf_err_t vsfusbd_CDCData_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_CDC_param_t *param = NULL;
	uint16_t pkg_size, ep_size;
	uint8_t buffer[64];
	struct vsf_buffer_t tx_buffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	ep_size = device->drv->ep.get_OUT_epsize(ep);
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if (pkg_size > ep_size)
	{
		return VSFERR_FAIL;
	}
	device->drv->ep.read_OUT_buffer(ep, buffer, pkg_size);
	if (param->cdc_out_enable)
	{
		device->drv->ep.set_OUT_state(ep, USB_EP_STAT_ACK);
	}
	
	if (vsf_fifo_get_avail_length(&param->usart_stream->fifo_tx) < ep_size)
	{
		param->cdc_out_enable = false;
	}
	tx_buffer.buffer= buffer;
	tx_buffer.size = pkg_size;
	return usart_stream_tx(param->usart_stream, &tx_buffer);
}

static vsf_err_t vsfusbd_CDCData_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_IN_iface_map[ep];
	struct vsfusbd_CDC_param_t *param = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64];
	uint32_t rx_data_length;
	struct vsf_buffer_t rx_buffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	rx_data_length = vsf_fifo_get_data_length(&param->usart_stream->fifo_rx);
	if (rx_data_length)
	{
		pkg_size = device->drv->ep.get_IN_epsize(ep);
		pkg_size = (rx_data_length > pkg_size) ? pkg_size : rx_data_length;
		rx_buffer.buffer = buffer;
		rx_buffer.size = pkg_size;
		usart_stream_rx(param->usart_stream, &rx_buffer);
		device->drv->ep.write_IN_buffer(ep, buffer, pkg_size);
		device->drv->ep.set_IN_count(ep, pkg_size);
	}
	else
	{
		device->drv->ep.set_IN_count(ep, 0);
	}
	device->drv->ep.set_IN_state(ep, USB_EP_STAT_ACK);
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCData_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
			(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	uint8_t port;
	uint32_t pin;
	
	if ((NULL == param) || 
		device->drv->ep.set_IN_handler(param->ep_in,
										vsfusbd_CDCData_IN_hanlder) || 
		device->drv->ep.set_IN_count(param->ep_in, 0) || 
		device->drv->ep.set_IN_state(param->ep_in, USB_EP_STAT_ACK) || 
		device->drv->ep.set_OUT_handler(param->ep_out,
										vsfusbd_CDCData_OUT_hanlder))
	{
		return VSFERR_FAIL;
	}
	
	if (param->gpio_rts_enable)
	{
		port = param->gpio_rts_port;
		pin = param->gpio_rts_pin;
		core_interfaces.gpio.init(port);
		core_interfaces.gpio.config(port, pin, pin, 0, 0);
	}
	if (param->gpio_dtr_enable)
	{
		port = param->gpio_dtr_port;
		pin = param->gpio_dtr_pin;
		core_interfaces.gpio.init(port);
		core_interfaces.gpio.config(port, pin, pin, 0, 0);
	}
	
	usart_stream_init(param->usart_stream);
	
	param->usart_stream->usart_info.datalength = param->line_coding.datatype;
	param->usart_stream->usart_info.baudrate = param->line_coding.bitrate;
	param->usart_stream->usart_info.mode = 0;
	switch(param->line_coding.stopbittype)
	{
	default:
	case 0:
		param->usart_stream->usart_info.mode |= USART_STOPBITS_1;
		break;
	case 1:
		param->usart_stream->usart_info.mode |= USART_STOPBITS_1P5;
		break;
	case 2:
		param->usart_stream->usart_info.mode |= USART_STOPBITS_2;
		break;
	}
	switch(param->line_coding.paritytype)
	{
	default:
	case 0:
		param->usart_stream->usart_info.mode |= USART_PARITY_NONE;
		param->usart_stream->usart_info.datalength = 8;
		break;
	case 1:
		param->usart_stream->usart_info.mode |= USART_PARITY_ODD;
		param->usart_stream->usart_info.datalength = 9;
		break;
	case 2:
		param->usart_stream->usart_info.mode |= USART_PARITY_EVEN;
		param->usart_stream->usart_info.datalength = 9;
		break;
	}
	return usart_stream_config(param->usart_stream);
}

static vsf_err_t vsfusbd_CDCData_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
			(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	if (!param->cdc_out_enable)
	{
		uint16_t ep_size = device->drv->ep.get_OUT_epsize(param->ep_out);
		if (vsf_fifo_get_avail_length(&param->usart_stream->fifo_tx) >= ep_size)
		{
			param->cdc_out_enable = true;
			device->drv->ep.set_OUT_state(param->ep_out, USB_EP_STAT_ACK);
		}
	}
	if (param->usart_stream->usart_index != IFS_DUMMY_PORT)
	{
		return usart_stream_poll(param->usart_stream);
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCMaster_GetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
			(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 7) || (request->value != 0))
	{
		return VSFERR_FAIL;
	}
	
	SET_LE_U32(&param->line_coding_buffer[0], param->line_coding.bitrate);
	param->line_coding_buffer[4] = param->line_coding.stopbittype;
	param->line_coding_buffer[5] = param->line_coding.paritytype;
	param->line_coding_buffer[6] = param->line_coding.datatype;
	buffer->buffer = param->line_coding_buffer;
	buffer->size = 7;
	
	return VSFERR_NONE;
}
static vsf_err_t vsfusbd_CDCMaster_GetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCMaster_SetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
			(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 7) || (request->value != 0))
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = param->line_coding_buffer;
	buffer->size = 7;
	return VSFERR_NONE;
}
static vsf_err_t vsfusbd_CDCMaster_SetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
			(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	
	param->line_coding.bitrate = GET_LE_U32(&buffer->buffer[0]);
	param->line_coding.stopbittype = buffer->buffer[4];
	param->line_coding.paritytype = buffer->buffer[5];
	param->line_coding.datatype = buffer->buffer[6];
	
	param->usart_stream->usart_info.datalength = param->line_coding.datatype;
	param->usart_stream->usart_info.baudrate = param->line_coding.bitrate;
	param->usart_stream->usart_info.mode = 0;
	switch(param->line_coding.stopbittype)
	{
	default:
	case 0:
		param->usart_stream->usart_info.mode |= USART_STOPBITS_1;
		break;
	case 1:
		param->usart_stream->usart_info.mode |= USART_STOPBITS_1P5;
		break;
	case 2:
		param->usart_stream->usart_info.mode |= USART_STOPBITS_2;
		break;
	}
	switch(param->line_coding.paritytype)
	{
	default:
	case 0:
		param->usart_stream->usart_info.mode |= USART_PARITY_NONE;
		param->usart_stream->usart_info.datalength = 8;
		break;
	case 1:
		param->usart_stream->usart_info.mode |= USART_PARITY_ODD;
		param->usart_stream->usart_info.datalength = 9;
		break;
	case 2:
		param->usart_stream->usart_info.mode |= USART_PARITY_EVEN;
		param->usart_stream->usart_info.datalength = 9;
		break;
	}
	return usart_stream_config(param->usart_stream);
}

static vsf_err_t vsfusbd_CDCMaster_SetControlLineState_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDC_param_t *param = 
			(struct vsfusbd_CDC_param_t *)config->iface[iface].protocol_param;
	uint8_t port;
	uint32_t pin;
	
	if ((NULL == param) || (request->length != 0) || 
		(request->value & ~USBCDC_CONTROLLINE_MASK))
	{
		return VSFERR_FAIL;
	}
	
	if (param->gpio_dtr_enable)
	{
		port = param->gpio_dtr_port;
		pin = param->gpio_dtr_pin;
		if (request->value & USBCDC_CONTROLLINE_DTR)
		{
			core_interfaces.gpio.out(port, pin, pin);
		}
		else
		{
			core_interfaces.gpio.out(port, pin, 0);
		}
	}
	if (param->gpio_rts_enable)
	{
		port = param->gpio_rts_port;
		pin = param->gpio_rts_pin;
		if (request->value & USBCDC_CONTROLLINE_RTS)
		{
			core_interfaces.gpio.out(port, pin, pin);
		}
		else
		{
			core_interfaces.gpio.out(port, pin, 0);
		}
	}
	
	return vsfusbd_request_prepare_0(device, buffer);
}
static vsf_err_t vsfusbd_CDCMaster_SetControlLineState_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCMaster_SendBreak_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (request->length != 0)
	{
		return VSFERR_FAIL;
	}
	
	return vsfusbd_request_prepare_0(device, buffer);
}
static vsf_err_t vsfusbd_CDCMaster_SendBreak_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_CDCMaster_class_setup[] = 
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
	NULL, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_CDCMaster_class_setup,
	
	NULL, NULL, NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCData_class = 
{
	NULL, NULL, NULL,
	
	vsfusbd_CDCData_class_init, NULL, vsfusbd_CDCData_class_poll
};

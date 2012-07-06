#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_CDCACM.h"

static vsf_err_t vsfusbd_CDCACMData_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_CDCACM_param_t *param = NULL;
	uint16_t pkg_size, ep_size;
	uint8_t buffer[64];
	struct vsf_buffer_t tx_buffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
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
	if (param->cdcacm_out_enable)
	{
		device->drv->ep.enable_OUT(ep);
	}
	
	if (stream_get_free_size(param->stream_tx) < ep_size)
	{
		param->cdcacm_out_enable = false;
	}
	tx_buffer.buffer = buffer;
	tx_buffer.size = pkg_size;
	return (stream_tx(param->stream_tx, &tx_buffer) == tx_buffer.size) ?
				VSFERR_NONE : VSFERR_FAIL;
}

static vsf_err_t vsfusbd_CDCACMData_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_IN_iface_map[ep];
	struct vsfusbd_CDCACM_param_t *param = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64];
	uint32_t rx_data_length;
	struct vsf_buffer_t rx_buffer;
	
	if (iface < 0)
	{
		return VSFERR_FAIL;
	}
	param = (struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	pkg_size = device->drv->ep.get_IN_epsize(ep);
	rx_buffer.buffer = buffer;
	rx_buffer.size = pkg_size;
	rx_data_length = stream_rx(param->stream_rx, &rx_buffer);
	if (rx_data_length)
	{
		pkg_size = (rx_data_length > pkg_size) ? pkg_size : rx_data_length;
		device->drv->ep.write_IN_buffer(ep, buffer, pkg_size);
		device->drv->ep.set_IN_count(ep, pkg_size);
	}
	else
	{
		device->drv->ep.set_IN_count(ep, 0);
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMData_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || 
		(NULL == param->stream_tx) || (NULL == param->stream_rx) || 
		device->drv->ep.set_IN_handler(param->ep_in,
										vsfusbd_CDCACMData_IN_hanlder) || 
		device->drv->ep.set_IN_count(param->ep_in, 0) || 
		device->drv->ep.set_OUT_handler(param->ep_out,
										vsfusbd_CDCACMData_OUT_hanlder))
	{
		return VSFERR_FAIL;
	}
	param->control_line = 0;
	param->cdcacm_out_enable = false;
	
	if ((param->callback.set_line_coding != NULL) &&
		(param->callback.set_line_coding(&param->line_coding)))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMData_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if (NULL == param)
	{
		return VSFERR_FAIL;
	}
	
	if (!param->cdcacm_out_enable)
	{
		uint16_t ep_size = device->drv->ep.get_OUT_epsize(param->ep_out);
		
		if (stream_get_free_size(param->stream_tx) >= ep_size)
		{
			param->cdcacm_out_enable = true;
			device->drv->ep.enable_OUT(param->ep_out);
		}
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_GetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_CDCACM_line_coding_t *line_coding = &param->line_coding;
	
	if ((NULL == param) || (request->length != 7) || (request->value != 0))
	{
		return VSFERR_FAIL;
	}
	
	SET_LE_U32(&param->line_coding_buffer[0], line_coding->bitrate);
	param->line_coding_buffer[4] = line_coding->stopbittype;
	param->line_coding_buffer[5] = line_coding->paritytype;
	param->line_coding_buffer[6] = line_coding->datatype;
	buffer->buffer = param->line_coding_buffer;
	buffer->size = 7;
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_SetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 7) || (request->value != 0))
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = param->line_coding_buffer;
	buffer->size = 7;
	return VSFERR_NONE;
}
static vsf_err_t vsfusbd_CDCACMMaster_SetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	struct vsfusbd_CDCACM_line_coding_t *line_coding = &param->line_coding;
	
	line_coding->bitrate = GET_LE_U32(&buffer->buffer[0]);
	line_coding->stopbittype = buffer->buffer[4];
	line_coding->paritytype = buffer->buffer[5];
	line_coding->datatype = buffer->buffer[6];
	
	if ((param->callback.set_line_coding != NULL) &&
		(param->callback.set_line_coding(line_coding)))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_SetControlLineState_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if ((NULL == param) || (request->length != 0) || 
		(request->value & ~USBCDCACM_CONTROLLINE_MASK))
	{
		return VSFERR_FAIL;
	}
	
	param->control_line = (uint8_t)request->value;
	if ((param->callback.set_control_line != NULL) &&
		(param->callback.set_control_line(param->control_line)))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_SendBreak_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (request->length != 0)
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_SendEncapsulatedCommand_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if (request->length > param->callback.encapsulated_data_buffer.size)
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = param->callback.encapsulated_data_buffer.buffer;
	buffer->size = request->length;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_SendEncapsulatedCommand_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if ((param->callback.send_encapsulated_command != NULL) &&
		param->callback.send_encapsulated_command(buffer))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_CDCACMMaster_GetEncapsulatedResponse_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	struct vsfusbd_CDCACM_param_t *param = 
		(struct vsfusbd_CDCACM_param_t *)config->iface[iface].protocol_param;
	
	if (request->length > param->callback.encapsulated_data_buffer.size)
	{
		return VSFERR_FAIL;
	}
	
	buffer->buffer = param->callback.encapsulated_data_buffer.buffer;
	buffer->size = request->length;
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_CDCACMMaster_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_GET_LINE_CODING,
		vsfusbd_CDCACMMaster_GetLineCoding_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_SET_LINE_CODING,
		vsfusbd_CDCACMMaster_SetLineCoding_prepare,
		vsfusbd_CDCACMMaster_SetLineCoding_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_SET_CONTROL_LINE_STATE,
		vsfusbd_CDCACMMaster_SetControlLineState_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_SEND_BREAK,
		vsfusbd_CDCACMMaster_SendBreak_prepare,
		NULL
	},
/*	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_SET_COMM_FEATURE,
		vsfusbd_CDCACMMaster_SetCommFeature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_GET_COMM_FEATURE,
		vsfusbd_CDCACMMaster_GetCommFeature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_CLEAR_COMM_FEATURE,
		vsfusbd_CDCACMMaster_ClearCommFeature_prepare,
		NULL
	},
*/	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_SEND_ENCAPSULATED_COMMAND,
		vsfusbd_CDCACMMaster_SendEncapsulatedCommand_prepare,
		vsfusbd_CDCACMMaster_SendEncapsulatedCommand_process,
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCACMREQ_GET_ENCAPSULATED_RESPONSE,
		vsfusbd_CDCACMMaster_GetEncapsulatedResponse_prepare,
		NULL,
	},
	VSFUSBD_SETUP_NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCACMMaster_class = 
{
	NULL, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_CDCACMMaster_class_setup,
	
	NULL, NULL, NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCACMData_class = 
{
	NULL, NULL, NULL,
	
	vsfusbd_CDCACMData_class_init, NULL, vsfusbd_CDCACMData_class_poll
};

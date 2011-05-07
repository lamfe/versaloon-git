/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "app_type.h"

#include "vsf_usbd.h"

static RESULT vsfusbd_device_get_descriptor(struct vsfusbd_device_t *device, 
		struct vsfusbd_desc_filter_t *filter, uint8_t type, uint8_t index, 
		uint16_t lanid, struct vsf_buffer_t *buffer)
{
	while ((filter->buffer.buffer != NULL) && (filter->buffer.size != 0))
	{
		if ((filter->type == type) && (filter->index == index) && 
			(filter->lanid == lanid))
		{
			buffer->size = filter->buffer.size;
			buffer->buffer = filter->buffer.buffer;
			
			if (filter->read != NULL)
			{
				return filter->read(buffer);
			}
			return ERROR_OK;
		}
		fitler++;
	}
	return ERROR_FAIL;
}

RESULT vsfusbd_device_init(struct vsfusbd_device_t *device)
{
	if ((ERROR_OK != device->drv->init()) || 
		(ERROR_OK != device->drv->connect()) || 
		((deivce->callback.init != NULL) && 
			(ERROR_OK != deivce->callback.init())))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT vsfusbd_device_fini(struct vsfusbd_device_t *device)
{
	if ((ERROR_OK != device->drv->fini()) || 
		(ERROR_OK != device->drv->disconnect()) || 
		((deivce->callback.fini != NULL) && 
			(ERROR_OK != deivce->callback.fini())))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT vsfusbd_device_poll(struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *cur_config = &device->config[device->configuration];
	uint8_t i;
	
	if ((ERROR_OK != device->drv->poll()) || 
		((deivce->callback.poll != NULL) && 
			(ERROR_OK != deivce->callback.poll())))
	{
		return ERROR_FAIL;
	}
	for (i = 0; i < config->iface_num; i++)
	{
		if ((config->iface[i].poll != NULL) && 
			(ERROR_OK != config->iface[i].poll()))
		{
			return ERROR_FAIL;
		}
	}
	return ERROR_OK;
}

RESULT vsfusbd_ep_in_nb(struct vsfusbd_device_t *device, 
						uint8_t ep, uint8_t *buff, uint16_t size)
{
}

RESULT vsfusbd_ep_in_nb_isready(struct vsfusbd_device_t *device, uint8_t ep, 
								bool *error)
{
}

RESULT vsfusbd_ep_out_nb(struct vsfusbd_device_t *device, 
							uint8_t ep, uint8_t *buff, uint16_t size)
{
}

RESULT vsfusbd_ep_out_nb_isready(struct vsfusbd_device_t *device, uint8_t ep, 
									bool *error)
{
}

RESULT vsfusbd_ep_in(struct vsfusbd_device_t *device, 
						uint8_t ep, uint8_t *buff, uint16_t size)
{
	bool error = false;
	
	if (ERROR_OK != vsfusbd_ep_in_nb(device, ep, buff, size))
	{
		return ERROR_FAIL;
	}
	while ((ERROR_OK != vsfusbd_ep_in_nb_isready(device, ep, &error)) && 
			!error);
	return error ? ERROR_FAIL : ERROR_OK;
}

RESULT vsfusbd_ep_out(struct vsfusbd_device_t *device, 
						uint8_t ep, uint8_t *buff, uint16_t size)
{
	bool error = false;
	
	if (ERROR_OK != vsfusbd_ep_out_nb(device, ep, buff, size))
	{
		return ERROR_FAIL;
	}
	while ((ERROR_OK != vsfusbd_ep_out_nb_isready(device, ep, &error)) && 
			!error);
	return error ? ERROR_FAIL : ERROR_OK;
}








// standard request handler
static RESULT vsfusbd_stdreq_prepare_0(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	buffer->buff = NULL;
	buffer->size = 0;
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_get_device_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->value != 0) || (request->index != 0))
	{
		return ERROR_FAIL;
	}
	
	device->reply_buff[0] = device->feature;
	device->reply_buff[1] = 0;
	buffer->buff = device->reply_buff;
	buffer->size = USB_STATUS_SIZE;
	return ERROR_OK;
}
static RESULT vsfusbd_stdreq_get_device_status_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_get_interface_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *cur_config = &device->config[device->configuration];
	
	if ((request->value != 0) || 
		(request->index >= cur_config->iface_num))
	{
		return ERROR_FAIL;
	}
	
	device->reply_buff[0] = 0;
	device->reply_buff[1] = 0;
	buffer->buff = device->reply_buff;
	buffer->size = USB_STATUS_SIZE;
	return ERROR_OK;
}
static RESULT vsfusbd_stdreq_get_interface_status_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_get_endpoint_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t ep_num = request->index & 0x7F;
	uint8_t ep_dir = request->index & 0x80;
	enum usb_ep_state_t ep_state;
	
	if ((request->value != 0) || 
		(request->index >= device->drv->ep.max_ep))
	{
		return ERROR_FAIL;
	}
	
	if (ep_dir)
	{
		ep_state = device->drv->ep[ep_num].get_in_status();
	}
	else
	{
		ep_state = device->drv->ep[ep_num].get_out_status();
	}
	
	if (ep_state == USBDRV_EP_STAT_STALLED)
	{
		device->reply_buff[0] = 1;
	}
	device->reply_buff[1] = 0;
	buffer->buff = device->reply_buff;
	buffer->size = USB_STATUS_SIZE;
	return ERROR_OK;
}
static RESULT vsfusbd_stdreq_get_endpoint_status_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_FAIL;
}

static RESULT vsfusbd_stdreq_clear_device_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value != USB_DEV_FEATURE_CMD_REMOTE_WAKEUP))
	{
		return ERROR_FAIL;
	}
	
	device->feature &= ~USB_CFGATTR_REMOTE_WEAKUP;
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_clear_device_feature_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{	
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_clear_interface_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_clear_interface_feature_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_clear_endpoint_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t ep_num = request->index & 0x7F;
	uint8_t ep_dir = request->index & 0x80;
	
	if ((request->value != USB_EP_FEATURE_CMD_HALT) || 
		(ep_num >= device->drv->ep.max_ep))
	{
		return ERROR_FAIL;
	}
	
	if (ep_dir)
	{
		device->drv->ep.set_IN_stat(ep_num, USB_EP_STAT_ACK);
	}
	else
	{
		device->drv->ep.set_OUT_stat(ep_num, USB_EP_STAT_ACK);
	}
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_clear_endpoint_feature_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_set_device_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value != USB_DEV_FEATURE_REMOTE_WAKEUP))
	{
		return ERROR_FAIL;
	}
	
	device->feature |= USB_CFGATTR_REMOTE_WEAKUP;
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_set_device_feature_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_set_interface_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_set_interface_feature_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_set_endpoint_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_FAIL;
}
static RESULT vsfusbd_stdreq_set_endpoint_feature_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_FAIL;
}

static RESULT vsfusbd_stdreq_set_address_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->value > 127) || (request->index != 0) || 
		(device->configuration != 0))
	{
		return ERROR_FAIL;
	}
	
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_set_address_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	return device->drv->set_address((uint8_t)request->value);
}

static RESULT vsfusbd_stdreq_get_device_descriptor_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t type = (request->value >> 8) & 0xFF, index = request->value & 0xFF;
	uint16_t lanid = request->index;
	
	return vsfusbd_device_get_descriptor(device, device->desc_filter, type, 
											index, lanid, buffer);
}
static RESULT vsfusbd_stdreq_get_device_descriptor_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_get_interface_descriptor_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *cur_config = &device->config[device->configuration];
	uint8_t type = (request->value >> 8) & 0xFF, index = request->value & 0xFF;
	uint16_t iface = request->index;
	
	if ((iface > cur_config->num_of_ifaces) || 
		(NULL == cur_config->iface[iface].class_protocol) || 
		(NULL == cur_config->iface[iface].class_protocol.desc_filter))
	{
		return ERROR_FAIL;
	}
	
	return vsfusbd_device_get_descriptor(device, 
			cur_config->iface[iface].class_protocol.desc_filter, type, index, 
			lanid, buffer);
}
static RESULT vsfusbd_stdreq_get_interface_descriptor_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_get_configuration_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->value != 0) || (request->index != 0))
	{
		return ERROR_FAIL;
	}
	
	device->reply_buff[0] = device->configuration;
	buffer->buff = device->reply_buff;
	buffer->size = USB_CONFIGURATION_SIZE;
	return ERROR_OK;
}
static RESULT vsfusbd_stdreq_get_configuration_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_set_configuration_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value >= device->num_of_configuration))
	{
		return ERROR_FAIL;
	}
	
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_set_configuration_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	device->configuration = request->value;
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_get_interface_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *cur_config = &device->config[device->configuration];
	
	if ((request->value != 0) || 
		(iface >= device->config[device->configuration].num_of_ifaces))
	{
		return ERROR_FAIL;
	}
	
	device->reply_buff[0] = cur_config->iface[iface].alternate_setting;
	buffer->buff = device->reply_buff;
	buffer->size = USB_ALTERNATE_SETTING_SIZE;
	return ERROR_OK;
}
static RESULT vsfusbd_stdreq_get_interface_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_stdreq_set_interface_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface_idx = request->index;
	uint8_t alternate_setting = request->value;
	struct vsfusbd_config_t *cur_config = &device->config[device->configuration];
	
	if (iface_idx >= device->config[device->configuration].num_of_ifaces)
	{
		return ERROR_FAIL;
	}
	
	cur_config->iface[iface_idx].alternate_setting = alternate_setting;
	return vsfusbd_stdreq_prepare_0(device, buffer);
}
static RESULT vsfusbd_stdreq_set_interface_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static const struct vsfusbd_setup_filter_t vsfusbd_standard_req_filter[] = 
{
	// USB_REQ_GET_STATUS
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_device_status_prepare,
		vsfusbd_stdreq_get_device_status_process
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_interface_status_prepare,
		vsfusbd_stdreq_get_interface_status_process
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_endpoint_status_prepare,
		vsfusbd_stdreq_get_endpoint_status_process
	},
	// USB_REQ_CLEAR_FEATURE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_device_feature_prepare,
		vsfusbd_stdreq_clear_device_feature_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_interface_feature_prepare,
		vsfusbd_stdreq_clear_interface_feature_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_endpoint_feature_prepare,
		vsfusbd_stdreq_clear_endpoint_feature_process
	},
	// USB_REQ_SET_FEATURE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_device_feature_prepare,
		vsfusbd_stdreq_set_device_feature_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_interface_feature_prepare,
		vsfusbd_stdreq_set_interface_feature_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_endpoint_feature_prepare,
		vsfusbd_stdreq_set_endpoint_feature_process
	},
	// USB_REQ_SET_ADDRESS
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_ADDRESS,
		vsfusbd_stdreq_set_address_prepare,
		vsfusbd_stdreq_set_address_process
	},
	// USB_REQ_GET_DESCRIPTOR
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_DESCRIPTOR,
		vsfusbd_stdreq_get_device_descriptor_prepare,
		vsfusbd_stdreq_get_device_descriptor_process
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_DESCRIPTOR,
		vsfusbd_stdreq_get_interface_descriptor_prepare,
		vsfusbd_stdreq_get_interface_descriptor_process
	},
	// USB_REQ_SET_DESCRIPTOR, not supported
	// USB_REQ_GET_CONFIGURATION
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_CONFIGURATION,
		vsfusbd_stdreq_get_configuration_prepare,
		vsfusbd_stdreq_get_configuration_process
	},
	// USB_REQ_SET_CONFIGURATION
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_CONFIGURATION,
		vsfusbd_stdreq_set_configuration_prepare,
		vsfusbd_stdreq_set_configuration_process
	},
	// USB_REQ_GET_INTERFACE
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_INTERFACE,
		vsfusbd_stdreq_get_interface_prepare,
		vsfusbd_stdreq_get_interface_process
	},
	// USB_REQ_SET_INTERFACE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_SET_INTERFACE,
		vsfusbd_stdreq_set_interface_prepare,
		vsfusbd_stdreq_set_interface_process
	},
	{0, 0, NULL, NULL}
};

static struct vsfusbd_setup_filter_t *vsfusbd_get_request_filter_do(
		struct vsfusbd_device_t *device, struct vsfusbd_setup_filter_t *list)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	while (list->process != NULL)
	{
		if ((list->type == request->type) && 
			(list->request == request->request))
		{
			return list;
		}
		list++;
	}
	return NULL;
}

static struct vsfusbd_setup_filter_t *vsfusbd_get_request_filter(
		struct vsfusbd_device_t *device)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (USB_REQ_GET_TYPE(request->type) == USB_REQ_TYPE_STANDARD)
	{
		return vsfusbd_get_request_filter_do(device, 
										vsfusbd_standard_req_filter);
	}
	
	if (USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_INTERFACE)
	{
		uint8_t iface = request->index;
		struct vsfusbd_config_t *config = &device->config[device->configuration];
		
		if ((index >= config->num_of_ifaces) || 
			(config->iface[iface].class_protocol != NULL))
		{
			return ERROR_FAIL;
		}
		return vsfusbd_get_request_filter_do(device, 
							config->iface[iface].class_protocol.req_filter);
	}
}

// Event handlers
static RESULT vsfusbd_config_ep(struct vsfusbd_drv_t *drv, uint8_t ep, 
		enum usb_ep_state_t in_state. enum usb_ep_state_t out_state)
{
	if ((ep >= drv->ep.max_ep) || 
		(ERROR_OK != drv->ep.set_IN_stat(ep, in_state)) || 
		(ERROR_OK != drv->ep.set_OUT_stat(ep, out_state)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT vsfusbd_on_IN0(struct vsfusbd_device_t *device)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsf_transaction_buffer_t *tbuffer = &ctrl_handler->tbuffer;
	RESULT ret = ERROR_OK;
	uint16_t cur_pkg_size, remain_size;
	uint8_t *buffer;
	
	switch (ctrl_handler->state)
	{
	case USB_CTRL_STAT_IN_DATA:
		remain_size = tbuffer->buffer.size - tbuffer->position;
		cur_pkg_size = (remain_size > ctrl_handler->ep_size) ? 
							ctrl_handler->ep_size : remain_size;
		buffer = &tbuffer->buffer[tbuffer->position];
		
		if ((ERROR_OK != device->drv->write_IN_buf(0, buffer, cur_pkg_size)) || 
			(ERROR_OK != device->drv->set_IN_count(0, cur_pkg_size)))
		{
			ret = ERROR_FAIL;
			break;
		}
		tbuffer->position += cur_pkg_size;
		if (tbuffer->position == tbuffer->buffer.size)
		{
			ctrl_handler->state = USB_CTRL_STAT_LAST_IN_DATA;
		}
		break;
	case USB_CTRL_STAT_LAST_IN_DATA:
		ctrl_handler->state = USB_CTRL_STAT_WAIT_STATUS_OUT;
		ret = vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, 
								USB_EP_STAT_ACK);
		break;
	case USB_CTRL_STAT_WAIT_STATUS_IN:
		if (ERROR_OK != ctrl_handler->filter->process(device, tbuffer->buffer))
		{
			ret = ERROR_FAIL;
			break;
		}
		ctrl_handler->state = USB_CTRL_STAT_WAIT_SETUP;
		vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, USB_EP_STAT_STALL);
		break;
	default:
		break;
	}
	
exit:
	if (ret != ERROR_OK)
	{
		vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, USB_EP_STAT_STALL);
	}
	return ret;
}

static RESULT vsfusbd_on_OUT0(struct vsfusbd_device_t *device)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsf_transaction_buffer_t *tbuffer = &ctrl_handler->tbuffer;
	RESULT ret = ERROR_OK;
	uint16_t cur_pkg_size, remain_size;
	uint8_t *buffer;
	
	switch (ctrl_handler->state)
	{
	case USB_CTRL_STAT_OUT_DATA:
		remain_size = tbuffer->buffer.size - tbuffer->position;
		cur_pkg_size = device->drv->ep.get_OUT_count(0);
		buffer = &tbuffer->buffer[tbuffer->position];
		if ((0 == cur_pkg_size) || 
			((tbuffer->position + cur_pkg_size) > tbuffer->buffer.size))
		{
			ret = ERROR_FAIL;
			break;
		}
		
		ret = device->drv->ep.read_OUT_buf(0, buffer, 0, cur_pkg_size);
		tbuffer->position += cur_pkg_size;
		if (tbuffer->position == tbuffer->buffer.size)
		{
			ctrl_handler->state = USB_CTRL_STAT_LAST_OUT_DATA;
		}
		break;
	case USB_CTRL_STAT_LAST_OUT_DATA:
		ctrl_handler->state = USB_CTRL_STAT_WAIT_STATUS_IN;
		if (ERROR_OK != device->drv->set_IN_count(0, 0))
		{
			ret = ERROR_FAIL;
		}
		break;
	case USB_CTRL_STAT_WAIT_STATUS_OUT:
		cur_pkg_size = device->drv->ep.get_OUT_count(0);
		buffer = tbuffer->buffer;
		if ((cur_pkg_size != 0) || 
			(ERROR_OK != ctrl_handler->filter->process(device, buffer)))
		{
			ret = ERROR_FAIL;
			break;
		}
		ctrl_handler->state = USB_CTRL_STAT_WAIT_SETUP;
		vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, USB_EP_STAT_STALL);
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	
exit:
	if (ret != ERROR_OK)
	{
		vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, USB_EP_STAT_STALL);
	}
	return ret;
}

RESULT vsfusbd_on_SETUP(struct vsfusbd_device_t *device)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	struct vsf_buffer_t *buffer = &ctrl_handler->tbuffer.buffer;
	uint8_t buff[USB_SETUP_PKG_SIZE];
	RESULT ret = ERROR_OK;
	
	if ((ERROR_OK != vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_NACK, 
										USB_EP_STAT_NACK) || 
		(USB_SETUP_PKG_SIZE != device->drv->get_OUT_count()) || 
		(ERROR_OK != device->drv->read_OUT_buf(0, buff, 0, USB_SETUP_PKG_SIZE)))
	{
		ret = ERROR_FAIL;
		goto exit;
	}
	request->type 			= buff[0];
	request->request		= buff[1];
	request->value			= GET_LE_U16(&buff[2]);
	request->index			= GET_LE_U16(&buff[4]);
	request->length			= GET_LE_U16(&buff[6]);
	ctrl_handler->state		= USB_CTRL_STAT_SETTING_UP;
	ctrl_handler->position	= 0;
	ctrl_handler->filter = vsfusbd_get_request_filter(device);
	
	if ((NULL == ctrl_handler->filter) || 
		(NULL == ctrl_handler->filter->prepare) || 
		(ERROR_OK != ctrl_handler->filter->prepare(device, buffer)) || 
		(buffer->size != request->length) || 
		(buffer->size && (NULL == buffer->buff)))
	{
		ret = ERROR_FAIL;
		goto exit;
	}
	
	if (0 == request->length)
	{
		ctrl_handler->state = USB_CTRL_STAT_WAIT_STATUS_IN;
		if (ERROR_OK != device->drv->ep.set_IN_count(0, 0))
		{
			ret = ERROR_FAIL;
			goto exit;
		}
	}
	else
	{
		if (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD)
		{
			ctrl_handler->state = USB_CTRL_STAT_OUT_DATA;
			return vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, 
										USB_EP_STAT_ACK);
		}
		else
		{
			ctrl_handler->state = USB_CTRL_STAT_IN_DATA;
			return vsfusbd_on_IN0(device);
		}
	}
	
exit:
	if (ret != ERROR_OK)
	{
		vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, USB_EP_STAT_STALL);
	}
	return ret;
}

RESULT vsfusbd_on_IN(struct vsfusbd_device_t *device, uint8_t ep)
{
	if (0 == ep)
	{
		return vsfusbd_on_IN0(device);
	}
}

RESULT vsfusbd_on_OUT(struct vsfusbd_device_t *device, uint8_t ep)
{
	if (0 == ep)
	{
		return vsfusbd_on_OUT0(device);
	}
}

RESULT vsfusbd_on_UNDERFLOW(struct vsfusbd_device_t *device, uint8_t ep)
{
	return ERROR_OK;
}

RESULT vsfusbd_on_OVERFLOW(struct vsfusbd_device_t *device, uint8_t ep)
{
	return ERROR_OK;
}

RESULT vsfusbd_on_RESET(struct vsfusbd_device_t *device)
{
	struct vsf_buffer_t desc = {NULL, 0};
	uint16_t pos;
	uint16_t ep_size, ep_addr, ep_index, ep_attr;
	uint8_t attr, feature;
	struct vsfusbd_config_t *config;
	uint8_t i;
#if __VSF_DEBUG__
	uint8_t num_iface;
#endif
	
	device->configuration = 0;
	
	if (ERROR_OK != vsfusbd_device_get_descriptor(device, device->desc_filter, 
											USB_DESC_TYPE_DEVICE, 0, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buff) || (desc.size != USB_DESC_SIZE_DEVICE)
		|| (desc.buff[0] != desc.size) 
		|| (desc.buff[1] != USB_DESC_TYPE_DEVICE)
#endif
		)
	{
		return ERROR_FAIL;
	}
	device->ctrl_handler.ep_size = desc.buff[USB_DESC_DEVICE_OFF_EP0SIZE];
	ep_size = device->ctrl_handler.ep_size;
	device->num_of_configuration = desc.buff[USB_DESC_DEVICE_OFF_CFGNUM];
	config = device->config;
	
	// call user initialization
	if ((config->init != NULL) && (ERROR_OK != config->init()))
	{
		return ERROR_FAIL;
	}
	for (i = 0; i < config->num_of_ifaces; i++)
	{
		if (((config->iface[i].init != NULL) && 
				(ERROR_OK != config->iface[i].init())) || 
			((config->iface[i].class_protocol != NULL) && 
				(config->iface[i].class_protocol.init != NULL) && 
				(ERROR_OK != config->iface[i].class_protocol.init())))
		{
			return ERROR_FAIL;
		}
	}
	
	// config ep0
	if ((ERROR_OK != device->drv->ep.set_type(0, USB_EP_TYPE_CONTROL)) || 
		(ERROR_OK != device->drv->ep.set_IN_size(ep_size)) || 
		(ERROR_OK != device->drv->ep.set_OUT_size(ep_size)) || 
		(ERROR_OK != vsfusbd_config_ep(device->drv, 0, USB_EP_STAT_STALL, 
										USB_EP_STAT_ACK)))
	{
		return ERROR_FAIL;
	}
	
	// config other eps according to descriptors
	if (ERROR_OK != vsfusbd_device_get_descriptor(device, device->desc_filter, 
				USB_DESC_TYPE_CONFIGURATION, device->configuration, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buff) || (desc.size <= USB_DESC_SIZE_CONFIGURATION)
		|| (desc.buff[0] != desc.size)
		|| (desc.buff[1] != USB_DESC_TYPE_CONFIGURATION)
#endif
		)
	{
		return ERROR_FAIL;
	}
	config->num_of_ifaces = desc.buff[USB_DESC_CONFIG_OFF_IFNUM];
	
	// initialize device feature according to 
	// bmAttributes field in configuration descriptor
	attr = desc.buff[USB_DESC_CONFIG_OFF_BMATTR];
	feature = 0;
	if (attr & USB_CFGATTR_SELFPOWERED)
	{
		feature |= USB_DEV_FEATURE_SELFPOWERED;
	}
	if (attr & USB_CFGATTR_REMOTE_WEAKUP)
	{
		feature |= USB_DEV_FEATURE_REMOTE_WEAKUP;
	}
	
#if __VSF_DEBUG__
	num_iface = desc.buff[USB_DESC_CONFIG_OFF_IFNUM];
#endif
	pos = USB_DESC_SIZE_CONFIGURATION;
	while (desc.size > pos)
	{
#if __VSF_DEBUG__
		if ((desc.buff[pos] < 2) || (desc.szie < (pos + desc.buff[pos]))
		{
			return ERROR_FAIL;
		}
#endif
		switch (desc.buff[pos + 1])
		{
#if __VSF_DEBUG__
		case USB_DESC_TYPE_INTERFACE:
			num_ifanc--;
			break;
#endif
		case USB_DESC_TYPE_ENDPOINT:
			ep_addr = desc.buff[pos + USB_DESC_EP_OFF_EPADDR];
			ep_attr = desc.buff[pos + USB_DESC_EP_OFF_EPATTR];
			ep_size = desc.buff[pos + USB_DESC_EP_OFF_EPSIZE];
			ep_index = ep_addr & 0x0F;
#if __VSF_DEBUG__
			if (ep_index > (device->drv->ep_max_num - 1))
			{
				return ERROR_FAIL;
			}
#endif
			switch (ep_attr & 0x03)
			{
			case 0x00:
				ep_attr = USB_EP_TYPE_CONTROL;
				break;
			case 0x01:
				ep_attr = USB_EP_TYPE_ISO;
				break;
			case 0x02:
				ep_attr = USB_EP_TYPE_BULK;
				break;
			case 0x03:
				ep_attr = USB_EP_TYPE_INTERRUPT;
				break;
			}
			device->drv->ep.set_type(ep_index, ep_attr);
			if (ep_addr & 0x80)
			{
				// IN ep
				device->drv->ep.set_IN_size(ep_index, ep_size);
				device->drv->ep.set_IN_stat(ep_index, USB_EP_STAT_NACK);
			}
			else
			{
				// OUT ep
				device->drv->ep.set_OUT_size(ep_index, ep_size);
				device->drv->ep.set_OUT_stat(ep_index, USB_EP_STAT_ACK);
			}
			break;
		}
	}
#if __VSF_DEBUG__
	if (num_iface || (desc.size != pos))
	{
		return ERROR_FAIL;
	}
#endif
	
	if (device->callback.on_RESET != NULL)
	{
		device->callback.on_RESET();
	}
	return ERROR_OK;
}

RESULT vsfusbd_on_WAKEUP(struct vsfusbd_device_t *device)
{
	if (device->callback.on_WAKEUP != NULL)
	{
		device->callback.on_WAKEUP();
	}
	return ERROR_OK;
}

RESULT vsfusbd_on_SUSPEND(struct vsfusbd_device_t *device)
{
	if (device->callback.on_SUSPEND != NULL)
	{
		device->callback.on_SUSPEND();
	}
	return device->drv->suspend();
}

RESULT vsfusbd_on_RESUME(struct vsfusbd_device_t *device)
{
	if (device->callback.on_RESUME != NULL)
	{
		device->callback.on_RESUME();
	}
	return device->dev->resume();
}

RESULT vsfusbd_on_SOF(struct vsfusbd_device_t *device)
{
	if (device->callback.on_SOF != NULL)
	{
		device->callback.on_FOS();
	}
	return ERROR_OK;
}

RESULT vsfusbd_on_ERROR(struct vsfusbd_device_t *device, 
						enum usb_err_type_t type)
{
	if (device->callback.on_ERROR != NULL)
	{
		device->callback.on_ERROR(type);
	}
	return ERROR_OK;
}


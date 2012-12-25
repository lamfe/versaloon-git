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

#include "app_cfg.h"
#include "interfaces.h"

#include "tool/buffer/buffer.h"

#include "vsf_usbd_cfg.h"
#include "vsf_usbd_const.h"
#include "vsf_usbd.h"
#include "vsf_usbd_drv_callback.h"

struct vsfusbd_transact_t vsfusbd_IN_transact[VSFUSBD_CFG_MAX_IN_EP+1];
struct vsfusbd_transact_t vsfusbd_OUT_transact[VSFUSBD_CFG_MAX_OUT_EP+1];

vsf_err_t vsfusbd_device_get_descriptor(struct vsfusbd_device_t *device, 
		struct vsfusbd_desc_filter_t *filter, uint8_t type, uint8_t index, 
		uint16_t lanid, struct vsf_buffer_t *buffer)
{
	while (((filter->buffer.buffer != NULL) && (filter->buffer.size != 0)) || 
			(filter->read != NULL))
	{
		if ((filter->type == type) && (filter->index == index) && 
			(filter->lanid == lanid))
		{
			buffer->size = filter->buffer.size;
			buffer->buffer = filter->buffer.buffer;
			
			if (filter->read != NULL)
			{
				return filter->read(device, buffer);
			}
			return VSFERR_NONE;
		}
		filter++;
	}
	return VSFERR_FAIL;
}

vsf_err_t vsfusbd_device_init(struct vsfusbd_device_t *device)
{
#if VSFUSBD_CFG_AUTOSETUP
	struct vsf_buffer_t desc = {NULL, 0};
	uint8_t i;
#endif
	
	device->configured = false;
	device->configuration = 0;
	device->feature = 0;
	
#if VSFUSBD_CFG_AUTOSETUP
	for (i = 0; i < device->num_of_configuration; i++)
	{
		if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
									USB_DESC_TYPE_CONFIGURATION, i, 0, &desc)
#if __VSF_DEBUG__
			|| (NULL == desc.buffer) 
			|| (desc.size <= USB_DESC_SIZE_CONFIGURATION)
			|| (desc.buffer[0] != USB_DESC_SIZE_CONFIGURATION)
			|| (desc.buffer[1] != USB_DESC_TYPE_CONFIGURATION)
			|| (config->num_of_ifaces != desc.buffer[USB_DESC_CONFIG_OFF_IFNUM])
#endif
			)
		{
			return VSFERR_FAIL;
		}
		device->config[i].configuration_value = 
									desc.buffer[USB_DESC_CONFIG_OFF_CFGVAL];
	}
#endif	// VSFUSBD_CFG_AUTOSETUP
	
	if (device->drv->init(device) || device->drv->connect() || 
		((device->callback.init != NULL) && device->callback.init()))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_device_fini(struct vsfusbd_device_t *device)
{
	if (device->drv->fini() || device->drv->disconnect() || 
		((device->callback.fini != NULL) && device->callback.fini()))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_device_poll(struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	uint8_t i;
	
	if (device->drv->poll() || 
		((device->callback.poll != NULL) && device->callback.poll()))
	{
		return VSFERR_FAIL;
	}
	if (device->configured)
	{
		for (i = 0; i < config->num_of_ifaces; i++)
		{
			if ((config->iface[i].class_protocol != NULL) && 
				(config->iface[i].class_protocol->poll != NULL) && 
				config->iface[i].class_protocol->poll(i, device))
			{
				return VSFERR_FAIL;
			}
		}
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_ep_receive_nb(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsf_transaction_buffer_t *tbuffer =
											&vsfusbd_OUT_transact[ep].tbuffer;
	
	vsfusbd_OUT_transact[ep].pkt.out.isshort = false;
	tbuffer->position = 0;
	
	return device->drv->ep.enable_OUT(ep);
}

vsf_err_t vsfusbd_ep_receive_nb_isready(struct vsfusbd_device_t *device,
										uint8_t ep, uint32_t *size)
{
	struct vsf_transaction_buffer_t *tbuffer =
											&vsfusbd_OUT_transact[ep].tbuffer;
	uint16_t position = tbuffer->position, buffer_size = tbuffer->buffer.size;
	
	if (size != NULL)
	{
		*size = buffer_size;
	}
	return vsfusbd_OUT_transact[ep].pkt.out.isshort ? VSFERR_NONE :
				((position >= buffer_size) ? VSFERR_NONE : VSFERR_NOT_READY);
}

vsf_err_t vsfusbd_ep_receive(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsf_transaction_buffer_t *tbuffer =
											&vsfusbd_OUT_transact[ep].tbuffer;
	vsf_err_t err = VSFERR_NONE;
	
	if (vsfusbd_ep_receive_nb(device, ep))
	{
		return VSFERR_FAIL;
	}
	while (1)
	{
		device->drv->poll();
		err = vsfusbd_ep_receive_nb_isready(device, ep, &tbuffer->buffer.size);
		if (!err || (err && (err != VSFERR_NOT_READY)))
		{
			break;
		}
	}
	
	return err;
}

vsf_err_t vsfusbd_ep_send_nb(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsf_transaction_buffer_t *tbuffer = &vsfusbd_IN_transact[ep].tbuffer;
	uint8_t *buffer_ptr;
	uint32_t remain_size;
	uint16_t pkg_size;
	
	tbuffer->position = 0;
	
	remain_size = tbuffer->buffer.size;
	pkg_size = device->drv->ep.get_IN_epsize(ep);
	vsfusbd_IN_transact[ep].pkt.in.num = (remain_size + pkg_size - 1) / pkg_size;
	if (vsfusbd_IN_transact[ep].pkt.in.zlp && !(tbuffer->buffer.size % pkg_size))
	{
		vsfusbd_IN_transact[ep].pkt.in.num++;
	}
	pkg_size = (remain_size > pkg_size) ? pkg_size : remain_size;
	
	buffer_ptr = tbuffer->buffer.buffer;
	device->drv->ep.write_IN_buffer(ep, buffer_ptr, pkg_size);
	device->drv->ep.set_IN_count(ep, pkg_size);
	tbuffer->position = pkg_size;
#if VSFUSBD_CFG_DBUFFER_EN
	if (device->drv->ep.is_IN_dbuffer(ep))
	{
		device->drv->ep.switch_IN_buffer(ep);
		
		remain_size -= pkg_size;
		if (remain_size)
		{
			buffer_ptr += pkg_size;
			pkg_size = (remain_size > pkg_size) ? pkg_size : remain_size;
			device->drv->ep.write_IN_buffer(ep, buffer_ptr, pkg_size);
			device->drv->ep.set_IN_count(ep, pkg_size);
			tbuffer->position += pkg_size;
		}
		else
		{
			device->drv->ep.set_IN_count(ep, 0);
		}
	}
#endif
	
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_ep_send_nb_isready(struct vsfusbd_device_t *device, uint8_t ep)
{
	struct vsf_transaction_buffer_t *tbuffer = &vsfusbd_IN_transact[ep].tbuffer;
	uint16_t position = tbuffer->position, buffer_size = tbuffer->buffer.size;
	return (position >= buffer_size) ? VSFERR_NONE : VSFERR_NOT_READY;
}

vsf_err_t vsfusbd_ep_send(struct vsfusbd_device_t *device, uint8_t ep)
{
	vsf_err_t err = VSFERR_NONE;
	
	if (vsfusbd_ep_send_nb(device, ep))
	{
		return VSFERR_FAIL;
	}
	while (1)
	{
		device->drv->poll();
		err = vsfusbd_ep_send_nb_isready(device, ep);
		if (!err || (err && (err != VSFERR_NOT_READY)))
		{
			break;
		}
	}
	
	return err;
}








static vsf_err_t vsfusbd_stdreq_get_device_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	
	if ((request->value != 0) || (request->index != 0))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = device->feature;
	ctrl_handler->ctrl_reply_buffer[1] = 0;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_STATUS_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_interface_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	
	if ((request->value != 0) || 
		(request->index >= config->num_of_ifaces))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = 0;
	ctrl_handler->ctrl_reply_buffer[1] = 0;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_STATUS_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_endpoint_status_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	uint8_t ep_num = request->index & 0x7F;
	uint8_t ep_dir = request->index & 0x80;
	
	if ((request->value != 0) || 
		(request->index >= *device->drv->ep.num_of_ep))
	{
		return VSFERR_FAIL;
	}
	
	if ((ep_dir && device->drv->ep.is_IN_stall(ep_num)) || 
		(!ep_dir && device->drv->ep.is_OUT_stall(ep_num)))
	{
		ctrl_handler->ctrl_reply_buffer[0] = 1;
	}
	else
	{
		ctrl_handler->ctrl_reply_buffer[0] = 0;
	}
	ctrl_handler->ctrl_reply_buffer[1] = 0;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_STATUS_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_clear_device_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value != USB_DEV_FEATURE_CMD_REMOTE_WAKEUP))
	{
		return VSFERR_FAIL;
	}
	
	device->feature &= ~USB_CFGATTR_REMOTE_WEAKUP;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_clear_interface_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_clear_endpoint_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t ep_num = request->index & 0x7F;
	uint8_t ep_dir = request->index & 0x80;
	
	if ((request->value != USB_EP_FEATURE_CMD_HALT) || 
		(ep_num >= *device->drv->ep.num_of_ep))
	{
		return VSFERR_FAIL;
	}
	
	if (ep_dir)
	{
		device->drv->ep.reset_IN_toggle(ep_num);
		device->drv->ep.clear_IN_stall(ep_num);
	}
	else
	{
		device->drv->ep.reset_OUT_toggle(ep_num);
		device->drv->ep.clear_OUT_stall(ep_num);
		device->drv->ep.enable_OUT(ep_num);
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_device_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(request->value != USB_DEV_FEATURE_CMD_REMOTE_WAKEUP))
	{
		return VSFERR_FAIL;
	}
	
	device->feature |= USB_CFGATTR_REMOTE_WEAKUP;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_interface_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_endpoint_feature_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return VSFERR_FAIL;
}

static vsf_err_t vsfusbd_stdreq_set_address_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->value > 127) || (request->index != 0) || 
		(device->configuration != 0))
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}
static vsf_err_t vsfusbd_stdreq_set_address_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	device->address = (uint8_t)request->value;
	return device->drv->set_address(device->address);
}

static vsf_err_t vsfusbd_stdreq_get_device_descriptor_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t type = (request->value >> 8) & 0xFF, index = request->value & 0xFF;
	uint16_t lanid = request->index;
	
	return vsfusbd_device_get_descriptor(device, device->desc_filter, type, 
											index, lanid, buffer);
}

static vsf_err_t vsfusbd_stdreq_get_interface_descriptor_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	uint8_t type = (request->value >> 8) & 0xFF, index = request->value & 0xFF;
	uint16_t iface = request->index;
	struct vsfusbd_class_protocol_t *protocol = 
										config->iface[iface].class_protocol;
	
	if ((iface > config->num_of_ifaces) || (NULL == protocol) || 
		((NULL == protocol->desc_filter) && (NULL == protocol->get_desc)))
	{
		return VSFERR_FAIL;
	}
	
	if (vsfusbd_device_get_descriptor(device, 
				protocol->desc_filter, type, index, 0, buffer) && 
		((NULL == protocol->get_desc) || 
		 	protocol->get_desc(device, type, index, 0, buffer)))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_configuration_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	
	if ((request->value != 0) || (request->index != 0))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = 
					device->config[device->configuration].configuration_value;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_CONFIGURATION_SIZE;
	return VSFERR_NONE;
}

static int16_t vsfusbd_get_config(struct vsfusbd_device_t *device,
									uint8_t value)
{
	uint8_t i;
	
	for (i = 0; i < device->num_of_configuration; i++)
	{
		if (value == device->config[i].configuration_value)
		{
			return i;
		}
	}
	return -1;
}

static vsf_err_t vsfusbd_stdreq_set_configuration_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if ((request->index != 0) || 
		(vsfusbd_get_config(device, request->value) < 0))
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

#if VSFUSBD_CFG_AUTOSETUP
static vsf_err_t vsfusbd_on_OUT(void *p, uint8_t ep);
static vsf_err_t vsfusbd_on_IN(void *p, uint8_t ep);

vsf_err_t vsfusbd_auto_init(struct vsfusbd_device_t *device)
{
	struct vsfusbd_config_t *config;
	struct vsf_buffer_t desc = {NULL, 0};
	enum usb_ep_type_t ep_type;
	uint16_t pos;
	uint8_t attr, feature;
	uint16_t ep_size, ep_addr, ep_index, ep_attr;
	int16_t cur_iface;
	
	config = &device->config[device->configuration];
	
	if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
											USB_DESC_TYPE_DEVICE, 0, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buffer) || (desc.size != USB_DESC_SIZE_DEVICE)
		|| (desc.buffer[0] != desc.size) 
		|| (desc.buffer[1] != USB_DESC_TYPE_DEVICE)
		|| (device->num_of_configuration != 
										desc.buffer[USB_DESC_DEVICE_OFF_CFGNUM])
#endif
		)
	{
		return VSFERR_FAIL;
	}
	
	// config ep0
	ep_size = desc.buffer[USB_DESC_DEVICE_OFF_EP0SIZE];
	if (device->drv->prepare_buffer() || 
		device->drv->ep.set_IN_epsize(0, ep_size) || 
		device->drv->ep.set_OUT_epsize(0, ep_size))
	{
		return VSFERR_FAIL;
	}
	
	// config other eps according to descriptors
	if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
				USB_DESC_TYPE_CONFIGURATION, device->configuration, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buffer) || (desc.size <= USB_DESC_SIZE_CONFIGURATION)
		|| (desc.buffer[0] != USB_DESC_SIZE_CONFIGURATION)
		|| (desc.buffer[1] != USB_DESC_TYPE_CONFIGURATION)
		|| (config->num_of_ifaces != desc.buffer[USB_DESC_CONFIG_OFF_IFNUM])
#endif
		)
	{
		return VSFERR_FAIL;
	}
	
	// initialize device feature according to 
	// bmAttributes field in configuration descriptor
	attr = desc.buffer[USB_DESC_CONFIG_OFF_BMATTR];
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
	num_iface = desc.buffer[USB_DESC_CONFIG_OFF_IFNUM];
	num_endpoint = 0;
#endif
	
	cur_iface = -1;
	pos = USB_DESC_SIZE_CONFIGURATION;
	while (desc.size > pos)
	{
#if __VSF_DEBUG__
		if ((desc.buffer[pos] < 2) || (desc.size < (pos + desc.buffer[pos])))
		{
			return VSFERR_FAIL;
		}
#endif
		switch (desc.buffer[pos + 1])
		{
		case USB_DESC_TYPE_INTERFACE:
#if __VSF_DEBUG__
			if (num_endpoint)
			{
				return VSFERR_FAIL;
			}
			num_endpoint = desc.buffer[pos + 4];
			num_iface--;
#endif
			cur_iface = desc.buffer[pos + 2];
			break;
		case USB_DESC_TYPE_ENDPOINT:
			ep_addr = desc.buffer[pos + USB_DESC_EP_OFF_EPADDR];
			ep_attr = desc.buffer[pos + USB_DESC_EP_OFF_EPATTR];
			ep_size = desc.buffer[pos + USB_DESC_EP_OFF_EPSIZE];
			ep_index = ep_addr & 0x0F;
#if __VSF_DEBUG__
			num_endpoint--;
			if (ep_index > (*device->drv->ep.num_of_ep - 1))
			{
				return VSFERR_FAIL;
			}
#endif
			switch (ep_attr & 0x03)
			{
			case 0x00:
				ep_type = USB_EP_TYPE_CONTROL;
				break;
			case 0x01:
				ep_type = USB_EP_TYPE_ISO;
				break;
			case 0x02:
				ep_type = USB_EP_TYPE_BULK;
				break;
			case 0x03:
				ep_type = USB_EP_TYPE_INTERRUPT;
				break;
			default:
				return VSFERR_FAIL;
			}
			device->drv->ep.set_type(ep_index, ep_type);
			if (ep_addr & 0x80)
			{
				// IN ep
				device->drv->ep.set_IN_epsize(ep_index, ep_size);
				device->drv->ep.set_IN_handler(ep_index, vsfusbd_on_IN);
				config->ep_IN_iface_map[ep_index] = cur_iface;
			}
			else
			{
				// OUT ep
				device->drv->ep.set_OUT_epsize(ep_index, ep_size);
				device->drv->ep.set_OUT_handler(ep_index, vsfusbd_on_OUT);
				config->ep_OUT_iface_map[ep_index] = cur_iface;
			}
			break;
		}
		pos += desc.buffer[pos];
	}
#if __VSF_DEBUG__
	if (num_iface || num_endpoint || (desc.size != pos))
	{
		return VSFERR_FAIL;
	}
#endif
	return VSFERR_NONE;
}
#endif

static vsf_err_t vsfusbd_stdreq_set_configuration_process(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_config_t *config;
	int16_t config_idx;
	uint8_t i;
#if __VSF_DEBUG__
	uint8_t num_iface, num_endpoint;
#endif
	
	config_idx = vsfusbd_get_config(device, request->value);
	if (config_idx < 0)
	{
		return VSFERR_FAIL;
	}
	device->configuration = (uint8_t)config_idx;
	config = &device->config[device->configuration];
	
#if VSFUSBD_CFG_AUTOSETUP
	if (vsfusbd_auto_init(device))
	{
		return VSFERR_FAIL;
	}
#endif
	
	// call user initialization
	if ((config->init != NULL) && config->init(device))
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < config->num_of_ifaces; i++)
	{
		config->iface[i].alternate_setting = 0;
		
		if (((config->iface[i].class_protocol != NULL) && 
				(config->iface[i].class_protocol->init != NULL) && 
				config->iface[i].class_protocol->init(i, device)))
		{
			return VSFERR_FAIL;
		}
	}
	
	device->configured = true;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_get_interface_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	uint8_t iface = request->index;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	
	if ((request->value != 0) || 
		(iface >= device->config[device->configuration].num_of_ifaces))
	{
		return VSFERR_FAIL;
	}
	
	ctrl_handler->ctrl_reply_buffer[0] = config->iface[iface].alternate_setting;
	buffer->buffer = ctrl_handler->ctrl_reply_buffer;
	buffer->size = USB_ALTERNATE_SETTING_SIZE;
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_stdreq_set_interface_prepare(
		struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	uint8_t iface_idx = request->index;
	uint8_t alternate_setting = request->value;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	
	if (iface_idx >= device->config[device->configuration].num_of_ifaces)
	{
		return VSFERR_FAIL;
	}
	
	config->iface[iface_idx].alternate_setting = alternate_setting;
	if (device->callback.on_set_interface != NULL)
	{
		return device->callback.on_set_interface(iface_idx, alternate_setting);
	}
	return VSFERR_NONE;
}

static const struct vsfusbd_setup_filter_t vsfusbd_standard_req_filter[] = 
{
	// USB_REQ_GET_STATUS
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_device_status_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_interface_status_prepare,
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_GET_STATUS,
		vsfusbd_stdreq_get_endpoint_status_prepare,
		NULL
	},
	// USB_REQ_CLEAR_FEATURE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_device_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_interface_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_CLEAR_FEATURE,
		vsfusbd_stdreq_clear_endpoint_feature_prepare,
		NULL
	},
	// USB_REQ_SET_FEATURE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_device_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_interface_feature_prepare,
		NULL
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_ENDPOINT,
		USB_REQ_SET_FEATURE,
		vsfusbd_stdreq_set_endpoint_feature_prepare,
		NULL
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
		NULL
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_GET_DESCRIPTOR,
		vsfusbd_stdreq_get_interface_descriptor_prepare,
		NULL
	},
	// USB_REQ_SET_DESCRIPTOR, not supported
	// USB_REQ_GET_CONFIGURATION
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_DEVICE,
		USB_REQ_GET_CONFIGURATION,
		vsfusbd_stdreq_get_configuration_prepare,
		NULL
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
		NULL
	},
	// USB_REQ_SET_INTERFACE
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_STANDARD | USB_REQ_RECP_INTERFACE,
		USB_REQ_SET_INTERFACE,
		vsfusbd_stdreq_set_interface_prepare,
		NULL
	},
	VSFUSBD_SETUP_NULL
};

static struct vsfusbd_setup_filter_t *vsfusbd_get_request_filter_do(
		struct vsfusbd_device_t *device, struct vsfusbd_setup_filter_t *list)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	while (list->type != VSFUSBD_SETUP_INVALID_TYPE)
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
		struct vsfusbd_device_t *device, int8_t *iface)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	
	if (USB_REQ_GET_TYPE(request->type) == USB_REQ_TYPE_STANDARD)
	{
		*iface = -1;
		return vsfusbd_get_request_filter_do(device, 
				(struct vsfusbd_setup_filter_t *)vsfusbd_standard_req_filter);
	}
	else
	{
		struct vsfusbd_config_t *config = 
										&device->config[device->configuration];
		
		if (USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_INTERFACE)
		{
			*iface = (int8_t)request->index;
		}
		else if (USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_OTHER)
		{
			// find an interface which will claim this request
			struct vsfusbd_setup_filter_t *setup_filter;
			uint8_t i;
			
			for (i = 0; i < config->num_of_ifaces; i++)
			{
				setup_filter = vsfusbd_get_request_filter_do(device, 
								config->iface[i].class_protocol->req_filter);
				if (setup_filter != NULL)
				{
					*iface = i;
					return setup_filter;
				}
			}
			*iface = -1;
			return NULL;
		}
		else if ((USB_REQ_GET_RECP(request->type) == USB_REQ_RECP_DEVICE) && 
				(USB_REQ_GET_TYPE(request->type) == USB_REQ_TYPE_CLASS))
		{
			*iface = (int8_t)device->device_class_iface;
		}
		else
		{
			*iface = -1;
			return NULL;
		}
		
		if ((*iface >= config->num_of_ifaces) || 
			(NULL == config->iface[*iface].class_protocol))
		{
			*iface = -1;
			return NULL;
		}
		return vsfusbd_get_request_filter_do(device, 
							config->iface[*iface].class_protocol->req_filter);
	}
}

static void vsfusbd_setup_end_callback(void *param)
{
	struct vsfusbd_device_t *device = param;
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	struct vsf_transaction_buffer_t *tbuffer;
	
	tbuffer = (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD) ?
		&vsfusbd_OUT_transact[0].tbuffer : &vsfusbd_IN_transact[0].tbuffer;
	if (ctrl_handler->filter->process != NULL)
	{
		ctrl_handler->filter->process(device, &tbuffer->buffer);
	}
}

static void vsfusbd_setup_status_callback(void *param)
{
	struct vsfusbd_device_t *device = param;
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	
	if (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD)
	{
		vsfusbd_IN_transact[0].tbuffer.buffer.buffer = NULL;
		vsfusbd_IN_transact[0].tbuffer.buffer.size = 0;
		vsfusbd_IN_transact[0].pkt.in.zlp = true;
		vsfusbd_IN_transact[0].callback.param = device;
		vsfusbd_IN_transact[0].callback.callback = vsfusbd_setup_end_callback;
		vsfusbd_ep_send_nb(device, 0);
	}
}

vsf_err_t vsfusbd_on_SETUP(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_ctrl_handler_t *ctrl_handler = &device->ctrl_handler;
	struct vsfusbd_ctrl_request_t *request = &ctrl_handler->request;
	struct vsf_buffer_t buffer;
	uint8_t buff[USB_SETUP_PKG_SIZE];
	vsf_err_t err = VSFERR_NONE;
	
	if (device->drv->get_setup(buff))
	{
		err = VSFERR_FAIL;
		goto exit;
	}
	memcpy(request, buff, USB_SETUP_PKG_SIZE);
	ctrl_handler->state		= USB_CTRL_STAT_SETTING_UP;
	ctrl_handler->filter	= vsfusbd_get_request_filter(device, 
														&ctrl_handler->iface);
	buffer.size				= 0;
	buffer.buffer			= NULL;
	
	if ((NULL == ctrl_handler->filter) || 
		((ctrl_handler->filter->prepare != NULL) && 
			ctrl_handler->filter->prepare(device, &buffer)) || 
		(buffer.size && (NULL == buffer.buffer)))
	{
		err = VSFERR_FAIL;
		goto exit;
	}
	if (buffer.size > request->length)
	{
		buffer.size = request->length;
	}
	
	if (0 == request->length)
	{
		vsfusbd_IN_transact[0].tbuffer.buffer.buffer = NULL;
		vsfusbd_IN_transact[0].tbuffer.buffer.size = 0;
		vsfusbd_IN_transact[0].pkt.in.zlp = true;
		vsfusbd_IN_transact[0].callback.param = device;
		vsfusbd_IN_transact[0].callback.callback = vsfusbd_setup_end_callback;
		vsfusbd_ep_send_nb(device, 0);
	}
	else
	{
		if (USB_REQ_GET_DIR(request->type) == USB_REQ_DIR_HTOD)
		{
			vsfusbd_OUT_transact[0].tbuffer.buffer = buffer;
			vsfusbd_OUT_transact[0].callback.param = device;
			vsfusbd_OUT_transact[0].callback.callback =
												vsfusbd_setup_status_callback;
			err = vsfusbd_ep_receive_nb(device, 0);
		}
		else
		{
			vsfusbd_IN_transact[0].tbuffer.buffer = buffer;
			vsfusbd_IN_transact[0].pkt.in.zlp = buffer.size < request->length;
			vsfusbd_IN_transact[0].callback.param = device;
			vsfusbd_IN_transact[0].callback.callback =
												vsfusbd_setup_status_callback;
			err = vsfusbd_ep_send_nb(device, 0);
			if (!err)
			{
				vsfusbd_OUT_transact[0].callback.param = device;
				vsfusbd_OUT_transact[0].callback.callback = vsfusbd_setup_end_callback;
				err = vsfusbd_ep_receive_nb(device, 0);
			}
		}
	}
	
exit:
	if (err)
	{
		device->drv->ep.set_IN_stall(0);
		device->drv->ep.set_OUT_stall(0);
	}
	return err;
}

static vsf_err_t vsfusbd_on_IN(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsf_transaction_buffer_t *tbuffer = &vsfusbd_IN_transact[ep].tbuffer;
	uint8_t *buffer_ptr = &tbuffer->buffer.buffer[tbuffer->position];
	uint32_t remain_size;
	uint16_t pkg_size;
	
	if (--vsfusbd_IN_transact[ep].pkt.in.num > 0)
	{
#if VSFUSBD_CFG_DBUFFER_EN
		uint32_t pkg_thres;
		
		if (device->drv->ep.is_IN_dbuffer(ep))
		{
			device->drv->ep.switch_IN_buffer(ep);
			pkg_thres = 1;
		}
		else
		{
			pkg_thres = 0;
		}
		
		if (vsfusbd_IN_transact[ep].pkt.in.num > pkg_thres)
#else
		if (vsfusbd_IN_transact[ep].pkt.in.num > 0)
#endif
		{
			remain_size = tbuffer->buffer.size - tbuffer->position;
			
			if (remain_size)
			{
				pkg_size = device->drv->ep.get_IN_epsize(ep);
				pkg_size = (remain_size > pkg_size) ? pkg_size : remain_size;
				device->drv->ep.write_IN_buffer(ep, buffer_ptr, pkg_size);
				device->drv->ep.set_IN_count(ep, pkg_size);
				tbuffer->position += pkg_size;
			}
			else
			{
				device->drv->ep.set_IN_count(ep, 0);
			}
		}
	}
	else
	{
		struct vsfusbd_transact_callback_t *callback =
											&vsfusbd_IN_transact[ep].callback;
		
		if (callback->callback != NULL)
		{
			callback->callback(callback->param);
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t vsfusbd_on_OUT(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsf_transaction_buffer_t *tbuffer =
											&vsfusbd_OUT_transact[ep].tbuffer;
	uint16_t pkg_size;
	
#if VSFUSBD_CFG_DBUFFER_EN
	if (device->drv->ep.is_OUT_dbuffer(ep))
	{
		device->drv->ep.switch_OUT_buffer(ep);
	}
#endif
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if ((tbuffer->position < tbuffer->buffer.size) && 
		((tbuffer->position + pkg_size) <= tbuffer->buffer.size))
	{
		device->drv->ep.read_OUT_buffer(ep, 
						&tbuffer->buffer.buffer[tbuffer->position], pkg_size);
		device->drv->ep.enable_OUT(ep);
		tbuffer->position += pkg_size;
		if (pkg_size < device->drv->ep.get_OUT_epsize(ep))
		{
			vsfusbd_OUT_transact[ep].pkt.out.isshort = true;
		}
		
		if (vsfusbd_OUT_transact[ep].pkt.out.isshort ||
			(tbuffer->position >= tbuffer->buffer.size))
		{
			struct vsfusbd_transact_callback_t *callback =
											&vsfusbd_OUT_transact[ep].callback;
		
			if (callback->callback != NULL)
			{
				callback->callback(callback->param);
			}
			return VSFERR_NONE;
		}
		return VSFERR_NONE;
	}
	return VSFERR_FAIL;
}

vsf_err_t vsfusbd_on_UNDERFLOW(void *p, uint8_t ep)
{
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_OVERFLOW(void *p, uint8_t ep)
{
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_RESET(void *p)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config;
	uint8_t i;
#if VSFUSBD_CFG_AUTOSETUP
	struct vsf_buffer_t desc = {NULL, 0};
	uint16_t ep_size;
#endif
	
	memset(vsfusbd_IN_transact, 0, sizeof(vsfusbd_IN_transact));
	memset(vsfusbd_OUT_transact, 0, sizeof(vsfusbd_OUT_transact));
	
	device->configured = false;
	device->configuration = 0;
	device->feature = 0;
	device->ctrl_handler.state = USB_CTRL_STAT_WAIT_SETUP;
	
	for (i = 0; i < device->num_of_configuration; i++)
	{
		config = &device->config[i];
		memset(config->ep_OUT_iface_map, -1, sizeof(config->ep_OUT_iface_map));
		memset(config->ep_IN_iface_map, -1, sizeof(config->ep_OUT_iface_map));
	}
	
	// reset usb hw
	if (device->drv->reset() || device->drv->init(device) || 
		device->drv->connect())
	{
		return VSFERR_FAIL;
	}
	
#if VSFUSBD_CFG_AUTOSETUP
	if (vsfusbd_device_get_descriptor(device, device->desc_filter, 
											USB_DESC_TYPE_DEVICE, 0, 0, &desc)
#if __VSF_DEBUG__
		|| (NULL == desc.buffer) || (desc.size != USB_DESC_SIZE_DEVICE)
		|| (desc.buffer[0] != desc.size) 
		|| (desc.buffer[1] != USB_DESC_TYPE_DEVICE)
		|| (device->num_of_configuration != 
										desc.buffer[USB_DESC_DEVICE_OFF_CFGNUM])
#endif
		)
	{
		return VSFERR_FAIL;
	}
	ep_size = desc.buffer[USB_DESC_DEVICE_OFF_EP0SIZE];
	device->ctrl_handler.ep_size = ep_size;
	
	// config ep0
	if (device->drv->prepare_buffer() || 
		device->drv->ep.set_type(0, USB_EP_TYPE_CONTROL) || 
		device->drv->ep.set_IN_epsize(0, ep_size) || 
		device->drv->ep.set_OUT_epsize(0, ep_size))
	{
		return VSFERR_FAIL;
	}
#endif	// VSFUSBD_CFG_AUTOSETUP
	
	if (device->callback.on_RESET != NULL)
	{
		device->callback.on_RESET();
	}
	
	if (device->drv->ep.set_IN_handler(0, vsfusbd_on_IN) || 
		device->drv->ep.set_OUT_handler(0, vsfusbd_on_OUT))
	{
		return VSFERR_FAIL;
	}
	return device->drv->set_address(0);
}

vsf_err_t vsfusbd_on_WAKEUP(void *p)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_WAKEUP != NULL)
	{
		device->callback.on_WAKEUP();
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_SUSPEND(void *p)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_SUSPEND != NULL)
	{
		device->callback.on_SUSPEND();
	}
	return device->drv->suspend();
}

vsf_err_t vsfusbd_on_RESUME(void *p)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_RESUME != NULL)
	{
		device->callback.on_RESUME();
	}
	return device->drv->resume();
}

vsf_err_t vsfusbd_on_SOF(void *p)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_SOF != NULL)
	{
		device->callback.on_SOF();
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_ATTACH(void *p)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_ATTACH != NULL)
	{
		device->callback.on_ATTACH();
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_DETACH(void *p)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_DETACH != NULL)
	{
		device->callback.on_DETACH();
	}
	return VSFERR_NONE;
}

vsf_err_t vsfusbd_on_ERROR(void *p, enum usb_err_type_t type)
{
	struct vsfusbd_device_t *device = p;
	
	if (device->callback.on_ERROR != NULL)
	{
		device->callback.on_ERROR(type);
	}
	return VSFERR_NONE;
}


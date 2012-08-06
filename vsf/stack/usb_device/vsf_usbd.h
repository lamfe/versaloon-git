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

#ifndef __VSF_USBD_H_INCLUDED__
#define __VSF_USBD_H_INCLUDED__

#include "tool/buffer/buffer.h"

struct vsfusbd_device_t;

enum vsfusbd_ctrl_state_t
{
	USB_CTRL_STAT_WAIT_SETUP,
	USB_CTRL_STAT_SETTING_UP,
	USB_CTRL_STAT_IN_DATA,
	USB_CTRL_STAT_OUT_DATA,
	USB_CTRL_STAT_LAST_IN_DATA,
	USB_CTRL_STAT_LAST_OUT_DATA,
	USB_CTRL_STAT_WAIT_STATUS_IN,
	USB_CTRL_STAT_WAIT_STATUS_OUT,
	USB_CTRL_STAT_STALLED,
	USB_CTRL_STAT_PAUSE,
};

struct vsfusbd_ctrl_request_t
{
	uint8_t type;
	uint8_t request;
	uint16_t value;
	uint16_t index;
	uint16_t length;
};

struct vsfusbd_ctrl_handler_t
{
	enum vsfusbd_ctrl_state_t state;
	uint16_t ep_size;
	struct vsfusbd_ctrl_request_t request;
	struct vsf_transaction_buffer_t tbuffer;
	struct vsfusbd_setup_filter_t *filter;
	int8_t iface;
	uint8_t ctrl_reply_buffer[2];
};

#define VSFUSBD_DESC_DEVICE(lanid, ptr, size, func)			\
	{USB_DESC_TYPE_DEVICE, 0, (lanid), {(uint8_t*)(ptr), (size)}, (func)}
#define VSFUSBD_DESC_CONFIG(lanid, idx, ptr, size, func)	\
	{USB_DESC_TYPE_CONFIGURATION, (idx), (lanid), {(uint8_t*)(ptr), (size)}, (func)}
#define VSFUSBD_DESC_STRING(lanid, idx, ptr, size, func)	\
	{USB_DESC_TYPE_STRING, (idx), (lanid), {(uint8_t*)(ptr), (size)}, (func)}
#define VSFUSBD_DESC_NULL									\
	{0, 0, 0, {NULL, 0}, NULL}
struct vsfusbd_desc_filter_t
{
	uint8_t type;
	uint8_t index;
	uint16_t lanid;
	
	struct vsf_buffer_t buffer;
	vsf_err_t (*read)(struct vsfusbd_device_t *device, 
						struct vsf_buffer_t *buffer);
};

#define VSFUSBD_SETUP_INVALID_TYPE	0xFF
#define VSFUSBD_SETUP_NULL			{VSFUSBD_SETUP_INVALID_TYPE, 0, NULL, NULL}

struct vsfusbd_setup_filter_t
{
	uint8_t type;
	uint8_t request;
	
	vsf_err_t (*prepare)(struct vsfusbd_device_t *device, 
							struct vsf_buffer_t *buffer);
	vsf_err_t (*process)(struct vsfusbd_device_t *device, 
							struct vsf_buffer_t *buffer);
};

struct vsfusbd_class_protocol_t
{
	vsf_err_t (*get_desc)(struct vsfusbd_device_t *device, uint8_t type, 
				uint8_t index, uint16_t lanid, struct vsf_buffer_t *buffer);
	struct vsfusbd_desc_filter_t *desc_filter;
	struct vsfusbd_setup_filter_t *req_filter;
	
	vsf_err_t (*init)(uint8_t iface, struct vsfusbd_device_t *device);
	vsf_err_t (*fini)(uint8_t iface, struct vsfusbd_device_t *device);
	vsf_err_t (*poll)(uint8_t iface, struct vsfusbd_device_t *device);
};

struct vsfusbd_iface_t
{
	// public
	struct vsfusbd_class_protocol_t *class_protocol;
	void *protocol_param;
	
	// private
	uint8_t alternate_setting;
};

struct vsfusbd_config_t
{
	// public
	vsf_err_t (*init)(struct vsfusbd_device_t *device);
	vsf_err_t (*fini)(struct vsfusbd_device_t *device);
	
	uint8_t num_of_ifaces;
	struct vsfusbd_iface_t *iface;
	
	// private
	uint8_t configuration_value;
	int8_t ep_OUT_iface_map[16];
	int8_t ep_IN_iface_map[16];
};

struct vsfusbd_device_t
{
	// public
	uint8_t num_of_configuration;
	struct vsfusbd_config_t *config;
	struct vsfusbd_desc_filter_t *desc_filter;
	uint8_t device_class_iface;
	struct interface_usbd_t *drv;
	
	const struct vsfusbd_user_callback_t
	{
		vsf_err_t (*init)(void);
		vsf_err_t (*fini)(void);
		vsf_err_t (*poll)(void);
		vsf_err_t (*on_set_interface)(uint8_t iface,
										uint8_t alternate_setting);
		
		void (*on_ATTACH)(void);
		void (*on_DETACH)(void);
		void (*on_RESET)(void);
		void (*on_ERROR)(enum usb_err_type_t type);
		void (*on_WAKEUP)(void);
		void (*on_SUSPEND)(void);
		void (*on_RESUME)(void);
		void (*on_SOF)(void);
		
		void (*on_IN)(uint8_t ep);
		void (*on_OUT)(uint8_t ep);
#if VSFUSBD_EP_ISO_EN
		void (*on_SYNC_UNDERFLOW)(uint8_t ep);
		void (*on_SYNC_OVERFLOW)(uint8_t ep);
#endif
	} callback;
	
	// private
	uint8_t address;
	bool configured;
	uint8_t configuration;
	uint8_t feature;
	struct vsfusbd_ctrl_handler_t ctrl_handler;
};

vsf_err_t vsfusbd_device_get_descriptor(struct vsfusbd_device_t *device, 
		struct vsfusbd_desc_filter_t *filter, uint8_t type, uint8_t index, 
		uint16_t lanid, struct vsf_buffer_t *buffer);

vsf_err_t vsfusbd_device_init(struct vsfusbd_device_t *device);
vsf_err_t vsfusbd_auto_init(struct vsfusbd_device_t *device);
vsf_err_t vsfusbd_device_fini(struct vsfusbd_device_t *device);
vsf_err_t vsfusbd_device_poll(struct vsfusbd_device_t *device);

vsf_err_t vsfusbd_ep_in_nb(struct vsfusbd_device_t *device, uint8_t ep, 
							struct vsf_buffer_t *buffer);
vsf_err_t vsfusbd_ep_in_nb_isready(struct vsfusbd_device_t *device,
									uint8_t ep);
vsf_err_t vsfusbd_ep_in(struct vsfusbd_device_t *device, uint8_t ep, 
						struct vsf_buffer_t *buffer);

vsf_err_t vsfusbd_ep_out_nb(struct vsfusbd_device_t *device, uint8_t ep, 
							struct vsf_buffer_t *buffer);
vsf_err_t vsfusbd_ep_out_nb_isready(struct vsfusbd_device_t *device,
									uint8_t ep);
vsf_err_t vsfusbd_ep_out(struct vsfusbd_device_t *device, uint8_t ep, 
						struct vsf_buffer_t *buffer);

#endif	// __VSF_USBD_H_INCLUDED__


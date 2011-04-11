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

#include "list.h"

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
	uint8_t request_type;
	uint8_t request;
	uint16_t value;
	uint16_t index;
	uint16_t length;
};

struct vsfusbd_ctrl_handler_t
{
	enum vsfusbd_ctrl_state_t state;
	struct vsfusbd_ctrl_request_t request;
};

struct vsfusbd_ctrl_request_filter_t
{
	enum filter_mask_t
	{
		FLTMSK_REQUEST_TYPE = (1 << 0),
		FLTMSK_REQUEST 		= (1 << 1),
		FLTMSK_VALUE 		= (1 << 2),
		FLTMSK_INDEX 		= (1 << 3),
		FLTMSK_LENGTH 		= (1 << 4),
	} filter_mask;
	
	struct vsfusbd_ctrl_request_t request_filter;
	
	struct vsf_buffer_t buffer;
	RESULT (*filter_func)(struct vsfusbd_ctrl_request_t ctrl_request, 
							struct vsf_buffer_t *buffer);
	
#if VSFUSBD_DYNAMIC_SETUP_FILTER_EN
	struct sllist list;
#endif
};

struct vsfusbd_t
{
	RESULT (*init)(void);
	RESULT (*fini)(void);
	RESULT (*poll)(void);
	
	uint8_t configuration;
	uint8_t interface;
	uint8_t alternate_setting;
	uint8_t feature;
	
	struct vsfusbd_ctrl_handler_t ctrl_handler;
	struct vsfusbd_ctrl_request_filter_t *filter;
#if VSFUSBD_DYNAMIC_SETUP_FILTER_EN
	RESULT (*add_setup_filter)(struct vsfusbd_ctrl_request_filter_t filter);
	RESULT (*remove_setup_filter)(struct vsfusbd_ctrl_request_filter_t filter);
#endif
	
	RESULT (*in_nb)(uint8_t ep, uint8_t *buff, uint16_t size);
	RESULT (*in_nb_isready)(void);
	RESULT (*in)(uint8_t ep, uint8_t *buff, uint16_t size);
	
	RESULT (*out_nb)(uint8_t ep, uint8_t *buff, uint16_t size);
	RESULT (*out_nb_isready)(void);
	RESULT (*out)(uint8_t ep, uint8_t *buff, uint16_t size);
	
	struct vsfusbd_drv_t drv;
};

#endif	// __VSF_USBD_H_INCLUDED__


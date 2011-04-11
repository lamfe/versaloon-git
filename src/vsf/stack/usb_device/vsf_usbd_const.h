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

#ifndef __VSF_USBD_CONST_H_INCLUDED__
#define __VSF_USBD_CONST_H_INCLUDED__

// description type
enum usb_description_type_t
{
	USB_DESC_TYPE_DEVICE			= 0x01,
	USB_DESC_TYPE_CONFIGURATION		= 0x02,
	USB_DESC_TYPE_STRING			= 0x03,
	USB_DESC_TYPE_INTERFACE			= 0x04,
	USB_DESC_TYPE_ENDPOINT			= 0x05,
	USB_DESC_TYPE_IAD				= 0X0B,
};

// request
enum usb_request_t
{
	// standard request
	USB_REQ_GET_STATUS				= 0x00,
	USB_REQ_CLEAR_FEATURE			= 0x01,
	USB_REQ_SET_FEATURE				= 0x02,
	USB_REQ_SET_ADDRESS				= 0x03,
	USB_REQ_GET_DESCRIPTOR			= 0x04,
	USB_REQ_SET_DESCRIPTOR			= 0x05,
	USB_REQ_GET_CONFIGURATION		= 0x06,
	USB_REQ_SET_CONFIGURATION		= 0x07,
	USB_REQ_GET_INTERFACE			= 0x08,
	USB_REQ_SET_INTERFACE			= 0x09,
	// CDC request
	
};

enum usb_request_type_t
{
	USB_REQ_TYPE_STANDARD			= 0x00,
	USB_REQ_TYPE_CLASS				= 0x20,
	USB_REQ_TYPE_VENDOR				= 0x40,
};
#define USB_REQ_TYPE_MASK			0x60
#define USB_REQ_GET_TYPE(req)		(enum usb_request_type_t)((req) & USB_REQ_TYPE_MASK)

enum usb_request_dir_t
{
	USB_REQ_DIR_HTOD				= 0x00,
	USB_REQ_DIR_DTOH				= 0x80,
};
#define USB_REQ_DIR_MASK			0x80
#define USB_REQ_GET_DIR(req)		(enum usb_request_dir_t)((req) & USB_REQ_DIR_MASK)

enum usb_request_recipent_t
{
	USB_REQ_RECP_DEVICE				= 0x00,
	USB_REQ_RECP_INTERFACE			= 0x01,
	USB_REQ_RECP_ENDPOINT			= 0x02,
};
#define USB_REQ_RECP_MASK			0x03
#define USB_REQ_GET_RECP(req)		(enum usb_request_recipent_t)((req) & USB_REQ_DIR_MASK)

// feature
enum usb_feature_t
{
	USB_FEATURE_STALL				= 0x01,
	USB_FEATURE_REMOTE_WAKEUP		= 0x02,
};

// error
enum usb_err_type_t
{
	USBERR_OK,
	USBERR_INVALID_CRC,
	USBERR_SOF_TO,
};

#endif	// __VSF_USBD_CONST_H_INCLUDED__


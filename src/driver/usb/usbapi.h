/***************************************************************************
 *   Copyright (C) 2009 by Simon Qian <SimonQian@SimonQian.com>            *
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
#ifndef __USBAPI_H_INCLUDED__
#define __USBAPI_H_INCLUDED__

#include "usb.h"

uint32_t print_usb_devices(uint16_t VID, uint16_t PID, uint8_t serialindex, 
							char *serialstring, uint8_t productindex, 
							char *productstring);
usb_dev_handle* find_usb_device(uint16_t VID, uint16_t PID, uint8_t interface, 
								uint8_t serialindex, char *serialstring, 
								uint8_t productindex, char *productstring);

#endif /* __USBAPI_H_INCLUDED__ */


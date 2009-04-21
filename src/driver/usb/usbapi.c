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
#include <stdio.h>

#include "app_type.h"
#include "app_log.h"

#include "usbapi.h"

usb_dev_handle* find_usb_device(uint16 VID, uint16 PID)
{
	usb_dev_handle *usb = NULL;
	struct usb_bus *busses;
	struct usb_bus *bus;
	struct usb_device *dev;
	int config_value;

	usb_init();
	usb_find_busses();
	usb_find_devices();
	busses = usb_get_busses();

	for (bus = busses; bus; bus = bus->next)
	{
		for (dev = bus->devices; dev; dev = dev->next)
		{
			if ((dev->descriptor.idVendor == VID) 
				&& (dev->descriptor.idProduct == PID))
			{
				usb = usb_open(dev);
				if (NULL == usb)
				{
					LOG_ERROR("failed to open %04X:%04X, %s\n", 
							  VID, PID, usb_strerror());
					return NULL;
				}
				
				/* usb_set_configuration required under win32 */
				config_value = dev->config[0].bConfigurationValue;
				if (usb_set_configuration(usb, config_value) != 0)
				{
					LOG_ERROR("fail to set configuration, %s\n", 
							  usb_strerror());
					return NULL;
				}
				if (usb_claim_interface(usb, 0) != 0)
				{
					LOG_ERROR("fail to claim interface, %s\n", usb_strerror());
					return NULL;
				}
				
				return usb;
			}
		}
	}
	
	return usb;
}


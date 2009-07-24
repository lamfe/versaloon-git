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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "usbapi.h"

uint32 print_usb_devices(uint16 VID, uint16 PID, uint8 stringindex, 
					   char *serialstring)
{
	usb_dev_handle *usb = NULL;
	struct usb_bus *busses;
	struct usb_bus *bus;
	struct usb_device *dev;
	int config_value, i, c = 0;
	uint8 buf[256];

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
					LOG_ERROR(_GETTEXT("failed to open %04X:%04X, %s\n"), 
							  VID, PID, usb_strerror());
					continue;
				}
				
				// check serialstring
				config_value = usb_get_string_simple(usb, stringindex, 
													 (char *)buf, 256);
				if ((config_value < 0) 
					|| (config_value != (int)strlen(serialstring)) - 1)
				{
					usb_close(usb);
					usb = NULL;
					continue;
				}
				if (serialstring != NULL)
				{
					for (i = 0; i < (config_value - 1); i++)
					{
						if (buf[i] != serialstring[i])
						{
							usb_close(usb);
							usb = NULL;
							break;
						}
					}
					if (i != (config_value - 1))
					{
						continue;
					}
				}
				
				if (usb != NULL)
				{
					// print current device
					if (config_value > 0)
					{
						LOG_INFO(
							_GETTEXT("%d: 0x%04X:0x%04X:%s on %s.\n"), 
							c, VID, PID, buf, dev->filename);
					}
					else
					{
						LOG_INFO(
							_GETTEXT("%d: 0x%04X:0x%04X on %s.\n"), 
							c, VID, PID, dev->filename);
					}
					c++;
					
					usb_close(usb);
					usb = NULL;
				}
			}
		}
	}
	
	if (usb != NULL)
	{
		usb_close(usb);
		usb = NULL;
	}
	
	return c;
}

usb_dev_handle* find_usb_device(uint16 VID, uint16 PID, uint8 interface, 
								uint8 stringindex, char *serialstring)
{
	usb_dev_handle *usb = NULL;
	struct usb_bus *busses;
	struct usb_bus *bus;
	struct usb_device *dev;
	int config_value, i;

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
					LOG_ERROR(_GETTEXT("failed to open %04X:%04X, %s\n"), 
							  VID, PID, usb_strerror());
					continue;
				}
				
				// check serialstring
				if (serialstring != NULL)
				{
					uint8 buf[256];
					
					config_value = usb_get_string_simple(usb, stringindex, 
														 (char *)buf, 256);
					if ((config_value < 0) 
						|| (config_value != (int)strlen(serialstring)) - 1)
					{
						usb_close(usb);
						usb = NULL;
						continue;
					}
					
					for (i = 0; i < (config_value - 1); i++)
					{
						if (buf[i] != serialstring[i])
						{
							usb_close(usb);
							usb = NULL;
							break;
						}
					}
					if (i != (config_value - 1))
					{
						continue;
					}
				}
				
				// usb_set_configuration required under win32
				config_value = dev->config[0].bConfigurationValue;
				if (usb_set_configuration(usb, config_value) != 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_MESSAGE), 
							  "set configuration", 
							  usb_strerror());
					usb_close(usb);
					usb = NULL;
					continue;
				}
				if (usb_claim_interface(usb, interface) != 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_MESSAGE), 
							  "claim interface", 
							  usb_strerror());
					usb_close(usb);
					usb = NULL;
					continue;
				}
				
				if (usb != NULL)
				{
					return usb;
				}
			}
		}
	}
	
	return usb;
}


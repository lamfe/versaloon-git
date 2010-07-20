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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "usbapi.h"

static uint8_t usb_check_string(usb_dev_handle *usb, uint8_t stringidx, 
								char * string, char * buff, uint16_t buf_size)
{
	int len;
	uint8_t alloced = 0;
	uint8_t ret = 1;
	
	if (NULL == buff)
	{
		buf_size = 256;
		buff = (char*)malloc(buf_size);
		if (NULL == buff)
		{
			ret = 0;
			goto free_and_return;
		}
		alloced = 1;
	}
	
	strcpy(buff, "");
	len = usb_get_string_simple(usb, stringidx, (char *)buff, buf_size);
	if ((len < 0) || (len != ((int)strlen((const char *)buff))))
	{
		ret = 0;
		goto free_and_return;
	}
	
	buff[len] = '\0';
	if ((string != NULL) && strcmp((const char *)buff, string))
	{
		ret = 0;
		goto free_and_return;
	}
	
free_and_return:
	if (alloced && (buff != NULL))
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

uint32_t print_usb_devices(uint16_t VID, uint16_t PID, uint8_t serialindex, 
							char *serialstring, uint8_t productindex, 
							char *productstring)
{
	usb_dev_handle *usb = NULL;
	struct usb_bus *busses;
	struct usb_bus *bus;
	struct usb_device *dev;
	int c = 0;
	uint8_t buf[256];

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
					LOG_ERROR("failed to open %04X:%04X, %s", VID, PID, 
								usb_strerror());
					continue;
				}
				
				// check description string
				if (((productstring != NULL) 
						&& !usb_check_string(usb, productindex, productstring, NULL, 0))
					|| !usb_check_string(usb, serialindex, serialstring, (char*)buf, sizeof(buf)))
				{
					usb_close(usb);
					usb = NULL;
					continue;
				}
				
				if (usb != NULL)
				{
					// print current device
					if (strlen((char *)buf) > 0)
					{
						printf("%s%d: 0x%04X:0x%04X:%s on %s.", 
								productstring, c, VID, PID, buf, dev->filename);
					}
					else
					{
						printf("%s%d: 0x%04X:0x%04X on %s.", 
								productstring, c, VID, PID, dev->filename);
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

usb_dev_handle* find_usb_device(uint16_t VID, uint16_t PID, uint8_t interface, 
								uint8_t serialindex, char *serialstring, 
								uint8_t productindex, char *productstring)
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
					LOG_ERROR("failed to open %04X:%04X, %s", VID, PID, 
								usb_strerror());
					continue;
				}
				
				// check description string
				if (((productstring != NULL) 
						&& !usb_check_string(usb, productindex, productstring, 
												NULL, 0))
					|| ((serialstring != NULL) 
						&& !usb_check_string(usb, serialindex, serialstring, 
												NULL, 0)))
				{
					usb_close(usb);
					usb = NULL;
					continue;
				}
				
				// usb_set_configuration required under win32
				config_value = dev->config[0].bConfigurationValue;
				if (usb_set_configuration(usb, config_value) != 0)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION_MESSAGE, 
								"set configuration", usb_strerror());
					usb_close(usb);
					usb = NULL;
					continue;
				}
				if (usb_claim_interface(usb, interface) != 0)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION_MESSAGE, 
								"claim interface", usb_strerror());
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


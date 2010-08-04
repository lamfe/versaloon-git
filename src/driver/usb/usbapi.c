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

#include "scripts.h"
#include "strparser.h"

#include "usbapi.h"

static struct usbapi_param_t usb_param;

MISC_HANDLER(usbapi_param);
struct misc_cmd_t usbapi_cmd[] = 
{
	MISC_CMD(	"usb",
				"set usb device, format: "
				"usb/U [VID_PID_EPIN_EPOUT_INTERFACE_]SERIALSTRING",
				usbapi_param),
	MISC_CMD(	"U",
				"set usb device, format: "
				"usb/U [VID_PID_EPIN_EPOUT_INTERFACE_]SERIALSTRING",
				usbapi_param),
	MISC_CMD_END
};

MISC_HANDLER(usbapi_param)
{
	// vid: 2 bytes
	// pid: 2 bytes
	// epin: 1 byte
	// epout: 1 byte
	// interface: 1 byte
	// serialstring: 256 bytes
	uint8_t usb_setting[256 + 7], *ptr;
	RESULT success;
	uint8_t i;
	char* formats[] = 
	{
		// VID_PID_EPIN_EPOUT_INTERFACE_SERIALSTRING
		// vid(2d):pid(2d):epin(1d):epout(1d):interface(1d):serialstring(s)
		"%2d%2d%1d%1d%1d%s",
		// serialstring(s)
		"%s"
	};
	
	MISC_CHECK_ARGC(2);
	
	success = ERROR_FAIL;
	for (i = 0; i < dimof(formats); i++)
	{
		memset(usb_setting, 0, sizeof(usb_setting));
		success = strparser_parse((char*)argv[1], formats[i], 
									usb_setting, sizeof(usb_setting));
		if (ERROR_OK == success)
		{
			break;
		}
	}
	
	if (success != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		misc_print_help(argv[0]);
		return ERRCODE_INVALID_OPTION;
	}
	
	ptr = usb_setting;
	if (1 == i)
	{
		strncpy(usb_param.serialstring, (char*)&ptr[0], 
					sizeof(usb_param.serialstring));
	}
	else if (0 == i)
	{
		usb_param.valid = 1;
		usb_param.vid = ptr[0] + (ptr[1] << 8);
		usb_param.pid = ptr[2] + (ptr[3] << 8);
		usb_param.epin = ptr[4];
		usb_param.epout = ptr[5];
		usb_param.interface = ptr[6];
		strncpy(usb_param.serialstring, (char*)&ptr[7], 
					sizeof(usb_param.serialstring));
	}
	
	if (strlen(usb_param.serialstring) > 0)
	{
		LOG_DEBUG("usb_device is on 0x%04X:0x%04X(0x%02x_0x%02X):%s.", 
			usb_param.valid, usb_param.pid, usb_param.epin, usb_param.epout, 
			usb_param.serialstring);
	}
	else
	{
		LOG_DEBUG("usb_device is on 0x%04X:0x%04X(0x%02x_0x%02X).", 
			usb_param.valid, usb_param.pid, usb_param.epin, usb_param.epout);
	}
	return ERROR_OK;
}

// usb_set_param will not ser serialstring
// because serialstring can be different for same usb_device type
// to set serialstring, please use --usb or -U
void usb_set_param(uint16_t vid, uint16_t pid, uint8_t epin, uint8_t epout, 
					uint8_t interface)
{
	usb_param.vid = vid;
	usb_param.pid = pid;
	usb_param.epin = epin;
	usb_param.epout = epout;
	usb_param.interface = interface;
	usb_param.valid = 1;
}

uint8_t usb_param_valid(void)
{
	return usb_param.valid;
}

uint16_t usb_param_vid(void)
{
	return usb_param.vid;
}

uint16_t usb_param_pid(void)
{
	return usb_param.pid;
}

uint8_t usb_param_epin(void)
{
	return usb_param.epin;
}

uint8_t usb_param_epout(void)
{
	return usb_param.epout;
}

uint8_t usb_param_interface(void)
{
	return usb_param.interface;
}

char *usb_param_serial(void)
{
	if (strlen(usb_param.serialstring) > 0)
	{
		return usb_param.serialstring;
	}
	else
	{
		return NULL;
	}
}

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
	usb_dev_handle *dev_handle = NULL;
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
				dev_handle = usb_open(dev);
				if (NULL == dev_handle)
				{
					LOG_ERROR("failed to open %04X:%04X, %s", VID, PID, 
								usb_strerror());
					continue;
				}
				
				// check description string
				if (((productstring != NULL) 
						&& !usb_check_string(dev_handle, productindex, 
												productstring, NULL, 0))
					|| !usb_check_string(dev_handle, serialindex, serialstring, 
												(char*)buf, sizeof(buf)))
				{
					usb_close(dev_handle);
					dev_handle = NULL;
					continue;
				}
				
				if (dev_handle != NULL)
				{
					// print current device
					if (strlen((char *)buf) > 0)
					{
						printf("%s%d: 0x%04X:0x%04X:%s on %s.\n", 
								productstring, c, VID, PID, buf, dev->filename);
					}
					else
					{
						printf("%s%d: 0x%04X:0x%04X on %s.\n", 
								productstring, c, VID, PID, dev->filename);
					}
					c++;
					
					usb_close(dev_handle);
					dev_handle = NULL;
				}
			}
		}
	}
	
	if (dev_handle != NULL)
	{
		usb_close(dev_handle);
		dev_handle = NULL;
	}
	
	return c;
}

usb_dev_handle* find_usb_device(uint16_t VID, uint16_t PID, uint8_t interface, 
								uint8_t serialindex, char *serialstring, 
								uint8_t productindex, char *productstring)
{
	usb_dev_handle *dev_handle = NULL;
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
				dev_handle = usb_open(dev);
				if (NULL == dev_handle)
				{
					LOG_ERROR("failed to open %04X:%04X, %s", VID, PID, 
								usb_strerror());
					continue;
				}
				
				// check description string
				if (((productstring != NULL) 
						&& !usb_check_string(dev_handle, productindex, 
												productstring, NULL, 0))
					|| ((serialstring != NULL) 
						&& !usb_check_string(dev_handle, serialindex, 
												serialstring, NULL, 0)))
				{
					usb_close(dev_handle);
					dev_handle = NULL;
					continue;
				}
				
#if !IS_WIN32
				usb_reset(usb);
				
				{
					char driver_name[256];
					memset(driver_name, 0, sizeof(driver_name));
					if (usb_get_driver_np(dev_handle, interface, driver_name, 
											sizeof(driver_name)) == 0)
					{
						if (usb_detach_kernel_driver_np(dev_handle, 
														interface) < 0)
						{
							LOG_ERROR("fail to detach form: %s, err: %s", 
										driver_name, usb_strerror());
							printf("%s\n", usb_strerror());
							continue;
						}
						usb_set_altinterface(dh, interface);
					}
				}
#endif
				
				// usb_set_configuration required under win32
				config_value = dev->config[0].bConfigurationValue;
				if (usb_set_configuration(dev_handle, config_value) != 0)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION_MESSAGE, 
								"set configuration", usb_strerror());
					usb_close(dev_handle);
					dev_handle = NULL;
					continue;
				}
				if (usb_claim_interface(dev_handle, interface) != 0)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION_MESSAGE, 
								"claim interface", usb_strerror());
					usb_close(dev_handle);
					dev_handle = NULL;
					continue;
				}
				
				if (dev_handle != NULL)
				{
					return dev_handle;
				}
			}
		}
	}
	
	return dev_handle;
}


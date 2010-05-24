/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_GPIO.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_GPIO                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_GPIO_EN

#include "USB_TO_XXX.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_GPIO_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint16 port_data, mask_data, io_data;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_GPIO_NUM)
		{
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = dat[index + 1] + (dat[index + 2] << 8);
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			buffer_reply[rep_len++] = USB_TO_GPIO_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			mask_data = dat[index + 0] + (dat[index + 1] << 8);
			io_data   = dat[index + 2] + (dat[index + 3] << 8);
			port_data = dat[index + 4] + (dat[index + 5] << 8);
			io_data  &= mask_data;

			if ((mask_data & io_data & ~USB_TO_GPIO_OUT_MSK) 
				|| (mask_data & ~io_data & ~USB_TO_GPIO_IN_MSK))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_INVALID_PARA;
				return;
			}

			if (mask_data & USB_TO_GPIO_SRST)
			{
				if (io_data & USB_TO_GPIO_SRST)
				{
					SW_SETOUTPUT();
				}
				else
				{
					if (port_data & USB_TO_GPIO_SRST)
					{
						SW_SETINPUT_PU();
					}
					else
					{
						SW_SETINPUT_PD();
					}
				}
			}

			if (mask_data & USB_TO_GPIO_TRST)
			{
				if (io_data & USB_TO_GPIO_TRST)
				{
					SW_RST_SETOUTPUT();
				}
				else
				{
					if (port_data & USB_TO_GPIO_TRST)
					{
						SW_RST_SETINPUT_PU();
					}
					else
					{
						SW_RST_SETINPUT_PD();
					}
				}
			}

			if (mask_data & USB_TO_GPIO_TMS)
			{
				if (io_data & USB_TO_GPIO_TMS)
				{
					JTAG_TAP_TMS_SETOUTPUT();
				}
				else
				{
					JTAG_TAP_TMS_SETINPUT();
				}
			}

#if JTAG_HAS_USER_PIN
			if (mask_data & USB_TO_GPIO_USR1)
			{
				if (io_data & USB_TO_GPIO_USR1)
				{
					JTAG_TAP_USR1_SETOUTPUT();
				}
				else
				{
					JTAG_TAP_USR1_SETINPUT();
				}
			}
			if (mask_data & USB_TO_GPIO_USR2)
			{
				if (io_data & USB_TO_GPIO_USR2)
				{
					JTAG_TAP_USR2_SETOUTPUT();
				}
				else
				{
					JTAG_TAP_USR2_SETINPUT();
				}
			}
#endif

			if (mask_data & USB_TO_GPIO_TCK)
			{
				JTAG_TAP_TCK_SETOUTPUT();
			}
			if (mask_data & USB_TO_GPIO_TDI)
			{
				JTAG_TAP_TDI_SETOUTPUT();
			}
			if(mask_data & USB_TO_GPIO_TDO)
			{
				JTAG_TAP_TDO_SETINPUT();
			}
			if(mask_data & USB_TO_GPIO_RTCK)
			{
				JTAG_TAP_RTCK_SETINPUT();
			}
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			break;
		case USB_TO_XXX_IN:
			mask_data = dat[index + 0] + (dat[index + 1] << 8);
			if(mask_data & ~USB_TO_GPIO_IN_MSK)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_INVALID_PARA;
				return;
			}
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			port_data = 0;
			if(mask_data & USB_TO_GPIO_SRST)
			{
				if(SW_GET())
				{
					port_data |= USB_TO_GPIO_SRST;
				}
			}
			if(mask_data & USB_TO_GPIO_TRST)
			{
				if(SW_RST_GET())
				{
					port_data |= USB_TO_GPIO_TRST;
				}
			}
			if(mask_data & USB_TO_GPIO_TMS)
			{
				if(JTAG_TAP_TMS_GET())
				{
					port_data |= USB_TO_GPIO_TMS;
				}
			}
#if JTAG_HAS_USER_PIN
			if(mask_data & USB_TO_GPIO_USR2)
			{
				if(JTAG_TAP_USR2_GET())
				{
					port_data |= USB_TO_GPIO_USR2;
				}
			}
#endif
			if(mask_data & USB_TO_GPIO_TDO)
			{
				if(JTAG_TAP_TDO_GET())
				{
					port_data |= USB_TO_GPIO_TDO;
				}
			}
			if(mask_data & USB_TO_GPIO_RTCK)
			{
				if(JTAG_TAP_RTCK_GET())
				{
					port_data |= USB_TO_GPIO_RTCK;
				}
			}

			buffer_reply[rep_len++] = (uint8)port_data;
			buffer_reply[rep_len++] = (uint8)(port_data >> 8);

			break;
		case USB_TO_XXX_OUT:
			mask_data = dat[index + 0] + (dat[index + 1] << 8);
			if((mask_data & ~USB_TO_GPIO_MSK) > 0)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_INVALID_PARA;
				return;
			}

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			port_data = dat[index + 2] + (dat[index + 3] << 8);

			if(mask_data & USB_TO_GPIO_SRST)
			{
				if(port_data & USB_TO_GPIO_SRST)
				{
					SW_SET();
				}
				else
				{
					SW_CLR();
				}
			}
			if(mask_data & USB_TO_GPIO_TRST)
			{
				if(port_data & USB_TO_GPIO_TRST)
				{
					SW_RST_SET();
				}
				else
				{
					SW_RST_CLR();
				}
			}
			if(mask_data & USB_TO_GPIO_TMS)
			{
				if(port_data & USB_TO_GPIO_TMS)
				{
					JTAG_TAP_TMS_SET();
				}
				else
				{
					JTAG_TAP_TMS_CLR();
				}
			}
#if JTAG_HAS_USER_PIN
			if(mask_data & USB_TO_GPIO_USR1)
			{
				if(port_data & USB_TO_GPIO_USR1)
				{
					JTAG_TAP_USR1_SET();
				}
				else
				{
					JTAG_TAP_USR1_CLR();
				}
			}
#endif
			if(mask_data & USB_TO_GPIO_TCK)
			{
				if(port_data & USB_TO_GPIO_TCK)
				{
					JTAG_TAP_TCK_SET();
				}
				else
				{
					JTAG_TAP_TCK_CLR();
				}
			}
			if(mask_data & USB_TO_GPIO_TDI)
			{
				if(port_data & USB_TO_GPIO_TDI)
				{
					JTAG_TAP_TDI_SET();
				}
				else
				{
					JTAG_TAP_TDI_CLR();
				}
			}

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif

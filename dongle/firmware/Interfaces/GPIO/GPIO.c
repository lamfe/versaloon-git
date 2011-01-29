/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       GPIO.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "interfaces.h"

#include "GPIO.h"

RESULT gpio_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT gpio_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT gpio_config(uint8_t index, uint16_t pin_mask, uint16_t io, 
					uint16_t pull_en_mask, uint16_t input_pull_mask)
{
	switch (index)
	{
	case 0:
		if ((pin_mask & io & ~GPIO_OUT_MSK) 
			|| (pin_mask & ~io & ~GPIO_IN_MSK))
		{
			return ERROR_FAIL;
		}

		if (pin_mask & GPIO_SRST)
		{
			if (io & GPIO_SRST)
			{
				if(input_pull_mask & GPIO_SRST)
				{
					SW_SET();
				}
				else
				{
					SW_CLR();
				}
				SW_SETOUTPUT();
			}
			else
			{
				if (pull_en_mask & GPIO_SRST)
				{
					if (input_pull_mask & GPIO_SRST)
					{
						SW_SETINPUT_PU();
					}
					else
					{
						SW_SETINPUT_PD();
					}
				}
				else
				{
					SW_SETINPUT();
				}
			}
		}
		if (pin_mask & GPIO_TRST)
		{
			if (io & GPIO_TRST)
			{
				if(input_pull_mask & GPIO_TRST)
				{
					SW_RST_SET();
				}
				else
				{
					SW_RST_CLR();
				}
				SW_RST_SETOUTPUT();
			}
			else
			{
				if (pull_en_mask & GPIO_TRST)
				{
					if (input_pull_mask & GPIO_TRST)
					{
						SW_RST_SETINPUT_PU();
					}
					else
					{
						SW_RST_SETINPUT_PD();
					}
				}
				else
				{
					SW_RST_SETINPUT();
				}
			}
		}
#if INTERFACE_BDM_EN
		if (pin_mask & GPIO_SYNCSWPWM_GPIO)
		{
			if (io & GPIO_SYNCSWPWM_GPIO)
			{
				if(input_pull_mask & GPIO_SYNCSWPWM_GPIO)
				{
					SYNCSWPWM_GPIO_SET();
				}
				else
				{
					SYNCSWPWM_GPIO_CLR();
				}
				SYNCSWPWM_GPIO_SETOUTPUT();
			}
			else
			{
				if (pull_en_mask & GPIO_SYNCSWPWM_GPIO)
				{
					if (input_pull_mask & GPIO_SYNCSWPWM_GPIO)
					{
						SYNCSWPWM_GPIO_SETINPUT_PU();
					}
					else
					{
						SYNCSWPWM_GPIO_SETINPUT_PD();
					}
				}
				else
				{
					SYNCSWPWM_GPIO_SETINPUT();
				}
			}
		}
#endif
		if (pin_mask & GPIO_TMS)
		{
			if (io & GPIO_TMS)
			{
				if(input_pull_mask & GPIO_TMS)
				{
					JTAG_TAP_TMS_SET();
				}
				else
				{
					JTAG_TAP_TMS_CLR();
				}
				JTAG_TAP_TMS_SETOUTPUT();
			}
			else
			{
				JTAG_TAP_TMS_SETINPUT();
			}
		}
#if JTAG_HAS_USER_PIN
		if (pin_mask & GPIO_USR1)
		{
			if (io & GPIO_USR1)
			{
				if(input_pull_mask & GPIO_USR1)
				{
					JTAG_TAP_USR1_SET();
				}
				else
				{
					JTAG_TAP_USR1_CLR();
				}
				JTAG_TAP_USR1_SETOUTPUT();
			}
			else
			{
				JTAG_TAP_USR1_SETINPUT();
			}
		}
		if (pin_mask & GPIO_USR2)
		{
			if (io & GPIO_USR2)
			{
				if(input_pull_mask & GPIO_USR2)
				{
					JTAG_TAP_USR2_SET();
				}
				else
				{
					JTAG_TAP_USR2_CLR();
				}
				JTAG_TAP_USR2_SETOUTPUT();
			}
			else
			{
				JTAG_TAP_USR2_SETINPUT();
			}
		}
#endif
		if (pin_mask & GPIO_TCK)
		{
			JTAG_TAP_TCK_SETOUTPUT();
		}
		if (pin_mask & GPIO_TDI)
		{
			JTAG_TAP_TDI_SETOUTPUT();
		}
		if(pin_mask & GPIO_TDO)
		{
			JTAG_TAP_TDO_SETINPUT();
		}
		if(pin_mask & GPIO_RTCK)
		{
			JTAG_TAP_RTCK_SETINPUT();
		}
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT gpio_out(uint8_t index, uint16_t pin_mask, uint16_t value)
{
	switch (index)
	{
	case 0:
		if((pin_mask & ~GPIO_MSK) > 0)
		{
			return ERROR_FAIL;
		}

		if(pin_mask & GPIO_SRST)
		{
			if(value & GPIO_SRST)
			{
				SW_SET();
			}
			else
			{
				SW_CLR();
			}
		}
		if(pin_mask & GPIO_TRST)
		{
			if(value & GPIO_TRST)
			{
				SW_RST_SET();
			}
			else
			{
				SW_RST_CLR();
			}
		}
#if INTERFACE_BDM_EN
		if(pin_mask & GPIO_SYNCSWPWM_GPIO)
		{
			if(value & GPIO_SYNCSWPWM_GPIO)
			{
				SYNCSWPWM_GPIO_SET();
			}
			else
			{
				SYNCSWPWM_GPIO_CLR();
			}
		}
#endif
		if(pin_mask & GPIO_TMS)
		{
			if(value & GPIO_TMS)
			{
				JTAG_TAP_TMS_SET();
			}
			else
			{
				JTAG_TAP_TMS_CLR();
			}
		}
#if JTAG_HAS_USER_PIN
		if(pin_mask & GPIO_USR1)
		{
			if(value & GPIO_USR1)
			{
				JTAG_TAP_USR1_SET();
			}
			else
			{
				JTAG_TAP_USR1_CLR();
			}
		}
		if(pin_mask & GPIO_USR2)
		{
			if(value & GPIO_USR2)
			{
				JTAG_TAP_USR2_SET();
			}
			else
			{
				JTAG_TAP_USR2_CLR();
			}
		}
#endif
		if(pin_mask & GPIO_TCK)
		{
			if(value & GPIO_TCK)
			{
				JTAG_TAP_TCK_SET();
			}
			else
			{
				JTAG_TAP_TCK_CLR();
			}
		}
		if(pin_mask & GPIO_TDI)
		{
			if(value & GPIO_TDI)
			{
				JTAG_TAP_TDI_SET();
			}
			else
			{
				JTAG_TAP_TDI_CLR();
			}
		}
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT gpio_in(uint8_t index, uint16_t pin_mask, uint16_t *value)
{
	uint16_t port_data;

	switch (index)
	{
	case 0:
		if(pin_mask & ~GPIO_IN_MSK)
		{
			return ERROR_FAIL;
		}

		port_data = 0;
		if(pin_mask & GPIO_SRST)
		{
			if(SW_GET())
			{
				port_data |= GPIO_SRST;
			}
		}
		if(pin_mask & GPIO_TRST)
		{
			if(SW_RST_GET())
			{
				port_data |= GPIO_TRST;
			}
		}
#if INTERFACE_BDM_EN
		if(pin_mask & GPIO_SYNCSWPWM_GPIO)
		{
			if(SYNCSWPWM_GPIO_GET())
			{
				port_data |= GPIO_SYNCSWPWM_GPIO;
			}
		}
#endif
		if(pin_mask & GPIO_TMS)
		{
			if(JTAG_TAP_TMS_GET())
			{
				port_data |= GPIO_TMS;
			}
		}
#if JTAG_HAS_USER_PIN
		if(pin_mask & GPIO_USR1)
		{
			if(JTAG_TAP_USR1_GET())
			{
				port_data |= GPIO_USR1;
			}
		}
		if(pin_mask & GPIO_USR2)
		{
			if(JTAG_TAP_USR2_GET())
			{
				port_data |= GPIO_USR2;
			}
		}
#endif
		if(pin_mask & GPIO_TDO)
		{
			if(JTAG_TAP_TDO_GET())
			{
				port_data |= GPIO_TDO;
			}
		}
		if(pin_mask & GPIO_RTCK)
		{
			if(JTAG_TAP_RTCK_GET())
			{
				port_data |= GPIO_RTCK;
			}
		}
		if (value != NULL)
		{
			*value = port_data;
		}
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

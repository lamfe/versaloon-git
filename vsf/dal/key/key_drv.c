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

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces.h"
#include "../dal.h"

#include "key_drv.h"

#if DAL_KEY_EN

// note:
// need to initialize interfaces.tickcnt first
// so that key module can calculate filter and duration

vsf_err_t key_init(struct dal_info_t *info)
{
	struct key_interface_t *ifs = (struct key_interface_t *)info->ifs;
	struct key_param_t *param = (struct key_param_t *)info->param;
	struct key_info_t *key_info = (struct key_info_t *)info->info;
	
	key_info->isdown_unfiltered = key_info->isdown_filtered = false;
	interfaces->gpio.init(ifs->port);
	if (param->valid_low)
	{
		interfaces->gpio.config_pin(ifs->port, ifs->pin, GPIO_INPU);
	}
	else
	{
		interfaces->gpio.config_pin(ifs->port, ifs->pin, GPIO_INPD);
	}
	// interrupt initialization
	
	return VSFERR_NONE;
}

vsf_err_t key_fini(struct dal_info_t *info)
{
	struct key_interface_t *ifs = (struct key_interface_t *)info->ifs;
	
	return interfaces->gpio.config_pin(ifs->port, ifs->pin, GPIO_INFLOAT);
}

bool key_isdown_unfiltered(struct dal_info_t *info)
{
	struct key_interface_t *ifs = (struct key_interface_t *)info->ifs;
	struct key_param_t *param = (struct key_param_t *)info->param;
	
	if (param->valid_low)
	{
		return !interfaces->gpio.get(ifs->port, 1 << ifs->pin);
	}
	else
	{
		return interfaces->gpio.get(ifs->port, 1 << ifs->pin);
	}
}

vsf_err_t key_poll(struct dal_info_t *info)
{
	struct key_info_t *key_info = (struct key_info_t *)info->info;
	struct key_param_t *param = (struct key_param_t *)info->param;
	uint32_t cur_tickcnt;
	
	cur_tickcnt = interfaces->tickclk.get_count();
	if (key_info->isdown_unfiltered)
	{
		uint32_t diff_tickcnt = cur_tickcnt - key_info->tickcnt_on_press;
		
		if (!key_isdown_unfiltered(info))
		{
			key_info->isdown_unfiltered = key_info->isdown_filtered = false;
			if (diff_tickcnt >= param->filter_ms)
			{
				if (param->callback.on_PRESSED != NULL)
				{
					param->callback.on_PRESSED(info, diff_tickcnt);
				}
			}
		}
		else if (diff_tickcnt >= param->filter_ms)
		{
			if (!key_info->isdown_filtered)
			{
				key_info->isdown_filtered = true;
				if (param->callback.on_PRESS != NULL)
				{
					param->callback.on_PRESS(info);
				}
			}
		}
	}
	else
	{
		if (key_isdown_unfiltered(info))
		{
			key_info->isdown_unfiltered = true;
			key_info->tickcnt_on_press = cur_tickcnt;
		}
	}
	return VSFERR_NONE;
}

#endif

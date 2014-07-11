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

#include "joystick_drv.h"

#if DAL_JOYSTICK_EN

vsf_err_t joystick_init(struct dal_info_t *info)
{
	struct joystick_interface_t *ifs = (struct joystick_interface_t *)info->ifs;
	struct joystick_param_t *param = (struct joystick_param_t *)info->param;
	struct joystick_info_t *joystick_info =
										(struct joystick_info_t *)info->info;
	
	joystick_info->adc_value_x = joystick_info->adc_value_y = 0;
	joystick_info->state = JOYSTICK_IDLE;
	joystick_info->adc_sample_tickcnt = interfaces->tickclk.get_count();
	
	interfaces->adc.init(ifs->adc_port_x);
	interfaces->adc.config(ifs->adc_port_x, param->adc_clock, ADC_ALIGNRIGHT);
	joystick_info->adc_max_value_x =
								interfaces->adc.get_max_value(ifs->adc_port_x);
	if (ifs->adc_port_x != ifs->adc_port_y)
	{
		interfaces->adc.init(ifs->adc_port_y);
		interfaces->adc.config(ifs->adc_port_y, param->adc_clock,
								ADC_ALIGNRIGHT);
		joystick_info->adc_max_value_y =
								interfaces->adc.get_max_value(ifs->adc_port_y);
	}
	interfaces->adc.config_channel(ifs->adc_port_x, ifs->adc_channel_x,
									param->adc_sample_cycles);
	interfaces->adc.calibrate(ifs->adc_port_x, ifs->adc_channel_x);
	interfaces->adc.config_channel(ifs->adc_port_y, ifs->adc_channel_y,
									param->adc_sample_cycles);
	interfaces->adc.calibrate(ifs->adc_port_y, ifs->adc_channel_y);
	return VSFERR_NONE;
}

vsf_err_t joystick_fini(struct dal_info_t *info)
{
	return VSFERR_NONE;
}

uint32_t joystick_get_x(struct dal_info_t *info)
{
	struct joystick_param_t *param = (struct joystick_param_t *)info->param;
	struct joystick_info_t *joystick_info =
										(struct joystick_info_t *)info->info;
	
	return joystick_info->adc_value_x * param->resolution /
				joystick_info->adc_max_value_x;
}

uint32_t joystick_get_y(struct dal_info_t *info)
{
	struct joystick_param_t *param = (struct joystick_param_t *)info->param;
	struct joystick_info_t *joystick_info =
										(struct joystick_info_t *)info->info;
	
	return joystick_info->adc_value_y * param->resolution /
				joystick_info->adc_max_value_y;
}

vsf_err_t joystick_poll(struct dal_info_t *info)
{
	struct joystick_interface_t *ifs = (struct joystick_interface_t *)info->ifs;
	struct joystick_param_t *param = (struct joystick_param_t *)info->param;
	struct joystick_info_t *joystick_info =
										(struct joystick_info_t *)info->info;
	uint32_t cur_tickcnt = interfaces->tickclk.get_count();
	
	switch (joystick_info->state)
	{
	case JOYSTICK_IDLE:
		if ((cur_tickcnt - joystick_info->adc_sample_tickcnt) >=
				param->sample_interval_ms)
		{
			interfaces->adc.start(ifs->adc_port_x, ifs->adc_channel_x);
			joystick_info->state = JOYSTICK_X_STARTED;
		}
	case JOYSTICK_X_STARTED:
		if (!interfaces->adc.isready(ifs->adc_port_x, ifs->adc_channel_x))
		{
			joystick_info->adc_value_x =
					interfaces->adc.get(ifs->adc_port_x, ifs->adc_channel_x);
			interfaces->adc.start(ifs->adc_port_y, ifs->adc_channel_y);
			joystick_info->state = JOYSTICK_Y_STARTED;
		}
	case JOYSTICK_Y_STARTED:
		if (!interfaces->adc.isready(ifs->adc_port_y, ifs->adc_channel_y))
		{
			joystick_info->adc_value_y =
					interfaces->adc.get(ifs->adc_port_y, ifs->adc_channel_y);
			joystick_info->state = JOYSTICK_IDLE;
		}
	}
	return VSFERR_NONE;
}

#endif

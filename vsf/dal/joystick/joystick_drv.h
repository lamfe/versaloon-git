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

#ifndef __JOYSTICK_DRV_H_INCLUDED__
#define __JOYSTICK_DRV_H_INCLUDED__

struct joystick_interface_t
{
	uint8_t adc_port_x;
	uint8_t adc_channel_x;
	uint8_t adc_port_y;
	uint8_t adc_channel_y;
};

struct joystick_param_t
{
	uint32_t sample_interval_ms;
	uint32_t resolution;
	uint32_t adc_clock;
	uint8_t adc_sample_cycles;
};

struct joystick_info_t
{
	// private
	uint32_t adc_value_x;
	uint32_t adc_max_value_x;
	uint32_t adc_value_y;
	uint32_t adc_max_value_y;
	
	uint32_t adc_sample_tickcnt;
	
	enum adc_state_t
	{
		JOYSTICK_IDLE,
		JOYSTICK_X_STARTED,
		JOYSTICK_Y_STARTED,
	} state;
};

vsf_err_t joystick_init(struct dal_info_t *info);
vsf_err_t joystick_fini(struct dal_info_t *info);
uint32_t joystick_get_x(struct dal_info_t *info);
uint32_t joystick_get_y(struct dal_info_t *info);
vsf_err_t joystick_poll(struct dal_info_t *info);

#endif

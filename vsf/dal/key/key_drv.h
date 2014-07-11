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

#ifndef __KEY_DRV_H_INCLUDED__
#define __KEY_DRV_H_INCLUDED__

struct key_interface_t
{
	uint8_t port;
	uint8_t pin;
};

struct key_param_t
{
	bool valid_low;
	uint32_t filter_ms;
	struct
	{
		void (*on_PRESS)(struct dal_info_t *info);
		void (*on_PRESSED)(struct dal_info_t *info, uint32_t ms);
	} callback;
};

struct key_info_t
{
	// private
	uint32_t tickcnt_on_press;
	bool isdown_unfiltered;
	bool isdown_filtered;
};

vsf_err_t key_init(struct dal_info_t *info);
vsf_err_t key_fini(struct dal_info_t *info);
bool key_isdown_unfiltered(struct dal_info_t *info);
vsf_err_t key_poll(struct dal_info_t *info);

#endif

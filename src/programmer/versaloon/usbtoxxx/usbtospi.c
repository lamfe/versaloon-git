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
#include <string.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

uint8 usbtospi_num_of_interface = 0;


RESULT usbtospi_init(void)
{
	return usbtoxxx_init_command(USB_TO_SPI, &usbtospi_num_of_interface);
}

RESULT usbtospi_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_SPI);
}

RESULT usbtospi_config(uint8 interface_index, uint16 freq, uint8 cpol, 
					   uint8 cpha, uint8 firstbit)
{
	uint8 conf[3];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	conf[0] = cpol | cpha | firstbit;
	conf[1] = (freq >> 0) & 0xFF;
	conf[2] = (freq >> 8) & 0xFF;
	
	return usbtoxxx_conf_command(USB_TO_SPI, interface_index, conf, 3);
}

RESULT usbtospi_io(uint8 interface_index, uint8 *out, uint8 *in, 
				   uint16 outlen, uint16 inpos, uint16 inlen)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_inout_command(USB_TO_SPI, interface_index, out, outlen, 
								  outlen, in, inpos, inlen, 1);
}


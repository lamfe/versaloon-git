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

#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"
#include "port.h"

#include "scripts.h"
#include "programmer.h"
#include "mic2826.h"

VSS_HANDLER(mic2826_vss_init)
{
	uint16_t kHz;
	
	VSS_CHECK_ARGC(2);
	
	kHz = (uint16_t)strtoul(argv[1], NULL, 0);
	return mic2826.init(kHz);
}

VSS_HANDLER(mic2826_vss_fini)
{
	VSS_CHECK_ARGC(1);
	return mic2826.fini();
}

VSS_HANDLER(mic2826_vss_config)
{
	uint16_t DCDC_mV, LDO1_mV, LDO2_mV, LDO3_mV;
	RESULT ret;
	
	VSS_CHECK_ARGC(5);
	
	DCDC_mV = (uint16_t)strtoul(argv[1], NULL, 0);
	LDO1_mV = (uint16_t)strtoul(argv[2], NULL, 0);
	LDO2_mV = (uint16_t)strtoul(argv[3], NULL, 0);
	LDO3_mV = (uint16_t)strtoul(argv[4], NULL, 0);
	
	LOG_PUSH();
	LOG_MUTE();
	ret = mic2826.config(DCDC_mV, LDO1_mV, LDO2_mV, LDO3_mV);
	LOG_POP();
	
	return ret;
}


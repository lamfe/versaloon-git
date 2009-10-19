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
#include <stdlib.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "programmer.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

adi_dp_if_t *cm3_dp_if = NULL;

RESULT cm3_dp_parameter_init(adi_dp_if_t *dp)
{
	dp->memaccess_tck = 8;
	dp->tar_autoincr_block = (1 << 12);
	
	return ERROR_OK;
}

RESULT cm3_dp_fini(void)
{
	cm3_dp_if = NULL;
	return adi_fini();
}

RESULT cm3_dp_init(programmer_info_t *prog, adi_dp_if_t *dp)
{
	uint32 cpuid;
	
	cm3_dp_if = dp;
	
	cm3_dp_parameter_init(cm3_dp_if);
	if (ERROR_OK != adi_init(prog, cm3_dp_if))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "initialize cm3 interface");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (ERROR_OK != adi_memap_read_reg(CM3_CPUID, &cpuid, 1))
	{
		return ERROR_FAIL;
	}
	if (((cpuid >> 4) & 0xC3F) == 0xC23)
	{
		LOG_INFO(_GETTEXT("CORTEX-M3 processor detected\n"));
	}
	else
	{
		LOG_WARNING(_GETTEXT("Is target a CORTEX-M3?\n"));
	}
	LOG_INFO("CPUID: 0x%08X\n", cpuid);
	
	return ERROR_OK;
}

uint32 cm3_get_max_block_size(uint32 address)
{
	return adi_memap_get_max_tar_block_size(cm3_dp_if->tar_autoincr_block, 
											address);
}

RESULT cm3_dp_run(void)
{
	uint32 dcb_dhcsr = 0;
	uint8 wait_halt_clear_delay_in_10ms;
	
	if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	
	// enable debug
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		dcb_dhcsr = (uint32)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN);
		adi_memap_write_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
	}
	
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		// clear halt
		dcb_dhcsr = (uint32)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN);
		adi_memap_write_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
	}
	// wait halt clear
	wait_halt_clear_delay_in_10ms = 100;	// 1000ms max delay in all
	while ((dcb_dhcsr & CM3_DCB_DHCSR_S_HALT) && wait_halt_clear_delay_in_10ms)
	{
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
		wait_halt_clear_delay_in_10ms--;
		sleep_ms(10);
	}
	
	LOG_INFO(_GETTEXT("dhcsr: 0x%08X\n"), dcb_dhcsr);
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		LOG_INFO(_GETTEXT("Fail to run target.\n"));
		return ERROR_OK;
	}
	else
	{
		LOG_INFO(_GETTEXT("Target running.\n"));
		return ERROR_FAIL;
	}
}

RESULT cm3_dp_halt(void)
{
	uint32 dcb_dhcsr = 0;
	uint8 wait_halt_delay_in_10ms;
	
	if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	
	// enable debug
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		dcb_dhcsr = (uint32)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN 
								| CM3_DCB_DHCSR_C_HALT);
		adi_memap_write_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
	}
	// halt
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_S_HALT))
	{
		dcb_dhcsr = (uint32)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN 
								| CM3_DCB_DHCSR_C_HALT);
		adi_memap_write_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
	}
	// wait halt
	wait_halt_delay_in_10ms = 100;	// 1000ms max delay in all
	while ((!(dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)) && wait_halt_delay_in_10ms)
	{
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
		wait_halt_delay_in_10ms--;
		sleep_ms(10);
	}
	
	LOG_INFO(_GETTEXT("dhcsr: 0x%08X\n"), dcb_dhcsr);
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		LOG_INFO(_GETTEXT("Target halted.\n"));
		return ERROR_OK;
	}
	else
	{
		LOG_INFO(_GETTEXT("Fail to halt target.\n"));
		return ERROR_FAIL;
	}
}


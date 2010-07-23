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
#include <stdlib.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

adi_dpif_t cm3_dp_if;

RESULT cm3_dp_fini(void)
{
	return adi_fini();
}

RESULT cm3_dp_init(struct program_context_t *context, adi_dpif_t *dp)
{
	uint32_t cpuid;
	enum adi_dp_target_core_t tgt_core = ADI_DP_INVALID;
	
	memcpy(&cm3_dp_if, dp, sizeof(cm3_dp_if));
	
	// adi_init will initialize the core type
	if (ERROR_OK != adi_init(context, &cm3_dp_if, &tgt_core))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize cm3 interface");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if ((ERROR_OK != cm3_dp_halt()) 
		|| (ERROR_OK != adi_memap_read_reg(CM3_CPUID, &cpuid, 1)))
	{
		return ERROR_FAIL;
	}
	// 0xC23 is for CortexM3
	// 0xC20 is for CortexM0
	if ((((cpuid >> 4) & 0xC3F) == 0xC23) && (ADI_DP_CM3 == tgt_core))
	{
		LOG_INFO("CORTEX-M3 processor detected");
	}
	else if ((((cpuid >> 4) & 0xC3F) == 0xC20) && (ADI_DP_CM0 == tgt_core))
	{
		LOG_INFO("CORTEX-M0 processor detected");
	}
	else
	{
		LOG_WARNING("Is target a CORTEX-Chip?");
	}
	LOG_INFO(INFOMSG_REG_08X, "CPUID", cpuid);
	
	return ERROR_OK;
}

RESULT cm3_write_core_register(uint8_t reg_idx, uint32_t *value)
{
	uint32_t dcrdr, reg;
	
	if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DCRDR, &dcrdr, 1))
	{
		return ERROR_FAIL;
	}
	
	reg = reg_idx | CM3_DCB_DCRSR_WnR;
	adi_memap_write_reg(CM3_DCB_DCRDR, value, 0);
	adi_memap_write_reg(CM3_DCB_DCRSR, &reg, 0);
	
	return adi_memap_write_reg(CM3_DCB_DCRDR, &dcrdr, 1);
}

RESULT cm3_read_core_register(uint8_t reg_idx, uint32_t *value)
{
	uint32_t dcrdr, reg;
	
	if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DCRDR, &dcrdr, 1))
	{
		return ERROR_FAIL;
	}
	
	reg = reg_idx;
	adi_memap_write_reg(CM3_DCB_DCRSR, &reg, 0);
	adi_memap_read_reg(CM3_DCB_DCRDR, value, 0);
	
	return adi_memap_write_reg(CM3_DCB_DCRDR, &dcrdr, 1);
}

uint32_t cm3_get_max_block_size(uint32_t address)
{
	return adi_memap_get_max_tar_block_size(address);
}

RESULT cm3_reset(void)
{
	uint32_t reg;
	
	// check result should not be enabled here
	// because after reset, dp maybe disabled
	reg = CM3_REG_NVIC_AIRCR_VECTKEY | CM3_REG_NVIC_AIRCR_SYSRESETREQ;
	if (ERROR_OK != adi_memap_write_reg(CM3_REG_NVIC_AIRCR, &reg, 0))
	{
		return ERROR_FAIL;
	}
	return adi_dp_commit();
}

RESULT cm3_dp_run(void)
{
	uint32_t dcb_dhcsr = 0;
	uint8_t wait_halt_clear_delay_in_10ms;
	
	if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	
	// enable debug
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		dcb_dhcsr = (uint32_t)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN);
		adi_memap_write_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
	}
	
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		// clear halt
		dcb_dhcsr = (uint32_t)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN);
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
	
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}

RESULT cm3_dp_halt(void)
{
	uint32_t dcb_dhcsr = 0;
	uint8_t wait_halt_delay_in_10ms;
	
	if (ERROR_OK != adi_memap_read_reg(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	
	// enable debug
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		dcb_dhcsr = (uint32_t)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN 
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
		dcb_dhcsr = (uint32_t)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN 
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
	
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		return ERROR_OK;
	}
	else
	{
		return ERROR_FAIL;
	}
}


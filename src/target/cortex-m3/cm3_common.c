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

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

struct adi_dpif_t cm3_dp_if;

RESULT cm3_dp_fini(void)
{
	return adi_fini();
}

RESULT cm3_dp_init(struct program_context_t *context, struct adi_dpif_t *dp)
{
	uint32_t cpuid, dcb_dhcsr;
	enum adi_dp_target_core_t tgt_core = ADI_DP_INVALID;
	
	memcpy(&cm3_dp_if, dp, sizeof(cm3_dp_if));
	
	// adi_init will initialize the core type
	if (ERROR_OK != adi_init(context, &cm3_dp_if, &tgt_core))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize cm3 interface");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enable debug
	if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		uint32_t reg0 = 0;
		dcb_dhcsr = (uint32_t)(CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN);
		if ((ERROR_OK != adi_memap_write_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 0)) || 
			(ERROR_OK != adi_memap_write_reg32(CM3_DCB_DCRDR, &reg0, 1)))
		{
			return ERROR_FAIL;
		}
	}
	
	if ((ERROR_OK != cm3_dp_halt()) 
		|| (ERROR_OK != adi_memap_read_reg32(CM3_CPUID, &cpuid, 1)))
	{
		return ERROR_FAIL;
	}
	cpuid = LE_TO_SYS_U32(cpuid);
	// 0xC23 is for CortexM3
	// 0xC20 is for CortexM0
	if ((((cpuid >> 4) & 0xC3F) == 0xC23) && (ADI_DP_CM3 == tgt_core))
	{
		LOG_INFO("CORTEX-M3 r%dp%d processor detected", 
					(cpuid >> 20) & 0x0F, (cpuid >> 0) & 0x0F);
	}
	else if ((((cpuid >> 4) & 0xC3F) == 0xC20) && (ADI_DP_CM0 == tgt_core))
	{
		LOG_INFO("CORTEX-M0 r%dp%d processor detected", 
					(cpuid >> 20) & 0x0F, (cpuid >> 0) & 0x0F);
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
	
	if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DCRDR, &dcrdr, 1))
	{
		return ERROR_FAIL;
	}
	
	reg = reg_idx | CM3_DCB_DCRSR_WnR;
	adi_memap_write_reg32(CM3_DCB_DCRDR, value, 0);
	adi_memap_write_reg32(CM3_DCB_DCRSR, &reg, 0);
	
	return adi_memap_write_reg32(CM3_DCB_DCRDR, &dcrdr, 1);
}

RESULT cm3_read_core_register(uint8_t reg_idx, uint32_t *value)
{
	uint32_t dcrdr, reg;
	
	if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DCRDR, &dcrdr, 1))
	{
		return ERROR_FAIL;
	}
	
	reg = reg_idx;
	adi_memap_write_reg32(CM3_DCB_DCRSR, &reg, 0);
	adi_memap_read_reg32(CM3_DCB_DCRDR, value, 0);
	
	return adi_memap_write_reg32(CM3_DCB_DCRDR, &dcrdr, 1);
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
	if (ERROR_OK != adi_memap_write_reg32(CM3_REG_NVIC_AIRCR, &reg, 0))
	{
		return ERROR_FAIL;
	}
	return adi_dp_commit();
}

RESULT cm3_dp_resume(void)
{
	uint32_t dcb_dhcsr = 0;
	uint8_t wait_halt_clear_delay_in_10ms;
	
	if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		return ERROR_FAIL;
	}
	
	if (dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)
	{
		uint32_t corereg20;
		
		// disable exception
		if (ERROR_OK != cm3_read_core_register(CM3_COREREG_20, &corereg20))
		{
			return ERROR_FAIL;
		}
		corereg20 |= CM3_PRIMASK_PM;
		if (ERROR_OK != cm3_write_core_register(CM3_COREREG_20, &corereg20))
		{
			return ERROR_FAIL;
		}
		
		// clear halt
		dcb_dhcsr &= ~(0xFFFF0000 | CM3_DCB_DHCSR_C_HALT);
		dcb_dhcsr |= CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN;
		adi_memap_write_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	}
	// wait halt clear
	wait_halt_clear_delay_in_10ms = 100;	// 1000ms max delay in all
	while ((dcb_dhcsr & CM3_DCB_DHCSR_S_HALT) && wait_halt_clear_delay_in_10ms)
	{
		if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
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
	
	if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
	{
		return ERROR_FAIL;
	}
	dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_C_DEBUGEN))
	{
		return ERROR_FAIL;
	}
	
	// halt
	if (!(dcb_dhcsr & CM3_DCB_DHCSR_S_HALT))
	{
		dcb_dhcsr &= ~0xFFFF0000;
		dcb_dhcsr |= CM3_DCB_DHCSR_DBGKEY | CM3_DCB_DHCSR_C_DEBUGEN | 
						CM3_DCB_DHCSR_C_HALT;
		adi_memap_write_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 0);
		
		if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
	}
	
	// wait halt
	wait_halt_delay_in_10ms = 100;	// 1000ms max delay in all
	while ((!(dcb_dhcsr & CM3_DCB_DHCSR_S_HALT)) && wait_halt_delay_in_10ms)
	{
		if (ERROR_OK != adi_memap_read_reg32(CM3_DCB_DHCSR, &dcb_dhcsr, 1))
		{
			return ERROR_FAIL;
		}
		dcb_dhcsr = LE_TO_SYS_U32(dcb_dhcsr);
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

RESULT cm3_dump(uint32_t addr, uint32_t size)
{
	uint32_t reg;
	uint8_t i;
	uint8_t *buffer;
	RESULT ret = ERROR_OK;
	
	buffer = (uint8_t *)malloc(size);
	if (NULL == buffer)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto end;
	}
	
	LOG_INFO("report to author on this message.");
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt cm3");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	
	for (i = 0; i < 13; i++)
	{
		if (ERROR_OK != cm3_read_core_register(i, &reg))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read register");
			ret = ERRCODE_FAILURE_OPERATION;
			goto end;
		}
		reg = LE_TO_SYS_U32(reg);
		LOG_INFO("r%d: %08X", i, reg);
	}
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_SP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read sp");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "sp", reg);
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_LR, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read lr");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "lr", reg);
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read pc");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "pc", reg);
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_XPSR, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read xpsr");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "xpsr", reg);
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_MSP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read msp");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "msp", reg);
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_PSP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read psp");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_08X, "psp", reg);
	
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_20, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read core_reg 20");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	reg = LE_TO_SYS_U32(reg);
	LOG_INFO(INFOMSG_REG_02X, "primask", (reg >> 0) & 0xFF);
	LOG_INFO(INFOMSG_REG_02X, "basepri", (reg >> 8) & 0xFF);
	LOG_INFO(INFOMSG_REG_02X, "faultmask", (reg >> 16) & 0xFF);
	LOG_INFO(INFOMSG_REG_02X, "control", (reg >> 24) & 0xFF);
	
	LOG_INFO("SRAM dump at 0x%08X:", addr);
	if (ERROR_OK != adi_memap_read_buf(addr, buffer, size))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read sram");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_BYTE_BUF(buffer, size, LOG_INFO, "%02X", 16);
	
end:
	if (buffer != NULL)
	{
		free(buffer);
		buffer = NULL;
	}
	
	return ret;
}


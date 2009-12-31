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

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "msp430.h"
#include "JTAGfunc.h"
#include "msp430_internal.h"

RESULT msp430_jtag_program(struct operation_t operations, 
					struct program_info_t *pi, struct programmer_info_t *prog)
{
	uint16_t chip_id;
	uint8_t tmp8;
	
	uint8_t erased = 0;
	uint8_t ir;
	uint32_t dr;
	int32_t i;
	uint32_t k, len_current_list;
	word page_size, addr_start;
	RESULT ret = ERROR_OK;
	word CRC_check, CRC_calc;
	uint32_t target_size;
	uint8_t *tbuff;
	struct memlist **ml, *ml_tmp;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	// here we go
	msp430_jtag_init();
	if (DeviceHas_TestPin())
	{
		// TEST pin
		msp430_jtag_config(1);
	}
	else
	{
		// no TEST pin
		msp430_jtag_config(0);
	}
	
	for (i = 10; i > 0; i--)
	{
		ResetTAP();
		// read ir return value, should be 0x89(MSP430_JTAG_ID)
		IR_Shift_Read(IR_BYPASS, &tmp8);
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "init chip");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		if (MSP430_JTAG_ID == tmp8)
		{
			break;
		}
		else
		{
			// re-init
			msp430_jtag_fini();
			msp430_jtag_init();
			if (DeviceHas_TestPin())
			{
				// TEST pin
				msp430_jtag_config(1);
			}
			else
			{
				// no TEST pin
				msp430_jtag_config(0);
			}
		}
	}
	if (0 == i)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "detect target chip");
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	
	ResetTAP();
	// check fuse blown
	for (i = 3; i > 0; i--)
	{
		IR_Shift(IR_CNTRL_SIG_CAPTURE);
		DR_Shift16_Read(0xAAAA, &dr);
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "init programming");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		if (0x5555 == dr)
		{
			LOG_ERROR(_GETTEXT("fuse of current chip is blown\n"));
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
	}
	
	// read chip_id
	IR_Shift(IR_CNTRL_SIG_16BIT);
	DR_Shift16(0x2401);
	IR_Shift(IR_CNTRL_SIG_CAPTURE);
	// wait until CPU is synchronized
	msp430_jtag_dr16_poll(0x0000, 0x0200, 0x0200, 50, 0);
	// read chip_id in 0x0FF0
	ReadMem(F_WORD, 0x0FF0, &chip_id);
	// perform PUC, includes target watchdog disable
	ExecutePOR();
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = ((chip_id << 8) + (chip_id >> 8)) & 0x0000FFFF;
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_CHIP_ID), pi->chip_id);
	if (!(operations.read_operations & CHIPID))
	{
		if (pi->chip_id != target_chip_param.chip_id)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHIP_ID), pi->chip_id, 
						target_chip_param.chip_id);
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
	}
	else
	{
		goto leave_program_mode;
	}
	
	page_size = (word)target_chip_param.chip_areas[APPLICATION_IDX].page_size;
	addr_start = Device_MainStart();
	
	if (operations.erase_operations > 0)
	{
		// chip erase
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, '=');
		
		// erase
		if (DeviceHas_CpuX())
		{
			// Global-Erase Flash
			// (for all devices with CPU-X)
			EraseFLASH(ERASE_GLOB, 0xFE00);
		}
		else
		{
			// Mass-Erase Flash (all types)
			EraseFLASH(ERASE_MASS, 0xFE00);
			// NOTE: the INFO memory in F2xx device will be not erased,
			// if the memory still locked.
			// For more info See EraseFLASH() in JTAGfunc.c
		}
		
		if (ERROR_OK != commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
		
		// check blank
		LOG_INFO(_GETTEXT(INFOMSG_CHECKING), "blank");
		pgbar_init("checking blank |", "|", 0, 
					target_chip_param.chip_areas[APPLICATION_IDX].page_num, 
					PROGRESS_STEP, '=');
		
		for (i = 0; 
			i < (int32_t)target_chip_param.chip_areas[APPLICATION_IDX].page_num; 
			i++)
		{
			CRC_calc = CRC_check = 0;
			CRC_calc = EraseCheck((word)(addr_start + i * page_size), 
								  page_size / 2, &CRC_check);
			
			if (ERROR_OK != commit())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read crc check");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
			if (CRC_calc != CRC_check)
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							"check blank", addr_start + i * page_size);
				ret = ERRCODE_FAILURE_OPERATION_ADDR;
				goto leave_program_mode;
			}
			
			pgbar_update(1);
		}
		
		erased = 1;
		pgbar_fini();
		LOG_INFO("blank checked\n");
	}
	
	ml = &pi->program_areas[APPLICATION_IDX].memlist;
	target_size = MEMLIST_CalcAllSize(*ml);
	tbuff = pi->program_areas[APPLICATION_IDX].buff;
	if (operations.write_operations & APPLICATION)
	{
		// program flash
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
						- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				WriteFLASH((word)(ml_tmp->addr + i), page_size / 2, 
						   (word*)(tbuff + ml_tmp->addr + i - addr_start));
				if (ERROR_OK != commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							 "write flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION_ADDR;
					goto leave_program_mode;
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", target_size);
	}
	
	if ((operations.read_operations & APPLICATION) 
		|| (operations.verify_operations & APPLICATION))
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), "read msp430 flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		pgbar_init("reading flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
						- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				CRC_calc = CRC_check = 0;
				CRC_calc = VerifyMem((word)(ml_tmp->addr + i), page_size / 2, 
					(word*)(tbuff + ml_tmp->addr + i - addr_start), &CRC_check);
				if (ERROR_OK != commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								"read crc check");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				if (CRC_calc != CRC_check)
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							 "verify flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION_ADDR;
					goto leave_program_mode;
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
		}
		
		pgbar_fini();
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash", target_size);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "flash");
		}
	}
	
leave_program_mode:
	// leave program mode
	ReleaseDevice(V_RESET);
	msp430_jtag_fini();
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "exit program mode");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}


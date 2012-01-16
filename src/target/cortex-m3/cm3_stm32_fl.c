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

#include "compiler.h"
#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"
#include "target.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "cm3_stm32_fl.h"

#if SYS_CFG_HAS_TIME_H
#include <time.h>
#endif

#define STM32_FL_COMMAND_OFFSET				0x80
#define STM32_FL_SYNC_OFFSET				0xA4

#define STM32_FL_DATATYPE_0					(uint32_t)40
#define STM32_FL_DATATYPE_BYTE				(uint32_t)-2
#define STM32_FL_DATATYPE_HWORD				(uint32_t)8
#define STM32_FL_DATATYPE_WORD				(uint32_t)18
#define STM32_FL_DATATYPE_DWORD				(uint32_t)28

static uint32_t stm32swj_fl_cnt = 0;
static uint32_t stm32swj_fl_base = 0;
static uint8_t fl_code[] = {
								/* wait_start: */
	0x28, 0x48,					/* ldr.n	r0, [PC, #0xA0]	;load SYNC */
	0x00, 0x28,					/* cmp		r0, #0 */
	0xFC, 0xD0,					/* beq.n	wait_start */
								/* update command: */
	0x1E, 0x48,					/* ldr.n	r0, [PC, #0x78]	;load CR_ADDR */
	0x1E, 0x49,					/* ldr.n	r1, [PC, #0x78] ;load CR_VALUE1 */
	0x1F, 0x4A,					/* ldr.n	r2, [PC, #0x7C] ;load CR_VALUE2 */
								/* write_cr: */
	0x01, 0x60,					/* str		r1, [r0]		;write CR */
	0x12, 0x42,					/* tst		r2, r2 */
	0x00, 0xD0,					/* beq.n	load_parameter */
	0x02, 0x60,					/* str		r2, [r0] */
								/* load_parameter: */
	0x1D, 0x48,					/* ldr.n	r0, [PC, #0x74] ;load SR_ADDR */
	0x1E, 0x49,					/* ldr.n	r1, [PC, #0x78] ;load SR_BUSY_MASK */
	0x1E, 0x4A,					/* ldr.n	r2, [PC, #0x78] ;load TARGET_ADDR */
	0x1F, 0x4B,					/* ldr.n	r3, [PC, #0x7C] ;load RAM_ADDR */
	0x1F, 0x4C,					/* ldr.n	r4, [PC, #0x7C] ;load DATA_TYPE */
	0x20, 0x4D,					/* ldr.n	r5, [PC, #0x80] ;load DATA_SIZE */
								/* clear_sync: */
	0x00, 0x26,					/* movs		r6, #0 */
	0x20, 0xA7,					/* adr.n	r7, PC, #0x80 */
	0x3E, 0x60,					/* str		r6, [r7] */
								/* check_data_size */
	0x2D, 0x42,					/* tst		r5, r5 */
	0xEA, 0xD0,					/* beq.n	wait_start */
								/* write_data: */
	0xA7, 0x44,					/* add		pc, pc, r4 */
								/* write_byte:				;offset:-2 */
	0x1F, 0x78,					/* ldrb		r7, [r3] */
	0x17, 0x70,					/* strb		r7, [r2] */
	0x01, 0x32,					/* adds		r2, #1 */
	0x01, 0x33,					/* adds		r3, #1 */
	0x0F, 0xE0,					/* b.n		wait_busy */
								/* write_hword:				;offset:8 */
	0x1F, 0x88,					/* ldrh		r7, [r3] */
	0x17, 0x80,					/* str		r7, [r2] */
	0x02, 0x32,					/* adds		r2, #1 */
	0x02, 0x33,					/* adds		r3, #1 */
	0x0A, 0xE0,					/* b.n		wait_busy */
								/* write_word:				;offset:18 */
	0x1F, 0x68,					/* ldrh		r7, [r3] */
	0x17, 0x60,					/* str		r7, [r2] */
	0x04, 0x32,					/* adds		r2, #1 */
	0x04, 0x33,					/* adds		r3, #1 */
	0x05, 0xE0,					/* b.n		wait_busy */
								/* write_dword:				;offset:28 */
	0x1F, 0x68,					/* ldr		r7, [r3] */
	0x17, 0x60,					/* str		r7, [r2] */
	0x5F, 0x68,					/* ldr		r7, [r3, #4] */
	0x57, 0x60,					/* str		r7, [r2, #4] */
	0x08, 0x32,					/* adds		r2, #1 */
	0x08, 0x33,					/* adds		r3, #1 */
								/* wait_busy:				;offset:40 */
	0x07, 0x68,					/* ldr		r7, [r0] */
	0x0F, 0x40,					/* ands		r7, r7, r1 */
	0xFC, 0xD1,					/* bne.n	wait_busy */
								/* check_all_written: */
	0x6D, 0x1E,					/* subs		r5, r5, #1 */
	0xE4, 0xD1,					/* bne.n	write_data */
								/* increase_result */
	0x11, 0x48,					/* ldr.n	r0, [PC, #0x44] */
	0x40, 0x1C,					/* adds		r0, r0, #1 */
	0x10, 0xA1,					/* adr.n	r1, PC, #0x40 */
	0x08, 0x60,					/* str		r0, [r1] */
	0xCA, 0xE7,					/* b.n		wait_start */
								/* exit: */
								/* dead_loop: */
	0xFE, 0xE7,					/* b $ */
	
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
								/* offset:0x100 */
	0x00, 0x00, 0x00, 0x00,		/* cr_addr */
	0x00, 0x00, 0x00, 0x00,		/* cr_value1 */
	0x00, 0x00, 0x00, 0x00,		/* cr_value2 */
	0x00, 0x00, 0x00, 0x00,		/* sr_addr */
	0x00, 0x00, 0x00, 0x00,		/* sr_busy_mask */
	0x00, 0x00, 0x00, 0x00,		/* target_addr */
	0x00, 0x00, 0x00, 0x00,		/* ram_addr */
	0x00, 0x00, 0x00, 0x00,		/* data_type */
	0x00, 0x00, 0x00, 0x00,		/* data_size */
	
	0x00, 0x00, 0x00, 0x00,		/* sync */
	
	0x00, 0x00, 0x00, 0x00,		/* result */
};

vsf_err_t stm32swj_fl_init(uint32_t fl_base)
{
	uint32_t reg;
	uint8_t verify_buff[sizeof(fl_code)];
	
	stm32swj_fl_base = fl_base;
	// download flash_loader
	if (cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt stm32");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write code to target SRAM
	if (adi_memap_write_buf(stm32swj_fl_base, fl_code, sizeof(fl_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load flash_loader to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify fl_code
	memset(verify_buff, 0, sizeof(fl_code));
	if (adi_memap_read_buf(stm32swj_fl_base, verify_buff, sizeof(fl_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (memcmp(verify_buff, fl_code, sizeof(fl_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "verify flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	reg = stm32swj_fl_base + 1;
	if (cm3_write_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (cm3_dp_resume())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	stm32swj_fl_cnt = 0;
	return VSFERR_NONE;
}

vsf_err_t stm32swj_fl_run(struct stm32_fl_cmd_t *cmd)
{
	uint32_t buff_tmp[10];
	
	buff_tmp[0] = SYS_TO_LE_U32(cmd->cr_addr);
	buff_tmp[1] = SYS_TO_LE_U32(cmd->cr_value1);
	buff_tmp[2] = SYS_TO_LE_U32(cmd->cr_value2);
	buff_tmp[3] = SYS_TO_LE_U32(cmd->sr_addr);
	buff_tmp[4] = SYS_TO_LE_U32(cmd->sr_busy_mask);
	buff_tmp[5] = SYS_TO_LE_U32(cmd->target_addr);
	buff_tmp[6] = SYS_TO_LE_U32(cmd->ram_addr);
	switch (cmd->data_type)
	{
	case 0:
		buff_tmp[7] = STM32_FL_DATATYPE_0;
		break;
	case 1:
		buff_tmp[7] = STM32_FL_DATATYPE_BYTE;
		break;
	case 2:
		buff_tmp[7] = STM32_FL_DATATYPE_HWORD;
		break;
	case 4:
		buff_tmp[7] = STM32_FL_DATATYPE_WORD;
		break;
	case 8:
		buff_tmp[7] = STM32_FL_DATATYPE_DWORD;
		break;
	default:
		return VSFERR_FAIL;
	}
	buff_tmp[8] = SYS_TO_LE_U32(cmd->data_size);
	buff_tmp[9] = SYS_TO_LE_U32(1);
	
	// write fl command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (adi_memap_write_buf(stm32swj_fl_base + STM32_FL_COMMAND_OFFSET,
							(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load flashloader cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	stm32swj_fl_cnt++;
	
	return VSFERR_NONE;
}

vsf_err_t stm32swj_fl_poll_result(struct stm32_fl_result_t *result)
{
	uint32_t buff_tmp[2];
	uint8_t i;
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (adi_memap_read_buf(stm32swj_fl_base + STM32_FL_SYNC_OFFSET,
							(uint8_t *)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read flashloader sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	for (i = 0; i < dimof(buff_tmp); i++)
	{
		buff_tmp[i] = LE_TO_SYS_U32(buff_tmp[i]);
	}
	
	if (0 == buff_tmp[0])
	{
		result->result = buff_tmp[1];
		return VSFERR_NONE;
	}
	
	return VSFERR_NOT_READY;
}

vsf_err_t stm32swj_fl_wait_ready(struct stm32_fl_result_t *result, bool last)
{
	vsf_err_t err;
	uint32_t start, end;
	
#if SYS_CFG_HAS_TIME_H
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
#else
	interfaces->tickclk.init();
	interfaces->tickclk.start();
	end = start = 0;
#endif
	while (1)
	{
		err = stm32swj_fl_poll_result(result);
		if (!err && (!last || (result->result == stm32swj_fl_cnt)))
		{
			break;
		}
		if (err && (err != VSFERR_NOT_READY))
		{
#if !SYS_CFG_HAS_TIME_H
			interfaces->tickclk.stop();
			interfaces->tickclk.fini();
#endif
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll flashloader result");
			return VSFERR_FAIL;
		}
#if SYS_CFG_HAS_TIME_H
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
#else
		if (interfaces->tickclk.is_trigger())
		{
			end++;
		}
#endif
		// wait 20s at most
		if ((end - start) > 20000)
		{
#if !SYS_CFG_HAS_TIME_H
			interfaces->tickclk.stop();
			interfaces->tickclk.fini();
#endif
			cm3_dump(stm32swj_fl_base, sizeof(fl_code));
			LOG_ERROR(ERRMSG_TIMEOUT, "wait for flashloader ready");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
#if !SYS_CFG_HAS_TIME_H
	interfaces->tickclk.stop();
	interfaces->tickclk.fini();
#endif
	
	return VSFERR_NONE;
}

vsf_err_t stm32swj_fl_call(struct stm32_fl_cmd_t *cmd,
							struct stm32_fl_result_t *result, bool last)
{
	if (stm32swj_fl_run(cmd) || stm32swj_fl_wait_ready(result, last))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run flashloader command");
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}


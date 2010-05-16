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

#include "c8051f.h"
#include "c8051f_internal.h"

#define CUR_TARGET_STRING		C8051F_STRING

RESULT c8051fc2_enter_program_mode(struct program_context_t *context);
RESULT c8051fc2_leave_program_mode(struct program_context_t *context, 
								uint8_t success);
RESULT c8051fc2_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size);
RESULT c8051fc2_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
RESULT c8051fc2_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
struct program_functions_t c8051fc2_program_functions = 
{
	NULL,			// execute
	c8051fc2_enter_program_mode, 
	c8051fc2_leave_program_mode, 
	c8051fc2_erase_target, 
	c8051fc2_write_target, 
	c8051fc2_read_target
};


#define delay_ms(ms)			p->delayms((ms) | 0x8000)
#define delay_us(us)			p->delayus((us) & 0x7FFF)

#define c2_init()				p->c2_init()
#define c2_fini()				p->c2_fini()
#define c2_write_ir(ir)			p->c2_addr_write(ir)
#define c2_read_ir(ir)			p->c2_addr_read(ir);
#define c2_write_dr(dr)			p->c2_data_write(&dr, 1)
#define c2_read_dr(dr)			p->c2_data_read(dr, 1)
#define c2_poll_out_ready()		c8051f_c2_addr_poll(0x01, 0x01, 500)
#define c2_poll_in_busy()		c8051f_c2_addr_poll(0x02, 0x00, 500)

#define poll_start(cnt)			p->poll_start((cnt), 100)
#define poll_end()				p->poll_end()
#define poll_check(o, m, v)		p->poll_checkbyte((o), (m), (v))

#define commit()				p->peripheral_commit()

static struct programmer_info_t *p = NULL;

RESULT c8051f_c2_addr_poll(uint8_t mask, uint8_t value, uint16_t poll_cnt)
{
	poll_start(poll_cnt * 10);
	
	c2_read_ir(NULL);
	poll_check(0, mask, value);
	
	poll_end();
	
	return ERROR_OK;
}

RESULT c8051fc2_enter_program_mode(struct program_context_t *context)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	
	p = context->prog;
	c2_init();
	// enable flash programming
	c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPCTL_ADDR]);
	dr = 0x02;
	c2_write_dr(dr);
	dr = 0x01;
	c2_write_dr(dr);
	delay_ms(30);
	return commit();
}

RESULT c8051fc2_leave_program_mode(struct program_context_t *context, 
								uint8_t success)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	c2_fini();
	return commit();
}

RESULT c8051fc2_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(page_size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPDAT_ADDR]);
		dr = C8051F_C2_CMD_DEVICE_ERASE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		dr = 0xDE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0xAD;
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0xA5;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT c8051fc2_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	uint32_t i;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPDAT_ADDR]);
		
		dr = C8051F_C2_CMD_BLOCK_WRITE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
					  "program flash", addr);
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// write address high byte
		dr = (uint8_t)(addr >> 8);
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		// write address low byte
		dr = (uint8_t)(addr & 0xFF);
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
					  "program flash", addr);
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		for (i = 0; i < page_size; i++)
		{
			c2_write_dr(buff[i]);
			c2_poll_in_busy();
		}
		
		c2_poll_out_ready();
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
					  "program flash", addr);
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT c8051fc2_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	struct chip_param_t *param = context->param;
	uint8_t dr;
	uint32_t i;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case CHIPID_CHAR:
		c2_write_ir(C8051F_C2_DEVICEID);
		c2_read_dr(&dr);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint8_t *)buff = dr;
		break;
	case APPLICATION_CHAR:
		c2_write_ir((uint8_t)param->param[C8051F_PARAM_FPDAT_ADDR]);
		
		dr = C8051F_C2_CMD_BLOCK_READ;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
					  "read flash", addr);
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// write address high byte
		dr = (uint8_t)(addr >> 8);
		c2_write_dr(dr);
		c2_poll_in_busy();
		// write address low byte
		dr = (uint8_t)(addr & 0xFF);
		c2_write_dr(dr);
		c2_poll_in_busy();
		dr = 0;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		for (i = 0; i < page_size; i++)
		{
//			c2_poll_out_ready();
			c2_read_dr(&buff[i]);
		}
		
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}


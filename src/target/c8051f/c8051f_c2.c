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
#define cur_chip_param			target_chip_param


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

#define commit()				p->c2_commit()

static struct programmer_info_t *p = NULL;

RESULT c8051f_c2_addr_poll(uint8_t mask, uint8_t value, uint16_t poll_cnt)
{
	poll_start(poll_cnt * 10);
	
	c2_read_ir(NULL);
	poll_check(0, mask, value);
	
	poll_end();
	
	return ERROR_OK;
}

RESULT c8051f_c2_program(struct operation_t operations, 
					struct program_info_t *pi, struct programmer_info_t *prog)
{
	uint8_t dr;
	uint16_t i;
	uint16_t j, k, page_size, len_current_list;
	RESULT ret = ERROR_OK;
	uint8_t page_buf[C8051F_BLOCK_SIZE];
	uint8_t *tbuff;
	struct memlist *ml_tmp;
	uint32_t target_size;
	struct memlist **ml;
	
	p = prog;
	
	c2_init();
	
	c2_write_ir(C8051F_C2_DEVICEID);
	c2_read_dr(&dr);
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = dr;
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_CHIP_ID), pi->chip_id);
	if (!(operations.read_operations & CHIPID))
	{
		if (pi->chip_id != cur_chip_param.chip_id)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHIP_ID), pi->chip_id, 
						cur_chip_param.chip_id);
			ret = ERRCODE_INVALID_CHIP_ID;
			goto leave_program_mode;
		}
	}
	else
	{
		goto leave_program_mode;
	}
	
	// enable flash programming
	c2_write_ir((uint8_t)cur_chip_param.param[C8051F_PARAM_FPCTL_ADDR]);
	dr = 0x02;
	c2_write_dr(dr);
	dr = 0x01;
	c2_write_dr(dr);
	delay_ms(30);
	
	if (operations.erase_operations > 0)
	{
		// erase
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "flash");
		pgbar_init("erasing flash |", "|", 0, 1, PROGRESS_STEP, '=');
		
		c2_write_ir((uint8_t)cur_chip_param.param[C8051F_PARAM_FPDAT_ADDR]);
		dr = C8051F_C2_CMD_DEVICE_ERASE;
		c2_write_dr(dr);
		c2_poll_in_busy();
		
		c2_poll_out_ready();
		c2_read_dr(&dr);
		if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
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
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "flash");
	}
	
	page_size = C8051F_BLOCK_SIZE;
	tbuff = pi->program_areas[APPLICATION_IDX].buff;
	ml = &pi->program_areas[APPLICATION_IDX].memlist;
	target_size = MEMLIST_CalcAllSize(*ml);
	if (operations.write_operations & APPLICATION)
	{
		// program
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = (uint16_t)ml_tmp->len;
			}
			else
			{
				k = page_size - (uint16_t)(ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint16_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
							- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				c2_write_ir(
					(uint8_t)cur_chip_param.param[C8051F_PARAM_FPDAT_ADDR]);
				
				dr = C8051F_C2_CMD_BLOCK_WRITE;
				c2_write_dr(dr);
				c2_poll_in_busy();
				
				c2_poll_out_ready();
				c2_read_dr(&dr);
				if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "program flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				// write address high byte
				dr = (uint8_t)((ml_tmp->addr + i) >> 8);
				c2_write_dr(dr);
				c2_poll_in_busy();
				
				// write address low byte
				dr = (uint8_t)((ml_tmp->addr + i) & 0xFF);
				c2_write_dr(dr);
				c2_poll_in_busy();
				dr = 0;
				c2_write_dr(dr);
				c2_poll_in_busy();
				
				c2_poll_out_ready();
				c2_read_dr(&dr);
				if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "program flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				for (j = 0; j < page_size; j++)
				{
					c2_write_dr(tbuff[ml_tmp->addr + i + j]);
					c2_poll_in_busy();
				}
				
				c2_poll_out_ready();
				if (ERROR_OK != commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "program flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
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
			ret = MEMLIST_Add(ml, 0, pi->program_areas[APPLICATION_IDX].size, 
								page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(*ml);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = (uint16_t)ml_tmp->len;
			}
			else
			{
				k = page_size - (uint16_t)(ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint16_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
							- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				c2_write_ir(
					(uint8_t)cur_chip_param.param[C8051F_PARAM_FPDAT_ADDR]);
				
				dr = C8051F_C2_CMD_BLOCK_READ;
				c2_write_dr(dr);
				c2_poll_in_busy();
				
				c2_poll_out_ready();
				c2_read_dr(&dr);
				if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "read flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				// write address high byte
				dr = (uint8_t)((ml_tmp->addr + i) >> 8);
				c2_write_dr(dr);
				c2_poll_in_busy();
				// write address low byte
				dr = (uint8_t)((ml_tmp->addr + i) & 0xFF);
				c2_write_dr(dr);
				c2_poll_in_busy();
				dr = 0;
				c2_write_dr(dr);
				c2_poll_in_busy();
				
				c2_poll_out_ready();
				c2_read_dr(&dr);
				if ((ERROR_OK != commit()) || (dr != C8051F_C2_REP_COMMAND_OK))
				{
					if (((ml_tmp->addr + i + 512) 
							== pi->program_areas[APPLICATION_IDX].size) 
						|| ((ml_tmp->addr + i + 1024) 
							== pi->program_areas[APPLICATION_IDX].size))
					{
						// protect flash of ISP, not available
						pgbar_update(pi->program_areas[APPLICATION_IDX].size 
										- ml_tmp->addr - i);
						pi->program_areas[APPLICATION_IDX].size = 
															ml_tmp->addr + i;
						goto fake_read_ok;
					}
					else
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "read flash", ml_tmp->addr + i);
						ret = ERRCODE_FAILURE_OPERATION;
						goto leave_program_mode;
					}
				}
				
				for (j = 0; j < page_size; j++)
				{
//					c2_poll_out_ready();
					c2_read_dr(&page_buf[j]);
				}
				
				if (ERROR_OK != commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "read flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				if (operations.verify_operations & APPLICATION)
				{
					// verify
					for (j = 0; j < page_size; j++)
					{
						if (tbuff[ml_tmp->addr + i + j] != page_buf[j])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"flash", ml_tmp->addr + i + j, page_buf[j], 
								tbuff[ml_tmp->addr + i + j]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
				}
				else
				{
					// read
					memcpy(&tbuff[ml_tmp->addr + i], page_buf, page_size);
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
fake_read_ok:
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
	c2_fini();
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "exit program mode");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}


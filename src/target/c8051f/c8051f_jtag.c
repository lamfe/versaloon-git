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
#define cur_chip_param			c8051f_chip_param

static programmer_info_t *p = NULL;

#if 0
static uint16 dr0 = 0, dr_read = C8051F_INDOPTCODE_READ;
static uint16 ir_flashdat = C8051F_IR_FLASHDAT | C8051F_IR_STATECNTL_SUSPEND;
#endif


#define jtag_init()				p->jtag_hl_init()
#define jtag_fini()				p->jtag_hl_fini()
#define jtag_config(kHz)		p->jtag_hl_config((kHz), 0, 0, 0, 0)
#define jtag_tms(tms, len)		p->jtag_hl_tms((tms), (len))
#define jtag_ir_write(ir, len)	p->jtag_hl_ir((uint8*)(ir), (len), 1, 0)
#define jtag_dr_write(dr, len)	p->jtag_hl_dr((uint8*)(dr), (len), 1, 0)
#define jtag_dr_read(dr, len)	p->jtag_hl_dr((uint8*)(dr), (len), 1, 1)
#define jtag_poll_dr1(dr, poll_count)	\
								p->jtag_hl_poll(NULL, 0, 0, (uint8*)dr, 1, \
												1, 0, 1, 0, poll_count)

#if 1
#define jtag_poll_busy()		p->jtag_hl_delay_us(20)
#else
#define jtag_poll_busy()		p->jtag_hl_poll(NULL, 0, 0, \
												(uint8*)&dr0, 1, 1, \
												NULL, 0, 0, \
												NULL, 0, 0, \
												NULL, 0, 0, \
												NULL, 0, 0, \
												0, 0x01, 0x00, \
												C8051F_MAX_POLL_COUNT)
#endif

#if 1
#define jtag_poll_flbusy(dly)	jtag_delay_us((dly))
#else
#define jtag_poll_flbusy()		p->jtag_hl_poll((uint8*)&ir_flashdat, 16, 1, \
												(uint8*)&dr_read, 2, 1, \
												NULL, 0, 0, \
												(uint8*)&dr0, 1, 1, \
												NULL, 0, 0, \
												(uint8*)&dr0, 2, 1, \
												0, 0x02, 0x00, \
												C8051F_MAX_POLL_COUNT)
#endif
#define jtag_delay_us(us)		p->jtag_hl_delay_us((us))
#define jtag_delay_ms(ms)		p->jtag_hl_delay_ms((ms))
#define jtag_commit()			p->jtag_hl_commit()

RESULT c8051f_jtag_ind_read(uint8 addr, uint32 *value, uint8 num_bits)
{
	static uint16 ir, dr;

#ifdef PARAM_CHECK
	if ((addr < C8051F_IR_FLASHCON) || (addr > C8051F_IR_FLASHSCL))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_ADDRESS), addr, CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
#endif
	
	ir = addr | C8051F_IR_STATECNTL_SUSPEND;
	jtag_ir_write(&ir, C8051F_IR_LEN);
	
	dr = C8051F_INDOPTCODE_READ;
	jtag_dr_write(&dr, 2);
	
	dr = 0;
	jtag_poll_busy();
	
	jtag_dr_read(value, num_bits + 1);
	
	return ERROR_OK;
}

RESULT c8051f_jtag_ind_write(uint8 addr, uint32 *value, uint8 num_bits)
{
	static uint16 ir, dr;
	
#ifdef PARAM_CHECK
	if ((addr < C8051F_IR_FLASHCON) || (addr > C8051F_IR_FLASHSCL))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_ADDRESS), addr, CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
#endif
	
	ir = addr | C8051F_IR_STATECNTL_SUSPEND;
	jtag_ir_write(&ir, C8051F_IR_LEN);
	
	*value |= C8051F_INDOPTCODE_WRITE << num_bits;
	jtag_dr_write(value, num_bits + 2);
	
	dr = 0;
	jtag_poll_busy();
	
	return ERROR_OK;
}

RESULT c8051f_jtag_program(operation_t operations, program_info_t *pi, 
						   programmer_info_t *prog)
{
	uint16 ir;
	uint32 dr;
	
	int32 i;
	uint32 j, k, len_current_list, page_size;
	uint32 page_buf[C8051F_BLOCK_SIZE];
	RESULT ret = ERROR_OK;
	memlist *ml_tmp;
	
	p = prog;
	
	jtag_init();
	jtag_config(4500);
	
	ir = C8051F_IR_STATECNTL_RESET | C8051F_IR_BYPASS;
	jtag_ir_write(&ir, C8051F_IR_LEN);
	ir = C8051F_IR_STATECNTL_HALT | C8051F_IR_IDCODE;
	jtag_ir_write(&ir, C8051F_IR_LEN);
	dr = 0;
	jtag_dr_read(&dr, C8051F_DR_IDCODE_LEN);
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = dr & C8051F_JTAG_ID_MASK;
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_CHIP_ID), pi->chip_id);
	if (!(operations.read_operations & CHIP_ID))
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
	
	// set FLASHSCL based on SYSCLK (2MHx = 0x86)
	dr = 0x86;
	c8051f_jtag_ind_write(C8051F_IR_FLASHSCL, &dr, C8051F_DR_FLASHSCL_LEN);
	
	if ((operations.erase_operations & ALL) || (0 == pi->app_size_valid))
	{
		// erase all flash
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "flash");
		pgbar_init("erasing flash |", "|", 0, 1, PROGRESS_STEP, '=');
		
		// set FLASHADR to erase_addr
		dr = cur_chip_param.jtag_param.erase_addr;
		c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr, 
							  C8051F_DR_FLASHADR_LEN);
		// set FLASHCON for flash erase operation(0x20)
		dr = 0x20;
		c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
							  C8051F_DR_FLASHCON_LEN);
		// set FLASHDAT to 0xA5
		dr = 0xA5;
		c8051f_jtag_ind_write(C8051F_IR_FLASHDAT, &dr, 
							  C8051F_DR_FLASHDAT_WLEN);
		// set FLASHCON for poll operation
		dr = 0;
		c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
							  C8051F_DR_FLASHCON_LEN);
		// 3 times 500 ms is 1.5s
		jtag_delay_ms(500);
		if (ERROR_OK != jtag_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		jtag_delay_ms(500);
		if (ERROR_OK != jtag_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		jtag_delay_ms(500);
		if (ERROR_OK != jtag_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		// read FLBusy and FLFail
		c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, &dr, 2);
		
		if ((ERROR_OK != jtag_commit()) || (dr & 0x02))
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
	else if (operations.erase_operations & APPLICATION)
	{
		// erase according to flash size
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "flash");
		pgbar_init("erasing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		for (i = 0; 
			 i < (int32)pi->app_size_valid; 
			 i += cur_chip_param.flash_page_size)
		{
			// set FLASHADR to erase_addr
			dr = i;
			c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr, 
								  C8051F_DR_FLASHADR_LEN);
			// set FLASHCON for flash erase operation(0x20)
			dr = 0x20;
			c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
								  C8051F_DR_FLASHCON_LEN);
			// set FLASHDAT to 0xA5
			dr = 0xA5;
			c8051f_jtag_ind_write(C8051F_IR_FLASHDAT, &dr, 
								  C8051F_DR_FLASHDAT_WLEN);
			// set FLASHCON for poll operation
			dr = 0;
			c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
								  C8051F_DR_FLASHCON_LEN);
			// poll for FLBusy
			jtag_poll_flbusy(20000);
			// read FLBusy and FLFail
			c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, &dr, 2);
			
			if ((ERROR_OK != jtag_commit()) || (dr & 0x06))
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase flash");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
			
			pgbar_update(cur_chip_param.flash_page_size);
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "flash");
	}
	
	page_size = C8051F_BLOCK_SIZE;
	
	if (operations.write_operations & APPLICATION)
	{
		// program flash
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = pi->app_memlist;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = (uint16)ml_tmp->len;
			}
			else
			{
				k = page_size - (uint16)(ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint16)ml_tmp->len;
			for (i = -(int32)(ml_tmp->addr % page_size); 
				 i < ((int32)ml_tmp->len - (int32)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				// set FLASHADR to address to write to
				dr = ml_tmp->addr + i;
				c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr, 
									  C8051F_DR_FLASHADR_LEN);
				
				for (j = 0; j < page_size; j++)
				{
					// set FLASHCON for flash write operation(0x10)
					dr = 0x10;
					c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
										  C8051F_DR_FLASHCON_LEN);
					// initiate the write operation
					dr = pi->app[ml_tmp->addr + i + j];
					c8051f_jtag_ind_write(C8051F_IR_FLASHDAT, &dr, 
										  C8051F_DR_FLASHDAT_WLEN);
					// set FLASHCON for poll operation
					dr = 0;
					c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
										  C8051F_DR_FLASHCON_LEN);
					// poll for FLBusy
					jtag_poll_flbusy(60);
					c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, 
										 ((uint32*)page_buf + j), 2);
				}
				
				if (ERROR_OK != jtag_commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "program flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION_ADDR;
					goto leave_program_mode;
				}
				
				for (j = 0; j < page_size; j++)
				{
					if ((*((uint32*)page_buf + j) & 0x06) != 0)
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "program flash", ml_tmp->addr + i + j);
						ret = ERRCODE_FAILURE_OPERATION;
						goto leave_program_mode;
					}
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
			
			ml_tmp = ml_tmp->next;
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", 
				 pi->app_size_valid);
	}
	
	if (operations.read_operations & APPLICATION)
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			pi->app_size_valid = cur_chip_param.flash_size;
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = pi->app_memlist;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = (uint16)ml_tmp->len;
			}
			else
			{
				k = page_size - (uint16)(ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint16)ml_tmp->len;
			for (i = -(int32)(ml_tmp->addr % page_size); 
				 i < ((int32)ml_tmp->len - (int32)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				// set FLASHADR to address to read from
				dr = ml_tmp->addr + i;
				c8051f_jtag_ind_write(C8051F_IR_FLASHADR, &dr, 
									  C8051F_DR_FLASHADR_LEN);
				
				for (j = 0; j < page_size; j++)
				{
					// set FLASHCON for flash read operation(0x02)
					dr = 0x02;
					c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
										  C8051F_DR_FLASHCON_LEN);
					// initiate the read operation
					dr = 0;
					c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, &dr, 
										 C8051F_DR_FLASHDAT_RLEN);
					// set FLASHCON for poll operation
					dr = 0;
					c8051f_jtag_ind_write(C8051F_IR_FLASHCON, &dr, 
										  C8051F_DR_FLASHCON_LEN);
					// poll for FLBusy
					jtag_poll_flbusy(0);
					c8051f_jtag_ind_read(C8051F_IR_FLASHDAT, 
										 ((uint32*)page_buf + j), 
										 C8051F_DR_FLASHDAT_RLEN);
				}
				
				if (ERROR_OK != jtag_commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "read flash", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION_ADDR;
					goto leave_program_mode;
				}
				
				for (j = 0; j < page_size; j++)
				{
					if ((*((uint32*)page_buf + j) & 0x06) != 0)
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "read flash");
						ret = ERRCODE_FAILURE_OPERATION;
						goto leave_program_mode;
					}
					
					if (operations.verify_operations & APPLICATION)
					{
						if (((*((uint32*)page_buf + j) >> 3) & 0xFF) 
							!= pi->app[ml_tmp->addr + i + j])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"flash", ml_tmp->addr + i + j, 
								(*((uint32*)page_buf + j) >> 3) & 0xFF, 
								pi->app[ml_tmp->addr + i + j]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
					else
					{
						pi->app[ml_tmp->addr + i + j] = 
									(*((uint32*)page_buf + j) >> 3) & 0xFF;
					}
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
			
			ml_tmp = ml_tmp->next;
		}
		
		pgbar_fini();
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash", 
					 pi->app_size_valid);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "flash");
		}
	}
	
leave_program_mode:
	jtag_fini();
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "exit program mode");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}


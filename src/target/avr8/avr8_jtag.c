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

#include "avr8.h"
#include "avr8_internal.h"

#define CUR_TARGET_STRING							AVR8_STRING
#define cur_chip_param								target_chip_param

#define AVR_JTAG_INS_LEN							4
// Public Instructions:
#define AVR_JTAG_INS_EXTEST							0x00
#define AVR_JTAG_INS_IDCODE							0x01
#define AVR_JTAG_INS_SAMPLE_PRELOAD					0x02
#define AVR_JTAG_INS_BYPASS							0x0F
// AVR Specified Public Instructions:
#define AVR_JTAG_INS_AVR_RESET						0x0C
#define AVR_JTAG_INS_PROG_ENABLE					0x04
#define AVR_JTAG_INS_PROG_COMMANDS					0x05
#define AVR_JTAG_INS_PROG_PAGELOAD					0x06
#define AVR_JTAG_INS_PROG_PAGEREAD					0x07

// Data Registers:
#define AVR_JTAG_REG_Bypass_Len						1
#define AVR_JTAG_REG_DeviceID_Len					32

#define AVR_JTAG_REG_Reset_Len						1
#define AVR_JTAG_REG_JTAGID_Len						32
#define AVR_JTAG_REG_ProgrammingEnable_Len			16
#define AVR_JTAG_REG_ProgrammingCommand_Len			15
#define AVR_JTAG_REG_FlashDataByte_Len				16

#define AVR_JTAG_RTI_CYCLE							1

#define AVR_JTAG_Reset(r)							(AVR_JTAG_SendIns(AVR_JTAG_INS_AVR_RESET), AVR_JTAG_SendDat((r),AVR_JTAG_REG_Reset_Len))


// JTAG Programming Instructions:
#define AVR_JTAG_PROG_OPERATIONCOMPLETE				0x0200
#define AVR_JTAG_PROG_INS(d)						AVR_JTAG_SendDat((d), AVR_JTAG_REG_ProgrammingCommand_Len)
#define AVR_JTAG_PROG_ReadDATA(d, p)				AVR_JTAG_ReadDat((d), (uint16_t*)(p), AVR_JTAG_REG_ProgrammingCommand_Len)
#define AVR_JTAG_PROG_LoadAddrExtendedHighByte(c)	AVR_JTAG_PROG_INS(0xB00 | ((c) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrHighByte(a)			AVR_JTAG_PROG_INS(0x0700 | ((a) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrLowByte(b)			AVR_JTAG_PROG_INS(0x0300 | ((b) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrByte(b)				AVR_JTAG_PROG_LoadAddrLowByte(b)
#define AVR_JTAG_PROG_LoadDataLowByte(i)			AVR_JTAG_PROG_INS(0x1300 | ((i) & 0xFF))
#define AVR_JTAG_PROG_LoadDataHighByte(i)			AVR_JTAG_PROG_INS(0x1700 | ((i) & 0xFF))
#define AVR_JTAG_PROG_LoadDataByte(i)				AVR_JTAG_PROG_LoadDataLowByte(i)
#define AVR_JTAG_PROG_LatchData()					(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x7700), AVR_JTAG_PROG_INS(0x3700))
// Chip Erase
#define AVR_JTAG_PROG_ChipErase()					(AVR_JTAG_PROG_INS(0x2380), AVR_JTAG_PROG_INS(0x3180), AVR_JTAG_PROG_INS(0x3380), AVR_JTAG_PROG_INS(0x3380))
#define AVR_JTAG_PROG_ChipEraseComplete_CMD			0x3380

// Write Flash
#define AVR_JTAG_PROG_EnterFlashWrite()				AVR_JTAG_PROG_INS(0x2310)
#define AVR_JTAG_PROG_WriteFlashPage()				(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFlashPageComplete_CMD	0x3700

// Read Flash
#define AVR_JTAG_PROG_EnterFlashRead()				AVR_JTAG_PROG_INS(0x2302)

// Write EEPROM
#define AVR_JTAG_PROG_EnterEEPROMWrite()			AVR_JTAG_PROG_INS(0x2311)
#define AVR_JTAG_PROG_WriteEEPROMPage()				(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteEEPROMPageComplete_CMD	0x3300

// Read EEPROM
#define AVR_JTAG_PROG_EnterEEPROMRead()				AVR_JTAG_PROG_INS(0x2303)
#define AVR_JTAG_PROG_ReadEEPROM(a, d)				(AVR_JTAG_PROG_INS(0x3300 | (a)), AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(d)))

// Write Fuses
#define AVR_JTAG_PROG_EnterFuseWrite()				AVR_JTAG_PROG_INS(0x2340)
#define AVR_JTAG_PROG_WriteFuseExtByte()			(AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3900), AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3B00))
#define AVR_JTAG_PROG_WriteFuseExtByteComplete_CMD	0x3700
#define AVR_JTAG_PROG_WriteFuseHighByte()			(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFuseHighByteComplete_CMD	0x3700
#define AVR_JTAG_PROG_WriteFuseLowByte()			(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteFuseLowByteComplete_CMD	0x3300

// Write Lockbits
#define AVR_JTAG_PROG_EnterLockbitWrite()			AVR_JTAG_PROG_INS(0x2320)
#define AVR_JTAG_PROG_WriteLockbit()				(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteLockbitComplete_CMD		0x3300

// Read Fuses/Lockbits
#define AVR_JTAG_PROG_EnterFuseLockbitRead()		AVR_JTAG_PROG_INS(0x2304)
#define AVR_JTAG_PROG_ReadExtFuseByte(e)			(AVR_JTAG_PROG_INS(0x3A00), AVR_JTAG_PROG_ReadDATA(0x3B00, &(e)))
#define AVR_JTAG_PROG_ReadFuseHighByte(h)			(AVR_JTAG_PROG_INS(0x3E00), AVR_JTAG_PROG_ReadDATA(0x3F00, &(h)))
#define AVR_JTAG_PROG_ReadFuseLowByte(l)			(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(l)))
#define AVR_JTAG_PROG_ReadLockbit(l)				(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_ReadDATA(0x3700, &(l)))

// Read Signature
#define AVR_JTAG_PROG_EnterSignByteRead()			AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadSignByte(sig)				(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(sig)))

// Read Calibration Byte
#define AVR_JTAG_PROG_EnterCaliByteRead()			AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadCaliByte(c)				(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_ReadDATA(0x3700, &(c)))

// No Operation Command
#define AVR_JTAG_PROG_LoadNoOperationCommand()		(AVR_JTAG_PROG_INS(0x2300), AVR_JTAG_PROG_INS(0x3300))












#define jtag_init()					p->jtag_hl_init()
#define jtag_fini()					p->jtag_hl_fini()
#define jtag_config(kHz,a,b,c,d)	p->jtag_hl_config((kHz), (a), (b), (c), (d))
#define jtag_runtest(len)			p->jtag_hl_runtest(len)
#define jtag_ir_write(ir, len)		p->jtag_hl_ir((uint8_t*)(ir), (len), 1, 0)
#define jtag_dr_write(dr, len)		p->jtag_hl_dr((uint8_t*)(dr), (len), 1, 0)
#define jtag_dr_read(dr, len)		p->jtag_hl_dr((uint8_t*)(dr), (len), 1, 1)

#define poll_start()				p->poll_start(20, 500)
#define poll_end()					p->poll_end()
#define poll_check(o, m, v)			p->poll_checkbyte((o), (m), (v))

#define jtag_delay_us(us)			p->jtag_hl_delay_us((us))
#define jtag_delay_ms(ms)			p->jtag_hl_delay_ms((ms))
#define jtag_commit()				p->jtag_hl_commit()

static programmer_info_t *p = NULL;

#define AVR_JTAG_SendIns(i)			(ir = (i), \
									 jtag_ir_write(&ir, AVR_JTAG_INS_LEN))
#define AVR_JTAG_SendDat(d, len)	(dr = (d), jtag_dr_write(&dr, (len)))
void AVR_JTAG_ReadDat(uint16_t w, uint16_t* r, uint8_t len)
{
	*r = w;
	jtag_dr_read(r, len);
}

void AVR_JTAG_WaitComplete(uint16_t cmd)
{
	uint16_t dr;
	
	poll_start();
	AVR_JTAG_PROG_INS(cmd);
	poll_check(0, 0x02, 0x02);
	poll_end();
}

RESULT avr8_jtag_program(operation_t operations, program_info_t *pi, 
						 programmer_info_t *prog)
{
	uint8_t ir;
	uint32_t dr;
	
	int32_t i;
	uint32_t j, k, page_size, len_current_list;
	uint8_t page_buf[256 + 1]; // one more byte for dummy 16bit read
	RESULT ret = ERROR_OK;
	memlist *ml_tmp;
	uint32_t target_size;

	p = prog;
	
	// here we go
	// init
	jtag_init();
	jtag_config(4500, target_jtag_pos.ub, target_jtag_pos.ua, 
				target_jtag_pos.bb, target_jtag_pos.ba);
	
	// enter program mode
	AVR_JTAG_Reset(1);
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_ENABLE);
	AVR_JTAG_SendDat(0xA370, AVR_JTAG_REG_ProgrammingEnable_Len);
	
	// read chip_id
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterSignByteRead();
	
	AVR_JTAG_PROG_LoadAddrByte(0);
	AVR_JTAG_PROG_ReadSignByte(page_buf[0]);
	AVR_JTAG_PROG_LoadAddrByte(1);
	AVR_JTAG_PROG_ReadSignByte(page_buf[1]);
	AVR_JTAG_PROG_LoadAddrByte(2);
	AVR_JTAG_PROG_ReadSignByte(page_buf[2]);
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read signature");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = page_buf[2] | (page_buf[1] << 8) | (page_buf[0] << 16);
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_CHIP_ID), pi->chip_id);
	if (!(operations.read_operations & CHIP_ID))
	{
		if (pi->chip_id != cur_chip_param.chip_id)
		{
			LOG_WARNING(_GETTEXT(ERRMSG_INVALID_CHIP_ID), pi->chip_id, 
						cur_chip_param.chip_id);
		}
	}
	else
	{
		goto leave_program_mode;
	}
	
	if (operations.erase_operations > 0)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, '=');

		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
		AVR_JTAG_PROG_ChipErase();
		AVR_JTAG_WaitComplete(AVR_JTAG_PROG_ChipEraseComplete_CMD);
		if (ERROR_OK != jtag_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	// set page size for flash
	if (cur_chip_param.app_page_num > 1)
	{
		page_size = cur_chip_param.app_page_size;
	}
	else
	{
		LOG_BUG(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "byte mode", "JTAG");
		ret = ERRCODE_NOT_SUPPORT;
		goto leave_program_mode;
	}
	
	target_size = MEMLIST_CalcAllSize(pi->app_memlist);
	if (operations.write_operations & APPLICATION)
	{
		// program
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = pi->app_memlist;
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
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len - (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
				AVR_JTAG_PROG_EnterFlashWrite();
				
				AVR_JTAG_PROG_LoadAddrHighByte((ml_tmp->addr + i) >> 9);
				AVR_JTAG_PROG_LoadAddrLowByte((ml_tmp->addr + i) >> 1);
				AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_PAGELOAD);
				
				if (cur_chip_param.param[AVR8_PARAM_JTAG_FULL_BITSTREAM])
				{
					jtag_dr_write(pi->app + ml_tmp->addr + i, 
								  (uint16_t)(cur_chip_param.app_page_size * 8));
				}
				else
				{
					for (j = 0; j < cur_chip_param.app_page_size; j++)
					{
						jtag_dr_write(pi->app + ml_tmp->addr + i + j, 8);
					}
				}
				AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
				AVR_JTAG_PROG_WriteFlashPage();
				AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFlashPageComplete_CMD);
				if (ERROR_OK != jtag_commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "program flash in page mode", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= cur_chip_param.app_page_size)
				{
					k = cur_chip_param.app_page_size;
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
	
	if (operations.read_operations & APPLICATION)
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			ret = MEMLIST_Add(&pi->app_memlist, 0, pi->app_size, page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(pi->app_memlist);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, target_size, PROGRESS_STEP, '=');

		ml_tmp = pi->app_memlist;
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
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len - (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
				AVR_JTAG_PROG_EnterFlashRead();
				
				AVR_JTAG_PROG_LoadAddrHighByte((ml_tmp->addr + i) >> 9);
				AVR_JTAG_PROG_LoadAddrLowByte((ml_tmp->addr + i) >> 1);
				AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_PAGEREAD);
				
				if (cur_chip_param.param[AVR8_PARAM_JTAG_FULL_BITSTREAM])
				{
					dr = 0;
					jtag_dr_write(&dr, 8);
					jtag_dr_read(page_buf, 
								 (uint16_t)(cur_chip_param.app_page_size * 8));
				}
				else
				{
					for (j = 0; j < cur_chip_param.app_page_size; j++)
					{
						jtag_dr_read(page_buf + j, 8);
					}
				}
				
				if (ERROR_OK != jtag_commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "read flash in page mode", ml_tmp->addr + i);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				if (operations.verify_operations & APPLICATION)
				{
					for (j = 0; j < page_size; j++)
					{
						if (page_buf[j] != pi->app[ml_tmp->addr + i + j])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"flash", 
								ml_tmp->addr + i + j, page_buf[j], 
								pi->app[ml_tmp->addr + i + j]);
							ret = ERROR_FAIL;
							goto leave_program_mode;
						}
					}
				}
				else
				{
					memcpy(&pi->app[ml_tmp->addr + i], page_buf, page_size);
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= cur_chip_param.app_page_size)
				{
					k = cur_chip_param.app_page_size;
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
	
	// set page size for eeprom
	if (cur_chip_param.ee_page_num > 1)
	{
		page_size = cur_chip_param.ee_page_size;
	}
	else
	{
		page_size = 256;
	}
	
	target_size = MEMLIST_CalcAllSize(pi->eeprom_memlist);
	if (operations.write_operations & EEPROM)
	{
		// program eeprom
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "eeprom");
		pgbar_init("writing eeprom |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = pi->eeprom_memlist;
		while(ml_tmp != NULL)
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
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len - (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				if (cur_chip_param.ee_page_num > 1)
				{
					// Page mode
					AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
					AVR_JTAG_PROG_EnterEEPROMWrite();
					AVR_JTAG_PROG_LoadAddrHighByte((ml_tmp->addr + i) >> 8);
					
					for (j = 0; j < page_size; j++)
					{
						AVR_JTAG_PROG_LoadAddrLowByte(ml_tmp->addr + i + j);
						AVR_JTAG_PROG_LoadDataByte(pi->eeprom[ml_tmp->addr + i + j]);
						AVR_JTAG_PROG_LatchData();
					}
					
					// write page
					AVR_JTAG_PROG_WriteEEPROMPage();
					AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteEEPROMPageComplete_CMD);
					
					if (ERROR_OK != jtag_commit())
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "program eeprom in page mode", 
								  ml_tmp->addr + i);
						ret = ERRCODE_FAILURE_OPERATION;
						goto leave_program_mode;
					}
				}
				else
				{
					// Byte mode
					LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
								"eeprom byte mode", "avr8 jtag");
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
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "eeprom", target_size);
	}
	
	if (operations.read_operations & EEPROM)
	{
		if (operations.verify_operations & EEPROM)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "eeprom");
		}
		else
		{
			ret = MEMLIST_Add(&pi->eeprom_memlist, 0, pi->eeprom_size, page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(pi->eeprom_memlist);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "eeprom");
		}
		pgbar_init("reading eeprom |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = pi->eeprom_memlist;
		page_size = 256;
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
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len - (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
				AVR_JTAG_PROG_EnterEEPROMRead();
				
				for (j = 0; j < page_size; j++)
				{
					AVR_JTAG_PROG_LoadAddrHighByte((ml_tmp->addr + i) >> 8);
					AVR_JTAG_PROG_LoadAddrLowByte(ml_tmp->addr + i + j);
					AVR_JTAG_PROG_ReadEEPROM(ml_tmp->addr + i + j, page_buf[j]);
				}
				
				if (ERROR_OK != jtag_commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "read eeprom in byte mode", ml_tmp->addr + i);
						ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				if (operations.verify_operations & EEPROM)
				{
					for (j = 0; j < page_size; j++)
					{
						if (page_buf[j] != pi->eeprom[ml_tmp->addr + i + j])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"eeprom", 
								ml_tmp->addr + i + j, page_buf[j], 
								pi->eeprom[ml_tmp->addr + i + j]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
				}
				else
				{
					memcpy(&pi->eeprom[ml_tmp->addr + i], page_buf, page_size);
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
		if (operations.verify_operations & EEPROM)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "eeprom", target_size);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "eeprom");
		}
	}
	
	if (operations.write_operations & FUSE)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "fuse");
		pgbar_init("writing fuse |", "|", 0, 1, PROGRESS_STEP, '=');
		
		// write fuse
		// low bits
		if (cur_chip_param.fuse_size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterFuseWrite();
			AVR_JTAG_PROG_LoadDataLowByte((pi->fuse_value >> 0) & 0xFF);
			AVR_JTAG_PROG_WriteFuseLowByte();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFuseLowByteComplete_CMD);
		}
		// high bits
		if (cur_chip_param.fuse_size > 1)
		{
			AVR_JTAG_PROG_LoadDataLowByte((pi->fuse_value >> 8) & 0xFF);
			AVR_JTAG_PROG_WriteFuseHighByte();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFuseHighByteComplete_CMD);
		}
		// extended bits
		if (cur_chip_param.fuse_size > 2)
		{
			AVR_JTAG_PROG_LoadDataLowByte((pi->fuse_value >> 16) & 0xFF);
			AVR_JTAG_PROG_WriteFuseExtByte();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteFuseExtByteComplete_CMD);
		}
		if (cur_chip_param.fuse_size > 0)
		{
			if (ERROR_OK != jtag_commit())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write fuse");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		else
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "fuse", 
						cur_chip_param.chip_name);
			ret = ERRCODE_NOT_SUPPORT;
			goto leave_program_mode;
		}
		pgbar_update(1);
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED), "fuse");
	}
	
	if (operations.read_operations & FUSE)
	{
		if (operations.verify_operations & FUSE)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "fuse");
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READING), "fuse");
		}
		pgbar_init("reading fuse |", "|", 0, 1, PROGRESS_STEP, '=');
		
		memset(page_buf, 0, 3);
		// read fuse
		// low bits
		if (cur_chip_param.fuse_size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterFuseLockbitRead();
			AVR_JTAG_PROG_ReadFuseLowByte(page_buf[0]);
		}
		// high bits
		if (cur_chip_param.fuse_size > 1)
		{
			AVR_JTAG_PROG_ReadFuseHighByte(page_buf[1]);
		}
		// extended bits
		if (cur_chip_param.fuse_size > 2)
		{
			AVR_JTAG_PROG_ReadExtFuseByte(page_buf[2]);
		}
		if (cur_chip_param.fuse_size > 0)
		{
			if (ERROR_OK != jtag_commit())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read fuse");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		else
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "fuse", 
						cur_chip_param.chip_name);
			ret = ERRCODE_NOT_SUPPORT;
			goto leave_program_mode;
		}
		pgbar_update(1);
		
		pgbar_fini();
		i = (uint32_t)(page_buf[0] + (page_buf[1] << 8) + (page_buf[2] << 16));
		if (operations.verify_operations & FUSE)
		{
			if ((uint32_t)i == pi->fuse_value)
			{
				LOG_INFO(_GETTEXT(INFOMSG_VERIFIED), "fuse");
			}
			else
			{
				if (cur_chip_param.fuse_size > 2)
				{
					LOG_INFO(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_06X), 
							 "fuse", i, pi->fuse_value);
				}
				else if (cur_chip_param.fuse_size > 1)
				{
					LOG_INFO(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_04X), 
							 "fuse", i, pi->fuse_value);
				}
				else if (cur_chip_param.fuse_size > 0)
				{
					LOG_INFO(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_02X), 
							 "fuse", i, pi->fuse_value);
				}
			}
		}
		else
		{
			if (cur_chip_param.fuse_size > 2)
			{
				LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_06X), "fuse", i);
			}
			else if (cur_chip_param.fuse_size > 1)
			{
				LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_04X), "fuse", i);
			}
			else if (cur_chip_param.fuse_size > 0)
			{
				LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_02X), "fuse", i);
			}
		}
	}
	
	if (operations.write_operations & LOCK)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "lock");
		pgbar_init("writing lock |", "|", 0, 1, PROGRESS_STEP, '=');
		
		// write lock
		if (cur_chip_param.lock_size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterLockbitWrite();
			AVR_JTAG_PROG_LoadDataByte(pi->lock_value);
			AVR_JTAG_PROG_WriteLockbit();
			AVR_JTAG_WaitComplete(AVR_JTAG_PROG_WriteLockbitComplete_CMD);
			if (ERROR_OK != jtag_commit())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write lock");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		else
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "locks", 
						cur_chip_param.chip_name);
			ret = ERRCODE_NOT_SUPPORT;
			goto leave_program_mode;
		}
		pgbar_update(1);
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED), "lock");
	}
	
	if (operations.read_operations & LOCK)
	{
		if (operations.verify_operations & LOCK)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "lock");
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READING), "lock");
		}
		pgbar_init("reading lock |", "|", 0, 1, PROGRESS_STEP, '=');
		
		memset(page_buf, 0, 1);
		// read lock
		if (cur_chip_param.lock_size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterFuseLockbitRead();
			AVR_JTAG_PROG_ReadLockbit(page_buf[0]);
			if (ERROR_OK != jtag_commit())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read lock");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		else
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "locks", 
						cur_chip_param.chip_name);
			ret = ERRCODE_NOT_SUPPORT;
			goto leave_program_mode;
		}
		pgbar_update(1);
		
		pgbar_fini();
		if (operations.verify_operations & LOCK)
		{
			if (page_buf[0] == (uint8_t)pi->lock_value)
			{
				LOG_INFO(_GETTEXT(INFOMSG_VERIFIED), "lock");
			}
			else
			{
				LOG_INFO(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_02X), 
						 "lock", page_buf[0], pi->lock_value);
			}
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_02X), "lock", page_buf[0]);
		}
	}
	
	if (operations.read_operations & CALIBRATION)
	{
		LOG_INFO(_GETTEXT(INFOMSG_READING), "calibration");
		pgbar_init("reading calibration |", "|", 0, 1, PROGRESS_STEP, '=');
		
		memset(page_buf, 0, 4);
		// read calibration
		if (cur_chip_param.cali_size > 0)
		{
			AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
			AVR_JTAG_PROG_EnterCaliByteRead();
			AVR_JTAG_PROG_LoadAddrByte(0);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[0]);
		}
		if (cur_chip_param.cali_size > 1)
		{
			AVR_JTAG_PROG_LoadAddrByte(1);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[1]);
		}
		if (cur_chip_param.cali_size > 2)
		{
			AVR_JTAG_PROG_LoadAddrByte(2);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[2]);
		}
		if (cur_chip_param.cali_size > 3)
		{
			AVR_JTAG_PROG_LoadAddrByte(3);
			AVR_JTAG_PROG_ReadCaliByte(page_buf[3]);
		}
		if (cur_chip_param.cali_size > 0)
		{
			if (ERROR_OK != jtag_commit())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read calibration");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		else
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "calibration", 
						cur_chip_param.chip_name);
			ret = ERRCODE_NOT_SUPPORT;
			goto leave_program_mode;
		}
		pgbar_update(1);
		
		pgbar_fini();
		i = (uint32_t)(page_buf[0] + (page_buf[1] << 8) 
					+ (page_buf[2] << 16) + (page_buf[3] << 24));
		if (cur_chip_param.fuse_size > 3)
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_08X), "calibration", i);
		}
		else if (cur_chip_param.fuse_size > 2)
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_06X), "calibration", i);
		}
		else if (cur_chip_param.fuse_size > 1)
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_04X), "calibration", i);
		}
		else if (cur_chip_param.fuse_size > 0)
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE_02X), "calibration", i);
		}
	}
	
leave_program_mode:
	// leave program mode
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_LoadNoOperationCommand();
	
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_ENABLE);
	AVR_JTAG_SendDat(0, AVR_JTAG_REG_ProgrammingEnable_Len);
	
	AVR_JTAG_Reset(0);
	jtag_fini();
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "exit program mode");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}


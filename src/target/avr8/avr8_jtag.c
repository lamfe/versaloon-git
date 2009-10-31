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

#define CUR_TARGET_STRING		AVR8_STRING
#define cur_chip_param			target_chip_param

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
#define AVR_JTAG_PROG_ChipEraseComplete()			(AVR_JTAG_PROG_INS(0x3380) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Write Flash
#define AVR_JTAG_PROG_EnterFlashWrite()				AVR_JTAG_PROG_INS(0x2310)
#define AVR_JTAG_PROG_WriteFlashPage()				(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFlashPageComplete()		(AVR_JTAG_PROG_INS(0x3700) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Read Flash
#define AVR_JTAG_PROG_EnterFlashRead()				AVR_JTAG_PROG_INS(0x2302)

// Write EEPROM
#define AVR_JTAG_PROG_EnterEEPROMWrite()			AVR_JTAG_PROG_INS(0x2311)
#define AVR_JTAG_PROG_WriteEEPROMPage()				(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteEEPROMPageComplete()		(AVR_JTAG_PROG_INS(0x3300) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Read EEPROM
#define AVR_JTAG_PROG_EnterEEPROMRead()				AVR_JTAG_PROG_INS(0x2303)

// Write Fuses
#define AVR_JTAG_PROG_EnterFuseWrite()				AVR_JTAG_PROG_INS(0x2340)
#define AVR_JTAG_PROG_WriteFuseExtByte()			(AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3900), AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3B00))
#define AVR_JTAG_PROG_WriteFuseExtByteComplete()	(AVR_JTAG_PROG_INS(0x3700) & AVR_JTAG_PROG_OPERATIONCOMPLETE)
#define AVR_JTAG_PROG_WriteFuseHighByte()			(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFuseHighByteComplete()	(AVR_JTAG_PROG_INS(0x3700) & AVR_JTAG_PROG_OPERATIONCOMPLETE)
#define AVR_JTAG_PROG_WriteFuseLowByte()			(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteFuseLowByteComplete()	(AVR_JTAG_PROG_INS(0x3300) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Write Lockbits
#define AVR_JTAG_PROG_EnterLockbitWrite()			AVR_JTAG_PROG_INS(0x2320)
#define AVR_JTAG_PROG_WriteLockbit()				(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteLockbitComplete()		(AVR_JTAG_PROG_INS(0x3300) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Read Fuses/Lockbits
#define AVR_JTAG_PROG_EnterFuseLockbitRead()		AVR_JTAG_PROG_INS(0x2304)
#define AVR_JTAG_PROG_ReadExtFuseByte()				(AVR_JTAG_PROG_INS(0x3A00), AVR_JTAG_PROG_INS(0x3B00))
#define AVR_JTAG_PROG_ReadFuseHighByte()			(AVR_JTAG_PROG_INS(0x3E00), AVR_JTAG_PROG_INS(0x3F00))
#define AVR_JTAG_PROG_ReadFuseLowByte()				(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_ReadLockbit()					(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_INS(0x3700))

// Read Signature
#define AVR_JTAG_PROG_EnterSignByteRead()			AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadSignByte(sig)				(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_ReadDATA(0x3300, &(sig)))

// Read Calibration Byte
#define AVR_JTAG_PROG_EnterCaliByteRead()			AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadCaliByte()				(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_INS(0x3700))

// No Operation Command
#define AVR_JTAG_PROG_LoadNoOperationCommand()		(AVR_JTAG_PROG_INS(0x2300), AVR_JTAG_PROG_INS(0x3300))












#define jtag_init()					p->jtag_hl_init()
#define jtag_fini()					p->jtag_hl_fini()
#define jtag_config(kHz,a,b,c,d)	\
								p->jtag_hl_config((kHz), (a), (b), (c), (d))
#define jtag_runtest(len)			p->jtag_hl_runtest(len)
#define jtag_ir_write(ir, len)		p->jtag_hl_ir((uint8_t*)(ir), (len), 1, 0)
#define jtag_dr_write(dr, len)		p->jtag_hl_dr((uint8_t*)(dr), (len), 1, 0)
#define jtag_dr_read(dr, len)		p->jtag_hl_dr((uint8_t*)(dr), (len), 1, 1)

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

RESULT avr8_jtag_program(operation_t operations, program_info_t *pi, 
						 programmer_info_t *prog)
{
	uint16_t chip_sig[3];
	uint8_t ir;
	uint32_t dr;
	
	int32_t i;
	uint32_t j, k, page_size, len_current_list;
	uint8_t page_buf[256];
	RESULT ret = ERROR_OK;
	memlist *ml_tmp;

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
	AVR_JTAG_PROG_ReadSignByte(chip_sig[0]);
	AVR_JTAG_PROG_LoadAddrByte(1);
	AVR_JTAG_PROG_ReadSignByte(chip_sig[1]);
	AVR_JTAG_PROG_LoadAddrByte(2);
	AVR_JTAG_PROG_ReadSignByte(chip_sig[2]);
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read signature");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = ((chip_sig[2] & 0xFF) << 0) | ((chip_sig[1] & 0xFF) << 8) 
				   | ((chip_sig[0] & 0xFF) << 16);
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
		jtag_delay_ms(20);
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
	
	if (operations.write_operations & APPLICATION)
	{
		// program
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
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
				jtag_delay_ms(5);
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
			pi->app_size_valid = cur_chip_param.app_size;
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
				
				for (j = 0; j < page_size; j++)
				{
					if (operations.verify_operations & APPLICATION)
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
					else
					{
						memcpy(&pi->app[ml_tmp->addr + i], page_buf, 
							   page_size);
					}
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


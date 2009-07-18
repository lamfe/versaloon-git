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

#include "port.h"
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

#include "lpc900.h"
#include "lpc900_internal.h"

#define CUR_TARGET_STRING			LPC900_STRING
#define cur_chip_param				target_chip_param
#define cur_chips_param				target_chips.chips_param
#define cur_chips_num				target_chips.num_of_chips
#define cur_flash_offset			lpc900_flash_offset

const program_area_map_t lpc900_program_area_map[] = 
{
	{APPLICATION, 'f', 1},
	{0, 0}
};

static uint32 lpc900_flash_offset = 0;

void lpc900_usage(void)
{
	printf("\
Usage of %s:\n\n", CUR_TARGET_STRING);
}

void lpc900_support(void)
{
	uint32 i;

	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < cur_chips_num; i++)
	{
		printf("\
%s: id = 0x%04x\n", 
				cur_chips_param[i].chip_name, 
				cur_chips_param[i].chip_id);
	}
	printf("\n");
}

RESULT lpc900_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		lpc900_usage();
		break;
	case 'S':
		lpc900_support();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT lpc900_probe_chip(char *chip_name)
{
	uint32 i;
	
	for (i = 0; i < cur_chips_num; i++)
	{
		if (!strcmp(cur_chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
}

RESULT lpc900_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, LPC900_FLASH_CHAR, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT lpc900_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT lpc900_init(program_info_t *pi, const char *dir, 
				 programmer_info_t *prog)
{
	uint8 i;
	operation_t opt_tmp = {0};
	
	dir = dir;
	
	if (strcmp(pi->chip_type, CUR_TARGET_STRING))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_HANDLER), CUR_TARGET_STRING, 
				pi->chip_type);
		return ERRCODE_INVALID_HANDLER;
	}
	
	if (NULL == pi->chip_name)
	{
		// auto detect
		LOG_INFO(_GETTEXT(INFOMSG_TRY_AUTODETECT));
		opt_tmp.read_operations = CHIP_ID;
		
		if (ERROR_OK != lpc900_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < cur_chips_num; i++)
		{
			if (pi->chip_id == cur_chips_param[i].chip_id)
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				cur_chip_param.app_size = cur_chip_param.app_page_num
										  * cur_chip_param.app_page_size;
				
				pi->app_size = cur_chip_param.app_size;
				pi->app_size_valid = 0;
				
				LOG_INFO(_GETTEXT(INFOMSG_CHIP_FOUND), 
						 cur_chip_param.chip_name);
				pi->chip_name = (char *)cur_chip_param.chip_name;
				
				return ERROR_OK;
			}
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < cur_chips_num; i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				cur_chip_param.app_size = cur_chip_param.app_page_num
										  * cur_chip_param.app_page_size;
				
				pi->app_size = cur_chip_param.app_size;
				pi->app_size_valid = 0;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32 lpc900_interface_needed(void)
{
	return LPC900_INTERFACE_NEEDED;
}

RESULT lpc900_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
											  uint8* data, uint32 length, 
											  void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32 mem_addr = address & 0x0000FFFF;
	RESULT ret;
	
#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (seg_addr != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
				  "segment address", CUR_TARGET_STRING);
		return ERRCODE_NOT_SUPPORT;
	}
	
	// flash from 0x00000000
	switch (address >> 16)
	{
	case 0x0000:
		if (NULL == pi->app)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "pi->app");
			return ERRCODE_INVALID_BUFFER;
		}
		
/*		if ((0 == cur_chip_param.flash_page_num) 
			|| (0 == cur_chip_param.flash_page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Flash", 
					  cur_chip_param.chip_name);
			return ERRCODE_INVALID;
		}
*/		
		mem_addr += cur_flash_offset;
		if ((mem_addr >= cur_chip_param.app_size)
 			|| (length > cur_chip_param.app_size)
 			|| ((mem_addr + length) > cur_chip_param.app_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
			return ERRCODE_INVALID;
		}
		memcpy(pi->app + mem_addr, data, length);
		pi->app_size_valid += (uint16)length;
		ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, 
						  cur_chip_param.app_page_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_ADDRESS), address, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID;
		break;
	}
	
	return ERROR_OK;
}






static programmer_info_t *p = NULL;

#define get_target_voltage(v)				p->get_target_voltage(v)

#define icp_init()							p->lpcicp_init()
#define icp_fini()							p->lpcicp_fini()
#define icp_enter_program_mode()			p->lpcicp_enter_program_mode()
#define icp_leave_program_mode()			p->lpcicp_leave_program_mode()
#define icp_poll_ready()					p->lpcicp_poll_ready()
#define icp_commit()						p->lpcicp_commit()
#define icp_in(buf, len)					p->lpcicp_in((buf), (len))
#define icp_out(buf, len)					p->lpcicp_out((buf), (len))

#define ICP_CMD_READ						0x01
#define ICP_CMD_WRITE						0x00

#define ICP_CMD_NOP							0x00
#define	ICP_CMD_FMDATA_I					0x04
#define ICP_CMD_FMADRL						0x08
#define ICP_CMD_FMADRH						0x0A
#define ICP_CMD_FMDATA						0x0C
#define ICP_CMD_FMCON						0x0E
#define ICP_CMD_FMDATA_PG					0x14

#define ICP_FMCMD_LOAD						0x00
#define ICP_FMCMD_PROG						0x48
#define ICP_FMCMD_ERS_G						0x72
#define ICP_FMCMD_ERS_S						0x71
#define ICP_FMCMD_ERS_P						0x70
#define ICP_FMCMD_CONF						0x6C
#define ICP_FMCMD_CRC_G						0x1A
#define ICP_FMCMD_CRC_S						0x19
#define ICP_FMCMD_CCP						0x67

#define ICP_CFG_UCFG1						0x00
#define ICP_CFG_UCFG2						0x01
#define ICP_CFG_BOOTVECTOR					0x02
#define ICP_CFG_STATUSBYTE					0x03
#define ICP_CFG_SEC0						0x08
#define ICP_CFG_SEC1						0x09
#define ICP_CFG_SEC2						0x0A
#define ICP_CFG_SEC3						0x0B
#define ICP_CFG_SEC4						0x0C
#define ICP_CFG_SEC5						0x0D
#define ICP_CFG_SEC6						0x0E
#define ICP_CFG_SEC7						0x0F
#define ICP_CFG_MFGID						0x10
#define ICP_CFG_ID1							0x11
#define ICP_CFG_ID2							0x12

RESULT lpc900_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog)
{
	uint16 voltage;
	uint8 page_buf[LPC900_PAGE_SIZE + 20];
	uint32 device_id, poll_cnt;
	int32 i;
	uint32 j, k, len_current_list;
	uint32 crc_in_file, crc_in_chip;
	uint16 page_size;
	RESULT ret = ERROR_OK;
	memlist *ml_tmp;
	
	p = prog;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	// get target voltage
	if (ERROR_OK != get_target_voltage(&voltage))
	{
		return ERROR_FAIL;
	}
	LOG_DEBUG(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000, 
			  voltage % 1000);
	if (voltage > 2000)
	{
		LOG_ERROR(_GETTEXT("Target should power off\n"));
		return ERROR_FAIL;
	}
	
	// here we go
	// ICP Init
	if (ERROR_OK != icp_init())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize icp");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enter program mode
	if (ERROR_OK != icp_enter_program_mode())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "enter program mode");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	
	// read chip_id
	// call table_read no.0 and read 2 bytes from 0xF8 in sram
	device_id = 0;
	
	page_buf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	page_buf[1] = ICP_FMCMD_CONF;
	page_buf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	page_buf[3] = ICP_CFG_MFGID;
	page_buf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(page_buf, 5);
	icp_in((uint8*)&device_id + 2, 1);
	
	page_buf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	page_buf[1] = ICP_FMCMD_CONF;
	page_buf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	page_buf[3] = ICP_CFG_ID1;
	page_buf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(page_buf, 5);
	icp_in((uint8*)&device_id + 1, 1);
	
	page_buf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	page_buf[1] = ICP_FMCMD_CONF;
	page_buf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	page_buf[3] = ICP_CFG_ID2;
	page_buf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(page_buf, 5);
	icp_in((uint8*)&device_id + 0, 1);
	
	if (ERROR_OK != icp_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = device_id;
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
	
	// chip erase
	if (operations.erase_operations > 0)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, '=');
		
		page_buf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		page_buf[1] = ICP_FMCMD_ERS_G;
		icp_out(page_buf, 2);
		if (ERROR_OK != icp_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		// poll ready
		poll_cnt = 0;
		do{
			page_buf[0] = ICP_CMD_READ | ICP_CMD_FMCON;
			icp_out(page_buf, 1);
			icp_in(page_buf, 1);
			if ((ERROR_OK != icp_commit()) || ((page_buf[0] & 0x0F) != 0) 
				|| (++poll_cnt > 100))
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}while (page_buf[0] & 0x80);
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	page_size = (uint16)cur_chip_param.app_page_size;
	
	// write flash
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
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint32)ml_tmp->len;
			for (i = -(int32)(ml_tmp->addr % page_size); 
				 i < ((int32)ml_tmp->len - (int32)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				page_buf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
				page_buf[1] = ICP_FMCMD_LOAD;
				page_buf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
				page_buf[3] = 0;
				page_buf[4] = ICP_CMD_WRITE | ICP_CMD_FMDATA_PG;
				
				for (j = 0; j < page_size; j++)
				{
					page_buf[5 + j] = pi->app[ml_tmp->addr + i + j];
				}
				
				page_buf[5 + page_size + 0] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
				page_buf[5 + page_size + 1] = ((ml_tmp->addr + i) >> 0) & 0xFF;
				page_buf[5 + page_size + 2] = ICP_CMD_WRITE | ICP_CMD_FMADRH;
				page_buf[5 + page_size + 3] = ((ml_tmp->addr + i) >> 8) & 0xFF;
				page_buf[5 + page_size + 4] = ICP_CMD_WRITE | ICP_CMD_FMCON;
				page_buf[5 + page_size + 5] = ICP_FMCMD_PROG;
				
				icp_out(page_buf, 11 + page_size);
				if (ERROR_OK != icp_commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "program flash");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				// poll ready
				poll_cnt = 0;
				do{
					page_buf[0] = ICP_CMD_READ | ICP_CMD_FMCON;
					icp_out(page_buf, 1);
					icp_in(page_buf, 1);
					if ((ERROR_OK != icp_commit()) || ((page_buf[0] & 0x0F) != 0) 
						|| (++poll_cnt > 100))
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "program flash");
						ret = ERRCODE_FAILURE_OPERATION;
						goto leave_program_mode;
					}
				}while (page_buf[0] & 0x80);
				
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
	
	// verify
	if (operations.read_operations & APPLICATION)
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), "read lpc900 flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		pgbar_init("verifying flash |", "|", 0, 1, PROGRESS_STEP, '=');
		
		// CRC verify
		page_buf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		page_buf[1] = ICP_FMCMD_CRC_G;
		icp_out(page_buf, 2);
		if (ERROR_OK != icp_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "verify flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		// poll ready
		poll_cnt = 0;
		do{
			page_buf[0] = ICP_CMD_READ | ICP_CMD_FMCON;
			icp_out(page_buf, 1);
			icp_in(page_buf, 1);
			if ((ERROR_OK != icp_commit()) || ((page_buf[0] & 0x0F) != 0) 
				|| (++poll_cnt > 1000))
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "verify chip");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}while (page_buf[0] & 0x80);
		
		page_buf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
		icp_out(page_buf, 1);
		icp_in((uint8*)&crc_in_chip, 1);
		page_buf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
		icp_out(page_buf, 1);
		icp_in((uint8*)&crc_in_chip + 1, 1);
		page_buf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
		icp_out(page_buf, 1);
		icp_in((uint8*)&crc_in_chip + 2, 1);
		page_buf[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
		icp_out(page_buf, 1);
		icp_in((uint8*)&crc_in_chip + 3, 1);
		if (ERROR_OK != icp_commit())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "verify flash");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		
		LOG_INFO("CRC in flash is %08X\n", crc_in_chip);
		
		// calc crc in file
		{
			uint32 crc_poly = 0x00400007;
			uint32 crc_tmp = 0x00000000;
			uint8 crc_msb = 0;
			uint32 loop;
			
			crc_in_file = 0;
			for (loop = 0; loop < cur_chip_param.app_size; loop++)
			{
				uint8 byte = pi->app[loop];
				
				crc_tmp = 0;
				if (byte & (1 << 0))
				{
					crc_tmp |= (1 << 0);
				}
				if (byte & (1 << 1))
				{
					crc_tmp |= (1 << 3);
				}
				if (byte & (1 << 2))
				{
					crc_tmp |= (1 << 5);
				}
				if (byte & (1 << 3))
				{
					crc_tmp |= (1 << 8);
				}
				if (byte & (1 << 4))
				{
					crc_tmp |= (1 << 10);
				}
				if (byte & (1 << 5))
				{
					crc_tmp |= (1 << 13);
				}
				if (byte & (1 << 6))
				{
					crc_tmp |= (1 << 16);
				}
				if (byte & (1 << 7))
				{
					crc_tmp |= (1 << 18);
				}
				
				crc_msb = (uint8)((crc_in_file & 0x80000000) > 0);
				crc_in_file <<= 1;
				crc_in_file ^= crc_tmp;
				if (crc_msb)
				{
					crc_in_file ^= crc_poly;
				}
			}
		}
		LOG_INFO("CRC in file is %08X\n", crc_in_file);
		if (crc_in_file == crc_in_chip)
		{
			LOG_INFO("CRC match\n");
		}
		else
		{
			LOG_ERROR("CRC mismatch\n");
		}
	}
	
leave_program_mode:
	// leave program mode
	icp_fini();
	if (ERROR_OK != icp_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), 
				  "target chip");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}


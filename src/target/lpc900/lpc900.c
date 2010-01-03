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

const struct program_area_map_t lpc900_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EW | AREA_ATTR_V | AREA_ATTR_RNP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t lpc900_program_mode[] = 
{
	{'*', "", LPC_ICP},
	{0, NULL, 0}
};

RESULT lpc900_enter_program_mode(struct program_context_t *context);
RESULT lpc900_leave_program_mode(struct program_context_t *context, 
								uint8_t success);
RESULT lpc900_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size);
RESULT lpc900_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
RESULT lpc900_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t size);
const struct program_functions_t lpc900_program_functions = 
{
	NULL,			// execute
	lpc900_enter_program_mode, 
	lpc900_leave_program_mode, 
	lpc900_erase_target, 
	lpc900_write_target, 
	lpc900_read_target
};

void lpc900_usage(void)
{
	printf("\
Usage of %s:\n\n", CUR_TARGET_STRING);
}

RESULT lpc900_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		lpc900_usage();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}






static struct programmer_info_t *p = NULL;

#define get_target_voltage(v)				p->get_target_voltage(v)

#define icp_init()							p->lpcicp_init()
#define icp_fini()							p->lpcicp_fini()
#define icp_enter_program_mode()			p->lpcicp_enter_program_mode()
#define icp_leave_program_mode()			p->lpcicp_leave_program_mode()
#define icp_commit()						p->lpcicp_commit()
#define icp_in(buf, len)					p->lpcicp_in((buf), (len))
#define icp_out(buf, len)					p->lpcicp_out((buf), (len))
#define icp_poll(dat, ptr, set, clear, cnt)	\
					p->lpcicp_poll_ready((dat), (ptr), (set), (clear), (cnt))

#define LPCICP_POLL_ON_SET					0
#define LPCICP_POLL_ON_CLEAR				1
#define LPCICP_POLL_TIME_OUT				2

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

RESULT lpc900_enter_program_mode(struct program_context_t *context)
{
	struct program_info_t *pi = context->pi;
	uint32_t device_id;	
	uint8_t tmpbuf[5], retry = 0;
	
	p = context->prog;
	// ICP Init
ProgramStart:
	if (ERROR_OK != icp_init())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize icp");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enter program mode
	if (ERROR_OK != icp_enter_program_mode())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// read chip_id
	// call table_read no.0 and read 2 bytes from 0xF8 in sram
	device_id = 0;
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_CONF;
	tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	tmpbuf[3] = ICP_CFG_MFGID;
	tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(tmpbuf, 5);
	icp_in((uint8_t*)&device_id + 2, 1);
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_CONF;
	tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	tmpbuf[3] = ICP_CFG_ID1;
	tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(tmpbuf, 5);
	icp_in((uint8_t*)&device_id + 1, 1);
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_CONF;
	tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
	tmpbuf[3] = ICP_CFG_ID2;
	tmpbuf[4] = ICP_CMD_READ | ICP_CMD_FMDATA;
	icp_out(tmpbuf, 5);
	icp_in((uint8_t*)&device_id + 0, 1);
	
	if (ERROR_OK != icp_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		return ERRCODE_FAILURE_OPERATION;
	}
	if ((device_id & 0x00FF0000) != 0x00150000)
	{
		icp_fini();
		if (ERROR_OK != icp_commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), "target chip");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (++retry < 10)
		{
			goto ProgramStart;
		}
		else
		{
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	pi->chip_id = device_id;
	return ERROR_OK;
}

RESULT lpc900_leave_program_mode(struct program_context_t *context, 
								uint8_t success)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	icp_fini();
	if (ERROR_OK != icp_commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

RESULT lpc900_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size)
{
	uint8_t tmpbuf[2];
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(page_size);
	
	tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
	tmpbuf[1] = ICP_FMCMD_ERS_G;
	icp_out(tmpbuf, 2);
	icp_poll(ICP_CMD_READ | ICP_CMD_FMDATA_I, tmpbuf, 0x80, 0x00, 10000);
	if (ERROR_OK != icp_commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

RESULT lpc900_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	uint8_t tmpbuf[256 + 11];
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		tmpbuf[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[1] = ICP_FMCMD_LOAD;
		tmpbuf[2] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[3] = 0;
		tmpbuf[4] = ICP_CMD_WRITE | ICP_CMD_FMDATA_PG;
		memcpy(tmpbuf + 5, buff, page_size);
		tmpbuf[5 + page_size + 0] = ICP_CMD_WRITE | ICP_CMD_FMADRL;
		tmpbuf[5 + page_size + 1] = (addr >> 0) & 0xFF;
		tmpbuf[5 + page_size + 2] = ICP_CMD_WRITE | ICP_CMD_FMADRH;
		tmpbuf[5 + page_size + 3] = (addr >> 8) & 0xFF;
		tmpbuf[5 + page_size + 4] = ICP_CMD_WRITE | ICP_CMD_FMCON;
		tmpbuf[5 + page_size + 5] = ICP_FMCMD_PROG;
		
		icp_out(tmpbuf, (uint16_t)(11 + page_size));
		icp_poll(ICP_CMD_READ | ICP_CMD_FMCON, tmpbuf, 
					0x0F, 0x80, 10000);
		if ((ERROR_OK != icp_commit()) 
			|| (tmpbuf[0] != LPCICP_POLL_ON_CLEAR))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), "target chip");
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

RESULT lpc900_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t size)
{
	uint8_t tmpbuff[2];
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case CHIPID_CHAR:
		// already read when enter program mode
		break;
	case APPLICATION_CHAR:
		if (context->op->verify_operations & APPLICATION)
		{
			uint32_t crc_in_file, crc_in_chip;
			uint32_t crc_poly = 0x00400007;
			uint32_t crc_tmp = 0x00000000;
			uint8_t crc_msb = 0;
			uint32_t loop;
			
			// CRC verify
			tmpbuff[0] = ICP_CMD_WRITE | ICP_CMD_FMCON;
			tmpbuff[1] = ICP_FMCMD_CRC_G;
			icp_out(tmpbuff, 2);
			icp_poll(ICP_CMD_READ | ICP_CMD_FMCON, tmpbuff, 0x0F, 0x80, 10000);
			if ((ERROR_OK != icp_commit()) 
				|| (tmpbuff[0] != LPCICP_POLL_ON_CLEAR))
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			tmpbuff[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuff, 1);
			icp_in((uint8_t*)&crc_in_chip, 1);
			tmpbuff[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuff, 1);
			icp_in((uint8_t*)&crc_in_chip + 1, 1);
			tmpbuff[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuff, 1);
			icp_in((uint8_t*)&crc_in_chip + 2, 1);
			tmpbuff[0] = ICP_CMD_READ | ICP_CMD_FMDATA_I;
			icp_out(tmpbuff, 1);
			icp_in((uint8_t*)&crc_in_chip + 3, 1);
			if (ERROR_OK != icp_commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			// calculate crc in file
			crc_in_file = 0;
			for (loop = 0; loop < size; loop++)
			{
				uint8_t byte = buff[loop];
				
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
				
				crc_msb = (uint8_t)((crc_in_file & 0x80000000) > 0);
				crc_in_file <<= 1;
				crc_in_file ^= crc_tmp;
				if (crc_msb)
				{
					crc_in_file ^= crc_poly;
				}
			}
			
			if (crc_in_file != crc_in_chip)
			{
				ret = ERROR_FAIL;
			}
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), "read lpc900 flash");
			ret = ERRCODE_NOT_SUPPORT;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}


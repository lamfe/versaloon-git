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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "93cx6.h"
#include "93cx6_internal.h"

#define CUR_TARGET_STRING			EE93CX6_STRING

struct program_area_map_t ee93cx6_program_area_map[] = 
{
	{EEPROM_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t ee93cx6_program_mode[] = 
{
	{'b', SET_FREQUENCY, MICROWIRE},
	{'w', SET_FREQUENCY, MICROWIRE},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(ee93cx6);
LEAVE_PROGRAM_MODE_HANDLER(ee93cx6);
ERASE_TARGET_HANDLER(ee93cx6);
WRITE_TARGET_HANDLER(ee93cx6);
READ_TARGET_HANDLER(ee93cx6);
const struct program_functions_t ee93cx6_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(ee93cx6), 
	LEAVE_PROGRAM_MODE_FUNCNAME(ee93cx6), 
	ERASE_TARGET_FUNCNAME(ee93cx6), 
	WRITE_TARGET_FUNCNAME(ee93cx6), 
	READ_TARGET_FUNCNAME(ee93cx6)
};

static uint8_t ee93cx6_addr_bitlen;
static uint8_t ee93cx6_origination_mode;

VSS_HANDLER(ee93cx6_help)
{
	VSS_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set MicroWire frequency, in KHz\n\
  -m,  --mode <MODE>                        set mode<b|w>\n\n", 
			CUR_TARGET_STRING);
	return ERROR_OK;
}

VSS_HANDLER(ee93cx6_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case EE93CX6_MODE_BYTE:
	case EE93CX6_MODE_WORD:
		ee93cx6_origination_mode = mode;
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	return ERROR_OK;
}

const struct vss_cmd_t ee93cx6_notifier[] = 
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				ee93cx6_help),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				ee93cx6_mode),
	VSS_CMD_END
};





static struct interfaces_info_t *interfaces = NULL;
#define mw_init()					interfaces->microwire.init(0)
#define mw_fini()					interfaces->microwire.fini(0)
#define mw_config(kHz)				interfaces->microwire.config(0, (kHz), 1)
#define mw_poll()					interfaces->microwire.poll(0, 1000, 10)
#define mw_cmd(cmd, bitlen)			\
	interfaces->microwire.transport(0, (cmd), (bitlen), 0, 0, 0, 0, NULL, 0)
#define mw_write(addr, addr_bitlen, data, data_bitlen)	\
	interfaces->microwire.transport(0, EE93CX6_OPCODE_WRITE, \
		EE93CX6_OPCODE_WRITE_BITLEN, (addr), (addr_bitlen), (data), \
		(data_bitlen), NULL, 0)
#define mw_read(addr, addr_bitlen, data, data_bitlen)	\
	interfaces->microwire.transport(0, EE93CX6_OPCODE_READ, \
		EE93CX6_OPCODE_READ_BITLEN, (addr), (addr_bitlen), 0, 0, \
		(data), (data_bitlen))
#define mw_erase(addr, addr_bitlen)	\
	interfaces->microwire.transport(0, EE93CX6_OPCODE_ERASE, \
		EE93CX6_OPCODE_ERASE_BITLEN, (addr), (addr_bitlen), 0, 0, NULL, 0)
#define commit()					interfaces->peripheral_commit()

ENTER_PROGRAM_MODE_HANDLER(ee93cx6)
{
	struct chip_param_t *param = context->param;
	struct program_info_t *pi = context->pi;
	uint32_t cmd;
	
	interfaces = &(context->prog->interfaces);
	ee93cx6_addr_bitlen = (uint8_t)param->param[EE93CX6_PARAM_ADDR_BITLEN];
	if (EE93CX6_MODE_BYTE != ee93cx6_origination_mode)
	{
		ee93cx6_addr_bitlen--;
	}
	
	if (ee93cx6_addr_bitlen > 32)
	{
		return ERROR_FAIL;
	}
	
	if (!pi->frequency)
	{
		pi->frequency = 2000;
	}
	
	mw_init();
	mw_config(pi->frequency);
	
	cmd = EE93CX6_OPCODE_WEN << 
			(ee93cx6_addr_bitlen - EE93CX6_OPCODE_WEN_BITLEN);
	mw_cmd(cmd, ee93cx6_addr_bitlen);
	return commit();
}

LEAVE_PROGRAM_MODE_HANDLER(ee93cx6)
{
	uint32_t cmd;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	cmd = EE93CX6_OPCODE_WDS << 
			(ee93cx6_addr_bitlen - EE93CX6_OPCODE_WDS_BITLEN);
	mw_cmd(cmd, ee93cx6_addr_bitlen);
	
	mw_fini();
	return commit();
}

ERASE_TARGET_HANDLER(ee93cx6)
{
	uint32_t cmd;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	cmd = EE93CX6_OPCODE_ERAL << 
			(ee93cx6_addr_bitlen - EE93CX6_OPCODE_ERAL_BITLEN);
	mw_cmd(cmd, ee93cx6_addr_bitlen);
	if (ERROR_OK == mw_poll())
	{
		return ERROR_FAIL;
	}
	return commit();
}

WRITE_TARGET_HANDLER(ee93cx6)
{
	uint32_t i;
	RESULT ret = ERROR_OK;
	uint16_t *ptr16;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case EEPROM_CHAR:
		switch (ee93cx6_origination_mode)
		{
		case EE93CX6_MODE_BYTE:
			for (i = 0; i < size; i++)
			{
				if (ERROR_OK != mw_write(addr, ee93cx6_addr_bitlen, buff[i], 8))
				{
					return ERROR_FAIL;
				}
				if (ERROR_OK != mw_poll())
				{
					return ERROR_FAIL;
				}
			}
			ret = commit();
			break;
		case EE93CX6_MODE_WORD:
			ptr16 = (uint16_t *)buff;
			for (i = 0; i < size; i += 2)
			{
				if (ERROR_OK != 
					mw_write(addr, ee93cx6_addr_bitlen, ptr16[i], 16))
				{
					return ERROR_FAIL;
				}
				if (ERROR_OK != mw_poll())
				{
					return ERROR_FAIL;
				}
			}
			ret = commit();
			break;
		default:
			ret = ERROR_FAIL;
			break;
		}
		break;
	default:
		break;
	}
	return ret;
}

READ_TARGET_HANDLER(ee93cx6)
{
	uint32_t i;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case EEPROM_CHAR:
		switch (ee93cx6_origination_mode)
		{
		case EE93CX6_MODE_BYTE:
			for (i = 0; i < size; i++)
			{
				if (ERROR_OK != mw_read(addr, ee93cx6_addr_bitlen, &buff[i], 8))
				{
					return ERROR_FAIL;
				}
			}
			ret = commit();
			break;
		case EE93CX6_MODE_WORD:
			for (i = 0; i < size; i += 2)
			{
				if (ERROR_OK != 
					mw_read(addr, ee93cx6_addr_bitlen, &buff[i], 16))
				{
					return ERROR_FAIL;
				}
			}
			ret = commit();
			break;
		default:
			ret = ERROR_FAIL;
			break;
		}
		break;
	default:
		break;
	}
	return ret;
}


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

#include <string.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"

#include "../dal_cfg.h"
#include "../dal_internal.h"
#include "../mal/mal_internal.h"
#include "../mal/mal.h"
#include "ee93cx6_drv_cfg.h"
#include "ee93cx6_drv.h"

#define EE93CX6_OPCODE_READ					0x02
#define EE93CX6_OPCODE_READ_BITLEN			2
#define EE93CX6_OPCODE_WRITE				0x01
#define EE93CX6_OPCODE_WRITE_BITLEN			2
#define EE93CX6_OPCODE_ERASE				0x03
#define EE93CX6_OPCODE_ERASE_BITLEN			2

#define EE93CX6_OPCODE_WEN					0x03
#define EE93CX6_OPCODE_WEN_BITLEN			4
#define EE93CX6_OPCODE_WDS					0x00
#define EE93CX6_OPCODE_WDS_BITLEN			4
#define EE93CX6_OPCODE_ERAL					0x02
#define EE93CX6_OPCODE_ERAL_BITLEN			4
#define EE93CX6_OPCODE_WRAL					0x01
#define EE93CX6_OPCODE_WRAL_BITLEN			4

static struct ee93cx6_drv_interface_t ee93cx6_drv_ifs;
static struct ee93cx6_drv_param_t ee93cx6_drv_param;

static RESULT ee93cx6_drv_poll(void)
{
	return interfaces->microwire.poll(ee93cx6_drv_ifs.mw_port, 
							EE93CX6_POLL_INTERVAL_US, EE93CX6_POLL_RETRY_CNT);
}

static RESULT ee93cx6_drv_config_interface(void *ifs)
{
	if (NULL == ifs)
	{
		return ERROR_FAIL;
	}
	
	memcpy(&ee93cx6_drv_ifs, ifs, sizeof(ee93cx6_drv_ifs));
	return ERROR_OK;
}

static RESULT ee93cx6_drv_init(void *param)
{
	uint32_t cmd;
	
	if (NULL == param)
	{
		return ERROR_FAIL;
	}
	memcpy(&ee93cx6_drv_param, param, sizeof(ee93cx6_drv_param));
	
	if (EE93CX6_ORIGINATION_BYTE != ee93cx6_drv_param.origination_mode)
	{
		ee93cx6_drv_param.cmd_bitlen += ee93cx6_drv_param.addr_bitlen - 1;
	}
	if (ee93cx6_drv_param.addr_bitlen > 32)
	{
		return ERROR_FAIL;
	}
	if (!ee93cx6_drv_param.iic_khz)
	{
		ee93cx6_drv_param.iic_khz = 2000;
	}
	
	interfaces->microwire.init(ee93cx6_drv_ifs.mw_port);
	interfaces->microwire.config(ee93cx6_drv_ifs.mw_port, 
									ee93cx6_drv_param.iic_khz, 1);
	
	cmd = EE93CX6_OPCODE_WEN << 
				(ee93cx6_drv_param.cmd_bitlen - EE93CX6_OPCODE_WEN_BITLEN);
	interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, cmd, 
				ee93cx6_drv_param.cmd_bitlen, 0, 0, 0, 0, NULL, 0);
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_fini(void)
{
	uint32_t cmd;
	
	cmd = EE93CX6_OPCODE_WDS << 
				(ee93cx6_drv_param.cmd_bitlen - EE93CX6_OPCODE_WDS_BITLEN);
	interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
				cmd, ee93cx6_drv_param.cmd_bitlen, 0, 0, 0, 0, NULL, 0);
	
	interfaces->microwire.fini(ee93cx6_drv_ifs.mw_port);
	return interfaces->peripheral_commit();
}

static RESULT ee93cx6_drv_eraseall_nb_start(void)
{
	uint32_t cmd;
	
	cmd = EE93CX6_OPCODE_ERAL << 
				(ee93cx6_drv_param.cmd_bitlen - EE93CX6_OPCODE_ERAL_BITLEN);
	interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
				cmd, ee93cx6_drv_param.cmd_bitlen, 0, 0, 0, 0, NULL, 0);
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_eraseall_nb_waitready(void)
{
	return ee93cx6_drv_poll();
}

static RESULT ee93cx6_drv_eraseall_nb_end(void)
{
	return ERROR_OK;
}

static RESULT ee93cx6_drv_eraseblock_nb_start(uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_eraseblock_nb(uint64_t address)
{
	switch (ee93cx6_drv_param.origination_mode)
	{
	case EE93CX6_ORIGINATION_BYTE:
		interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
			EE93CX6_OPCODE_ERASE, EE93CX6_OPCODE_ERASE_BITLEN, 
			(uint32_t)address, ee93cx6_drv_param.addr_bitlen, 0, 0, NULL, 0);
		break;
	case EE93CX6_ORIGINATION_WORD:
		interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
			EE93CX6_OPCODE_ERASE, EE93CX6_OPCODE_ERASE_BITLEN, 
			(uint32_t)(address / 2), ee93cx6_drv_param.addr_bitlen - 1, 0, 0, 
			NULL, 0);
		break;
	default:
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_eraseblock_nb_waitready(void)
{
	return ee93cx6_drv_poll();
}

static RESULT ee93cx6_drv_eraseblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT ee93cx6_drv_readblock_nb_start(uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_readblock_nb(uint64_t address, uint8_t *buff)
{
	uint8_t i;
	
	switch (ee93cx6_drv_param.origination_mode)
	{
	case EE93CX6_ORIGINATION_BYTE:
		for (i = 0; i < ee93cx6_drv.capacity.block_size; i++)
		{
			if (ERROR_OK != 
				interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
					EE93CX6_OPCODE_READ, EE93CX6_OPCODE_READ_BITLEN, 
					(uint32_t)(address + i), ee93cx6_drv_param.addr_bitlen, 
					0, 0, &buff[i], 8))
			{
				return ERROR_FAIL;
			}
		}
		break;
	case EE93CX6_ORIGINATION_WORD:
		for (i = 0; i < ee93cx6_drv.capacity.block_size; i += 2)
		{
			if (ERROR_OK != 
				interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
					EE93CX6_OPCODE_READ, EE93CX6_OPCODE_READ_BITLEN, 
					(uint32_t)((address + i) / 2), ee93cx6_drv_param.addr_bitlen - 1, 
					0, 0, &buff[i], 16))
			{
				return ERROR_FAIL;
			}
		}
		break;
	default:
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_readblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT ee93cx6_drv_readblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT ee93cx6_drv_writeblock_nb_start(uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_writeblock_nb(uint64_t address, uint8_t *buff)
{
	uint8_t i;
	uint16_t *ptr16;
	
	switch (ee93cx6_drv_param.origination_mode)
	{
	case EE93CX6_ORIGINATION_BYTE:
		for (i = 0; i < ee93cx6_drv.capacity.block_size; i++)
		{
			if (ERROR_OK != 
				interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
					EE93CX6_OPCODE_WRITE, EE93CX6_OPCODE_WRITE_BITLEN, 
					(uint32_t)(address + i), ee93cx6_drv_param.addr_bitlen, 
					buff[i], 8, NULL, 0))
			{
				return ERROR_FAIL;
			}
		}
		break;
	case EE93CX6_ORIGINATION_WORD:
		ptr16 = (uint16_t *)buff;
		for (i = 0; i < ee93cx6_drv.capacity.block_size; i += 2)
		{
			if (ERROR_OK != 
				interfaces->microwire.transport(ee93cx6_drv_ifs.mw_port, 
					EE93CX6_OPCODE_WRITE, EE93CX6_OPCODE_WRITE_BITLEN, 
					(uint32_t)((address + i) / 2), ee93cx6_drv_param.addr_bitlen - 1, 
					SYS_TO_LE_U16(ptr16[i / 2]), 16, NULL, 0))
			{
				return ERROR_FAIL;
			}
		}
		break;
	default:
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

static RESULT ee93cx6_drv_writeblock_nb_waitready(void)
{
	return ee93cx6_drv_poll();
}

static RESULT ee93cx6_drv_writeblock_nb_end(void)
{
	return ERROR_OK;
}

struct mal_driver_t ee93cx6_drv = 
{
	MAL_IDX_EE93CX6,
	MAL_SUPPORT_ERASEALL | MAL_SUPPORT_ERASEBLOCK | 
	MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_READBLOCK,
	{0, 0},
	
	ee93cx6_drv_config_interface,
	
	ee93cx6_drv_init,
	ee93cx6_drv_fini,
	NULL,
	NULL,
	
	ee93cx6_drv_eraseall_nb_start,
	NULL,
	ee93cx6_drv_eraseall_nb_waitready,
	ee93cx6_drv_eraseall_nb_end,
	
	ee93cx6_drv_eraseblock_nb_start,
	ee93cx6_drv_eraseblock_nb,
	NULL,
	ee93cx6_drv_eraseblock_nb_waitready,
	ee93cx6_drv_eraseblock_nb_end,
	
	ee93cx6_drv_readblock_nb_start,
	ee93cx6_drv_readblock_nb,
	NULL,
	ee93cx6_drv_readblock_nb_waitready,
	ee93cx6_drv_readblock_nb_end,
	
	ee93cx6_drv_writeblock_nb_start,
	ee93cx6_drv_writeblock_nb,
	NULL,
	ee93cx6_drv_writeblock_nb_waitready,
	ee93cx6_drv_writeblock_nb_end
};


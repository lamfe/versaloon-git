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

#include "app_type.h"

#include "interfaces.h"
#include "../mal/mal.h"
#include "../mal/mal_driver.h"

#if DAL_NAND_EN

#include "nand_drv.h"

static vsf_err_t nand_drv_write_command8(struct dal_info_t *info, uint8_t cmd)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.cmd, 1, &cmd, 1);
}

static vsf_err_t nand_drv_write_address8(struct dal_info_t *info, uint8_t addr)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.addr, 1, &addr, 1);
}

static vsf_err_t nand_drv_write_address16(struct dal_info_t *info, uint16_t addr)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	addr = SYS_TO_LE_U32(addr);
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.addr, 1, (uint8_t *)&addr, 2);
}

static vsf_err_t nand_drv_write_address24(struct dal_info_t *info, uint32_t addr)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	addr = SYS_TO_LE_U32(addr);
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.addr, 1, (uint8_t *)&addr, 3);
}

static vsf_err_t nand_drv_write_address32(struct dal_info_t *info, uint32_t addr)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	addr = SYS_TO_LE_U32(addr);
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.addr, 1, (uint8_t *)&addr, 4);
}

static vsf_err_t nand_drv_write_data8(struct dal_info_t *info, uint8_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.data, 1, buff, count);
}

static vsf_err_t nand_drv_write_data16(struct dal_info_t *info, uint16_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	return interfaces->ebi.write(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.data, 2, (uint8_t *)buff, count);
}

static vsf_err_t nand_drv_read_data8(struct dal_info_t *info, uint8_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	return interfaces->ebi.read(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.data, 1, buff, count);
}

static vsf_err_t nand_drv_read_data16(struct dal_info_t *info, uint16_t *buff, 
									uint32_t count)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	
	return interfaces->ebi.read(ifs->ebi_port, 
				ifs->nand_index | EBI_TGTTYP_NAND, 
				param->nand_info.param.addr.data, 2, (uint8_t *)buff, count);
}

static vsf_err_t nand_drv_read_status(struct dal_info_t *info, uint8_t *status)
{
	nand_drv_write_command8(info, 0x70);
	return nand_drv_read_data8(info, status, 1);
}

static vsf_err_t nand_drv_waitready(struct dal_info_t *info, uint8_t *status)
{
	uint8_t tmp_status;
	
	nand_drv_write_command8(info, 0x70);
	do {
		nand_drv_read_data8(info, &tmp_status, 1);
		if (interfaces->peripheral_commit())
		{
			return VSFERR_FAIL;
		}
	} while (!(tmp_status & 0x40));
	
	if (status != NULL)
	{
		*status = tmp_status;
	}
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_init(struct dal_info_t *info)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	struct nand_drv_param_t *nand_drv_param = 
										(struct nand_drv_param_t *)info->param;
	
	interfaces->ebi.init(ifs->ebi_port);
	interfaces->ebi.config(ifs->ebi_port, ifs->nand_index | EBI_TGTTYP_NAND, 
							&nand_drv_param->nand_info);
	
	nand_drv_write_command8(info, 0xFF);
	return nand_drv_waitready(info, NULL);
}

static vsf_err_t nand_drv_getinfo(struct dal_info_t *info)
{
	uint8_t id[4];
	struct nand_drv_info_t *pinfo = (struct nand_drv_info_t *)info->info;
	
	nand_drv_write_command8(info, 0x90);
	nand_drv_write_address8(info, 0x00);
	nand_drv_read_data8(info, id, 4);
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	pinfo->manufacturer_id = id[0];
	pinfo->device_id[0] = id[1];
	pinfo->device_id[1] = id[2];
	pinfo->device_id[2] = id[3];
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_fini(struct dal_info_t *info)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	
	interfaces->ebi.fini(ifs->ebi_port);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_eraseblock_nb(struct dal_info_t *info, uint64_t address)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint32_t address32;
	
	if (param->small_page)
	{
		address32 = (address & 0xFFFFFE00) >> 9;
		nand_drv_write_command8(info, 0x60);
		nand_drv_write_address24(info, address32);
		nand_drv_write_command8(info, 0xD0);
	}
	else
	{
		address32 = (address & 0x0FFF) | ((address << 4) & 0xFFFF0000);
		nand_drv_write_command8(info, 0x60);
		nand_drv_write_address16(info, (uint16_t)(address32 >> 16));
		nand_drv_write_command8(info, 0xD0);
	}
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_eraseblock_nb_isready(struct dal_info_t *info, 
												uint64_t address)
{
	uint8_t status;
	
	REFERENCE_PARAMETER(address);
	if (nand_drv_read_status(info, &status) || interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return (status & 1) ? VSFERR_FAIL :
			((status & 0x40) == 0x40) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t nand_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_readblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_readblock_nb(struct dal_info_t *info, uint64_t address, 
									uint8_t *buff)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint8_t data_width = param->nand_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t read_page_size = mal_info->read_page_size;
	uint32_t address32;
	uint64_t capacity = 
				mal_info->capacity.block_number * mal_info->capacity.block_size;
	
	if ((data_width != 1) && (data_width != 2))
	{
		return VSFERR_FAIL;
	}
	
	if (param->small_page)
	{
		address32 = (address & 0xFFFFFE00) >> 1;
	}
	else
	{
		if (2 == data_width)
		{
			address >>= 1;
		}
		address32 = (address & 0x0FFF) | ((address << 4) & 0xFFFF0000);
	}
	nand_drv_write_command8(info, 0x00);
	if (capacity >= 64 * 1024 * 1024)
	{
		nand_drv_write_address32(info, address32);
	}
	else
	{
		nand_drv_write_address24(info, address32);
	}
	nand_drv_write_command8(info, 0x30);
	interfaces->delay.delayus(0);
	switch (data_width)
	{
	case 1:
		nand_drv_read_data8(info, buff, read_page_size);
		break;
	case 2:
		nand_drv_read_data16(info, (uint16_t *)buff, read_page_size / 2);
		break;
	default:
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_readblock_nb_isready(struct dal_info_t *info, 
											uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_writeblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return VSFERR_NONE;
}

static vsf_err_t nand_drv_writeblock_nb(struct dal_info_t *info, uint64_t address, 
										uint8_t *buff)
{
	struct nand_drv_param_t *param = (struct nand_drv_param_t *)info->param;
	uint8_t data_width = param->nand_info.common_info.data_width / 8;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t write_page_size = mal_info->write_page_size;
	uint32_t address32;
	uint64_t capacity = 
				mal_info->capacity.block_number * mal_info->capacity.block_size;
	
	if ((data_width != 1) && (data_width != 2))
	{
		return VSFERR_FAIL;
	}
	
	if (param->small_page)
	{
		address32 = (address & 0xFFFFFE00) >> 1;
	}
	else
	{
		if (2 == data_width)
		{
			address >>= 1;
		}
		address32 = (address & 0x0FFF) | ((address << 4) & 0xFFFF0000);
	}
	nand_drv_write_command8(info, 0x80);
	if (capacity >= 64 * 1024 * 1024)
	{
		nand_drv_write_address32(info, address32);
	}
	else
	{
		nand_drv_write_address24(info, address32);
	}
	switch (data_width)
	{
	case 1:
		nand_drv_write_data8(info, buff, write_page_size);
		break;
	case 2:
		nand_drv_write_data16(info, (uint16_t *)buff, write_page_size / 2);
		break;
	default:
		return VSFERR_FAIL;
	}
	return nand_drv_write_command8(info, 0x10);
}

static vsf_err_t nand_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	uint8_t status;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	if (nand_drv_read_status(info, &status) || interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	return (status & 1) ? VSFERR_FAIL :
			((status & 0x40) == 0x40) ? VSFERR_NONE : VSFERR_NOT_READY;
}

static vsf_err_t nand_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t nand_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct nand_drv_interface_t *ifs = (struct nand_drv_interface_t *)info->ifs;
	
	ifs->ebi_port = buff[0];
	ifs->nand_index = buff[1];
	return VSFERR_NONE;
}
#endif

struct mal_driver_t nand_drv = 
{
	{
		"nand",
#if DAL_INTERFACE_PARSER_EN
		"ebi:%1dnand:%1d",
		nand_drv_parse_interface,
#endif
	},
	
	MAL_IDX_NAND,
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_ERASEBLOCK,
	
	nand_drv_init,
	nand_drv_fini,
	nand_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	NULL, NULL, NULL, NULL,
	
	nand_drv_eraseblock_nb_start,
	nand_drv_eraseblock_nb,
	nand_drv_eraseblock_nb_isready,
	NULL,
	nand_drv_eraseblock_nb_end,
	
	nand_drv_readblock_nb_start,
	nand_drv_readblock_nb,
	nand_drv_readblock_nb_isready,
	NULL,
	nand_drv_readblock_nb_end,
	
	nand_drv_writeblock_nb_start,
	nand_drv_writeblock_nb,
	nand_drv_writeblock_nb_isready,
	NULL,
	nand_drv_writeblock_nb_end
};

#endif

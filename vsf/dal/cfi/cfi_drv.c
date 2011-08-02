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

#include "port.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"

#include "interfaces.h"
#include "../mal/mal.h"
#include "../mal/mal_driver.h"
#include "cfi_drv.h"

static RESULT cfi_wait_busy(struct dal_info_t *info, uint64_t address)
{
	uint16_t cur_status, orig_status;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	REFERENCE_PARAMETER(address);
	
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							(uint32_t)address, 2, (uint8_t *)&orig_status, 1);
	
	do {
		interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
								0, 2, (uint8_t *)&cur_status, 1);
		interfaces->peripheral_commit();
		
		if ((cur_status ^ orig_status) & 0x0040)
		{
			if (cur_status & 0x0020)
			{
				interfaces->ebi.read(ifs->ebi_port, 
										ifs->nor_index | EBI_TGTTYP_NOR, 0, 2, 
										(uint8_t *)&orig_status, 1);
				interfaces->ebi.read(ifs->ebi_port, 
										ifs->nor_index | EBI_TGTTYP_NOR, 0, 2, 
										(uint8_t *)&cur_status, 1);
				interfaces->peripheral_commit();
				if ((cur_status ^ orig_status) & 0x0040)
				{
					return ERROR_FAIL;
				}
				else
				{
					return ERROR_OK;
				}
			}
		}
		else
		{
			break;
		}
		
		orig_status = cur_status;
	} while (1);
	return ERROR_OK;
}

static RESULT cfi_drv_init(struct dal_info_t *info)
{
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	struct cfi_drv_param_t *cfi_drv_param = 
										(struct cfi_drv_param_t *)info->param;
	
	interfaces->ebi.init(ifs->ebi_port);
	interfaces->ebi.config(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							&cfi_drv_param->nor_info);
	return ERROR_OK;
}

static RESULT cfi_drv_getinfo(struct dal_info_t *info)
{
	uint16_t cfi_info[128];
	uint16_t manufacturer_id;
	uint16_t cmd;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	struct cfi_drv_info_t *pinfo = (struct cfi_drv_info_t *)info->info;
	struct mal_info_t *cfi_mal_info = (struct mal_info_t *)info->extra;
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x90);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0000 << 1, 2, (uint8_t *)&manufacturer_id, 1);
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0001 << 1, 2, (uint8_t *)&pinfo->device_id[0], 1);
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x000E << 1, 2, (uint8_t *)&pinfo->device_id[1], 1);
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x000F << 1, 2, (uint8_t *)&pinfo->device_id[2], 1);
	interfaces->peripheral_commit();
	
	pinfo->manufacturer_id = (uint8_t)manufacturer_id;
	pinfo->device_id[0] = LE_TO_SYS_U16(pinfo->device_id[0]);
	pinfo->device_id[1] = LE_TO_SYS_U16(pinfo->device_id[1]);
	pinfo->device_id[2] = LE_TO_SYS_U16(pinfo->device_id[2]);
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0xF0);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0, 2, (uint8_t *)&cmd, 1);
	
	cmd = SYS_TO_LE_U16(0x98);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0055 << 1, 2, (uint8_t *)&cmd, 1);
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
						0x0010 << 1, 2, (uint8_t *)cfi_info, dimof(cfi_info));
	if ((ERROR_OK != interfaces->peripheral_commit()) || 
		(cfi_info[0] != 'Q') || (cfi_info[1] != 'R') || (cfi_info[2] != 'Y'))
	{
		return ERROR_FAIL;
	}
	if (!cfi_mal_info->capacity.block_number || 
		!cfi_mal_info->capacity.block_size)
	{
		cfi_mal_info->capacity.block_number = cfi_info[0x1D] + 1;
		cfi_mal_info->capacity.block_size = 
						((uint64_t)1 << cfi_info[0x17]) / (cfi_info[0x1D] + 1);
	}
	cfi_mal_info->write_page_size = 1 << cfi_info[0x1A];
	cfi_mal_info->erase_page_size = (uint32_t)cfi_mal_info->capacity.block_size;
	cfi_mal_info->read_page_size = 0;
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0xF0);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0, 2, (uint8_t *)&cmd, 1);
	return ERROR_OK;
}

static RESULT cfi_drv_fini(struct dal_info_t *info)
{
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	interfaces->ebi.fini(ifs->ebi_port);
	return ERROR_OK;
}

static RESULT cfi_drv_eraseall_nb_start(struct dal_info_t *info)
{
	uint16_t cmd;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x80);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x10);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	return ERROR_OK;
}

static RESULT cfi_drv_eraseall_nb_isready(struct dal_info_t *info, bool *ready)
{
	uint16_t val1, val2;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0000 << 1, 2, (uint8_t *)&val1, 1);
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0000 << 1, 2, (uint8_t *)&val2, 1);
	interfaces->peripheral_commit();
	
	*ready = ((val1 ^ val2) & 0x0040) == 0;
	return ERROR_OK;
}

static RESULT cfi_drv_eraseall_nb_waitready(struct dal_info_t *info)
{
	return cfi_wait_busy(info, 0);
}

static RESULT cfi_drv_eraseall_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT cfi_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return ERROR_OK;
}

static RESULT cfi_drv_eraseblock_nb(struct dal_info_t *info, uint64_t address)
{
	uint16_t cmd;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x80);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x30);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							(uint32_t)address, 2, (uint8_t *)&cmd, 1);
	return ERROR_OK;
}

static RESULT cfi_drv_eraseblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, bool *ready)
{
	uint16_t val1, val2;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	REFERENCE_PARAMETER(address);
	
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0000 << 1, 2, (uint8_t *)&val1, 1);
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0000 << 1, 2, (uint8_t *)&val2, 1);
	interfaces->peripheral_commit();
	
	*ready = ((val1 ^ val2) & 0x0040) == 0;
	return ERROR_OK;
}

static RESULT cfi_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT cfi_drv_readblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	uint16_t cmd;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0xF0);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0, 2, (uint8_t *)&cmd, 1);
	
	return ERROR_OK;
}

static RESULT cfi_drv_readblock_nb(struct dal_info_t *info, uint64_t address, 
									uint8_t *buff)
{
	uint32_t count, i, cur_count;
	uint8_t data_width;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	
	data_width = param->nor_info.common_info.data_width / 8;
	count = (uint32_t)mal_info->capacity.block_size / data_width;
	
	i = 0;
	while (i < count)
	{
		cur_count = (count > 1024) ? 1024 : count;
		interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
				(uint32_t)address, data_width, buff, cur_count);
		address += cur_count * data_width;
		buff += cur_count * data_width;
		i += cur_count;
	}
	return interfaces->peripheral_commit();
}

static RESULT cfi_drv_readblock_nb_isready(struct dal_info_t *info, 
								uint64_t address, uint8_t *buff, bool *ready)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	*ready = true;
	return ERROR_OK;
}

static RESULT cfi_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT cfi_drv_writeblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return ERROR_OK;
}

static RESULT cfi_drv_writeblock_nb(struct dal_info_t *info, uint64_t address, 
									uint8_t *buff)
{
	uint32_t cmd;
	uint8_t data_width;
	struct cfi_drv_param_t *param = (struct cfi_drv_param_t *)info->param;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t write_page_size = mal_info->write_page_size;
	
	data_width = param->nor_info.common_info.data_width / 8;
	
	cmd = SYS_TO_LE_U16(0xAA);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x0555 << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x55);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							0x02AA << 1, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(0x25);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							(uint32_t)address, 2, (uint8_t *)&cmd, 1);
	cmd = SYS_TO_LE_U16(write_page_size / 2 - 1);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							(uint32_t)address, 2, (uint8_t *)&cmd, 1);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
				(uint32_t)address, 2, buff, write_page_size / 2);
	cmd = SYS_TO_LE_U16(0x29);
	interfaces->ebi.write(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							(uint32_t)address, 2, (uint8_t *)&cmd, 1);
	
	return ERROR_OK;
}

static RESULT cfi_drv_writeblock_nb_isready(struct dal_info_t *info, 
								uint64_t address, uint8_t *buff, bool *ready)
{
	uint16_t status;
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	uint32_t write_page_size = mal_info->write_page_size;
	
	interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
							(uint32_t)address + write_page_size - 2, 
							2, (uint8_t *)&status, 1);
	interfaces->peripheral_commit();
	
	if (((uint16_t *)buff)[write_page_size / 2 - 1] == status)
	{
		*ready = true;
	}
	else if ((status & 0x20) || (status & 0x02))
	{
		interfaces->ebi.read(ifs->ebi_port, ifs->nor_index | EBI_TGTTYP_NOR, 
								(uint32_t)address + write_page_size - 2, 
								2, (uint8_t *)&status, 1);
		if ((ERROR_OK != interfaces->peripheral_commit()) || 
			(((uint16_t *)buff)[write_page_size / 2 - 1] != status))
		{
			return ERROR_FAIL;
		}
	}
	
	return ERROR_OK;
}

static RESULT cfi_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

#if DAL_INTERFACE_PARSER_EN
static RESULT cfi_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct cfi_drv_interface_t *ifs = (struct cfi_drv_interface_t *)info->ifs;
	
	ifs->ebi_port = buff[0];
	ifs->nor_index = buff[1];
	return ERROR_OK;
}
#endif

struct mal_driver_t cfi_drv = 
{
	{
		"cfi",
#if DAL_INTERFACE_PARSER_EN
		"ebi:%1dnor:%1d",
		cfi_drv_parse_interface,
#endif
	},
	
	MAL_IDX_CFI,
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK | MAL_SUPPORT_ERASEBLOCK,
	
	cfi_drv_init,
	cfi_drv_fini,
	cfi_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
	cfi_drv_eraseall_nb_start,
	cfi_drv_eraseall_nb_isready,
	cfi_drv_eraseall_nb_waitready,
	cfi_drv_eraseall_nb_end,
	
	cfi_drv_eraseblock_nb_start,
	cfi_drv_eraseblock_nb,
	cfi_drv_eraseblock_nb_isready,
	cfi_wait_busy,
	cfi_drv_eraseblock_nb_end,
	
	cfi_drv_readblock_nb_start,
	cfi_drv_readblock_nb,
	cfi_drv_readblock_nb_isready,
	NULL,
	cfi_drv_readblock_nb_end,
	
	cfi_drv_writeblock_nb_start,
	cfi_drv_writeblock_nb,
	cfi_drv_writeblock_nb_isready,
	NULL,
	cfi_drv_writeblock_nb_end
};


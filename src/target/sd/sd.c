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
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "dal/mal/mal.h"
#include "dal/sd/sd_common.h"
#include "dal/sd/sd_spi_drv.h"

#include "sd.h"
#include "sd_internal.h"

#define CUR_TARGET_STRING			SD_STRING

#define SD_SPI_STRING				"sd_spi"
#define SD_SDIO_STRING				"sd_sdio"

struct program_area_map_t sd_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_WR},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t sd_program_mode[] =
{
	{'s', SET_FREQUENCY, IFS_SPI | IFS_GPIO},
	{'d', SET_FREQUENCY, IFS_SDIO},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(sd);
LEAVE_PROGRAM_MODE_HANDLER(sd);
ERASE_TARGET_HANDLER(sd);
WRITE_TARGET_HANDLER(sd);
READ_TARGET_HANDLER(sd);
const struct program_functions_t sd_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(sd),
	LEAVE_PROGRAM_MODE_FUNCNAME(sd),
	ERASE_TARGET_FUNCNAME(sd),
	WRITE_TARGET_FUNCNAME(sd),
	READ_TARGET_FUNCNAME(sd)
};

VSS_HANDLER(sd_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set IIC frequency, in KHz\n\n",
			CUR_TARGET_STRING);
	return ERROR_OK;
}

const struct vss_cmd_t sd_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				sd_help),
	VSS_CMD_END
};






static struct sd_info_t sd_info;
static struct sd_spi_drv_interface_t sd_spi_drv_ifs;
static struct mal_info_t sd_mal_info =
{
	{0, 0}, &sd_info
};
static struct dal_info_t sd_dal_info =
{
	NULL,
	NULL,
	NULL,
	&sd_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(sd)
{
	struct chip_param_t *param = context->param;
	struct program_info_t *pi = context->pi;
	uint64_t capacity;
	uint32_t page_size;
	
	if (ERROR_OK != dal_init(context->prog))
	{
		return ERROR_FAIL;
	}
	
	sd_dal_info.ifs = &sd_spi_drv_ifs;
	if (pi->ifs_indexes != NULL)
	{
		if (ERROR_OK != dal_config_interface(SD_SPI_STRING, pi->ifs_indexes,
												&sd_dal_info))
		{
			return ERROR_FAIL;
		}
	}
	else
	{
		sd_spi_drv_ifs.cs_port = 0;
		sd_spi_drv_ifs.cs_pin = GPIO_SRST;
		sd_spi_drv_ifs.spi_port = 0;
	}
	
	if (ERROR_OK != mal.init(MAL_IDX_SD_SPI, &sd_dal_info))
	{
		return ERROR_FAIL;
	}
	capacity = sd_mal_info.capacity.block_number *
				sd_mal_info.capacity.block_size;
	LOG_INFO("Card capacity: %d MB", (int)(capacity >> 20));
	switch (sd_info.cardtype)
	{
	case SD_CARDTYPE_SD_V1:
		LOG_INFO("Card type: SD V1");
		break;
	case SD_CARDTYPE_SD_V2:
		LOG_INFO("Card type: SD V2");
		break;
	case SD_CARDTYPE_SD_V2HC:
		LOG_INFO("Card type: SD V2HC");
		break;
	case SD_CARDTYPE_MMC:
		LOG_INFO("Card type: MMC");
		break;
	case SD_CARDTYPE_MMC_HC:
		LOG_INFO("Card type: MMC HC");
		break;
	default:
		LOG_INFO("Card type: UNKNOWN");
		break;
	}
	
	page_size = param->chip_areas[APPLICATION_IDX].page_size = 4 * 1024;
	if (capacity > (16 << 20))
	{
		param->chip_areas[APPLICATION_IDX].page_num =
											(uint32_t)((16 << 20) / page_size);
	}
	else
	{
		param->chip_areas[APPLICATION_IDX].page_num =
											(uint32_t)(capacity / page_size);
	}
	param->chip_areas[APPLICATION_IDX].size =
		param->chip_areas[APPLICATION_IDX].page_num *
		param->chip_areas[APPLICATION_IDX].page_size;
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(MAL_IDX_SD_SPI, &sd_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	if (ERROR_OK != mal.eraseall(MAL_IDX_SD_SPI, &sd_dal_info))
	{
		return ERROR_FAIL;
	}
	return dal_commit();
}

WRITE_TARGET_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (size % 512)
		{
			return ERROR_FAIL;
		}
		size /= 512;
		
		if (ERROR_OK != mal.writeblock(MAL_IDX_SD_SPI, &sd_dal_info,
										addr, buff, size))
		{
			return ERROR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return ERROR_FAIL;
	}
}

READ_TARGET_HANDLER(sd)
{
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		memset(buff, 0, size);
		return ERROR_OK;
		break;
	case APPLICATION_CHAR:
		if (size % 512)
		{
			return ERROR_FAIL;
		}
		size /= 512;
		
		if (ERROR_OK != mal.readblock(MAL_IDX_SD_SPI, &sd_dal_info,
										addr, buff, size))
		{
			return ERROR_FAIL;
		}
		return ERROR_OK;
		break;
	default:
		return ERROR_FAIL;
	}
}


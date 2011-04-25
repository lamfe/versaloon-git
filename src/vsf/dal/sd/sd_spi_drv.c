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

#include "../mal/mal.h"
#include "../mal/mal_internal.h"
#include "sd_common.h"
#include "sd_spi_drv_cfg.h"
#include "sd_spi_drv.h"

static struct sd_spi_drv_interface_t sd_spi_drv_ifs;
static enum sd_cardtype_t sd_cardtype = SD_CARDTYPE_NONE;

static RESULT sd_spi_drv_cs_assert(void)
{
	return interfaces->gpio.config(sd_spi_drv_ifs.cs_port, 
		sd_spi_drv_ifs.cs_pin, sd_spi_drv_ifs.cs_pin, 0, 0);
}

static RESULT sd_spi_drv_cs_deassert(void)
{
	return interfaces->gpio.config(sd_spi_drv_ifs.cs_port, 
		sd_spi_drv_ifs.cs_pin, 0, sd_spi_drv_ifs.cs_pin, sd_spi_drv_ifs.cs_pin);
}

static RESULT sd_spi_drv_send_empty_bytes(uint8_t cnt)
{
	uint8_t byte_0xFF = 0xFF;
	
	while (cnt--)
	{
		interfaces->spi.io(sd_spi_drv_ifs.spi_port, &byte_0xFF, NULL, 1);
	}
	return ERROR_OK;
}

static RESULT sd_spi_cmd(uint8_t cmd, uint32_t arg, uint8_t *respr1)
{
	uint8_t cmd_pkt[6], resp = 0xFF;
	uint16_t retry;
	
	// write command
	cmd_pkt[0] = cmd | 0x40;
	cmd_pkt[1] = (arg >> 24) & 0xFF;
	cmd_pkt[2] = (arg >> 16) & 0xFF;
	cmd_pkt[3] = (arg >>  8) & 0xFF;
	cmd_pkt[4] = (arg >>  0) & 0xFF;
	cmd_pkt[5] = (sd_spi_calc_chksum(&cmd_pkt[0], 5) << 1) | 0x01;
	interfaces->spi.io(sd_spi_drv_ifs.spi_port, cmd_pkt, NULL, 6);
	
	// read resp
	retry = 0;
	while (retry++ < SD_SPI_CMD_TIMEOUT)
	{
		interfaces->spi.io(sd_spi_drv_ifs.spi_port, NULL, &resp, 1);
		if (ERROR_OK != interfaces->peripheral_commit())
		{
			return ERROR_FAIL;
		}
		if (!(resp & 0x80))
		{
			break;
		}
	}
	if (respr1 != NULL)
	{
		*respr1 = resp;
	}
	return (SD_SPI_CMD_TIMEOUT == retry) ? ERROR_OK : ERROR_FAIL;
}

static RESULT sd_spi_cmdr(uint8_t cmd, uint32_t arg, uint8_t *respr1, 
							uint8_t *respr3_7)
{
	uint8_t resp_r1 = 0xFF;
	
	sd_spi_drv_cs_assert();
	if (ERROR_OK != sd_spi_cmd(cmd, arg, &resp_r1))
	{
		sd_spi_drv_cs_deassert();
		return ERROR_FAIL;
	}
	if (respr1 != NULL)
	{
		*respr1 = resp_r1;
	}
	
	if (respr3_7 != NULL)
	{
		if ((resp_r1 != SD_R1_NONE) && (resp_r1 != SD_R1_IN_IDLE_STATE))
		{
			sd_spi_drv_cs_deassert();
			return ERROR_FAIL;
		}
		interfaces->spi.io(sd_spi_drv_ifs.spi_port, NULL, respr3_7, 4);
	}
	sd_spi_drv_cs_deassert();
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_acmdr1(uint8_t acmd, uint32_t arg, uint8_t *respr1)
{
	uint8_t resp_r1;
	RESULT ret;
	
	sd_spi_drv_cs_assert();
	if ((ERROR_OK != sd_spi_cmd(SD_CMD_APP_CMD, 0, &resp_r1)) || 
		((resp_r1 != SD_R1_NONE) && (resp_r1 != SD_R1_IN_IDLE_STATE)))
	{
		sd_spi_drv_cs_deassert();
		return ERROR_FAIL;
	}
	sd_spi_drv_send_empty_bytes(1);
	ret = sd_spi_cmd(acmd, arg, &resp_r1);
	sd_spi_drv_cs_deassert();
	if (respr1 != NULL)
	{
		*respr1 = resp_r1;
	}
	return ret;
}

static RESULT sd_spi_wait_for_start(void)
{
	uint32_t clks;
	uint8_t data;
	
	clks = 0;
	data = 0xFF;
	while (clks++ < 312500)
	{
		interfaces->spi.io(sd_spi_drv_ifs.spi_port, NULL, &data, 1);
		if (ERROR_OK != interfaces->peripheral_commit())
		{
			return ERROR_FAIL;
		}
		if (data != 0xFF)
		{
			return (SD_TOKEN_START_BLK == data) ? ERROR_OK : ERROR_FAIL;
		}
	}
	return ERROR_FAIL;
}

static RESULT sd_spi_cmdrdata(uint8_t cmd, uint32_t arg, uint8_t *data, 
								uint32_t len)
{
	uint8_t crc[2];
	uint8_t resp;
	
	sd_spi_drv_cs_assert();
	if ((ERROR_OK != sd_spi_cmd(cmd, arg, &resp)) || (resp != SD_R1_NONE) || 
		(ERROR_OK != sd_spi_wait_for_start()))
	{
		sd_spi_drv_cs_deassert();
		return ERROR_FAIL;
	}
	interfaces->spi.io(sd_spi_drv_ifs.spi_port, NULL, data, (uint16_t)len);
	interfaces->spi.io(sd_spi_drv_ifs.spi_port, NULL, crc, 2);
	sd_spi_drv_cs_deassert();
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_config_interface(void *ifs)
{
	if (NULL == ifs)
	{
		return ERROR_FAIL;
	}
	
	memcpy(&sd_spi_drv_ifs, ifs, sizeof(sd_spi_drv_ifs));
	return ERROR_OK;
}

static RESULT sd_spi_drv_init(void *param)
{
	uint8_t resp_r1 = 0xFF, resp_r3to7[4];
	uint16_t retry;
	uint32_t ocr;
	struct sd_info_t sd_info;
	
	interfaces->gpio.init(sd_spi_drv_ifs.cs_port);
	interfaces->gpio.config(sd_spi_drv_ifs.cs_port, sd_spi_drv_ifs.cs_pin, 
							0, sd_spi_drv_ifs.cs_pin, sd_spi_drv_ifs.cs_pin);
	interfaces->spi.init(sd_spi_drv_ifs.spi_port);
	// use slowest spi speed when initializing
	interfaces->spi.config(sd_spi_drv_ifs.spi_port, 0, SPI_CPOL_HIGH, 
							SPI_CPHA_2EDGE, SPI_MSB_FIRST);
	
	// SD Init
	sd_cardtype = SD_CARDTYPE_NONE;
	
	retry = 0;
	while (retry++ < SD_SPI_CMD_TIMEOUT)
	{
		sd_spi_drv_cs_assert();
		sd_spi_drv_send_empty_bytes(20);
		if (ERROR_OK != sd_spi_cmd(SD_CMD_GO_IDLE_STATE, 0, &resp_r1))
		{
			sd_spi_drv_cs_deassert();
			return ERROR_FAIL;
		}
		sd_spi_drv_cs_deassert();
		
		if (SD_R1_IN_IDLE_STATE == resp_r1)
		{
			break;
		}
		interfaces->delay.delayms(20);
	}
	if (resp_r1 != SD_R1_IN_IDLE_STATE)
	{
		sd_cardtype = SD_CARDTYPE_NONE;
		return ERROR_FAIL;
	}
	
	interfaces->delay.delayms(100);
	
	// detect card type
	// send CMD8 to get card op
	if (ERROR_OK != sd_spi_cmdr(SD_CMD_SEND_IF_COND, 
		SD_CMD8_VHS_27_36_V | SD_CMD8_CHK_PATTERN, &resp_r1, resp_r3to7))
	{
		return ERROR_FAIL;
	}
	if ((resp_r1 == SD_R1_IN_IDLE_STATE) && 
		(resp_r3to7[3] == SD_CMD8_CHK_PATTERN))
	{
		sd_cardtype = SD_CARDTYPE_SD_V2;
	}
	else
	{
		sd_cardtype = SD_CARDTYPE_SD_V1;
	}
	
	// send acmd41 to get card status
	retry = 0;
	resp_r1 = 0xFF;
	while (retry++ < SD_SPI_CMD_TIMEOUT)
	{
		if (ERROR_OK != sd_spi_acmdr1(SD_ACMD_SD_SEND_OP_COND, SD_ACMD41_HCS, 
										&resp_r1))
		{
			return ERROR_FAIL;
		}
		if (resp_r1 == SD_R1_NONE)
		{
            break;
        }
		interfaces->delay.delayms(20);
	}
	
	// send cmd1 for MMC card
	if (resp_r1 != SD_R1_NONE)
	{
		retry = 0;
		while (retry++ < SD_SPI_CMD_TIMEOUT)
		{
			if (ERROR_OK != sd_spi_cmdr(SD_CMD_SEND_OP_COND, 0, &resp_r1, NULL))
			{
				return ERROR_FAIL;
			}
			if (resp_r1 == SD_R1_NONE)
			{
			    break;
			}
		}
		if (resp_r1 != SD_R1_NONE)
		{
			sd_cardtype = SD_CARDTYPE_NONE;
			return ERROR_FAIL;
		}

		sd_cardtype = SD_CARDTYPE_MMC;
	}
	
	// send cmd58 to get card ocr
	retry = 0;
	while (retry++ < SD_SPI_CMD_TIMEOUT)
	{
		if (ERROR_OK != sd_spi_cmdr(SD_CMD_READ_OCR, 0x40000000, 
									(uint8_t*)&ocr, resp_r3to7))
		{
			return ERROR_FAIL;
		}
		ocr = BE_TO_SYS_U32(ocr);
		if ((SD_R1_NONE == resp_r1) && (ocr & SD_OCR_BUSY) && 
			(ocr & SD_OCR_CCS))
		{
			if (SD_CARDTYPE_SD_V2 == sd_cardtype)
			{
				sd_cardtype = SD_CARDTYPE_SD_V2HC;
			}
			else
			{
				sd_cardtype = SD_CARDTYPE_MMC_HC;
			}
		}
	}
	
	if ((NULL == sd_spi_drv.getinfo) || 
		(ERROR_OK != sd_spi_drv.getinfo(&sd_info)) || 
		(ERROR_OK != interfaces->spi.config(sd_spi_drv_ifs.spi_port, 
											sd_info.frequency_mHz, 
											SPI_CPOL_HIGH, 
											SPI_CPHA_2EDGE, 
											SPI_MSB_FIRST)) || 
		(ERROR_OK != interfaces->peripheral_commit()) ||
		(ERROR_OK != sd_spi_cmdr(SD_CMD_SET_BLOCKLEN, 512, &resp_r1, NULL)))
	{
		return ERROR_FAIL;
	}
	sd_spi_drv.capacity = sd_info.capacity;
	return ERROR_OK;
}

static RESULT sd_spi_drv_fini(void)
{
	interfaces->gpio.fini(sd_spi_drv_ifs.cs_port);
	interfaces->spi.fini(sd_spi_drv_ifs.spi_port);
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_getinfo(void *info)
{
	uint8_t csd[16];
	
	if ((ERROR_OK != sd_spi_cmdrdata(SD_CMD_SEND_CSD, 0, csd, 16)) || 
		(ERROR_OK != sd_spi_cmdrdata(SD_CMD_SEND_CID, 0, 
									((struct sd_info_t *)info)->cid, 16)) || 
		(ERROR_OK != sd_parse_csd(csd, sd_cardtype, (struct sd_info_t *)info)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_end(void)
{
	return ERROR_OK;
}

#if DAL_INTERFACE_PARSER_EN
static RESULT sd_spi_drv_parse_interface(uint8_t *buff)
{
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	sd_spi_drv_ifs.spi_port = buff[0];
	sd_spi_drv_ifs.cs_port = buff[1];
	sd_spi_drv_ifs.cs_pin = *(uint32_t *)&buff[2];
	return ERROR_OK;
}
#endif

struct mal_driver_t sd_spi_drv = 
{
	{
		"sd_spi",
#if DAL_INTERFACE_PARSER_EN
		"spi:%1dcs:%1d,%4x",
		sd_spi_drv_parse_interface,
#endif
		sd_spi_drv_config_interface,
	},
	
	MAL_IDX_SD_SPI,
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	{0, 0},
	
	sd_spi_drv_init,
	sd_spi_drv_fini,
	sd_spi_getinfo,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	
	sd_spi_drv_readblock_nb_start,
	sd_spi_drv_readblock_nb,
	sd_spi_drv_readblock_nb_isready,
	NULL,
	sd_spi_drv_readblock_nb_end,
	
	sd_spi_drv_writeblock_nb_start,
	sd_spi_drv_writeblock_nb,
	sd_spi_drv_writeblock_nb_isready,
	NULL,
	sd_spi_drv_writeblock_nb_end
};


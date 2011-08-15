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

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces.h"
#include "../mal/mal.h"
#include "../mal/mal_driver.h"
#include "sd_common.h"
#include "sd_spi_drv_cfg.h"
#include "sd_spi_drv.h"

static RESULT sd_spi_drv_cs_assert(struct sd_spi_drv_interface_t *ifs)
{
	return interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, ifs->cs_pin, 0, 
									0);
}

static RESULT sd_spi_drv_cs_deassert(struct sd_spi_drv_interface_t *ifs)
{
	return interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, 0, ifs->cs_pin, 
									ifs->cs_pin);
}

static RESULT sd_spi_wait_datatoken(struct sd_spi_drv_interface_t *ifs, 
									uint8_t *datatoken)
{
	uint32_t retry = 0;
	uint8_t tmp_0xFF = 0xFF;
	
	while (retry < 312500)
	{
		interfaces->spi.io(ifs->spi_port, &tmp_0xFF, datatoken, 1);
		if (ERROR_OK != interfaces->peripheral_commit())
		{
			return ERROR_FAIL;
		}
		if (*datatoken != 0xFF)
		{
			return ERROR_OK;
		}
	}
	if (retry == 312500)
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT sd_spi_drv_send_empty_bytes(struct sd_spi_drv_interface_t *ifs, 
											uint8_t cnt)
{
	uint8_t byte_0xFF = 0xFF;
	
	while (cnt--)
	{
		interfaces->spi.io(ifs->spi_port, &byte_0xFF, NULL, 1);
	}
	return ERROR_OK;
}

static RESULT sd_spi_transact_init(struct sd_spi_drv_interface_t *ifs)
{
	interfaces->gpio.init(ifs->cs_port);
	interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, 0, ifs->cs_pin, 
							ifs->cs_pin);
	interfaces->spi.init(ifs->spi_port);
	// use slowest spi speed when initializing
	interfaces->spi.config(ifs->spi_port, 100, 
							SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER);
	return sd_spi_drv_send_empty_bytes(ifs, 20);
}

static RESULT sd_spi_transact_fini(struct sd_spi_drv_interface_t *ifs)
{
	interfaces->gpio.fini(ifs->cs_port);
	return interfaces->spi.fini(ifs->spi_port);
}

static RESULT sd_spi_transact_start(struct sd_spi_drv_interface_t *ifs)
{
	sd_spi_drv_cs_assert(ifs);
	return sd_spi_drv_send_empty_bytes(ifs, 1);
}

static RESULT sd_spi_transact_cmd(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t cmd, uint32_t arg, uint32_t block_num)
{
	uint8_t cmd_pkt[6];
	
	REFERENCE_PARAMETER(block_num);
	REFERENCE_PARAMETER(token);
	
	cmd_pkt[0] = cmd | 0x40;
	cmd_pkt[1] = (arg >> 24) & 0xFF;
	cmd_pkt[2] = (arg >> 16) & 0xFF;
	cmd_pkt[3] = (arg >>  8) & 0xFF;
	cmd_pkt[4] = (arg >>  0) & 0xFF;
	cmd_pkt[5] = (sd_spi_cmd_chksum(&cmd_pkt[0], 5) << 1) | 0x01;
	return interfaces->spi.io(ifs->spi_port, cmd_pkt, NULL, 6);
}

static RESULT sd_spi_transact_cmd_isready(struct sd_spi_drv_interface_t *ifs, 
		bool *ready, uint16_t token, uint8_t *resp, uint8_t *resp_buff)
{
	uint8_t tmp_0xFF = 0xFF;
	
	*ready = false;
	interfaces->spi.io(ifs->spi_port, &tmp_0xFF, resp, 1);
	if (ERROR_OK != interfaces->peripheral_commit())
	{
		return ERROR_FAIL;
	}
	if (token & SD_TRANSTOKEN_RESP_CHECKBUSY)
	{
//		if (!(*resp & 0x80))
		if (*resp != 0xFF)
		{
			*ready = true;
		}
		return ERROR_OK;
	}
	if (!(*resp & 0x80))
	{
		uint8_t len;
		
		if (*resp & SD_CS8_ERROR_MASK)
		{
			return ERROR_FAIL;
		}
		
		*ready = true;
		switch (token & SD_TRANSTOKEN_RESP_MASK)
		{
		case SD_TRANSTOKEN_RESP_R2:
			len = 1;
			break;
		case SD_TRANSTOKEN_RESP_R3:
		case SD_TRANSTOKEN_RESP_R7:
			len = 4;
			break;
		default:
			len = 0;
			break;
		}
		if (len)
		{
			interfaces->spi.io(ifs->spi_port, NULL, resp_buff, len);
		}
	}
	return ERROR_OK;
}

static RESULT sd_spi_transact_cmd_waitready(struct sd_spi_drv_interface_t *ifs, 
		bool *ready, uint16_t token, uint8_t *resp, uint8_t *resp_buff)
{
	uint32_t retry;
	
	*ready = false;
	retry = 32;
	do {
		if ((ERROR_OK != sd_spi_transact_cmd_isready(ifs, ready, token, resp, 
													resp_buff)) || 
			(!--retry))
		{
			return ERROR_FAIL;
		}
	} while (!*ready);
	
	if (token & SD_TRANSTOKEN_RESP_CHECKBUSY)
	{
		uint8_t tmp_0xFF = 0xFF, tmp;
		
		retry = 0;
		while (retry < 312500)
		{
			interfaces->spi.io(ifs->spi_port, &tmp_0xFF, &tmp, 1);
			if (ERROR_OK != interfaces->peripheral_commit())
			{
				return ERROR_FAIL;
			}
			if (0xFF == tmp)
			{
				return ERROR_OK;
			}
		}
		if (retry == 312500)
		{
			return ERROR_FAIL;
		}
	}
	
	return ERROR_OK;
}

static RESULT sd_spi_transact_datablock_init(
		struct sd_spi_drv_interface_t *ifs, uint16_t token, uint8_t *buffer, 
		uint8_t data_token)
{
	REFERENCE_PARAMETER(buffer);
	
	if ((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_IN)
	{
		uint8_t tmp;
		
		if ((ERROR_OK != sd_spi_wait_datatoken(ifs, &tmp)) || 
			(tmp != data_token))
		{
			return ERROR_FAIL;
		}
	}
	else
	{
		interfaces->spi.io(ifs->spi_port, &data_token, NULL, 1);
	}
	return ERROR_OK;
}

static RESULT sd_spi_transact_datablock(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t *buffer, uint16_t size)
{
	uint16_t dummy_crc16;
	
	if ((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_IN)
	{
		interfaces->spi.io(ifs->spi_port, NULL, buffer, size);
		interfaces->spi.io(ifs->spi_port, NULL, (uint8_t *)&dummy_crc16, 2);
	}
	else
	{
		interfaces->spi.io(ifs->spi_port, buffer, NULL, size);
		interfaces->spi.io(ifs->spi_port, (uint8_t *)&dummy_crc16, NULL, 2);
	}
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_transact_datablock_isready(
		struct sd_spi_drv_interface_t *ifs, bool *ready, uint16_t token)
{
	uint8_t resp;
	
	if ((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_IN)
	{
		*ready = true;
	}
	else
	{
		interfaces->spi.io(ifs->spi_port, NULL, &resp, 1);
		if (ERROR_OK != interfaces->peripheral_commit())
		{
			return ERROR_FAIL;
		}
		*ready = (resp & SD_DATATOKEN_RESP_MASK) == SD_DATATOKEN_RESP_ACCEPTED;
	}
	return ERROR_OK;
}

static RESULT sd_spi_transact_datablock_waitready(
		struct sd_spi_drv_interface_t *ifs, bool *ready, uint16_t token)
{
	uint32_t retry;
	
	*ready = false;
	retry = 312500;
	do {
		if ((ERROR_OK != sd_spi_transact_datablock_isready(ifs, ready, 
															token)) || 
			(!--retry))
		{
			return ERROR_FAIL;
		}
	} while (!*ready);
	return ERROR_OK;
}

static RESULT sd_spi_transact_datablock_fini(
		struct sd_spi_drv_interface_t *ifs, uint16_t token)
{
	bool ready;
	uint8_t resp;
	
	if (((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_OUT) && 
		(	(ERROR_OK != sd_spi_transact_cmd_waitready(ifs, &ready, 
								SD_TRANSTOKEN_RESP_CHECKBUSY, &resp, NULL)) || 
			(!ready)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT sd_spi_transact_end(struct sd_spi_drv_interface_t *ifs)
{
	sd_spi_drv_cs_deassert(ifs);
	return sd_spi_drv_send_empty_bytes(ifs, 1);
}

static RESULT sd_spi_transact_do(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t cmd, uint32_t arg, uint32_t block_num, 
		uint8_t *data_buff, uint16_t size, uint8_t data_token, 
		uint8_t *resp, uint8_t *resp_buff)
{
	RESULT ret = ERROR_OK;
	bool ready;
	
	if ((ERROR_OK != sd_spi_transact_cmd(ifs, token, cmd, arg, block_num)) || 
		(ERROR_OK != sd_spi_transact_cmd_waitready(ifs, &ready, token, resp, 
													resp_buff)) || 
		(!ready) || 
		(((data_buff != NULL) && size) && 
			((ERROR_OK != sd_spi_transact_datablock_init(ifs, token, data_buff, 
													data_token)) || 
			(ERROR_OK != sd_spi_transact_datablock(ifs, token, data_buff, 
													size)) || 
			(ERROR_OK != sd_spi_transact_datablock_waitready(ifs, &ready, 
													token)) || 
			(!ready) || 
			(ERROR_OK != sd_spi_transact_datablock_fini(ifs, token)))))
	{
		ret = ERROR_FAIL;
	}
	return ret;
}

static RESULT sd_spi_transact(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t cmd, uint32_t arg, uint32_t block_num, 
		uint8_t *data_buff, uint16_t size, uint8_t data_token, 
		uint8_t *resp, uint8_t *resp_buff)
{
	RESULT ret = ERROR_OK;
	
	if ((ERROR_OK != sd_spi_transact_start(ifs)) || 
		(ERROR_OK != sd_spi_transact_do(ifs, token, cmd, arg, block_num, 
							data_buff, size, data_token, resp, resp_buff)) || 
		(ERROR_OK != sd_spi_transact_end(ifs)))
	{
		ret = ERROR_FAIL;
	}
	interfaces->peripheral_commit();
	return ret;
}

static RESULT sd_spi_drv_init(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint8_t resp_r1 = 0xFF, resp_r7[4];
	uint16_t retry;
	uint32_t ocr;
	
	if (ERROR_OK != sd_spi_transact_init(ifs))
	{
		return ERROR_FAIL;
	}
	
	// SD Init
	sd_info->cardtype = SD_CARDTYPE_NONE;
	
	retry = 0;
	while (retry++ < SD_SPI_CMD_TIMEOUT)
	{
		if (ERROR_OK != sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
					SD_CMD_GO_IDLE_STATE, 0, 0, NULL, 0, 0, &resp_r1, NULL))
		{
			return ERROR_FAIL;
		}
		
		if (SD_CS8_IN_IDLE_STATE == resp_r1)
		{
			break;
		}
		interfaces->delay.delayms(1);
	}
	if (resp_r1 != SD_CS8_IN_IDLE_STATE)
	{
		sd_info->cardtype = SD_CARDTYPE_NONE;
		return ERROR_FAIL;
	}
	
	// detect card type
	// send CMD8 to get card op
	if (ERROR_OK != sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R7, 
				SD_CMD_SEND_IF_COND, SD_CMD8_VHS_27_36_V | SD_CMD8_CHK_PATTERN, 
				0, NULL, 0, 0, &resp_r1, resp_r7))
	{
		return ERROR_FAIL;
	}
	if ((resp_r1 == SD_CS8_IN_IDLE_STATE) && 
		(resp_r7[3] == SD_CMD8_CHK_PATTERN))
	{
		sd_info->cardtype = SD_CARDTYPE_SD_V2;
	}
	else
	{
		sd_info->cardtype = SD_CARDTYPE_SD_V1;
	}
	
	// send acmd41 to get card status
	retry = 0;
	resp_r1 = 0xFF;
	while (retry++ < 1024)
	{
		if ((ERROR_OK != sd_spi_transact_start(ifs)) || 
			(ERROR_OK != sd_spi_transact_do(ifs, SD_TRANSTOKEN_RESP_R1, 
				SD_CMD_APP_CMD, 0, 0, NULL, 0, 0, &resp_r1, NULL)) || 
			(ERROR_OK != sd_spi_drv_send_empty_bytes(ifs, 1)) || 
			(ERROR_OK != sd_spi_transact_do(ifs, SD_TRANSTOKEN_RESP_R1, 
				SD_ACMD_SD_SEND_OP_COND, SD_ACMD41_HCS, 0, NULL, 0, 0, 
				&resp_r1, NULL)))
		{
			sd_spi_transact_end(ifs);
			interfaces->peripheral_commit();
			return ERROR_FAIL;
		}
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		if (SD_CS8_NONE == resp_r1)
		{
			break;
		}
		interfaces->delay.delayms(1);
	}
	
	// send cmd1 for MMC card
	if (resp_r1 != SD_CS8_NONE)
	{
		retry = 0;
		while (retry++ < SD_SPI_CMD_TIMEOUT)
		{
			if (ERROR_OK != sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
					SD_CMD_SEND_OP_COND, 0, 0, NULL, 0, 0, &resp_r1, resp_r7))
			{
				return ERROR_FAIL;
			}
			if (resp_r1 == SD_CS8_NONE)
			{
			    break;
			}
		}
		if (resp_r1 != SD_CS8_NONE)
		{
			sd_info->cardtype = SD_CARDTYPE_NONE;
			return ERROR_FAIL;
		}
		sd_info->cardtype = SD_CARDTYPE_MMC;
	}
	
	// send cmd58 to get card ocr
	retry = 0;
	while (retry++ < SD_SPI_CMD_TIMEOUT)
	{
		if (ERROR_OK != sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R7, 
				SD_CMD_READ_OCR, 0x40000000, 0, NULL, 0, 0, &resp_r1, resp_r7))
		{
			return ERROR_FAIL;
		}
		ocr = GET_BE_U32(resp_r7);
		if (SD_CS8_NONE == resp_r1)
		{
			if ((ocr & SD_OCR_BUSY) && (ocr & SD_OCR_CCS))
			{
				if (SD_CARDTYPE_SD_V2 == sd_info->cardtype)
				{
					sd_info->cardtype = SD_CARDTYPE_SD_V2HC;
				}
				else
				{
					sd_info->cardtype = SD_CARDTYPE_MMC_HC;
				}
			}
			break;
		}
	}
	
	if ((NULL == sd_spi_drv.getinfo) || 
		(NULL == sd_info) || 
		(ERROR_OK != sd_spi_drv.getinfo(info)) || 
		(ERROR_OK != interfaces->spi.config(ifs->spi_port, 
			sd_info->frequency_kHz, SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER)) || 
		(ERROR_OK != interfaces->peripheral_commit()) || 
		(ERROR_OK != sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
				SD_CMD_SET_BLOCKLEN, 512, 0, NULL, 0, 0, &resp_r1, NULL)))
	{
		return ERROR_FAIL;
	}
	mal_info->capacity = sd_info->capacity;
	return ERROR_OK;
}

static RESULT sd_spi_drv_fini(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	
	return sd_spi_transact_fini(ifs);
}

static RESULT sd_spi_getinfo(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint8_t csd[16];
	uint8_t resp_r1, resp_r7[4];
	
	token = SD_TRANSTOKEN_RESP_R1;
	if ((NULL == sd_info) || 
		(ERROR_OK != sd_spi_transact(ifs, token, SD_CMD_SEND_CSD, 0, 0, 
				csd, 16, SD_DATATOKEN_START_BLK, &resp_r1, resp_r7)) || 
		(ERROR_OK != sd_parse_csd(csd, sd_info)) || 
		(ERROR_OK != sd_spi_transact(ifs, token, SD_CMD_SEND_CID, 0, 0, 
				sd_info->cid, 16, SD_DATATOKEN_START_BLK, &resp_r1, resp_r7)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	uint8_t resp;
	bool ready;
	
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if ((ERROR_OK != sd_spi_transact_start(ifs)) || 
		(ERROR_OK != sd_spi_transact_cmd(ifs, token, 
						SD_CMD_READ_MULTIPLE_BLOCK, arg, (uint32_t)count)) || 
		(ERROR_OK != sd_spi_transact_cmd_waitready(ifs, &ready, token, &resp, 
													NULL)) || 
		(!ready) || (resp != SD_CS8_NONE))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if ((ERROR_OK != sd_spi_transact_datablock_init(ifs, token, buff, 
												SD_DATATOKEN_START_BLK)) || 
		(ERROR_OK != sd_spi_transact_datablock(ifs, token, buff, 512)))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_readblock_nb_isready(struct dal_info_t *info, 
								uint64_t address, uint8_t *buff, bool *ready)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (ERROR_OK != sd_spi_transact_datablock_isready(ifs, ready, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	if (*ready)
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_readblock_nb_waitready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	bool ready;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (ERROR_OK != sd_spi_transact_datablock_waitready(ifs, &ready, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	if (ready)
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_readblock_nb_end(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	uint8_t resp;
	bool ready;
	
	token = SD_TRANSTOKEN_RESP_R1B;
	if ((ERROR_OK != sd_spi_transact_cmd(ifs, token, SD_CMD_STOP_TRANSMISSION, 
											0, 0)) || 
		(ERROR_OK != sd_spi_transact_cmd_waitready(ifs, &ready, token, &resp, 
													NULL)) || 
		(!ready))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	uint8_t resp;
	bool ready;
	
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if ((ERROR_OK != sd_spi_transact_start(ifs)) || 
		(ERROR_OK != sd_spi_transact_cmd(ifs, token, 
						SD_CMD_WRITE_MULTIPLE_BLOCK, arg, (uint32_t)count)) || 
		(ERROR_OK != sd_spi_transact_cmd_waitready(ifs, &ready, token, &resp, 
													NULL)) || 
		(!ready) || (resp != SD_CS8_NONE))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if ((ERROR_OK != sd_spi_transact_datablock_init(ifs, token, buff, 
											SD_DATATOKEN_START_BLK_MULT)) || 
		(ERROR_OK != sd_spi_transact_datablock(ifs, token, buff, 512)))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_writeblock_nb_isready(struct dal_info_t *info, 
								uint64_t address, uint8_t *buff, bool *ready)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (ERROR_OK != sd_spi_transact_datablock_isready(ifs, ready, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	if (*ready)
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_writeblock_nb_waitready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	bool ready;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (ERROR_OK != sd_spi_transact_datablock_waitready(ifs, &ready, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	if (ready)
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static RESULT sd_spi_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint8_t datatoken, resp;
	bool ready;
	
	datatoken = SD_DATATOKEN_STOP_TRAN;
	if ((ERROR_OK != interfaces->spi.io(ifs->spi_port, &datatoken, NULL, 1)) || 
		(	(ERROR_OK != sd_spi_transact_cmd_waitready(ifs, &ready, 
								SD_TRANSTOKEN_RESP_CHECKBUSY, &resp, NULL)) || 
			(!ready)))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return ERROR_FAIL;
	}
	
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return ERROR_OK;
}

#if DAL_INTERFACE_PARSER_EN
static RESULT sd_spi_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;	
	
	ifs->spi_port = buff[0];
	ifs->cs_port = buff[1];
	ifs->cs_pin = *(uint32_t *)&buff[2];
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
	},
	
	MAL_IDX_SD_SPI,
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	
	sd_spi_drv_init,
	sd_spi_drv_fini,
	sd_spi_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
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
	sd_spi_drv_readblock_nb_waitready,
	sd_spi_drv_readblock_nb_end,
	
	sd_spi_drv_writeblock_nb_start,
	sd_spi_drv_writeblock_nb,
	sd_spi_drv_writeblock_nb_isready,
	sd_spi_drv_writeblock_nb_waitready,
	sd_spi_drv_writeblock_nb_end
};


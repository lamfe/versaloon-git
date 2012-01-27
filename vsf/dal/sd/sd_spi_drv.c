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

#if DAL_SD_SPI_EN

#include "sd_common.h"
#include "sd_spi_drv_cfg.h"
#include "sd_spi_drv.h"

// RESP
#define  SD_RESP_EMPTY						0xFF

#define  SD_DATATOKEN_RESP_ACCEPTED			0x05
#define  SD_DATATOKEN_RESP_CRC_REJECTED		0x0B
#define  SD_DATATOKEN_RESP_WR_REJECTED		0x0D
#define  SD_DATATOKEN_RESP_MASK				0x1F

#define  SD_DATATOKEN_START_BLK_MULT		0xFC
#define  SD_DATATOKEN_STOP_TRAN				0xFD
#define  SD_DATATOKEN_START_BLK				0xFE

#define  SD_DATATOKEN_ERR_ERROR				0x01
#define  SD_DATATOKEN_ERR_CC_ERROR			0x02
#define  SD_DATATOKEN_ERR_CARD_ECC_ERROR	0x04
#define  SD_DATATOKEN_ERR_OUT_OF_RANGE		0x08

// Card Status
#define SD_CS8_NONE							0x00
#define SD_CS8_IN_IDLE_STATE				0x01
#define SD_CS8_ERASE_RESET					0x02
#define SD_CS8_ILLEGAL_COMMAND				0x04
#define SD_CS8_COM_CRC_ERROR				0x08
#define SD_CS8_ERASE_SEQUENCE_ERROR			0x10
#define SD_CS8_ADDRESS_ERROR				0x20
#define SD_CS8_PARAMETER_ERROR				0x40
#define SD_CS8_ERROR_MASK					\
	(SD_CS8_ERASE_RESET | SD_CS8_ILLEGAL_COMMAND | SD_CS8_COM_CRC_ERROR | \
	SD_CS8_ERASE_SEQUENCE_ERROR | SD_CS8_ADDRESS_ERROR | SD_CS8_PARAMETER_ERROR)

#define SD_TRANSTOKEN_DATA_DIR				0x8000
#define SD_TRANSTOKEN_DATA_OUT				0x8000
#define SD_TRANSTOKEN_DATA_IN				0x0000

#define SD_TRANSTOKEN_RESP_MASK				0x00FF
#define SD_TRANSTOKEN_RESP_CHECKBUSY		0x0080
#define SD_TRANSTOKEN_RESP_CMD				0x0040
#define SD_TRANSTOKEN_RESP_CRC7				0x0020
#define SD_TRANSTOKEN_RESP_LONG				0x0010
#define SD_TRANSTOKEN_RESP_SHORT			0x0000

#define SD_TRANSTOKEN_RESP_NONE				0x0000
#define SD_TRANSTOKEN_RESP_R1				0x0061
#define SD_TRANSTOKEN_RESP_R1B				0x00E1
#define SD_TRANSTOKEN_RESP_R2				0x0012
#define SD_TRANSTOKEN_RESP_R3				0x0003
#define SD_TRANSTOKEN_RESP_R4				0x0004
#define SD_TRANSTOKEN_RESP_R5				0x0005
#define SD_TRANSTOKEN_RESP_R6				0x0066
#define SD_TRANSTOKEN_RESP_R7				0x0067

static vsf_err_t sd_spi_drv_cs_assert(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		return interfaces->gpio.clear(ifs->cs_port, ifs->cs_pin);
	}
	else
	{
//		return interfaces->spi.select(ifs->spi_port, ifs->cs_pin);
	}
}

static vsf_err_t sd_spi_drv_cs_deassert(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		return interfaces->gpio.set(ifs->cs_port, ifs->cs_pin);
	}
	else
	{
//		return interfaces->spi.deselect(ifs->spi_port, ifs->cs_pin);
	}
}

static vsf_err_t sd_spi_wait_datatoken(struct sd_spi_drv_interface_t *ifs, 
									uint8_t *datatoken)
{
	uint32_t retry = 0;
	uint8_t tmp_0xFF = 0xFF;
	
	while (retry < 312500)
	{
		interfaces->spi.io(ifs->spi_port, &tmp_0xFF, datatoken, 1);
		if (interfaces->peripheral_commit())
		{
			return VSFERR_FAIL;
		}
		if (*datatoken != 0xFF)
		{
			return VSFERR_NONE;
		}
	}
	return (retry == 312500) ? VSFERR_FAIL : VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_send_empty_bytes(struct sd_spi_drv_interface_t *ifs, 
											uint8_t cnt)
{
	uint8_t byte_0xFF = 0xFF;
	
	while (cnt--)
	{
		interfaces->spi.io(ifs->spi_port, &byte_0xFF, NULL, 1);
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_init(struct sd_spi_drv_interface_t *ifs)
{
	interfaces->spi.init(ifs->spi_port);
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.init(ifs->cs_port);
		interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, ifs->cs_pin,
								ifs->cs_pin, ifs->cs_pin);
	}
	else
	{
//		interfaces->spi.cs_init(ifs->spi_port, ifs->cs_pin);
	}
	// use slowest spi speed when initializing
	interfaces->spi.config(ifs->spi_port, 400, 
							SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER);
	return sd_spi_drv_send_empty_bytes(ifs, 20);
}

static vsf_err_t sd_spi_transact_fini(struct sd_spi_drv_interface_t *ifs)
{
	if (ifs->cs_port != IFS_DUMMY_PORT)
	{
		interfaces->gpio.config(ifs->cs_port, ifs->cs_pin, 0, 0, 0);
		interfaces->gpio.fini(ifs->cs_port);
	}
	else
	{
//		interfaces->spi.cs_fini(ifs->spi_port, ifs->cs_pin);
	}
	return interfaces->spi.fini(ifs->spi_port);
}

static vsf_err_t sd_spi_transact_start(struct sd_spi_drv_interface_t *ifs)
{
	sd_spi_drv_cs_assert(ifs);
	return sd_spi_drv_send_empty_bytes(ifs, 1);
}

static vsf_err_t sd_spi_transact_cmd(struct sd_spi_drv_interface_t *ifs, 
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

static vsf_err_t sd_spi_transact_cmd_isready(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t *resp, uint8_t *resp_buff)
{
	uint8_t tmp_0xFF = 0xFF;
	
	interfaces->spi.io(ifs->spi_port, &tmp_0xFF, resp, 1);
	if (interfaces->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	if (token & SD_TRANSTOKEN_RESP_CHECKBUSY)
	{
		return (*resp != 0xFF) ? VSFERR_NONE : VSFERR_NOT_READY;
	}
	if (!(*resp & 0x80))
	{
		uint8_t len;
		
		if (*resp & SD_CS8_ERROR_MASK)
		{
			return VSFERR_FAIL;
		}
		
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
		return VSFERR_NONE;
	}
	return VSFERR_NOT_READY;
}

static vsf_err_t sd_spi_transact_cmd_waitready(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t *resp, uint8_t *resp_buff)
{
	vsf_err_t err;
	uint32_t retry;
	
	retry = 32;
	do {
		err = sd_spi_transact_cmd_isready(ifs, token, resp, resp_buff);
		if ((err && (err != VSFERR_NOT_READY)) || (!--retry))
		{
			return VSFERR_FAIL;
		}
	} while (err);
	
	if (token & SD_TRANSTOKEN_RESP_CHECKBUSY)
	{
		uint8_t tmp_0xFF = 0xFF, tmp;
		
		retry = 0;
		while (retry < 312500)
		{
			interfaces->spi.io(ifs->spi_port, &tmp_0xFF, &tmp, 1);
			if (interfaces->peripheral_commit())
			{
				return VSFERR_FAIL;
			}
			if (0xFF == tmp)
			{
				return VSFERR_NONE;
			}
		}
		if (retry == 312500)
		{
			return VSFERR_FAIL;
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_datablock_init(
		struct sd_spi_drv_interface_t *ifs, uint16_t token, uint8_t *buffer, 
		uint8_t data_token)
{
	REFERENCE_PARAMETER(buffer);
	
	if ((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_IN)
	{
		uint8_t tmp;
		
		if (sd_spi_wait_datatoken(ifs, &tmp) || 
			(tmp != data_token))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		interfaces->spi.io(ifs->spi_port, &data_token, NULL, 1);
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_datablock(struct sd_spi_drv_interface_t *ifs, 
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

static vsf_err_t sd_spi_transact_datablock_isready(
		struct sd_spi_drv_interface_t *ifs, uint16_t token)
{
	uint8_t resp;
	
	if ((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_IN)
	{
		return VSFERR_NONE;
	}
	else
	{
		interfaces->spi.io(ifs->spi_port, NULL, &resp, 1);
		if (interfaces->peripheral_commit())
		{
			return VSFERR_FAIL;
		}
		return ((resp & SD_DATATOKEN_RESP_MASK) == SD_DATATOKEN_RESP_ACCEPTED) ?
					VSFERR_NONE : VSFERR_NOT_READY;
	}
}

static vsf_err_t sd_spi_transact_datablock_waitready(
		struct sd_spi_drv_interface_t *ifs, uint16_t token)
{
	vsf_err_t err;
	uint32_t retry;
	
	retry = 312500;
	do {
		err = sd_spi_transact_datablock_isready(ifs, token);
		if ((err && (err != VSFERR_NOT_READY)) || (!--retry))
		{
			return VSFERR_FAIL;
		}
	} while (err);
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_datablock_fini(
		struct sd_spi_drv_interface_t *ifs, uint16_t token)
{
	uint8_t resp;
	
	if (((token & SD_TRANSTOKEN_DATA_DIR) == SD_TRANSTOKEN_DATA_OUT) && 
		sd_spi_transact_cmd_waitready(ifs, SD_TRANSTOKEN_RESP_CHECKBUSY,
											&resp, NULL))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact_end(struct sd_spi_drv_interface_t *ifs)
{
	sd_spi_drv_cs_deassert(ifs);
	return sd_spi_drv_send_empty_bytes(ifs, 1);
}

static vsf_err_t sd_spi_transact_do(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t cmd, uint32_t arg, uint32_t block_num, 
		uint8_t *data_buff, uint16_t size, uint8_t data_token, 
		uint8_t *resp, uint8_t *resp_buff)
{
	if (sd_spi_transact_cmd(ifs, token, cmd, arg, block_num) || 
		sd_spi_transact_cmd_waitready(ifs, token, resp, resp_buff) || 
		(((data_buff != NULL) && size) &&
			(sd_spi_transact_datablock_init(ifs, token, data_buff,
											data_token) || 
			sd_spi_transact_datablock(ifs, token, data_buff, size) || 
			sd_spi_transact_datablock_waitready(ifs, token) || 
			sd_spi_transact_datablock_fini(ifs, token))))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_transact(struct sd_spi_drv_interface_t *ifs, 
		uint16_t token, uint8_t cmd, uint32_t arg, uint32_t block_num, 
		uint8_t *data_buff, uint16_t size, uint8_t data_token, 
		uint8_t *resp, uint8_t *resp_buff)
{
	vsf_err_t err = VSFERR_NONE;
	
	if (sd_spi_transact_start(ifs) || 
		sd_spi_transact_do(ifs, token, cmd, arg, block_num, 
							data_buff, size, data_token, resp, resp_buff) || 
		sd_spi_transact_end(ifs))
	{
		err = VSFERR_FAIL;
	}
	interfaces->peripheral_commit();
	return err;
}

static vsf_err_t sd_spi_drv_init(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint8_t resp_r1 = 0xFF, resp_r7[4];
	
	if (sd_spi_transact_init(ifs))
	{
		return VSFERR_FAIL;
	}
	
	// SD Init
	sd_info->cardtype = SD_CARDTYPE_NONE;
	sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
				SD_CMD_GO_IDLE_STATE, 0, 0, NULL, 0, 0, &resp_r1, NULL);
	
	// detect card type
	// send CMD8 to get card op
	if (sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R7, 
				SD_CMD_SEND_IF_COND, SD_CMD8_VHS_27_36_V | SD_CMD8_CHK_PATTERN, 
				0, NULL, 0, 0, &resp_r1, resp_r7))
	{
		return VSFERR_FAIL;
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
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_init_isready(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint8_t resp_r1, resp_r7[4];
	uint32_t ocr;
	
	// send acmd41 to get card status
	if (sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
			SD_CMD_APP_CMD, 0, 0, NULL, 0, 0, &resp_r1, NULL) || 
		sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
			SD_ACMD_SD_SEND_OP_COND, SD_ACMD41_HCS, 0, NULL, 0, 0, &resp_r1, NULL))
	{
		return VSFERR_FAIL;
	}
	if (resp_r1 != SD_CS8_NONE)
	{
		return VSFERR_NOT_READY;
	}
	
	// send cmd58 to get card ocr
	if (sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R7, 
			SD_CMD_READ_OCR, 0x40000000, 0, NULL, 0, 0, &resp_r1, resp_r7))
	{
		return VSFERR_FAIL;
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
	}
	
	if ((NULL == sd_spi_drv.getinfo) || 
		(NULL == sd_info) || 
		sd_spi_drv.getinfo(info) || 
		interfaces->spi.config(ifs->spi_port, 
			sd_info->frequency_kHz, SPI_MODE3 | SPI_MSB_FIRST | SPI_MASTER) || 
		interfaces->peripheral_commit() || 
		sd_spi_transact(ifs, SD_TRANSTOKEN_RESP_R1, 
				SD_CMD_SET_BLOCKLEN, 512, 0, NULL, 0, 0, &resp_r1, NULL))
	{
		return VSFERR_FAIL;
	}
	mal_info->capacity = sd_info->capacity;
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_fini(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	
	return sd_spi_transact_fini(ifs);
}

static vsf_err_t sd_spi_getinfo(struct dal_info_t *info)
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
		sd_spi_transact(ifs, token, SD_CMD_SEND_CSD, 0, 0, 
				csd, 16, SD_DATATOKEN_START_BLK, &resp_r1, resp_r7) || 
		sd_parse_csd(csd, sd_info) || 
		sd_spi_transact(ifs, token, SD_CMD_SEND_CID, 0, 0, 
				sd_info->cid, 16, SD_DATATOKEN_START_BLK, &resp_r1, resp_r7))
	{
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_readblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	uint8_t resp;
	
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (sd_spi_transact_start(ifs) || 
		sd_spi_transact_cmd(ifs, token, 
						SD_CMD_READ_MULTIPLE_BLOCK, arg, (uint32_t)count) || 
		sd_spi_transact_cmd_waitready(ifs, token, &resp, NULL) || 
		(resp != SD_CS8_NONE))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (sd_spi_transact_datablock_init(ifs, token, buff,
										SD_DATATOKEN_START_BLK) || 
		sd_spi_transact_datablock(ifs, token, buff, 512))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_readblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	vsf_err_t err;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	err = sd_spi_transact_datablock_isready(ifs, token);
	if (err && (err != VSFERR_NOT_READY))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return err;
	}
	if (!err)
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_readblock_nb_waitready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_IN;
	if (sd_spi_transact_datablock_waitready(ifs, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	else
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_readblock_nb_end(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	uint8_t resp;
	
	token = SD_TRANSTOKEN_RESP_R1B;
	if (sd_spi_transact_cmd(ifs, token, SD_CMD_STOP_TRANSMISSION, 0, 0) || 
		sd_spi_transact_cmd_waitready(ifs, token, &resp, NULL))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return VSFERR_NONE;
}

static vsf_err_t sd_spi_drv_writeblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	struct mal_info_t *mal_info = (struct mal_info_t *)info->extra;
	struct sd_info_t *sd_info = (struct sd_info_t *)mal_info->extra;
	uint16_t token;
	uint32_t arg;
	uint8_t resp;
	
	if (SD_CARDTYPE_SD_V2HC == sd_info->cardtype)
	{
		arg = (uint32_t)(address >> 9);
	}
	else
	{
		arg = (uint32_t)address;
	}
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (sd_spi_transact_start(ifs) || 
		sd_spi_transact_cmd(ifs, token, SD_CMD_WRITE_MULTIPLE_BLOCK, arg,
							(uint32_t)count) || 
		sd_spi_transact_cmd_waitready(ifs, token, &resp, NULL) || 
		(resp != SD_CS8_NONE))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (sd_spi_transact_datablock_init(ifs, token, buff, 
											SD_DATATOKEN_START_BLK_MULT) || 
		sd_spi_transact_datablock(ifs, token, buff, 512))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_writeblock_nb_isready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	vsf_err_t err;
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	err = sd_spi_transact_datablock_isready(ifs, token);
	if (err && (err != VSFERR_NOT_READY))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return err;
	}
	if (!err)
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_writeblock_nb_waitready(struct dal_info_t *info, 
												uint64_t address, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint16_t token;
	
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	
	token = SD_TRANSTOKEN_RESP_R1 | SD_TRANSTOKEN_DATA_OUT;
	if (sd_spi_transact_datablock_waitready(ifs, token))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	else
	{
		sd_spi_transact_datablock_fini(ifs, token);
	}
	
	return interfaces->peripheral_commit();
}

static vsf_err_t sd_spi_drv_writeblock_nb_end(struct dal_info_t *info)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;
	uint8_t datatoken, resp;
	
	datatoken = SD_DATATOKEN_STOP_TRAN;
	if (interfaces->spi.io(ifs->spi_port, &datatoken, NULL, 1) || 
		sd_spi_transact_cmd_waitready(ifs, SD_TRANSTOKEN_RESP_CHECKBUSY,
											&resp, NULL))
	{
		sd_spi_transact_end(ifs);
		interfaces->peripheral_commit();
		return VSFERR_FAIL;
	}
	
	sd_spi_transact_end(ifs);
	interfaces->peripheral_commit();
	return VSFERR_NONE;
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t sd_spi_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct sd_spi_drv_interface_t *ifs = 
								(struct sd_spi_drv_interface_t *)info->ifs;	
	
	ifs->spi_port = buff[0];
	ifs->cs_port = buff[1];
	ifs->cs_pin = *(uint32_t *)&buff[2];
	return VSFERR_NONE;
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
	sd_spi_drv_init_isready,
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

#endif

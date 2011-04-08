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

#include "../mal/mal_internal.h"
#include "sd_common.h"

uint8_t sd_spi_calc_chksum(uint8_t *data, uint32_t num)
{
	uint8_t crc, mask, i;
	uint32_t j;
	
	crc = 0;
	for (j = 0; j < num; j++)
	{
		mask = data[j] & 0x80 ? 0x40 : 0x00;
		crc <<= 1;
		if ((crc & 0x40) ^ mask)
		{
			crc ^= SD_CRC7_POLY;
		}
		crc ^= data[j];
		
		for (i = 0; i < 7; i++)
		{
			crc <<= 1;
			if (crc & 0x40)
			{
				crc ^= SD_CRC7_POLY;
			}
		}
	}
	return crc & 0x7F;
}

RESULT sd_parse_csd(uint8_t *csd, enum sd_cardtype_t cardtype, 
					struct sd_info_t *info)
{
	struct mal_capacity_t tmp_cap;
	uint32_t i, tmp_u32;
	uint8_t r1;
	
	tmp_cap.block_size = 512;
	switch (cardtype)
	{
	case SD_CARDTYPE_SD_V2HC:
		tmp_cap.block_number = ((((csd[7] & 0xFC) << 16) | 
								((csd[8] & 0xFF) <<  8) | 
								((csd[9] & 0xFF) <<  0)) + 1) * 2;
		break;
	case SD_CARDTYPE_SD_V1:
	case SD_CARDTYPE_SD_V2:
	case SD_CARDTYPE_MMC:
	case SD_CARDTYPE_MMC_HC:
		i = csd[6] & 0x03 ;
		i <<= 8;
		i += csd[7];
		i <<= 2;
		i += ((csd[8] & 0xc0) >> 6);
		
		r1 = csd[9] & 0x03;
		r1 <<= 1;
		r1 += ((csd[10] & 0x80) >> 7);
		
		r1 += 2;
        tmp_u32 = 1;
        while(r1--)
        {
            tmp_u32 *= 2;
        }
        tmp_cap.block_number = (i + 1) * tmp_u32;
		
		i = csd[5] & 0x0F;
        tmp_u32 = 1;
        while(i--)
        {
            tmp_u32 *= 2 ;
        }
        tmp_cap.block_number *= tmp_u32;
		break;
	default:
		return ERROR_FAIL;
	}
	if (info != NULL)
	{
		info->capacity = tmp_cap;
		info->frequency_mHz = 50 * 1000;
	}
	return ERROR_OK;
}


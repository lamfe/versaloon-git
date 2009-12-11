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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "file_parser.h"

#include "hex.h"

typedef enum 
{
	HEX_TYPE_DATA		= 0x00,
	HEX_TYPE_EOF		= 0x01,
	HEX_TYPE_SEG_ADDR	= 0x02,
	HEX_TYPE_EXT_ADDR	= 0x04
} HEX_TEYP;

RESULT get_hex_from_str(uint8_t *str, uint8_t len, uint32_t *value)
{
	char num[5], *ptr;
	
#ifdef PARAM_CHECK
	if ((NULL == str) || (NULL == value) || (len > 4))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	memcpy(num, str, len);
	num[len] = 0;
	
	*value = strtoul(num, &ptr, 16);
	if ((num + len) == ptr)
	{
		return ERROR_OK;
	}
	else
	{
		return ERROR_FAIL;
	}
}

RESULT read_hex_file(FILE *hex_file, WRITE_MEMORY_CALLBACK callback, 
					 void *buffer, uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t line_buf[10 + 0xFF * 2 + 2], pos, type, checksum;
	char ch;
	uint32_t data_addr = 0, seg_addr = 0, length, tmp32, i;
	RESULT ret;
	
#ifdef PARAM_CHECK
	if ((NULL == hex_file) || (NULL == callback))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	rewind(hex_file);
	while (!feof(hex_file))
	{
		// ignore empty lines
		do{
			ch = (char)fgetc(hex_file);
			if (EOF == ch)
			{
				return ERROR_OK;
			}
		}while (('\n' == ch) || ('\r' == ch));
		// check first character of a line, whichi MUST be ':'
		if (ch != ':')
		{
			return ERROR_FAIL;
		}
		length = 0;
		// read line
		do{
			ch = (char)fgetc(hex_file);
			line_buf[length++] = ch;
		}while ((ch != '\n') && (ch != '\r') && (ch != EOF));
		length -= 1;
		
		// process line
		if (length < 9)
		{
			return ERROR_FAIL;
		}
		checksum = 0;
		pos = 0;
		
		// get data length
		ret = get_hex_from_str(line_buf + pos, 2, &tmp32);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
		pos += 2;
		checksum += (uint8_t)tmp32;
		// verify data length
		if (length != (10 + tmp32 * 2))
		{
			return ERROR_FAIL;
		}
		length = tmp32;
		
		// get address
		ret = get_hex_from_str(line_buf + pos, 4, &tmp32);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
		pos += 4;
		checksum += (uint8_t)tmp32;
		checksum += (uint8_t)(tmp32 >> 8);
		data_addr = (data_addr & 0xFFFF0000) | (tmp32 & 0x0000FFFF);
		
		// get type
		ret = get_hex_from_str(line_buf + pos, 2, &tmp32);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
		pos += 2;
		checksum += (uint8_t)tmp32;
		type = (uint8_t)tmp32;
		
		// get data
		for (i = 0; i < length; i++)
		{
			ret = get_hex_from_str(line_buf + pos, 2, &tmp32);
			if (ret != ERROR_OK)
			{
				return ERROR_FAIL;
			}
			
			pos += 2;
			checksum += (uint8_t)tmp32;
			line_buf[i] = (uint8_t)tmp32;
		}
		
		// get checksum
		ret = get_hex_from_str(line_buf + pos, 2, &tmp32);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
		// verify checksum
		if (((checksum + tmp32) & 0xFF) != 0)
		{
			return ERROR_FAIL;
		}
		
		// process data according data type
		switch (type)
		{
		case HEX_TYPE_DATA:
			// data record
			ret = callback(data_addr + addr_offset, seg_addr + seg_offset, 
							line_buf, length, buffer);
			if (ret != ERROR_OK)
			{
				return ERROR_FAIL;
			}
			break;
		case HEX_TYPE_EOF:
			// end of file
			return ERROR_OK;
			break;
		case HEX_TYPE_SEG_ADDR:
			// segment address
			if (length > 4)
			{
				return ERROR_FAIL;
			}
			memcpy(&seg_addr, line_buf, length);
			break;
		case HEX_TYPE_EXT_ADDR:
			// extended address
			if (length > 2)
			{
				return ERROR_FAIL;
			}
			if (1 == length)
			{
				data_addr = (data_addr & 0x0000FFFF) | (line_buf[0] << 16);
			}
			else //if (2 == length)
			{
				data_addr = (data_addr & 0x0000FFFF) | (line_buf[0] << 24) 
							| (line_buf[1] << 16);
			}
			break;
		default:
			LOG_WARNING(_GETTEXT(ERRMSG_INVALID_VALUE_MESSAGE), type, 
						"hex type", "current line ignored!!");
			break;
		}
	}
	
	return ERROR_FAIL;
}

static RESULT write_hex_line(FILE *hex_file, uint8_t data_len, 
								uint16_t data_addr, uint8_t type, 
								uint8_t *data)
{
	uint8_t line_buf[10 + 0xFF * 2 + 2], checksum = 0, pos = 0;
	uint32_t i;
	
#ifdef PARAM_CHECK
	if ((NULL == hex_file) || ((data_len > 0) && (NULL == data)))
	{
		LOG_BUG("parameter invalid\n\n");
		return ERROR_FAIL;
	}
#endif
	
	line_buf[pos] = ':';
	pos += 1;
	
	// data length
	checksum += data_len;
	sprintf((char *)line_buf + pos, "%02x", data_len);
	pos += 2;
	
	// address
	checksum += (uint8_t)data_addr;
	checksum += (uint8_t)(data_addr >> 8);
	sprintf((char *)line_buf + pos, "%04x", data_addr);
	pos += 4;
	
	// type
	checksum += type;
	sprintf((char *)line_buf + pos, "%02x", type);
	pos += 2;
	
	// data
	for (i = 0; i < data_len; i++)
	{
		checksum += data[i];
		sprintf((char *)line_buf + pos, "%02x", data[i]);
		pos += 2;
	}
	
	// checksum
	sprintf((char *)line_buf + pos, "%02x", checksum);
	pos += 2;
	
	// \n\r
	sprintf((char *)line_buf + pos, "\r\n");
	pos += 2;
	
	return (pos == fwrite(line_buf, 1, pos, hex_file));
}

RESULT write_hex_file(FILE *hex_file, uint32_t file_addr, 
						uint8_t *buff, uint32_t buff_size, 
						uint32_t seg_addr, uint32_t start_addr)
{
	uint32_t tmp;
	RESULT ret;
	
	file_addr = file_addr;
	
	// write seg_addr
	ret = write_hex_line(hex_file, 4, (uint16_t)0, HEX_TYPE_SEG_ADDR, 
							(uint8_t *)&seg_addr);
	if (ret != ERROR_OK)
	{
		return ret;
	}
	// write ext_addr
	tmp = start_addr >> 16;
	ret = write_hex_line(hex_file, 2, (uint16_t)0, HEX_TYPE_EXT_ADDR, 
							(uint8_t *)&tmp);
	if (ret != ERROR_OK)
	{
		return ret;
	}
	// write data
	while (buff_size)
	{
		if (buff_size > 16)
		{
			tmp = 16;
		}
		else
		{
			tmp = buff_size;
		}
		
		// write
		ret = write_hex_line(hex_file, (uint8_t)tmp, 
								(uint16_t)(start_addr & 0xFFFF), 
								HEX_TYPE_DATA, buff);
		if (ret != ERROR_OK)
		{
			return ret;
		}
		
		start_addr += tmp;
		buff += tmp;
		buff_size -= tmp;
	}
	
	return ERROR_OK;
}

RESULT write_hex_file_end(FILE *hex_file)
{
	if (fwrite(":00000001ff\n\r", 1, 13, hex_file) != 13)
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}


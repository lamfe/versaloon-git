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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "byte_tap.h"
#include "svf.h"
#include "svf_parser.h"
#include "svf_player.h"

const char *svf_trst_mode_name[4] =
{
	"ON",
	"OFF",
	"Z",
	"ABSENT"
};

const char *svf_command_name[14] = 
{
	"ENDDR",
	"ENDIR",
	"FREQUENCY",
	"HDR",
	"HIR",
	"PIO",
	"PIOMAP",
	"RUNTEST",
	"SDR",
	"SIR",
	"STATE",
	"TDR",
	"TIR",
	"TRST"
};

typedef struct
{
	uint32 line_num;
	uint8 enabled;
	uint32 buffer_offset;
	uint32 bit_len;
}svf_parser_check_tdo_para_t;

uint32 svf_file_index = 0;
uint32 svf_line_number = 0;

static uint8 svf_parser_tdo_buffer[SVF_PARSER_DATA_BUFFER_SIZE];
static uint8 svf_parser_mask_buffer[SVF_PARSER_DATA_BUFFER_SIZE];
static uint32 svf_parser_buffer_index = 0;

static uint8 svf_parser_tdi_buffer[SVF_PARSER_DATA_BUFFER_SIZE];
static svf_parser_check_tdo_para_t 
						svf_parser_check[SVF_PARSER_DATA_BUFFER_SIZE];
static uint32 svf_parser_check_index = 0;

static svf_para_t svf_parser_para;

void svf_parser_free_xxd_para(svf_xxr_para_t *para)
{
	if (para->tdi != NULL)
	{
		free(para->tdi);
		para->tdi = NULL;
	}
	if (para->tdo != NULL)
	{
		free(para->tdo);
		para->tdo = NULL;
	}
	if (para->mask != NULL)
	{
		free(para->mask);
		para->mask = NULL;
	}
	if (para->smask != NULL)
	{
		free(para->smask);
		para->smask = NULL;
	}
}

void svf_parser_init(void)
{
	// init svf_para
	svf_line_number = 1;
	svf_file_index = 0;
	memset(&svf_parser_para, 0, sizeof(svf_parser_para));
	
	// reset all state to IDLE
	svf_parser_para.runtest_end_state = IDLE;
	svf_parser_para.runtest_run_state = IDLE;
	svf_parser_para.ir_end_state = IDLE;
	svf_parser_para.dr_end_state = IDLE;
}

void svf_parser_fini(void)
{
	svf_parser_free_xxd_para(&svf_parser_para.hdr_para);
	svf_parser_free_xxd_para(&svf_parser_para.hir_para);
	svf_parser_free_xxd_para(&svf_parser_para.tdr_para);
	svf_parser_free_xxd_para(&svf_parser_para.tir_para);
	svf_parser_free_xxd_para(&svf_parser_para.sdr_para);
	svf_parser_free_xxd_para(&svf_parser_para.sir_para);
	
	jtag_fini();
}

static RESULT svf_parser_adjust_array_length(uint8 **arr, uint32 orig_bit_len, 
											 uint32 new_bit_len)
{
	uint32 new_byte_len = (new_bit_len + 7) >> 3;
	
	if ((NULL == *arr) 
		|| (((orig_bit_len + 7) >> 3) < ((new_bit_len + 7) >> 3)))
	{
		if (*arr != NULL)
		{
			free(*arr);
			*arr = NULL;
		}
		*arr = (uint8*)malloc(new_byte_len);
		if (NULL == *arr)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
		memset(*arr, 0, new_byte_len);
	}
	
	return ERROR_OK;
}

static void svf_parser_append_1s(uint8 *dest, uint32 dest_bit_len, 
								 uint32 append_bit_len)
{
	uint32 i;
	
	for (i = 0; i < append_bit_len; i++)
	{
		if (((dest_bit_len + i) & 7) == 0)
		{
			dest[(dest_bit_len + i) / 8] = 0;
		}
		dest[(dest_bit_len + i) / 8] |= 1 << ((dest_bit_len + i) % 8);
	}
}

static void svf_parser_append_bit(uint8 *dest, uint32 dest_bit_len, 
								  uint8 *src, uint32 src_bit_len)
{
	uint32 i;
	
	if ((src_bit_len > 0) && ((NULL == src) || (NULL == dest)))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), "source");
		return;
	}
	
	for (i = 0; i < src_bit_len; i++)
	{
		if (((dest_bit_len + i) & 7) == 0)
		{
			dest[(dest_bit_len + i) / 8] = 0;
		}
		if (src[i / 8] & (1 << (i % 8)))
		{
			dest[(dest_bit_len + i) / 8] |= 1 << ((dest_bit_len + i) % 8);
		}
	}
}

static uint8 svf_parser_find_string_in_array(char *str, char **strs, 
											 uint32 num_of_element)
{
	uint8 i;
	
	for (i = 0; i < num_of_element; i++)
	{
		if (!strcmp(str, strs[i]))
		{
			return i;
		}
	}
	
	return 0xFF;
}

static RESULT svf_parser_parse_cmd_string(char *str, uint32 len, char **argus, 
										  uint32 *num_of_argu)
{
	uint32 pos = 0, num = 0;
	uint8 space_found = 1;
	
	while (pos < len)
	{
		switch (str[pos])
		{
		case '\n':
		case '\r':
		case '!':
		case '/':
			LOG_BUG("error when parsing svf command\n");
			return ERROR_FAIL;
			break;
		case ' ':
			space_found = 1;
			str[pos] = '\0';
			break;
		default:
			if (space_found)
			{
				argus[num++] = &str[pos];
				space_found = 0;
			}
			break;
		}
		pos++;
	}
	
	*num_of_argu = num;
	return ERROR_OK;
}

static RESULT svf_parser_copy_hexstring_to_binary(char *str, uint8 **bin, 
												  uint32 orig_bit_len, 
												  uint32 bit_len)
{
	uint32 i, str_len = (uint32)strlen(str);
	uint32 str_byte_len = (bit_len + 3) >> 2, loop_cnt;
	uint8 ch, need_write = 1;
	
	if (ERROR_OK != svf_parser_adjust_array_length(bin, orig_bit_len, bit_len))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "adjust length of array");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (str_byte_len > str_len)
	{
		loop_cnt = str_byte_len;
	}
	else
	{
		loop_cnt = str_len;
	}
	
	for (i = 0; i < loop_cnt; i++)
	{
		if (i < str_len)
		{
			ch = str[str_len - i - 1];
			if ((ch >= '0') && (ch <= '9'))
			{
				ch = ch - '0';
			}
			else if ((ch >= 'A') && (ch <= 'F'))
			{
				ch = ch - 'A' + 10;
			}
			else
			{
				LOG_ERROR(_GETTEXT("invalid hex string\n"));
				return ERROR_FAIL;
			}
		}
		else
		{
			ch = 0;
		}
		
		// check valid
		if (i >= str_byte_len)
		{
			// all data written, 
			// other data should be all '0's and needn't to be written
			need_write = 0;
			if (ch != 0)
			{
				LOG_ERROR(_GETTEXT("value execede length\n"));
				return ERROR_FAIL;
			}
		}
		else if (i == (str_byte_len - 1))
		{
			// last data byte, written if valid
			if ((ch & ~((1 << (bit_len - 4 * i)) - 1)) != 0)
			{
				LOG_ERROR(_GETTEXT("value execede length\n"));
				return ERROR_FAIL;
			}
		}
		
		if (need_write)
		{
			// write bin
			if (i % 2)
			{
				// MSB
				(*bin)[i / 2] |= ch << 4;
			}
			else
			{
				// LSB
				(*bin)[i / 2] = 0;
				(*bin)[i / 2] |= ch;
			}
		}
	}
	
	return ERROR_OK;
}

#define SVFP_CMD_INC_CNT			1024
uint32 svf_parser_get_command(FILE *file, char **cmd_buffer, uint32 *cmd_len)
{
	char *tmp_buffer = NULL;
	uint32 cmd_pos = 0;
	char ch;
	uint8 comment = 0, slash = 0, cmd_ok = 0;
	
	// get 1 valid command
	while (!feof(file) && !cmd_ok)
	{
		ch = (char)fgetc(file);
		svf_file_index++;
		switch (ch)
		{
		case EOF:
			break;
		case '!':
			slash = 0;
			comment = 1;
			break;
		case '/':
			if (++slash == 2)
			{
				comment = 1;
			}
			break;
		case ';':
			slash = 0;
			if (!comment)
			{
				cmd_ok = 1;
			}
			break;
		case '\n':
			svf_line_number++;
		case '\r':
			slash = 0;
			comment = 0;
			break;
		default:
			if (!comment)
			{
				if (cmd_pos >= *cmd_len)
				{
					// 1 more byte for '\0'
					tmp_buffer = (char*)malloc(*cmd_len 
											   + SVFP_CMD_INC_CNT + 1);
					if (NULL == tmp_buffer)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
						return ERRCODE_NOT_ENOUGH_MEMORY;
					}
					if (*cmd_len > 0)
					{
						memcpy(tmp_buffer, *cmd_buffer, *cmd_len);
					}
					if (*cmd_buffer != NULL)
					{
						free(*cmd_buffer);
					}
					*cmd_buffer = tmp_buffer;
					*cmd_len += SVFP_CMD_INC_CNT;
					tmp_buffer = NULL;
				}
				(*cmd_buffer)[cmd_pos++] = (char)toupper(ch);
			}
			break;
		}
	}
	
	if (cmd_ok)
	{
		(*cmd_buffer)[cmd_pos] = '\0';
		return ERROR_OK;
	}
	else
	{
		return ERROR_FAIL;
	}
}

RESULT svf_parser_check_tdo(void)
{
	uint32 i, j, byte_len, index;
	uint32 bit_mask;

	for (i = 0; i < svf_parser_check_index; i++)
	{
		if (svf_parser_check[i].enabled)
		{
			byte_len = (svf_parser_check[i].bit_len + 7) >> 3;
			index = svf_parser_check[i].buffer_offset;
			for (j = 0; j < byte_len; j++)
			{
				if ((   svf_parser_tdi_buffer[index + j] 
						& svf_parser_mask_buffer[index + j]) 
					!= svf_parser_tdo_buffer[index + j])
				{
					if (svf_parser_check[i].bit_len >= 32)
					{
						bit_mask = 0xFFFFFFFF;
					}
					else
					{
						bit_mask = (1 << svf_parser_check[i].bit_len) - 1;
					}
					LOG_ERROR("\
tdo check error at line %d, read = 0x%X, want = 0x%X, mask = 0x%X\n", 
							svf_parser_check[i].line_num, 
							(*(uint32*)(svf_parser_tdi_buffer + index)) 
								& bit_mask, 
							(*(uint32*)(svf_parser_tdo_buffer + index)) 
								& bit_mask, 
							(*(uint32*)(svf_parser_mask_buffer + index)) 
								& bit_mask);
					
					return ERROR_FAIL;
				}
			}
		}
	}
	
	svf_parser_check_index = 0;
	svf_parser_buffer_index = 0;
	
	return ERROR_OK;
}

RESULT svf_parser_add_check_para(uint8 enabled, uint32 buffer_offset, 
								 uint32 bit_len)
{
	if (svf_parser_check_index >= SVF_PARSER_DATA_BUFFER_SIZE)
	{
		LOG_BUG(_GETTEXT("toooooo many operation undone\n"));
		return ERROR_FAIL;
	}
	
	svf_parser_check[svf_parser_check_index].line_num = svf_line_number;
	svf_parser_check[svf_parser_check_index].bit_len = bit_len;
	svf_parser_check[svf_parser_check_index].enabled = enabled;
	svf_parser_check[svf_parser_check_index].buffer_offset = buffer_offset;
	svf_parser_check_index++;
	
	return ERROR_OK;
}

RESULT svf_parser_run_command(char *cmd_str)
{
	char *argus[256];
	uint32 num_of_argu = 0;
	uint32 i, i_tmp;
	uint8 command;
	RESULT ret = ERROR_OK;
	
	// for RUNTEST
	uint32 run_count;
	float min_time, max_time;
	
	// for XXR
	svf_xxr_para_t *xxr_para_tmp;
	uint8 **pbuffer_tmp;
	
	// for STATE
	tap_state_t *path = NULL;
	
	LOG_DEBUG("%s\n", cmd_str);
	
	ret = svf_parser_parse_cmd_string(cmd_str, (uint32)strlen(cmd_str), 
									  argus, &num_of_argu);
	if (ret != ERROR_OK)
	{
		return ERROR_FAIL;
	}
	
	command = svf_parser_find_string_in_array(
										argus[0], (char **)svf_command_name, 
										dimof(svf_command_name));
	
	switch (command)
	{
	case ENDDR:
	case ENDIR:
		if (num_of_argu != 2)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), argus[0]);
			return ERRCODE_INVALID_PARAMETER;
		}
		
		i_tmp = svf_parser_find_string_in_array(
											argus[1], (char **)tap_state_name, 
											dimof(tap_state_name));
		if (!tap_state_is_stable(i_tmp))
		{
			LOG_ERROR(_GETTEXT("state defined is not stable state\n"));
			return ERROR_FAIL;
		}
		if (ENDDR == command)
		{
			svf_parser_para.dr_end_state = i_tmp;
			LOG_DEBUG("\tdr_end_state = %s\n", 
					  tap_state_name[svf_parser_para.dr_end_state]);
		}
		else
		{
			svf_parser_para.ir_end_state = i_tmp;
			LOG_DEBUG("\tir_end_state = %s\n", 
					  tap_state_name[svf_parser_para.ir_end_state]);
		}
		break;
	case FREQUENCY:
		if ((num_of_argu != 1) && (num_of_argu != 3))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), argus[0]);
			return ERRCODE_INVALID_PARAMETER;
		}
		
		if (1 == num_of_argu)
		{
			svf_parser_para.frequency = 0;
		}
		else
		{
			if (strcmp(argus[2], "HZ"))
			{
				LOG_ERROR(_GETTEXT("HZ not found in FREQUENCY command\n"));
				return ERROR_FAIL;
			}
			if (ERROR_OK != tap_commit())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			if (ERROR_OK != svf_parser_check_tdo())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			svf_parser_para.frequency = (float)atof(argus[1]);
			jtag_set_frequency((uint16)(svf_parser_para.frequency / 1000));
			
			LOG_DEBUG("\tfrequency = %f\n", svf_parser_para.frequency);
		}
		break;
	case HDR:
		xxr_para_tmp = &svf_parser_para.hdr_para;
		goto XXR_common;
	case HIR:
		xxr_para_tmp = &svf_parser_para.hir_para;
		goto XXR_common;
	case TDR:
		xxr_para_tmp = &svf_parser_para.tdr_para;
		goto XXR_common;
	case TIR:
		xxr_para_tmp = &svf_parser_para.tir_para;
		goto XXR_common;
	case SDR:
		xxr_para_tmp = &svf_parser_para.sdr_para;
		goto XXR_common;
	case SIR:
		xxr_para_tmp = &svf_parser_para.sir_para;
		goto XXR_common;
XXR_common:
		if ((num_of_argu > 10) || (num_of_argu % 2))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), argus[0]);
			return ERRCODE_INVALID_PARAMETER;
		}
		
		i_tmp = xxr_para_tmp->len;
		xxr_para_tmp->len = atoi(argus[1]);
		LOG_DEBUG("\tlength = %d\n", xxr_para_tmp->len);
		xxr_para_tmp->data_mask = 0;
		for (i = 2; i < num_of_argu; i += 2)
		{
			if ((argus[i + 1][0] != '(') 
				|| (argus[i + 1][strlen(argus[i + 1]) - 1] != ')'))
			{
				LOG_ERROR(_GETTEXT("data section error\n"));
				return ERROR_FAIL;
			}
			
			argus[i + 1][strlen(argus[i + 1]) - 1] = '\0';
			// TDI, TDO, MASK, SMASK
			if (!strcmp(argus[i], "TDI"))
			{
				// TDI
				pbuffer_tmp = &xxr_para_tmp->tdi;
				xxr_para_tmp->data_mask |= XXR_TDI;
			}
			else if (!strcmp(argus[i], "TDO"))
			{
				// TDO
				pbuffer_tmp = &xxr_para_tmp->tdo;
				xxr_para_tmp->data_mask |= XXR_TDO;
			}
			else if (!strcmp(argus[i], "MASK"))
			{
				// MASK
				pbuffer_tmp = &xxr_para_tmp->mask;
				xxr_para_tmp->data_mask |= XXR_MASK;
			}
			else if (!strcmp(argus[i], "SMASK"))
			{
				// SMASK
				pbuffer_tmp = &xxr_para_tmp->smask;
				xxr_para_tmp->data_mask |= XXR_SMASK;
			}
			else
			{
				LOG_ERROR("unknow parameter: %s\n", argus[i]);
				return ERROR_FAIL;
			}
			
			ret = svf_parser_copy_hexstring_to_binary(
										&argus[i + 1][1], pbuffer_tmp, i_tmp, 
										xxr_para_tmp->len);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "parse hex value");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			LOG_DEBUG("\t%s = 0x%X\n", argus[i], 
					  ( **(int**)pbuffer_tmp) 
						& ((1 << (xxr_para_tmp->len)) - 1));
		}
		
		// If a command changes the length of the last scan of the same type 
		// and the MASK parameter is absent, the mask pattern used is all cares
		if (!(xxr_para_tmp->data_mask & XXR_MASK) 
			&& (i_tmp != xxr_para_tmp->len))
		{
			// MASK not defined and length changed
			ret = svf_parser_adjust_array_length(&xxr_para_tmp->mask, i_tmp, 
												 xxr_para_tmp->len);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "adjust length of array");
				return ERRCODE_FAILURE_OPERATION;
			}
			svf_parser_append_1s(xxr_para_tmp->mask, 0, xxr_para_tmp->len);
		}
		
		// do scan if necessary
		if (SDR == command)
		{
			// assemble ir data
			i = 0;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.hdr_para.tdi, 
							svf_parser_para.hdr_para.len);
			i += svf_parser_para.hdr_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.sdr_para.tdi, 
							svf_parser_para.sdr_para.len);
			i += svf_parser_para.sdr_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.tdr_para.tdi, 
							svf_parser_para.tdr_para.len);
			i += svf_parser_para.tdr_para.len;
			
			// add check data
			if (svf_parser_para.sdr_para.data_mask & XXR_TDO)
			{
				// assemble dr mask data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.hdr_para.mask, 
							svf_parser_para.hdr_para.len);
				i += svf_parser_para.hdr_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.sdr_para.mask, 
							svf_parser_para.sdr_para.len);
				i += svf_parser_para.sdr_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.tdr_para.mask, 
							svf_parser_para.tdr_para.len);
				i += svf_parser_para.tdr_para.len;
				// assemble dr check data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.hdr_para.tdo, 
							svf_parser_para.hdr_para.len);
				i += svf_parser_para.hdr_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.sdr_para.tdo, 
							svf_parser_para.sdr_para.len);
				i += svf_parser_para.sdr_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.tdr_para.tdo, 
							svf_parser_para.tdr_para.len);
				i += svf_parser_para.tdr_para.len;
				
				svf_parser_add_check_para(1, svf_parser_buffer_index, i);
			}
			else
			{
				svf_parser_add_check_para(0, svf_parser_buffer_index, i);
			}
			
			tap_scan_dr(&svf_parser_tdi_buffer[svf_parser_buffer_index], i);
			svf_parser_buffer_index += (i + 7) >> 3;
		}
		else if (SIR == command)
		{
			// assemble dr data
			i = 0;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.hir_para.tdi, 
							svf_parser_para.hir_para.len);
			i += svf_parser_para.hir_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.sir_para.tdi, 
							svf_parser_para.sir_para.len);
			i += svf_parser_para.sir_para.len;
			svf_parser_append_bit(
							&svf_parser_tdi_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.tir_para.tdi, 
							svf_parser_para.tir_para.len);
			i += svf_parser_para.tir_para.len;
			
			// add check data
			if (svf_parser_para.sir_para.data_mask & XXR_TDO)
			{
				// assemble dr mask data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.hir_para.mask, 
							svf_parser_para.hir_para.len);
				i += svf_parser_para.hir_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.sir_para.mask, 
							svf_parser_para.sir_para.len);
				i += svf_parser_para.sir_para.len;
				svf_parser_append_bit(
							&svf_parser_mask_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.tir_para.mask, 
							svf_parser_para.tir_para.len);
				i += svf_parser_para.tir_para.len;
				
				// assemble dr check data
				i = 0;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.hir_para.tdo, 
							svf_parser_para.hir_para.len);
				i += svf_parser_para.hir_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.sir_para.tdo, 
							svf_parser_para.sir_para.len);
				i += svf_parser_para.sir_para.len;
				svf_parser_append_bit(
							&svf_parser_tdo_buffer[svf_parser_buffer_index], 
							i, svf_parser_para.tir_para.tdo, 
							svf_parser_para.tir_para.len);
				i += svf_parser_para.tir_para.len;
				
				svf_parser_add_check_para(1, svf_parser_buffer_index, i);
			}
			else
			{
				svf_parser_add_check_para(0, svf_parser_buffer_index, i);
			}
			
			tap_scan_ir(&svf_parser_tdi_buffer[svf_parser_buffer_index], i);
			svf_parser_buffer_index += (i + 7) >> 3;
		}
		break;
	case PIO:
	case PIOMAP:
		LOG_ERROR("PIO and PIOMAP are not supported\n");
		return ERROR_FAIL;
		break;
	case RUNTEST:
		if ((num_of_argu < 3) && (num_of_argu > 11))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), argus[0]);
			return ERRCODE_INVALID_PARAMETER;
		}
		
		// init
		run_count = 0;
		min_time = 0;
		max_time = 0;
		i = 1;
		
		// run_state
		i_tmp = svf_parser_find_string_in_array(
											argus[i], (char **)tap_state_name, 
											dimof(tap_state_name));
		if (tap_state_is_valid(i_tmp))
		{
			if (tap_state_is_stable(i_tmp))
			{
				svf_parser_para.runtest_run_state = i_tmp;
				
				// When a run_state is specified, 
				// the new  run_state becomes the default end_state
				svf_parser_para.runtest_end_state = i_tmp;
				LOG_DEBUG("\trun_state = %s\n", 
						  tap_state_name[svf_parser_para.runtest_run_state]);
				i++;
			}
			else
			{
				LOG_ERROR("%s is not valid state\n", tap_state_name[i_tmp]);
				return ERROR_FAIL;
			}
		}
		
		// run_count run_clk
		if (((i + 2) <= num_of_argu) && strcmp(argus[i + 1], "SEC"))
		{
			if (!strcmp(argus[i + 1], "TCK"))
			{
				// clock source is TCK
				run_count = atoi(argus[i]);
				LOG_DEBUG("\trun_count@TCK = %d\n", run_count);
			}
			else
			{
				LOG_ERROR("%s not supported for clock\n", argus[i + 1]);
				return ERROR_FAIL;
			}
			i += 2;
		}
		
		// min_time SEC
		if (((i + 2) <= num_of_argu) && !strcmp(argus[i + 1], "SEC"))
		{
			min_time = (float)atof(argus[i]);
			LOG_DEBUG("\tmin_time = %fs\n", min_time);
			i += 2;
		}
		
		// MAXIMUM max_time SEC
		if (((i + 3) <= num_of_argu) && !strcmp(argus[i], "MAXIMUM") 
			&& !strcmp(argus[i + 2], "SEC"))
		{
			max_time = (float)atof(argus[i + 1]);
			LOG_DEBUG("\tmax_time = %fs\n", max_time);
			i += 3;
		}
		// ENDSTATE end_state
		if (((i + 2) <= num_of_argu) && !strcmp(argus[i], "ENDSTATE"))
		{
			i_tmp = svf_parser_find_string_in_array(
										argus[i + 1], (char **)tap_state_name, 
										dimof(tap_state_name));
			if (tap_state_is_stable(i_tmp))
			{
				svf_parser_para.runtest_end_state = i_tmp;
				LOG_DEBUG("\tend_state = %s\n", 
						  tap_state_name[svf_parser_para.runtest_end_state]);
			}
			else
			{
				LOG_ERROR("%s is not valid state\n", tap_state_name[i_tmp]);
				return ERROR_FAIL;
			}
			i += 2;
		}
		
		// calculate run_count
		if ((0 == run_count) && (min_time > 0))
		{
			run_count = (uint32)(min_time * svf_parser_para.frequency);
		}
		// all parameter should be parsed
		if (i == num_of_argu)
		{
			if (run_count > 0)
			{
				// TODO: do runtest
				ret = tap_runtest(svf_parser_para.runtest_run_state, 
								  svf_parser_para.runtest_end_state, 
								  run_count);
				if (ret != ERROR_OK)
				{
					return ERROR_FAIL;
				}
			}
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "parse parameter of RUNTEST");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case STATE:
		if (num_of_argu < 2)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), argus[0]);
			return ERRCODE_INVALID_PARAMETER;
		}
		
		if (num_of_argu > 2)
		{
			// STATE pathstate1 ... stable_state
			path = malloc((num_of_argu - 1) * sizeof(tap_state_t));
			for (i = 1; i < num_of_argu; i++)
			{
				path[i - 1] = svf_parser_find_string_in_array(
											argus[i], (char **)tap_state_name, 
											dimof(tap_state_name));
				if (!tap_state_is_valid(path[i - 1]))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_INVALID), 
							  tap_state_name[path[i - 1]], "tap state");
					return ERRCODE_INVALID;
				}
			}
			if (tap_state_is_stable(path[num_of_argu - 1]))
			{
				// last state MUST be stable state
				// TODO: call path_move
				tap_path_move(num_of_argu - 1, path);
				LOG_DEBUG("\tmove to %s by path_move\n", 
						  tap_state_name[path[num_of_argu - 1]]);
			}
			else
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID), 
						  tap_state_name[path[num_of_argu - 1]], "tap state");
				return ERRCODE_INVALID;
			}
			if (NULL != path)
			{
				free(path);
				path = NULL;
			}
		}
		else
		{
			// STATE stable_state
			i_tmp = svf_parser_find_string_in_array(
											argus[1], (char **)tap_state_name, 
											dimof(tap_state_name));
			if (tap_state_is_stable(i_tmp))
			{
				// TODO: call state_move
				tap_end_state(i_tmp);
				tap_state_move();
				LOG_DEBUG("\tmove to %s by state_move\n", tap_state_name[i_tmp]);
			}
			else
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID), 
						  tap_state_name[i_tmp], "tap state");
				return ERRCODE_INVALID;
			}
		}
		break;
	case TRST:
		if (num_of_argu != 2)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), argus[0]);
			return ERRCODE_INVALID_PARAMETER;
		}
		
		if (svf_parser_para.trst_mode != TRST_ABSENT)
		{
			if (ERROR_OK != tap_commit())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			if (ERROR_OK != svf_parser_check_tdo())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			i_tmp = svf_parser_find_string_in_array(
										argus[1], (char **)svf_trst_mode_name, 
										dimof(svf_trst_mode_name));
			switch (i_tmp)
			{
			case TRST_ON:
				if (ERROR_OK != jtag_trst_output())
				{
					return ERROR_FAIL;
				}
				if (ERROR_OK != jtag_trst_0())
				{
					return ERROR_FAIL;
				}
				break;
			case TRST_OFF:
				if (ERROR_OK != jtag_trst_output())
				{
					return ERROR_FAIL;
				}
				if (ERROR_OK != jtag_trst_1())
				{
					return ERROR_FAIL;
				}
				break;
			case TRST_Z:
			case TRST_ABSENT:
				if (ERROR_OK != jtag_trst_input())
				{
					return ERROR_FAIL;
				}
				break;
			default:
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID), argus[1], "TRST mode");
				return ERRCODE_INVALID;
			}
			svf_parser_para.trst_mode = i_tmp;
			LOG_DEBUG("\ttrst_mode = %s\n", 
					  svf_trst_mode_name[svf_parser_para.trst_mode]);
		}
		else
		{
			LOG_ERROR(_GETTEXT(
					  "can not accpet TRST command if trst_mode is ABSENT\n"));
			return ERROR_FAIL;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID), argus[0], "svf command");
		return ERRCODE_INVALID;
		break;
	}
	
	if (verbosity >= DEBUG_LEVEL)
	{
		if ((svf_parser_buffer_index >= 0) 
			&& (((command != STATE) && (command != RUNTEST)) 
				|| ((command == STATE) && (num_of_argu == 2))))
		{
			if (ERROR_OK != tap_commit())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			else if (ERROR_OK != svf_parser_check_tdo())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			switch (command)
			{
			case SIR:
				LOG_DEBUG("\tTDO read = 0x%X\n", 
						  ( *(uint32*)svf_parser_tdi_buffer) 
							& ((1 << svf_parser_para.sir_para.len) - 1));
				break;
			case SDR:
				LOG_DEBUG("\tTDO read = 0x%X\n", 
						  ( *(uint32*)svf_parser_tdi_buffer) 
							& ((1 << svf_parser_para.sdr_para.len) - 1));
				break;
			default:
				break;
			}
		}
	}
	else
	{
		// half of the space is left for next command
		if ((svf_parser_buffer_index >= SVF_PARSER_DATA_BUFFER_SIZE / 2) 
			&& (((command != STATE) && (command != RUNTEST)) 
				|| ((command == STATE) && (num_of_argu == 2))))
		{
			if (ERROR_OK != tap_commit())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "do jtag");
				return ERRCODE_FAILURE_OPERATION;
			}
			else if (ERROR_OK != svf_parser_check_tdo())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "check tdo data");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
	}
	
	return ret;
}


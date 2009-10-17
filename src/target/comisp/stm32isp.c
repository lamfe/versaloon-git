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

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "comisp.h"
#include "stm32isp.h"

#include "comisp_internal.h"

#include "comport.h"

#define CUR_TARGET_STRING			COMISP_STRING
#define cur_chip_param				comisp_chip_param

#define STM32ISP_MAX_ERROR_CNT		10

RESULT stm32isp_sycn(void)
{
	uint8 buffer[1], retry = 10;
	int32 comm_ret;

	if (com_mode.auxpin)
	{
		// DTR <==> Reset
		// RTS <==> BOOT0
		
		// Reset:0, BOOT0:1
		comm_ctrl(0, 1);
		sleep_ms(10);
		// Reset:1, BOOT0:1
		comm_ctrl(1, 1);
		sleep_ms(10);
	}
	
	while(1)
	{
		comm_ret = comm_read(buffer, 1);
		if (comm_ret < 0)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), com_mode.comport);
			return ERRCODE_FAILURE_OPERATION;
		}
		if (comm_ret == 0)
		{
			break;
		}
	}
	
	// write the sync byte to the chip
	buffer[0] = STM32ISP_SYNC;
	if (1 != comm_write(buffer, 1))
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write sync byte");
		return ERRCODE_FAILURE_OPERATION;
	}
	comm_ret = comm_read(buffer, 1);
	if (comm_ret < 0)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read ACK byte from chip");
		return ERRCODE_FAILURE_OPERATION;
	}
	while (((1 == comm_ret) && (0 == buffer[0])) && (--retry > 0))
	{
		comm_ret = comm_read(buffer, 1);
		if (comm_ret < 0)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read ACK byte from chip");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
// check number of bytes actually read
// from here we can deduce the state of the chip:
//	if 1 byte received:
//		if it is "ACK" (0x79) then the chip is sync'd and ready to go
//		if it is 0x00 then it might be garbage received
//		if it is anything else, then ???
//  if 0 bytes received:
//		chip/board could be powered off
//		serial cable could be disconnected
//		chip is already sync'd...
//		chip is not in boot loader mode
//		must have switches set and then chip reset
	if (0 == comm_ret)
	{
		// write bummy byte
		buffer[0] = 0;
		if (1 != comm_write(buffer, 1))
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "send dummy byte to test chip synchronization");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (1 != comm_read(buffer, 1))
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "read ACK byte from chip (2nd attempt)");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	switch (buffer[0])
	{
	case STM32ISP_ACK:
		LOG_DEBUG(_GETTEXT("Chip synced\n"));
		return ERROR_OK;
	case STM32ISP_NACK:
		LOG_DEBUG(_GETTEXT("Chip already synced\n"));
		return ERROR_OK;
	default:
		LOG_DEBUG(_GETTEXT("Chip is in unknown state (response = 0x%02x)\n"), 
				  buffer[0]);
		return ERROR_FAIL;
		break;
	}
}

RESULT stm32isp_send_command(uint8 cmd, const char *cmd_name, 
							 uint8 *test_protect)
{
	uint8 buffer[2];
	
	// send command
	buffer[0] = cmd;
	buffer[1] = ~buffer[0];
	if (2 != comm_write(buffer, 2))
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
				  "send", cmd_name);
		return ERRCODE_FAILURE_OPERATION;
	}
	// get the initial ACK to the command
	if ((1 != comm_read(buffer, 1)) || (buffer[0] != STM32ISP_ACK))
	{
		// fail to receive initial ACK to the command
		if (test_protect == NULL)
		{
			// output message only if not in test protect mode
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "receive initial ACK");
		}
		else
		{
			// set test_protest
			*test_protect = 1;
		}
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT stm32isp_get_ack(uint8 accept_0_as_final_ack, uint8 *ret, uint8 quiet)
{
	uint8 buffer[1];
	
	*ret = (uint8)comm_read(buffer, 1);
	if (*ret != 1)
	{
		return ERROR_FAIL;
	}
	
	if ((buffer[0] != STM32ISP_ACK) 
		&& (!accept_0_as_final_ack || (buffer[0] != 0)))
	{
		if (!quiet)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "receive ACK");
		}
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

#define STM32ISP_SEND				1
#define STM32ISP_RECEIVE			0
// data_len_size > 0 means that data_len bytes 
// MUST be send/received first for data_len_size bytes
RESULT stm32isp_process_data(uint16 *data_len, uint8 data_len_size, 
							 uint8 *data, uint8 send1_receive0)
{
	uint16 actual_len = 0;
	
#if PARAM_CHECK
	if ((NULL == data_len) || (NULL == data))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (send1_receive0)
	{
		// send data
		actual_len = *data_len;
		if (data_len_size)
		{
			// send *data_len first
			(*data_len)--;
			if (data_len_size != comm_write((uint8*)data_len, data_len_size))
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "send data size");
				return ERRCODE_FAILURE_OPERATION;
			}
			*data_len = actual_len;
		}
		// send data
		if (actual_len != comm_write(data, actual_len))
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send data");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	else
	{
		// receive data
		actual_len = *data_len;
		if (data_len_size)
		{
			// receive actual_len first
			// if large than data_len, return with error
			if (data_len_size 
				!= comm_read((uint8*)&actual_len, data_len_size))
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "read data size");
				return ERRCODE_FAILURE_OPERATION;
			}
			actual_len++;
			if (actual_len > *data_len)
			{
				LOG_DEBUG(_GETTEXT("Too many data to receive."));
				return ERROR_FAIL;
			}
			*data_len = actual_len;
		}
		// receive data
		if (actual_len != comm_read(data, actual_len))
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "receive data");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return ERROR_OK;
}

RESULT stm32isp_read_bootloader_version(uint8 *rev)
{
	RESULT ret;
	uint8 buffer[3];
	uint16 len = 3;
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_GET_VERSION, "Get Version", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	// read data
	ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_RECEIVE);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "get bootloader data");
		return ERRCODE_FAILURE_OPERATION;
	}
	// process data
	*rev = buffer[0];
	return stm32isp_get_ack(0, buffer, 0);
}

RESULT stm32isp_read_product_id(uint32 *id)
{
	RESULT ret;
	uint16 len = 4;
	uint8 buffer[1];
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_GET_ID, "Get ID", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	// read data
	ret = stm32isp_process_data(&len, 1, (uint8*)id, STM32ISP_RECEIVE);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "get product id");
		return ERRCODE_FAILURE_OPERATION;
	}
	return stm32isp_get_ack(0, buffer, 0);
}

RESULT stm32isp_readout_protect(void)
{
	RESULT ret;
	uint8 buffer[1];
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_READOUT_PROTECT, 
								"Readout Protect", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return stm32isp_get_ack(1, buffer, 0);
}

RESULT stm32isp_readout_unprotect(void)
{
	RESULT ret;
	uint8 buffer[1];
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_READOUT_UNPROTECT, 
								"Readout Unprotect", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return stm32isp_get_ack(1, buffer, 0);
}

RESULT stm32isp_write_protect(void)
{
	RESULT ret;
	uint8 buffer[1];
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_WRITE_PROTECT, 
								"Write Protect", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return stm32isp_get_ack(1, buffer, 0);
}

RESULT stm32isp_write_unprotect(void)
{
	RESULT ret;
	uint8 buffer[1];
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_WRITE_UNPROTECT, 
								"Write Unprotect", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return stm32isp_get_ack(1, buffer, 0);
}

RESULT stm32isp_read_memory(uint32 addr, uint8 *data, uint16 *data_len)
{
	uint8 buffer[5], test_protect = 0;
	uint16 len = 5;
	RESULT ret;
	int comm_ret;
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_READ_MEMORY, 
								"Read Memory", &test_protect);
	if (ret != ERROR_OK)
	{
		if (1 == test_protect)
		{
			// protected
			LOG_DEBUG(_GETTEXT(
					"readout protect enabled, send Read Unprotect command\n"));
			
			// write unprotect
			if (ERROR_OK != stm32isp_readout_unprotect())
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "unprotect readout");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			// resync
			if (ERROR_OK != stm32isp_sycn())
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "resync to stm32 chip");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			ret = stm32isp_send_command(STM32ISP_CMD_READ_MEMORY, 
								"Read Memory", NULL);
			if (ret != ERROR_OK)
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	// send address + checksum
	buffer[0] = (addr >> 24) & 0xFF;
	buffer[1] = (addr >> 16) & 0xFF;
	buffer[2] = (addr >> 8) & 0xFF;
	buffer[3] = (addr >> 0) & 0xFF;
	buffer[4] = buffer[0] ^ buffer[1] ^ buffer[2] ^ buffer[3];
	ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send address data");
		return ERRCODE_FAILURE_OPERATION;
	}
	// read ack
	if (ERROR_OK != stm32isp_get_ack(0, buffer, 0))
	{
		return ERROR_FAIL;
	}
	// send data
	buffer[0] = (uint8)(*data_len - 1);
	buffer[1] = ~buffer[0];
	len = 2;
	ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send data");
		return ERRCODE_FAILURE_OPERATION;
	}
	// read ack
	if (ERROR_OK != stm32isp_get_ack(0, buffer, 0))
	{
		return ERROR_FAIL;
	}
	// get data
	comm_ret = (uint16)comm_read(data, *data_len);
	if (comm_ret != *data_len)
	{
		if (comm_ret > 0)
		{
			*data_len = (uint16)comm_ret;
		}
		else
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "receive data in Read Memroy command");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return ERROR_OK;
}

RESULT stm32isp_write_memory(uint32 addr, uint8 *data, uint16 data_len)
{
	uint8 buffer[5], test_protect = 0, time_out = 10;
	uint16 len = 5, i;
	RESULT ret;
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_WRITE_MEMORY, 
								"Write Memory", &test_protect);
	if (ret != ERROR_OK)
	{
		if (1 == test_protect)
		{
			// protected
			LOG_DEBUG(_GETTEXT(
					"write protect enabled, send Write Unprotect command\n"));
			
			// write unprotect
			if (ERROR_OK != stm32isp_write_unprotect())
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "unprotect write");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			// resync
			if (ERROR_OK != stm32isp_sycn())
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "resync to stm32 chip");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			ret = stm32isp_send_command(STM32ISP_CMD_WRITE_MEMORY, 
								"Write Memory", NULL);
			if (ret != ERROR_OK)
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	// send address + checksum
	buffer[0] = (addr >> 24) & 0xFF;
	buffer[1] = (addr >> 16) & 0xFF;
	buffer[2] = (addr >> 8) & 0xFF;
	buffer[3] = (addr >> 0) & 0xFF;
	buffer[4] = buffer[0] ^ buffer[1] ^ buffer[2] ^ buffer[3];
	ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send address data");
		return ERRCODE_FAILURE_OPERATION;
	}
	// read ack
	if (ERROR_OK != stm32isp_get_ack(0, buffer, 0))
	{
		return ERROR_FAIL;
	}
	// send data
	len = data_len;
	ret = stm32isp_process_data(&len, 1, data, STM32ISP_SEND);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send data");
		return ERRCODE_FAILURE_OPERATION;
	}
	// send checksum
	buffer[0] = (uint8)(len - 1);
	for (i = 0; i < len; i++)
	{
		buffer[0] ^= data[i];
	}
	len = 1;
	ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send checksum");
		return ERRCODE_FAILURE_OPERATION;
	}
	// get final ack
	while (time_out--)
	{
		if (ERROR_OK != stm32isp_get_ack(0, buffer, 1))
		{
			if (1 == buffer[0])
			{
				time_out = 0;
				break;
			}
			continue;
		}
		else
		{
			break;
		}
	}
	if (!time_out)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read final ack for Write Memory command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT stm32isp_erase_sector(uint8 num_of_sector, uint8 *sector_num)
{
	uint8 buffer[2], dly_cnt, i;
	RESULT ret;
	uint16 len = 5;
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_ERASE, "Erase", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (0xFF == num_of_sector)
	{
		// send erase page count
		buffer[0] = 0xFF;
		buffer[1] = ~buffer[0];
		len = 2;
		ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
		if (ret != ERROR_OK)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send sectors");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	else
	{
		len = num_of_sector;
		ret = stm32isp_process_data(&len, 1, sector_num, STM32ISP_SEND);
		if (ret != ERROR_OK)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send sectors");
			return ERRCODE_FAILURE_OPERATION;
		}
		// send checksum
		buffer[0] = num_of_sector - 1;
		for (i = 0; i < num_of_sector; i++)
		{
			buffer[0] ^= sector_num[i];
		}
		len = 1;
		ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
		if (ret != ERROR_OK)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "send sector checksum");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	// delay
	dly_cnt = 60;
	while (ERROR_OK != stm32isp_get_ack(0, buffer, 1))
	{
		sleep_ms(50);
		if (!--dly_cnt)
		{
			LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "receive final ACK in response to Erase command");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	return ERROR_OK;
}

RESULT stm32isp_execute_code(uint32 addr)
{
	uint8 buffer[5];
	RESULT ret;
	uint16 len = 5;
	
	// send command
	ret = stm32isp_send_command(STM32ISP_CMD_GO, "Go", NULL);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send command");
		return ERRCODE_FAILURE_OPERATION;
	}
	// send address + checksum
	buffer[0] = (addr >> 24) & 0xFF;
	buffer[1] = (addr >> 16) & 0xFF;
	buffer[2] = (addr >> 8) & 0xFF;
	buffer[3] = (addr >> 0) & 0xFF;
	buffer[4] = buffer[0] ^ buffer[1] ^ buffer[2] ^ buffer[3];
	ret = stm32isp_process_data(&len, 0, buffer, STM32ISP_SEND);
	if (ret != ERROR_OK)
	{
		LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "send address data");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return stm32isp_get_ack(0, buffer, 0);
}

RESULT stm32isp_program(operation_t operations, program_info_t *pi)
{
	uint8 page_buf[STM32ISP_PAGE_SIZE];
	uint16 page_size, page_size_tmp;
	uint8 bootloader_version = 0;
	int32 i, j, k, len_current_list;
	uint32 product_id = 0;
	uint32 flash_kb, sram_kb;
	uint8 error_cnt, tmp8;
	RESULT ret = ERROR_OK;
	memlist *ml_tmp;

	// comm init
	ret = comm_open(com_mode.comport, com_mode.baudrate, 8, 
					COMM_PARITYBIT_EVEN, COMM_STOPBIT_1, COMM_HANDSHAKE_NONE);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPEN), com_mode.comport);
		return ERRCODE_FAILURE_OPEN;
	}
	
	// sync first
	if (ERROR_OK != stm32isp_sycn())
	{
		// try again
		if (ERROR_OK != stm32isp_sycn())
		{
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
	}
	// read bootloader version
	if (ERROR_OK != stm32isp_read_bootloader_version(&bootloader_version))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"read bootloader version");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	LOG_INFO(_GETTEXT("Bootloader version %1X.%1X detected\n"), 
			 (bootloader_version & 0xF0) >> 4, bootloader_version & 0x0F);
	// Get ID command returns chip ID (same as JTAG ID)
	if (ERROR_OK != stm32isp_read_product_id(&product_id))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"read product id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	pi->chip_id = product_id;
	LOG_INFO(_GETTEXT("Product id = 0x%04X\n"), pi->chip_id);
	
	// read memory size
	page_size = 4;
	if (ERROR_OK != stm32isp_read_memory(0x1FFFF7E0, page_buf, &page_size))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read memroy size");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	flash_kb = (page_buf[1] << 8) + page_buf[0];
	sram_kb = (page_buf[3] << 8) + page_buf[2];
	LOG_INFO(_GETTEXT("Flash memory size: %i KB, SRAM memory size:  %i KB\n"), 
			 flash_kb, sram_kb);
	
	if (operations.read_operations & CHIP_ID)
	{
		goto leave_program_mode;
	}
	
	// erase
	if (operations.erase_operations > 0)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, '=');
		
/*		if (pi->app_size_valid > 0)
		{
			// erase flash according to data
		}
		else
*/
		{
			// erase all flash
			if (ERROR_OK != stm32isp_erase_sector(0xFF, NULL))
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	page_size = STM32ISP_PAGE_SIZE_TX;
	
	if (operations.write_operations & APPLICATION)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		page_size = STM32ISP_PAGE_SIZE_TX;
		ml_tmp = pi->app_memlist;
		while (ml_tmp != NULL)
		{
			error_cnt = 0;
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = ml_tmp->len;
			for (i = -(int32)(ml_tmp->addr % 1024); 
				 i < ((int32)ml_tmp->len - (int32)(ml_tmp->addr % 1024)); 
				 i += page_size)
			{
				uint32 start_addr = cur_chip_param.flash_start_addr;
				uint32 page_addr = start_addr + i;
				
				ret = stm32isp_write_memory(
									ml_tmp->addr + i, 
									&pi->app[ml_tmp->addr - start_addr + i], 
									page_size);
				if (ret != ERROR_OK)
				{
					uint32 tmp32;
					
					if (++error_cnt > STM32ISP_MAX_ERROR_CNT)
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "write flash page", page_addr);
						ret = ERRCODE_FAILURE_OPERATION_ADDR;
						goto leave_program_mode;
					}
					tmp32 = ml_tmp->addr - start_addr + i;
					tmp8 = (uint8)(tmp32 / 1024);
					if (ERROR_OK != stm32isp_erase_sector(1, &tmp8))
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "write flash page", page_addr);
						ret = ERRCODE_FAILURE_OPERATION_ADDR;
						goto leave_program_mode;
					}
					tmp32 -= tmp8 * 1024;
					len_current_list += tmp32;
					pgbar_update(-(int32)tmp32);
					
					if (i > 1024)
					{
						i = tmp8 * 1024 - (ml_tmp->addr - start_addr);
					}
					else
					{
						i = -(int32)(ml_tmp->addr % 1024);
					}
					i -= page_size;
					continue;
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = ml_tmp->next;
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", 
				 pi->app_size_valid);
	}

	page_size = STM32ISP_PAGE_SIZE_RX;
	
	if (operations.read_operations & APPLICATION)
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			pi->app_size_valid =flash_kb * 1024;
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = pi->app_memlist;
		while (ml_tmp != NULL)
		{
			error_cnt = 0;
			if ((ml_tmp->addr + ml_tmp->len) 
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = ml_tmp->len;
			for (i = -(int32)(ml_tmp->addr % 1024); 
				 i < ((int32)ml_tmp->len - (int32)(ml_tmp->addr % 1024)); 
				 i += page_size)
			{
				uint32 start_addr = cur_chip_param.flash_start_addr;
				uint32 page_addr = start_addr + i;
				
				page_size_tmp = page_size;
				ret = stm32isp_read_memory(ml_tmp->addr + i, page_buf, 
										   &page_size_tmp);
				if ((ret != ERROR_OK) || (page_size_tmp != page_size))
				{
					if (++error_cnt > STM32ISP_MAX_ERROR_CNT)
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "read flash page", page_addr);
						ret = ERRCODE_FAILURE_OPERATION_ADDR;
						goto leave_program_mode;
					}
					
					page_size = page_size_tmp;
					i -= page_size;
					continue;
				}
				else
				{
					// read success, clear error
					error_cnt = 0;
				}
				for (j = 0; j < page_size; j++)
				{
					if (page_buf[j] != pi->app[ml_tmp->addr + i 
											  + j - start_addr])
					{
						pgbar_fini();
						LOG_ERROR(
							_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
							"flash", 
							ml_tmp->addr + i + j, page_buf[j], 
							pi->app[ml_tmp->addr + i + j - start_addr]);
						ret = ERRCODE_FAILURE_VERIFY_TARGET;
						goto leave_program_mode;
					}
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = ml_tmp->next;
		}
		
		pgbar_fini();
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash", 
				 pi->app_size_valid);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "flash");
		}
	}
	
	if (operations.write_operations & LOCK)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "lock");
		pgbar_init("writing lock |", "|", 0, 1, PROGRESS_STEP, '=');
		
		if (ERROR_OK != stm32isp_readout_protect())
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "protect readout");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO("lock programmed\n");
	}
	
	if (comisp_execute_flag && (operations.write_operations & APPLICATION))
	{
		if (ERROR_OK != stm32isp_execute_code(comisp_execute_addr))
		{
			LOG_ERROR("fail to execute code at 0x%04X\n", comisp_execute_addr);
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
	}
	
leave_program_mode:
	comm_close();
	return ret;
}


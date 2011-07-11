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
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "timer.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "avrxmega.h"
#include "avrxmega_internal.h"

#define CUR_TARGET_STRING			AVRXMEGA_STRING

struct program_area_map_t avrxmega_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RAE | AREA_ATTR_RAW | AREA_ATTR_RNP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t avrxmega_program_mode[] =
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'p', 0, IFS_PDI},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(avrxmega);
LEAVE_PROGRAM_MODE_HANDLER(avrxmega);
ERASE_TARGET_HANDLER(avrxmega);
WRITE_TARGET_HANDLER(avrxmega);
READ_TARGET_HANDLER(avrxmega);
struct program_functions_t avrxmega_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(avrxmega),
	LEAVE_PROGRAM_MODE_FUNCNAME(avrxmega),
	ERASE_TARGET_FUNCNAME(avrxmega),
	WRITE_TARGET_FUNCNAME(avrxmega),
	READ_TARGET_FUNCNAME(avrxmega)
};

VSS_HANDLER(avrxmega_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j>\n\
  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz\n\n",
			CUR_TARGET_STRING);
	return ERROR_OK;
}

VSS_HANDLER(avrxmega_mode)
{
	uint8_t mode;
	
	VSS_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case AVRXMEGA_JTAG:
		break;
	case AVRXMEGA_PDI:
		break;
	}
	return ERROR_OK;
}

const struct vss_cmd_t avrxmega_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				avrxmega_help),
	VSS_CMD(	"mode",
				"set programming mode of target for internal call",
				avrxmega_mode),
	VSS_CMD_END
};

#define jtag_init()					interfaces->jtag_hl.init(0)
#define jtag_fini()					interfaces->jtag_hl.fini(0)
#define jtag_config(kHz,a,b,c,d)	\
	interfaces->jtag_hl.config(0, (kHz), (a), (b), (c), (d))
#define jtag_runtest(len)			interfaces->jtag_hl.runtest(0, len)
#define jtag_ir_write(i, len)		\
	interfaces->jtag_hl.ir(0, (uint8_t*)(i), (len), AVRXMEGA_JTAG_RTI_CYCLE, 0)
#define jtag_dr_write(d, len)		\
	interfaces->jtag_hl.dr(0, (uint8_t*)(d), (len), AVRXMEGA_JTAG_RTI_CYCLE, 0)
#define jtag_dr_read(d, len)		\
	interfaces->jtag_hl.dr(0, (uint8_t*)(d), (len), AVRXMEGA_JTAG_RTI_CYCLE, 1)
#define jtag_register_callback(s,r)	\
	interfaces->jtag_hl.register_callback(0, (s), (r))

// retry 4000 times with 1us interval
#define poll_start(ten_us)			interfaces->poll.start((ten_us), 10)
#define poll_end()					interfaces->poll.end()
#define poll_ok(t, s, m, v)			\
	interfaces->poll.checkok((t), 1, (s), (m), (v))
#define poll_fail(t, s, m, v)		\
	interfaces->poll.checkfail((t), 1, (s), (m), (v))

#define delay_ms(ms)				interfaces->delay.delayms((ms) | 0x8000)
#define delay_us(us)				interfaces->delay.delayus((us) & 0x7FFF)
#define commit()					interfaces->peripheral_commit()

#define avrxmega_jtag_ir(ir)		jtag_ir_write((ir), AVRXMEGA_JTAG_INS_Len)

#define pdi_poll_delay_empty_jtag()	\
	do{\
		poll_fail(POLL_CHECK_EQU, 2, 0x01FF, AVRXMEGA_JTAG_EMPTY);\
		poll_ok(POLL_CHECK_UNEQU, 2, 0x01FF, AVRXMEGA_JTAG_DELAY);\
	} while (0)

static uint16_t pdi_append_parity(uint8_t data, enum pdi_parity_t parity)
{
	uint8_t i, p;
	
	p = 0;
	if (PDI_PARITY_ODD == parity)
	{
		p = 1;
	}
	
	for (i = 0; i < 8 * sizeof(data); i++)
	{
		if (data & (1 << i))
		{
			p ^= 1;
		}
	}
	return (p << 8) | data;
}

static struct interfaces_info_t *interfaces = NULL;
static uint8_t avrxmega_progmode = 0;
static struct program_info_t *pi = NULL;
static uint8_t pdi_err = 0;
static uint8_t pdi_append_0 = 0;

static RESULT avrxmegajtag_receive_callback(uint8_t index, enum jtag_irdr_t cmd,
						uint32_t ir, uint8_t *dest_buffer, uint8_t *src_buffer,
						uint16_t bytelen, uint16_t *processed)
{
	REFERENCE_PARAMETER(index);
	
	if (NULL == src_buffer)
	{
		return ERROR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return ERROR_OK;
		break;
	case JTAG_SCANTYPE_DR:
		if ((AVRXMEGA_JTAG_INS_PDICOM == ir)
			&& (2 == bytelen))
		{
			uint16_t dr = src_buffer[0] + ((src_buffer[1] & 1) << 8);
			pdi_err = 0;
			if ((dest_buffer != NULL)
				&&(dr != pdi_append_parity(src_buffer[0], PDI_PARITY_EVEN)))
			{
				pdi_err = 1;
			}
			*processed = 1;
			if (dest_buffer != NULL)
			{
				*dest_buffer = *src_buffer;
			}
		}
		return ERROR_OK;
		break;
	}
	
	return ERROR_FAIL;
}

static RESULT avrxmegajtag_send_callback(uint8_t index, enum jtag_irdr_t cmd,
					uint32_t ir, uint8_t *dest_buffer, uint8_t *src_buffer,
					uint16_t bytelen, uint16_t *processed_len)
{
	REFERENCE_PARAMETER(index);
	
	if (NULL == dest_buffer)
	{
		return ERROR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return ERROR_OK;
		break;
	case JTAG_SCANTYPE_DR:
		if ((AVRXMEGA_JTAG_INS_PDICOM == ir)
			&& (2 == bytelen) && pdi_append_0)
		{
			if (NULL == src_buffer)
			{
				dest_buffer[0] = 0;
			}
			else
			{
				dest_buffer[0] = src_buffer[0];
			}
			dest_buffer[1] = 0x00;
			*processed_len = 2;
		}
		return ERROR_OK;
		break;
	}
	
	return ERROR_FAIL;
}

static RESULT pdi_commit(void)
{
	RESULT ret;
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
	case AVRXMEGA_PDI:
		ret = commit();
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

static RESULT pdi_poll(void)
{
	RESULT ret = ERROR_OK;
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
		pdi_poll_delay_empty_jtag();
		break;
	case AVRXMEGA_PDI:
		ret = ERROR_FAIL;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

static RESULT pdi_write(uint16_t data)
{
	RESULT ret;
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
		ret = jtag_dr_write(&data, AVRXMEGA_JTAG_DR_PDICOM_LEN);
		break;
	case AVRXMEGA_PDI:
		ret = ERROR_FAIL;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

static RESULT pdi_read(uint8_t *data)
{
	RESULT ret;
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
		if (NULL != data)
		{
			*data = 0;
		}
		poll_start(1000);
		pdi_append_0 = 1;
		ret = jtag_dr_read(data, AVRXMEGA_JTAG_DR_PDICOM_LEN);
		pdi_append_0 = 0;
		pdi_poll();
		poll_end();
		break;
	case AVRXMEGA_PDI:
		ret = ERROR_FAIL;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

static RESULT pdi_break(void)
{
	RESULT ret;
	uint16_t dr;
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
		dr = AVRXMEGA_JTAG_BREAK;
		ret = pdi_write(dr);
		break;
	case AVRXMEGA_PDI:
		ret = ERROR_FAIL;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

static RESULT pdi_init(void)
{
	uint8_t ir;
	uint32_t id;
	
	if (NULL == pi)
	{
		return ERROR_FAIL;
	}
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
		if (!pi->frequency)
		{
			pi->frequency = 4500;
		}
		jtag_init();
		jtag_config(pi->frequency, pi->jtag_pos.ub, pi->jtag_pos.ua,
						pi->jtag_pos.bb, pi->jtag_pos.ba);
		ir = AVRXMEGA_JTAG_INS_IDCODE;
		avrxmega_jtag_ir(&ir);
		jtag_dr_read(&id, 32);
		ir = AVRXMEGA_JTAG_INS_PDICOM;
		avrxmega_jtag_ir(&ir);
		pdi_break();
		pdi_break();
		if (ERROR_OK != pdi_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "init jtag");
			return ERROR_FAIL;
		}
		LOG_INFO(INFOMSG_REG_08X, "JTAGID", id);
		jtag_register_callback(avrxmegajtag_send_callback,
								   avrxmegajtag_receive_callback);
		break;
	case AVRXMEGA_PDI:
		break;
	default:
		return ERROR_FAIL;
	}
	
	return pdi_commit();
}

static RESULT pdi_fini(void)
{
	RESULT ret;
	
	switch (avrxmega_progmode)
	{
	case AVRXMEGA_JTAG:
		jtag_fini();
		ret = pdi_commit();
		jtag_register_callback(NULL, NULL);
		break;
	case AVRXMEGA_PDI:
		ret = ERROR_FAIL;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}





static const uint64_t pdi_en_key = PDI_NVM_KEY;

static RESULT pdi_write_parity(uint8_t *data, uint16_t bytelen)
{
	uint16_t dr;
	uint16_t i;
	
	for (i = 0; i < bytelen; i++)
	{
		dr = pdi_append_parity(data[i], PDI_PARITY_EVEN);
		if (ERROR_OK != pdi_write(dr))
		{
			return ERROR_FAIL;
		}
	}
	
	return ERROR_OK;
}
static RESULT pdi_write_parity_value(uint32_t data, uint8_t size)
{
	if (size > 4)
	{
		return ERROR_FAIL;
	}
	return pdi_write_parity((uint8_t*)&data, size);
}

static RESULT pdi_write_key(uint8_t *key, uint8_t bytelen)
{
	pdi_write_parity_value(PDI_INSTR_KEY, 1);
	pdi_write_parity(key, bytelen);
	return ERROR_OK;
}

static RESULT pdi_write_reg(uint8_t reg, uint8_t value)
{
	pdi_write_parity_value(PDI_INSTR_STCS(reg), 1);
	pdi_write_parity_value(value, 1);
	
	return ERROR_OK;
}

static RESULT pdi_read_reg(uint8_t reg, uint8_t *value)
{
	pdi_write_parity_value(PDI_INSTR_LDCS(reg), 1);
	pdi_read(value);
	
	return ERROR_OK;
}

static RESULT pdi_read_memory(uint32_t addr, uint16_t size, uint8_t *buff)
{
	uint16_t i;
	
	if (size > 256)
	{
		return ERROR_FAIL;
	}
	
	// write target address on PDIBUS
	pdi_write_parity_value(
				PDI_INSTR_ST(PDI_PTR_DIRECT, PDI_DATASIZE_4BYTES), 1);
	pdi_write_parity_value(addr, 4);
	
	// set repeat size
	pdi_write_parity_value(PDI_INSTR_REPEAT(PDI_DATASIZE_1BYTE), 1);
	pdi_write_parity_value(size - 1, 1);
	
	// read data
	pdi_write_parity_value(
				PDI_INSTR_LD(PDI_PTR_INDIRECT_PI, PDI_DATASIZE_1BYTE), 1);
	for (i = 0; i < size; i++)
	{
		pdi_read(buff + i);
	}
	return ERROR_OK;
}

static RESULT pdi_write_memory(uint32_t addr, uint16_t size, uint8_t *buff)
{
	uint16_t i;
	
	if (size > 256)
	{
		return ERROR_FAIL;
	}
	
	// write target address on PDIBUS
	pdi_write_parity_value(
				PDI_INSTR_ST(PDI_PTR_DIRECT, PDI_DATASIZE_4BYTES), 1);
	pdi_write_parity_value(addr, 4);
	
	// set repeat size
	pdi_write_parity_value(PDI_INSTR_REPEAT(PDI_DATASIZE_1BYTE), 1);
	pdi_write_parity_value(size - 1, 1);
	
	// read data
	pdi_write_parity_value(
				PDI_INSTR_ST(PDI_PTR_INDIRECT_PI, PDI_DATASIZE_1BYTE), 1);
	for (i = 0; i < size; i++)
	{
		pdi_write_parity_value(buff[i], 1);
	}
	return ERROR_OK;
}

RESULT pdi_sts(uint32_t addr, uint8_t data)
{
	pdi_write_parity_value(
				PDI_INSTR_STS(PDI_DATASIZE_4BYTES, PDI_DATASIZE_1BYTE), 1);
	pdi_write_parity_value(addr, 4);
	pdi_write_parity_value(data, 1);
	return ERROR_OK;
}

RESULT pdi_lds(uint32_t addr, uint8_t *data)
{
	pdi_write_parity_value(
				PDI_INSTR_LDS(PDI_DATASIZE_4BYTES, PDI_DATASIZE_1BYTE), 1);
	pdi_write_parity_value(addr, 4);
	pdi_read(data);
	return ERROR_OK;
}





#define nvm_poll_bus_ready()		\
	do{\
		poll_ok(POLL_CHECK_EQU, 1, PDI_REG_STATUS_NVM, PDI_REG_STATUS_NVM);\
	} while(0)
#define nvm_poll_control_ready()	\
	do{\
		poll_ok(POLL_CHECK_EQU, 1, AVRXMEGA_NVM_REG_STATUS_BUSY, 0x00);\
	} while(0)
static RESULT avrxmega_nvm_pollready(void)
{
	poll_start(5000);
	
	pdi_read_reg(PDI_REG_STATUS, NULL);
	nvm_poll_bus_ready();
	
	pdi_lds(AVRXMEGA_NVM_REG_STATUS, NULL);
	nvm_poll_control_ready();
	
	poll_end();
	
	return ERROR_OK;
}

static RESULT avrxmega_nvm_read(uint32_t addr, uint16_t size, uint8_t *buff)
{
	avrxmega_nvm_pollready();
	
	// write AVRXMEGA_NVM_CMD_READNVM to NVM_CMD
	pdi_sts(AVRXMEGA_NVM_REG_CMD, AVRXMEGA_NVM_CMD_READNVM);
	
	return pdi_read_memory(addr, size, buff);
}

static RESULT avrxmega_nvm_writepage(uint8_t write_buff_cmd,
	uint8_t erase_buff_cmd, uint8_t write_page_cmd, uint32_t addr,
	uint16_t size, uint8_t *buff)
{
	avrxmega_nvm_pollready();
	
	pdi_sts(AVRXMEGA_NVM_REG_CMD, erase_buff_cmd);
	pdi_sts(AVRXMEGA_NVM_REG_CTRLA, AVRXMEGA_NVM_REG_CTRLA_CMDEX);
	
	avrxmega_nvm_pollready();
	
	pdi_sts(AVRXMEGA_NVM_REG_CMD, write_buff_cmd);
	pdi_write_memory(addr, size, buff);
	
//	avrxmega_nvm_pollready();
	
	pdi_sts(AVRXMEGA_NVM_REG_CMD, write_page_cmd);
	pdi_sts(addr, 0x00);
	
//	avrxmega_nvm_pollready();
	return pdi_commit();
//	return ERROR_OK;
}

#if 0
static RESULT avrxmega_nvm_writebyte(uint8_t cmd, uint32_t addr, uint8_t data)
{
	avrxmega_nvm_pollready();
	
	// write cmd to NVM_CMD
	pdi_sts(AVRXMEGA_NVM_REG_CMD, cmd);
	
	// write new byte to the target
	pdi_sts(addr, data);
	
	return ERROR_OK;
}
#endif

static RESULT avrxmega_nvm_chip_erase(uint8_t cmd)
{
	avrxmega_nvm_pollready();
	
	// write cmd to NVM_CMD
	pdi_sts(AVRXMEGA_NVM_REG_CMD, cmd);
	
	// write AVRXMEGA_NVM_REG_CTRLA_CMDEX to NVM_CTRLA
	pdi_sts(AVRXMEGA_NVM_REG_CTRLA, AVRXMEGA_NVM_REG_CTRLA_CMDEX);
	
	avrxmega_nvm_pollready();
	return pdi_commit();
}

static RESULT avrxmega_nvm_erase_target(uint8_t cmd, uint32_t addr)
{
	avrxmega_nvm_pollready();
	
	// write cmd to NVM_CMD
	pdi_sts(AVRXMEGA_NVM_REG_CMD, cmd);
	
	pdi_sts(addr, 0x00);
	
	avrxmega_nvm_pollready();
	return pdi_commit();
}





ENTER_PROGRAM_MODE_HANDLER(avrxmega)
{
	interfaces = context->prog;
	avrxmega_progmode = context->pi->mode;
	pi = context->pi;
	pdi_append_0 = 0;
	pdi_err = 0;
	
	// init
	if (ERROR_OK != pdi_init())
	{
		return ERROR_FAIL;
	}
	
	// force reset
	pdi_write_reg(PDI_REG_RESET, PDI_REG_RESET_KEY);
	// +0 Guard Time
	pdi_write_reg(PDI_REG_CTRL, 0x07);
	// nvm bus enable key
	pdi_write_key((uint8_t*)&pdi_en_key, sizeof(pdi_en_key));
	// wait ready
	poll_start(5000);
	pdi_read_reg(PDI_REG_STATUS, NULL);
	nvm_poll_bus_ready();
	poll_end();
	return pdi_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(avrxmega)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	pdi_write_reg(PDI_REG_RESET, 0x00);
	avrxmega_nvm_pollready();
	if (ERROR_OK != pdi_fini())
	{
		return ERROR_FAIL;
	}
	
	return pdi_commit();
}

ERASE_TARGET_HANDLER(avrxmega)
{
	struct operation_t *op = context->op;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	if (op->erase_operations & ALL)
	{
		// chip erase
		ret = avrxmega_nvm_chip_erase(AVRXMEGA_NVM_CMD_CHIPERASE);
	}
	else
	{
		uint8_t cmd;
		uint32_t addr;
		
		switch (area)
		{
		case BOOTLOADER_CHAR:
			cmd = AVRXMEGA_NVM_CMD_ERASEBOOTSEC;
			addr = 0;
			break;
		case APPLICATION_CHAR:
			cmd = AVRXMEGA_NVM_CMD_ERASEAPPSEC;
			addr = 0;
			break;
		case EEPROM_CHAR:
			cmd = AVRXMEGA_NVM_CMD_ERASEEEPROM;
			addr = 0;
			break;
		case USRSIG_CHAR:
			cmd = AVRXMEGA_NVM_CMD_ERASEUSERSIG;
			addr = 0;
			break;
		default:
			return ERROR_FAIL;
			break;
		}
		ret = avrxmega_nvm_erase_target(cmd, addr);
	}
	
	return ret;
}

WRITE_TARGET_HANDLER(avrxmega)
{
	uint8_t write_buff_cmd;
	uint8_t erase_buff_cmd;
	uint8_t write_page_cmd;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		write_buff_cmd = AVRXMEGA_NVM_CMD_LOADFLASHPAGEBUFF;
		erase_buff_cmd = AVRXMEGA_NVM_CMD_ERASEFLASHPAGEBUFF;
		write_page_cmd = AVRXMEGA_NVM_CMD_WRITEFLASHPAGE;
		addr += AVRXMEGA_PDIBUS_APP_BASE;
		goto do_write_page;
	case EEPROM_CHAR:
		write_buff_cmd = AVRXMEGA_NVM_CMD_LOADEEPROMPAGEBUFF;
		erase_buff_cmd = AVRXMEGA_NVM_CMD_ERASEEEPROMPAGEBUFF;
		write_page_cmd = AVRXMEGA_NVM_CMD_WRITEEEPROMPAGE;
		addr += AVRXMEGA_PDIBUS_EE_BASE;
do_write_page:
		avrxmega_nvm_writepage(write_buff_cmd, erase_buff_cmd,
								write_page_cmd, addr, (uint16_t)size, buff);
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

READ_TARGET_HANDLER(avrxmega)
{
	struct chip_param_t *param = context->param;
	RESULT ret = ERROR_OK;
	uint16_t cur_size, page_size;
	uint32_t cur_addr;
	
	switch (area)
	{
	case CHIPID_CHAR:
		pdi_read_memory(0x01000090, 3, buff);
		ret = pdi_commit();
		{
			uint8_t tmp;
			tmp = buff[0];
			buff[0] = buff[2];
			buff[2] = tmp;
		}
		break;
	case EEPROM_CHAR:
		cur_addr = addr + AVRXMEGA_PDIBUS_EE_BASE;
		page_size =(uint16_t)param->chip_areas[EEPROM_IDX].page_size;
		goto do_read;
	case APPLICATION_CHAR:
		cur_addr = addr + AVRXMEGA_PDIBUS_APP_BASE;
		page_size =(uint16_t)param->chip_areas[APPLICATION_IDX].page_size;
do_read:
		while (size > 0)
		{
			if (size > page_size)
			{
				cur_size = page_size;
			}
			else
			{
				cur_size = (uint16_t)size;
			}
			
			avrxmega_nvm_read(cur_addr, cur_size, buff);
			
			cur_addr += cur_size;
			size -= cur_size;
			buff += cur_size;
			pgbar_update(cur_size);
		}
		ret = pdi_commit();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ret;
}

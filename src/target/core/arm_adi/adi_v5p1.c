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

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "vsprog.h"
#include "programmer.h"

#include "adi_v5p1.h"


static programmer_info_t *adi_prog = NULL;
static adi_dp_if_t *adi_dp_if;
static adi_dp_t adi_dp;
static uint8_t ack_value;
adi_dp_info_t adi_dp_info;

// Reset
#define reset_init()			adi_prog->jtag_hl_aux_io_init()
#define reset_fini()			adi_prog->jtag_hl_aux_io_fini()
#define reset_output()			adi_prog->jtag_hl_aux_io_config(JTAG_SRST, 1)
#define reset_input()			adi_prog->jtag_hl_aux_io_config(JTAG_SRST, 0)
#define reset_set()				reset_input()
#define reset_clr()				adi_prog->jtag_hl_aux_io_out(JTAG_SRST, 0)

// JTAG
#define jtag_init()				adi_prog->jtag_hl_init()
#define jtag_fini()				adi_prog->jtag_hl_fini()
#define jtag_config(kHz,a,b,c,d)	\
							adi_prog->jtag_hl_config((kHz), (a), (b), (c), (d))
#define jtag_tms(tms, len)		adi_prog->jtag_hl_tms((tms), (len))
#define jtag_runtest(len)		adi_prog->jtag_hl_runtest(len)
#define jtag_ir_w(ir, len)		adi_prog->jtag_hl_ir((uint8_t*)(ir), (len), 1, 0)
#define jtag_dr_w(dr, len)		adi_prog->jtag_hl_dr((uint8_t*)(dr), (len), 1, 0)
#define jtag_dr_rw(dr, len)		adi_prog->jtag_hl_dr((uint8_t*)(dr), (len), 1, 1)

#define jtag_delay_us(us)		adi_prog->jtag_hl_delay_us((us))
#define jtag_delay_ms(ms)		adi_prog->jtag_hl_delay_ms((ms))
#define jtag_register_callback(s,r)	\
								adi_prog->jtag_hl_register_callback((s), (r))
#define jtag_commit()			adi_prog->jtag_hl_commit()

// SWJ
#define swj_init()				adi_prog->swj_init()
#define swj_fini()				adi_prog->swj_fini()
#define swj_seqout(b, l)		adi_prog->swj_seqout((b), (l))
#define swj_seqin(b, l)			adi_prog->swj_seqin((b), (l))
#define swj_transact(r, v)		adi_prog->swj_transact((r), (v))
#define swj_setpara(t, r, d)	adi_prog->swj_setpara((t), (r), (d))
#define swj_commit(ack)			adi_prog->swj_commit(ack)

RESULT adi_dp_read_reg(uint8_t reg_addr, uint32_t *value, uint8_t check_result);
RESULT adi_dp_write_reg(uint8_t reg_addr, uint32_t *value, uint8_t check_result);
RESULT adi_dp_transaction_endcheck(void);

const uint8_t adi_swj_reset_seq[] = 
{
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

const uint8_t adi_jtag_to_swj_seq[] = 
{
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
	// 16-bit JTAG-to-SWD sequence
	0x9E, 0xE7, 
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F
};

const uint8_t adi_swj_to_jtag_seq[] = 
{
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
	// 16-bit JTAG-to-SWD sequence
	0x3C, 0xE7, 
	// at least 5-bit '1'
	0xFF
};

RESULT adi_dpif_receive_callback(jtag_irdr_t cmd, uint32_t ir, 
								 uint8_t *dest_buffer, uint8_t *src_buffer, 
								 uint16_t buffer_len, uint16_t *processed)
{
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
		if ((5 == buffer_len) 
			&& ((ADI_JTAGDP_IR_DPACC == ir) || (ADI_JTAGDP_IR_APACC == ir)))
		{
			*processed = 1;
			adi_dp.ack = src_buffer[0] & 0x07;
			if (dest_buffer != NULL)
			{
				dest_buffer[0] = (src_buffer[0] >> 3) | (src_buffer[1] << 5);
				dest_buffer[1] = (src_buffer[1] >> 3) | (src_buffer[2] << 5);
				dest_buffer[2] = (src_buffer[2] >> 3) | (src_buffer[3] << 5);
				dest_buffer[3] = (src_buffer[3] >> 3) | (src_buffer[4] << 5);
			}
		}
		return ERROR_OK;
		break;
	}
	
	return ERROR_FAIL;
}

static uint8_t adi_dp_first3bits;
RESULT adi_dpif_send_callback(jtag_irdr_t cmd, uint32_t ir, 
							  uint8_t *dest_buffer, uint8_t *src_buffer, 
							  uint16_t buffer_len, uint16_t *processed_len)
{
	if ((NULL == src_buffer) || (NULL == dest_buffer))
	{
		return ERROR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return ERROR_OK;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == buffer_len) 
			&& ((ADI_JTAGDP_IR_DPACC == ir) || (ADI_JTAGDP_IR_APACC == ir)))
		{
			*processed_len = 5;
			
			adi_dp_first3bits &= 0x07;
			dest_buffer[0] = adi_dp_first3bits | (src_buffer[0] << 3);
			dest_buffer[1] = (src_buffer[0] >> 5) | (src_buffer[1] << 3);
			dest_buffer[2] = (src_buffer[1] >> 5) | (src_buffer[2] << 3);
			dest_buffer[3] = (src_buffer[2] >> 5) | (src_buffer[3] << 3);
			dest_buffer[4] = (src_buffer[3] >> 5);
		}
		return ERROR_OK;
		break;
	}
	
	return ERROR_FAIL;
}

RESULT adi_dp_commit(void)
{
	switch (adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		return jtag_commit();
		break;
	case ADI_DP_SWJ:
		return swj_commit(&adi_dp.ack);
		break;
	default:
		return ERROR_FAIL;
	}
}

static RESULT adi_dpif_fini(void)
{
	adi_dp_if_type_t dp_type;
	RESULT ret = ERROR_OK;
	
	if (NULL == adi_dp_if)
	{
		LOG_BUG(_GETTEXT("adi dp not initialized\n"));
		return ERROR_FAIL;
	}
	
	reset_fini();
	
	dp_type = adi_dp_if->type;
	switch(dp_type)
	{
	case ADI_DP_JTAG:
		jtag_register_callback(NULL, NULL);
		adi_dp_commit();
		ret = jtag_fini();
		adi_dp_if = NULL;
		break;
	case ADI_DP_SWJ:
		adi_dp_commit();
		ret = swj_fini();
		adi_dp_if = NULL;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

static RESULT adi_dpif_init(programmer_info_t *prog, adi_dp_if_t *interf)
{
	if ((NULL == prog) || (NULL == interf) 
		|| ((interf->type != ADI_DP_JTAG) && (interf->type != ADI_DP_SWJ)))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERROR_FAIL;
	}
	
	adi_prog = prog;
	adi_dp_if = interf;
	
	reset_init();
	reset_input();
	
	switch(adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		ack_value = ADI_JTAGDP_ACK_OK_FAIL;
		jtag_init();
		jtag_config(adi_dp_if->adi_dp_if_info.adi_dp_jtag.jtag_khz, 
				adi_dp_if->adi_dp_if_info.adi_dp_jtag.ub + target_jtag_pos.ub, 
				adi_dp_if->adi_dp_if_info.adi_dp_jtag.ua + target_jtag_pos.ua, 
				adi_dp_if->adi_dp_if_info.adi_dp_jtag.bb + target_jtag_pos.bb, 
				adi_dp_if->adi_dp_if_info.adi_dp_jtag.ba + target_jtag_pos.ba);
		jtag_tms((uint8_t*)adi_swj_to_jtag_seq, sizeof(adi_swj_to_jtag_seq) * 8);
		if (ERROR_OK == adi_dp_commit())
		{
			jtag_register_callback(adi_dpif_send_callback, 
								   adi_dpif_receive_callback);
			return ERROR_OK;
		}
		else
		{
			return ERROR_FAIL;
		}
		break;
	case ADI_DP_SWJ:
		ack_value = ADI_SWJDP_ACK_OK;
		swj_init();
		swj_setpara(adi_dp_if->adi_dp_if_info.adi_dp_swj.swj_trn, 
					adi_dp_if->adi_dp_if_info.adi_dp_swj.swj_retry, 
					adi_dp_if->adi_dp_if_info.adi_dp_swj.swj_dly);
		swj_seqout((uint8_t*)adi_jtag_to_swj_seq, 
				   sizeof(adi_jtag_to_swj_seq) * 8);
		return adi_dp_commit();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
}

RESULT adi_dp_scan(uint8_t instr, uint8_t reg_addr, uint8_t RnW, uint32_t *value)
{
	switch(adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		// convert to JTAG IR
		switch (instr)
		{
		case ADI_DP_IR_DPACC:
			instr = ADI_JTAGDP_IR_DPACC;
			break;
		case ADI_DP_IR_APACC:
			instr = ADI_JTAGDP_IR_APACC;
			break;
		default:
			LOG_BUG(_GETTEXT("Invalid instruction\n"));
			return ERROR_FAIL;
			break;
		}
		// scan ir if necessary
		if (adi_dp.cur_ir != instr)
		{
			adi_dp.cur_ir = instr;
			jtag_ir_w(&instr, ADI_JTAGDP_IRLEN);
		}
		
		// memory access tck clocks
		if ((ADI_JTAGDP_IR_APACC == instr) 
			&& ((reg_addr == ADI_AP_REG_DRW) 
				|| ((reg_addr & 0xF0) == ADI_AP_REG_BD0)) 
			&& (adi_dp_if->memaccess_tck != 0))
		{
			jtag_runtest(adi_dp_if->memaccess_tck);
		}
		
		// scan dr
		adi_dp_first3bits = ((reg_addr >> 1) & 0x06) | (RnW & 1);
		if (RnW)
		{
			// read
			jtag_dr_rw(value, ADI_JTAGDP_IR_APDPACC_LEN);
		}
		else
		{
			// write
			jtag_dr_w(value, ADI_JTAGDP_IR_APDPACC_LEN);
		}
		break;
	case ADI_DP_SWJ:
		if (instr > 1)
		{
			LOG_BUG(_GETTEXT("Invalid instruction\n"));
			return ERROR_FAIL;
		}
		// switch reg_addr
		reg_addr = (reg_addr << 1) & 0x18;
		swj_transact(reg_addr | ((RnW & 1) << 2) 
						| ((instr & 1) << 1), value);
		break;
	}
	
	return ERROR_OK;
}

RESULT adi_dpif_read_id(uint32_t *id)
{
	uint16_t ir;
	
	if (NULL == adi_dp_if)
	{
		LOG_BUG(_GETTEXT("adi dp not initialized\n"));
		return ERROR_FAIL;
	}
	
	switch(adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		ir = ADI_JTAGDP_IR_IDCODE;
		jtag_ir_w(&ir, ADI_JTAGDP_IRLEN);
		jtag_dr_rw(id, ADI_JTAGDP_IR_IDCODE_LEN);
		
		if (ERROR_OK != adi_dp_commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read JTAG_ID");
			return ERRCODE_FAILURE_OPERATION;
		}
		LOG_INFO(_GETTEXT("JTAG_ID: 0x%X.\n"), *id);
		break;
	case ADI_DP_SWJ:
		adi_dp_read_reg(ADI_SWJDP_REG_DPIDR, id, 0);
		
		if (ERROR_OK != adi_dp_commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read SWJ_ID");
			return ERRCODE_FAILURE_OPERATION;
		}
		LOG_INFO(_GETTEXT("SWJ_ID read is 0x%X.\n"), *id);
		break;
	default:
		return ERROR_FAIL;
		break;
	}

	return ERROR_OK;
}

// codes below are interface independent
RESULT adi_dp_rw(uint8_t instr, uint8_t reg_addr, uint8_t RnW, uint32_t *value, 
					 uint8_t check_result)
{
	adi_dp_scan(instr, reg_addr, RnW, value);
	
	// read result, DP registers of SWJ are not posted
	if ((ADI_DAP_READ == RnW) 
		&& ((adi_dp_if->type == ADI_DP_JTAG) || (instr == ADI_DP_IR_APACC)))
	{
		adi_dp_scan(ADI_DP_IR_DPACC, ADI_DP_REG_RDBUFF, ADI_DAP_READ, value);
	}
	
	if (check_result)
	{
		return adi_dp_transaction_endcheck();
	}
	
	return ERROR_OK;
}

RESULT adi_dp_read_reg(uint8_t reg_addr, uint32_t *value, uint8_t check_result)
{
	if (NULL == adi_dp_if)
	{
		LOG_BUG(_GETTEXT("adi dp not initialized\n"));
		return ERROR_FAIL;
	}
	
	return adi_dp_rw(ADI_DP_IR_DPACC, reg_addr, ADI_DAP_READ, 
					 value, check_result);
}

RESULT adi_dp_write_reg(uint8_t reg_addr, uint32_t *value, uint8_t check_result)
{
	if (NULL == adi_dp_if)
	{
		LOG_BUG(_GETTEXT("adi dp not initialized\n"));
		return ERROR_FAIL;
	}
	
	return adi_dp_rw(ADI_DP_IR_DPACC, reg_addr, ADI_DAP_WRITE, 
					 value, check_result);
}

RESULT adi_dp_transaction_endcheck(void)
{
	uint32_t ctrl_stat;
	uint32_t cnt = 20;
	
	do
	{
		ctrl_stat = 0;
		adi_dp_rw(ADI_DP_IR_DPACC, ADI_DP_REG_CTRL_STAT, 
					  ADI_DAP_READ, &ctrl_stat, 0);
		if (ERROR_OK != adi_dp_commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "access dap");
			return ERRCODE_FAILURE_OPERATION;
		}
	} while ((adi_dp.ack != ack_value) && (--cnt));
	
	if (!cnt)
	{
		LOG_ERROR(_GETTEXT("Timeout......\n"));
		return ERROR_FAIL;
	}
	
	// check ctrl_stat
	if (ctrl_stat & (ADI_DP_REG_CTRL_STAT_SSTICKYORUN 
						| ADI_DP_REG_CTRL_STAT_SSTICKYERR 
						| ADI_DP_REG_CTRL_STAT_WDATAERR))
	{
		LOG_ERROR(_GETTEXT("Stiky Error/Overrun.\n"));
		return ERROR_FAIL;
	}

	return ERROR_OK;
}

RESULT adi_dp_bankselect(uint8_t ap_reg)
{
	uint32_t select = ap_reg & 0x000000F0;
	
	if (select != adi_dp.dp_sel_value)
	{
		select |= adi_dp.ap_sel_value;
		adi_dp_write_reg(ADI_DP_REG_SELECT, &select, 0);
		adi_dp.dp_sel_value = select;
	}
	
	return ERROR_OK;
}

RESULT adi_ap_read_reg(uint8_t reg_addr, uint32_t *value, uint8_t check_result)
{
	adi_dp_bankselect(reg_addr);
	return adi_dp_rw(ADI_DP_IR_APACC, reg_addr, ADI_DAP_READ, value, 
					 check_result);
}

RESULT adi_ap_write_reg(uint8_t reg_addr, uint32_t *value, uint8_t check_result)
{
	adi_dp_bankselect(reg_addr);
	return adi_dp_rw(ADI_DP_IR_APACC, reg_addr, ADI_DAP_WRITE, value, 
					 check_result);
}

RESULT adi_dp_setup_accessport(uint32_t csw, uint32_t tar)
{
	csw = csw | ADI_AP_REG_CSW_DBGSWENABLE | ADI_AP_REG_CSW_MASTER_DEBUG 
			| ADI_AP_REG_CSW_HPROT;
	if (csw != adi_dp.ap_csw_value)
	{
		adi_ap_write_reg(ADI_AP_REG_CSW, &csw, 0);
		adi_dp.ap_csw_value = csw;
	}
	if (tar != adi_dp.ap_tar_value)
	{
		adi_ap_write_reg(ADI_AP_REG_TAR, &tar, 0);
		adi_dp.ap_tar_value = tar;
	}
	if (csw & ADI_AP_REG_CSW_ADDRINC_MASK)
	{
		adi_dp.ap_tar_value = 0xFFFFFFFF;
	}
	return ERROR_OK;
}

RESULT adi_memap_write_reg(uint32_t address, uint32_t *reg, uint8_t check_result)
{
	adi_dp_setup_accessport(ADI_AP_REG_CSW_32BIT | ADI_AP_REG_CSW_ADDRINC_OFF, 
							address & 0xFFFFFFF0);
	
	return adi_ap_write_reg(ADI_AP_REG_BD0 | (address & 0x0000000C), reg, 
							check_result);
}

RESULT adi_memap_read_reg(uint32_t address, uint32_t *reg, uint8_t check_result)
{
	adi_dp_setup_accessport(ADI_AP_REG_CSW_32BIT | ADI_AP_REG_CSW_ADDRINC_OFF, 
							address & 0xFFFFFFF0);
	
	return adi_ap_read_reg(ADI_AP_REG_BD0 | (address & 0x0000000C), reg, 
						   check_result);
}

uint32_t adi_memap_get_max_tar_block_size(uint32_t tar_autoincr_block, 
										uint32_t address)
{
	return (tar_autoincr_block - ((tar_autoincr_block - 1) & address)) >> 2;
}

// only support 32-bit aligned operation
RESULT adi_memap_write_buf(uint32_t address, uint8_t *buffer, uint32_t len)
{
	uint32_t block_dword_size, write_count;
	
	if ((address & 0x03) || (len & 0x03) || (NULL == buffer) || (0 == len))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	
	while (len > 0)
	{
		block_dword_size = \
			adi_memap_get_max_tar_block_size(adi_dp_if->tar_autoincr_block, 
											 address);
		if (block_dword_size > (len >> 2))
		{
			block_dword_size = len >> 2;
		}
		
		adi_dp_setup_accessport(
			ADI_AP_REG_CSW_32BIT | ADI_AP_REG_CSW_ADDRINC_SINGLE, address);
		
		for (write_count = 0; write_count < block_dword_size; write_count++)
		{
			adi_ap_write_reg(ADI_AP_REG_DRW, 
							 (uint32_t*)(buffer + 4 * write_count), 0);
		}
		
		if (ERROR_OK != adi_dp_transaction_endcheck())
		{
			LOG_WARNING(_GETTEXT("Block write error at 0x%08X, %d dwords\n"), 
						address, block_dword_size);
			return ERROR_FAIL;
		}
		
		len -= (block_dword_size << 2);
		address += (block_dword_size << 2);
		buffer += (block_dword_size << 2);
	}
	
	return ERROR_OK;
}

RESULT adi_memap_read_buf(uint32_t address, uint8_t *buffer, uint32_t len)
{
	uint32_t block_dword_size, read_count, dummy;
	
	if ((address & 0x03) || (len & 0x03) || (NULL == buffer) || (0 == len))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	
	while (len > 0)
	{
		block_dword_size = \
			adi_memap_get_max_tar_block_size(adi_dp_if->tar_autoincr_block, 
											 address);
		if (block_dword_size > (len >> 2))
		{
			block_dword_size = len >> 2;
		}
		
		adi_dp_setup_accessport(
			ADI_AP_REG_CSW_32BIT | ADI_AP_REG_CSW_ADDRINC_SINGLE, address);
		
		// first read
		adi_dp_scan(ADI_DP_IR_APACC, ADI_AP_REG_DRW, ADI_DAP_READ, &dummy);
		for (read_count = 0; read_count < block_dword_size - 1; read_count++)
		{
			adi_dp_scan(ADI_DP_IR_APACC, ADI_AP_REG_DRW, ADI_DAP_READ, (uint32_t*)(buffer + 4 * read_count));
		}
		// last read
		adi_dp_scan(ADI_DP_IR_DPACC, ADI_DP_REG_RDBUFF, ADI_DAP_READ, (uint32_t*)(buffer + 4 * read_count));
		
		if (ERROR_OK != adi_dp_transaction_endcheck())
		{
			LOG_WARNING(_GETTEXT("Block read error at 0x%08X, %d dwords\n"), 
						address, block_dword_size);
			return ERROR_FAIL;
		}
		
		len -= (block_dword_size << 2);
		address += (block_dword_size << 2);
		buffer += (block_dword_size << 2);
	}
	
	return ERROR_OK;
}

RESULT adi_fini(void)
{
	return adi_dpif_fini();
}

RESULT adi_init(programmer_info_t *prog, adi_dp_if_t *interf)
{
	uint32_t tmp;
	uint8_t cnt;
	
	// initialize interface
	if (ERROR_OK != adi_dpif_init(prog, interf))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "initialize adi debugport interface");
		return ERROR_FAIL;
	}
	adi_dp_info.type = adi_dp_if->type;
	
	// read debugport interface id
	if (ERROR_OK != adi_dpif_read_id(&adi_dp_info.if_id))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read cm3 id");
		return ERROR_FAIL;
	}
	
	// initialize debugport
	adi_dp.ap_sel_value = 0x00000000;
	adi_dp.dp_sel_value = 0xFFFFFFFF;
	adi_dp.ap_csw_value = 0xFFFFFFFF;
	adi_dp.cur_ir = 0xFF;
	
	tmp = 0;
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	
	if (adi_dp_if->type == ADI_DP_JTAG)
	{
		tmp = ADI_DP_REG_CTRL_STAT_SSTICKYERR;
		adi_dp_write_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	}
	else
	{
		tmp = ADI_SWJDP_REG_ABORT_STKERRCLR 
				| ADI_SWJDP_REG_ABORT_WDERRCLR 
				| ADI_SWJDP_REG_ABORT_ORUNERRCLR 
				| ADI_SWJDP_REG_ABORT_DAPABORT;
		adi_dp_write_reg(ADI_SWJDP_REG_ABORT, &tmp, 0);
	}
	
	tmp = 0;
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	
	tmp = ADI_DP_REG_CTRL_STAT_CDBGPWRUPREQ 
			| ADI_DP_REG_CTRL_STAT_CSYSPWRUPREQ;
	adi_dp_write_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	tmp = 0;
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	if (ERROR_OK != adi_dp_commit())
	{
		return ERROR_FAIL;
	}
	
	cnt = 0;
	while (!(tmp & ADI_DP_REG_CTRL_STAT_CDBGPWRUPACK) && (cnt++ < 10))
	{
		LOG_DEBUG(_GETTEXT("wait CDBGPWRUPACK"));
		if (ERROR_OK != adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 1))
		{
			return ERROR_FAIL;
		}
		sleep_ms(10);
	}
	while (!(tmp & ADI_DP_REG_CTRL_STAT_CSYSPWRUPACK) && (cnt++ < 10))
	{
		LOG_DEBUG(_GETTEXT("wait CSYSPWRUPACK"));
		if (ERROR_OK != adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 1))
		{
			return ERROR_FAIL;
		}
		sleep_ms(10);
	}
	
	// activate OVERRUN checking in JTAG mode
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	adi_dp.dp_ctrl_stat = ADI_DP_REG_CTRL_STAT_CDBGPWRUPREQ 
							| ADI_DP_REG_CTRL_STAT_CSYSPWRUPREQ 
							| ADI_DP_REG_CTRL_STAT_CORUNDETECT;
	adi_dp_write_reg(ADI_DP_REG_CTRL_STAT, &adi_dp.dp_ctrl_stat, 0);
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	
	// read AHB-AP ID and Debug ROM address
	if (ERROR_OK != adi_ap_read_reg(ADI_AP_REG_IDR, &adi_dp_info.ahb_ap_id, 1))
	{
		return ERROR_FAIL;
	}
	LOG_INFO(_GETTEXT("AHB-AP_ID: 0x%08X\n"), adi_dp_info.ahb_ap_id);
	
	if (ERROR_OK != 
			adi_ap_read_reg(ADI_AP_REG_DBGROMA, &adi_dp_info.rom_address, 1))
	{
		return ERROR_FAIL;
	}
	LOG_INFO(_GETTEXT("ROM_ADDRESS: 0x%08X\n"), adi_dp_info.rom_address);
	
	if (ERROR_OK != 
			adi_ap_read_reg(ADI_AP_REG_CFG, &adi_dp_info.config, 1))
	{
		return ERROR_FAIL;
	}
	if (adi_dp_info.config & 1)
	{
		LOG_INFO(_GETTEXT("CFG: 0x%08X, Big-endian\n"), adi_dp_info.config);
	}
	else
	{
		LOG_INFO(_GETTEXT("CFG: 0x%08X, Small-endian\n"), adi_dp_info.config);
	}
	
	return ERROR_OK;
}


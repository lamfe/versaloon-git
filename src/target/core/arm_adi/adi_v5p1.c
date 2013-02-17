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

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"

#include "adi_v5p1.h"
#include "adi_v5p1_internal.h"

static struct INTERFACES_INFO_T *adi_prog = NULL;
static struct adi_dpif_t *adi_dp_if;
static struct adi_dp_t adi_dp;
static uint8_t ack_value;
struct adi_dp_info_t adi_dp_info;

// Reset
#define reset_init()			adi_prog->gpio.init(0)
#define reset_fini()			adi_prog->gpio.fini(0)
#define reset_output()			\
	adi_prog->gpio.config(0, JTAG_SRST, JTAG_SRST, 0, 0)
#define reset_input()			\
	adi_prog->gpio.config(0, JTAG_SRST, 0, JTAG_SRST, JTAG_SRST)
#define reset_set()				reset_input()
#define reset_clr()				reset_output()
#define trst_output(value)		\
	adi_prog->gpio.config(0, JTAG_TRST, JTAG_TRST, 0, (value) ? JTAG_TRST : 0)
#define trst_input()			\
	adi_prog->gpio.config(0, JTAG_TRST, 0, JTAG_TRST, JTAG_TRST)
#define trst_set()				trst_output(1)
#define trst_clr()				trst_output(0)
#define reset_commit()			adi_prog->peripheral_commit()

// Delay
#define delay_ms(ms)			adi_prog->delay.delayms((ms) | 0x8000)

// JTAG
#define jtag_init()				adi_prog->jtag_hl.init(0)
#define jtag_fini()				adi_prog->jtag_hl.fini(0)
#define jtag_config(kHz,pos)	adi_prog->jtag_hl.config(0, (kHz), (pos))
#define jtag_tms(m, len)		adi_prog->jtag_hl.tms(0, (m), (len))
#define jtag_runtest(len)		adi_prog->jtag_hl.runtest(0, len)
#define jtag_ir_w(i, len)		\
	adi_prog->jtag_hl.ir(0, (uint8_t*)(i), (len), 1, 0)
#define jtag_dr_w(d, len)		\
	adi_prog->jtag_hl.dr(0, (uint8_t*)(d), (len), 1, 0)
#define jtag_dr_rw(d, len)		\
	adi_prog->jtag_hl.dr(0, (uint8_t*)(d), (len), 1, 1)

#define jtag_register_callback(s,r)	\
	adi_prog->jtag_hl.register_callback(0, (s), (r))
#define jtag_commit()			adi_prog->peripheral_commit()

// SWD
#define swd_init()				adi_prog->swd.init(0)
#define swd_fini()				adi_prog->swd.fini(0)
#define swd_seqout(b, l)		adi_prog->swd.seqout(0, (b), (l))
#define swd_seqin(b, l)			adi_prog->swd.seqin(0, (b), (l))
#define swd_transact(r, v, a)	adi_prog->swd.transact(0, (r), (v), (a))
#define swd_config(t, r, d)		adi_prog->swd.config(0, (t), (r), (d))
#define swd_get_last_ack(ack)	adi_prog->swd.get_last_ack(0, ack)
#define swd_commit()			adi_prog->peripheral_commit()

static vsf_err_t adi_dp_read_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result);
static vsf_err_t adi_dp_write_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result);
static vsf_err_t adi_dp_transaction_endcheck(void);

static const uint8_t adi_swd_reset_seq[] =
{
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

static const uint8_t adi_jtag_to_swd_seq[] =
{
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
	// 16-bit JTAG-to-SWD sequence
	0x9E, 0xE7,
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F
};

static const uint8_t adi_swd_to_jtag_seq[] =
{
	// at least 50-bit '1'
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
	// 16-bit JTAG-to-SWD sequence
	0x3C, 0xE7,
	// at least 5-bit '1'
	0xFF
};

static vsf_err_t adi_dpif_receive_callback(uint8_t index, enum jtag_irdr_t cmd,
						uint32_t ir, uint8_t *dest_buffer, uint8_t *src_buffer,
						uint16_t bytelen, uint16_t *processed)
{
	REFERENCE_PARAMETER(index);
	
	if (NULL == src_buffer)
	{
		return VSFERR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return VSFERR_NONE;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == bytelen)
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
		return VSFERR_NONE;
		break;
	}
	
	return VSFERR_FAIL;
}

static uint8_t adi_dp_first3bits;
static vsf_err_t adi_dpif_send_callback(uint8_t index, enum jtag_irdr_t cmd,
					uint32_t ir, uint8_t *dest_buffer, uint8_t *src_buffer,
					uint16_t bytelen, uint16_t *processed_len)
{
	REFERENCE_PARAMETER(index);
	
	if ((NULL == src_buffer) || (NULL == dest_buffer))
	{
		return VSFERR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return VSFERR_NONE;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == bytelen)
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
		return VSFERR_NONE;
		break;
	}
	
	return VSFERR_FAIL;
}

vsf_err_t adi_dp_commit(void)
{
	switch (adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		return jtag_commit();
		break;
	case ADI_DP_SWD:
		return swd_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

static vsf_err_t adi_dpif_fini(void)
{
	enum adi_dpif_type_t dp_type;
	
	if (NULL == adi_dp_if)
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "adi dp", "");
		return VSFERR_FAIL;
	}
	
	trst_input();
	reset_input();
	reset_fini();
	reset_commit();
	
	dp_type = adi_dp_if->type;
	switch(dp_type)
	{
	case ADI_DP_JTAG:
		adi_dp_commit();
		jtag_register_callback(NULL, NULL);
		jtag_fini();
		adi_dp_commit();
		adi_dp_if = NULL;
		break;
	case ADI_DP_SWD:
		adi_dp_commit();
		swd_fini();
		adi_dp_commit();
		adi_dp_if = NULL;
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	return VSFERR_NONE;
}

static vsf_err_t adi_dpif_init(struct INTERFACES_INFO_T *ifs, struct adi_dpif_t *interf)
{
	if ((NULL == ifs) || (NULL == interf)
		|| ((interf->type != ADI_DP_JTAG) && (interf->type != ADI_DP_SWD)))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_FAIL;
	}
	
	adi_prog = ifs;
	
	reset_init();
	reset_input();
	delay_ms(100);
	reset_commit();
	
	switch(adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		trst_output(1);
		ack_value = ADI_JTAGDP_ACK_OK_FAIL;
		jtag_init();
		jtag_config(
			adi_dp_if->dpif_setting.dpif_jtag_setting.jtag_khz,
			&adi_dp_if->dpif_setting.dpif_jtag_setting.jtag_pos);
		jtag_tms((uint8_t*)adi_swd_to_jtag_seq,
					sizeof(adi_swd_to_jtag_seq) * 8);
		if (adi_dp_commit())
		{
			return VSFERR_FAIL;
		}
		else
		{
			jtag_register_callback(adi_dpif_send_callback,
									adi_dpif_receive_callback);
			return VSFERR_NONE;
		}
		break;
	case ADI_DP_SWD:
		trst_input();
		ack_value = ADI_SWDDP_ACK_OK;
		swd_init();
		swd_config(
			adi_dp_if->dpif_setting.dpif_swd_setting.swd_trn,
			adi_dp_if->dpif_setting.dpif_swd_setting.swd_retry,
			adi_dp_if->dpif_setting.dpif_swd_setting.swd_dly);
		swd_seqout((uint8_t*)adi_jtag_to_swd_seq,
				   sizeof(adi_jtag_to_swd_seq) * 8);
		return adi_dp_commit();
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
}

static vsf_err_t adi_dp_scan(uint8_t instr, uint8_t reg_addr, uint8_t RnW,
							uint32_t *value)
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
			LOG_BUG(ERRMSG_INVALID_VALUE, instr, "adi_dp instruction");
			return VSFERR_FAIL;
			break;
		}
		// scan ir if necessary
		if (adi_dp.cur_ir != instr)
		{
			adi_dp.cur_ir = instr;
			jtag_ir_w(&instr, ADI_JTAGDP_IRLEN);
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
			uint32_t value_temp = SYS_TO_LE_U32(*value);
			jtag_dr_w(&value_temp, ADI_JTAGDP_IR_APDPACC_LEN);
		}
		
		// memory access tck clocks
		if ((ADI_JTAGDP_IR_APACC == instr)
			&& ((reg_addr == ADI_AP_REG_DRW)
				|| ((reg_addr & 0xF0) == ADI_AP_REG_BD0))
			&& (adi_dp_info.memaccess_tck != 0))
		{
			jtag_runtest(adi_dp_info.memaccess_tck);
		}
		break;
	case ADI_DP_SWD:
		if (instr > 1)
		{
			LOG_BUG(ERRMSG_INVALID_VALUE, instr, "adi_dp instruction");
			return VSFERR_FAIL;
		}
		// switch reg_addr
		reg_addr = (reg_addr << 1) & 0x18;
		swd_transact(reg_addr | ((RnW & 1) << 2) | ((instr & 1) << 1), value,
						&adi_dp.ack);
		break;
	}
	
	return VSFERR_NONE;
}

static vsf_err_t adi_dpif_read_id(uint32_t *id)
{
	uint8_t ir;
	
	if (NULL == adi_dp_if)
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "adi dp", "");
		return VSFERR_FAIL;
	}
	
	switch(adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		ir = ADI_JTAGDP_IR_IDCODE;
		jtag_ir_w(&ir, ADI_JTAGDP_IRLEN);
		jtag_dr_rw(id, ADI_JTAGDP_IR_IDCODE_LEN);
		
		if (adi_dp_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read JTAG_ID");
			return ERRCODE_FAILURE_OPERATION;
		}
		*id = LE_TO_SYS_U32(*id);
		LOG_INFO(INFOMSG_REG_08X, "JTAG_ID", *id);
		break;
	case ADI_DP_SWD:
		adi_dp_read_reg(ADI_SWDDP_REG_DPIDR, id, 0);
		
		if (adi_dp_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read SWD_ID");
			return ERRCODE_FAILURE_OPERATION;
		}
		*id = LE_TO_SYS_U32(*id);
		LOG_INFO(INFOMSG_REG_08X, "SWDID", *id);
		break;
	default:
		return VSFERR_FAIL;
		break;
	}

	return VSFERR_NONE;
}

// codes below are interface independent
static vsf_err_t adi_dp_rw(uint8_t instr, uint8_t reg_addr, uint8_t RnW,
						uint32_t *value, uint8_t check_result)
{
	adi_dp_scan(instr, reg_addr, RnW, value);
	
	// read result, DP registers of SWD are not posted
	if ((ADI_DAP_READ == RnW)
		&& ((adi_dp_if->type == ADI_DP_JTAG) || (instr == ADI_DP_IR_APACC)))
	{
		adi_dp_scan(ADI_DP_IR_DPACC, ADI_DP_REG_RDBUFF, ADI_DAP_READ, value);
	}
	
	if (check_result)
	{
		return adi_dp_transaction_endcheck();
	}
	
	return VSFERR_NONE;
}

static vsf_err_t adi_dp_read_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result)
{
	if (NULL == adi_dp_if)
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "adi dp", "");
		return VSFERR_FAIL;
	}
	
	return adi_dp_rw(ADI_DP_IR_DPACC, reg_addr, ADI_DAP_READ,
					 value, check_result);
}

static vsf_err_t adi_dp_write_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result)
{
	if (NULL == adi_dp_if)
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "adi dp", "");
		return VSFERR_FAIL;
	}
	
	return adi_dp_rw(ADI_DP_IR_DPACC, reg_addr, ADI_DAP_WRITE,
					 value, check_result);
}

static vsf_err_t adi_dp_transaction_endcheck(void)
{
	uint32_t ctrl_stat;
	uint32_t cnt = 20;
	
	do
	{
		ctrl_stat = 0;
		adi_dp_rw(ADI_DP_IR_DPACC, ADI_DP_REG_CTRL_STAT,
					  ADI_DAP_READ, &ctrl_stat, 0);
		if (adi_dp_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "access dap");
			return ERRCODE_FAILURE_OPERATION;
		}
		ctrl_stat = LE_TO_SYS_U32(ctrl_stat);
	} while ((adi_dp.ack != ack_value) && (--cnt));
	
	if (!cnt)
	{
		LOG_ERROR(ERRMSG_TIMEOUT, "access dap");
		return VSFERR_FAIL;
	}
	
	// check ctrl_stat
	if (ctrl_stat & (ADI_DP_REG_CTRL_STAT_SSTICKYORUN
						| ADI_DP_REG_CTRL_STAT_SSTICKYERR
						| ADI_DP_REG_CTRL_STAT_WDATAERR))
	{
		LOG_ERROR("Stiky Error/Overrun.");
		return VSFERR_FAIL;
	}

	return VSFERR_NONE;
}

static void adi_ap_select(uint8_t apsel)
{
	uint32_t select = (apsel << 24) & 0xFF000000;
	
	if (select != adi_dp.ap_sel_value)
	{
		adi_dp.ap_sel_value = select;
		adi_dp.ap_bank_value = 0xFFFFFFFF;
		adi_dp.ap_csw_value = 0xFFFFFFFF;
		adi_dp.ap_tar_value = 0xFFFFFFFF;
	}
}

static vsf_err_t adi_ap_bankselect(uint8_t ap_reg)
{
	uint32_t select = ap_reg & 0x000000F0;
	
	if (select != adi_dp.ap_bank_value)
	{
		adi_dp.ap_bank_value = select;
		select |= adi_dp.ap_sel_value;
		adi_dp_write_reg(ADI_DP_REG_SELECT, &select, 0);
	}
	
	return VSFERR_NONE;
}

static vsf_err_t adi_ap_read_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result)
{
	adi_ap_bankselect(reg_addr);
	return adi_dp_rw(ADI_DP_IR_APACC, reg_addr, ADI_DAP_READ, value,
					 check_result);
}

static vsf_err_t adi_ap_write_reg(uint8_t reg_addr, uint32_t *value,
								uint8_t check_result)
{
	adi_ap_bankselect(reg_addr);
	return adi_dp_rw(ADI_DP_IR_APACC, reg_addr, ADI_DAP_WRITE, value,
					 check_result);
}

static vsf_err_t adi_dp_setup_accessport(uint32_t csw, uint32_t tar)
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
	return VSFERR_NONE;
}

vsf_err_t adi_memap_write_reg32(uint32_t address, uint32_t *reg,
								uint8_t check_result)
{
	adi_dp_setup_accessport(ADI_AP_REG_CSW_32BIT | ADI_AP_REG_CSW_ADDRINC_OFF,
							address & 0xFFFFFFF0);
	
	return adi_ap_write_reg(ADI_AP_REG_BD0 | (address & 0x0000000C), reg,
							check_result);
}

vsf_err_t adi_memap_read_reg32(uint32_t address, uint32_t *reg, uint8_t check_result)
{
	adi_dp_setup_accessport(ADI_AP_REG_CSW_32BIT | ADI_AP_REG_CSW_ADDRINC_OFF,
							address & 0xFFFFFFF0);
	
	return adi_ap_read_reg(ADI_AP_REG_BD0 | (address & 0x0000000C), reg,
						   check_result);
}

vsf_err_t adi_memap_write_reg16(uint32_t address, uint16_t *reg,
								uint8_t check_result)
{
	uint32_t reg32;
	
	adi_dp_setup_accessport(ADI_AP_REG_CSW_16BIT | ADI_AP_REG_CSW_ADDRINC_OFF,
							address);
	
	reg32 = *reg << (8 * (address & 3));
	return adi_ap_write_reg(ADI_AP_REG_DRW, &reg32, check_result);
}

vsf_err_t adi_memap_read_reg16(uint32_t address, uint16_t *reg,
							uint8_t check_result)
{
	uint32_t reg32;
	
	REFERENCE_PARAMETER(check_result);
	
	adi_dp_setup_accessport(ADI_AP_REG_CSW_16BIT | ADI_AP_REG_CSW_ADDRINC_OFF,
							address);
	
	if (adi_ap_read_reg(ADI_AP_REG_DRW, &reg32, 1))
	{
		return VSFERR_FAIL;
	}
	*reg = (uint16_t)(reg32 >> (8 * (address & 3)));
	return VSFERR_NONE;
}

vsf_err_t adi_memap_write_reg8(uint32_t address, uint8_t *reg,
								uint8_t check_result)
{
	uint32_t reg32;
	
	adi_dp_setup_accessport(ADI_AP_REG_CSW_8BIT | ADI_AP_REG_CSW_ADDRINC_OFF,
							address);
	
	reg32 = *reg << (8 * (address & 3));
	return adi_ap_write_reg(ADI_AP_REG_DRW, &reg32, check_result);
}

vsf_err_t adi_memap_read_reg8(uint32_t address, uint8_t *reg,
							uint8_t check_result)
{
	uint32_t reg32;
	
	REFERENCE_PARAMETER(check_result);
	
	adi_dp_setup_accessport(ADI_AP_REG_CSW_8BIT | ADI_AP_REG_CSW_ADDRINC_OFF,
							address);
	
	if (adi_ap_read_reg(ADI_AP_REG_DRW, &reg32, 1))
	{
		return VSFERR_FAIL;
	}
	*reg = (uint8_t)(reg32 >> (8 * (address & 3)));
	return VSFERR_NONE;
}

uint32_t adi_memap_get_max_tar_block_size(uint32_t address)
{
	return (adi_dp_info.tar_autoincr_block
			- ((adi_dp_info.tar_autoincr_block - 1) & address)) >> 2;
}

// only support 32-bit aligned operation
vsf_err_t adi_memap_write_buf(uint32_t address, uint8_t *buffer, uint32_t len)
{
	uint32_t block_dword_size, write_count;
	
	if ((address & 0x03) || (len & 0x03) || (NULL == buffer) || (0 == len))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
	
	while (len > 0)
	{
		block_dword_size = adi_memap_get_max_tar_block_size(address);
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
		
		if (adi_dp_transaction_endcheck())
		{
			LOG_WARNING("Block write error at 0x%08X, %d dwords", address,
							block_dword_size);
			return VSFERR_FAIL;
		}
		
		len -= (block_dword_size << 2);
		address += (block_dword_size << 2);
		buffer += (block_dword_size << 2);
	}
	
	return VSFERR_NONE;
}

vsf_err_t adi_memap_read_buf(uint32_t address, uint8_t *buffer, uint32_t len)
{
	uint32_t block_dword_size, read_count, dummy;
	
	if ((address & 0x03) || (len & 0x03) || (NULL == buffer) || (0 == len))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
	
	while (len > 0)
	{
		block_dword_size = adi_memap_get_max_tar_block_size(address);
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
			adi_dp_scan(ADI_DP_IR_APACC, ADI_AP_REG_DRW, ADI_DAP_READ,
							(uint32_t*)(buffer + 4 * read_count));
		}
		// last read
		adi_dp_scan(ADI_DP_IR_DPACC, ADI_DP_REG_RDBUFF, ADI_DAP_READ,
							(uint32_t*)(buffer + 4 * read_count));
		
		if (adi_dp_transaction_endcheck())
		{
			LOG_WARNING("Block read error at 0x%08X, %d dwords", address,
							block_dword_size);
			return VSFERR_FAIL;
		}
		
		len -= (block_dword_size << 2);
		address += (block_dword_size << 2);
		buffer += (block_dword_size << 2);
	}
	
	return VSFERR_NONE;
}

vsf_err_t adi_fini(void)
{
	return adi_dpif_fini();
}

vsf_err_t adi_init(struct INTERFACES_INFO_T *ifs, struct adi_dpif_t *interf,
					enum adi_dp_target_core_t *core)
{
	uint32_t tmp;
	uint8_t cnt, retry = 3;
	
init:
	adi_dp_if = interf;
	// initialize interface
	if (adi_dpif_init(ifs, interf))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION,
					"initialize adi debugport interface");
		return VSFERR_FAIL;
	}
	adi_dp_info.type = adi_dp_if->type;
	
	// read debugport interface id
	if (adi_dpif_read_id(&adi_dp_info.if_id))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read cm3 id");
		return VSFERR_FAIL;
	}
	// 0x0BA00477 is for CortexM3/4
	// 0x0BB10477 is for CortexM0
	// 0x0BC11477 is for CortexM0+
	if (0x0BA00477 == (adi_dp_info.if_id & 0x0FFFEFFF))
	{
		adi_dp_info.memaccess_tck = 8;
		adi_dp_info.tar_autoincr_block = (1 << 12);
		adi_dp_info.core = ADI_DP_CM3;
	}
	else if ((0x0BB10477 == (adi_dp_info.if_id & 0x0FFFEFFF))  ||
			(0x0BC11477 == (adi_dp_info.if_id & 0x0FFFEFFF)))
	{
		adi_dp_info.memaccess_tck = 8;
		adi_dp_info.tar_autoincr_block = (1 << 10);
		adi_dp_info.core = ADI_DP_CM0;
	}
	else
	{
		if (retry-- > 0)
		{
			LOG_WARNING(ERRMSG_INVALID_HEX_MESSAGE, adi_dp_info.if_id, "id",
							"retry...");
			adi_dpif_fini();
			sleep_ms(100);
			goto init;
		}
		else
		{
			LOG_ERROR(ERRMSG_INVALID_HEX, adi_dp_info.if_id, "id");
			return ERRCODE_INVALID;
		}
	}
	if (core != NULL)
	{
		*core = adi_dp_info.core;
	}
	
	// initialize debugport
	adi_dp.ap_sel_value = !0;
	adi_ap_select(0);
	adi_dp.cur_ir = 0xFF;
	
	tmp = 0;
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	
	switch (adi_dp_if->type)
	{
	case ADI_DP_JTAG:
		tmp = ADI_DP_REG_CTRL_STAT_SSTICKYERR;
		adi_dp_write_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
		break;
	case ADI_DP_SWD:
		tmp = ADI_SWDDP_REG_ABORT_STKERRCLR
				| ADI_SWDDP_REG_ABORT_WDERRCLR
				| ADI_SWDDP_REG_ABORT_ORUNERRCLR
				| ADI_SWDDP_REG_ABORT_DAPABORT;
		adi_dp_write_reg(ADI_SWDDP_REG_ABORT, &tmp, 0);
		break;
	default:
		return VSFERR_FAIL;
		break;
	}
	
	tmp = 0;
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	
	tmp = ADI_DP_REG_CTRL_STAT_CDBGPWRUPREQ
			| ADI_DP_REG_CTRL_STAT_CSYSPWRUPREQ;
	adi_dp_write_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	tmp = 0;
	adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 0);
	if (adi_dp_commit())
	{
		return VSFERR_FAIL;
	}
	tmp = LE_TO_SYS_U32(tmp);
	
	cnt = 0;
	while (!(tmp & ADI_DP_REG_CTRL_STAT_CDBGPWRUPACK) && (cnt++ < 10))
	{
		LOG_DEBUG("wait CDBGPWRUPACK");
		if (adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 1))
		{
			return VSFERR_FAIL;
		}
		sleep_ms(10);
	}
	while (!(tmp & ADI_DP_REG_CTRL_STAT_CSYSPWRUPACK) && (cnt++ < 10))
	{
		LOG_DEBUG("wait CSYSPWRUPACK");
		if (adi_dp_read_reg(ADI_DP_REG_CTRL_STAT, &tmp, 1))
		{
			return VSFERR_FAIL;
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
	if (adi_ap_read_reg(ADI_AP_REG_IDR, &adi_dp_info.ahb_ap_id, 1))
	{
		return VSFERR_FAIL;
	}
	adi_dp_info.ahb_ap_id = LE_TO_SYS_U32(adi_dp_info.ahb_ap_id);
	LOG_INFO(INFOMSG_REG_08X, "AHB-AP_ID", adi_dp_info.ahb_ap_id);
	
	if (adi_ap_read_reg(ADI_AP_REG_DBGROMA, &adi_dp_info.rom_address, 1))
	{
		return VSFERR_FAIL;
	}
	adi_dp_info.rom_address = LE_TO_SYS_U32(adi_dp_info.rom_address);
	LOG_INFO(INFOMSG_REG_08X, "ROM_ADDRESS", adi_dp_info.rom_address);
	
	if (adi_ap_read_reg(ADI_AP_REG_CFG, &adi_dp_info.config, 1))
	{
		return VSFERR_FAIL;
	}
	adi_dp_info.config = LE_TO_SYS_U32(adi_dp_info.config);
	if (adi_dp_info.config & 1)
	{
		LOG_INFO(INFOMSG_REG_08X_STR, "CFG", adi_dp_info.config, "Big-endian");
	}
	else
	{
		LOG_INFO(INFOMSG_REG_08X_STR, "CFG", adi_dp_info.config,
					"Little-endian");
	}
	
	return VSFERR_NONE;
}


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

#ifndef __ADI_V5P1_H_INCLUDED__
#define __ADI_V5P1_H_INCLUDED__

enum adi_dp_if_type_t
{
	ADI_DP_JTAG = 1, 
	ADI_DP_SWJ = 2
};

typedef struct
{
	enum adi_dp_if_type_t type;
	union
	{
		struct
		{
			uint8_t ub;
			uint8_t ua;
			uint16_t bb;
			uint16_t ba;
			uint16_t jtag_khz;
		}adi_dp_jtag;
		struct
		{
			uint8_t swj_trn;
			uint16_t swj_retry;
			uint16_t swj_dly;
		}adi_dp_swj;
	}adi_dp_if_info;
	uint8_t memaccess_tck;
	uint32_t tar_autoincr_block;
}adi_dp_if_t;

struct adi_dp_t
{
	uint8_t cur_ir;
	uint8_t ack;
	uint32_t dp_ctrl_stat;
	uint32_t ap_sel_value;
	uint32_t dp_sel_value;
	uint32_t ap_csw_value;
	uint32_t ap_tar_value;
};

struct adi_dp_info_t
{
	enum adi_dp_if_type_t type;
	uint32_t if_id;
	uint32_t config;			// MEM-AP register: CFG
	uint32_t rom_address;		// MEM-AP register: BASE
	uint32_t ahb_ap_id;			// MEM-AP register: IDR
	struct adi_dp_t dp_state;
};

#define ADI_DP_IR_DPACC						0
#define ADI_DP_IR_APACC						1

#define ADI_SWJDP_REG_DPIDR					0x00
#define ADI_SWJDP_REG_ABORT					0x00
#define ADI_SWJDP_REG_DLCR					0x04
#define ADI_SWJDP_REG_RESEND				0x08

#define ADI_SWJDP_REG_ABORT_DAPABORT		(1<<0)
#define ADI_SWJDP_REG_ABORT_STKCMPCLR		(1<<1)
#define ADI_SWJDP_REG_ABORT_STKERRCLR		(1<<2)
#define ADI_SWJDP_REG_ABORT_WDERRCLR		(1<<3)
#define ADI_SWJDP_REG_ABORT_ORUNERRCLR		(1<<4)

#define ADI_JTAGDP_IRLEN					4
#define ADI_JTAGDP_IR_ABORT					0x04
#define ADI_JTAGDP_IR_ABORT_LEN				32
#define ADI_JTAGDP_IR_DPACC					0x0A
#define ADI_JTAGDP_IR_APACC					0x0B
#define ADI_JTAGDP_IR_APDPACC_LEN			35
#define ADI_JTAGDP_IR_IDCODE				0x0E
#define ADI_JTAGDP_IR_IDCODE_LEN			32
#define ADI_JTAGDP_IR_BYPASS				0x0F

#define ADI_DAP_READ						1
#define ADI_DAP_WRITE						0

#define ADI_SWJDP_ACK_OK					0x01
#define ADI_SWJDP_ACK_WAIT					0x02
#define ADI_SWJDP_ACK_FAIL					0x04

#define ADI_JTAGDP_ACK_WAIT					0x01
#define ADI_JTAGDP_ACK_OK_FAIL				0x02

#define ADI_DP_REG_CTRL_STAT				0x04
#define ADI_DP_REG_SELECT					0x08
#define ADI_DP_REG_RDBUFF					0x0C

#define	ADI_AP_REG_CSW						0x00
#define ADI_AP_REG_TAR						0x04
#define ADI_AP_REG_DRW						0x0C
#define ADI_AP_REG_BD0						0x10
#define ADI_AP_REG_BD1						0x14
#define ADI_AP_REG_BD2						0x18
#define ADI_AP_REG_BD3						0x1C
#define ADI_AP_REG_CFG						0xF4
#define ADI_AP_REG_DBGROMA					0xF8
#define ADI_AP_REG_IDR						0xFC

#define ADI_DP_REG_CTRL_STAT_CORUNDETECT	(1 << 0)
#define ADI_DP_REG_CTRL_STAT_SSTICKYORUN	(1 << 1)
#define ADI_DP_REG_CTRL_STAT_SSTICKYERR		(1 << 5)
#define ADI_DP_REG_CTRL_STAT_WDATAERR		(1 << 7)
#define ADI_DP_REG_CTRL_STAT_CDBGRSTREQ		(1 << 26)
#define ADI_DP_REG_CTRL_STAT_CDBGRSTACK		(1 << 27)
#define ADI_DP_REG_CTRL_STAT_CDBGPWRUPREQ	(1 << 28)
#define ADI_DP_REG_CTRL_STAT_CDBGPWRUPACK	(1 << 29)
#define ADI_DP_REG_CTRL_STAT_CSYSPWRUPREQ	(1 << 30)
#define ADI_DP_REG_CTRL_STAT_CSYSPWRUPACK	(1 << 31)

#define ADI_AP_REG_CSW_8BIT					0
#define ADI_AP_REG_CSW_16BIT				1
#define ADI_AP_REG_CSW_32BIT				2

#define ADI_AP_REG_CSW_ADDRINC_MASK			(3 << 4)
#define ADI_AP_REG_CSW_ADDRINC_OFF			0
#define ADI_AP_REG_CSW_ADDRINC_SINGLE		(1 << 4)
#define ADI_AP_REG_CSW_ADDRINC_PACKED		(2 << 4)
#define ADI_AP_REG_CSW_HPROT				(1 << 25)
#define ADI_AP_REG_CSW_MASTER_DEBUG			(1 << 29)
#define ADI_AP_REG_CSW_DBGSWENABLE			(1 << 31)

RESULT adi_init(struct programmer_info_t *prog, adi_dp_if_t *interf);
RESULT adi_fini(void);

uint32_t adi_memap_get_max_tar_block_size(uint32_t tar_autoincr_block, 
										uint32_t address);

RESULT adi_memap_read_reg(uint32_t address, uint32_t *reg, 
							uint8_t check_result);
RESULT adi_memap_write_reg(uint32_t address, uint32_t *reg, 
							uint8_t check_result);
RESULT adi_memap_read_buf(uint32_t address, uint8_t *buffer, uint32_t len);
RESULT adi_memap_write_buf(uint32_t address, uint8_t *buffer, uint32_t len);

extern struct adi_dp_info_t adi_dp_info;

#endif		// __ADI_V5P1_H_INCLUDED__


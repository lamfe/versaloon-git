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
#include "../dal.h"

#if DAL_S6B0724_EN

#include "s6b0724_drv_cfg.h"
#include "s6b0724_drv.h"

#define S6B0724_COM_NUM					64
#define S6B0724_SEG_NUM					8
#define S6B0724_SEG_SIZE				128

#define S6B0724_CMD_DISPLAY_ON          0xAF
#define S6B0724_CMD_DISPLAY_OFF         0xAE
#define S6B0724_CMD_INIT_DISPLAY_LINE   0x40
#define S6B0724_CMD_SET_PAGE_ADDR       0xB0
#define S6B0724_CMD_SET_COL_ADDR_MSB    0x10
#define S6B0724_CMD_SET_COL_ADDR_LSB    0x00
#define S6B0724_CMD_ADC_NORMAL          0xA0
#define S6B0724_CMD_ADC_REVERSE         0xA1
#define S6B0724_CMD_DISPLAY_NORMAL      0xA6
#define S6B0724_CMD_DISPLAY_REVERSE     0xA7
#define S6B0724_CMD_DISPLAY_DEPEND      0xA4
#define S6B0724_CMD_DISPLAY_ALL         0xA5
#define S6B0724_CMD_BIAS_1_9            0xA2   
#define S6B0724_CMD_SET_MODIFY_READ     0xE0
#define S6B0724_CMD_RESET_MODIFY_READ   0xEE
#define S6B0724_CMD_RESET               0xE2
#define S6B0724_CMD_COM_DIR_NORMAL      0xC0
#define S6B0724_CMD_COM_DIR_REVERSE     0xC8

#define S6B0724_CMD_POWER_CTL           0x28
#define S6B0724_POWER_VC_ON             0x04
#define S6B0724_POWER_VR_ON             0x02
#define S6B0724_POWER_VF_ON             0x01

#define S6B0724_CMD_RES_RATIO_3_0       0x20
#define S6B0724_CMD_RES_RATIO_3_5       0x21
#define S6B0724_CMD_RES_RATIO_4_0       0x22
#define S6B0724_CMD_RES_RATIO_4_5       0x23
#define S6B0724_CMD_RES_RATIO_5_0       0x24
#define S6B0724_CMD_RES_RATIO_5_5       0x25
#define S6B0724_CMD_RES_RATIO_6_0       0x26
#define S6B0724_CMD_RES_RATIO_6_4       0x27

#define S6B0724_CMD_REF_VOL_MODE        0x81

#define S6B0724_CMD_STATIC_IND_OFF      0xAD
#define S6B0724_CMD_STATIC_IND_ON       0xAC
#define S6B0724_CMD_IND_OFF             0x00
#define S6B0724_CMD_IND_1S_BLINK        0x01
#define S6B0724_CMD_IND_1_2S_BLINK      0x02
#define S6B0724_CMD_IND_ALWAYS_ON       0x03

#define S6B0724_STATUS_BUSY             0x80
#define S6B0724_STATUS_ADC              0x40
#define S6B0724_STATUS_DISPLAY          0x20
#define S6B0724_STATUS_RESETB           0x10

static vsf_err_t s6b0724_drv_write_cmd(struct dal_info_t *info, uint8_t cmd)
{
	struct s6b0724_drv_param_t *param =
									(struct s6b0724_drv_param_t *)info->param;
	struct s6b0724_drv_interface_t *ifs =
									(struct s6b0724_drv_interface_t *)info->ifs;
	uint32_t addr = ifs->cmd_addr + param->nor_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.write(ifs->ebi_port, ifs->lcd_index | EBI_TGTTYP_NOR,
									addr, 1, &cmd, 1);
}

static vsf_err_t s6b0724_drv_write_data(struct dal_info_t *info, uint8_t *data,
										uint32_t len)
{
	struct s6b0724_drv_param_t *param =
									(struct s6b0724_drv_param_t *)info->param;
	struct s6b0724_drv_interface_t *ifs =
									(struct s6b0724_drv_interface_t *)info->ifs;
	uint32_t addr = ifs->data_addr + param->nor_info.common_info.mux_addr_mask;
	
	return interfaces->ebi.write(ifs->ebi_port, ifs->lcd_index | EBI_TGTTYP_NOR,
									addr, 1, data, len);
}

static vsf_err_t s6b0724_drv_set_addr(struct dal_info_t *info,
										uint8_t page, uint8_t col)
{
	if ((page >= S6B0724_SEG_NUM) || (col >= S6B0724_SEG_SIZE))
	{
		return VSFERR_FAIL;
	}
	
	s6b0724_drv_write_cmd(info, S6B0724_CMD_SET_PAGE_ADDR | page);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_SET_COL_ADDR_MSB | ((col >> 4) & 0x0F));
	s6b0724_drv_write_cmd(info, S6B0724_CMD_SET_COL_ADDR_LSB | ((col >> 0) & 0x0F));
	return VSFERR_NONE;
}

static vsf_err_t s6b0724_drv_init(struct dal_info_t *info)
{
	struct s6b0724_drv_interface_t *ifs = 
								(struct s6b0724_drv_interface_t *)info->ifs;
	struct s6b0724_drv_param_t *param = 
								(struct s6b0724_drv_param_t *)info->param;
	
	interfaces->ebi.init(ifs->ebi_port);
	interfaces->ebi.config(ifs->ebi_port, ifs->lcd_index | EBI_TGTTYP_NOR,
							&param->nor_info);
	
	s6b0724_drv_write_cmd(info, S6B0724_CMD_RESET);
	interfaces->delay.delayms(2);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_BIAS_1_9);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_ADC_NORMAL);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_COM_DIR_REVERSE);
	
	s6b0724_drv_write_cmd(info, S6B0724_CMD_POWER_CTL | S6B0724_POWER_VC_ON);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_POWER_CTL | S6B0724_POWER_VC_ON | S6B0724_POWER_VR_ON);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_POWER_CTL | S6B0724_POWER_VC_ON | S6B0724_POWER_VR_ON | S6B0724_POWER_VF_ON);
	
	s6b0724_drv_write_cmd(info, S6B0724_CMD_REF_VOL_MODE);
	s6b0724_drv_write_cmd(info, 0x16);
	
	s6b0724_drv_write_cmd(info, S6B0724_CMD_RES_RATIO_5_5);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_DISPLAY_ON);
	s6b0724_drv_write_cmd(info, S6B0724_CMD_INIT_DISPLAY_LINE | 0);
	
	return interfaces->peripheral_commit();
}

static vsf_err_t s6b0724_drv_fini(struct dal_info_t *info)
{
	struct s6b0724_drv_interface_t *ifs = 
								(struct s6b0724_drv_interface_t *)info->ifs;
	
	interfaces->ebi.fini(ifs->ebi_port);
	return VSFERR_NONE;
}

static vsf_err_t s6b0724_drv_display(struct dal_info_t *info, uint8_t *buff)
{
	uint8_t i;
	
	for(i = 0; i < S6B0724_SEG_NUM; i++)
	{
		if (s6b0724_drv_set_addr(info, i, 0) ||
			s6b0724_drv_write_data(info, buff, S6B0724_SEG_SIZE))
		{
			return VSFERR_FAIL;
		}
		buff += S6B0724_SEG_SIZE;
	}
	
	return interfaces->peripheral_commit();
}

#if DAL_INTERFACE_PARSER_EN
static vsf_err_t s6b0724_drv_parse_interface(struct dal_info_t *info, 
											uint8_t *buff)
{
	struct s6b0724_drv_interface_t *ifs = 
								(struct s6b0724_drv_interface_t *)info->ifs;
	
	ifs->ebi_port = buff[0];
	ifs->lcd_index = buff[1];
	ifs->cmd_addr = GET_LE_U32(&buff[2]);
	ifs->data_addr = GET_LE_U32(&buff[6]);
	return VSFERR_NONE;
}
#endif

const struct s6b0724_drv_t s6b0724_drv = 
{
	{
		"s6b0724",
#if DAL_INTERFACE_PARSER_EN
		"ebi:%1dlcd:%1dcmd:%4xdata:%4x",
		s6b0724_drv_parse_interface,
#endif
	},
	{
		s6b0724_drv_init,
		s6b0724_drv_fini,
		s6b0724_drv_display,
		NULL
	}
};

#endif

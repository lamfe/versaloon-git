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

#include "app_type.h"
#include "interfaces.h"

#include "../versaloon_include.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

vsf_err_t usbtoebi_init(uint8_t interface_index)
{
	return usbtoxxx_init_command(USB_TO_EBI, interface_index);
}

vsf_err_t usbtoebi_fini(uint8_t interface_index)
{
	return usbtoxxx_fini_command(USB_TO_EBI, interface_index);
}

vsf_err_t usbtoebi_config(uint8_t interface_index, uint8_t target_index,
						void *param)
{
	uint8_t target_type = target_index & 0xF0;
	struct ebi_nor_info_t *nor_info = (struct ebi_nor_info_t *)param;
	struct ebi_nand_info_t *nand_info = (struct ebi_nand_info_t *)param;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return VSFERR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = target_index;
	switch (target_type)
	{
	case EBI_TGTTYP_NOR:
		versaloon_cmd_buf[1] = nor_info->common_info.data_width;
		versaloon_cmd_buf[2] = nor_info->common_info.wait_signal;
		versaloon_cmd_buf[3] = nor_info->param.addr_multiplex ? 1 : 0;
		SET_LE_U16(&versaloon_cmd_buf[4], 
								nor_info->param.timing.address_setup_cycle_r);
		SET_LE_U16(&versaloon_cmd_buf[6], 
								nor_info->param.timing.address_hold_cycle_r);
		SET_LE_U16(&versaloon_cmd_buf[8], 
								nor_info->param.timing.data_setup_cycle_r);
		SET_LE_U32(&versaloon_cmd_buf[10], 
								nor_info->param.timing.clock_hz_r);
		SET_LE_U16(&versaloon_cmd_buf[14], 
								nor_info->param.timing.address_setup_cycle_w);
		SET_LE_U16(&versaloon_cmd_buf[16], 
								nor_info->param.timing.address_hold_cycle_w);
		SET_LE_U16(&versaloon_cmd_buf[18], 
								nor_info->param.timing.data_setup_cycle_w);
		SET_LE_U32(&versaloon_cmd_buf[20], nor_info->param.timing.clock_hz_w);
		
		return usbtoxxx_conf_command(USB_TO_EBI, interface_index,
										versaloon_cmd_buf, 24);
	case EBI_TGTTYP_NAND:
		versaloon_cmd_buf[1] = nand_info->common_info.data_width;
		versaloon_cmd_buf[2] = nand_info->common_info.wait_signal;
		SET_LE_U32(&versaloon_cmd_buf[3], nand_info->param.clock_hz);
		versaloon_cmd_buf[7] = nand_info->param.ecc.ecc_enable ? 1 : 0;
		SET_LE_U16(&versaloon_cmd_buf[8], nand_info->param.ecc.ecc_page_size);
		versaloon_cmd_buf[10] = nand_info->param.timing.ale_to_re_cycle;
		versaloon_cmd_buf[11] = nand_info->param.timing.cle_to_re_cycle;
		SET_LE_U16(&versaloon_cmd_buf[12], nand_info->param.timing.setup_cycle);
		SET_LE_U16(&versaloon_cmd_buf[14], nand_info->param.timing.wait_cycle);
		versaloon_cmd_buf[16] = nand_info->param.timing.hold_cycle;
		versaloon_cmd_buf[17] = nand_info->param.timing.hiz_cycle;
		SET_LE_U16(&versaloon_cmd_buf[18], 
									nand_info->param.timing.setup_cycle_attr);
		SET_LE_U16(&versaloon_cmd_buf[20], 
									nand_info->param.timing.wait_cycle_attr);
		versaloon_cmd_buf[22] = nand_info->param.timing.hold_cycle_attr;
		versaloon_cmd_buf[23] = nand_info->param.timing.hiz_cycle_attr;
		
		return usbtoxxx_conf_command(USB_TO_EBI, interface_index,
										versaloon_cmd_buf, 24);
	default:
		return VSFERR_FAIL;
	}
}

vsf_err_t usbtoebi_read(uint8_t interface_index, uint8_t target_index,
			uint32_t address,uint8_t data_size, uint8_t *buff, uint32_t count)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return VSFERR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = target_index;
	versaloon_cmd_buf[1] = data_size;
	SET_LE_U32(&versaloon_cmd_buf[2], address);
	SET_LE_U32(&versaloon_cmd_buf[6], count);
	return usbtoxxx_in_command(USB_TO_EBI, interface_index, versaloon_cmd_buf,
				(uint16_t)(10 + count * data_size), 
				(uint16_t)(count * data_size), buff, 0, 
				(uint16_t)(count * data_size), 0);
}

vsf_err_t usbtoebi_write(uint8_t interface_index, uint8_t target_index,
			uint32_t address,uint8_t data_size, uint8_t *buff, uint32_t count)
{
	uint32_t i;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return VSFERR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = target_index;
	versaloon_cmd_buf[1] = data_size;
	SET_LE_U32(&versaloon_cmd_buf[2], address);
	SET_LE_U32(&versaloon_cmd_buf[6], count);
	switch (data_size)
	{
	case 1:
		memcpy(&versaloon_cmd_buf[10], buff, data_size * count);
		break;
	case 2:
		for (i = 0; i < count; i++)
		{
			SET_LE_U16(&versaloon_cmd_buf[10 + i * 2], 
						GET_SYS_U16(&buff[i * 2]));
		}
		break;
	case 4:
		for (i = 0; i < count; i++)
		{
			SET_LE_U32(&versaloon_cmd_buf[10 + i * 4], 
						GET_SYS_U32(&buff[i * 4]));
		}
		break;
	default:
		return VSFERR_FAIL;
	}
	return usbtoxxx_out_command(USB_TO_EBI, interface_index, versaloon_cmd_buf,
									(uint16_t)(10 + count * data_size), 0);
}

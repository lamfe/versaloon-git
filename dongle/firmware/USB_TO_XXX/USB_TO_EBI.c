/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_EBI.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SPI                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_EBI_EN

#include "USB_TO_XXX.h"
#include "app_interfaces.h"

void USB_TO_EBI_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint8_t target_index;
	uint8_t target_type;
	uint8_t data_size;
	uint32_t address, count;
	struct ebi_nor_info_t nor_info;
	struct ebi_nand_info_t nand_info;
	
	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_idx = dat[index] & USB_TO_XXX_IDXMASK;
		length = GET_LE_U16(&dat[index + 1]);
		index += 3;
		
		switch(command)
		{
		case USB_TO_XXX_INIT:
			if (ERROR_OK == interfaces->ebi.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			target_index = dat[index + 0];
			target_type = (target_index >> 4) & 0x0F;
			
			switch (target_type)
			{
			case EBI_TGTTYP_NOR:
				nor_info.common_info.data_width = dat[index + 1];
				nor_info.common_info.wait_signal = 
											(enum wait_signal_t)dat[index + 2];
				nor_info.param.addr_multiplex = dat[index + 3];
				nor_info.param.address_setup_cycle_r = 
												GET_LE_U16(&dat[index + 4]);
				nor_info.param.address_hold_cycle_r = 
												GET_LE_U16(&dat[index + 6]);
				nor_info.param.data_setup_cycle_r = GET_LE_U16(&dat[index + 8]);
				nor_info.param.clock_hz_r = GET_LE_U32(&dat[index + 10]);
				nor_info.param.address_setup_cycle_w = 
												GET_LE_U16(&dat[index + 14]);
				nor_info.param.address_hold_cycle_w = 
												GET_LE_U16(&dat[index + 16]);
				nor_info.param.data_setup_cycle_w = 
												GET_LE_U16(&dat[index + 18]);
				nor_info.param.clock_hz_w = GET_LE_U32(&dat[index + 20]);
				if (ERROR_OK == app_interfaces.ebi.config(device_idx, 
						target_index, &nor_info))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
				break;
			case EBI_TGTTYP_NAND:
				nand_info.common_info.data_width = dat[index + 1];
				nand_info.common_info.wait_signal = 
											(enum wait_signal_t)dat[index + 2];
				nand_info.param.clock_hz = GET_LE_U32(&dat[index + 3]);
				nand_info.param.ecc_enable = dat[index + 7];
				nand_info.param.ecc_page_size = GET_LE_U16(&dat[index + 8]);
				nand_info.param.ale_to_re_cycle = dat[index + 10];
				nand_info.param.cle_to_re_cycle = dat[index + 11];
				nand_info.param.setup_cycle = dat[index + 12];
				nand_info.param.wait_cycle = dat[index + 13];
				nand_info.param.hold_cycle = dat[index + 14];
				nand_info.param.hiz_cycle = dat[index + 15];
				nand_info.param.setup_cycle_attr = dat[index + 16];
				nand_info.param.wait_cycle_attr = dat[index + 17];
				nand_info.param.hold_cycle_attr = dat[index + 18];
				nand_info.param.hiz_cycle_attr = dat[index + 19];
				if (ERROR_OK == app_interfaces.ebi.config(device_idx, 
						target_index, &nand_info))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
				break;
			default:
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->ebi.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_IN:
			target_index = dat[index + 0];
			data_size = dat[index + 1];
			address = GET_LE_U32(&dat[index + 2]);
			count = GET_LE_U32(&dat[index + 6]);
			if (ERROR_OK != app_interfaces.ebi.read(device_idx, target_index, 
					address, data_size, &buffer_reply[rep_len + 1], count))
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
			}
			rep_len += 1 + data_size * count;
			break;
		case USB_TO_XXX_OUT:
			target_index = dat[index + 0];
			data_size = dat[index + 1];
			address = GET_LE_U32(&dat[index + 2]);
			count = GET_LE_U32(&dat[index + 6]);
			if (ERROR_OK != app_interfaces.ebi.write(device_idx, target_index, 
					address, data_size, &dat[index + 10], count))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}
		index += length;
	}
}

#endif

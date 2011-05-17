/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       BDM.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    BDM interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-05-09:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "interfaces.h"
#include "STM32_USBD.h"

#include "usb_regs.h"
#include "usb_mem.h"

#define STM32_USBD_EP_NUM					8

const uint8_t stm32_usbd_ep_num = STM32_USBD_EP_NUM;
static void *USBD_Device = NULL;
static uint16_t EP_Cfg_Ptr = 0x200;

vsfusbd_IN_hanlder_t stm32_usbd_IN_handlers[STM32_USBD_EP_NUM];
vsfusbd_OUT_hanlder_t stm32_usbd_OUT_handlers[STM32_USBD_EP_NUM];
uint16_t stm32_usbd_IN_epsize[STM32_USBD_EP_NUM];
uint16_t stm32_usbd_OUT_epsize[STM32_USBD_EP_NUM];
bool stm32_usbd_IN_dbuffer[STM32_USBD_EP_NUM];
bool stm32_usbd_OUT_dbuffer[STM32_USBD_EP_NUM];

RESULT stm32_usbd_init(void *device)
{
	memset(stm32_usbd_IN_handlers, 0, sizeof(stm32_usbd_IN_handlers));
	memset(stm32_usbd_OUT_handlers, 0, sizeof(stm32_usbd_OUT_handlers));
	memset(stm32_usbd_IN_epsize, 0, sizeof(stm32_usbd_IN_epsize));
	memset(stm32_usbd_OUT_epsize, 0, sizeof(stm32_usbd_OUT_epsize));
	memset(stm32_usbd_IN_dbuffer, 0, sizeof(stm32_usbd_IN_dbuffer));
	memset(stm32_usbd_OUT_dbuffer, 0, sizeof(stm32_usbd_OUT_dbuffer));
	USBD_Device = device;
	// reset
	SetCNTR(CNTR_FRES);
	SetCNTR(0);
	
	SetISTR(0);
	SetCNTR(CNTR_CTRM | CNTR_WKUPM | CNTR_SUSPM | CNTR_ERRM | CNTR_RESETM);
	SetBTABLE(0);
	return ERROR_OK;
}

RESULT stm32_usbd_fini(void)
{
	// reset
	SetCNTR(CNTR_FRES);
	SetISTR(0);
	
	SetCNTR(CNTR_FRES + CNTR_PDWN);
	return ERROR_OK;
}

RESULT stm32_usbd_reset(void)
{
	return ERROR_OK;
}

RESULT stm32_usbd_poll(void)
{
	return ERROR_OK;
}

RESULT stm32_usbd_connect(void)
{
	return ERROR_OK;
}

RESULT stm32_usbd_disconnect(void)
{
	return ERROR_OK;
}

RESULT stm32_usbd_set_address(uint8_t address)
{
	uint8_t i;
	
	for (i = 0; i < STM32_USBD_EP_NUM; i++)
	{
		SetEPAddress(i, i);
	}
	SetDADDR(address | DADDR_EF);
	return ERROR_OK;
}

uint8_t stm32_usbd_get_address(void)
{
	return (_GetDADDR() & 0x7F);
}

RESULT stm32_usbd_suspend(void)
{
	return ERROR_OK;
}

RESULT stm32_usbd_resume(void)
{
	return ERROR_OK;
}

RESULT stm32_usbd_lowpower(uint8_t level)
{
	return ERROR_OK;
}

uint32_t stm32_usbd_get_frame_number(void)
{
	return GetFNR() & 0x7FF;
}

RESULT stm32_usbd_ep_reset(uint8_t idx)
{
	return ERROR_OK;
}

RESULT stm32_usbd_ep_set_type(uint8_t idx, enum usb_ep_type_t type)
{
	switch (type)
	{
	case USB_EP_TYPE_CONTROL:
		SetEPType(idx, EP_CONTROL);
		Clear_Status_Out(idx);
		break;
	case USB_EP_TYPE_INTERRUPT:
		SetEPType(idx, EP_INTERRUPT);
		break;
	case USB_EP_TYPE_BULK:
		SetEPType(idx, EP_BULK);
		ClearEPDoubleBuff(idx);
		break;
	case USB_EP_TYPE_ISO:
		SetEPType(idx, EP_ISOCHRONOUS);
		break;
	default:
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

enum usb_ep_type_t stm32_usbd_ep_get_type(uint8_t idx)
{
	switch (GetEPType(idx))
	{
	default:
	case EP_CONTROL:
		return USB_EP_TYPE_CONTROL;
	case EP_INTERRUPT:
		return USB_EP_TYPE_INTERRUPT;
	case EP_BULK:
		return USB_EP_TYPE_BULK;
	case EP_ISOCHRONOUS:
		return USB_EP_TYPE_ISO;
	}
}

RESULT stm32_usbd_ep_set_IN_handler(uint8_t idx, vsfusbd_IN_hanlder_t handler)
{
	if (idx >= STM32_USBD_EP_NUM)
	{
		return ERROR_FAIL;
	}
	stm32_usbd_IN_handlers[idx] = handler;
	return ERROR_OK;
}

RESULT stm32_usbd_ep_set_IN_dbuffer(uint8_t idx)
{
	uint16_t epsize = stm32_usbd_ep_get_IN_epsize(idx);
	
	SetEPDoubleBuff(idx);
	EP_Cfg_Ptr -= epsize;
	if (EP_Cfg_Ptr < STM32_USBD_EP_NUM * 8)
	{
		return ERROR_FAIL;
	}
	SetEPDblBuffAddr(idx, GetEPTxAddr(idx), EP_Cfg_Ptr);
	SetEPDblBuffCount(idx, EP_DBUF_IN, 0);
	ClearDTOG_RX(idx);
	ClearDTOG_TX(idx);
	SetEPRxStatus(idx, EP_RX_DIS);
	SetEPTxStatus(idx, EP_TX_NAK);
	stm32_usbd_IN_dbuffer[idx] = true;
	return ERROR_OK;
}

bool stm32_usbd_ep_is_IN_dbuffer(uint8_t idx)
{
	return stm32_usbd_IN_dbuffer[idx];
}

RESULT stm32_usbd_ep_switch_IN_buffer(uint8_t idx)
{
	FreeUserBuffer(idx, EP_DBUF_IN);
	return ERROR_OK;
}

RESULT stm32_usbd_ep_set_IN_epsize(uint8_t idx, uint16_t epsize)
{
	if (epsize & 0x0007)
	{
		return ERROR_FAIL;
	}
	
	stm32_usbd_IN_epsize[idx] = epsize;
	SetEPTxCount(idx, epsize);
	EP_Cfg_Ptr -= epsize;
	if (EP_Cfg_Ptr < STM32_USBD_EP_NUM * 8)
	{
		return ERROR_FAIL;
	}
	SetEPTxAddr(idx, EP_Cfg_Ptr);
	return ERROR_OK;
}

uint16_t stm32_usbd_ep_get_IN_epsize(uint8_t idx)
{
	return stm32_usbd_IN_epsize[idx];
}

RESULT stm32_usbd_ep_set_IN_state(uint8_t idx, enum usb_ep_state_t state)
{
	switch (state)
	{
	case USB_EP_STAT_STALL:
		SetEPTxStatus(idx, EP_TX_STALL);
		break;
	case USB_EP_STAT_ACK:
		SetEPTxStatus(idx, EP_TX_VALID);
		break;
	case USB_EP_STAT_NACK:
		SetEPTxStatus(idx, EP_TX_NAK);
		break;
	case USB_EP_STAT_DIS:
		SetEPTxStatus(idx, EP_TX_DIS);
		break;
	default:
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

enum usb_ep_state_t stm32_usbd_ep_get_IN_state(uint8_t idx)
{
	switch (GetEPTxStatus(idx))
	{
	default:
	case EP_TX_DIS:
		return USB_EP_STAT_DIS;
	case EP_TX_NAK:
		return USB_EP_STAT_NACK;
	case EP_TX_VALID:
		return USB_EP_STAT_ACK;
	case EP_TX_STALL:
		return USB_EP_STAT_STALL;
	}
}

RESULT stm32_usbd_ep_set_IN_count(uint8_t idx, uint16_t size)
{
	if (stm32_usbd_IN_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_RX)
		{
			SetEPDblBuf1Count(idx, EP_DBUF_IN, size);
		}
		else
		{
			SetEPDblBuf0Count(idx, EP_DBUF_IN, size);
		}
	}
	else
	{
		SetEPTxCount(idx, size);
	}
	return ERROR_OK;
}

RESULT stm32_usbd_ep_write_IN_buffer(uint8_t idx, uint8_t *buffer, uint16_t size)
{
	uint32_t PMA_ptr;
	
	if (stm32_usbd_IN_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_RX)
		{
			PMA_ptr = GetEPDblBuf1Addr(idx);
		}
		else
		{
			PMA_ptr = GetEPDblBuf0Addr(idx);
		}
	}
	else
	{
		PMA_ptr = GetEPTxAddr(idx);
	}
	UserToPMABufferCopy(buffer, PMA_ptr, size);
	return ERROR_OK;
}

RESULT stm32_usbd_ep_set_OUT_handler(uint8_t idx, vsfusbd_OUT_hanlder_t handler)
{
	if (idx >= STM32_USBD_EP_NUM)
	{
		return ERROR_FAIL;
	}
	stm32_usbd_OUT_handlers[idx] = handler;
	return ERROR_OK;
}

RESULT stm32_usbd_ep_set_OUT_dbuffer(uint8_t idx)
{
	uint16_t epsize = stm32_usbd_ep_get_OUT_epsize(idx);
	
	SetEPDoubleBuff(idx);
	EP_Cfg_Ptr -= epsize;
	if (EP_Cfg_Ptr < STM32_USBD_EP_NUM * 8)
	{
		return ERROR_FAIL;
	}
	SetEPDblBuffAddr(idx, GetEPRxAddr(idx), EP_Cfg_Ptr);
	SetEPDblBuffCount(idx, EP_DBUF_OUT, epsize);
	ClearDTOG_RX(idx);
	ClearDTOG_TX(idx);
	ToggleDTOG_TX(idx);
	SetEPRxStatus(idx, EP_RX_VALID);
	SetEPTxStatus(idx, EP_TX_DIS);
	stm32_usbd_OUT_dbuffer[idx] = true;
	return ERROR_OK;
}

bool stm32_usbd_ep_is_OUT_dbuffer(uint8_t idx)
{
	return stm32_usbd_OUT_dbuffer[idx];
}

RESULT stm32_usbd_ep_switch_OUT_buffer(uint8_t idx)
{
	FreeUserBuffer(idx, EP_DBUF_OUT);
	return ERROR_OK;
}

RESULT stm32_usbd_ep_set_OUT_epsize(uint8_t idx, uint16_t epsize)
{
	if (epsize & 0x0007)
	{
		return ERROR_FAIL;
	}
	
	stm32_usbd_OUT_epsize[idx] = epsize;
	SetEPRxCount(idx, epsize);
	EP_Cfg_Ptr -= epsize;
	if (EP_Cfg_Ptr < STM32_USBD_EP_NUM * 8)
	{
		return ERROR_FAIL;
	}
	SetEPRxAddr(idx, EP_Cfg_Ptr);
	return ERROR_OK;
}

uint16_t stm32_usbd_ep_get_OUT_epsize(uint8_t idx)
{
	return stm32_usbd_OUT_epsize[idx];
}

RESULT stm32_usbd_ep_set_OUT_state(uint8_t idx, enum usb_ep_state_t state)
{
	switch (state)
	{
	case USB_EP_STAT_STALL:
		SetEPRxStatus(idx, EP_RX_STALL);
		break;
	case USB_EP_STAT_ACK:
		SetEPRxStatus(idx, EP_RX_VALID);
		break;
	case USB_EP_STAT_NACK:
		SetEPRxStatus(idx, EP_RX_NAK);
		break;
	case USB_EP_STAT_DIS:
		SetEPRxStatus(idx, EP_RX_DIS);
		break;
	default:
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

enum usb_ep_state_t stm32_usbd_ep_get_OUT_state(uint8_t idx)
{
	switch (GetEPRxStatus(idx))
	{
	default:
	case EP_RX_DIS:
		return USB_EP_STAT_DIS;
	case EP_RX_NAK:
		return USB_EP_STAT_NACK;
	case EP_RX_VALID:
		return USB_EP_STAT_ACK;
	case EP_RX_STALL:
		return USB_EP_STAT_STALL;
	}
}

uint16_t stm32_usbd_ep_get_OUT_count(uint8_t idx)
{
	if (stm32_usbd_OUT_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_TX)
		{
			return GetEPDblBuf1Count(idx);
		}
		else
		{
			return GetEPDblBuf0Count(idx);
		}
	}
	else
	{
		return GetEPRxCount(idx);
	}
}

RESULT stm32_usbd_ep_read_OUT_buffer(uint8_t idx, uint8_t *buffer, uint16_t size)
{
	if (stm32_usbd_OUT_dbuffer[idx])
	{
		if(GetENDPOINT(idx) & EP_DTOG_TX)
		{
			PMAToUserBufferCopy(buffer, GetEPDblBuf1Addr(idx), size);
		}
		else
		{
			PMAToUserBufferCopy(buffer, GetEPDblBuf0Addr(idx), size);
		}
	}
	else
	{
		PMAToUserBufferCopy(buffer, GetEPRxAddr(idx), size);
	}
	return ERROR_OK;
}







void CTR_LP(void)
{
	uint8_t EPindex;
	uint16_t wIstr;
	volatile uint16_t wEPVal = 0;
	
	while (((wIstr = _GetISTR()) & ISTR_CTR) != 0)
	{
		EPindex = (uint8_t)(wIstr & ISTR_EP_ID);
		if (EPindex == 0)
		{
			if ((wIstr & ISTR_DIR) == 0)
			{
				_ClearEP_CTR_TX(ENDP0);
				stm32_usbd_IN_handlers[0](USBD_Device, 0);
				return;
			}
			else
			{
				wEPVal = _GetENDPOINT(ENDP0);
				_ClearEP_CTR_RX(ENDP0);
				if ((wEPVal & EP_SETUP) != 0)
				{
					vsfusbd_on_SETUP(USBD_Device);
				}
				else if ((wEPVal & EP_CTR_RX) != 0)
				{
					stm32_usbd_OUT_handlers[0](USBD_Device, 0);
				}
				return;
			}
		}
		else
		{
			wEPVal = _GetENDPOINT(EPindex);
			if ((wEPVal & EP_CTR_RX) != 0)
			{
				_ClearEP_CTR_RX(EPindex);
				if (stm32_usbd_OUT_handlers[EPindex] != NULL)
				{
					stm32_usbd_OUT_handlers[EPindex](USBD_Device, EPindex);
				}
			}
			if ((wEPVal & EP_CTR_TX) != 0)
			{
				_ClearEP_CTR_TX(EPindex);
				if (stm32_usbd_IN_handlers[EPindex] != NULL)
				{
					stm32_usbd_IN_handlers[EPindex](USBD_Device, EPindex);
				}
			}
		}
	}
}

void CTR_HP(void)
{
	uint8_t EPindex;
	uint16_t wIstr;
	uint32_t wEPVal = 0;
	
	while (((wIstr = _GetISTR()) & ISTR_CTR) != 0)
	{
		_SetISTR((uint16_t)CLR_CTR);
		
		EPindex = (uint8_t)(wIstr & ISTR_EP_ID);
		wEPVal = _GetENDPOINT(EPindex);
		if ((wEPVal & EP_CTR_RX) != 0)
		{
			_ClearEP_CTR_RX(EPindex);
			if (stm32_usbd_OUT_handlers[EPindex] != NULL)
			{
				stm32_usbd_OUT_handlers[EPindex](USBD_Device, EPindex);
			}
		}
		else if ((wEPVal & EP_CTR_TX) != 0)
		{
			_ClearEP_CTR_TX(EPindex);
			if (stm32_usbd_IN_handlers[EPindex] != NULL)
			{
				stm32_usbd_IN_handlers[EPindex](USBD_Device, EPindex);
			}
		}
	}
}

void USB_Istr(void)
{
	uint16_t wIstr = _GetISTR();
	
	if (wIstr & ISTR_RESET)
	{
		_SetISTR((uint16_t)CLR_RESET);
		EP_Cfg_Ptr = 0x200;
		vsfusbd_on_RESET(USBD_Device);
	}
	if (wIstr & ISTR_DOVR)
	{
		_SetISTR((uint16_t)CLR_DOVR);
	}
	if (wIstr & ISTR_ERR)
	{
		_SetISTR((uint16_t)CLR_ERR);
		vsfusbd_on_ERROR(USBD_Device, USBERR_ERROR);
	}
	if (wIstr & ISTR_WKUP)
	{
		_SetISTR((uint16_t)CLR_WKUP);
		vsfusbd_on_WAKEUP(USBD_Device);
	}
	if (wIstr & ISTR_SUSP)
	{
		vsfusbd_on_SUSPEND(USBD_Device);
		_SetISTR((uint16_t)CLR_SUSP);
	}
	if (wIstr & ISTR_SOF)
	{
		_SetISTR((uint16_t)CLR_SOF);
		vsfusbd_on_SOF(USBD_Device);
	}
	if (wIstr & ISTR_ESOF)
	{
		_SetISTR((uint16_t)CLR_ESOF);
		vsfusbd_on_ERROR(USBD_Device, USBERR_SOF_TO);
	}
	if (wIstr & ISTR_CTR)
	{
		CTR_LP();
	}
}

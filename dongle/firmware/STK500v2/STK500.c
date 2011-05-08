/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500.c                                                  *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for STK500 protocol                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "STK500_Const.h"
#include "STK500.h"

#include "STK_Param.h"

uint8_t STK500_PARAM_SCK_Duration = 0;
uint8_t STK500_PARAM_Reset_Polarity = 0;
uint8_t STK500_PARAM_DischargeDelay = 0;
uint32_t SKT500_Target_Address = 0;

const struct STKPARAM STK500_Param[] = 
{
	STKPARAM_DECLEAR(PARAM_SCK_DURATION, STKPARAM_ATTR_RW, 1, &STK500_PARAM_SCK_Duration, 0),
	STKPARAM_DECLEAR(PARAM_RESET_POLARITY, STKPARAM_ATTR_W, 1, &STK500_PARAM_Reset_Polarity, 0),
	STKPARAM_DECLEAR(PARAM_DISCHARGEDELAY, STKPARAM_ATTR_W, 1, &STK500_PARAM_DischargeDelay, 0),
	STKPARAM_NULL
};

/// STK500 Process General Command
/// @param[in]	dat		Command Array
/// @param[in]	len		Command Length
/// @return		Command Result
static uint8_t STK500_ProcessGenCmd(uint8_t* dat, uint16_t len)
{
	uint8_t length;

	switch(dat[0])
	{
	case CMD_SET_PARAMETER:
		length = 0;
		if (STKPARAM_GetSize(STK500_Param, dat[1], (uint8_t*)&length))
		{
			STKPARAM_SetValue(STK500_Param, dat[1], &dat[2]);
		}
		rep_len = 2;
		break;
	case CMD_GET_PARAMETER:
		length = 0;
		if (STKPARAM_GetSize(STK500_Param, dat[1], (uint8_t*)&length))
		{
			STKPARAM_GetValue(STK500_Param, dat[1], &dat[2]);
		}
		rep_len = 3;
		break;
	case CMD_OSCCAL:
		rep_len = 2;
		break;
	case CMD_LOAD_ADDRESS:
		SKT500_Target_Address = GET_BE_U32(&dat[1]);
		rep_len = 2;
		return STATUS_CMD_OK;
	default:
		rep_len = 2;
		return STATUS_CMD_UNKNOWN;
	}

	return STATUS_CMD_OK;
}


/// STK500 Process Command, support ISP, HVPP and HVSP
/// @param[in]	dat		Command Array
/// @param[in]	len		Command Length
void STK500_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint8_t tmp = 0;

	if((dat[0] & STK500_CMDTYPE_MASK) == CMDTYPE_GENERAL)
	{
		tmp = STK500_ProcessGenCmd(dat, len);
	}
	else if((dat[0] & STK500_CMDTYPE_MASK) == CMDTYPE_ISP)
	{
		tmp = STK500_ISP_ProcessProgCmd(dat, len);
	}
	else
	{
		rep_len = 2;
		tmp = STATUS_CMD_UNKNOWN;
	}

	dat[1] = tmp;
}

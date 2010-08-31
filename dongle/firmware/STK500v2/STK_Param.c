/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK_Param.c                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation of STK Parameter Processing                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "STK_Param.h"

static bool STKPARAM_GetIdx(const struct STKPARAM *param, uint8 index, uint8 *idx)
{
	uint8 i;

	i = 0;
	while ((param[i].ptr.p_void != 0) && (param[i].size != 0))
	{
		if (index == param[i].index)
		{
			*idx = i;
			return true;
		}
		i++;
	}
	return false;
}

bool STKPARAM_GetSize(const struct STKPARAM *param, uint8 index, uint8 *size)
{
	uint8 idx;
	if (STKPARAM_GetIdx(param, index, &idx))
	{
		if ((1 == param[idx].size) || (2 == param[idx].size) || (4 == param[idx].size))
		{
			*size = param[idx].size;
			return true;
		}
		return false;
	}
	return false;
}

bool STKPARAM_GetValue(const struct STKPARAM *param, uint8 index, void *value)
{
	uint8 idx;
	if ((STKPARAM_GetIdx(param, index, &idx)) && (param[idx].attr & STKPARAM_ATTR_R))
	{
		if (param[idx].OnReadWrite != 0)
		{
			if (!param[idx].OnReadWrite(&param[idx], STKPARAM_ATTR_R))
			{
				return false;
			}
		}
		switch (param[idx].size)
		{
		case 1:
			*(uint8*)value = *param[idx].ptr.p_u8;
			break;
		case 2:
			*(uint16*)value = *param[idx].ptr.p_u16;
			break;
		case 4:
			*(uint32*)value = *param[idx].ptr.p_u32;
			break;
		default:
			return false;
		}
		return true;
	}
	return false;
}

bool STKPARAM_SetValue(const struct STKPARAM *param, uint8 index, void *value)
{
	uint8 idx;
	if ((STKPARAM_GetIdx(param, index, &idx)) && (param[idx].attr & STKPARAM_ATTR_W))
	{
		switch (param[idx].size)
		{
		case 1:
			*param[idx].ptr.p_u8 = *(uint8*)value;
			break;
		case 2:
			*param[idx].ptr.p_u16 = *(uint16*)value;
			break;
		case 4:
			*param[idx].ptr.p_u32 = *(uint32*)value;
			break;
		default:
			return false;
		}
		if (param[idx].OnReadWrite != 0)
		{
			return param[idx].OnReadWrite(&param[idx], STKPARAM_ATTR_W);
		}
		return true;
	}
	return false;
}

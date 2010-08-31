/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK_Param.h                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for STK Parameter Processing                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

#define STKPARAM_ATTR_R								(1 << 0)
#define STKPARAM_ATTR_W								(1 << 1)
#define STKPARAM_ATTR_RW							(STKPARAM_ATTR_R | STKPARAM_ATTR_W)

struct STKPARAM
{
	uint8 index;
	uint8 attr;
	uint8 size;
	union
	{
		void *p_void;
		uint8 *p_u8;
		uint16 *p_u16;
		uint32 *p_u32;
	}ptr;
	bool (*OnReadWrite)(const struct STKPARAM *param, uint8 attr);
};

#define STKPARAM_DECLEAR(idx, attr, size, p, rw)	{(idx), (attr), (size), {(p)}, (rw)}
#define STKPARAM_NULL								STKPARAM_DECLEAR(0, 0, 0, 0, 0)

bool STKPARAM_GetSize(const struct STKPARAM *param, uint8 index, uint8 *size);
bool STKPARAM_GetValue(const struct STKPARAM *param, uint8 index, void *value);
bool STKPARAM_SetValue(const struct STKPARAM *param, uint8 index, void *value);

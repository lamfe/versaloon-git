/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MicroWire.c                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MicroWire interface implementation file                   *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_MICROWIRE_EN

#include "interfaces.h"
#include "MicroWire.h"

RESULT microwire_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT microwire_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT microwire_config(uint8_t index, uint16_t kHz, uint8_t sel_polarity)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT microwire_io(uint8_t index, uint8_t *opcode, uint16_t opcode_bitlen, 
					uint8_t *input, uint16_t input_bitlen)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT microwire_poll(uint8_t index, uint16_t interval_us, uint16_t retry_cnt)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

#endif

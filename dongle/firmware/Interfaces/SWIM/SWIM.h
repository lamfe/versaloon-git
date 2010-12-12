/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWIM.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWIM interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

RESULT swim_init(uint8_t index);
RESULT swim_fini(uint8_t index);
RESULT swim_config(uint8_t index, uint8_t mHz, uint8_t cnt0, uint8_t cnt1);
RESULT swim_srst(uint8_t index);
RESULT swim_wotf(uint8_t index, uint8_t *data, uint16_t bytelen, 
			   uint32_t addr);
RESULT swim_rotf(uint8_t index, uint8_t *data, uint16_t bytelen, 
			   uint32_t addr);
RESULT swim_sync(uint8_t index, uint8_t mHz);
RESULT swim_enable(uint8_t index);

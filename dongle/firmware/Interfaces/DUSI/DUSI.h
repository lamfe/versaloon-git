/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       DUSI.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    DUSI interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define DUSI_MAX_KHZ			(_SYS_FREQUENCY * 500 / 2)		// in KHz
#define DUSI_MIN_KHZ			(_SYS_FREQUENCY * 500 / 256)	// in KHz

RESULT dusi_init(uint8_t index);
RESULT dusi_fini(uint8_t index);
RESULT dusi_io(uint8_t index, uint8_t *mo, uint8_t *mi, uint8_t *so, uint8_t *si, uint32_t len);
RESULT dusi_config(uint8_t index, uint16_t kHz, uint8_t cpol, uint8_t cpha, 
					uint8_t first_bit);

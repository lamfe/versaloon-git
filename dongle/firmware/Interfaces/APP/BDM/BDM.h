/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       BDM.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    BDM interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define BDM_OUT_LEN(token)		(((token) & 0x000F) >> 0)
#define BDM_OUT_DLY_CNT(token)	(((token) & 0x00C0) >> 6)
#define BDM_IN_LEN(token)		(((token) & 0x0F00) >> 8)
#define BDM_ACK(token)			(((token) & 0x8000) != 0)

RESULT bdm_init(uint8_t index);
RESULT bdm_fini(uint8_t index);
RESULT bdm_sync(uint8_t index, uint16_t *khz);
RESULT bdm_transact(uint8_t index, uint8_t *out, uint8_t outlen, uint8_t *in, 
			uint8_t inlen, uint8_t delay, uint8_t ack);

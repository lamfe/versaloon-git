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

#define BDM_OUT_LEN(token)		(((token) & 0x07) >> 0)
#define BDM_OUT_DLY(token)		(((token) & 0x08) != 0)
#define BDM_IN_LEN(token)		(((token) & 0x70) >> 4)
#define BDM_ACK(token)			(((token) & 0x80) != 0)

void BDM_Init(void);
void BDM_Fini(void);
uint8 BDM_Sync(uint16 *khz);
uint8 BDM_Transact(uint8 token, uint8 *out, uint8 *in);

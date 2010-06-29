/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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

void SWIM_Init();
void SWIM_Fini();
uint8 SWIM_Sync(uint8 mHz);
uint8 SWIM_SetClockParam(uint8 mHz, uint8 cnt0, uint8 cnt1);
uint8 SWIM_WOTF(uint32 addr, uint16 len, uint8 *data);
uint8 SWIM_ROTF(uint32 addr, uint16 len, uint8 *data);
uint8 SWIM_SRST(void);
uint8 SWIM_Enable(void);

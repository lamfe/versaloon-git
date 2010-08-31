/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500.h                                                  *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for STK500 protocol                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

extern uint8 STK500_PARAM_SCK_Duration;
extern uint8 STK500_PARAM_Reset_Polarity;
extern uint8 STK500_PARAM_DischargeDelay;
extern uint32 SKT500_Target_Address;
extern uint8 STK500V2_PARAM_RunAfterProgramming;

uint8 STK500_ISP_ProcessProgCmd(uint8* dat, uint16 len);

void STK500_ProcessCmd(uint8 *dat, uint16 len);

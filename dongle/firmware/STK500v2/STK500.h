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

extern uint8_t STK500_PARAM_SCK_Duration;
extern uint8_t STK500_PARAM_Reset_Polarity;
extern uint8_t STK500_PARAM_DischargeDelay;
extern uint32_t SKT500_Target_Address;
extern uint8_t STK500V2_PARAM_RunAfterProgramming;

uint8_t STK500_ISP_ProcessProgCmd(uint8_t* dat, uint16_t len);

void STK500_ProcessCmd(uint8_t *dat, uint16_t len);

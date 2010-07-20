/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MSP430_JTAG.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MSP430_JTAG interface header file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define MSP430_JTAG_IR_LEN					8

void MSP430_JTAG_Init(uint8 has_test);
void MSP430_JTAG_Fini(void);
void MSP430_JTAG_TCLK(uint8 tclk);
void MSP430_JTAG_TCLK_STROKE(uint16 cnt);
uint8 MSP430_JTAG_Poll(uint32 data, uint32 mask, uint32 value, uint8 len, uint16 poll_cnt);
uint32 MSP430_JTAG_DR(uint32 dr, uint8 len);
uint8 MSP430_JTAG_IR(uint8 ir, uint8 len);
void MSP430_JTAG_Reset(void);

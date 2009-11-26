/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       LPC_ICP.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    LPC_ICP interface header file                             *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define LPCICP_POLL_ON_SET			0
#define LPCICP_POLL_ON_CLEAR		1
#define LPCICP_POLL_TIME_OUT		2

void LPCICP_Init(void);
void LPCICP_Fini(void);
void LPCICP_LeavrProgMode(void);
void LPCICP_EnterProgMode(void);
void LPCICP_In(uint8 *buff, uint16 len);
void LPCICP_Out(uint8 *buff, uint16 len);
uint8 LPCICP_Poll(uint8 out, uint8 setbit, uint8 clearbit, uint16 pollcnt);

/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       ISSP.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    ISSP interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define ISSP_VECTOR_BITNUM		22

// read or write
#define ISSP_VECTOR_R			1
#define ISSP_VECTOR_W			0

// target
#define ISSP_VECTOR_SRAM		0
#define ISSP_VECTOR_CPUBANK		1

// Prog Mode
#define ISSP_PM_RESET			(1 << 0)
#define ISSP_PM_POWER_ON		(0 << 0)

// wap result
#define ISSP_WAP_OK				0x00
#define ISSP_WAP_TIMEOUT		0x01

uint8 ISSP_Vector(uint8 bank, uint8 addr, uint8 data, uint8 r, uint8 append_bit);
void ISSP_Vector_0s(void);
void ISSP_EnterProgMode(uint8 mode);
void ISSP_LeaveProgMode(uint8 mode);
uint8 ISSP_WaitAndPoll(void);

void ISSP_Init(void);
void ISSP_Fini(void);

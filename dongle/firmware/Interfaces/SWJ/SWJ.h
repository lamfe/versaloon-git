/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWJ.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWJ interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define SWJ_SUCCESS				0x00
#define SWJ_FAULT				0x80
#define SWJ_RETRY_OUT			0x40
#define SWJ_ACK_ERROR			0x20
#define SWJ_PARITY_ERROR		0x10

#define SWJ_ACK_OK				0x01
#define SWJ_ACK_WAIT			0x02
#define SWJ_ACK_FAULT			0x04

#define SWJ_TRANS_RnW			(1 << 2)

uint8 SWJ_SeqIn(uint8 *seq, uint16 num_of_bits);
uint8 SWJ_SeqOut(uint8 *seq, uint16 num_of_bits);
void SWJ_StopClock(void);
uint8 SWJ_Transaction(uint8 request, uint32 *buff);

void SWJ_Init(void);
void SWJ_Fini(void);
void SWJ_SetTurnaround(uint8 cycles);
void SWJ_SetRetryCount(uint16 retry);
void SWJ_SetDelay(uint16 dly);

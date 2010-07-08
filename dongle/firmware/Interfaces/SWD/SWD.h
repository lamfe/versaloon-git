/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWD.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWD interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define SWD_SUCCESS				0x00
#define SWD_FAULT				0x80
#define SWD_RETRY_OUT			0x40
#define SWD_ACK_ERROR			0x20
#define SWD_PARITY_ERROR		0x10

#define SWD_ACK_OK				0x01
#define SWD_ACK_WAIT			0x02
#define SWD_ACK_FAULT			0x04

#define SWD_TRANS_RnW			(1 << 2)

extern uint8 (*SWD_SeqIn)(uint8 *seq, uint16 num_of_bits);
extern uint8 (*SWD_SeqOut)(uint8 *seq, uint16 num_of_bits);
void SWD_StopClock(void);
uint8 SWD_Transaction(uint8 request, uint32 *buff);

void SWD_Init(void);
void SWD_Fini(void);
void SWD_SetTurnaround(uint8 cycles);
void SWD_SetRetryCount(uint16 retry);
void SWD_SetDelay(uint16 dly);

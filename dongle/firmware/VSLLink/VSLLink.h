/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       VSLLink.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    VSLLink header file                                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

/// no parameters
/// return version string
#define VSLLINK_CMD_CONN			0x80
/// no parameters
/// no return data
#define VSLLINK_CMD_DISCONN			0x81
/// parameter 1(1 byte): jtag speed
/// no return data
#define VSLLINK_CMD_SET_SPEED		0x82
/// parameter 1(1 byte): port mask
/// parameter 2(1 byte): port data
/// no return data(VSLLINK_CMD_GET_PORT will return port data)
#define VSLLINK_CMD_SET_PORT		0x90
#define VSLLINK_CMD_GET_PORT		0x91
#define VSLLINK_CMD_SET_PORTDIR		0x92
/// parameter 1(1 byte): ub
/// parameter 2(1 byte): ua
/// parameter 3(1 byte): bb
/// parameter 4(1 byte): ba
/// no return data
#define VSLLINK_CMD_JTAGHL_SET_DAISYCHAIN_POS	0x98
/// parameter 1(2 bytes): length in bytes
/// parameter 2(length in bytes): JTAGSEQ Commands
/// return data: JTAGSEQ return data
#define VSLLINK_CMD_HW_JTAGSEQCMD	0xA0
/// parameter 1(2 bytes): length in bytes
/// parameter 2(length in bytes): JTAGHL Commands
/// return data: JTAGSEQ return data
#define VSLLINK_CMD_HW_JTAGHLCMD	0xA1
#define VSLLINK_CMD_HW_SWDCMD		0xA2
#define VSLLINK_CMD_HW_JTAGRAWCMD	0xA3


#define VSLLINK_CMDJTAGSEQ_CMDMSK	0xC0
#define VSLLINK_CMDJTAGSEQ_LENMSK	0x3F

#define VSLLINK_CMDJTAGSEQ_TMSBYTE	0x00
#define VSLLINK_CMDJTAGSEQ_TMSCLOCK	0x40
#define VSLLINK_CMDJTAGSEQ_SCAN		0x80


#define VSLLINK_CMDJTAGHL_CMDMSK	0xC0
#define VSLLINK_CMDJTAGHL_LENMSK	0x3F


#define VSLLINK_CMDJTAGHL_TMS		0x00
#define VSLLINK_CMDJTAGHL_IR		0x40
#define VSLLINK_CMDJTAGHL_DR		0x80
#define VSLLINK_CMDJTAGHL_POLL_DLY	0xC0

#define JTAG_PINMSK_SRST			(1 << 0)
#define JTAG_PINMSK_TRST			(1 << 1)
#define JTAG_PINMSK_USR1			(1 << 2)
#define JTAG_PINMSK_USR2			(1 << 3)
#define JTAG_PINMSK_TCK				(1 << 4)
#define JTAG_PINMSK_TMS				(1 << 5)
#define JTAG_PINMSK_TDI				(1 << 6)
#define JTAG_PINMSK_TDO				(1 << 7)

void VSLLink_ProcessCmd(uint8* dat, uint16 len);

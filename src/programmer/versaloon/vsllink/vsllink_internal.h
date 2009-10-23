/***************************************************************************
 *   Copyright (C) 2009 by Simon Qian <SimonQian@SimonQian.com>            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef __VSLLINK_INTERNAL_H_INCLUDED__
#define __VSLLINK_INTERNAL_H_INCLUDED__

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
#define VSLLINK_CMD_HW_SWJCMD		0xA2


#define VSLLINK_MODE_NORMAL			0
#define VSLLINK_MODE_DMA			1
#define VSLLINK_MODE_SWJ			0x80
#define VSLLINK_MODE_JTAG			0x00


// SWJ
#define VSLLINK_CMDSWJ_CMDMSK		0xC0
#define VSLLINK_CMDSWJ_LENMSK		0x3F

#define VSLLINK_CMDSWJ_SEQOUT		0x00
#define VSLLINK_CMDSWJ_SEQIN		0x40
#define VSLLINK_CMDSWJ_TRANS		0x80
#define VSLLINK_CMDSWJ_PARA			0xC0

// JTAG_LL
#define VSLLINK_CMDJTAGSEQ_CMDMSK	0xC0
#define VSLLINK_CMDJTAGSEQ_LENMSK	0x3F

#define VSLLINK_CMDJTAGSEQ_TMSBYTE	0x00
#define VSLLINK_CMDJTAGSEQ_TMSCLOCK	0x40
#define VSLLINK_CMDJTAGSEQ_SCAN		0x80

// JTAG_HL
#define VSLLINK_CMDJTAGHL_CMDMSK	0xC0
#define VSLLINK_CMDJTAGHL_LENMSK	0x3F

#define VSLLINK_CMDJTAGHL_TMS		0x00
#define VSLLINK_CMDJTAGHL_IR		0x40
#define VSLLINK_CMDJTAGHL_DR		0x80
#define VSLLINK_CMDJTAGHL_POLL_DLY	0xC0

#endif /* __VSLLINK_INTERNAL_H_INCLUDED__ */


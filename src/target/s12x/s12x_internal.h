/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
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
#ifndef __S12X_INTERNAL_H_INCLUDED__
#define __S12X_INTERNAL_H_INCLUDED__

#define S12X_PPAGE_ADDR							0x0015

#define S12X_FTMR_FCLKDIV_ADDR					0x0100
#define S12X_FTMR_FCDIV_DIVMASK					0x7F

#define S12X_FTMR_CCOBIX_ADDR					0x0102

#define S12X_FTMR_FSTAT_ADDR					0x0106
#define S12X_FTMR_FSTAT_CCIF					0x80
#define S12X_FTMR_FSTAT_FPVIOL					0x10
#define S12X_FTMR_FSTAT_FACCERR					0x20

#define S12X_FTMR_FPROT_ADDR					0x0108

#define S12X_FTMR_FCCOB_ADDR					0x010A
#define S12X_FTMR_FCMD_EraseVerifyAllBlocks		0x01
#define S12X_FTMR_FCMD_EraseVerifyBlock			0x02
#define S12X_FTMR_FCMD_EraseVerifyPFlashSection	0x03
#define S12X_FTMR_FCMD_ReadOnce					0x04
#define S12X_FTMR_FCMD_ProgramPFlash			0x06
#define S12X_FTMR_FCMD_ProgramOnce				0x07
#define S12X_FTMR_FCMD_EraseAllBlocks			0x08
#define S12X_FTMR_FCMD_EraseFlashBlock			0x09
#define S12X_FTMR_FCMD_ErasePFlashSector		0x0A
#define S12X_FTMR_FCMD_UnsecureFlash			0x0B
#define S12X_FTMR_FCMD_VerifyBackdoorAccessKey	0x0C
#define S12X_FTMR_FCMD_SetUserMarginLevel		0x0D
#define S12X_FTMR_FCMD_SetFieldMarginLevel		0x0E
#define S12X_FTMR_FCMD_EraseverifyDFlashSection	0x10
#define S12X_FTMR_FCMD_ProgramDFlash			0x11
#define S12X_FTMR_FCMD_EraseDFlashSector		0x12

#define S12X_BDM_BDMGPR_ADDR					0x7FFF08

#define S12X_BDM_ROM_START						0x7FFF00
#define S12X_BDM_ROM_END						0x7FFFF0

#endif /* __S12X_INTERNAL_H_INCLUDED__ */


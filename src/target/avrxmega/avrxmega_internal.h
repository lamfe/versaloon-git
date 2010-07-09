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

#ifndef __AVRXMEGA_INTERNAL_H_INCLUDED__
#define __AVRXMEGA_INTERNAL_H_INCLUDED__

#define AVRXMEGA_JTAG							0
#define AVRXMEGA_PDI							1

#define AVRXMEGA_JTAG_RTI_CYCLE					1

#define AVRXMEGA_JTAG_INS_Len					4
#define AVRXMEGA_JTAG_INS_IDCODE				0x03
#define AVRXMEGA_JTAG_INS_SAMPLE_PRELOAD		0x02
#define AVRXMEGA_JTAG_INS_EXTEST				0x01
#define AVRXMEGA_JTAG_INS_CLAMP					0x04
#define AVRXMEGA_JTAG_INS_HIGHZ					0x05
#define	AVRXMEGA_JTAG_INS_PDICOM				0x07
#define AVRXMEGA_JTAG_INS_BYPASS				0x0F

#define AVRXMEGA_JTAG_DR_PDICOM_LEN				9

#endif /* __AVRXMEGA_INTERNAL_H_INCLUDED__ */


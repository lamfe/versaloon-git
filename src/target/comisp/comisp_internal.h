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
#ifndef __COMISP_INTERNAL_H_INCLUDED__
#define __COMISP_INTERNAL_H_INCLUDED__

#define COMM_PARAMETER_UNSURE			'*'

#define COMM_AUXPIN_DISABLE				'N'
#define COMM_AUXPIN_ENABLE				'A'

typedef struct
{
	char comport[21];
	int32 baudrate;
	int8 datalength;
	char paritybit;
	char stopbit;
	char handshake;
	char auxpin;
}com_mode_t;

typedef struct
{
	const char *chip_name;
	com_mode_t com_mode;
	uint8 default_char;
	uint32 flash_start_addr;
}comisp_param_t;

extern comisp_param_t comisp_chip_param;
extern com_mode_t com_mode;
extern uint8 comisp_execute_flag;
extern uint32 comisp_execute_addr;

#endif /* __COMISP_INTERNAL_H_INCLUDED__ */


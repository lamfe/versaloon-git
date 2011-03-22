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

#include "app_type.h"

#if DAL_EE93CX6_EN
#	include "../ee93cx6/ee93cx6_drv.h"
#endif
#if DAL_EE24CXX_EN
#	include "../ee24cxx/ee24cxx_drv.h"
#endif
#if DAL_DF25XX_EN
#	include "../df25xx/df25xx_drv.h"
#endif
#if DAL_DF45XX_EN
#	include "../df45xx/df45xx_drv.h"
#endif

#define MAL_IDX_EE93CX6						1
#define MAL_IDX_EE24CXX						2
#define MAL_IDX_DF25XX						3
#define MAL_IDX_DF45XX						4

struct mal_t
{
	RESULT (*init)(uint16_t index, void *param);
	RESULT (*fini)(uint16_t index);
	RESULT (*getcapacity)(uint16_t index, uint64_t *block_size, 
							uint64_t *block_number);
	RESULT (*setcapacity)(uint16_t index, uint64_t block_size, 
							uint64_t block_number);
	
	RESULT (*eraseall)(uint16_t index);
	RESULT (*eraseblock)(uint16_t index, uint64_t address, uint64_t count);
	RESULT (*readblock)(uint16_t index, uint64_t address, uint8_t *buff, 
						uint64_t count);
	RESULT (*writeblock)(uint16_t index, uint64_t address, uint8_t *buff, 
							uint64_t count);
	
	RESULT (*eraseall_nb_start)(uint16_t index);
	RESULT (*eraseall_nb_isready)(uint16_t index);
	RESULT (*eraseall_nb_waitready)(uint16_t index);
	RESULT (*eraseall_nb_end)(uint16_t index);
	
	RESULT (*eraseblock_nb_start)(uint16_t index, uint64_t address, 
									uint64_t count);
	RESULT (*eraseblock_nb)(uint16_t index, uint64_t address);
	RESULT (*eraseblock_nb_isready)(uint16_t index);
	RESULT (*eraseblock_nb_waitready)(uint16_t index);
	RESULT (*eraseblock_nb_end)(uint16_t index);
	
	RESULT (*readblock_nb_start)(uint16_t index, uint64_t address, 
									uint64_t count);
	RESULT (*readblock_nb)(uint16_t index, uint64_t address, uint8_t *buff);
	RESULT (*readblock_nb_isready)(uint16_t index);
	RESULT (*readblock_nb_waitready)(uint16_t index);
	RESULT (*readblock_nb_end)(uint16_t index);
	
	RESULT (*writeblock_nb_start)(uint16_t index, uint64_t address, 
									uint64_t count);
	RESULT (*writeblock_nb)(uint16_t index, uint64_t address, uint8_t *buff);
	RESULT (*writeblock_nb_isready)(uint16_t index);
	RESULT (*writeblock_nb_waitready)(uint16_t index);
	RESULT (*writeblock_nb_end)(uint16_t index);
};

extern struct mal_t mal;


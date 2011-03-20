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

struct mal_capacity_t
{
	uint64_t block_size;
	uint64_t block_number;
};

struct mal_driver_t
{
	uint16_t index;
	struct mal_capacity_t capacity;
	
	RESULT (*init)(void);
	RESULT (*fini)(void);
	RESULT (*config)(void *param);
	
	RESULT (*eraseall_nb_start)(void);
	RESULT (*eraseall_nb_isready)(void);
	RESULT (*eraseall_nb_waitready)(void);
	RESULT (*eraseall_nb_end)(void);
	
	RESULT (*eraseblock_nb_start)(uint64_t address, uint64_t count);
	RESULT (*eraseblock_nb)(uint64_t address);
	RESULT (*eraseblock_nb_isready)(void);
	RESULT (*eraseblock_nb_waitready)(void);
	RESULT (*eraseblock_nb_end)(void);
	
	RESULT (*readblock_nb_start)(uint64_t address, uint64_t count);
	RESULT (*readblock_nb)(uint64_t address, uint8_t *buff);
	RESULT (*readblock_nb_isready)(void);
	RESULT (*readblock_nb_waitready)(void);
	RESULT (*readblock_nb_end)(void);
	
	RESULT (*writeblock_nb_start)(uint64_t address, uint64_t count);
	RESULT (*writeblock_nb)(uint64_t address, uint8_t *buff);
	RESULT (*writeblock_nb_isready)(void);
	RESULT (*writeblock_nb_waitready)(void);
	RESULT (*writeblock_nb_end)(void);
};


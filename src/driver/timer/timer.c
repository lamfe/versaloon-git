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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <time.h>

#if IS_WIN32
#else /* !IS_WIN32 */
#include <sys/time.h>
#endif /* IS_WIN32 */

#include "app_type.h"

#include "timer.h"

uint32_t get_time_in_ms(void)
{
#if IS_WIN32
	return (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
#else
	struct timeval cur_time;
	gettimeofday(&cur_time, NULL);
	return (uint32_t)(cur_time.tv_usec / 1000 + cur_time.tv_sec * 1000);
#endif /* IS_WIN32 */
}


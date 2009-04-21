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
#ifndef __APP_LOG_H_INCLUDED__
#define __APP_LOG_H_INCLUDED__

#define ERROR_LEVEL			0
#define WARNING_LEVEL		0
#define INFO_LEVEL			1
#define DEBUG_LEVEL			2

#define LOG_DEFAULT_LEVEL	INFO_LEVEL

#define _GETTEXT(str)		(str)

extern int verbosity;

#if 1
#	define LOG_ERROR(...)	do{\
								if (verbosity >= ERROR_LEVEL)\
								{\
									fprintf(stderr, "Error:  ");\
									fprintf(stderr, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_WARNING(...)	do{\
								if (verbosity >= WARNING_LEVEL)\
								{\
									fprintf(stdout, "Warning:");\
									fprintf(stdout, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_INFO(...)	do{\
								if (verbosity >= INFO_LEVEL)\
								{\
									fprintf(stdout, "Info:   ");\
									fprintf(stdout, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_DEBUG(...)	do{\
								if (verbosity >= DEBUG_LEVEL)\
								{\
									fprintf(stderr, "Debug:  ");\
									fprintf(stdout, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_BUG(...)		do{\
								fprintf(stderr, "Bug:    ");\
								fprintf(stderr, __VA_ARGS__);\
							}while(0)
#elif 1
#	define LOG_ERROR(...)	do{\
								if (verbosity >= ERROR_LEVEL)\
								{\
									fprintf(stderr, "Error:  %s:%d %s: ", \
											__FILE__, __LINE__, __FUNCTION__);\
									fprintf(stderr, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_WARNING(...)	do{\
								if (verbosity >= WARNING_LEVEL)\
								{\
									fprintf(stderr, "Warning:%s:%d %s: ", \
											__FILE__, __LINE__, __FUNCTION__);\
									fprintf(stdout, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_INFO(...)	do{\
								if (verbosity >= INFO_LEVEL)\
								{\
									fprintf(stderr, "Info:   %s:%d %s: ", \
											__FILE__, __LINE__, __FUNCTION__);\
									fprintf(stdout, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_DEBUG(...)	do{\
								if (verbosity >= DEBUG_LEVEL)\
								{\
									fprintf(stderr, "Debug:  %s:%d %s: ", \
											__FILE__, __LINE__, __FUNCTION__);\
									fprintf(stdout, __VA_ARGS__);\
								}\
							}while(0)
#	define LOG_BUG(...)		do{\
								fprintf(stderr, "Bug:    %s:%d %s: ", \
											__FILE__, __LINE__, __FUNCTION__);\
								fprintf(stderr, __VA_ARGS__);\
							}while(0)
#else
#	define LOG_ERROR		printf
#	define LOG_WARNING		printf
#	define LOG_INFO			printf
#	define LOG_DEBUG		printf
#	define LOG_BUG			printf
#endif

#endif /* __APP_LOG_H_INCLUDED__ */


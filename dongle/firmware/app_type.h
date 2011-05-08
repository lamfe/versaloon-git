/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       app_type.h                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    type defines                                              *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#ifndef __APP_TYPE_H_INCLUDED__
#define __APP_TYPE_H_INCLUDED__

#include <stdint.h>
#include <stdbool.h>

#define dimof(arr)					(sizeof(arr) / sizeof((arr)[0]))

#define GET_U16_MSBFIRST(p)			(	((*((uint8_t *)(p) + 0)) << 8) | \
										((*((uint8_t *)(p) + 1)) << 0))
#define GET_U32_MSBFIRST(p)			(	((*((uint8_t *)(p) + 0)) << 24) | \
										((*((uint8_t *)(p) + 1)) << 16) | \
										((*((uint8_t *)(p) + 2)) << 8) | \
										((*((uint8_t *)(p) + 3)) << 0))
#define GET_U16_LSBFIRST(p)			(	((*((uint8_t *)(p) + 0)) << 0) | \
										((*((uint8_t *)(p) + 1)) << 8))
#define GET_U32_LSBFIRST(p)			(	((*((uint8_t *)(p) + 0)) << 0) | \
										((*((uint8_t *)(p) + 1)) << 8) | \
										((*((uint8_t *)(p) + 2)) << 16) | \
										((*((uint8_t *)(p) + 3)) << 24))

#define SET_U16_MSBFIRST(p, v)		do{\
										*((uint8_t *)(p) + 0) = (((uint16_t)(v)) >> 8) & 0xFF;\
										*((uint8_t *)(p) + 1) = (((uint16_t)(v)) >> 0) & 0xFF;\
									} while (0)
#define SET_U32_MSBFIRST(p, v)		do{\
										*((uint8_t *)(p) + 0) = (((uint32_t)(v)) >> 24) & 0xFF;\
										*((uint8_t *)(p) + 1) = (((uint32_t)(v)) >> 16) & 0xFF;\
										*((uint8_t *)(p) + 2) = (((uint32_t)(v)) >> 8) & 0xFF;\
										*((uint8_t *)(p) + 3) = (((uint32_t)(v)) >> 0) & 0xFF;\
									} while (0)
#define SET_U16_LSBFIRST(p, v)		do{\
										*((uint8_t *)(p) + 0) = (((uint16_t)(v)) >> 0) & 0xFF;\
										*((uint8_t *)(p) + 1) = (((uint16_t)(v)) >> 8) & 0xFF;\
									} while (0)
#define SET_U32_LSBFIRST(p, v)		do{\
										*((uint8_t *)(p) + 0) = (((uint32_t)(v)) >> 0) & 0xFF;\
										*((uint8_t *)(p) + 1) = (((uint32_t)(v)) >> 8) & 0xFF;\
										*((uint8_t *)(p) + 2) = (((uint32_t)(v)) >> 16) & 0xFF;\
										*((uint8_t *)(p) + 3) = (((uint32_t)(v)) >> 24) & 0xFF;\
									} while (0)

#define GET_LE_U16(p)				GET_U16_LSBFIRST(p)
#define GET_LE_U32(p)				GET_U32_LSBFIRST(p)
#define GET_BE_U16(p)				GET_U16_MSBFIRST(p)
#define GET_BE_U32(p)				GET_U32_MSBFIRST(p)
#define SET_LE_U16(p, v)			SET_U16_LSBFIRST(p, v)
#define SET_LE_U32(p, v)			SET_U32_LSBFIRST(p, v)
#define SET_BE_U16(p, v)			SET_U16_MSBFIRST(p, v)
#define SET_BE_U32(p, v)			SET_U32_MSBFIRST(p, v)

#ifndef NULL
#	define NULL						((void*)0)
#endif

#endif // __APP_TYPE_H_INCLUDED__

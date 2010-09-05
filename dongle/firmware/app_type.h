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

typedef u8 uint8;
typedef u16 uint16;
typedef s8 int8;
typedef s16 int16;
typedef u32 uint32;
typedef s32 int32;

#define GET_U16_MSBFIRST(p)			(	((*((uint8 *)(p) + 0)) << 8) | \
										((*((uint8 *)(p) + 1)) << 0))
#define GET_U32_MSBFIRST(p)			(	((*((uint8 *)(p) + 0)) << 24) | \
										((*((uint8 *)(p) + 1)) << 16) | \
										((*((uint8 *)(p) + 2)) << 8) | \
										((*((uint8 *)(p) + 3)) << 0))
#define GET_U16_LSBFIRST(p)			(	((*((uint8 *)(p) + 0)) << 0) | \
										((*((uint8 *)(p) + 1)) << 8))
#define GET_U32_LSBFIRST(p)			(	((*((uint8 *)(p) + 0)) << 0) | \
										((*((uint8 *)(p) + 1)) << 8) | \
										((*((uint8 *)(p) + 2)) << 16) | \
										((*((uint8 *)(p) + 3)) << 24))

#define SET_U16_MSBFIRST(p, v)		do{\
										*((uint8 *)(p) + 0) = (((uint16)(v)) >> 8) & 0xFF;\
										*((uint8 *)(p) + 1) = (((uint16)(v)) >> 0) & 0xFF;\
									} while (0)
#define SET_U32_MSBFIRST(p, v)		do{\
										*((uint8 *)(p) + 0) = (((uint32)(v)) >> 24) & 0xFF;\
										*((uint8 *)(p) + 1) = (((uint32)(v)) >> 16) & 0xFF;\
										*((uint8 *)(p) + 2) = (((uint32)(v)) >> 8) & 0xFF;\
										*((uint8 *)(p) + 3) = (((uint32)(v)) >> 0) & 0xFF;\
									} while (0)
#define SET_U16_LSBFIRST(p, v)		do{\
										*((uint8 *)(p) + 0) = (((uint16)(v)) >> 0) & 0xFF;\
										*((uint8 *)(p) + 1) = (((uint16)(v)) >> 8) & 0xFF;\
									} while (0)
#define SET_U32_LSBFIRST(p, v)		do{\
										*((uint8 *)(p) + 0) = (((uint32)(v)) >> 0) & 0xFF;\
										*((uint8 *)(p) + 1) = (((uint32)(v)) >> 8) & 0xFF;\
										*((uint8 *)(p) + 2) = (((uint32)(v)) >> 16) & 0xFF;\
										*((uint8 *)(p) + 3) = (((uint32)(v)) >> 24) & 0xFF;\
									} while (0)

#define GET_LE_U16(p)				GET_U16_LSBFIRST(p)
#define GET_LE_U32(p)				GET_U32_LSBFIRST(p)
#define GET_BE_U16(p)				GET_U16_MSBFIRST(p)
#define GET_BE_U32(p)				GET_U32_MSBFIRST(p)
#define SET_LE_U16(p, v)			SET_U16_LSBFIRST(p, v)
#define SET_LE_U32(p, v)			SET_U32_LSBFIRST(p, v)
#define SET_BE_U16(p, v)			SET_U16_MSBFIRST(p, v)
#define SET_BE_U32(p, v)			SET_U32_MSBFIRST(p, v)

#endif // __APP_TYPE_H_INCLUDED__

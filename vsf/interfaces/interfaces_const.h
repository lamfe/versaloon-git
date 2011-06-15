/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces_const.h                                        *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    consts of interface module                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-04-08:     created(by SimonQian)                             *
 **************************************************************************/
#ifndef __INTERFACE_CONST_H_INCLUDED__
#define __INTERFACE_CONST_H_INCLUDED__

#define SPI_CPOL_MASK			0x20
#define SPI_CPOL_HIGH			0x20
#define SPI_CPOL_LOW			0x00
#define SPI_CPHA_MASK			0x40
#define SPI_CPHA_2EDGE			0x40
#define SPI_CPHA_1EDGE			0x00
#define SPI_FIRSTBIT_MASK		0x80
#define SPI_MSB_FIRST			0x80
#define SPI_LSB_FIRST			0x00

#endif /* __INTERFACE_CONST_H_INCLUDED__ */


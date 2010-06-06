/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SPI.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SPI interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

// Only Support Master Mode

#define SPI_MAX_SPEED			(_SYS_FREQUENCY * 500 / 2)		// in KHz
#define SPI_MIN_SPEED			(_SYS_FREQUENCY * 500 / 256)	// in KHz

#define SPI_DATA_LEN			8
#define SPI_MSB					(1 << (SPI_DATA_LEN - 1))

uint8 SPI_RW_Emu(uint8 data);
uint8 SPI_RW_HW(uint8 data);
void SPI_Config(uint32 freq_hz, uint32 firstbit, uint32 cpol, uint32 cpha);
uint8 SPI_GetSCKDiv(uint16 freq_khz);
uint8 SPI_RW(uint8 data);

extern uint8 SPI_Emu;

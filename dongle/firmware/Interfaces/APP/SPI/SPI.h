/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
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

#define SPI_MAX_KHZ				(_SYS_FREQUENCY * 500 / 2)		// in KHz
#define SPI_MIN_KHZ				(_SYS_FREQUENCY * 500 / 256)	// in KHz

uint8_t SPI_GetSCKDiv(uint16_t freq_khz);

RESULT spi_init(uint8_t index);
RESULT spi_fini(uint8_t index);
RESULT spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint16_t len);
RESULT spi_config(uint8_t index, uint16_t kHz, uint8_t cpol, uint8_t cpha, 
					 uint8_t first_bit);

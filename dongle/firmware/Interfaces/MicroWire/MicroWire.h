/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MicroWire.h                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MicroWire interface header file                           *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

RESULT microwire_init(uint8_t index);
RESULT microwire_fini(uint8_t index);
RESULT microwire_config(uint8_t index, uint16_t kHz, uint8_t sel_polarity);
RESULT microwire_io(uint8_t index, uint8_t *opcode, uint16_t opcode_bitlen, 
					uint8_t *input, uint16_t input_bitlen);
RESULT microwire_poll(uint8_t index, uint16_t interval_us, uint16_t retry_cnt);

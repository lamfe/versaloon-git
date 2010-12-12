/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       JTAG_TAP.h                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    JTAG interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define JTAG_TAP_HS_MAX_KHZ				(_SYS_FREQUENCY * 500 / 2)		// in KHz
#define JTAG_TAP_HS_MIN_KHZ				(_SYS_FREQUENCY * 500 / 256)	// in KHz

RESULT jtag_hl_init(uint8_t index);
RESULT jtag_hl_fini(uint8_t index);
RESULT jtag_hl_config(uint8_t index, uint16_t kHz, uint8_t ub, uint8_t ua, 
						uint16_t bb, uint16_t ba);
RESULT jtag_hl_tms(uint8_t index, uint8_t* tms, uint16_t bitlen);
RESULT jtag_hl_runtest(uint8_t index, uint32_t cycles);
RESULT jtag_hl_ir(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle);
RESULT jtag_hl_dr(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle);

RESULT jtag_ll_init(uint8_t index);
RESULT jtag_ll_fini(uint8_t index);
RESULT jtag_ll_config(uint8_t index, uint16_t kHz);
RESULT jtag_ll_tms(uint8_t index, uint8_t *tms, uint8_t bytelen);
RESULT jtag_ll_tms_clocks(uint8_t index, uint32_t bytelen, uint8_t tms);
RESULT jtag_ll_scan(uint8_t index, uint8_t* data, uint16_t bitlen, 
					uint8_t tms_before_valid, uint8_t tms_before, 
					uint8_t tms_after0, uint8_t tms_after1);

RESULT jtag_raw_init(uint8_t index);
RESULT jtag_raw_fini(uint8_t index);
RESULT jtag_raw_config(uint8_t index, uint16_t kHz);
RESULT jtag_raw_execute(uint8_t index, uint8_t* tdi, uint8_t* tms, 
						uint8_t *tdo, uint32_t bitlen);

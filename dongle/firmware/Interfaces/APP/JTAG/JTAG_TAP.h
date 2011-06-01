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

RESULT jtaghl_init(uint8_t index);
RESULT jtaghl_fini(uint8_t index);
RESULT jtaghl_config_speed(uint8_t index, uint32_t kHz);
RESULT jtaghl_config_daisychain(uint8_t index, uint8_t ub, uint8_t ua, 
						uint16_t bb, uint16_t ba);
RESULT jtaghl_config(uint8_t index, uint32_t kHz, uint8_t ub, uint8_t ua, 
						uint16_t bb, uint16_t ba);
RESULT jtaghl_tms(uint8_t index, uint8_t* tms, uint16_t bitlen);
RESULT jtaghl_runtest(uint8_t index, uint32_t cycles);
RESULT jtaghl_ir(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle, 
				 uint8_t want_ret);
RESULT jtaghl_dr(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle, 
				 uint8_t want_ret);
RESULT jtaghl_register_callback(uint8_t index, jtag_callback_t send_callback, 
								 jtag_callback_t receive_callback);

RESULT jtagll_init(uint8_t index);
RESULT jtagll_fini(uint8_t index);
RESULT jtagll_config(uint8_t index, uint32_t kHz);
RESULT jtagll_tms(uint8_t index, uint8_t *tms, uint8_t bytelen);
RESULT jtagll_tms_clocks(uint8_t index, uint32_t bytelen, uint8_t tms);
RESULT jtagll_scan(uint8_t index, uint8_t* data, uint16_t bitlen, 
					uint8_t tms_before_valid, uint8_t tms_before, 
					uint8_t tms_after0, uint8_t tms_after1);

RESULT jtagraw_init(uint8_t index);
RESULT jtagraw_fini(uint8_t index);
RESULT jtagraw_config(uint8_t index, uint32_t kHz);
RESULT jtagraw_execute(uint8_t index, uint8_t* tdi, uint8_t* tms, 
						uint8_t *tdo, uint32_t bitlen);

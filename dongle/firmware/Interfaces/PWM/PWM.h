/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       PWM.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    PWM header file                                           *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-03-06:     created(by SimonQian)                             *
 **************************************************************************/

RESULT pwm_init(uint8_t index);
RESULT pwm_fini(uint8_t index);
RESULT pwm_config(uint8_t index, uint16_t kHz);
RESULT pwm_out(uint8_t index, uint16_t count, uint16_t *rate);

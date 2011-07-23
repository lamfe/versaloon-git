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

#define PWM_OUTPP				0x01
#define PWM_OUTPOLARITY			0x02

RESULT pwm_init(uint8_t index);
RESULT pwm_fini(uint8_t index);
RESULT pwm_config(uint8_t index, uint16_t kHz, uint8_t mode);
RESULT pwm_out(uint8_t index, uint16_t count, uint16_t *rate);
RESULT pwm_in(uint8_t index, uint16_t count, uint16_t *rate);

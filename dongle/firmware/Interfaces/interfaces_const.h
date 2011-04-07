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
#ifndef __PROG_INTERFACE_H_INCLUDED__
#define __PROG_INTERFACE_H_INCLUDED__

// interfaces
#define IFS_USART				(1ULL << 0)
#define IFS_SPI					(1ULL << 1)
#define IFS_I2C					(1ULL << 2)
#define IFS_GPIO				(1ULL << 3)
#define IFS_CAN					(1ULL << 4)
#define IFS_CLOCK				(1ULL << 5)
#define IFS_ADC					(1ULL << 6)
#define IFS_DAC					(1ULL << 7)
#define IFS_POWER				(1ULL << 8)
#define IFS_ISSP				(1ULL << 16)
#define IFS_JTAG_LL				(1ULL << 17)
#define IFS_JTAG_HL				(1ULL << 18)
#define IFS_MSP430_SBW			(1ULL << 19)
#define IFS_C2					(1ULL << 20)
#define IFS_MSP430_JTAG			(1ULL << 21)
#define IFS_LPC_ICP				(1ULL << 22)
#define IFS_SWD					(1ULL << 23)
#define IFS_SWIM				(1ULL << 24)
#define IFS_HV					(1ULL << 25)
#define IFS_PDI					(1ULL << 26)
#define IFS_JTAG_RAW			(1ULL << 27)
#define IFS_BDM					(1ULL << 28)
#define IFS_POLL				(1ULL << 29)
#define IFS_DUSI				(1ULL << 30)
#define IFS_MICROWIRE			(1ULL << 31)
#define IFS_PWM					(1ULL << 32)
#define IFS_INVALID_INTERFACE	(1ULL << 63)
#define IFS_MASK				(USART | SPI | I2C | GPIO | CAN | CLOCK | ADC \
								 | DAC | POWER | ISSP | JTAG | MSP430_JTAG \
								 | LPC_ICP | MSP430_SBW | SWD | SWIM | HV | BDM\
								 | MICROWIRE)

#endif /* __PROG_INTERFACE_H_INCLUDED__ */


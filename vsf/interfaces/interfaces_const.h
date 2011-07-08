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

// PWM
#define PWM_OUTPP				0x01
#define PWM_OUTPOLARITY			0x02

// SPI
#define SPI_MODE0				0
#define SPI_MODE1				1
#define SPI_MODE2				2
#define SPI_MODE3				3
#define SPI_MSB_FIRST			0x80
#define SPI_LSB_FIRST			0x00

// USART
#define USART_MODE0				0x00
#define USART_MODE1				0x04
#define USART_MODE2				0x08
#define USART_MODE3				0x0C
#define USART_CLKEN				0x80
#define USART_STOPBITS_0P5		0x20
#define USART_STOPBITS_1		0x00
#define USART_STOPBITS_1P5		0x60
#define USART_STOPBITS_2		0x40
#define USART_PARITY_NONE		0x00
#define USART_PARITY_ODD		0x03
#define USART_PARITY_EVEN		0x02

#endif /* __INTERFACE_CONST_H_INCLUDED__ */

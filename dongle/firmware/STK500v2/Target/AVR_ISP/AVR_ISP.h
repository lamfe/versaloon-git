/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       AVR_ISP.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for AVR_ISP support                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

// ISP Interfaces:
// MOSI		<-->	OUT	defined in spi module
// MISO		<-->	IN	defined in spi module
// SCK		<-->	OUT	defined in spi module
// RST		<-->	OD

/// ISP Disable Reset
#define AVRISP_RST_Off()			do{\
										if(STK500_PARAM_Reset_Polarity)\
										{\
											interfaces->gpio.out(0, GPIO_SRST, GPIO_SRST);\
										}\
										else\
										{\
											interfaces->gpio.out(0, GPIO_SRST, 0);\
										}\
									} while (0)
/// ISP Enable Reset
#define AVRISP_RST_On()				do {\
										if(STK500_PARAM_Reset_Polarity)\
										{\
											interfaces->gpio.out(0, GPIO_SRST, 0);\
										}\
										else\
										{\
											interfaces->gpio.out(0, GPIO_SRST, GPIO_SRST);\
										}\
									} while (0)


void AVRISP_Init(uint32 freq);
void AVRISP_Fini(void);
#define AVRISP_Comm(cmd, ret)		AVRISP_CommInt(cmd, ret, 4)
void AVRISP_CommInt(uint8*, uint8*, uint32);

#define AVRISP_Error				1
#define AVRISP_Success				0

uint8 AVRISP_RDY_Wait(void);

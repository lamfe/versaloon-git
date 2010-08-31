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
#define AVRISP_RST_Off()			if(STK500_PARAM_Reset_Polarity)RST_SET();else RST_CLR()
/// ISP Enable Reset
#define AVRISP_RST_On()				if(STK500_PARAM_Reset_Polarity)RST_CLR();else RST_SET()


void AVRISP_Init(uint32 freq);
void AVRISP_Fini(void);
#define AVRISP_Comm(cmd, ret)		AVRISP_CommInt(cmd, ret, 4)
void AVRISP_CommInt(uint8*, uint8*, uint32);

#define AVRISP_Error				1
#define AVRISP_Success				0

uint8 AVRISP_RDY_Wait(void);

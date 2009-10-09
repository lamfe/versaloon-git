/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       I2C.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    I2C interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define I2C_ADDR_MASK				0x03FF
#define I2C_ADDR_10BIT				0x8000

uint8 I2C_Init(uint16 kHz);
void I2C_Fini(void);
uint8 I2C_SetParameter(uint16 kHz);
uint16 I2C_Write(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop);
uint16 I2C_Read(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop);

/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       IIC.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    IIC interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define IIC_ADDR_MASK				0x03FF
#define IIC_ADDR_10BIT				0x8000

uint8 IIC_Init(uint16 kHz, uint16 ByteInterval, uint16 max_dly);
void IIC_Fini(void);
uint8 IIC_Write(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop, uint16_t *actual_len);
uint8 IIC_Read(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop, uint16_t *actual_len);

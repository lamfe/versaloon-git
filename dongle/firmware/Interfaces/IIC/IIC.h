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

RESULT iic_init(uint8_t index);
RESULT iic_fini(uint8_t index);
RESULT iic_config(uint8_t index, uint16_t kHz, uint16_t byte_interval, 
				 uint16_t max_dly);
RESULT iic_read(uint8_t index, uint16_t chip_addr, uint8_t *data, 
			   uint16_t data_len, uint8_t stop);
RESULT iic_write(uint8_t index, uint16_t chip_addr, uint8_t *data, 
				uint16_t data_len, uint8_t stop);

/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SPI.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SPI interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

RESULT ebi_init(uint8_t index);
RESULT ebi_fini(uint8_t index);
RESULT ebi_config(uint8_t index, uint8_t target_index, void *param);
RESULT ebi_read(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count);
RESULT ebi_write(uint8_t index, uint8_t target_index, uint32_t address, 
					uint8_t data_size, uint8_t *buff, uint32_t count);

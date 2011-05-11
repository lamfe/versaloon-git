/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       C2.h                                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    C2 interface header file                                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

RESULT c2_init(uint8_t index);
RESULT c2_fini(uint8_t index);
RESULT c2_addr_write(uint8_t index, uint8_t addr);
RESULT c2_addr_read(uint8_t index, uint8_t *data);
RESULT c2_data_read(uint8_t index, uint8_t *data, uint8_t len);
RESULT c2_data_write(uint8_t index, uint8_t *data, uint8_t len);

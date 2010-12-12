/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWD.h                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWD interface header file                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

RESULT swd_init(uint8_t index);
RESULT swd_fini(uint8_t index);
RESULT swd_config(uint8_t index, uint8_t trn, uint16_t retry, uint16_t dly);
RESULT swd_seqout(uint8_t index, uint8_t *data, uint16_t bitlen);
RESULT swd_seqin(uint8_t index, uint8_t *data, uint16_t bitlen);
RESULT swd_transact(uint8_t index, uint8_t request, uint32_t *data, uint8_t *ack);

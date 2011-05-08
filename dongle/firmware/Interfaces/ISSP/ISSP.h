/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       ISSP.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    ISSP interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define ISSP_OPERATE_BANK		(1 << 0)
#define ISSP_OPERATE_READ		(1 << 1)
#define ISSP_OPERATE_APPENDBIT	(1 << 2)
#define ISSP_OPERATE_0s			(1 << 3)

RESULT issp_vector(uint8_t index, uint8_t operate, uint8_t addr, 
					 uint8_t data, uint8_t *buf);
RESULT issp_enter_program_mode(uint8_t index, uint8_t mode);
RESULT issp_leave_program_mode(uint8_t index, uint8_t mode);
RESULT issp_wait_and_poll(uint8_t index);
RESULT issp_init(uint8_t index);
RESULT issp_fini(uint8_t index);

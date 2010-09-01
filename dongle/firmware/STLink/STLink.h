/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STLink                                                    *
 *  File:       STLink.c                                                  *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for STLink STM8 protocol                           *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-01:     created(by SimonQian)                             *
 **************************************************************************/
#define STLINK_SCSI_MIN_CMD						0xF0

void STLink_SWIM_Process(uint8_t *cmd);
void STLink_SCSI_Process(uint8_t *cmd);

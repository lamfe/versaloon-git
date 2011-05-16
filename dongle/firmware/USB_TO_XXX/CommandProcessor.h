/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       CommandProcesor.h                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header of CommandProcesor for Versaloon/USB_TO_XXX        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

// Common Commands
#define VERSALOON_COMMON_CMD_START		0x00
#define VERSALOON_COMMON_CMD_END		0x0F

#define VERSALOON_GET_INFO				0x00
#define VERSALOON_GET_TVCC				0x01
#define VERSALOON_GET_HARDWARE			0x02
#define VERSALOON_GET_OFFLINE_SIZE		0x08
#define VERSALOON_ERASE_OFFLINE_DATA	0x09
#define VERSALOON_WRITE_OFFLINE_DATA	0x0A
#define VERSALOON_GET_OFFLINE_CHECKSUM	0x0B
#define VERSALOON_FW_UPDATE				0x0F

// USB_TO_XXX Command
#define VERSALOON_USB_TO_XXX_CMD_START	0x20
#define VERSALOON_USB_TO_XXX_CMD_END	0x7F

void ProcessCommand(uint8_t *dat, uint16_t len);
void ProcessIdle(void);

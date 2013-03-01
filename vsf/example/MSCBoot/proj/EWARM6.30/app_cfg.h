/**************************************************************************
 *  Copyright (C) 2008 - 2012 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    VSF                                                       *
 *  File:       app_cfg.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    configuration file                                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

// hardware config file
#include "hw_cfg_STM32.h"

// compiler config
#include "compiler.h"

#define APP_CFG_MSC_WRITEONLY			0
#define APP_CFG_BOOTSIZE				(32 * 1024)

#define EVSPROG_EN						0
#if EVSPROG_EN
#	define TARGET_CFG_ADDR				0x08040000
#	define EVSPROG_SCRIPT_ADDR			0x0807C000
#	define EVSPROG_SCRIPT_SIZE			(16 * 1024)
#endif

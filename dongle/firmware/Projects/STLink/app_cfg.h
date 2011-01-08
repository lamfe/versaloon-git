/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STLink                                                    *
 *  File:       app_cfg.h                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    configuration file                                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-01:     created(by SimonQian)                             *
 **************************************************************************/

/************************ What do U have ************************/
// enable ONLY one below according to your hardware
//#include "hw_cfg_NanoRelease1.h"
//#include "hw_cfg_MiniRC2.h"
//#include "hw_cfg_MiniRC3.h"
//#include "hw_cfg_MiniRC4.h"
#include "hw_cfg_MiniRelease1.h"
//#include "hw_cfg_ProRC1.h"
//#include "hw_cfg_STBee_Mini.h"

/*************************** Includes Library ***************************/
#include "stm32f10x_conf.h"
#include "app_type.h"
#include "port.h"
#include "myString.h"
#include "CommandProcessor.h"

/************************ What do U want ************************/
#define USB_STLINK						0x30

#define USB_PROTOCOL					USB_STLINK

#define USB_OUT_EN						0

#define INTERFACE_GPIO_EN				1
#define INTERFACE_SWIM_EN				1
#define POWER_OUT_EN					1
#define POWER_SAMPLE_EN					1

#define USB_WITH_MASSSTORAGE			1

/****************************** Mass ******************************/
#define MSD_MEMORY_START_ADDR			0x0800A000	// from 40K
#define MSD_MEMORY_BLOCK_SIZE			1024
#define MSD_BUFF_SIZE_IN_DWORD			2048

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				8192
#define ASYN_DATA_BUFF_SIZE				1

/**************************** Checks ****************************/
#if !POWER_OUT_EN
#define PWREXT_Acquire()
#define PWREXT_Release()
#endif

#define INTERFACE_SWIM_EN				1

/*************************** Includes Application ***************************/
#include "HW.h"

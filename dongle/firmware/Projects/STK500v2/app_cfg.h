/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500v2                                                  *
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
#define USB_AT_DRAGON					10
#define USB_AT_JTAGICE_MKII				11

#define USB_PROTOCOL					USB_AT_JTAGICE_MKII

#define USB_OUT_EN						1

#define POWER_OUT_EN					1
#define POWER_SAMPLE_EN					1

#define AVR_JTAG_DEFAULT_SPEED			4500		// in KHz

/***************************** Buffer ****************************/
#define USB_DATA_BUFF_SIZE				(12 * 1024)
#define ASYN_DATA_BUFF_SIZE				(4 * 1024)

/**************************** Checks ****************************/
#if !POWER_OUT_EN
#define PWREXT_Acquire()
#define PWREXT_Release()
#endif

#define USB_RX_DOUBLEBUFFER_EN			0
#define USB_TX_DOUBLEBUFFER_EN			0

#define INTERFACE_JTAG_EN				1
#define INTERFACE_SPI_EN				1

/*************************** Includes Application ***************************/
#include "HW.h"

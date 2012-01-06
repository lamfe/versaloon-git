/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       main.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    main.c file                                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

/* Includes ------------------------------------------------------------------*/
#include "app_cfg.h"
#include "app_log.h"
#include "interfaces.h"

#include "usb_protocol.h"

#if SCRIPTS_EN
#include "scripts.h"
#include "interfaces_script.h"

int verbosity = LOG_DEFAULT_LEVEL;
int verbosity_stack[1];
#endif

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
int main(void)
{
	core_interfaces.core.init(NULL);
	usb_protocol_init();
#if SCRIPTS_EN
	vss_init();
	vss_register_cmd_list(&interface_cmd_list);
#endif
	
	while (1)
	{
		usb_protocol_poll();
	}
}

/******************* (C) COPYRIGHT 2007 STMicroelectronics *****END OF FILE****/

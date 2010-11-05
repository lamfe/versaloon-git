/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    USBDM                                                     *
 *  File:       CommandProcesor.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for USBDM                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-02:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "USBBlaster_JTAG&AS.h"

void BeforeInit(void){}

void AfterInit(void)
{
	USBBlaster_TMS_SETOUTPUT();
	USBBlaster_TCK_SETOUTPUT();
	USBBlaster_TDI_SETOUTPUT();
	USBBlaster_TDO_SETINPUT();
	USBBlaster_NCS_SETOUTPUT();
	USBBlaster_NCE_SETOUTPUT();
	USBBlaster_ASDO_SETINPUT();
	
	GPIO_SetMode(GPIOB, GPIO_PIN_7, GPIO_MODE_IN_FLOATING);
	GPIO_SetMode(GPIOB, GPIO_PIN_6, GPIO_MODE_IN_FLOATING);
	GPIO_SetMode(GPIOB, GPIO_PIN_3, GPIO_MODE_IN_FLOATING);
}

void ProcessCommand(uint8* dat, uint16 len) {}

void ProcessIdle(void) {}

uint8 CheckLocalHandler(void)
{
	return 1;
}

void ProcessLocalHandler(void)
{
	if ((usb_in_data_remain != usb_in_numofpackage) && (usb_ovf == 0))
	{ 		  
		Handle_USBAsynchXfer();
	}
	if ((usb_ovf == 0) && (CALL != 0))
	{
		usb_ovf = 1;
		STAT();
		CALL--;
	}
}

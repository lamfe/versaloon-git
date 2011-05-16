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
#include "interfaces.h"

#include "usb_protocol.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/

int main(void)
{
	core_interfaces.init(NULL);
	usb_protocol_init();

	LED_GREEN_ON();
	LED_USB_ON();

	while(1)							// system main loop
	{
		if(cmd_len & 0x80000000)
		{
			// A valid USB package has been received
			LED_USB_OFF();				// USB LED indicate the USB transmission

			ProcessCommand(&buffer_out[0], cmd_len & 0xFFFF);

			if(rep_len > 0)
			{
				// indicate reply data is valid
				rep_len |= 0x80000000;
			}
			else
			{
				// no data to send, set cmd_len to 0
				cmd_len = 0;
			}

			count_out = 0;				// set USB receive pointer to 0
		}
		else
		{
			PWREXT_Check(1);
			ProcessIdle();				// Idle loop
		}

		if(rep_len & 0x80000000)		// there is valid data to be sent to PC
		{
			struct vsf_buffer_t buffer;
			
			buffer.buffer = buffer_out;
			buffer.size = rep_len & 0xFFFF;
			vsfusbd_ep_out(&usb_device, 2, &buffer);

			// reset command length and reply length for next command
			cmd_len = 0;
			rep_len = 0;
		}
		LED_USB_ON();					// command processed, light USB LED
	}
}

/******************* (C) COPYRIGHT 2007 STMicroelectronics *****END OF FILE****/

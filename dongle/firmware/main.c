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

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/

int main(void)
{
	interfaces->init(NULL);

	LED_GREEN_ON();
	LED_USB_ON();

	while(1)							// system main loop
	{
		if (CheckLocalHandler())
		{
			ProcessLocalHandler();
		}
		else if(cmd_len & 0x80000000)
		{
			// A valid USB package has been received
			LED_USB_OFF();				// USB LED indicate the USB transmission

			ProcessCommand(&buffer_out[0], cmd_len & 0xFFFF);

#ifdef USB_OUT_EN
#if USB_OUT_EN
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
#else
			cmd_len = 0;
#endif
#else
			cmd_len = 0;
#endif
		}
		else
		{
			PWREXT_Check(1);
			ProcessIdle();				// Idle loop
		}

#ifdef USB_OUT_EN
#if USB_OUT_EN
		if(rep_len & 0x80000000)		// there is valid data to be sent to PC
		{
			USB_Out(buffer_out, rep_len & 0xFFFF);
			if (USB_Out_PollReady() < 0)
			{
				// error process
			}

			// reset command length and reply length for next command
			cmd_len = 0;
			rep_len = 0;
		}
#endif
#endif
		LED_USB_ON();					// command processed, light USB LED
	}
}

/******************* (C) COPYRIGHT 2007 STMicroelectronics *****END OF FILE****/

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
#include "USB_proto.h"
#include "HW.h"
#include "fifo.h"

#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#include "usb_lib.h"
#include "hw_config.h"
#include "usb_conf.h"
#include "usb_desc.h"
#include "usb_pwr.h"
#include "usb_cdc.h"
#define USB_DATA_SIZE		BULK_MAX_PACKET_SIZE

#if USB_TO_XXX_EN
#	include "USB_TO_XXX.h"
#endif

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
#define FWU_KEY				0x55AA
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
uint16 Vtarget = 0;

uint8 asyn_rx_buf[ASYN_DATA_BUFF_SIZE];		// used as temporary buff in In-System-Programming

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
uint8 Versaloon_Ver[] = "Versaloon(" _HARDWARE_VER_STR ")by Simon(compiled on " __DATE__ ")";

/**
	processor of general command of Versaloon
*/
void Versaloon_ProcessCommonCmd(uint8* dat, uint16 len)
{
#if VERSALOON_FW_UPDATE_EN
	uint32 key;
#endif

	switch(dat[0])
	{
	case VERSALOON_GET_INFO:
		dat[0] = USB_DATA_BUFF_SIZE & 0xFF;
		dat[1] = (USB_DATA_BUFF_SIZE >> 8) & 0xFF;
		memcpy(dat + 2, Versaloon_Ver, sizeof(Versaloon_Ver));
		rep_len = sizeof(Versaloon_Ver) + 2;
		break;
	case VERSALOON_GET_TVCC:
		dat[0] = Vtarget & 0xFF;
		dat[1] = (Vtarget >> 8) & 0xFF;
		rep_len = 2;
		break;
	case VERSALOON_GET_HARDWARE:
		dat[0] = _HARDWARE_VER;
		rep_len = 1;
		break;
#if VERSALOON_FW_UPDATE_EN
	case VERSALOON_FW_UPDATE:
		key = dat[1] + (dat[2] << 8);
		if(key == FWU_KEY)
		{
			RCC_APB1PeriphClockCmd(RCC_APB1Periph_BKP | RCC_APB1Periph_PWR, ENABLE);
			PWR_BackupAccessCmd(ENABLE);
			BKP_WriteBackupRegister(BKP_DR1, FWU_KEY);

			// Reset USB, and Reset MCU
			USB_Disable();

			USB_D_CLR();
			USB_D_SETOUTPUT();
			DelayMS(10);
			USB_D_SET();

			NVIC_SystemReset();
		}
		break;
#endif
	}
}

// GLOBAL_OUTPUT
uint8 GLOBAL_OUTPUT_Count = 0;
void GLOBAL_OUTPUT_Acquire(void)
{
	if(!GLOBAL_OUTPUT_Count)
	{
		GLOBAL_OUTPUT_ENABLE();
	}
	GLOBAL_OUTPUT_Count++;
}

void GLOBAL_OUTPUT_Release(void)
{
	if (GLOBAL_OUTPUT_Count)
	{
		GLOBAL_OUTPUT_Count--;
		if(!GLOBAL_OUTPUT_Count)
		{
			GLOBAL_OUTPUT_DISABLE();
		}
	}
}

void PWREXT_Check(uint8 b_control_led)
{
	static u32 dly = 0;

	if(++dly > 0xFFFF)
	{
		dly = 0;
#if POWER_SAMPLE_EN
		Vtarget = ADC_GetConversionValue(TVCC_ADC_PORT);
		// convert target power to be in mV unit
		Vtarget = Vtarget * TVCC_SAMPLE_VREF * TVCC_SAMPLE_DIV / TVCC_SAMPLE_MAXVAL;
#if POWER_OUT_EN
		// if PowerExt is enabled and no power on the line
		if((Vtarget < TVCC_SAMPLE_MIN_POWER))
		{
			// release power
			PWREXT_ForceRelease();
		}
#endif
		if (b_control_led)
		{
			// Control LED
			if(Vtarget > TVCC_SAMPLE_MIN_POWER)	// Red LED indicate the target power
			{
				LED_RED_ON();
			}
			else
			{
				LED_RED_OFF();
			}
		}
#endif
	}
}

int main(void)
{
	uint8 cmd = 0;

	Sys_Init();

	LED_GREEN_ON();
	LED_USB_ON();

	while(1)							// system main loop
	{
		if(CDC_enable)
		{
			// nothing to do
			while(CDC_enable)
			{
				CDC_Process();
				PWREXT_Check(0);
			}
		}
		else if(cmd_len & 0x80000000)
		{
			// A valid USB package has been received
			LED_USB_OFF();				// USB LED indicate the USB transmission

			cmd = buffer_out[0];		// first byte of the USB package is the command byte
			// check command and call corresponding module
			if(cmd <= VERSALOON_COMMON_CMD_END)
			{
				// Common Commands
				Versaloon_ProcessCommonCmd(buffer_out, cmd_len & 0xFFFF);
			}
#if USB_TO_XXX_EN
			else if(cmd <= VERSALOON_USB_TO_XXX_CMD_END)
			{
				buffer_reply = buffer_out;
				USB_TO_POLL_Index = -1;
				USB_TO_XXX_ProcessCmd(buffer_out, cmd_len & 0xFFFF);
			}
#endif		// #if USB_TO_XXX_EN
			if(rep_len > 0)
			{
				// indicate reply data is valie
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
			System_Idle_Loop();			// Idle loop
		}

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
		LED_USB_ON();					// command processed, light USB LED
	}
}

/**
	called when system is idle
*/
extern uint8 USBTOUSART_En;
void System_Idle_Loop(void)
{
	if (USBTOUSART_En)
	{
		CDC_Process();
	}
}

/******************* (C) COPYRIGHT 2007 STMicroelectronics *****END OF FILE****/

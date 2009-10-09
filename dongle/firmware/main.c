/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       main.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    main .c file                                              *
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
#if USB_PROTOCOL == USB_ST_VCOM
#	include "hw_config.h"
#	include "usb_conf.h"
#	include "usb_desc.h"
#	include "usb_pwr.h"
#	include "usb_cdc.h"
#elif (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
#	include "hw_config_at.h"
#	include "usb_conf_at.h"
#	include "usb_desc_at.h"
#	include "usb_pwr_at.h"
#	include "usb_cdc_at.h"
#endif
#define USB_DATA_SIZE		BULK_MAX_PACKET_SIZE

#if ATMEL_DEVICE_EN
#	include "AVR_Device_Common.h"
#endif
#if VSLLINK_EN
#	include "VSLLink.h"
#endif
#if USB_TO_XXX_EN
#	include "USB_TO_XXX.h"
#endif
#if MP_EN
#	include "MassProduct.h"
#endif

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
#define FWU_KEY				0x55AA
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
uint16 Vtarget = 0;
SPI_InitTypeDef		SPI_InitStructure;

uint8 asyn_rx_buf[ASYN_DATA_BUFF_SIZE];		// used as temporary buff in In-System-Programming
uint16 asyn_rx_idx, asyn_tx_idx;

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
uint8 Versaloon_Ver[] = "Versaloon(" _HARDWARE_VER_STR ")by Simon(compiled on " __DATE__ ")";

/**
	processor of general command of Versaloon
*/
void Versaloon_ProcessCommonCmd(uint8* dat, uint16 len)
{
#if VERSALOON_FW_UPDATE_EN || MP_EN
	uint32 address;
#endif
#if MP_EN
	uint32 length;
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
#if MP_EN
	case VERSALOON_GET_OFFLINE_SIZE:
		dat[0] = (VERSALOON_OFFLINE_SIZE >> 0) & 0xFF;
		dat[1] = (VERSALOON_OFFLINE_SIZE >> 8) & 0xFF;
		dat[2] = (VERSALOON_OFFLINE_SIZE >> 16) & 0xFF;
		dat[3] = (VERSALOON_OFFLINE_SIZE >> 24) & 0xFF;
		rep_len = 4;

		break;
	case VERSALOON_ERASE_OFFLINE_DATA:
		dat[0] = MP_Erase();
		rep_len = 1;

		break;
	case VERSALOON_WRITE_OFFLINE_DATA:
		length = (dat[1] << 0) + (dat[2] << 8);
		address = (dat[3] << 0) + (dat[4] << 8) + (dat[5] << 16) + (dat[6] << 24);

		dat[0] = MP_Write(address, (uint32 *)(dat + 7), length);
		rep_len = 1;
		break;
	case VERSALOON_GET_OFFLINE_CHECKSUM:
		address = MP_Checksum((dat[1] << 0) + (dat[2] << 8));
		dat[0] = (address >> 0) & 0xFF;
		dat[1] = (address >> 8) & 0xFF;
		rep_len = 2;

		break;
#endif
#if VERSALOON_FW_UPDATE_EN
	case VERSALOON_FW_UPDATE:
		address = dat[1] + (dat[2] << 8);
		if(address == FWU_KEY)
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
	GLOBAL_OUTPUT_Count--;
	if(!GLOBAL_OUTPUT_Count)
	{
		GLOBAL_OUTPUT_DISABLE();
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
			while(PWREXT_EnableCount)
			{
				PWREXT_Release();
			}
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
			LED_GREEN_OFF();			// Green LED indicate the USB transmission

			cmd = buffer_out[0];		// first byte of the USB package is the command byte
			// check command and call corresponding module
			if(cmd <= VERSALOON_COMMON_CMD_END)
			{
				// Common Commands
				Versaloon_ProcessCommonCmd(buffer_out, cmd_len & 0xFFFF);
			}
#if MCU_PROG_EN
			else if(cmd <= VERSALOON_MCU_CMD_END)
			{
#if ATMEL_DEVICE_EN
				uint16 len;
#endif
				// MCU Support
				switch(cmd)
				{
#if ATMEL_DEVICE_EN
				case AVR_CMD_CHAR:
					// first 8 bytes are protocol area, and last 2 bytes are CRC16
					// CRC16 is not checked, because there is already a CRC check on the USB SIE
					AVR_Device_ProcessCmd(buffer_out + 8, (cmd_len & 0xFFFF) - 10);

					// set reply length in the 8-byte protocol area
					buffer_out[3] = (uint8)(rep_len & 0xFFFF);
					buffer_out[4] = (uint8)((rep_len & 0xFFFF) >> 8);

					// Calc CRC16
					CRC_Val = CRC_INIT;
					for(len = 0; len < (rep_len & 0xFFFF) + 8; len++)
					{
						CRC16(buffer_out[len]);
					}
					// Append CRC16
					memcpy(buffer_out + (rep_len & 0xFFFF) + 8, (u8*)&CRC_Val, 2);

					rep_len += 10;		// 8 bytes protocol area and 2 bytes CRC16
					break;
#endif
				}
			}
#endif		// #if MCU_PROG_EN
#if USB_TO_XXX_EN
			else if(cmd <= VERSALOON_USB_TO_XXX_CMD_END)
			{
				USB_TO_XXX_ProcessCmd(buffer_out, cmd_len & 0xFFFF);
			}
#endif		// #if USB_TO_XXX_EN
#if VSLLINK_EN
			else //if(cmd <= VERSALOON_VSLLINK_CMD_END)
			{
				// JTAG Debugger Support
				VSLLink_ProcessCmd(buffer_out, cmd_len & 0xFFFF);
			}
#endif
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
#if ATMEL_DEVICE_EN
			// Idle, no command to process
			AVR_Device_Poll();			// handle AVR Idle process(for AVR_JTAG_OCD and AVR_DW_OCD use)
			if(rep_len > 0)				// there is data need to be send
			{
				// indicate reply data is valie
				rep_len |= 0x80000000;
			}
#endif

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
		LED_GREEN_ON();				// command processed, light green LED
	}
}

/**
	called when system is idle, process mass-product operation
*/
void System_Idle_Loop(void)
{
#if MP_EN
	static uint32 key_down_count = 0;
	uint8 ret;
#endif

#if MP_EN
	if(KEY_IsDown())						// Check key for Mass-Production operation
	{
		if(++key_down_count > 50000)		// key is down for long time
		{
			// mass-product mode
			LED_ALL_OFF();
			while(KEY_IsDown());			// Wait key up

			// wait key
		wait_key:
			key_down_count = 0;
			while(key_down_count <= 50000)
			{
				if(KEY_IsDown())
				{
					key_down_count++;
				}
				else
				{
					key_down_count = 0;
				}
			}
			LED_ALL_OFF();
			while(KEY_IsDown());

			// do the work
			ret = MP_Go();
			LED_ALL_OFF();					// LED may toggle to indicate programming process, turn them off
			if(ret > 0)						// If success, green LED will be on, otherwise, red LED will be on
			{
				LED_RED_ON();
			}
			else
			{
				LED_GREEN_ON();
			}
			goto wait_key;
		}
	}
	else
	{
		key_down_count = 0;
	}
#endif
}

/******************* (C) COPYRIGHT 2007 STMicroelectronics *****END OF FILE****/

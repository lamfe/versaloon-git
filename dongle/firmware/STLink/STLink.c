/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STLink                                                    *
 *  File:       STLink.c                                                  *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for STLink STM8 protocol                   *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-01:     created(by SimonQian)                             *
 **************************************************************************/
#include "app_cfg.h"

#include "STLink.h"
#include "SWIM.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#define STLINK_CMD_READSERN				0xF1
#define STLINK_CMD_0XF2					0xF2
#define STLINK_CMD_0XF3					0xF3
#define STLINK_CMD_SWIM					0xF4
#define STLINK_CMD_0XF5					0xF5

#define STLINK_SUBCMD_INIT				0x00
#define STLINK_SUBCMD_FINI				0x01
#define STLINK_SUBCMD_0X02				0x02
#define STLINK_SUBCMD_SPEED				0x03
#define STLINK_SUBCMD_SWIM_ENTRY		0x04
#define STLINK_SUBCMD_SWIM_SRST			0x05
#define STLINK_SUBCMD_SWIM_SYNC			0x06
#define STLINK_SUBCMD_RESET_ON			0x07
#define STLINK_SUBCMD_RESET_OFF			0x08
#define STLINK_SUBCMD_POLL				0x09
#define STLINK_SUBCMD_SWIM_WOTF			0x0A
#define STLINK_SUBCMD_SWIM_ROTF			0x0B
#define STLINK_SUBCMD_GET_RESULT		0x0C
#define STLINK_SUBCMD_GET_BUFSIZE		0x0D

#define STLINK_STATE_READY				0x00
#define STLINK_STATE_BUSY				0x01
#define STLINK_STATE_TIMEOUT			0x04
#define STLINK_STATE_FAIL_ENTRY			0x09

static const uint8_t STLink_SerNum[] = {0x11, 0x43, 0x83, 0x04, 0x44, 0x37};
static uint8_t STLink_SWIM_Speed = 0;
static uint8_t STLink_state = STLINK_STATE_READY;
static uint32_t STLink_process_queue_len = 0;
static uint16_t STLink_DataPos = 0;
static uint8_t STLink_target_clock = 8;

static void STLink_Reset_On(void)
{
	SW_CLR();
	SW_SETOUTPUT();
}

static void STLink_Reset_Off(void)
{
	SW_SETINPUT_PU();
}

static uint8_t STLink_SetSWIMSpeed(uint8_t speed)
{
	uint8_t cnt0, cnt1;

	if (speed)
	{
		// high speed
		cnt0 = 8;
		cnt1 = 2;
	}
	else
	{
		// low speed
		cnt0 = 20;
		cnt1 = 2;
	}
	return SWIM_SetClockParam(STLink_target_clock, cnt0, cnt1);
}

void STLink_SWIM_Process(uint8_t *cmd)
{
	uint16_t byte_num;
	uint32_t addr;
	
	switch (cmd[0])
	{
	case STLINK_SUBCMD_SWIM_ENTRY:
		SWIM_EnableClockInput();
		if (SWIM_EnterProgMode())
		{
			STLink_state = STLINK_STATE_FAIL_ENTRY;
		}
		else
		{
			STLink_state = STLINK_STATE_READY;
		}

		SWIM_Init();
		break;
	case STLINK_SUBCMD_SWIM_SRST:
		if (SWIM_SRST())
		{
			STLink_state = STLINK_STATE_TIMEOUT;
		}
		else
		{
			STLink_state = STLINK_STATE_READY;
		}
		break;
	case STLINK_SUBCMD_SWIM_SYNC:
		if (SWIM_Sync(STLink_target_clock) || STLink_SetSWIMSpeed(STLink_SWIM_Speed))
		{
			STLink_state = STLINK_STATE_TIMEOUT;
		}
		else
		{
			STLink_state = STLINK_STATE_READY;
		}
		break;
	case STLINK_SUBCMD_SWIM_WOTF:
		byte_num = GET_BE_U16(&cmd[1]);
		addr = GET_BE_U32(&cmd[3]);

		if (SWIM_WOTF(addr, byte_num, &cmd[7]))
		{
			STLink_state = STLINK_STATE_TIMEOUT;
		}
		else
		{
			STLink_process_queue_len = byte_num;
			STLink_state = STLINK_STATE_READY;
		}
		break;
	case STLINK_SUBCMD_SWIM_ROTF:
		byte_num = GET_BE_U16(&cmd[1]);
		addr = GET_BE_U32(&cmd[3]);

		if (SWIM_ROTF(addr, byte_num, &cmd[7]))
		{
			STLink_state = STLINK_STATE_TIMEOUT;
		}
		else
		{
			STLink_process_queue_len = byte_num;
			STLink_state = STLINK_STATE_READY;
		}
		break;
	}
}



#include "hw_config.h"
#include "usb_bot.h"
#include "usb_scsi.h"
#include "usb_regs.h"
#include "usb_mem.h"


extern uint8_t Bulk_Data_Buff[BULK_MAX_PACKET_SIZE];
extern uint8_t Bot_State;
extern uint16_t Data_Len;
extern Bulk_Only_CBW CBW;
extern Bulk_Only_CSW CSW;

void STLink_SCSI_Process(uint8_t *cmd)
{
	uint16_t temp;
	uint16_t byte_num;
	
	switch (cmd[0])
	{
	case STLINK_CMD_READSERN:
		Transfer_Data_Request((uint8_t*)STLink_SerNum, (uint16_t)sizeof(STLink_SerNum));
		break;
	case STLINK_CMD_0XF2:
		Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
		break;
	case STLINK_CMD_0XF3:
		Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
		break;
	case STLINK_CMD_SWIM:
		switch (cmd[1])
		{
		case STLINK_SUBCMD_INIT:
			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(20);

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_FINI:
			SWIM_Fini();
			SW_SETINPUT();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_0X02:
			{
				uint8 buff[] = {0x00, 0x01, 0x01, 0x03, 0x00, 0x00, 0x00, 0x00};
				Transfer_Data_Request((uint8_t*)buff, (uint16_t)sizeof(buff));
			}
			break;
		case STLINK_SUBCMD_SPEED:
			STLink_SWIM_Speed = cmd[2];
			if (STLink_SetSWIMSpeed(STLink_SWIM_Speed))
			{
				STLink_state = STLINK_STATE_TIMEOUT;
			}

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_SWIM_ENTRY:
#if 0
			buffer_out[0] = cmd[1];
			cmd_len = 0x80000000 | 1;
			STLink_state = STLINK_STATE_BUSY;
#else
			SWIM_EnableClockInput();
			if (SWIM_EnterProgMode())
			{
				STLink_state = STLINK_STATE_FAIL_ENTRY;
			}
			else
			{
				STLink_state = STLINK_STATE_READY;
			}

			SWIM_Init();
#endif

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_SWIM_SRST:
			buffer_out[0] = cmd[1];
			cmd_len = 0x80000000 | 1;
			STLink_state = STLINK_STATE_BUSY;

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_SWIM_SYNC:
			buffer_out[0] = cmd[1];
			cmd_len = 0x80000000 | 1;
			STLink_state = STLINK_STATE_BUSY;

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_SWIM_WOTF:
			STLink_process_queue_len = 0;
			byte_num = (cmd[2] << 8) + cmd[3];

			if (Bot_State == BOT_IDLE)
			{
				if (byte_num <= 8)
				{
					STLink_DataPos = 0;
					memcpy(buffer_out, &cmd[1], byte_num + 7);
					cmd_len = 0x80000000 | (byte_num + 7);
					Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
				}
				else
				{
					if ((CBW.bmFlags & 0x80) == 0)
					{
						STLink_DataPos = 15;
						memcpy(buffer_out, &cmd[1], 15);
						Bot_State = BOT_DATA_OUT;
						SetEPRxStatus(ENDP4, EP_RX_VALID);
					}
					else
					{
						Bot_Abort(DIR_IN);
						Set_Scsi_Sense_Data(CBW.bLUN, ILLEGAL_REQUEST, INVALID_FIELED_IN_COMMAND);
						Set_CSW (CSW_CMD_FAILED, SEND_CSW_DISABLE);
					}
				}
			}
			else if (Bot_State == BOT_DATA_OUT)
			{
				memcpy(buffer_out + STLink_DataPos, Bulk_Data_Buff, Data_Len);
				STLink_DataPos += Data_Len;
				if (STLink_DataPos >= (byte_num + 7))
				{
					STLink_DataPos = 0;
					cmd_len = 0x80000000 | (byte_num + 7);
					Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
				}
				else
				{
					SetEPRxStatus(ENDP4, EP_RX_VALID);
				}
			}
			STLink_state = STLINK_STATE_BUSY;
			break;
		case STLINK_SUBCMD_SWIM_ROTF:
			STLink_process_queue_len = 0;

			memcpy(buffer_out, &cmd[1], 7);
			cmd_len = 0x80000000 | 7;
			STLink_state = STLINK_STATE_BUSY;

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_RESET_ON:
			STLink_Reset_On();
			STLink_state = STLINK_STATE_READY;

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_RESET_OFF:
			STLink_Reset_Off();
			STLink_state = STLINK_STATE_READY;

			Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
			break;
		case STLINK_SUBCMD_POLL:
			{
				uint8 buff[4];
				buff[0] = STLink_state;
				buff[1] = (STLink_process_queue_len >> 0) &0xFF;
				buff[2] = (STLink_process_queue_len >> 8) &0xFF;
				buff[3] = (STLink_process_queue_len >> 16) &0xFF;
				Transfer_Data_Request((uint8_t*)buff, (uint16_t)sizeof(buff));
			}
			break;
		case STLINK_SUBCMD_GET_RESULT:
			if (Bot_State == BOT_IDLE)
			{
				if ((CBW.bmFlags & 0x80) != 0)
				{
					if (STLink_process_queue_len > BULK_MAX_PACKET_SIZE)
					{
						Bot_State = BOT_DATA_IN;
						UserToPMABufferCopy(&buffer_out[7], ENDP4_TXADDR, BULK_MAX_PACKET_SIZE);
						SetEPTxCount(ENDP4, BULK_MAX_PACKET_SIZE);
						STLink_DataPos = BULK_MAX_PACKET_SIZE;
						STLink_process_queue_len -= BULK_MAX_PACKET_SIZE;
					}
					else
					{
						UserToPMABufferCopy(&buffer_out[7], ENDP4_TXADDR, STLink_process_queue_len);
						SetEPTxCount(ENDP4, STLink_process_queue_len);
						STLink_process_queue_len = 0;
						STLink_DataPos = 0;
						Bot_State = BOT_DATA_IN_LAST;
					}
					SetEPTxStatus(ENDP4, EP_TX_VALID);
				}
				else
				{
					Bot_Abort(BOTH_DIR);
					Set_Scsi_Sense_Data(CBW.bLUN, ILLEGAL_REQUEST, INVALID_FIELED_IN_COMMAND);
					Set_CSW (CSW_CMD_FAILED, SEND_CSW_ENABLE);
				}
			}
			else if (Bot_State == BOT_DATA_IN)
			{
				if (STLink_process_queue_len > BULK_MAX_PACKET_SIZE)
				{
					UserToPMABufferCopy(&buffer_out[7 + STLink_DataPos], ENDP4_TXADDR, BULK_MAX_PACKET_SIZE);
					SetEPTxCount(ENDP4, BULK_MAX_PACKET_SIZE);
					STLink_DataPos += BULK_MAX_PACKET_SIZE;
					STLink_process_queue_len -= BULK_MAX_PACKET_SIZE;
				}
				else
				{
					UserToPMABufferCopy(&buffer_out[7 + STLink_DataPos], ENDP4_TXADDR, STLink_process_queue_len);
					SetEPTxCount(ENDP4, STLink_process_queue_len);
					STLink_process_queue_len = 0;
					STLink_DataPos = 0;
					Bot_State = BOT_DATA_IN_LAST;
				}
				SetEPTxStatus(ENDP4, EP_TX_VALID);
			}
			break;
		case STLINK_SUBCMD_GET_BUFSIZE:
			temp = MSD_BUFF_SIZE_IN_DWORD * 4;
			Transfer_Data_Request((uint8_t*)&temp, 2);
			break;
		}
		break;
	case STLINK_CMD_0XF5:
		temp = 0;
		Transfer_Data_Request((uint8_t*)&temp, 2);
		break;
	}
}

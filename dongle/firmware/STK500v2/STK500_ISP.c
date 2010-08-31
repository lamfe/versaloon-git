/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500_ISP.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for STK500 ISP                            *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/
#include "app_cfg.h"

#include "STK500_Const.h"
#include "STK500.h"

#include "SPI.h"
#include "Target/AVR_ISP/AVR_ISP.h"

/// SPI Command data
uint8 AVRISP_SPI_cmd[4];
/// SPI Return data
uint8 AVRISP_SPI_ret[4];

/// ISP Frequency Array
const static uint32 AVRISP_Freqs[] = {
	8000000, 4000000, 2000000, 1000000, 500000, 250000, 125000,
	96386, 89888, 84211, 79208, 74767, 70797, 67227, 64000,
	61069, 58395, 55945, 51613, 49690, 47905, 46243, 43244,
	41885, 39409, 38278, 36200, 34335, 32654, 31129, 29740,
	28470, 27304, 25724, 24768, 23461, 22285, 21221, 20254,
	19371, 18562, 17583, 16914, 16097, 15356, 14520, 13914,
	13224, 12599, 12031, 11511, 10944, 10431, 9963, 9468,
	9081, 8612, 8239, 7851, 7498, 7137, 6809, 6478, 6178,
	5879, 5607, 5359, 5093, 4870, 4633, 4418, 4209, 4019,
	3823, 3645, 3474, 3310, 3161, 3011, 2869, 2734, 2611,
	2484, 2369, 2257, 2152, 2052, 1956, 1866, 1779, 1695,
	1615, 1539, 1468, 1398, 1333, 1271, 1212, 1155, 1101,
	1049, 1000, 953, 909, 866, 826, 787, 750, 715, 682,
	650, 619, 590, 563, 536, 511, 487, 465, 443, 422,
	402, 384, 366, 349, 332, 317, 302, 288, 274, 261,
	249, 238, 226, 216, 206, 196, 187, 178, 170, 162,
	154, 147, 140, 134, 128, 122, 116, 111, 105, 100,
	95, 91, 87, 83, 79, 75, 72, 68, 65, 62, 59, 56, 54, 51
};

/// ISP Programming for AVR under STK500 Protocol
/// @param[in]	dat		Command Array
/// @param[in]	len		Command Length
/// @return		Command Result
uint8 STK500_ISP_ProcessProgCmd(uint8* dat, uint16 len)
{
	uint16 i;
	uint16 tmp16, length16;
	uint8 operation = 0, command, command_tmp, delay;

	switch(dat[0] & ~STK500_CMDTYPE_MASK)
	{
	case CMD_ENTER_PROGMODE:
//dat[idx]	para(value)				Description
//dat[1]	timeout					Command timeout(in ms)
//dat[2]	stabDelay				Delay(in ms) used for pin stabilization
//dat[3]	cmdexeDelay				Delay(in ms) in connection with the EnterProgMode command execution
//dat[4]	synchLoops				Number of synchronization loops
//dat[5]	byteDelay				Delay(in ms) between each byte in the EnterProgMode command
//dat[6]	pollValue				Poll value:0x53 for AVR,0x69 for AT89xx
//dat[7]	pollIndex				Start address of received byte:0 = no polling,3 = AVR,4 = AT89xx
//dat[8]	cmd1
//dat[9]	cmd2
//dat[10]	cmd3
//dat[11]	cmd4

		if(STK500_PARAM_SCK_Duration >= (sizeof(AVRISP_Freqs) / sizeof(AVRISP_Freqs[0])))
		{
			STK500_PARAM_SCK_Duration = (sizeof(AVRISP_Freqs) / sizeof(AVRISP_Freqs[0])) - 1;
		}

		AVRISP_RST_On();
		AVRISP_Init(AVRISP_Freqs[STK500_PARAM_SCK_Duration] / 4);

		// Atmel give a long delay,could be shorter
		DelayMS(1);
		AVRISP_RST_Off();
		DelayMS(1);
		AVRISP_RST_On();

		DelayMS(dat[3]);

		for(delay = 0; delay < 4; delay++)
		{
			AVRISP_SPI_ret[delay] = SPI_RW(dat[8 + delay]);
// NOT necessary to delay this delay
//			DelayMS(dat[5]);
		}

		if((dat[7] > 0) && (AVRISP_SPI_ret[dat[7] - 1] != dat[6]))
		{
			// poll fails
			rep_len = 2;
			return STATUS_CMD_FAILED;
		}

		rep_len = 2;
		return STATUS_CMD_OK;
	case CMD_LEAVE_PROGMODE:
//dat[idx]	para(value)				Description
//dat[1]	preDelay				Pre-delay(in ms)
//dat[2]	postDelay				Post-delay(in ms)

		DelayMS(20);		// Wait last operation ready
//		AVRISP_RDY_Wait();

		AVRISP_RST_Off();
		DelayMS(dat[1]);
		AVRISP_Fini();
		DelayMS(dat[2]);

		rep_len = 2;
		return STATUS_CMD_OK;
	case CMD_CHIP_ERASE:
//dat[idx]	para(value)				Description
//dat[1]	eraseDelay				Delay(in ms) to ensure that the erase of the device is finished
//dat[2]	pollMethod				Poll method,0 = use delay,1 = use RDY/BSY command
//dat[3]	cmd1
//dat[4]	cmd2
//dat[5]	cmd3
//dat[6]	cmd4

		if(AVRISP_RDY_Wait())
		{
			rep_len = 2;
			return STATUS_RDY_BSY_TOUT;
		}

		AVRISP_Comm(&dat[3], (uint8*)AVRISP_SPI_ret);

		DelayMS(dat[1]);
		if(dat[2])
		{
			if(AVRISP_RDY_Wait())
			{
				rep_len = 2;
				return STATUS_RDY_BSY_TOUT;
			}
		}
//		else
//		{
//			DelayMS(dat[1]);
//		}

		rep_len = 2;
		return STATUS_CMD_OK;
	case CMD_PROGRAM_FLASH:
//dat[idx]	para(value)				Description
//dat[1]	NumBytes(MSB)			Total number of bytes to program(MSB)
//dat[2]	NumBytes(LSB)			Total number of bytes to program(LSB)
//dat[3]	mode					Mode byte
//dat[4]	delay					Delay used for different types of programming termination,according to mode byte
//dat[5]	cmd1					Comamnd byte for Load Page
//dat[6]	cmd2					Command byte for Write Memory
//dat[7]	cmd3					Command byte for Read Memory
//dat[8]	poll1					
//dat[9]	poll2					
//data[10]	Data					Data to program
		operation = dat[3];
		command = dat[5];
		command_tmp = dat[7];
		length16 = ((uint16)dat[1] << 8) | dat[2];

		if(AVRISP_RDY_Wait())
		{
			rep_len = 2;
			return STATUS_RDY_BSY_TOUT;
		}

		if(operation & 0x01)
		{
			// Page Mode
			length16 >>= 1;

			tmp16 = SKT500_Target_Address;

			for(i = 0; i < length16; i++)
			{
				AVRISP_SPI_cmd[0] = command;
				AVRISP_SPI_cmd[1] = SKT500_Target_Address >> 8;
				AVRISP_SPI_cmd[2] = SKT500_Target_Address;

				AVRISP_SPI_cmd[3] = dat[10 + i * 2];
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				AVRISP_SPI_cmd[0] = command | 0x08;
				AVRISP_SPI_cmd[3] = dat[11 + i * 2];
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				SKT500_Target_Address++;
			}
			if(operation & 0x80)
			{
				// Write Flash Page
				AVRISP_SPI_cmd[0] = dat[6];
				AVRISP_SPI_cmd[1] = tmp16 >> 8;
				AVRISP_SPI_cmd[2] = tmp16;
				AVRISP_SPI_cmd[3] = 0;
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				if(operation & 0x20)
				{
					// Value Polling
					operation = dat[8];		// Flash use dat[8]
					if(dat[10] != operation)
					{
						delay = 255;
						do
						{
							DelayUS(80);
							AVRISP_SPI_cmd[0] = command_tmp;
							AVRISP_SPI_cmd[1] = tmp16 >> 8;
							AVRISP_SPI_cmd[2] = tmp16;
							AVRISP_SPI_cmd[3] = 0;
							AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
						}while((AVRISP_SPI_ret[3] == operation) && --delay);
						if(!delay | (AVRISP_SPI_ret[3] != dat[10]))
						{
							rep_len = 2;
							return STATUS_RDY_BSY_TOUT;
						}
					}
					else
					{
						DelayMS(dat[4]);
					}
				}
				else if(operation & 0x40)
				{
					// RDY/BSY Polling
//					if(AVRISP_RDY_Wait())
//					{
//						rep_len = 2;
//						return STATUS_RDY_BSY_TOUT;
//					}
				}
				else
				{
					// Delay
					DelayMS(dat[4]);
				}
			}
		}
		else
		{
			// Non Page Mode
			for(i = 0; i < length16; i++)
			{
				if(i & 1)
				{
					AVRISP_SPI_cmd[0] = command | 0x08;
				}
				else
				{
					AVRISP_SPI_cmd[0] = command;
				}
				AVRISP_SPI_cmd[1] = SKT500_Target_Address >> 8;
				AVRISP_SPI_cmd[2] = SKT500_Target_Address;
				AVRISP_SPI_cmd[3] = dat[10 + i];
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				// MUST Full Polling
				if(operation & 0x04)
				{
					// Value Polling
					operation = dat[8];		// Flash use dat[8]
					if(dat[10 + i] != operation)
					{
						delay = 255;
						do
						{
							DelayUS(80);
							if(i & 1)
							{
								AVRISP_SPI_cmd[0] = command_tmp | 0x08;
							}
							else
							{
								AVRISP_SPI_cmd[0] = command_tmp;
							}
							AVRISP_SPI_cmd[3] = 0;
							AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
						}while((AVRISP_SPI_ret[3] == operation) && --delay);
						if(!delay | (AVRISP_SPI_ret[3] != dat[10 + i]))
						{
							rep_len = 2;
							return STATUS_RDY_BSY_TOUT;
						}
					}
					else
					{
						DelayMS(dat[4]);
					}
				}
				else if(operation & 0x08)
				{
					// RDY/BSY Polling
					if(AVRISP_RDY_Wait())
					{
						rep_len = 2;
						return STATUS_RDY_BSY_TOUT;
					}
				}
				else
				{
					// Delay
					DelayMS(dat[4]);
				}

				if(i & 1)
				{
					SKT500_Target_Address++;
				}
			}
		}

		rep_len = 2;
		return STATUS_CMD_OK;
	case CMD_READ_FLASH:
//dat[idx]	para(value)				Description
//dat[1]	NumBytes(MSB)			Total number of bytes to read(MSB)
//dat[2]	NumBytes(LSB)			Total number of bytes to read(LSB)
//dat[3]	cmd1					Command for Read Memory
		length16 = ((uint16)dat[1] << 8) | dat[2];
		command = dat[3];

		length16 >>= 1;
		AVRISP_RDY_Wait();
		for(i = 0; i < length16; i++)
		{
			AVRISP_SPI_cmd[0] = command;
			AVRISP_SPI_cmd[1] = SKT500_Target_Address >> 8;
			AVRISP_SPI_cmd[2] = SKT500_Target_Address;
			AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
			dat[2 + i * 2] = AVRISP_SPI_ret[3];

			AVRISP_SPI_cmd[0] = command | 0x08;
			AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
			dat[3 + i * 2] = AVRISP_SPI_ret[3];

			SKT500_Target_Address++;
		}
		dat[2 + length16 * 2] = STATUS_CMD_OK;
		rep_len = 3 + length16 * 2;
		return STATUS_CMD_OK;
	case CMD_PROGRAM_EEPROM:
//dat[idx]	para(value)				Description
//same as CMD_PROGRAM_FLASH
		length16 = ((uint16)dat[1] << 8) | dat[2];
		operation = dat[3];
		command = dat[5];
		command_tmp = dat[7];

		if(AVRISP_RDY_Wait())
		{
			rep_len = 2;
			return STATUS_RDY_BSY_TOUT;
		}

		if(operation & 0x01)
		{
			// Page Mode
			tmp16 = SKT500_Target_Address;

			for(i = 0; i < length16; i++)
			{
				AVRISP_SPI_cmd[0] = command;
				AVRISP_SPI_cmd[1] = SKT500_Target_Address >> 8;
				AVRISP_SPI_cmd[2] = SKT500_Target_Address;
				AVRISP_SPI_cmd[3] = dat[10 + i];
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				SKT500_Target_Address++;
			}
			if(operation & 0x80)
			{
				// Write Flash Page
				AVRISP_SPI_cmd[0] = dat[6];
				AVRISP_SPI_cmd[1] = tmp16 >> 8;
				AVRISP_SPI_cmd[2] = tmp16;
				AVRISP_SPI_cmd[3] = 0;
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				// Polling
				if(operation & 0x20)
				{
					// Value Polling
					operation = dat[9];		// EEPROM use dat[9]
					if(dat[10] != operation)
					{
						delay = 255;
						do
						{
							DelayUS(80);
							AVRISP_SPI_cmd[0] = command_tmp;
							AVRISP_SPI_cmd[1] = tmp16 >> 8;
							AVRISP_SPI_cmd[2] = tmp16;
							AVRISP_SPI_cmd[3] = 0;
							AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
						}while((AVRISP_SPI_ret[3] == operation) && --delay);
						if(!delay | (AVRISP_SPI_ret[3] != dat[10]))
						{
							rep_len = 2;
							return STATUS_RDY_BSY_TOUT;
						}
					}
					else
					{
						DelayMS(dat[4]);
					}
				}
				else if(operation & 0x40)
				{
					// RDY/BSY Polling
//					if(AVRISP_RDY_Wait())
//					{
//						rep_len = 2;
//						return STATUS_RDY_BSY_TOUT;
//					}
				}
				else
				{
					// Delay
					DelayMS(dat[4]);
				}

			}
		}
		else
		{
			// Non Page Mode
			for(i = 0; i < length16; i++)
			{
				AVRISP_SPI_cmd[0] = command;
				AVRISP_SPI_cmd[1] = SKT500_Target_Address >> 8;
				AVRISP_SPI_cmd[2] = SKT500_Target_Address;
				AVRISP_SPI_cmd[3] = dat[10 + i];
				AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);

				// MUST Full Polling
				if(operation & 0x04)
				{
					// Value Polling
					operation = dat[9];		// EEPROM use dat[9]
					if(dat[10 + i] != operation)
					{
						delay = 255;
						do
						{
							DelayUS(80);
							AVRISP_SPI_cmd[0] = command_tmp;
							AVRISP_SPI_cmd[3] = 0;
							AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
						}while((AVRISP_SPI_ret[3] == operation) && --delay);
						if(!delay | (AVRISP_SPI_ret[3] != dat[10 + i]))
						{
							rep_len = 2;
							return STATUS_RDY_BSY_TOUT;
						}
					}
					else
					{
						DelayMS(dat[4]);
					}
				}
				else if(operation & 0x08)
				{
					// RDY/BSY Polling
					if(AVRISP_RDY_Wait())
					{
						rep_len = 2;
						return STATUS_RDY_BSY_TOUT;
					}
				}
				else
				{
					// Delay
					DelayMS(dat[4]);
				}

				SKT500_Target_Address++;
			}
		}

		rep_len = 2;
		return STATUS_CMD_OK;
	case CMD_READ_EEPROM:
//dat[idx]	para(value)				Description
//same as CMD_READ_FLASH
		length16 = ((uint16)dat[1] << 8) | dat[2];
		command = dat[3];

		AVRISP_RDY_Wait();
		for(i = 0; i < length16; i++)
		{
			AVRISP_SPI_cmd[0] = command;
			AVRISP_SPI_cmd[1] = SKT500_Target_Address >> 8;
			AVRISP_SPI_cmd[2] = SKT500_Target_Address;
			AVRISP_Comm((uint8*)AVRISP_SPI_cmd, (uint8*)AVRISP_SPI_ret);
			dat[2 + i] = AVRISP_SPI_ret[3];

			SKT500_Target_Address++;
		}
		dat[2 + length16] = STATUS_CMD_OK;
		rep_len = 3 + length16;
		return STATUS_CMD_OK;
	case CMD_PROGRAM_FUSE:
	case CMD_PROGRAM_LOCK:
//dat[idx]	para(value)				Description
//dat[1]	cmd1
//dat[2]	cmd2
//dat[3]	cmd3
//dat[4]	cmd4
		AVRISP_RDY_Wait();

		AVRISP_Comm(&dat[1], (uint8*)AVRISP_SPI_ret);

		dat[2] = STATUS_CMD_OK;
		rep_len = 3;
		return STATUS_CMD_OK;
	case CMD_READ_FUSE:
	case CMD_READ_LOCK:
	case CMD_READ_SIGNATURE:
	case CMD_READ_OSCCAL:
//dat[idx]	para(value)				Description
//dat[1]	RetAddr					Return address
//dat[2]	cmd1
//dat[3]	cmd2
//dat[4]	cmd3
//dat[5]	cmd4
		AVRISP_RDY_Wait();

		AVRISP_Comm(dat + 2, (uint8*)AVRISP_SPI_ret);
		dat[2] = AVRISP_SPI_ret[dat[1] - 1];

		dat[3] = STATUS_CMD_OK;
		rep_len = 4;
		return STATUS_CMD_OK;
	case CMD_SPI_MULTI:
//dat[idx]	para(value)				Description
//dat[1]	NumTx					Number of bytes to transmit
//dat[2]	NumRx					Number of bytes to receive
//dat[3]	RxStartAddr				Start address of returned data.Specifies on what transmitted byte the response is to be stored and returned
//dat[4]..	TxData					The data to be transmitted.The size is specified by NumTx

		operation = dat[1];	// Tx Len
		delay = dat[2];		// Rx Len
		command = dat[3];	// StartAddr

		length16 = (delay + command) > operation ? delay + command : operation;
		tmp16 = 0;

		for(i = 0; i < length16; i++)
		{
			if(i < operation)
			{
				command_tmp = SPI_RW(dat[4 + i]);
			}
			else
			{
				command_tmp = SPI_RW(0);
			}

			if((i >= command) && (tmp16 < delay))
			{
				dat[tmp16++ + 2] = command_tmp;
			}
		}

		dat[2 + delay] = STATUS_CMD_OK;
		rep_len = delay + 3;
		return STATUS_CMD_OK;
	default:
		rep_len = 2;
		return STATUS_CMD_UNKNOWN;
	}
}

/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       VSLLink.c                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    VSLLink implementation file                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "VSLLink.h"

#include "JTAG_TAP.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#if VSLLINK_EN

#define VSLLink_Wait_IR_DR_Prepare()		

uint8 VSLLink_Ver[] = "VSLLink_20080905 by Simon(compiled on " __DATE__ ")";

static void VSLLink_SetPortDir(uint8 msk, uint8 dir)
{
	if(msk & JTAG_PINMSK_TRST)
	{
		if(dir & JTAG_PINMSK_TRST)
		{
			JTAG_TAP_TRST_SETOUTPUT();
		}
		else
		{
			JTAG_TAP_TRST_SETINPUT();
		}
	}

#if JTAG_HAS_USER_PIN
	if(msk & JTAG_PINMSK_USR1)
	{
		if(dir & JTAG_PINMSK_USR1)
		{
			JTAG_TAP_USR1_SETOUTPUT();
		}
		else
		{
			JTAG_TAP_USR1_SETINPUT();
		}
	}

	if(msk & JTAG_PINMSK_USR2)
	{
		if(dir & JTAG_PINMSK_USR2)
		{
			// output not supported
		}
		else
		{
			JTAG_TAP_USR2_SETINPUT();
		}
	}
#endif
}

static void VSLLink_SetPort(uint8 msk, uint8 val)
{
	if(msk & JTAG_PINMSK_SRST)
	{
		if(val & JTAG_PINMSK_SRST)
		{
//			JTAG_TAP_SRST_SET();
//			JTAG_TAP_SRST_SETOUTPUT();
			JTAG_TAP_SRST_SETINPUT();
		}
		else
		{
			JTAG_TAP_SRST_CLR();
			JTAG_TAP_SRST_SETOUTPUT();
		}
	}

	if(msk & JTAG_PINMSK_TRST)
	{
		if(val & JTAG_PINMSK_TRST)
		{
			JTAG_TAP_TRST_SET();
		}
		else
		{
			JTAG_TAP_TRST_CLR();
		}
	}

#if JTAG_HAS_USER_PIN
	if(msk & JTAG_PINMSK_USR1)
	{
		if(val & JTAG_PINMSK_USR1)
		{
			JTAG_TAP_USR1_SET();
		}
		else
		{
			JTAG_TAP_USR1_CLR();
		}
	}
#endif
}

static uint8 VSLLink_GetPort(uint8 msk)
{
	uint8 ret = 0;

	if(msk & JTAG_PINMSK_SRST)
	{
		if(JTAG_TAP_SRST_GET())
		{
			ret |= JTAG_PINMSK_SRST;
		}
	}

	if(msk & JTAG_PINMSK_TRST)
	{
		if(JTAG_TAP_TRST_GET())
		{
			ret |= JTAG_PINMSK_TRST;
		}
	}

#if JTAG_HAS_USER_PIN
	if(msk & JTAG_PINMSK_USR1)
	{
		if(JTAG_TAP_USR1_GET())
		{
			ret |= JTAG_PINMSK_USR1;
		}
	}

	if(msk & JTAG_PINMSK_USR2)
	{
		if(JTAG_TAP_USR2_GET())
		{
			ret |= JTAG_PINMSK_USR2;
		}
	}
#endif

	return ret;
}

void VSLLink_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 length, cur_cmd_pos;
	uint32 cur_dat_len;
	uint8 cur_cmd;
	uint8 port_msk, port_value;
#if VSLLINK_HL_EN
	uint8 poll_buf[32], tmp8;
#endif

	switch(dat[0])
	{
	case VSLLINK_CMD_CONN:
		GLOBAL_OUTPUT_Acquire();
		PWREXT_Acquire();
		DelayMS(1);

		JTAG_TAP_HS_Fini();
		JTAG_TAP_HS_Init(JTAG_TAP_HS_MIN_SPEED, dat[1] & 1);

		dat[0] = USB_DATA_BUFF_SIZE & 0xFF;
		dat[1] = (USB_DATA_BUFF_SIZE >> 8) & 0xFF;
		memcpy(dat + 2, VSLLink_Ver, sizeof(VSLLink_Ver));
		rep_len = sizeof(VSLLink_Ver) + 2;
		break;
	case VSLLINK_CMD_DISCONN:
		JTAG_TAP_HS_Fini();
		VSLLink_SetPortDir(0xFF, 0x00);

		PWREXT_Release();
		GLOBAL_OUTPUT_Release();
		break;
	case VSLLINK_CMD_SET_SPEED:
		length = dat[1] | (dat[2] << 8);

		JTAG_TAP_HS_SetTCKFreq(length);
		break;
	case VSLLINK_CMD_SET_PORT:
		port_msk = dat[1];
		port_value = dat[2];

		VSLLink_SetPort(port_msk, port_value);
		break;
	case VSLLINK_CMD_GET_PORT:
		port_msk = dat[1];
		port_value = VSLLink_GetPort(port_msk);

		dat[0] = port_value;
		rep_len = 1;
		break;
	case VSLLINK_CMD_SET_PORTDIR:
		port_msk = dat[1];
		port_value = dat[2];

		VSLLink_SetPortDir(port_msk, port_value);
		break;
#if VSLLINK_HL_EN
	case VSLLINK_CMD_JTAGHL_SET_DAISYCHAIN_POS:
		JTAG_TAP_SetDaisyChainPos(dat[1], dat[2], dat[3] + (dat[4] << 8), dat[5] + (dat[6] << 8));
		break;
#endif
	case VSLLINK_CMD_HW_JTAGSEQCMD:
		cur_cmd_pos = 3;

		while(cur_cmd_pos < len)
		{
			cur_cmd = dat[cur_cmd_pos];
			cur_cmd_pos += 1;

			switch(cur_cmd & VSLLINK_CMDJTAGSEQ_CMDMSK)
			{
			case VSLLINK_CMDJTAGSEQ_TMSBYTE:
				cur_dat_len = (cur_cmd & VSLLINK_CMDJTAGSEQ_LENMSK) + 1;

				while(cur_dat_len--)
				{
					JTAG_TAP_HS_WriteTMSByte_ASYN(dat[cur_cmd_pos++]);
				}
				break;
			case VSLLINK_CMDJTAGSEQ_TMSCLOCK:
				if(cur_cmd & 1)
				{
					cur_cmd = 0xFF;
				}
				else
				{
					cur_cmd = 0x00;
				}
				cur_dat_len = (	(uint32)dat[cur_cmd_pos + 0] << 0) + 
							  (	(uint32)dat[cur_cmd_pos + 1] << 8) + 
							  (	(uint32)dat[cur_cmd_pos + 2] << 16) + 
							  (	(uint32)dat[cur_cmd_pos + 3] << 24);
				cur_cmd_pos += 4;
				while(cur_dat_len--)
				{
					JTAG_TAP_HS_WriteTMSByte_ASYN(0);
				}
				dat[rep_len++] = 0;
				break;
			case VSLLINK_CMDJTAGSEQ_SCAN:
				cur_dat_len = dat[cur_cmd_pos] + ((uint16)dat[cur_cmd_pos + 1] << 8);
				cur_cmd_pos += 2;

				cur_cmd &= 1;
				JTAG_TAP_HS_RW(dat + rep_len,								// tdo
							   dat + cur_cmd_pos + cur_cmd,					// tdi
							   dat[cur_cmd_pos],							// tms_before
							   dat[cur_cmd_pos + cur_dat_len + cur_cmd],	// tms_after0
							   dat[cur_cmd_pos + cur_dat_len + cur_cmd + 1],// tms_after1
							   cur_dat_len | ((uint16)cur_cmd << 15));		// dat_byte_len
				rep_len += cur_dat_len;
				cur_cmd_pos += cur_dat_len + cur_cmd + 2;
				break;
			}
		}
		break;
#if VSLLINK_HL_EN
	case VSLLINK_CMD_HW_JTAGHLCMD:
		cur_cmd_pos = 3;

		while(cur_cmd_pos < len)
		{
			cur_cmd = dat[cur_cmd_pos];
			cur_cmd_pos += 1;

			switch(cur_cmd & VSLLINK_CMDJTAGHL_CMDMSK)
			{
			case VSLLINK_CMDJTAGHL_TMS:
				cur_dat_len = (cur_cmd & VSLLINK_CMDJTAGHL_LENMSK) + 1;

				JTAG_TAP_TMS_Bit(dat + cur_cmd_pos, cur_dat_len);
				cur_cmd_pos += (cur_dat_len + 7) >> 3;
				break;
			case VSLLINK_CMDJTAGHL_IR:
				length = cur_cmd & VSLLINK_CMDJTAGHL_LENMSK;
				cur_dat_len = dat[cur_cmd_pos++];

				JTAG_TAP_InstrPtr(dat + cur_cmd_pos,		// intstr
								    dat + rep_len,			// tdo
									cur_dat_len,			// length_in_bits
									length);				// idle

				cur_dat_len = (cur_dat_len + 7) >> 3;
				rep_len += cur_dat_len;
				cur_cmd_pos += cur_dat_len;
				break;
			case VSLLINK_CMDJTAGHL_DR:
				length = cur_cmd & VSLLINK_CMDJTAGHL_LENMSK;
				cur_dat_len = dat[cur_cmd_pos] + (dat[cur_cmd_pos + 1] << 8);
				cur_cmd_pos += 2;

				JTAG_TAP_DataPtr(dat + cur_cmd_pos,			// tdi
								 dat + rep_len,				// tdo
								 cur_dat_len,				// length_in_bits
								 length);					// idle

				cur_dat_len = (cur_dat_len + 7) >> 3;
				rep_len += cur_dat_len;
				cur_cmd_pos += cur_dat_len;
				break;
			case VSLLINK_CMDJTAGHL_POLL_DLY:
				port_msk = cur_cmd & VSLLINK_CMDJTAGHL_LENMSK;
				if(port_msk > 0)
				{
					// max times to poll
					length = dat[cur_cmd_pos] + (dat[cur_cmd_pos + 1] << 8);
					cur_cmd_pos += 2;

					while(length > 0)
					{
						cur_cmd = 0;
						for(port_value = 0; port_value < 3; port_value++)
						{
							// ir
							if(port_msk & (1 << port_value))
							{
								cur_dat_len = dat[cur_cmd_pos + cur_cmd + 0] + 1;
								JTAG_TAP_InstrPtr(dat + cur_cmd_pos + cur_cmd + 2,	// intstr
												    poll_buf,						// tdo
													cur_dat_len,					// length_in_bits
													dat[cur_cmd_pos + cur_cmd + 1]);// idle
								cur_cmd += 2 + ((cur_dat_len + 7) >> 3);
							}
							// dr
							if(port_msk & (1 << (port_value + 3)))
							{
								cur_dat_len = dat[cur_cmd_pos + cur_cmd + 0] + 1;
								JTAG_TAP_DataPtr(dat + cur_cmd_pos + cur_cmd + 2,	// tdi
												 poll_buf,							// tdo
												 cur_dat_len,						// length_in_bits
												 dat[cur_cmd_pos + cur_cmd + 1]);	// idle
								cur_cmd += 2 + ((cur_dat_len + 7) >> 3);
							}
						}
						// check
						tmp8 = dat[cur_cmd_pos + cur_cmd++];
						for(port_value = 0; port_value < tmp8; port_value++)
						{
							if((poll_buf[dat[cur_cmd_pos + cur_cmd + 3 * port_value]] & dat[cur_cmd_pos + cur_cmd + 3 * port_value + 1]) != dat[cur_cmd_pos + cur_cmd + 3 * port_value + 2])
							{
								break;
							}
							cur_cmd += 3;
						}
						if(port_value == tmp8)
						{
							// poll_ok
							break;
						}
						else
						{
							length--;
						}
					}
					cur_cmd_pos += cur_cmd;
				}
				else
				{
					// simple delay
					DelayUSMS(dat[cur_cmd_pos + 0] + (dat[cur_cmd_pos + 1] << 8));
					cur_cmd_pos += 2;
					length = 1;
				}
				dat[rep_len++] = (0 == length);
				break;
			}
		}
		break;
#endif	// VSLLINK_HL_EN
	case VSLLINK_CMD_HW_JTAGRAWCMD:
		cur_dat_len = (len - 3) / 2;

		JTAG_TAP_HS_Operate_DMA(cur_dat_len, &dat[3], &dat[3 + cur_dat_len], &dat[rep_len]);
		rep_len += cur_dat_len;
		break;
	case VSLLINK_CMD_HW_SWDCMD:
	default:
		break;
	}
}

#endif

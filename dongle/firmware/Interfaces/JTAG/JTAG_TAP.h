/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       JTAG_TAP.h                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    JTAG interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define JTAG_TAP_HS_MAX_KHZ				(_SYS_FREQUENCY * 500 / 2)		// in KHz
#define JTAG_TAP_HS_MIN_KHZ				(_SYS_FREQUENCY * 500 / 256)	// in KHz

#define JTAG_TAP_TMS_2TLR				0xFF
#define JTAG_TAP_TMS_2RTI				0x7F
#define JTAG_TAP_TMS_E12UPDATE			0xC0

#define JTAG_TAP_TMS_UPDATERTI2SD		0x01
#define JTAG_TAP_TMS_UPDATERTI2SD_LEN	3
#define JTAG_TAP_TMS_UPDATERTI2SI		0x03
#define JTAG_TAP_TMS_UPDATERTI2SI_LEN	4

#define JTAG_TAP_Reset_ASYN()			JTAG_TAP_WriteTMSByte_ASYN(JTAG_TAP_TMS_2RTI)
#define JTAG_TAP_WriteTMSByte_ASYN(tms)	JTAG_TAP_Operate_Asyn(0, tms)

void (*JTAG_TAP_Operate_RAW)(uint32 bit_len, uint8 *tdi, uint8 *tms, uint8 *tdo);
uint16 (*JTAG_TAP_Operate_Asyn)(uint16 tdi, uint16 tms);

void JTAG_TAP_SetTCKFreq(uint16 kHz);
void JTAG_TAP_SetDaisyChainPos(uint32 ub, uint32 ua, uint32 bb, uint32 ba);
#define JTAG_TAP_ASYN					0
#define JTAG_TAP_RAW					1
void JTAG_TAP_Init(uint16 kHz, uint8 mode);
void JTAG_TAP_Fini(void);

void JTAG_TAP_RW(uint8 *tdo, uint8 *tdi, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len);
void JTAG_TAP_R(uint8 *tdo, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len);
void JTAG_TAP_W(uint8 *tdi, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len);

// About idle parameter: 
// If 0,		JTAG_TAP will stay in UPDATE_IR or UPDATE_DR
// If 1 -- 5,	JTAG_TAP will stay in RT/I, and idle - 1 shifts on RT/I
void JTAG_TAP_TMS_Bit(uint8* tms, uint8 bit_len);
uint32 JTAG_TAP_Instr(uint32 instr, uint8 bit_len, uint8 idle);
void JTAG_TAP_InstrPtr(uint8 *instr, uint8 *tdo, uint16 bit_len, uint8 idle);
void JTAG_TAP_InstrOutPtr(uint8 *instr, uint16 bit_len, uint8 idle);
void JTAG_TAP_DataInPtr(uint8 *tdo, uint16 bit_len, uint8 idle);
void JTAG_TAP_DataOutPtr(uint8 *tdi, uint16 bit_len, uint8 idle);
void JTAG_TAP_DataPtr(uint8 *tdi, uint8 *tdo, uint16 bit_len, uint8 idle);
uint32 JTAG_TAP_Data(uint32 tdi, uint16 bit_len, uint8 idle);
void JTAG_TAP_DataOut(uint32 tdi, uint16 bit_len, uint8 idle);
uint32 JTAG_TAP_DataIn(uint16 bit_len, uint8 idle);

extern int16 JTAG_Freq;		// in KHz

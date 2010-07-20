/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       C2.h                                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    C2 interface header file                                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#define C2_INS_Read							0x00
#define C2_INS_Write						0x01
#define C2_INS_Data							0x00
#define C2_INS_Addr							0x02

#define C2_INS_DataRead						(C2_INS_Data | C2_INS_Read)
#define C2_INS_AddrRead						(C2_INS_Addr | C2_INS_Read)
#define C2_INS_DataWrite					(C2_INS_Data | C2_INS_Write)
#define C2_INS_AddrWrite					(C2_INS_Addr | C2_INS_Write)

void C2_Init(void);
void C2_Fini(void);
void C2_ReadAddr(uint8 *ir);
void C2_WriteAddr(uint8 ir);
uint8 C2_ReadData(uint8 *data);
uint8 C2_WriteData(uint8 data);


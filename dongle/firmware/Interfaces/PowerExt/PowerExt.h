/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       PowerExt.h                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    Power output interface header file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2009-06-20:     created(by SimonQian)                             *
 **************************************************************************/

void PWREXT_Acquire(void);
void PWREXT_Release(void);
void PWREXT_ForceRelease(void);
uint8 PWREXT_GetState(void);

//RESULT target_voltage_set(uint8_t index, uint16_t voltage);
//RESULT target_voltage_get(uint8_t index, uint16_t *voltage);

/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    USBDM                                                     *
 *  File:       CommandProcesor.h                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header of CommandProcesor for USBDM                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-02:     created(by SimonQian)                             *
 **************************************************************************/

void BeforeInit(void);
void AfterInit(void);
void ProcessCommand(uint8* dat, uint16 len);
void ProcessIdle(void);
uint8 CheckLocalHandler(void);
void ProcessLocalHandler(void);

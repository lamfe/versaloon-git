/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWIM.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWIM interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

/*
HighSpeed(duty=2, period=10):
0:_    ________    ________    ________
   |__|        |__|        |__|        |_
   |2     8    |2     8    |2     8    |
1:_|         __|         __|         __|
   |________|  |________|  |________|  |_
       8     2     8     2     8     2
LowSpeed(duty=2, period=22):
0:_    ____________________    ____________________
   |__|                    |__|                    |_
   |2           20         |2           20         |
1:_|                     __|                     __|
   |____________________|  |____________________|  |_
             20          2            20         2
*/
#define SWIM_BITLEN_HS_PERIOD	10
#define SWIM_BITLEN_LS_PERIOD	22

#define SWIM_BITLEN_DUTY		2


#define SWIM_CMD_LEN			3
#define SWIM_DATA_LEN			8

#define SWIM_OutCmd(cmd)		SWIM_Out((cmd), SWIM_CMD_LEN)
#define SWIM_OutData(data)		SWIM_Out((data), SWIM_DATA_LEN)

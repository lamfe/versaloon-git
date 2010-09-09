#ifndef _USB_H_
#define _USB_H_

#include "Common.h"

extern void initUSB(void);

extern U8   receiveUSBCommand( U8 size, U8 *buffer);
extern void sendUSBResponse( U8 size, U8 *buffer);

interrupt void USBInterruptHandler( void );

#endif  // _USB_H_

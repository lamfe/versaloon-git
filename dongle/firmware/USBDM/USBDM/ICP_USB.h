#ifndef _USB_H_
#define _USB_H_

#include "Common.h"

#pragma CODE_SEG BOOT_ROM
extern void initICP_USB(void);
extern void USBEventPollingLoop(void);
#pragma CODE_SEG DEFAULT

#endif  // _USB_H_

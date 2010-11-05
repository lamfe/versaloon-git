#ifndef __USBBLASTER_H
#define __USBBLASTER_H

/* Includes ------------------------------------------------------------------*/
#include "usb_type.h"
#include "app_cfg.h"

#define bmBIT0   0x01
#define bmBIT1   0x02
#define bmBIT2   0x04
#define bmBIT3   0x08
#define bmBIT4   0x10
#define bmBIT5   0x20
#define bmBIT6   0x40
#define bmBIT7   0x80

#define TCK_BIT  bmBIT0
#define TMS_BIT  bmBIT1
#define NCE_BIT  bmBIT2
#define NCS_BIT  bmBIT3
#define TDI_BIT  bmBIT4
#define JTAGEEN_BIT  bmBIT5
#define WriteOnly_BIT  bmBIT6
#define ClockBytes_BIT bmBIT7

extern uint8_t stat[2];
extern uint16_t  CALL ;

void ShiftOut(uint8_t shiftdata);
void OutputByes_ShiftOut(uint8_t outbotbyesdata);
void Set_Get_State(uint8_t setgetstatedata);
void OutputByes_Set_Get_State(uint8_t outputsetgetdata);
void USB_To_JTAG_Send_Data(uint8_t* data_buffer, uint8_t Nb_bytes);
void Handle_USBAsynchXfer (void);
void STAT(void);

#endif 
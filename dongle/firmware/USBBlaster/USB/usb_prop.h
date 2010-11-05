/******************** (C) COPYRIGHT 2010 STMicroelectronics ********************
* File Name          : usb_prop.h
* Author             : MCD Application Team
* Version            : V3.2.1
* Date               : 07/05/2010
* Description        : All processing related to Virtual COM Port Demo (Endpoint 0)
********************************************************************************
* THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
* WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE TIME.
* AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY DIRECT,
* INDIRECT OR CONSEQUENTIAL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING FROM THE
* CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE CODING
* INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
*******************************************************************************/

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __usb_prop_H
#define __usb_prop_H

/* Includes ------------------------------------------------------------------*/
/* Exported types ------------------------------------------------------------*/
typedef struct
{
  uint32_t bitrate;
  uint8_t format;
  uint8_t paritytype;
  uint8_t datatype;
}LINE_CODING;

/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported define -----------------------------------------------------------*/

#define USB_BLASTER_GetConfiguration          NOP_Process
//#define USB_BLASTER_SetConfiguration          NOP_Process
#define USB_BLASTER_GetInterface              NOP_Process
#define USB_BLASTER_SetInterface              NOP_Process
#define USB_BLASTER_GetStatus                 NOP_Process
#define USB_BLASTER_ClearFeature              NOP_Process
#define USB_BLASTER_SetEndPointFeature        NOP_Process
#define USB_BLASTER_SetDeviceFeature          NOP_Process
//#define USB_BLASTER_SetDeviceAddress          NOP_Process

#define SEND_ENCAPSULATED_COMMAND   0x00
#define GET_ENCAPSULATED_RESPONSE   0x01
#define SET_COMM_FEATURE            0x02
#define GET_COMM_FEATURE            0x03
#define CLEAR_COMM_FEATURE          0x04
#define SET_LINE_CODING             0x20
#define GET_LINE_CODING             0x21
#define SET_CONTROL_LINE_STATE      0x22
#define SEND_BREAK                  0x23

/* Exported functions ------------------------------------------------------- */
void USB_BLASTER_init(void);
void USB_BLASTER_Reset(void);
void USB_BLASTER_SetConfiguration(void);
void USB_BLASTER_SetDeviceAddress (void);
void USB_BLASTER_Status_In (void);
void USB_BLASTER_Status_Out (void);
RESULT USB_BLASTER_Data_Setup(uint8_t);
RESULT USB_BLASTER_NoData_Setup(uint8_t);
RESULT USB_BLASTER_Get_Interface_Setting(uint8_t Interface, uint8_t AlternateSetting);
uint8_t *USB_BLASTER_GetDeviceDescriptor(uint16_t );
uint8_t *USB_BLASTER_GetConfigDescriptor(uint16_t);
uint8_t *USB_BLASTER_GetStringDescriptor(uint16_t);

uint8_t *Write_eeprom(uint16_t Length);
uint8_t *Read_eeprom(uint16_t Length);

#endif /* __usb_prop_H */

/******************* (C) COPYRIGHT 2010 STMicroelectronics *****END OF FILE****/


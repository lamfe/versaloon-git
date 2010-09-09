/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_prop.h
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : All processing related to USBDM
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
  uint8_t stopbittype;
  uint8_t paritytype;
  uint8_t datatype;
}LINE_CODING;

/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported define -----------------------------------------------------------*/

#define USBDM_GetConfiguration                     NOP_Process
//#define USBDM_SetConfiguration                     NOP_Process
#define USBDM_GetInterface                         NOP_Process
#define USBDM_SetInterface                         NOP_Process
#define USBDM_GetStatus                            NOP_Process
//#define USBDM_ClearFeature                         NOP_Process
#define USBDM_SetEndPointFeature                   NOP_Process
#define USBDM_SetDeviceFeature                     NOP_Process
//#define USBDM_SetDeviceAddress                     NOP_Process

/* USBDM */
#define USBDM_ICP_GET_VER			0x0C


/* MASS Storage Requests*/
#define GET_MAX_LUN                 0xFE
#define MASS_STORAGE_RESET          0xFF
#define LUN_DATA_LENGTH             1

/* Exported functions ------------------------------------------------------- */
void USBDM_init(void);
void USBDM_Reset(void);
void USBDM_ClearFeature(void);
void USBDM_SetConfiguration(void);
void USBDM_SetDeviceAddress (void);
void USBDM_Status_In (void);
void USBDM_Status_Out (void);
RESULT USBDM_Data_Setup(uint8_t);
RESULT USBDM_NoData_Setup(uint8_t);
RESULT USBDM_Get_Interface_Setting(uint8_t Interface, uint8_t AlternateSetting);
uint8_t *USBDM_GetDeviceDescriptor(uint16_t);
uint8_t *USBDM_GetConfigDescriptor(uint16_t);
uint8_t *USBDM_GetStringDescriptor(uint16_t);

uint8_t *USBDM_GetLineCoding(uint16_t Length);
uint8_t *USBDM_SetLineCoding(uint16_t Length);

#endif /* __usb_prop_H */

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_prop.h
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : All processing related to MSC Demo (Endpoint 0)
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
/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported define -----------------------------------------------------------*/

#define MSC_GetConfiguration          NOP_Process
//#define MSC_SetConfiguration          NOP_Process
#define MSC_GetInterface              NOP_Process
#define MSC_SetInterface              NOP_Process
#define MSC_GetStatus                 NOP_Process
//#define MSC_ClearFeature              NOP_Process
#define MSC_SetEndPointFeature        NOP_Process
#define MSC_SetDeviceFeature          NOP_Process
//#define MSC_SetDeviceAddress          NOP_Process

/* MASS Storage Requests*/
#define GET_MAX_LUN                 0xFE
#define MASS_STORAGE_RESET          0xFF
#define LUN_DATA_LENGTH             1

/* Exported functions ------------------------------------------------------- */
void MSC_init(void);
void MSC_Reset(void);
void MSC_ClearFeature(void);
void MSC_SetConfiguration(void);
void MSC_SetDeviceAddress (void);
void MSC_Status_In (void);
void MSC_Status_Out (void);
RESULT MSC_Data_Setup(uint8_t);
RESULT MSC_NoData_Setup(uint8_t);
RESULT MSC_Get_Interface_Setting(uint8_t Interface, uint8_t AlternateSetting);
uint8_t *MSC_GetDeviceDescriptor(uint16_t);
uint8_t *MSC_GetConfigDescriptor(uint16_t);
uint8_t *MSC_GetStringDescriptor(uint16_t);

#endif /* __usb_prop_H */
/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_prop.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : All processing related to Virtual Com Port Demo
********************************************************************************
* THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
* WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE TIME.
* AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY DIRECT,
* INDIRECT OR CONSEQUENTIAL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING FROM THE
* CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE CODING
* INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
*******************************************************************************/

#include "app_cfg.h"
#if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)

/* Includes ------------------------------------------------------------------*/
#include "usb_lib.h"

#include "usb_conf.h"
#include "usb_prop.h"
#include "usb_desc.h"
#include "usb_pwr.h"
#include "hw_config.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
/* -------------------------------------------------------------------------- */
/*  Structures initializations */
/* -------------------------------------------------------------------------- */

DEVICE Device_Table =
  {
    EP_NUM,
    1
  };

DEVICE_PROP Device_Property =
  {
    STK500V2_USB_init,
    STK500V2_USB_Reset,
    STK500V2_USB_Status_In,
    STK500V2_USB_Status_Out,
    STK500V2_USB_Data_Setup,
    STK500V2_USB_NoData_Setup,
    STK500V2_USB_Get_Interface_Setting,
    STK500V2_USB_GetDeviceDescriptor,
    STK500V2_USB_GetConfigDescriptor,
    STK500V2_USB_GetStringDescriptor,
    0,
    0x08 /*MAX PACKET SIZE*/
  };

USER_STANDARD_REQUESTS User_Standard_Requests =
  {
    STK500V2_USB_GetConfiguration,
    STK500V2_USB_SetConfiguration,
    STK500V2_USB_GetInterface,
    STK500V2_USB_SetInterface,
    STK500V2_USB_GetStatus,
    STK500V2_USB_ClearFeature,
    STK500V2_USB_SetEndPointFeature,
    STK500V2_USB_SetDeviceFeature,
    STK500V2_USB_SetDeviceAddress
  };

ONE_DESCRIPTOR Device_Descriptor =
  {
    (uint8_t*)STK500V2_USB_DeviceDescriptor,
    STK500V2_USB_SIZ_DEVICE_DESC
  };

ONE_DESCRIPTOR Config_Descriptor =
  {
    (uint8_t*)STK500V2_USB_ConfigDescriptor,
    STK500V2_USB_SIZ_CONFIG_DESC
  };

ONE_DESCRIPTOR String_Descriptor[4] =
  {
    {(uint8_t*)STK500V2_USB_StringLangID, STK500V2_USB_SIZ_STRING_LANGID},
    {(uint8_t*)STK500V2_USB_StringVendor, STK500V2_USB_SIZ_STRING_VENDOR},
    {(uint8_t*)STK500V2_USB_StringProduct, STK500V2_USB_SIZ_STRING_PRODUCT},
    {(uint8_t*)STK500V2_USB_StringSerial, STK500V2_USB_SIZ_STRING_SERIAL},
  };

/* Extern variables ----------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Extern function prototypes ------------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
/*******************************************************************************
* Function Name  : STK500V2_USB_init.
* Description    : AVR Dragon init routine.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_init(void)
{
  pInformation->Current_Configuration = 0;

  /* Connect the device */
  PowerOn();

  /* USB interrupts initialization */
  /* clear pending interrupts */
  _SetISTR(0);
  wInterrupt_Mask = IMR_MSK;
  /* set interrupts mask */
  _SetCNTR(wInterrupt_Mask);

  bDeviceState = UNCONNECTED;
}

/*******************************************************************************
* Function Name  : STK500V2_USB_Reset
* Description    : STK500V2_USB Mouse reset routine
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_Reset(void)
{
  /* Set STK500V2_USB DEVICE as not configured */
  Device_Info.Current_Configuration = 0;

  /* Current Feature initialization */
  Device_Info.Current_Feature = STK500V2_USB_ConfigDescriptor[7];

  /* Set STK500V2_USB DEVICE with the default Interface*/
  Device_Info.Current_Interface = 0;
  SetBTABLE(BTABLE_ADDRESS);

  /* Initialize Endpoint 0 */
  SetEPType(ENDP0, EP_CONTROL);
  SetEPTxStatus(ENDP0, EP_TX_STALL);
  SetEPRxAddr(ENDP0, ENDP0_RXADDR);
  SetEPTxAddr(ENDP0, ENDP0_TXADDR);
  Clear_Status_Out(ENDP0);
  SetEPRxCount(ENDP0, Device_Property.MaxPacketSize);
  SetEPRxValid(ENDP0);

  /* Initialize Endpoint 2 */
  SetEPType(ENDP2, EP_BULK);
  SetEPTxStatus(ENDP2, EP_TX_NAK);
  SetEPRxAddr(ENDP2, ENDP2_RXADDR);
  SetEPTxAddr(ENDP2, ENDP2_TXADDR);
  Clear_Status_Out(ENDP2);
  SetEPRxCount(ENDP2, STK500V2_USB_DATA_SIZE);
  SetEPRxStatus(ENDP2, EP_RX_VALID);

  /* Set this device to response on default address */
  SetDeviceAddress(0);
  bDeviceState = ATTACHED;
}

/*******************************************************************************
* Function Name  : STK500V2_USB_SetConfiguration
* Description    : Handle the SetConfiguration request.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_SetConfiguration(void)
{
  if (pInformation->Current_Configuration != 0)
  {
    /* Device configured */
    bDeviceState = CONFIGURED;
  }
}

/*******************************************************************************
* Function Name  : STK500V2_USB_SetDeviceAddress
* Description    : Udpade the device state to addressed.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_SetDeviceAddress (void)
{
  bDeviceState = ADDRESSED;
}

/*******************************************************************************
* Function Name  : STK500V2_USB_ClearFeature
* Description    : Handle the ClearFeature request.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_ClearFeature(void)
{
}

/*******************************************************************************
* Function Name  : STK500V2_USB_Status_In.
* Description    : AVR Dragon Status In Routine.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_Status_In(void)
{
}

/*******************************************************************************
* Function Name  : STK500V2_USB_Status_Out
* Description    : AVR Dragon Status OUT Routine.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void STK500V2_USB_Status_Out(void)
{
}

/*******************************************************************************
* Function Name  : STK500V2_USB_Data_Setup
* Description    : handle the data class specific requests
* Input          : Request Nb.
* Output         : None.
* Return         : USB_UNSUPPORT or USB_SUCCESS.
*******************************************************************************/
uint8_t *Virtual_Com_Port_GetLineCoding(uint16_t Length);
uint8_t *Virtual_Com_Port_SetLineCoding(uint16_t Length);
RESULT STK500V2_USB_Data_Setup(uint8_t RequestNo)
{
  uint8_t    *(*CopyRoutine)(uint16_t);

  CopyRoutine = NULL;

  if (CopyRoutine == NULL)
  {
    return USB_UNSUPPORT;
  }

  pInformation->Ctrl_Info.CopyData = CopyRoutine;
  pInformation->Ctrl_Info.Usb_wOffset = 0;
  (*CopyRoutine)(0);
  return USB_SUCCESS;
}

/*******************************************************************************
* Function Name  : STK500V2_USB_NoData_Setup.
* Description    : handle the no data class specific requests.
* Input          : Request Nb.
* Output         : None.
* Return         : USB_UNSUPPORT or USB_SUCCESS.
*******************************************************************************/
RESULT STK500V2_USB_NoData_Setup(uint8_t RequestNo)
{
  return USB_UNSUPPORT;
}

/*******************************************************************************
* Function Name  : STK500V2_USB_GetDeviceDescriptor.
* Description    : Gets the device descriptor.
* Input          : Length.
* Output         : None.
* Return         : The address of the device descriptor.
*******************************************************************************/
uint8_t *STK500V2_USB_GetDeviceDescriptor(uint16_t Length)
{
  return Standard_GetDescriptorData(Length, &Device_Descriptor );
}

/*******************************************************************************
* Function Name  : STK500V2_USB_GetConfigDescriptor.
* Description    : get the configuration descriptor.
* Input          : Length.
* Output         : None.
* Return         : The address of the configuration descriptor.
*******************************************************************************/
uint8_t *STK500V2_USB_GetConfigDescriptor(uint16_t Length)
{
  return Standard_GetDescriptorData(Length, &Config_Descriptor );
}

/*******************************************************************************
* Function Name  : STK500V2_USB_GetStringDescriptor
* Description    : Gets the string descriptors according to the needed index
* Input          : Length.
* Output         : None.
* Return         : The address of the string descriptors.
*******************************************************************************/
uint8_t *STK500V2_USB_GetStringDescriptor(uint16_t Length)
{
  uint8_t wValue0 = pInformation->USBwValue0;
  
  if (wValue0 >= (sizeof(String_Descriptor) / sizeof(String_Descriptor[0])))
  {
    return NULL;
  }
  else
  {
    return Standard_GetDescriptorData(Length, &String_Descriptor[wValue0]);
  }
}

/*******************************************************************************
* Function Name  : STK500V2_USB_Get_Interface_Setting.
* Description    : test the interface and the alternate setting according to the
*                  supported one.
* Input1         : uint8_t: Interface : interface number.
* Input2         : uint8_t: AlternateSetting : Alternate Setting number.
* Output         : None.
* Return         : The address of the string descriptors.
*******************************************************************************/
RESULT STK500V2_USB_Get_Interface_Setting(uint8_t Interface, uint8_t AlternateSetting)
{
  if (AlternateSetting > 0)
  {
    return USB_UNSUPPORT;
  }
  return USB_SUCCESS;
}

#endif  // #if (USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON)
/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

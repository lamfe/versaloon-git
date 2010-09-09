/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : usb_prop.c
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

/* Includes ------------------------------------------------------------------*/
#include "app_cfg.h"

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
uint32_t Max_Lun = 0;

/* -------------------------------------------------------------------------- */
/*  Structures initializations */
/* -------------------------------------------------------------------------- */

uint8_t USBDM_Vendor[] = 
{0x00, 0x32, 0x87, 0x15, 0x87};

DEVICE Device_Table =
  {
    EP_NUM,
    1
  };

DEVICE_PROP Device_Property =
  {
    USBDM_init,
    USBDM_Reset,
    USBDM_Status_In,
    USBDM_Status_Out,
    USBDM_Data_Setup,
    USBDM_NoData_Setup,
    USBDM_Get_Interface_Setting,
    USBDM_GetDeviceDescriptor,
    USBDM_GetConfigDescriptor,
    USBDM_GetStringDescriptor,
    0,
    0x08 /*MAX PACKET SIZE*/
  };

USER_STANDARD_REQUESTS User_Standard_Requests =
  {
    USBDM_GetConfiguration,
    USBDM_SetConfiguration,
    USBDM_GetInterface,
    USBDM_SetInterface,
    USBDM_GetStatus,
    USBDM_ClearFeature,
    USBDM_SetEndPointFeature,
    USBDM_SetDeviceFeature,
    USBDM_SetDeviceAddress
  };

ONE_DESCRIPTOR Device_Descriptor =
  {
    (uint8_t*)USBDM_DeviceDescriptor,
    USBDM_SIZ_DEVICE_DESC
  };

ONE_DESCRIPTOR Config_Descriptor =
  {
    (uint8_t*)USBDM_ConfigDescriptor,
    USBDM_SIZ_CONFIG_DESC
  };

ONE_DESCRIPTOR String_Descriptor[4] =
  {
    {(uint8_t*)USBDM_StringLangID, USBDM_SIZ_STRING_LANGID},
    {(uint8_t*)USBDM_StringVendor, USBDM_SIZ_STRING_VENDOR},
    {(uint8_t*)USBDM_StringProduct, USBDM_SIZ_STRING_PRODUCT},
    {(uint8_t*)USBDM_StringSerial, USBDM_SIZ_STRING_SERIAL}
  };

/* Extern variables ----------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Extern function prototypes ------------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
/*******************************************************************************
* Function Name  : USBDM_init.
* Description    : USBDM init routine.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_init(void)
{

  /* Update the serial number string descriptor with the data from the unique
  ID*/
  USB_Init_SerialString(USBDM_StringSerial + 2, USBDM_SIZ_STRING_SERIAL - 2);

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
* Function Name  : USBDM_Reset
* Description    : USBDM Mouse reset routine
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_Reset(void)
{
  /* Set USBDM DEVICE as not configured */
  pInformation->Current_Configuration = 0;

  /* Current Feature initialization */
  pInformation->Current_Feature = USBDM_ConfigDescriptor[7];

  /* Set USBDM DEVICE with the default Interface*/
  pInformation->Current_Interface = 0;
  SetBTABLE(BTABLE_ADDRESS);

  /* Initialize Endpoint 0 */
  SetEPType(ENDP0, EP_CONTROL);
  SetEPTxStatus(ENDP0, EP_TX_STALL);
  SetEPRxAddr(ENDP0, ENDP0_RXADDR);
  SetEPTxAddr(ENDP0, ENDP0_TXADDR);
  Clear_Status_Out(ENDP0);
  SetEPRxCount(ENDP0, Device_Property.MaxPacketSize);
  SetEPRxValid(ENDP0);

  /* Initialize Endpoint 1 */
  SetEPType(ENDP1, EP_BULK);
#if USB_RX_DOUBLEBUFFER_EN
  SetEPDoubleBuff(ENDP1);
  SetEPDblBuffAddr(ENDP1, ENDP3_RXADDR0, ENDP3_RXADDR1);
  SetEPDblBuffCount(ENDP1, EP_DBUF_OUT, USBDM_DATA_SIZE);
  ClearDTOG_RX(ENDP1);
  ClearDTOG_TX(ENDP1);
  ToggleDTOG_TX(ENDP1);
  SetEPRxStatus(ENDP1, EP_RX_VALID);
  SetEPTxStatus(ENDP1, EP_TX_DIS);
#else
  SetEPRxAddr(ENDP1, ENDP3_RXADDR);
  SetEPRxCount(ENDP1, USBDM_DATA_SIZE);
  SetEPRxStatus(ENDP1, EP_RX_VALID);
  SetEPTxStatus(ENDP1, EP_TX_DIS);
#endif

  /* Initialize Endpoint 2 */
  SetEPType(ENDP2, EP_BULK);
#if USB_TX_DOUBLEBUFFER_EN
  SetEPDoubleBuff(ENDP2);
  SetEPDblBuffAddr(ENDP2, ENDP2_TXADDR0, ENDP2_TXADDR1);
  SetEPDblBuffCount(ENDP2, EP_DBUF_IN, 0);
  ClearDTOG_RX(ENDP2);
  ClearDTOG_TX(ENDP2);
  SetEPRxStatus(ENDP2, EP_RX_DIS);
  SetEPTxStatus(ENDP2, EP_TX_NAK);
#else
  SetEPTxStatus(ENDP2, EP_TX_NAK);
  SetEPTxAddr(ENDP2, ENDP2_TXADDR);
  SetEPRxStatus(ENDP2, EP_RX_DIS);
#endif

  /* Set this device to response on default address */
  SetDeviceAddress(0);
  bDeviceState = ATTACHED;
}

/*******************************************************************************
* Function Name  : USBDM_SetConfiguration.
* Description    : Udpade the device state to configured.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_SetConfiguration(void)
{
  if (pInformation->Current_Configuration != 0)
  {
    /* Device configured */
    bDeviceState = CONFIGURED;
  }
}

/*******************************************************************************
* Function Name  : USBDM_SetConfiguration.
* Description    : Udpade the device state to addressed.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_SetDeviceAddress (void)
{
  bDeviceState = ADDRESSED;
}

/*******************************************************************************
* Function Name  : USBDM_ClearFeature
* Description    : Handle the ClearFeature request.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_ClearFeature(void)
{
}

/*******************************************************************************
* Function Name  : Get_Max_Lun
* Description    : Handle the Get Max Lun request.
* Input          : uint16_t Length.
* Output         : None.
* Return         : None.
*******************************************************************************/
uint8_t *Get_Max_Lun(uint16_t Length)
{
  if (Length == 0)
  {
    pInformation->Ctrl_Info.Usb_wLength = LUN_DATA_LENGTH;
    return 0;
  }
  else
  {
    return((uint8_t*)(&Max_Lun));
  }
}

/*******************************************************************************
* Function Name  : USBDM_Get_Vendor
* Description    : Handle the Get Max Lun request.
* Input          : uint16_t Length.
* Output         : None.
* Return         : None.
*******************************************************************************/
uint8_t *USBDM_Get_Vendor(uint16_t Length)
{
  if (Length == 0)
  {
    pInformation->Ctrl_Info.Usb_wLength = sizeof(USBDM_Vendor);
    return 0;
  }
  else
  {
    return USBDM_Vendor;
  }
}

/*******************************************************************************
* Function Name  : USBDM_Status_In.
* Description    : USBDM Status In Routine.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_Status_In(void)
{
}

/*******************************************************************************
* Function Name  : USBDM_Status_Out
* Description    : USBDM Status OUT Routine.
* Input          : None.
* Output         : None.
* Return         : None.
*******************************************************************************/
void USBDM_Status_Out(void)
{
}

/*******************************************************************************
* Function Name  : USBDM_Data_Setup
* Description    : handle the data class specific requests
* Input          : Request Nb.
* Output         : None.
* Return         : USB_UNSUPPORT or USB_SUCCESS.
*******************************************************************************/
RESULT USBDM_Data_Setup(uint8_t RequestNo)
{
  uint8_t    *(*CopyRoutine)(uint16_t);

  CopyRoutine = NULL;

  if (RequestNo == USBDM_ICP_GET_VER)
  {
    CopyRoutine = USBDM_Get_Vendor;
  }

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
* Function Name  : USBDM_NoData_Setup.
* Description    : handle the no data class specific requests.
* Input          : Request Nb.
* Output         : None.
* Return         : USB_UNSUPPORT or USB_SUCCESS.
*******************************************************************************/
RESULT USBDM_NoData_Setup(uint8_t RequestNo)
{

  return USB_UNSUPPORT;
}

/*******************************************************************************
* Function Name  : USBDM_GetDeviceDescriptor.
* Description    : Gets the device descriptor.
* Input          : Length.
* Output         : None.
* Return         : The address of the device descriptor.
*******************************************************************************/
uint8_t *USBDM_GetDeviceDescriptor(uint16_t Length)
{
  return Standard_GetDescriptorData(Length, &Device_Descriptor);
}

/*******************************************************************************
* Function Name  : USBDM_GetConfigDescriptor.
* Description    : get the configuration descriptor.
* Input          : Length.
* Output         : None.
* Return         : The address of the configuration descriptor.
*******************************************************************************/
uint8_t *USBDM_GetConfigDescriptor(uint16_t Length)
{
  return Standard_GetDescriptorData(Length, &Config_Descriptor);
}

/*******************************************************************************
* Function Name  : USBDM_GetStringDescriptor
* Description    : Gets the string descriptors according to the needed index
* Input          : Length.
* Output         : None.
* Return         : The address of the string descriptors.
*******************************************************************************/
uint8_t *USBDM_GetStringDescriptor(uint16_t Length)
{
  uint8_t wValue0 = pInformation->USBwValue0;
  if (wValue0 > (sizeof(String_Descriptor) / sizeof(String_Descriptor[0])))
  {
    return NULL;
  }
  else
  {
    return Standard_GetDescriptorData(Length, &String_Descriptor[wValue0]);
  }
}

/*******************************************************************************
* Function Name  : USBDM_Get_Interface_Setting.
* Description    : test the interface and the alternate setting according to the
*                  supported one.
* Input1         : uint8_t: Interface : interface number.
* Input2         : uint8_t: AlternateSetting : Alternate Setting number.
* Output         : None.
* Return         : The address of the string descriptors.
*******************************************************************************/
RESULT USBDM_Get_Interface_Setting(uint8_t Interface, uint8_t AlternateSetting)
{
  if (AlternateSetting > 0)
  {
    return USB_UNSUPPORT;
  }
  else if (Interface > 1)
  {
    return USB_UNSUPPORT;
  }
  return USB_SUCCESS;
}

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


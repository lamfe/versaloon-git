/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : scsi_data.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Initialization of the SCSI data
********************************************************************************
* THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
* WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE TIME.
* AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY DIRECT,
* INDIRECT OR CONSEQUENTIAL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING FROM THE
* CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE CODING
* INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
*******************************************************************************/

#include "app_cfg.h"

#if (	((USB_PROTOCOL == USB_AT_JTAGICE_MKII) || (USB_PROTOCOL == USB_AT_DRAGON))	\
		&& (USB_WITH_CDC == USB_WITH_IAD_CDC) && USB_WITH_MASSSTORAGE)				\
	|| ((USB_PROTOCOL == USB_ST_VCOM) && USB_WITH_MASSSTORAGE)

/* Includes ------------------------------------------------------------------*/
#include "usb_scsi.h"

uint8_t Page00_Inquiry_Data[] =
  {
    0x00, /* PERIPHERAL QUALIFIER & PERIPHERAL DEVICE TYPE*/
    0x00,
    0x00,
    0x00,
    0x00 /* Supported Pages 00*/
  };
uint8_t Standard_Inquiry_Data[] =
  {
    0x00,          /* Direct Access Device */
    0x80,          /* RMB = 1: Removable Medium */
    0x02,          /* Version: No conformance claim to standard */
    0x02,

    36 - 4,        /* Additional Length */
    0x00,          /* SCCS = 1: Storage Controller Component */
    0x00,
    0x00,
    /* Vendor Identification */
    'S', 'i', 'm', 'o', 'n', ' ', ' ', ' ',
    /* Product Identification */
#if USB_PROTOCOL == USB_ST_VCOM
    'F', 'D', 'o', 'n', 'V', 'C', 'O',
    'M', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
#else
    'F', 'D', 'o', 'n', 'J', 'T', 'A',
    'G', 'I', 'C', 'E', 'm', 'k', 'I', 'I', ' ',
#endif
	/* Product Revision Level */
    '1', '.', '0', ' '
  };
/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/
uint8_t Mode_Sense6_data[] =
  {
    0x03,
    0x00,
    0x00,
    0x00,
  };

/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

uint8_t Mode_Sense10_data[] =
  {
    0x00,
    0x06,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00
  };
uint8_t Scsi_Sense_Data[] =
  {
    0x70, /*RespCode*/
    0x00, /*SegmentNumber*/
    NO_SENSE, /* Sens_Key*/
    0x00,
    0x00,
    0x00,
    0x00, /*Information*/
    0x0A, /*AdditionalSenseLength*/
    0x00,
    0x00,
    0x00,
    0x00, /*CmdInformation*/
    NO_SENSE, /*Asc*/
    0x00, /*ASCQ*/
    0x00, /*FRUC*/
    0x00, /*TBD*/
    0x00,
    0x00 /*SenseKeySpecific*/
  };
uint8_t ReadCapacity10_Data[] =
  {
    /* Last Logical Block */
    0,
    0,
    0,
    0,

    /* Block Length */
    0,
    0,
    0,
    0
  };

uint8_t ReadFormatCapacity_Data [] =
  {
    0x00,
    0x00,
    0x00,
    0x08, /* Capacity List Length */

    /* Block Count */
    0,
    0,
    0,
    0,

    /* Block Length */
    0x02,/* Descriptor Code: Formatted Media */
    0,
    0,
    0
  };

#endif

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

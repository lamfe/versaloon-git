/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : memory.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Memory management layer
********************************************************************************
* THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
* WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE TIME.
* AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY DIRECT,
* INDIRECT OR CONSEQUENTIAL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING FROM THE
* CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE CODING
* INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
*******************************************************************************/

#include "app_cfg.h"

#if USB_WITH_MASSSTORAGE

/* Includes ------------------------------------------------------------------*/
#include <stdint.h>
#include "memory.h"

#include "usb_desc.h"
#include "usb_conf.h"
#include "hw_config.h"
#include "usb_bot.h"
#include "usb_scsi.h"
#include "usb_regs.h"
#include "usb_mem.h"
#include "mass_mal.h"

#ifdef MSD_BUFF_SIZE_IN_DWORD
#	if MSD_BUFF_SIZE_IN_DWORD < (MSD_MEMORY_BLOCK_SIZE / 4)
#		undef MSD_BUFF_SIZE_IN_DWORD
#		define MSD_BUFF_SIZE_IN_DWORD	MSD_MEMORY_BLOCK_SIZE / 4
#	endif
#else
#	define MSD_BUFF_SIZE_IN_DWORD		MSD_MEMORY_BLOCK_SIZE / 4
#endif

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
uint32_t Block_Read_count = 0;
uint32_t Block_offset;
uint32_t Counter = 0;
uint32_t  Idx;
uint32_t Data_Buffer[MSD_BUFF_SIZE_IN_DWORD];
uint8_t TransferState = TXFR_IDLE;
/* Extern variables ----------------------------------------------------------*/
extern uint8_t Bulk_Data_Buff[BULK_MAX_PACKET_SIZE];  /* data buffer*/
extern uint16_t Data_Len;
extern uint8_t Bot_State;
extern Bulk_Only_CBW CBW;
extern Bulk_Only_CSW CSW;
extern uint32_t Mass_Memory_Size[MAL_LUN_NUM];
extern uint32_t Mass_Block_Size[MAL_LUN_NUM];

/* Private function prototypes -----------------------------------------------*/
/* Extern function prototypes ------------------------------------------------*/
/* Private functions ---------------------------------------------------------*/

void Read_Memory(uint8_t lun, uint32_t Memory_Offset, uint32_t Transfer_Length)
{
  static uint32_t Offset, Length;

  if (TransferState == TXFR_IDLE )
  {
    Offset = Memory_Offset * Mass_Block_Size[lun];
    Length = Transfer_Length * Mass_Block_Size[lun];
    TransferState = TXFR_ONGOING;
  }

  if (TransferState == TXFR_ONGOING )
  {
    if (!Block_Read_count)
    {
      MAL_Read(lun ,
               Offset ,
               Data_Buffer,
               Mass_Block_Size[lun]);

      UserToPMABufferCopy((uint8_t *)Data_Buffer, ENDP4_TXADDR, BULK_MAX_PACKET_SIZE);
      Block_Read_count = Mass_Block_Size[lun] - BULK_MAX_PACKET_SIZE;
      Block_offset = BULK_MAX_PACKET_SIZE;
    }
    else
    {
      UserToPMABufferCopy((uint8_t *)Data_Buffer + Block_offset, ENDP4_TXADDR, BULK_MAX_PACKET_SIZE);
      Block_Read_count -= BULK_MAX_PACKET_SIZE;
      Block_offset += BULK_MAX_PACKET_SIZE;
    }

    SetEPTxCount(ENDP4, BULK_MAX_PACKET_SIZE);
    SetEPTxStatus(ENDP4, EP_TX_VALID);
    Offset += BULK_MAX_PACKET_SIZE;
    Length -= BULK_MAX_PACKET_SIZE;

    CSW.dDataResidue -= BULK_MAX_PACKET_SIZE;
    Led_RW_ON();
  }
  if (Length == 0)
  {
    Block_Read_count = 0;
    Block_offset = 0;
    Offset = 0;
    Bot_State = BOT_DATA_IN_LAST;
    TransferState = TXFR_IDLE;
    Led_RW_OFF();
  }
}

void Write_Memory (uint8_t lun, uint32_t Memory_Offset, uint32_t Transfer_Length)
{

  static uint32_t W_Offset, W_Length;

  uint32_t temp =  Counter + 64;

  if (TransferState == TXFR_IDLE )
  {
    W_Offset = Memory_Offset * Mass_Block_Size[lun];
    W_Length = Transfer_Length * Mass_Block_Size[lun];
    TransferState = TXFR_ONGOING;
  }

  if (TransferState == TXFR_ONGOING )
  {

    for (Idx = 0 ; Counter < temp; Counter++)
    {
      *((uint8_t *)Data_Buffer + Counter) = Bulk_Data_Buff[Idx++];
    }

    W_Offset += Data_Len;
    W_Length -= Data_Len;

    if (!(W_Length % Mass_Block_Size[lun]))
    {
      Counter = 0;
      MAL_Write(lun ,
                W_Offset - Mass_Block_Size[lun],
                Data_Buffer,
                Mass_Block_Size[lun]);
    }

    CSW.dDataResidue -= Data_Len;
    SetEPRxStatus(ENDP4, EP_RX_VALID); /* enable the next transaction*/

    Led_RW_ON();
  }

  if ((W_Length == 0) || (Bot_State == BOT_CSW_Send))
  {
    Counter = 0;
    Set_CSW (CSW_CMD_PASSED, SEND_CSW_ENABLE);
    TransferState = TXFR_IDLE;
    Led_RW_OFF();
  }
}

#endif

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/


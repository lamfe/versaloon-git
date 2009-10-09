/******************** (C) COPYRIGHT 2009 STMicroelectronics ********************
* File Name          : mass_mal.c
* Author             : MCD Application Team
* Version            : V3.0.1
* Date               : 04/27/2009
* Description        : Medium Access Layer interface
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
#include "mass_mal.h"

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
uint32_t Mass_Memory_Size[MAL_LUN_NUM];
uint32_t Mass_Block_Size[MAL_LUN_NUM];
uint32_t Mass_Block_Count[MAL_LUN_NUM];
uint32_t Status = 0;

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/
uint16_t MAL_GetRealBlockCount(void)
{
  uint32_t flash_size = *(uint16_t*)0x1FFFF7E0 * 1024;
  
  if (flash_size > (MSD_MEMORY_START_ADDR - 0x08000000))
  {
    return (flash_size + 0x08000000 - MSD_MEMORY_START_ADDR) / MSD_MEMORY_BLOCK_SIZE;
  }
  else
  {
    return 0;
  }
}

/*******************************************************************************
* Function Name  : MAL_Init
* Description    : Initializes the Media on the STM32
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
uint16_t MAL_Init(uint8_t lun)
{
  switch (lun)
  {
    case 0:
      if ((*(uint16*)(MSD_MEMORY_START_ADDR + 11) != 0xFFFF) 
          && (*(uint16*)(MSD_MEMORY_START_ADDR + 19) != 0xFFFF))
      {
        Mass_Block_Size[0]  = *(uint16*)(MSD_MEMORY_START_ADDR + 11);
        Mass_Block_Count[0] = *(uint16*)(MSD_MEMORY_START_ADDR + 19);
      }
      else
      {
        Mass_Block_Size[0]  = 0;
        Mass_Block_Count[0] = 0;
      }
      if (Mass_Block_Size[0] != MSD_MEMORY_BLOCK_SIZE)
      {
          Mass_Block_Size[0] = MSD_MEMORY_BLOCK_SIZE;
      }
      if (Mass_Block_Count[0] > MAL_GetRealBlockCount())
      {
//          Mass_Block_Count[0] = MAL_GetRealBlockCount();
      }
      Mass_Memory_Size[0] = Mass_Block_Count[0] * Mass_Block_Size[0];
      break;
    default:
      return MAL_FAIL;
  }
  return MAL_OK;
}
/*******************************************************************************
* Function Name  : MAL_Write
* Description    : Write sectors
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
uint16_t MAL_Write(uint8_t lun, uint32_t Memory_Offset, uint32_t *Writebuff, uint16_t Transfer_Length)
{
  uint16_t i;
  switch (lun)
  {
    case 0:
      if ((Memory_Offset + Transfer_Length) > Mass_Memory_Size[lun])
      {
        LED_RED_ON();
        return MAL_FAIL;
      }
      
      FLASH_Unlock();
      FLASH_ClearFlag(FLASH_FLAG_BSY | FLASH_FLAG_EOP | FLASH_FLAG_PGERR | FLASH_FLAG_WRPRTERR);
      
      // Erase page first
      if (FLASH_COMPLETE != FLASH_ErasePage((uint32)MSD_MEMORY_START_ADDR + Memory_Offset))
      {
        LED_RED_ON();
        return MAL_FAIL;
      }
      
      // Write page
      for (i = 0; i < Transfer_Length / 4; i++)
      {
        FLASH_ProgramWord((uint32)MSD_MEMORY_START_ADDR + Memory_Offset + i * 4, Writebuff[i]);
      }
      
      FLASH_Lock();
      
      return MAL_OK;
    default:
      return MAL_FAIL;
  }
}

/*******************************************************************************
* Function Name  : MAL_Read
* Description    : Read sectors
* Input          : None
* Output         : None
* Return         : Buffer pointer
*******************************************************************************/
uint16_t MAL_Read(uint8_t lun, uint32_t Memory_Offset, uint32_t *Readbuff, uint16_t Transfer_Length)
{
  uint16_t i;
  switch (lun)
  {
    case 0:
      for (i = 0; i < Transfer_Length / 4; i++)
      {
        if ((Memory_Offset + i * 4 + 4) <= Mass_Memory_Size[lun])
        {
          Readbuff[i] = *(uint32*)(MSD_MEMORY_START_ADDR + Memory_Offset + i * 4);
        }
        else
        {
          Readbuff[i] = 0;
        }
      }
      break;
    default:
      return MAL_FAIL;
  }
  return MAL_OK;
}

/*******************************************************************************
* Function Name  : MAL_GetStatus
* Description    : Get status
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
uint16_t MAL_GetStatus (uint8_t lun)
{
  switch (lun)
  {
    case 0:
      break;
    default:
      return MAL_FAIL;
  }
  return MAL_OK;
}

#endif

/******************* (C) COPYRIGHT 2009 STMicroelectronics *****END OF FILE****/

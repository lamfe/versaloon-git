#include <string.h>

#include "app_type.h"
#include "interfaces.h"

#include "STM32_FLASH.h"

#define STM32_FLASH_START_ADDR			0x08000000
#define STM32_FLASH_ADDR(offset)		(STM32_FLASH_START_ADDR + (offset))
#define STM32_FLASH_SIZE_KB				(*(uint16_t *)0x1FFFF7E0)

#define STM32_FLASH_KEYR_KEY1			(uint32_t)0x45670123
#define STM32_FLASH_KEYR_KEY2			(uint32_t)0xCDEF89AB
#define STM32_FLASH_OPTKEYR_KEY1		(uint32_t)0x45670123
#define STM32_FLASH_OPTKEYR_KEY2		(uint32_t)0xCDEF89AB

#define STM32_FLASH_CR_LOCK				((uint32_t)1 << 7)
#define STM32_FLASH_CR_PER				((uint32_t)1 << 1)
#define STM32_FLASH_CR_OPTER			((uint32_t)1 << 5)
#define STM32_FLASH_CR_OPTPG			((uint32_t)1 << 4)
#define STM32_FLASH_CR_PG				((uint32_t)1 << 0)
#define STM32_FLASH_CR_STAT				((uint32_t)1 << 6)

#define STM32_FLASH_SR_EOP				((uint32_t)1 << 5)
#define STM32_FLASH_SR_BSY				((uint32_t)1 << 0)
#define STM32_FLASH_SR_PGERR			((uint32_t)1 << 2)
#define STM32_FLASH_SR_WRPRTERR			((uint32_t)1 << 4)

#define STM32_FLASH_OBR_RDPRT			((uint32_t)1 << 1)

vsf_err_t stm32_flash_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_lock(uint8_t index)
{
	switch (index)
	{
	case 0:
		FLASH->CR |= STM32_FLASH_CR_LOCK;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_unlock(uint8_t index)
{
	switch (index)
	{
	case 0:
		FLASH->KEYR = STM32_FLASH_KEYR_KEY1;
		FLASH->KEYR = STM32_FLASH_KEYR_KEY2;
		FLASH->SR |= STM32_FLASH_SR_EOP | STM32_FLASH_SR_BSY | 
						STM32_FLASH_SR_PGERR | STM32_FLASH_SR_WRPRTERR;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_getcapacity(uint8_t index, uint32_t *pagesize, 
									uint32_t *pagenum)
{
	uint16_t flash_size = STM32_FLASH_SIZE_KB;
	
	switch (index)
	{
	case 0:
		if (flash_size >= 256)
		{
			if (NULL != pagesize)
			{
				*pagesize = 2 * 1024;
			}
			if (NULL != pagenum)
			{
				*pagenum = flash_size / 2;
			}
		}
		else
		{
			if (NULL != pagesize)
			{
				*pagesize = 1024;
			}
			if (NULL != pagenum)
			{
				*pagenum = flash_size;
			}
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_read(uint8_t index, uint32_t offset, uint8_t *buff, 
						uint32_t size)
{
	switch (index)
	{
	case 0:
		memcpy(buff, (uint8_t *)STM32_FLASH_ADDR(offset), size);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_read_isready(uint8_t index, uint32_t offset,
									uint8_t *buff, uint32_t size)
{
	REFERENCE_PARAMETER(offset);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(size);
	
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_write(uint8_t index, uint32_t offset, uint8_t *buff, 
							uint32_t size)
{
	uint32_t i;
	
	switch (index)
	{
	case 0:
		FLASH->CR |= STM32_FLASH_CR_PG;
		for (i = 0; i < size / 2; i++)
		{
			*(uint16_t *)STM32_FLASH_ADDR(offset) = *(uint16_t *)buff;
			while (FLASH->SR & STM32_FLASH_SR_BSY);
			if (FLASH->SR & (/*STM32_FLASH_SR_PGERR | */STM32_FLASH_SR_WRPRTERR))
			{
				FLASH->CR &= ~STM32_FLASH_CR_PG;
				return VSFERR_FAIL;
			}
			offset += 2;
			buff += 2;
		}
		FLASH->CR &= ~STM32_FLASH_CR_PG;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_write_isready(uint8_t index, uint32_t offset,
									uint8_t *buff, uint32_t size)
{
	REFERENCE_PARAMETER(offset);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(size);
	
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_readpage(uint8_t index, uint32_t offset, uint8_t *buff)
{
	uint32_t page_size;
	
	switch (index)
	{
	case 0:
		stm32_flash_getcapacity(index, &page_size, NULL);
		return stm32_flash_read(index, offset, buff, page_size);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_readpage_isready(uint8_t index, uint32_t offset,
										uint8_t *buff)
{
	uint32_t page_size;
	
	switch (index)
	{
	case 0:
		stm32_flash_getcapacity(index, &page_size, NULL);
		return stm32_flash_read_isready(index, offset, buff, page_size);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_erasepage(uint8_t index, uint32_t offset)
{
	switch (index)
	{
	case 0:
		FLASH->CR |= STM32_FLASH_CR_PER;
		FLASH->AR = STM32_FLASH_ADDR(offset); 
		FLASH->CR |= STM32_FLASH_CR_STAT;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_erasepage_isready(uint8_t index, uint32_t offset)
{
	vsf_err_t err;
	
	REFERENCE_PARAMETER(offset);
	
	switch (index)
	{
	case 0:
		err = ((FLASH->SR & STM32_FLASH_SR_BSY) == 0) ?
					VSFERR_NONE : VSFERR_NOT_READY;
		if (!err)
		{
			FLASH->CR &= ~STM32_FLASH_CR_PER;
		}
		return (FLASH->SR & STM32_FLASH_SR_WRPRTERR) ? VSFERR_FAIL : err;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_writepage(uint8_t index, uint32_t offset, uint8_t *buff)
{
	uint32_t page_size;
	
	switch (index)
	{
	case 0:
		stm32_flash_getcapacity(index, &page_size, NULL);
		return stm32_flash_write(index, offset, buff, page_size);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_writepage_isready(uint8_t index, uint32_t offset, 
										uint8_t *buff)
{
	uint32_t page_size;
	
	switch (index)
	{
	case 0:
		stm32_flash_getcapacity(index, &page_size, NULL);
		return stm32_flash_write_isready(index, offset, buff, page_size);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t stm32_flash_protect(uint8_t index)
{
	switch (index)
	{
	case 0:
		FLASH->OPTKEYR = STM32_FLASH_OPTKEYR_KEY1;
		FLASH->OPTKEYR = STM32_FLASH_OPTKEYR_KEY2;
		FLASH->CR |= STM32_FLASH_CR_OPTER;
		FLASH->CR |= STM32_FLASH_CR_STAT;
		while (FLASH->SR & STM32_FLASH_SR_BSY);
		FLASH->CR &= ~STM32_FLASH_CR_OPTER;
		FLASH->CR |= STM32_FLASH_CR_OPTPG;
		OB->RDP = 0;
		while (FLASH->SR & STM32_FLASH_SR_BSY);
		FLASH->CR &= ~STM32_FLASH_CR_OPTPG;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

bool stm32_flash_isprotected(uint8_t index)
{
	switch (index)
	{
	case 0:
		return (FLASH->OBR & STM32_FLASH_OBR_RDPRT) ? true : false;
	default:
		return false;
	}
}

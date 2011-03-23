/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#include "../dal.h"
#include "mal.h"
#include "mal_internal.h"

#define MAL_RETRY_CNT					0xFFFF

static struct mal_driver_t *mal_drivers[] = 
{
#if DAL_EE93CX6_EN
	&ee93cx6_drv,
#endif
#if DAL_EE24CXX_EN
	&ee24cxx_drv,
#endif
#if DAL_DF25XX_EN
	&df25xx_drv,
#endif
#if DAL_DF45XX_EN
	&df45xx_drv,
#endif
	NULL
};

static struct mal_driver_t* mal_find_driver(uint16_t index)
{
	struct mal_driver_t **p = mal_drivers;
	
	while (*p != NULL)
	{
		if (index == (*p)->index)
		{
			return *p;
		}
		p++;
	}
	return NULL;
}

static RESULT mal_init(uint16_t index, void *param)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->init))
	{
		return ERROR_FAIL;
	}
	
	return driver->init(param);
}

static RESULT mal_fini(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->fini))
	{
		return ERROR_FAIL;
	}
	
	return driver->fini();
}

static RESULT mal_getinfo(uint16_t index, void *info)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->getinfo))
	{
		return ERROR_FAIL;
	}
	
	return driver->getinfo(info);
}

static RESULT mal_getcapacity(uint16_t index, uint64_t *block_size, 
								uint64_t *block_number)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if (NULL == driver)
	{
		return ERROR_FAIL;
	}
	
	if (block_size != NULL)
	{
		*block_size = driver->capacity.block_size;
	}
	if (block_number != NULL)
	{
		*block_number = driver->capacity.block_number;
	}
	return ERROR_OK;
}

static RESULT mal_setcapacity(uint16_t index, uint64_t block_size, 
								uint64_t block_number)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if (NULL == driver)
	{
		return ERROR_FAIL;
	}
	
	driver->capacity.block_size = block_size;
	driver->capacity.block_number = block_number;
	return ERROR_OK;
}

static RESULT mal_eraseblock_nb_start(uint16_t index, uint64_t address, 
										uint64_t count)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseblock_nb_start))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseblock_nb_start(address, count);
}

static RESULT mal_eraseblock_nb(uint16_t index, uint64_t address)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseblock_nb))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseblock_nb(address);
}

static RESULT mal_eraseblock_nb_isready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseblock_nb_isready))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseblock_nb_isready();
}

static RESULT mal_eraseblock_nb_waitready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if (NULL == driver)
	{
		return ERROR_FAIL;
	}
	
	if (driver->eraseblock_nb_waitready != NULL)
	{
		return driver->eraseblock_nb_waitready();
	}
	else
	{
		if (driver->eraseblock_nb_isready != NULL)
		{
			uint32_t dly;
			RESULT ret;
			
			dly = MAL_RETRY_CNT;
			ret = ERROR_FAIL;
			while (dly--)
			{
				if (ERROR_OK == mal_eraseblock_nb_isready(index))
				{
					ret = ERROR_OK;
					break;
				}
			}
			return ret;
		} 
		return ERROR_FAIL;
	}
}

static RESULT mal_eraseblock_nb_end(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseblock_nb_end))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseblock_nb_end();
}

static RESULT mal_eraseall_nb_start(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseall_nb_start))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseall_nb_start();
}

static RESULT mal_eraseall_nb_isready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseall_nb_isready))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseall_nb_isready();
}

static RESULT mal_eraseall_nb_waitready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if (NULL == driver)
	{
		return ERROR_FAIL;
	}
	
	if (driver->eraseall_nb_waitready != NULL)
	{
		return driver->eraseall_nb_waitready();
	}
	else
	{
		if (driver->eraseall_nb_isready != NULL)
		{
			uint32_t dly;
			RESULT ret;
			
			dly = MAL_RETRY_CNT;
			ret = ERROR_FAIL;
			while (dly--)
			{
				if (ERROR_OK == mal_eraseall_nb_isready(index))
				{
					ret = ERROR_OK;
					break;
				}
			}
			return ret;
		} 
		return ERROR_FAIL;
	}
}

static RESULT mal_eraseall_nb_end(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->eraseall_nb_end))
	{
		return ERROR_FAIL;
	}
	
	return driver->eraseall_nb_end();
}

static RESULT mal_readblock_nb_start(uint16_t index, uint64_t address, 
										uint64_t count)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->readblock_nb_start))
	{
		return ERROR_FAIL;
	}
	
	return driver->readblock_nb_start(address, count);
}

static RESULT mal_readblock_nb(uint16_t index, uint64_t address, uint8_t *buff)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->readblock_nb))
	{
		return ERROR_FAIL;
	}
	
	return driver->readblock_nb(address, buff);
}

static RESULT mal_readblock_nb_isready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->readblock_nb_isready))
	{
		return ERROR_FAIL;
	}
	
	return driver->readblock_nb_isready();
}

static RESULT mal_readblock_nb_waitready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if (NULL == driver)
	{
		return ERROR_FAIL;
	}
	
	if (driver->readblock_nb_waitready != NULL)
	{
		return driver->readblock_nb_waitready();
	}
	else
	{
		if (driver->readblock_nb_isready != NULL)
		{
			uint32_t dly;
			RESULT ret;
			
			dly = MAL_RETRY_CNT;
			ret = ERROR_FAIL;
			while (dly--)
			{
				if (ERROR_OK == mal_readblock_nb_isready(index))
				{
					ret = ERROR_OK;
					break;
				}
			}
			return ret;
		} 
		return ERROR_FAIL;
	}
}

static RESULT mal_readblock_nb_end(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->readblock_nb_end))
	{
		return ERROR_FAIL;
	}
	
	return driver->readblock_nb_end();
}

static RESULT mal_writeblock_nb_start(uint16_t index, uint64_t address, 
										uint64_t count)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->writeblock_nb_start))
	{
		return ERROR_FAIL;
	}
	
	return driver->writeblock_nb_start(address, count);
}

static RESULT mal_writeblock_nb(uint16_t index, uint64_t address, uint8_t *buff)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->writeblock_nb))
	{
		return ERROR_FAIL;
	}
	
	return driver->writeblock_nb(address, buff);
}

static RESULT mal_writeblock_nb_isready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->writeblock_nb_isready))
	{
		return ERROR_FAIL;
	}
	
	return driver->writeblock_nb_isready();
}

static RESULT mal_writeblock_nb_waitready(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if (NULL == driver)
	{
		return ERROR_FAIL;
	}
	
	if (driver->writeblock_nb_waitready != NULL)
	{
		return driver->writeblock_nb_waitready();
	}
	else
	{
		if (driver->writeblock_nb_isready != NULL)
		{
			uint32_t dly;
			RESULT ret;
			
			dly = MAL_RETRY_CNT;
			ret = ERROR_FAIL;
			while (dly--)
			{
				if (ERROR_OK == mal_writeblock_nb_isready(index))
				{
					ret = ERROR_OK;
					break;
				}
			}
			return ret;
		} 
		return ERROR_FAIL;
	}
}

static RESULT mal_writeblock_nb_end(uint16_t index)
{
	struct mal_driver_t* driver;
	
	driver = mal_find_driver(index);
	if ((NULL == driver) || (NULL == driver->writeblock_nb_end))
	{
		return ERROR_FAIL;
	}
	
	return driver->writeblock_nb_end();
}

static RESULT mal_eraseblock(uint16_t index, uint64_t address, uint64_t count)
{
	uint64_t block_size, i;
	
	if ((ERROR_OK != mal_getcapacity(index, &block_size, NULL)) || 
		(ERROR_OK != mal_eraseblock_nb_start(index, address, count)))
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < count; i++)
	{
		if ((ERROR_OK != mal_eraseblock_nb(index, address)) || 
			(ERROR_OK != mal_eraseblock_nb_waitready(index)))
		{
			return ERROR_FAIL;
		}
		address += block_size;
	}
	
	if (ERROR_OK != mal_eraseblock_nb_end(index))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT mal_eraseall(uint16_t index)
{
	if (ERROR_OK != mal_eraseall_nb_start(index))
	{
		// erase all not available, try erase block
		uint64_t block_number;
		
		if ((ERROR_OK != mal_getcapacity(index, NULL, &block_number)) || 
			(ERROR_OK != mal_eraseblock(index, 0, block_number)))
		{
			return ERROR_FAIL;
		}
		return ERROR_OK;
	}
	else
	{
		if ((ERROR_OK != mal_eraseall_nb_waitready(index)) || 
			(ERROR_OK != mal_eraseall_nb_end(index)))
		{
			return ERROR_FAIL;
		}
		return ERROR_OK;
	}
}

static RESULT mal_readblock(uint16_t index, uint64_t address, uint8_t *buff, 
							uint64_t count)
{
	uint64_t block_size, i;
	
	if ((ERROR_OK != mal_getcapacity(index, &block_size, NULL)) || 
		(ERROR_OK != mal_readblock_nb_start(index, address, count)))
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < count; i++)
	{
		if ((ERROR_OK != mal_readblock_nb(index, address, buff)) || 
			(ERROR_OK != mal_readblock_nb_waitready(index)))
		{
			return ERROR_FAIL;
		}
		buff += block_size;
	}
	
	return mal_readblock_nb_end(index);
}

static RESULT mal_writeblock(uint16_t index, uint64_t address, uint8_t *buff, 
								uint64_t count)
{
	uint64_t block_size, i;
	
	if ((ERROR_OK != mal_getcapacity(index, &block_size, NULL)) || 
		(ERROR_OK != mal_writeblock_nb_start(index, address, count)))
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < count; i++)
	{
		if ((ERROR_OK != mal_writeblock_nb(index, address, buff)) || 
			(ERROR_OK != mal_writeblock_nb_waitready(index)))
		{
			return ERROR_FAIL;
		}
		buff += block_size;
	}
	
	return mal_writeblock_nb_end(index);
}

struct mal_t mal = 
{
	mal_init,
	mal_fini,
	mal_getinfo,
	mal_getcapacity,
	mal_setcapacity,
	
	mal_eraseall,
	mal_eraseblock,
	mal_readblock,
	mal_writeblock,
	
	mal_eraseall_nb_start,
	mal_eraseall_nb_isready,
	mal_eraseall_nb_waitready,
	mal_eraseall_nb_end,
	
	mal_eraseblock_nb_start,
	mal_eraseblock_nb,
	mal_eraseblock_nb_isready,
	mal_eraseblock_nb_waitready,
	mal_eraseblock_nb_end,
	
	mal_readblock_nb_start,
	mal_readblock_nb,
	mal_readblock_nb_isready,
	mal_readblock_nb_waitready,
	mal_readblock_nb_end,
	
	mal_writeblock_nb_start,
	mal_writeblock_nb,
	mal_writeblock_nb_isready,
	mal_writeblock_nb_waitready,
	mal_writeblock_nb_end
};


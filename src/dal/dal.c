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

#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "scripts.h"
#include "programmer.h"

#include "dal.h"

#if DAL_MIC2826_EN
#include "mic2826/mic2826_script.h"
#endif

VSS_HANDLER(dal_vss_init);
VSS_HANDLER(dal_vss_fini);
struct vss_cmd_t dal_cmd[] = 
{
	VSS_CMD(	"dal.init",
				"initialize driver abstraction layer, format: dal.init KHZ",
				dal_vss_init),
	VSS_CMD(	"dal.fini",
				"finialize driver abstraction layer, format: dal.fini",
				dal_vss_fini),
#if DAL_MIC2826_EN
	VSS_CMD(	"mic2826.init",
				"initialize mic2826 through IIC, format: mic2826.init",
				mic2826_vss_init),
	VSS_CMD(	"mic2826.fini",
				"finalize mic2826 through IIC, format: mic2826.fini",
				mic2826_vss_fini),
	VSS_CMD(	"mic2826.config",
				"config mic2826 through IIC, format: "
				"mic2826.config DCDC LDO1 LOD2 LDO3",
				mic2826_vss_config),
#endif
	VSS_CMD_END
};

#ifdef SYS_ON_VERSALOON
RESULT dal_init(struct interfaces_info_t *ifs)
{
	REFERENCE_PARAMETER(ifs);
	return ERROR_OK;
}
#else
struct interfaces_info_t *interfaces = NULL;
RESULT dal_init(struct interfaces_info_t *ifs)
{
	interfaces = ifs;
	return ERROR_OK;
}
#endif

VSS_HANDLER(dal_vss_init)
{
	struct programmer_info_t *prog = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	return dal_init(&prog->interfaces);
}

VSS_HANDLER(dal_vss_fini)
{
	VSS_CHECK_ARGC(1);
	return ERROR_OK;
}


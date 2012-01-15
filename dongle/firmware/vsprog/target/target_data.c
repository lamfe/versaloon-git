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

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "interfaces.h"
#include "target.h"

vsf_err_t target_data_free(struct program_context_t *context)
{
	// target data is in flash, can not be freed
	return VSFERR_NONE;
}

vsf_err_t target_data_read(struct program_context_t *context)
{
	uint32_t reafile, writefile;
	
	// target data is in flash
	target_prepare_operations(context, NULL, NULL);
	return VSFERR_NONE;
}

vsf_err_t target_data_save(struct program_context_t *context)
{
	// not supported in embedded vsprog
	return VSFERR_FAIL;
}

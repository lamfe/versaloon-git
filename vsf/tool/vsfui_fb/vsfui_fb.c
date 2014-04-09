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

#include "app_type.h"
#include "tool/buffer/buffer.h"

#include "dal/dal.h"
#include "vsfui_fb.h"

// frame_buffer driver for vsfui

vsf_err_t vsfui_fb_init(struct vsfui_fb_t *vsfui_fb)
{
	if (vsf_multibuf_init(vsfui_fb->mbuffer) ||
		(NULL == vsfui_fb->screen.dal) ||
		(NULL == vsfui_fb->screen.driver) ||
		(NULL == vsfui_fb->screen.driver->display) ||
		((vsfui_fb->screen.driver->init != NULL) &&
			vsfui_fb->screen.driver->init(vsfui_fb->screen.dal)))
	{
		return VSFERR_FAIL;
	}
	vsfui_fb->displaying = false;
	return VSFERR_NONE;
}

vsf_err_t vsfui_fb_fini(struct vsfui_fb_t *vsfui_fb)
{
	if (vsfui_fb->screen.driver->fini != NULL)
	{
		vsfui_fb->screen.driver->fini(vsfui_fb->screen.dal);
	}
	return VSFERR_NONE;
}

void* vsfui_fb_get_buffer(struct vsfui_fb_t *vsfui_fb)
{
	return vsf_multibuf_get_empty(vsfui_fb->mbuffer);
}

vsf_err_t vsfui_fb_validate_buffer(struct vsfui_fb_t *vsfui_fb)
{
	return vsf_multibuf_push(vsfui_fb->mbuffer);
}

vsf_err_t vsfui_fb_poll(struct vsfui_fb_t *vsfui_fb)
{
	struct vsfui_fb_driver_t *driver = vsfui_fb->screen.driver;
	struct dal_info_t *dal = vsfui_fb->screen.dal;
	uint8_t *buffer = vsf_multibuf_get_payload(vsfui_fb->mbuffer);
	
	if (NULL == buffer)
	{
		return VSFERR_NONE;
	}
	
	if (vsfui_fb->displaying)
	{
		if ((NULL == driver->display_isready) ||
			!driver->display_isready(dal, buffer))
		{
			vsf_multibuf_pop(vsfui_fb->mbuffer);
			vsfui_fb->displaying = false;
		}
	}
	else if (!driver->display(dal, buffer))
	{
		vsfui_fb->displaying  = true;
	}
	return VSFERR_NONE;
}

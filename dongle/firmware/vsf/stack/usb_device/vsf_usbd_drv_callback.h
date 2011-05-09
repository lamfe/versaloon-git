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

#ifndef __VSF_USBD_DRV_CALLBACK_H_INCLUDED__
#define __VSF_USBD_DRV_CALLBACK_H_INCLUDED__

RESULT vsfusbd_on_SETUP(void *p);
RESULT vsfusbd_on_IN(void *p, uint8_t ep);
RESULT vsfusbd_on_OUT(void *p, uint8_t ep);
RESULT vsfusbd_on_UNDERFLOW(void *p, uint8_t ep);
RESULT vsfusbd_on_OVERFLOW(void *p, uint8_t ep);
RESULT vsfusbd_on_RESET(void *p);
RESULT vsfusbd_on_WAKEUP(void *p);
RESULT vsfusbd_on_SUSPEND(void *p);
RESULT vsfusbd_on_RESUME(void *p);
RESULT vsfusbd_on_SOF(void *p);
RESULT vsfusbd_on_ERROR(void *p, enum usb_err_type_t type);

#endif	// __VSF_USBD_DRV_CALLBACK_H_INCLUDED__


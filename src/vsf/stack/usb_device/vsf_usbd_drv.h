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

#ifndef __VSF_USBD_DRV_H_INCLUDED__
#define __VSF_USBD_DRV_H_INCLUDED__

enum ep_state_t
{
	EP_STAT_STALL,
	EP_STAT_NACK,
	EP_STAT_RXEN_TXEN,
	EP_STAT_RXEN_TXDIS,
	EP_STAT_RXDIS_TXEN,
	EP_STAT_RXDIS_TXDIS,
};

enum ep_type_t
{
	EP_TYPE_CONTROL,
	EP_TYPE_INTERRUPT,
	EP_TYPE_BULK,
	EP_TYPE_ISO,
};

enum usberr_type_t
{
	USBERR_OK,
	USBERR_INVALID_CRC,
};

#define USB_DRV_ABILITY_SUSPEND_RESUME
#define USB_DRV_ABILITY_CONNECT_DISCONNECT
#define USB_DRV_ABILITY_LOWPOWER
#define USB_DRV_ABILITY_FRAMENUMBER

struct vsfusbd_drv_t
{
	uint16_t abilities;
	
	RESULT (*init)(void);
	RESULT (*fini)(void);
	RESULT (*reset)(void);
	RESULT (*poll)(void);
	RESULT (*suspend)(void);
	RESULT (*resume)(void);
	RESULT (*connect)(void);
	RESULT (*disconnect)(void);
	RESULT (*lowpower_mode)(enum lp_mode_t mode);
	uint32_t (*get_framenumber)(void);
	
	void (*setaddr)(uint8_t addr);
	uint8_t (*getaddr)(void);
	
	struct vsfusbd_callback_t
	{
		RESULT (*on_RESET)(void);
		RESULT (*on_ERROR)(enum usberr_type_t type);
		RESULT (*on_WAKEUP)(void);
		RESULT (*on_SUSPEND)(void);
		RESULT (*on_RESUME)(void);
		RESULT (*on_SOF)(void);
		RESULT (*on_NO_SOF)(void);
		RESULT (*on_SETUP)(void);
	} callback;
	
	struct vsfusbd_ep_t
	{
		RESULT (*reset)(uint8_t idx);
		RESULT (*config)(uint8_t idx, enum ep_type_t type, enum ep_state_t state, uint16_t ovf_thre, uint16_t udf_tres);
		RESULT (*readbuf)(uint8_t idx, uint8_t *buff, uint16_t offset, uint16_t size);
		RESULT (*writebuf)(uint8_t idx, uint8_t *buff, uint16_t offset, uint16_t size);
		RESULT (*set_toggle)(uint8_t toggle);
		uint16_t (*get_rx_count)(void);
		uint16_t (*get_tx_count)(void);
		void (*set_tx_count)(uint16_t count);
		
		struct vsfusb_ep_callback_t
		{
			struct vsf_transaction_buffer_t buffer_out;
			struct vsf_transaction_buffer_t buffer_in;
			
			RESULT (*on_IN)(void);
			RESULT (*on_OUT)(void);
			RESULT (*on_SYNC_UNDERFLOW)(void);
			RESULT (*on_SYNC_OVERFLOW)(void);
		} ep_callback[VSFUSBD_EP_NUM];
	} ep;
};

#endif	// __VSF_USBD_DRV_H_INCLUDED__


/***************************************************************************
 *   Copyright (C) 2009 by Simon Qian <SimonQian@SimonQian.com>            *
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
#ifndef __VSLLINK_H_INCLUDED__
#define __VSLLINK_H_INCLUDED__

// SWJ
RESULT vsllink_swj_connect(void);
RESULT vsllink_swj_disconnect(void);
RESULT vsllink_swj_commit(uint8 *result);
RESULT vsllink_swj_seqout(uint8 *data, uint16 bit_len);
RESULT vsllink_swj_seqin(uint8 *data, uint16 bit_len);
RESULT vsllink_swj_transact(uint8 request, uint32 *data);
RESULT vsllink_swj_setpara(uint8 trn, uint16 retry, uint16 dly);

// JTAG common
RESULT vsllink_jtag_connect(void);
RESULT vsllink_jtag_set_freq(uint16 kHz);
RESULT vsllink_jtag_auxio_config(uint8 pin_mask, uint8 io);
RESULT vsllink_jtag_auxio_out(uint8 pin_mask, uint8 value);
RESULT vsllink_jtag_auxio_in(uint8 pin_mask, uint8 *value);

// JTAG_LL
RESULT vsllink_jtagll_disconnect(void);
RESULT vsllink_jtagll_commit(void);
RESULT vsllink_jtagll_tms(uint8 *tms, uint8 len);
RESULT vsllink_jtagll_tms_clocks(uint32 len, uint8 tms);
RESULT vsllink_jtagll_xr(uint8* r, uint16 len, uint8 tms_before_valid, 
						 uint8 tms_before, uint8 tms_after0, uint8 tms_after1);


// JTAG_HL
RESULT vsllink_jtaghl_disconnect(void);
RESULT vsllink_jtaghl_config(uint16 kHz, uint8 ub, uint8 ua, uint16 bb, 
							 uint16 ba);
RESULT vsllink_jtaghl_tms(uint8 *tms, uint8 len);
RESULT vsllink_jtaghl_runtest(uint32 cycles);
RESULT vsllink_jtaghl_ir(uint8 *ir, uint8 len, uint8 idle, uint8 want_ret);
RESULT vsllink_jtaghl_dr(uint8 *dr, uint16 len, uint8 idle, uint8 want_ret);
RESULT vsllink_jtaghl_poll(uint8 *ir0, uint8 ir0idle, uint8 ir0len, 
						   uint8 *dr0, uint8 dr0idle, uint8 dr0len,
						   uint8 *ir1, uint8 ir1idle, uint8 ir1len, 
						   uint8 *dr1, uint8 dr1idle, uint8 dr1len,
						   uint8 *ir2, uint8 ir2idle, uint8 ir2len, 
						   uint8 *dr2, uint8 dr2idle, uint8 dr2len,
						   uint8 pos, uint8 mask, uint8 value, 
						   uint16 poll_count);
RESULT vsllink_jtaghl_delay_us(uint16 us);
RESULT vsllink_jtaghl_delay_ms(uint16 ms);
RESULT vsllink_jtaghl_register_callback(jtag_callback_t send_callback, 
										jtag_callback_t receive_callback);
RESULT vsllink_jtaghl_commit(void);

#endif /* __VSLLINK_H_INCLUDED__ */


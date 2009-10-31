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
RESULT vsllink_swj_commit(uint8_t *result);
RESULT vsllink_swj_seqout(uint8_t *data, uint16_t bit_len);
RESULT vsllink_swj_seqin(uint8_t *data, uint16_t bit_len);
RESULT vsllink_swj_transact(uint8_t request, uint32_t *data);
RESULT vsllink_swj_setpara(uint8_t trn, uint16_t retry, uint16_t dly);

// JTAG common
RESULT vsllink_jtag_connect(void);
RESULT vsllink_jtag_set_freq(uint16_t kHz);
RESULT vsllink_jtag_auxio_config(uint8_t pin_mask, uint8_t io);
RESULT vsllink_jtag_auxio_out(uint8_t pin_mask, uint8_t value);
RESULT vsllink_jtag_auxio_in(uint8_t pin_mask, uint8_t *value);

// JTAG_LL
RESULT vsllink_jtagll_disconnect(void);
RESULT vsllink_jtagll_commit(void);
RESULT vsllink_jtagll_tms(uint8_t *tms, uint8_t len);
RESULT vsllink_jtagll_tms_clocks(uint32_t len, uint8_t tms);
RESULT vsllink_jtagll_xr(uint8_t* r, uint16_t len, uint8_t tms_before_valid, 
						 uint8_t tms_before, uint8_t tms_after0, uint8_t tms_after1);


// JTAG_HL
RESULT vsllink_jtaghl_disconnect(void);
RESULT vsllink_jtaghl_config(uint16_t kHz, uint8_t ub, uint8_t ua, uint16_t bb, 
							 uint16_t ba);
RESULT vsllink_jtaghl_tms(uint8_t *tms, uint8_t len);
RESULT vsllink_jtaghl_runtest(uint32_t cycles);
RESULT vsllink_jtaghl_ir(uint8_t *ir, uint8_t len, uint8_t idle, uint8_t want_ret);
RESULT vsllink_jtaghl_dr(uint8_t *dr, uint16_t len, uint8_t idle, uint8_t want_ret);
RESULT vsllink_jtaghl_poll(uint8_t *ir0, uint8_t ir0idle, uint8_t ir0len, 
						   uint8_t *dr0, uint8_t dr0idle, uint8_t dr0len,
						   uint8_t *ir1, uint8_t ir1idle, uint8_t ir1len, 
						   uint8_t *dr1, uint8_t dr1idle, uint8_t dr1len,
						   uint8_t *ir2, uint8_t ir2idle, uint8_t ir2len, 
						   uint8_t *dr2, uint8_t dr2idle, uint8_t dr2len,
						   uint8_t pos, uint8_t mask, uint8_t value, 
						   uint16_t poll_count);
RESULT vsllink_jtaghl_delay_us(uint16_t us);
RESULT vsllink_jtaghl_delay_ms(uint16_t ms);
RESULT vsllink_jtaghl_register_callback(jtag_callback_t send_callback, 
										jtag_callback_t receive_callback);
RESULT vsllink_jtaghl_commit(void);

#endif /* __VSLLINK_H_INCLUDED__ */


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
#ifndef __PROGRAMMER_H_INCLUDED__
#define __PROGRAMMER_H_INCLUDED__

typedef enum
{
	JTAG_SCANTYPE_IR, 
	JTAG_SCANTYPE_DR
}jtag_irdr_t;
typedef RESULT (*jtag_callback_t)(jtag_irdr_t cmd, uint32_t ir, 
								  uint8_t *dest_buffer, uint8_t *src_buffer, 
								  uint16_t buffer_len, uint16_t *processed);

#include "vsprog.h"
typedef struct
{
	// the 3 element listed below MUST be define valid
	// other functions can be initialized in .init_ability()
	char *name;
	RESULT (*parse_argument)(char cmd, const char *argu);
	RESULT (*init_capability)(void *p);
	uint32_t (*display_programmer)(void);
	misc_cmd_t *misc_cmd;
	
	// init and fini
	RESULT (*init)(void);
	RESULT (*fini)(void);
	
	// interfaces supported
	uint32_t interfaces;
	
	// peripheral
	// spi
	RESULT (*spi_init)(void);
	RESULT (*spi_fini)(void);
	RESULT (*spi_config)(uint16_t kHz, uint8_t cpol, uint8_t cpha, uint8_t first_bit);
	RESULT (*spi_io)(uint8_t *out, uint8_t *in, uint16_t len, uint16_t inpos, 
					 uint16_t inlen);
	
	// gpio
	RESULT (*gpio_init)(void);
	RESULT (*gpio_fini)(void);
	RESULT (*gpio_config)(uint16_t pin_mask, uint16_t io, uint16_t value);
	RESULT (*gpio_out)(uint16_t pin_mask, uint16_t value);
	RESULT (*gpio_in)(uint16_t pin_mask, uint16_t *value);
	
	// delay
	RESULT (*delayms)(uint16_t ms);
	RESULT (*delayus)(uint16_t us);
	
	RESULT (*peripheral_commit)(void);
	
	// target
	RESULT (*get_target_voltage)(uint16_t *voltage);
	
	// issp for PSoC
	RESULT (*issp_init)(void);
	RESULT (*issp_fini)(void);
	RESULT (*issp_enter_program_mode)(uint8_t mode);
	RESULT (*issp_leave_program_mode)(uint8_t mode);
	RESULT (*issp_wait_and_poll)(void);
	RESULT (*issp_vector)(uint8_t operate, uint8_t addr, uint8_t data, uint8_t *buf);
	RESULT (*issp_commit)(void);
	
	// swj
	RESULT (*swj_init)(void);
	RESULT (*swj_fini)(void);
	RESULT (*swj_seqout)(uint8_t *data, uint16_t bit_len);
	RESULT (*swj_seqin)(uint8_t *data, uint16_t bit_len);
	RESULT (*swj_transact)(uint8_t request, uint32_t *data);
	RESULT (*swj_setpara)(uint8_t trn, uint16_t retry, uint16_t dly);
	RESULT (*swj_commit)(uint8_t *result);
	
	// jtag_hl
	RESULT (*jtag_hl_init)(void);
	RESULT (*jtag_hl_fini)(void);
	RESULT (*jtag_hl_config)(uint16_t kHz, uint8_t ub, uint8_t ua, uint16_t bb, 
							 uint16_t ba);
	RESULT (*jtag_hl_tms)(uint8_t* tms, uint8_t len);
	RESULT (*jtag_hl_runtest)(uint32_t len);
	RESULT (*jtag_hl_ir)(uint8_t *ir, uint8_t len, uint8_t idle, uint8_t want_ret);
	RESULT (*jtag_hl_dr)(uint8_t *dr, uint16_t len, uint8_t idle, uint8_t want_ret);
/*
	RESULT (*jtag_hl_poll)(uint8_t *ir0, uint8_t ir0idle, uint8_t ir0len, 
						   uint8_t *dr0, uint8_t dr0idle, uint8_t dr0len,
						   uint8_t *ir1, uint8_t ir1idle, uint8_t ir1len, 
						   uint8_t *dr1, uint8_t dr1idle, uint8_t dr1len,
						   uint8_t *ir2, uint8_t ir2idle, uint8_t ir2len, 
						   uint8_t *dr2, uint8_t dr2idle, uint8_t dr2len,
						   uint8_t pos, uint8_t mask, uint8_t value, 
						   uint16_t poll_count);
*/
	RESULT (*jtag_hl_delay_us)(uint16_t us);
	RESULT (*jtag_hl_delay_ms)(uint16_t ms);
	RESULT (*jtag_hl_aux_io_init)(void);
	RESULT (*jtag_hl_aux_io_fini)(void);
	RESULT (*jtag_hl_aux_io_config)(uint8_t pin_mask, uint8_t io);
	RESULT (*jtag_hl_aux_io_out)(uint8_t pin_mask, uint8_t value);
	RESULT (*jtag_hl_aux_io_in)(uint8_t pin_mask, uint8_t *value);
	RESULT (*jtag_hl_register_callback)(jtag_callback_t send_callback, 
										jtag_callback_t receive_callback);
	RESULT (*jtag_hl_commit)(void);
	
	// jtag_ll
	RESULT (*jtag_ll_init)(void);
	RESULT (*jtag_ll_fini)(void);
	RESULT (*jtag_ll_set_frequency)(uint16_t kHz);
	RESULT (*jtag_ll_tms)(uint8_t *tms, uint8_t len);
	RESULT (*jtag_ll_tms_clocks)(uint32_t len, uint8_t tms);
	RESULT (*jtag_ll_xr)(uint8_t* r, uint16_t len, uint8_t tms_before_valid, 
						 uint8_t tms_before, uint8_t tms_after0, uint8_t tms_after1);
	RESULT (*jtag_ll_aux_io_init)(void);
	RESULT (*jtag_ll_aux_io_fini)(void);
	RESULT (*jtag_ll_aux_io_config)(uint8_t pin_mask, uint8_t io);
	RESULT (*jtag_ll_aux_io_out)(uint8_t pin_mask, uint8_t value);
	RESULT (*jtag_ll_aux_io_in)(uint8_t pin_mask, uint8_t *value);
	RESULT (*jtag_ll_commit)(void);
	
	// msp430_jtag
	RESULT (*msp430jtag_init)(void);
	RESULT (*msp430jtag_fini)(void);
	RESULT (*msp430jtag_config)(uint8_t has_test);
	RESULT (*msp430jtag_ir)(uint8_t *ir, uint8_t want_ret);
	RESULT (*msp430jtag_dr)(uint32_t *dr, uint8_t len, uint8_t want_ret);
	RESULT (*msp430jtag_tclk)(uint8_t value);
	RESULT (*msp430jtag_tclk_strobe)(uint16_t cnt);
	RESULT (*msp430jtag_reset)(void);
	RESULT (*msp430jtag_poll)(uint32_t dr, uint32_t mask, uint32_t value, uint8_t len, 
							  uint16_t poll_cnt, uint8_t toggle_tclk);
	
	// msp430_sbw
	RESULT (*msp430sbw_init)(void);
	RESULT (*msp430sbw_fini)(void);
	RESULT (*msp430sbw_config)(uint8_t has_test);
	RESULT (*msp430sbw_ir)(uint8_t *ir, uint8_t want_ret);
	RESULT (*msp430sbw_dr)(uint32_t *dr, uint8_t len, uint8_t want_ret);
	RESULT (*msp430sbw_tclk)(uint8_t value);
	RESULT (*msp430sbw_tclk_strobe)(uint16_t cnt);
	RESULT (*msp430sbw_reset)(void);
	RESULT (*msp430sbw_poll)(uint32_t dr, uint32_t mask, uint32_t value, uint8_t len, 
							 uint16_t poll_cnt, uint8_t toggle_tclk);
	
	// c2 for c8051f
	RESULT (*c2_init)(void);
	RESULT (*c2_fini)(void);
	RESULT (*c2_addr_write)(uint8_t addr);
	RESULT (*c2_data_read)(uint8_t *data, uint8_t len);
	RESULT (*c2_data_write)(uint8_t *data, uint8_t len);
	RESULT (*c2_addr_poll)(uint8_t mask, uint8_t value, uint16_t poll_cnt);
	RESULT (*c2_commit)(void);
	
	// i2c
	RESULT (*i2c_init)(void);
	RESULT (*i2c_fini)(void);
	RESULT (*i2c_set_speed)(uint16_t kHz);
	RESULT (*i2c_read)(uint16_t chip_addr, uint8_t chip_addr_len, uint8_t *data, 
					   uint16_t data_len, uint8_t stop);
	RESULT (*i2c_write)(uint16_t chip_addr, uint8_t chip_addr_len, uint8_t *data, 
						uint16_t data_len, uint8_t stop);
	RESULT (*i2c_commit)(void);
	
	// lpcicp
	RESULT (*lpcicp_init)(void);
	RESULT (*lpcicp_fini)(void);
	RESULT (*lpcicp_enter_program_mode)(void);
	RESULT (*lpcicp_in)(uint8_t *buff, uint16_t len);
	RESULT (*lpcicp_out)(uint8_t *buff, uint16_t len);
	RESULT (*lpcicp_poll_ready)(uint8_t data, uint8_t *ret, uint8_t setmask, 
								uint8_t clearmask, uint16_t pollcnt);
	RESULT (*lpcicp_commit)(void);
	
	// mass-product support
	RESULT (*query_mass_product_data_size)(uint32_t *size);
	RESULT (*download_mass_product_data)(const char *name, uint8_t *buffer, 
										 uint32_t len);
	
	// firmware update support
	RESULT (*enter_firmware_update_mode)(void);
}programmer_info_t;

#define PROGRAMMER_DEFINE(name, parse_argument, init_capability, 	\
						  display_programmer, misc_cmd)				\
	{\
		name, parse_argument, init_capability, display_programmer, 	\
		misc_cmd, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0\
	}

extern programmer_info_t *cur_programmer;
extern programmer_info_t programmers_info[];

void programmer_print_list(void);
void programmer_print_help(void);
RESULT programmer_init(const char *programmer);

#endif /* __PROGRAMMER_H_INCLUDED__ */


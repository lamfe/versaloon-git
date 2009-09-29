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

typedef struct
{
	// the 3 element listed below MUST be define valid
	// other functions can be initialized in .init_ability()
	char *name;
	RESULT (*parse_argument)(char cmd, const char *argu);
	RESULT (*init_capability)(void *p);
	uint32 (*display_programmer)(void);
	
	// init and fini
	RESULT (*init)(void);
	RESULT (*fini)(void);
	
	// interfaces supported
	uint32 interfaces;
	
	// peripheral
	// spi
	RESULT (*spi_init)(void);
	RESULT (*spi_fini)(void);
	RESULT (*spi_config)(uint16 kHz, uint8 cpol, uint8 cpha, uint8 first_bit);
	RESULT (*spi_io)(uint8 *out, uint8 *in, uint16 len, uint16 inpos, 
					 uint16 inlen);
	
	// gpio
	RESULT (*gpio_init)(void);
	RESULT (*gpio_fini)(void);
	RESULT (*gpio_config)(uint16 pin_mask, uint16 io);
	RESULT (*gpio_out)(uint16 pin_mask, uint16 value);
	RESULT (*gpio_in)(uint16 pin_mask, uint16 *value);
	
	// delay
	RESULT (*delayms)(uint16 ms);
	RESULT (*delayus)(uint16 us);
	
	RESULT (*peripheral_commit)(void);
	
	// target
	RESULT (*get_target_voltage)(uint16 *voltage);
	
	// issp for PSoC
	RESULT (*issp_init)(void);
	RESULT (*issp_fini)(void);
	RESULT (*issp_enter_program_mode)(uint8 mode);
	RESULT (*issp_leave_program_mode)(uint8 mode);
	RESULT (*issp_wait_and_poll)(void);
	RESULT (*issp_vector)(uint8 operate, uint8 addr, uint8 data, uint8 *buf);
	RESULT (*issp_commit)(void);
	
	// jtag_hl
	RESULT (*jtag_hl_init)(void);
	RESULT (*jtag_hl_fini)(void);
	RESULT (*jtag_hl_config)(uint16 kHz, uint8 ub, uint8 ua, uint16 bb, 
							 uint16 ba);
	RESULT (*jtag_hl_tms)(uint8 *tms, uint8 len);
	RESULT (*jtag_hl_ir)(uint8 *ir, uint8 len, uint8 idle, uint8 want_ret);
	RESULT (*jtag_hl_dr)(uint8 *dr, uint16 len, uint8 idle, uint8 want_ret);
/*
	RESULT (*jtag_hl_poll)(uint8 *ir0, uint8 ir0idle, uint8 ir0len, 
						   uint8 *dr0, uint8 dr0idle, uint8 dr0len,
						   uint8 *ir1, uint8 ir1idle, uint8 ir1len, 
						   uint8 *dr1, uint8 dr1idle, uint8 dr1len,
						   uint8 *ir2, uint8 ir2idle, uint8 ir2len, 
						   uint8 *dr2, uint8 dr2idle, uint8 dr2len,
						   uint8 pos, uint8 mask, uint8 value, 
						   uint16 poll_count);
*/
	RESULT (*jtag_hl_delay_us)(uint16 us);
	RESULT (*jtag_hl_delay_ms)(uint16 ms);
	RESULT (*jtag_hl_aux_io_init)(void);
	RESULT (*jtag_hl_aux_io_fini)(void);
	RESULT (*jtag_hl_aux_io_config)(uint8 pin_mask, uint8 io);
	RESULT (*jtag_hl_aux_io_out)(uint8 pin_mask, uint8 value);
	RESULT (*jtag_hl_aux_io_in)(uint8 pin_mask, uint8 *value);
	RESULT (*jtag_hl_commit)(void);
	
	// jtag_ll
	RESULT (*jtag_ll_init)(void);
	RESULT (*jtag_ll_fini)(void);
	RESULT (*jtag_ll_set_frequency)(uint16 kHz);
	RESULT (*jtag_ll_tms)(uint8 *tms, uint8 len);
	RESULT (*jtag_ll_tms_clocks)(uint32 len, uint8 tms);
	RESULT (*jtag_ll_xr)(uint8* r, uint16 len, uint8 tms_before_valid, 
						 uint8 tms_before, uint8 tms_after0, uint8 tms_after1);
	RESULT (*jtag_ll_aux_io_init)(void);
	RESULT (*jtag_ll_aux_io_fini)(void);
	RESULT (*jtag_ll_aux_io_config)(uint8 pin_mask, uint8 io);
	RESULT (*jtag_ll_aux_io_out)(uint8 pin_mask, uint8 value);
	RESULT (*jtag_ll_aux_io_in)(uint8 pin_mask, uint8 *value);
	RESULT (*jtag_ll_commit)(void);
	
	// msp430_jtag
	RESULT (*msp430jtag_init)(void);
	RESULT (*msp430jtag_fini)(void);
	RESULT (*msp430jtag_config)(uint8 has_test);
	RESULT (*msp430jtag_ir)(uint8 *ir, uint8 want_ret);
	RESULT (*msp430jtag_dr)(uint32 *dr, uint8 len, uint8 want_ret);
	RESULT (*msp430jtag_tclk)(uint8 value);
	RESULT (*msp430jtag_tclk_strobe)(uint16 cnt);
	RESULT (*msp430jtag_reset)(void);
	RESULT (*msp430jtag_poll)(uint32 dr, uint32 mask, uint32 value, uint8 len, 
							  uint16 poll_cnt, uint8 toggle_tclk);
	
	// msp430_sbw
	RESULT (*msp430sbw_init)(void);
	RESULT (*msp430sbw_fini)(void);
	RESULT (*msp430sbw_config)(uint8 has_test);
	RESULT (*msp430sbw_ir)(uint8 *ir, uint8 want_ret);
	RESULT (*msp430sbw_dr)(uint32 *dr, uint8 len, uint8 want_ret);
	RESULT (*msp430sbw_tclk)(uint8 value);
	RESULT (*msp430sbw_tclk_strobe)(uint16 cnt);
	RESULT (*msp430sbw_reset)(void);
	RESULT (*msp430sbw_poll)(uint32 dr, uint32 mask, uint32 value, uint8 len, 
							 uint16 poll_cnt, uint8 toggle_tclk);
	
	// c2 for c8051f
	RESULT (*c2_init)(void);
	RESULT (*c2_fini)(void);
	RESULT (*c2_addr_write)(uint8 addr);
	RESULT (*c2_data_read)(uint8 *data, uint8 len);
	RESULT (*c2_data_write)(uint8 *data, uint8 len);
	RESULT (*c2_addr_poll)(uint8 mask, uint8 value, uint16 poll_cnt);
	RESULT (*c2_commit)(void);
	
	// i2c
	RESULT (*i2c_init)(void);
	RESULT (*i2c_fini)(void);
	RESULT (*i2c_set_speed)(uint16 kHz);
	RESULT (*i2c_read)(uint16 chip_addr, uint8 chip_addr_len, uint8 *data, 
					   uint16 data_len, uint8 stop);
	RESULT (*i2c_write)(uint16 chip_addr, uint8 chip_addr_len, uint8 *data, 
						uint16 data_len, uint8 stop);
	RESULT (*i2c_commit)(void);
	
	// lpcicp
	RESULT (*lpcicp_init)(void);
	RESULT (*lpcicp_fini)(void);
	RESULT (*lpcicp_enter_program_mode)(void);
	RESULT (*lpcicp_in)(uint8 *buff, uint16 len);
	RESULT (*lpcicp_out)(uint8 *buff, uint16 len);
	RESULT (*lpcicp_poll_ready)(uint8 data, uint8 *ret, uint8 setmask, 
								uint8 clearmask, uint16 pollcnt);
	RESULT (*lpcicp_commit)(void);
	
	// mass-product support
	RESULT (*query_mass_product_data_size)(uint32 *size);
	RESULT (*download_mass_product_data)(const char *name, uint8 *buffer, 
										 uint32 len);
	
	// firmware update support
	RESULT (*enter_firmware_update_mode)(void);
}programmer_info_t;

#define PROGRAMMER_DEFINE(name, parse_argument, init_capability, 	\
						  display_programmer)						\
	{\
		name, parse_argument, init_capability, display_programmer, 	\
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0\
	}

extern programmer_info_t *cur_programmer;
extern programmer_info_t programmers_info[];

void programmer_print_list(void);
void programmer_print_help(void);
RESULT programmer_init(const char *programmer, const char *app_dir);

#endif /* __PROGRAMMER_H_INCLUDED__ */


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
#ifndef __USBTOXXX_H_INCLUDED__
#define __USBTOXXX_H_INCLUDED__

RESULT usbtoxxx_execute_command(void);

// USB_TO_DELAY
RESULT usbtodelay_delay(uint16_t dly);



// USB_TO_SPI
RESULT usbtospi_init(void);
RESULT usbtospi_fini(void);
RESULT usbtospi_config(uint8_t interface_index, uint16_t freq, uint8_t cpol, 
					   uint8_t cpha, uint8_t firstbit);
RESULT usbtospi_io(uint8_t interface_index, uint8_t *out, uint8_t *in, 
				   uint16_t outlen, uint16_t inpos, uint16_t inlen);



// USB_TO_GPIO
RESULT usbtogpio_init(void);
RESULT usbtogpio_fini(void);
RESULT usbtogpio_config(uint8_t interface_index, uint16_t mask, 
						uint16_t dir_mask, uint16_t value);
RESULT usbtogpio_in(uint8_t interface_index, uint16_t mask, uint16_t *value);
RESULT usbtogpio_out(uint8_t interface_index, uint16_t mask, uint16_t value);



// USB_TO_ISSP
RESULT usbtoissp_init(void);
RESULT usbtoissp_fini(void);
RESULT usbtoissp_config(uint8_t interface_index);
RESULT usbtoissp_enter_program_mode(uint8_t interface_index, uint8_t mode);
RESULT usbtoissp_leave_program_mode(uint8_t interface_index, uint8_t mode);
RESULT usbtoissp_wait_and_poll(uint8_t interface_index);
RESULT usbtoissp_vector(uint8_t interface_index, uint8_t operate, uint8_t addr, 
						uint8_t data, uint8_t *buf);



// USB_TO_LPCICP
RESULT usbtolpcicp_init(void);
RESULT usbtolpcicp_fini(void);
RESULT usbtolpcicp_config(uint8_t interface_index);
RESULT usbtolpcicp_enter_program_mode(uint8_t interface_index);
RESULT usbtolpcicp_in(uint8_t interface_index, uint8_t *buff, uint16_t len);
RESULT usbtolpcicp_out(uint8_t interface_index, uint8_t *buff, uint16_t len);
RESULT usbtolpcicp_poll_ready(uint8_t interface_index, uint8_t *ret, uint8_t data, 
							  uint8_t setmask, uint8_t clearmask, uint16_t pollcnt);


// USB_TO_JTAG_HL
RESULT usbtojtaghl_init(void);
RESULT usbtojtaghl_fini(void);
RESULT usbtojtaghl_config(uint8_t interface_index, uint16_t kHz, uint8_t ub, 
						  uint8_t ua, uint16_t bb, uint16_t ba);
RESULT usbtojtaghl_ir(uint8_t interface_index, uint8_t *ir, uint16_t bitlen, 
					  uint8_t idle, uint8_t want_ret);
RESULT usbtojtaghl_dr(uint8_t interface_index, uint8_t *dr, uint16_t bitlen, 
					  uint8_t idle, uint8_t want_ret);
RESULT usbtojtaghl_tmsbyte(uint8_t interface_index, uint8_t *tms, uint8_t bytelen);



// USB_TO_C2
RESULT usbtoc2_init(void);
RESULT usbtoc2_fini(void);
RESULT usbtoc2_config(uint8_t interface_index);
RESULT usbtoc2_addr(uint8_t interface_index, uint8_t addr);
RESULT usbtoc2_data(uint8_t interface_index, uint8_t r, uint8_t len, uint8_t *buf);
RESULT usbtoc2_addr_poll(uint8_t interface_index, uint8_t mask, uint8_t value, 
						 uint16_t poll_cnt);



// USB_TO_I2C
RESULT usbtoi2c_init(void);
RESULT usbtoi2c_fini(void);
RESULT usbtoi2c_set_speed(uint8_t interface_index, uint16_t kHz);
RESULT usbtoi2c_read(uint8_t interface_index, uint16_t chip_addr, 
					 uint8_t chip_addr_len, uint8_t *data, uint16_t data_len, 
					 uint8_t stop);
RESULT usbtoi2c_write(uint8_t interface_index, uint16_t chip_addr, 
					  uint8_t chip_addr_len, uint8_t *data, uint16_t data_len, 
					  uint8_t stop);



// USB_TO_MSP430_JTAG
RESULT usbtomsp430jtag_init(void);
RESULT usbtomsp430jtag_fini(void);
RESULT usbtomsp430jtag_config(uint8_t interface_index, uint8_t has_test);
RESULT usbtomsp430jtag_ir(uint8_t interface_index, uint8_t *ir, uint8_t want_ret);
RESULT usbtomsp430jtag_dr(uint8_t interface_index, uint32_t *dr, uint8_t len, 
						  uint8_t want_ret);
RESULT usbtomsp430jtag_tclk(uint8_t interface_index, uint8_t value);
RESULT usbtomsp430jtag_tclk_strobe(uint8_t interface_index, uint16_t cnt);
RESULT usbtomsp430jtag_reset(uint8_t interface_index);
RESULT usbtomsp430jtag_poll(uint8_t interface_index, uint32_t dr, uint32_t mask, 
							uint32_t value, uint8_t len, uint16_t poll_cnt, 
							uint8_t toggle_tclk);



// USB_TO_MSP430_SBW
RESULT usbtomsp430sbw_init(void);
RESULT usbtomsp430sbw_fini(void);
RESULT usbtomsp430sbw_config(uint8_t interface_index);
RESULT usbtomsp430sbw_ir(uint8_t interface_index, uint8_t *ir, uint8_t want_ret);
RESULT usbtomsp430sbw_dr(uint8_t interface_index, uint32_t *dr, uint8_t len, 
						 uint8_t want_ret);
RESULT usbtomsp430sbw_tclk(uint8_t interface_index, uint8_t value);
RESULT usbtomsp430sbw_tclk_strobe(uint8_t interface_index, uint16_t cnt);
RESULT usbtomsp430sbw_reset(uint8_t interface_index);
RESULT usbtomsp430sbw_poll(uint8_t interface_index, uint32_t dr, uint32_t mask, 
						   uint32_t value, uint8_t len, uint16_t poll_cnt, 
						   uint8_t toggle_tclk);

#endif /* __USBTOXXX_H_INCLUDED__ */


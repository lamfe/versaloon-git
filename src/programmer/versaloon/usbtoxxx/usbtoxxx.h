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
RESULT usbtodelay_delay(uint16 dly);



// USB_TO_SPI
RESULT usbtospi_init(void);
RESULT usbtospi_fini(void);
RESULT usbtospi_config(uint8 interface_index, uint16 freq, uint8 cpol, 
					   uint8 cpha, uint8 firstbit);
RESULT usbtospi_io(uint8 interface_index, uint8 *out, uint8 *in, 
				   uint16 outlen, uint16 inpos, uint16 inlen);



// USB_TO_GPIO
RESULT usbtogpio_init(void);
RESULT usbtogpio_fini(void);
RESULT usbtogpio_config(uint8 interface_index, uint16 mask, uint16 direction);
RESULT usbtogpio_in(uint8 interface_index, uint16 mask, uint16 *value);
RESULT usbtogpio_out(uint8 interface_index, uint16 mask, uint16 value);



// USB_TO_ISSP
RESULT usbtoissp_init(void);
RESULT usbtoissp_fini(void);
RESULT usbtoissp_config(uint8 interface_index);
RESULT usbtoissp_enter_program_mode(uint8 interface_index, uint8 mode);
RESULT usbtoissp_leave_program_mode(uint8 interface_index, uint8 mode);
RESULT usbtoissp_wait_and_poll(uint8 interface_index);
RESULT usbtoissp_vector(uint8 interface_index, uint8 operate, uint8 addr, 
						uint8 data, uint8 *buf);



// USB_TO_LPCICP
RESULT usbtolpcicp_init(void);
RESULT usbtolpcicp_fini(void);
RESULT usbtolpcicp_config(uint8 interface_index);
RESULT usbtolpcicp_enter_program_mode(uint8 interface_index);
RESULT usbtolpcicp_in(uint8 interface_index, uint8 *buff, uint16 len);
RESULT usbtolpcicp_out(uint8 interface_index, uint8 *buff, uint16 len);
RESULT usbtolpcicp_poll_ready(uint8 interface_index);


// USB_TO_JTAG_HL
RESULT usbtojtaghl_init(void);
RESULT usbtojtaghl_fini(void);
RESULT usbtojtaghl_config(uint8 interface_index, uint16 kHz, uint8 ub, 
						  uint8 ua, uint16 bb, uint16 ba);
RESULT usbtojtaghl_ir(uint8 interface_index, uint8 *ir, uint16 bitlen, 
					  uint8 idle, uint8 want_ret);
RESULT usbtojtaghl_dr(uint8 interface_index, uint8 *dr, uint16 bitlen, 
					  uint8 idle, uint8 want_ret);
RESULT usbtojtaghl_tmsbyte(uint8 interface_index, uint8 *tms, uint8 bytelen);



// USB_TO_C2
RESULT usbtoc2_init(void);
RESULT usbtoc2_fini(void);
RESULT usbtoc2_config(uint8 interface_index);
RESULT usbtoc2_addr(uint8 interface_index, uint8 addr);
RESULT usbtoc2_data(uint8 interface_index, uint8 r, uint8 len, uint8 *buf);
RESULT usbtoc2_addr_poll(uint8 interface_index, uint8 mask, uint8 value, 
						 uint16 poll_cnt);



// USB_TO_I2C
RESULT usbtoi2c_init(void);
RESULT usbtoi2c_fini(void);
RESULT usbtoi2c_set_speed(uint8 interface_index, uint16 kHz);
RESULT usbtoi2c_read(uint8 interface_index, uint16 chip_addr, 
					 uint8 chip_addr_len, uint8 *data, uint16 data_len, 
					 uint8 stop);
RESULT usbtoi2c_write(uint8 interface_index, uint16 chip_addr, 
					  uint8 chip_addr_len, uint8 *data, uint16 data_len, 
					  uint8 stop);



// USB_TO_MSP430_JTAG
RESULT usbtomsp430jtag_init(void);
RESULT usbtomsp430jtag_fini(void);
RESULT usbtomsp430jtag_config(uint8 interface_index, uint8 has_test);
RESULT usbtomsp430jtag_ir(uint8 interface_index, uint8 *ir, uint8 want_ret);
RESULT usbtomsp430jtag_dr(uint8 interface_index, uint32 *dr, uint8 len, 
						  uint8 want_ret);
RESULT usbtomsp430jtag_tclk(uint8 interface_index, uint8 value);
RESULT usbtomsp430jtag_tclk_strobe(uint8 interface_index, uint16 cnt);
RESULT usbtomsp430jtag_reset(uint8 interface_index);
RESULT usbtomsp430jtag_poll(uint8 interface_index, uint32 dr, uint32 mask, 
							uint32 value, uint8 len, uint16 poll_cnt, 
							uint8 toggle_tclk);



// USB_TO_MSP430_SBW
RESULT usbtomsp430sbw_init(void);
RESULT usbtomsp430sbw_fini(void);
RESULT usbtomsp430sbw_config(uint8 interface_index);
RESULT usbtomsp430sbw_ir(uint8 interface_index, uint8 *ir, uint8 want_ret);
RESULT usbtomsp430sbw_dr(uint8 interface_index, uint32 *dr, uint8 len, 
						 uint8 want_ret);
RESULT usbtomsp430sbw_tclk(uint8 interface_index, uint8 value);
RESULT usbtomsp430sbw_tclk_strobe(uint8 interface_index, uint16 cnt);
RESULT usbtomsp430sbw_reset(uint8 interface_index);
RESULT usbtomsp430sbw_poll(uint8 interface_index, uint32 dr, uint32 mask, 
						   uint32 value, uint8 len, uint16 poll_cnt, 
						   uint8 toggle_tclk);

#endif /* __USBTOXXX_H_INCLUDED__ */


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



// USB_TO_USART
RESULT usbtousart_init(void);
RESULT usbtousart_fini(void);
RESULT usbtousart_config(uint8_t interface_index, uint32_t baudrate, 
			uint8_t datalength, char paritybit, char stopbit, char handshake);
RESULT usbtousart_send(uint8_t interface_index, uint8_t *buf, uint16_t len);
RESULT usbtousart_receive(uint8_t interface_index, uint8_t *buf, uint16_t len);
RESULT usbtousart_status(uint8_t interface_index, uint32_t buffer_len[2]);

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
						uint16_t dir_mask, uint16_t input_pull_mask);
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
RESULT usbtolpcicp_poll_ready(uint8_t interface_index, uint8_t *ret, 
		uint8_t data, uint8_t setmask, uint8_t clearmask, uint16_t pollcnt);



// USB_TO_JTAG_LL
RESULT usbtojtagll_init(void);
RESULT usbtojtagll_fini(void);
RESULT usbtojtagll_config(uint8_t interface_index, uint16_t kHz);
RESULT usbtojtagll_tms(uint8_t interface_index, uint8_t *tms, uint8_t bytelen);
RESULT usbtojtagll_tms_clocks(uint8_t interface_index, uint32_t bytelen, 
								uint8_t tms);
RESULT usbtojtagll_scan(uint8_t interface_index, uint8_t* r, uint16_t bitlen, 
						uint8_t tms_before_valid, uint8_t tms_before, 
						uint8_t tms_after0, uint8_t tms_after1);



// USB_TO_JTAG_HL
RESULT usbtojtaghl_init(void);
RESULT usbtojtaghl_fini(void);
RESULT usbtojtaghl_config(uint8_t interface_index, uint16_t kHz, uint8_t ub, 
						  uint8_t ua, uint16_t bb, uint16_t ba);
RESULT usbtojtaghl_ir(uint8_t interface_index, uint8_t *ir, uint16_t bitlen, 
					  uint8_t idle, uint8_t want_ret);
RESULT usbtojtaghl_dr(uint8_t interface_index, uint8_t *dr, uint16_t bitlen, 
					  uint8_t idle, uint8_t want_ret);
RESULT usbtojtaghl_tms(uint8_t interface_index, uint8_t *tms, uint16_t bitlen);
RESULT usbtojtaghl_runtest(uint8_t interface_index, uint32_t cycles);
RESULT usbtojtaghl_register_callback(jtag_callback_t send_callback, 
									 jtag_callback_t receive_callback);



// USB_TO_C2
RESULT usbtoc2_init(void);
RESULT usbtoc2_fini(void);
RESULT usbtoc2_config(uint8_t interface_index);
RESULT usbtoc2_writeaddr(uint8_t interface_index, uint8_t addr);
RESULT usbtoc2_readaddr(uint8_t interface_index, uint8_t *data);
RESULT usbtoc2_data(uint8_t interface_index, uint8_t r, uint8_t len, 
					uint8_t *buf);



// USB_TO_I2C
RESULT usbtoi2c_init(void);
RESULT usbtoi2c_fini(void);
RESULT usbtoi2c_set_speed(uint8_t interface_index, uint16_t kHz, 
							uint16_t dead_cnt, uint16_t byte_interval);
RESULT usbtoi2c_read(uint8_t interface_index, uint16_t chip_addr, 
						uint8_t *data, uint16_t data_len, uint8_t stop);
RESULT usbtoi2c_write(uint8_t interface_index, uint16_t chip_addr, 
						uint8_t *data, uint16_t data_len, uint8_t stop);



// USB_TO_MSP430_JTAG
RESULT usbtomsp430jtag_init(void);
RESULT usbtomsp430jtag_fini(void);
RESULT usbtomsp430jtag_config(uint8_t interface_index, uint8_t has_test);
RESULT usbtomsp430jtag_ir(uint8_t interface_index, uint8_t *ir, 
							uint8_t want_ret);
RESULT usbtomsp430jtag_dr(uint8_t interface_index, uint32_t *dr, uint8_t len, 
						  uint8_t want_ret);
RESULT usbtomsp430jtag_tclk(uint8_t interface_index, uint8_t value);
RESULT usbtomsp430jtag_tclk_strobe(uint8_t interface_index, uint16_t cnt);
RESULT usbtomsp430jtag_reset(uint8_t interface_index);
RESULT usbtomsp430jtag_poll(uint8_t interface_index, uint32_t dr, 
							uint32_t mask, uint32_t value, uint8_t len, 
							uint16_t poll_cnt, uint8_t toggle_tclk);



// USB_TO_MSP430_SBW
RESULT usbtomsp430sbw_init(void);
RESULT usbtomsp430sbw_fini(void);
RESULT usbtomsp430sbw_config(uint8_t interface_index);
RESULT usbtomsp430sbw_ir(uint8_t interface_index, uint8_t *ir, 
							uint8_t want_ret);
RESULT usbtomsp430sbw_dr(uint8_t interface_index, uint32_t *dr, uint8_t len, 
						 uint8_t want_ret);
RESULT usbtomsp430sbw_tclk(uint8_t interface_index, uint8_t value);
RESULT usbtomsp430sbw_tclk_strobe(uint8_t interface_index, uint16_t cnt);
RESULT usbtomsp430sbw_reset(uint8_t interface_index);
RESULT usbtomsp430sbw_poll(uint8_t interface_index, uint32_t dr, uint32_t mask, 
						   uint32_t value, uint8_t len, uint16_t poll_cnt, 
						   uint8_t toggle_tclk);



// USB_TO_POWER
RESULT usbtopwr_init(void);
RESULT usbtopwr_fini(void);
RESULT usbtopwr_config(uint8_t interface_index);
RESULT usbtopwr_output(uint8_t interface_index, uint16_t mV);



// USB_TO_POLL
RESULT usbtopoll_start(uint16_t retry, uint16_t interval_us);
RESULT usbtopoll_end(void);
RESULT usbtopoll_checkbyte(uint8_t offset, uint8_t mask, uint8_t value);
RESULT usbtopoll_checkfail(uint8_t offset, uint8_t mask, uint8_t value);



// USB_TO_SWJ
RESULT usbtoswj_init(void);
RESULT usbtoswj_fini(void);
RESULT usbtoswj_config(uint8_t interface_index, uint8_t trn, uint16_t retry, 
					   uint16_t dly);
RESULT usbtoswj_seqout(uint8_t interface_index, uint8_t *data, uint16_t bitlen);
RESULT usbtoswj_seqin(uint8_t interface_index, uint8_t *data, uint16_t bitlen);
RESULT usbtoswj_transact(uint8_t interface_index, uint8_t request, 
							uint32_t *data);
uint8_t usbtoswj_get_last_ack(void);



// USB_TO_SWIM
RESULT usbtoswim_init(void);
RESULT usbtoswim_fini(void);
RESULT usbtoswim_set_param(uint8_t interface_index, uint8_t mHz, 
							uint8_t cnt0, uint8_t cnt1);
RESULT usbtoswim_out(uint8_t interface_index, uint8_t data, uint8_t bitlen);
RESULT usbtoswim_in(uint8_t interface_index, uint8_t *data, uint8_t bytelen);
RESULT usbtoswim_sync(uint8_t interface_index, uint8_t mHz);
RESULT usbtoswim_enable(uint8_t interface_index);


#endif /* __USBTOXXX_H_INCLUDED__ */


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
#ifndef __PROGRAMMER_H_INCLUDED__
#define __PROGRAMMER_H_INCLUDED__

enum jtag_irdr_t
{
	JTAG_SCANTYPE_IR, 
	JTAG_SCANTYPE_DR
};
typedef RESULT (*jtag_callback_t)(uint8_t index, enum jtag_irdr_t cmd, 
									uint32_t ir, uint8_t *dest_buffer, 
									uint8_t *src_buffer, uint16_t buffer_len, 
									uint16_t *processed);

struct usart_status_t
{
	uint32_t tx_buff_avail;
	uint32_t tx_buff_size;
	uint32_t rx_buff_avail;
	uint32_t rx_buff_size;
};

struct interface_usart_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint32_t baudrate, uint8_t datalength, 
						char paritybit, char stopbit, char handshake);
	RESULT (*send)(uint8_t index, uint8_t *buf, uint16_t len);
	RESULT (*receive)(uint8_t index, uint8_t *buf, uint16_t len);
	RESULT (*status)(uint8_t index, struct usart_status_t *status);
};

struct interface_spi_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t cpol, uint8_t cpha, 
						uint8_t first_bit);
	RESULT (*io)(uint8_t index, uint8_t *out, uint8_t *in, uint16_t len, 
					uint16_t inpos, uint16_t inlen);
};

struct interface_gpio_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t pin_mask, uint16_t io, 
						uint16_t input_pull_mask);
	RESULT (*out)(uint8_t index, uint16_t pin_mask, uint16_t value);
	RESULT (*in)(uint8_t index, uint16_t pin_mask, uint16_t *value);
};

struct interface_delay_t
{
	RESULT (*delayms)(uint16_t ms);
	RESULT (*delayus)(uint16_t us);
};

struct interface_issp_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*enter_program_mode)(uint8_t index, uint8_t mode);
	RESULT (*leave_program_mode)(uint8_t index, uint8_t mode);
	RESULT (*wait_and_poll)(uint8_t index);
	RESULT (*vector)(uint8_t index, uint8_t operate, uint8_t addr, 
						uint8_t data, uint8_t *buf);
};

struct interface_swd_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint8_t trn, uint16_t retry, uint16_t dly);
	RESULT (*seqout)(uint8_t index, uint8_t *data, uint16_t bitlen);
	RESULT (*seqin)(uint8_t index, uint8_t *data, uint16_t bitlen);
	RESULT (*transact)(uint8_t index, uint8_t request, uint32_t *data, 
						uint8_t *ack);
};

struct interface_jtag_hl_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t ub, uint8_t ua, 
						uint16_t bb, uint16_t ba);
	RESULT (*tms)(uint8_t index, uint8_t* tms, uint16_t bitlen);
	RESULT (*runtest)(uint8_t index, uint32_t cycles);
	RESULT (*ir)(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle);
	RESULT (*dr)(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle);
	RESULT (*register_callback)(uint8_t index, jtag_callback_t send_callback, 
										jtag_callback_t receive_callback);
};

struct interface_jtag_ll_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz);
	RESULT (*tms)(uint8_t index, uint8_t *tms, uint8_t bytelen);
	RESULT (*tms_clocks)(uint8_t index, uint32_t bytelen, uint8_t tms);
	RESULT (*scan)(uint8_t index, uint8_t* data, uint16_t bitlen, 
					uint8_t tms_before_valid, uint8_t tms_before, 
					uint8_t tms_after0, uint8_t tms_after1);
};

struct interface_jtag_raw_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz);
	RESULT (*execute)(uint8_t index, uint8_t* tdi, uint8_t* tms, 
						uint8_t *tdo, uint32_t bitlen);
};

struct interface_msp430jtag_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint8_t has_test);
	RESULT (*ir)(uint8_t index, uint8_t *ir);
	RESULT (*dr)(uint8_t index, uint32_t *dr, uint8_t bitlen);
	RESULT (*tclk)(uint8_t index, uint8_t value);
	RESULT (*tclk_strobe)(uint8_t index, uint16_t cnt);
	RESULT (*reset)(uint8_t index);
	RESULT (*poll)(uint8_t index, uint32_t dr, uint32_t mask, uint32_t value, 
					uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk);
};

struct interface_msp430sbw_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint8_t has_test);
	RESULT (*ir)(uint8_t index, uint8_t *ir);
	RESULT (*dr)(uint8_t index, uint32_t *dr, uint8_t len);
	RESULT (*tclk)(uint8_t index, uint8_t value);
	RESULT (*tclk_strobe)(uint8_t index, uint16_t cnt);
	RESULT (*reset)(uint8_t index);
	RESULT (*poll)(uint8_t index, uint32_t dr, uint32_t mask, uint32_t value, 
					uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk);
};

struct interface_c2_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*addr_write)(uint8_t index, uint8_t addr);
	RESULT (*addr_read)(uint8_t index, uint8_t *data);
	RESULT (*data_read)(uint8_t index, uint8_t *data, uint8_t len);
	RESULT (*data_write)(uint8_t index, uint8_t *data, uint8_t len);
};

struct interface_i2c_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint16_t byte_interval, 
						uint16_t max_dly);
	RESULT (*read)(uint8_t index, uint16_t chip_addr, uint8_t *data, 
					uint16_t data_len, uint8_t stop);
	RESULT (*write)(uint8_t index, uint16_t chip_addr, uint8_t *data, 
					uint16_t data_len, uint8_t stop);
};

struct interface_lpcicp_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*enter_program_mode)(uint8_t index);
	RESULT (*in)(uint8_t index, uint8_t *buff, uint16_t len);
	RESULT (*out)(uint8_t index, uint8_t *buff, uint16_t len);
	RESULT (*poll_ready)(uint8_t index, uint8_t data, uint8_t *ret, 
							uint8_t setmask, uint8_t clearmask, 
							uint16_t pollcnt);
};

struct interface_swim_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint8_t mHz, uint8_t cnt0, uint8_t cnt1);
	RESULT (*srst)(uint8_t index);
	RESULT (*wotf)(uint8_t index, uint8_t *data, uint16_t bytelen, 
					uint32_t addr);
	RESULT (*rotf)(uint8_t index, uint8_t *data, uint16_t bytelen, 
					uint32_t addr);
	RESULT (*sync)(uint8_t index, uint8_t mHz);
	RESULT (*enable)(uint8_t index);
};

struct interface_bdm_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*sync)(uint8_t index, uint16_t *khz);
	RESULT (*transact)(uint8_t index, uint8_t *out, uint8_t outlen, 
						uint8_t *in, uint8_t inlen, uint8_t delay, 
						uint8_t ack);
};

struct interface_target_voltage_t
{
	RESULT (*get)(uint8_t index, uint16_t *voltage);
	RESULT (*set)(uint8_t index, uint16_t voltage);
};

enum poll_check_type_t
{
	POLL_CHECK_EQU,
	POLL_CHECK_UNEQU
};
struct interface_poll_t
{
	RESULT (*start)(uint16_t retry, uint16_t interval_us);
	RESULT (*end)(void);
	RESULT (*checkok)(enum poll_check_type_t type, uint16_t offset, 
						uint8_t size, uint32_t mask, uint32_t value);
	RESULT (*checkfail)(enum poll_check_type_t type, uint16_t offset, 
						uint8_t size, uint32_t mask, uint32_t value);
	RESULT (*verifybuff)(uint16_t offset, uint16_t size, uint8_t *buff);
};

struct interfaces_info_t
{
	struct interface_target_voltage_t target_voltage;
	struct interface_usart_t usart;
	struct interface_spi_t spi;
	struct interface_gpio_t gpio;
	struct interface_delay_t delay;
	struct interface_issp_t issp;
	struct interface_swd_t swd;
	struct interface_jtag_hl_t jtag_hl;
	struct interface_jtag_ll_t jtag_ll;
	struct interface_jtag_raw_t jtag_raw;
	struct interface_msp430jtag_t msp430jtag;
	struct interface_msp430sbw_t msp430sbw;
	struct interface_c2_t c2;
	struct interface_i2c_t i2c;
	struct interface_lpcicp_t lpcicp;
	struct interface_swim_t swim;
	struct interface_bdm_t bdm;
	struct interface_poll_t poll;
	RESULT (*peripheral_commit)(void);
};

struct programmer_info_t
{
	// the 3 element listed below MUST be define valid
	// other functions can be initialized in .init_ability()
	char *name;
	const struct misc_cmd_t *notifier;
	RESULT (*init_capability)(void *p);
	uint32_t (*display_programmer)(void);
	
	// init and fini
	RESULT (*init)(void);
	RESULT (*fini)(void);
	
	// interfaces supported
	uint32_t interfaces_mask;
	
	// peripheral
	struct interfaces_info_t interfaces;
	
	// mass-product support
	RESULT (*query_mass_product_data_size)(uint32_t *size);
	RESULT (*download_mass_product_data)(const char *name, uint8_t *buffer, 
										 uint32_t len);
	
	// firmware update support
	RESULT (*enter_firmware_update_mode)(void);
};

#define PROGRAMMER_DEFINE(name, parse_argument, init_capability, 	\
						  display_programmer)						\
	{\
		name, parse_argument, init_capability, display_programmer, 	\
		0, 0, 0, \
		{\
			{0, 0},\
			{0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0},\
			{0, 0, 0, 0, 0},\
			{0, 0},\
			{0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0, 0, 0, 0, 0},\
			{0, 0, 0, 0},\
			{0, 0, 0, 0, 0},\
			0\
		},\
		0, 0, 0\
	}

extern struct programmer_info_t *cur_programmer;
extern struct programmer_info_t programmers_info[];

void programmer_print_list(void);
void programmer_print_help(void);
RESULT programmer_init(const char *programmer);
RESULT programmer_run_script(char *cmd);
RESULT programmer_assert(struct programmer_info_t **prog);

#endif /* __PROGRAMMER_H_INCLUDED__ */


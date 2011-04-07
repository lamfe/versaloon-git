/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces.h                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    interfaces header file                                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#ifndef __INTERFACES_H__
#define __INTERFACES_H__

#include "interfaces_const.h"

typedef enum result_s
{
	ERROR_OK = 0,
	ERROR_FAIL = 1
} RESULT;

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

#define SPI_CPOL_MASK			0x20
#define SPI_CPOL_HIGH			0x20
#define SPI_CPOL_LOW			0x00
#define SPI_CPHA_MASK			0x40
#define SPI_CPHA_2EDGE			0x40
#define SPI_CPHA_1EDGE			0x00
#define SPI_FIRSTBIT_MASK		0x80
#define SPI_MSB_FIRST			0x80
#define SPI_LSB_FIRST			0x00
struct interface_spi_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t cpol, uint8_t cpha, 
					 uint8_t first_bit);
	RESULT (*io)(uint8_t index, uint8_t *out, uint8_t *in, uint16_t len);
};

struct interface_gpio_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t pin_mask, uint16_t io, 
						uint16_t pull_en_mask, uint16_t input_pull_mask);
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

enum jtag_irdr_t
{
	JTAG_SCANTYPE_IR,
	JTAG_SCANTYPE_DR
};
typedef RESULT (*jtag_callback_t)(uint8_t index, enum jtag_irdr_t cmd, 
								  uint32_t ir, uint8_t *dest_buffer, 
								  uint8_t *src_buffer, uint16_t bytelen, 
								  uint16_t *processed);
struct interface_jtag_hl_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config_speed)(uint8_t index, uint16_t kHz);
	RESULT (*config_daisychain)(uint8_t index, uint8_t ub, uint8_t ua, 
									uint16_t bb, uint16_t ba);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t ub, uint8_t ua, 
					 uint16_t bb, uint16_t ba);
	RESULT (*tms)(uint8_t index, uint8_t* tms, uint16_t bitlen);
	RESULT (*runtest)(uint8_t index, uint32_t cycles);
	RESULT (*ir)(uint8_t index, uint8_t *ir, uint16_t bitlen, uint8_t idle, 
				 uint8_t want_ret);
	RESULT (*dr)(uint8_t index, uint8_t *dr, uint16_t bitlen, uint8_t idle, 
				 uint8_t want_ret);
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
	RESULT (*ir)(uint8_t index, uint8_t *ir, uint8_t want_ret);
	RESULT (*dr)(uint8_t index, uint32_t *dr, uint8_t bitlen, uint8_t want_ret);
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
	RESULT (*ir)(uint8_t index, uint8_t *ir, uint8_t want_ret);
	RESULT (*dr)(uint8_t index, uint32_t *dr, uint8_t len, uint8_t want_ret);
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
	RESULT (*data_write)(uint8_t index, uint8_t *data, uint8_t len);
	RESULT (*data_read)(uint8_t index, uint8_t *data, uint8_t len);
};

struct interface_i2c_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint16_t byte_interval, 
					 uint16_t max_dly);
	RESULT (*read)(uint8_t index, uint16_t chip_addr, uint8_t *data, 
				   uint16_t data_len, uint8_t stop, bool nacklast);
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

struct interface_dusi_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t cpol, uint8_t cpha, 
					 uint8_t first_bit);
	RESULT (*io)(uint8_t index, uint8_t *mo, uint8_t *mi, uint8_t *so, 
				 uint8_t *si, uint32_t bitlen);
};

struct interface_microwire_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t sel_polarity);
	RESULT (*transport)(uint8_t index, 
						uint32_t opcode, uint8_t opcode_bitlen, 
						uint32_t addr, uint8_t addr_bitlen, 
						uint32_t data, uint8_t data_bitlen, 
						uint8_t *reply, uint8_t reply_bitlen);
	RESULT (*poll)(uint8_t index, uint16_t interval_us, uint16_t retry_cnt);
};

struct interface_pwm_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t pushpull, uint8_t polarity);
	RESULT (*out)(uint8_t index, uint16_t count, uint16_t *rate);
	RESULT (*in)(uint8_t index, uint16_t count, uint16_t *rate);
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

struct interfaces_info_t
{
	RESULT (*init)(void *p);
	RESULT (*fini)(void);
	
	uint64_t support_mask;
#if POWER_OUT_EN
	struct interface_target_voltage_t target_voltage;
#endif
#if	INTERFACE_USART_EN
	struct interface_usart_t usart;
#endif
#if	INTERFACE_SPI_EN
	struct interface_spi_t spi;
#endif
#if	INTERFACE_GPIO_EN
	struct interface_gpio_t gpio;
#endif
// 	Allways included
	struct interface_delay_t delay;
//
#if	INTERFACE_ISSP_EN
	struct interface_issp_t issp;
#endif
#if	INTERFACE_SWD_EN
	struct interface_swd_t swd;
#endif
#if	INTERFACE_JTAG_EN
	struct interface_jtag_hl_t jtag_hl;
#endif
#if	INTERFACE_JTAG_EN
	struct interface_jtag_ll_t jtag_ll;
#endif
#if	INTERFACE_JTAG_EN
	struct interface_jtag_raw_t jtag_raw;
#endif
#if	INTERFACE_MSP430_JTAG_EN
	struct interface_msp430jtag_t msp430jtag;
#endif
#if	INTERFACE_MSP430_SBW_EN
	struct interface_msp430sbw_t msp430sbw;
#endif
#if	INTERFACE_C2_EN
	struct interface_c2_t c2;
#endif
#if	INTERFACE_IIC_EN
	struct interface_i2c_t i2c;
#endif
#if	INTERFACE_LPC_ICP_EN
	struct interface_lpcicp_t lpcicp;
#endif
#if	INTERFACE_SWIM_EN
	struct interface_swim_t swim;
#endif
#if	INTERFACE_BDM_EN
	struct interface_bdm_t bdm;
#endif
#if	INTERFACE_DUSI_EN
	struct interface_dusi_t dusi;
#endif
#if INTERFACE_MICROWIRE_EN
	struct interface_microwire_t microwire;
#endif
#if INTERFACE_PWM_EN
	struct interface_pwm_t pwm;
#endif
	RESULT (*peripheral_commit)(void);
};

extern const struct interfaces_info_t *interfaces;

#endif	// __INTERFACES_H__

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

#ifndef __INTERFACES_H_INCLUDED__
#define __INTERFACES_H_INCLUDED__

#include "app_type.h"
#include "interfaces/interfaces_const.h"

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
	RESULT (*poll)(uint8_t index);
};

struct interface_spi_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint32_t kHz, uint8_t cpol, uint8_t cpha, 
						uint8_t first_bit);
	RESULT (*io)(uint8_t index, uint8_t *out, uint8_t *in, uint16_t len);
};

struct interface_gpio_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint32_t pin_mask, uint32_t io, 
						uint32_t pull_en_mask, uint32_t input_pull_mask);
	RESULT (*out)(uint8_t index, uint32_t pin_mask, uint32_t value);
	RESULT (*in)(uint8_t index, uint32_t pin_mask, uint32_t *value);
};

struct interface_delay_t
{
	RESULT (*delayms)(uint16_t ms);
	RESULT (*delayus)(uint16_t us);
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

struct interface_pwm_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint16_t kHz, uint8_t pushpull, uint8_t polarity);
	RESULT (*out)(uint8_t index, uint16_t count, uint16_t *rate);
	RESULT (*in)(uint8_t index, uint16_t count, uint16_t *rate);
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

struct interface_timer_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, uint32_t kHz, uint32_t mode, 
						void (*overflow)(void));
	RESULT (*start)(uint8_t index);
	RESULT (*stop)(uint8_t index);
	RESULT (*get_count)(uint8_t index, uint32_t *count);
	RESULT (*set_count)(uint8_t index, uint32_t count);
	
	RESULT (*config_channel)(uint8_t index, uint8_t channel, uint32_t mode, 
								void (*callback)(void));
	RESULT (*get_channel)(uint8_t index, uint8_t channel, uint32_t *count);
	RESULT (*set_channel)(uint8_t index, uint8_t channel, uint32_t count);
};

struct interface_eint_t
{
	RESULT (*init)(uint8_t index);
	RESULT (*fini)(uint8_t index);
	RESULT (*config)(uint8_t index, bool on_fall, bool on_rise, 
						void (*callback)(void));
	RESULT (*enable)(uint8_t index);
	RESULT (*disable)(uint8_t index);
	RESULT (*trigger)(uint8_t index);
};

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

enum usb_ep_state_t
{
	USB_EP_STAT_STALL,
	USB_EP_STAT_ACK,
	USB_EP_STAT_NACK,
	USB_EP_STAT_DIS,
};

enum usb_ep_type_t
{
	USB_EP_TYPE_CONTROL,
	USB_EP_TYPE_INTERRUPT,
	USB_EP_TYPE_BULK,
	USB_EP_TYPE_ISO
};

struct interface_usbd_t
{
	RESULT (*init)(void *device);
	RESULT (*fini)(void);
	RESULT (*reset)(void);
	RESULT (*poll)(void);
	
	RESULT (*connect)(void);
	RESULT (*disconnect)(void);
	
	RESULT (*set_address)(uint8_t addr);
	uint8_t (*get_address)(void);
	
	RESULT (*suspend)(void);
	RESULT (*resume)(void);
	RESULT (*lowpower)(uint8_t level);
	
	uint32_t (*get_frame_number)(void);
	
	struct usbd_endpoint_t
	{
		const uint8_t *num_of_ep;
		
		RESULT (*reset)(uint8_t idx);
		RESULT (*set_type)(uint8_t idx, enum usb_ep_type_t type);
		enum usb_ep_type_t (*get_type)(uint8_t idx);
		
		RESULT (*set_IN_handler)(uint8_t idx, vsfusbd_IN_hanlder_t handler);
		RESULT (*set_IN_dbuffer)(uint8_t idx);
		bool (*is_IN_dbuffer)(uint8_t idx);
		RESULT (*switch_IN_buffer)(uint8_t idx);
		RESULT (*set_IN_epsize)(uint8_t idx, uint16_t size);
		uint16_t (*get_IN_epsize)(uint8_t idx);
		RESULT (*reset_IN_toggle)(uint8_t idx);
		RESULT (*toggle_IN_toggle)(uint8_t idx);
		enum usb_ep_state_t (*get_IN_state)(uint8_t idx);
		RESULT (*set_IN_state)(uint8_t idx, enum usb_ep_state_t state);
		RESULT (*set_IN_count)(uint8_t idx, uint16_t size);
		RESULT (*write_IN_buffer)(uint8_t idx, uint8_t *buffer, uint16_t size);
		
		RESULT (*set_OUT_handler)(uint8_t idx, vsfusbd_OUT_hanlder_t handler);
		RESULT (*set_OUT_dbuffer)(uint8_t idx);
		bool (*is_OUT_dbuffer)(uint8_t idx);
		RESULT (*switch_OUT_buffer)(uint8_t idx);
		RESULT (*set_OUT_epsize)(uint8_t idx, uint16_t size);
		uint16_t (*get_OUT_epsize)(uint8_t idx);
		RESULT (*reset_OUT_toggle)(uint8_t idx);
		RESULT (*toggle_OUT_toggle)(uint8_t idx);
		enum usb_ep_state_t (*get_OUT_state)(uint8_t idx);
		RESULT (*set_OUT_state)(uint8_t idx, enum usb_ep_state_t state);
		uint16_t (*get_OUT_count)(uint8_t idx);
		RESULT (*read_OUT_buffer)(uint8_t idx, uint8_t *buffer, uint16_t size);
	} ep;
};




#define CORE_INFO(m)					__CONNECT(m, _info)
#define	CORE_INFO_TYPE(m)				__CONNECT(m, _info_t)

#define CORE_INIT(m)					__CONNECT(m, _interface_init)
#define CORE_FINI(m)					__CONNECT(m, _interface_fini)

// GPIO
#define CORE_GPIO_INIT(m)				__CONNECT(m, _gpio_init)
#define CORE_GPIO_FINI(m)				__CONNECT(m, _gpio_fini)
#define CORE_GPIO_CONFIG(m)				__CONNECT(m, _gpio_config)
#define CORE_GPIO_IN(m)					__CONNECT(m, _gpio_in)
#define CORE_GPIO_OUT(m)				__CONNECT(m, _gpio_out)

// SPI
#define CORE_SPI_INIT(m)				__CONNECT(m, _spi_init)
#define CORE_SPI_FINI(m)				__CONNECT(m, _spi_fini)
#define CORE_SPI_CONFIG(m)				__CONNECT(m, _spi_config)
#define CORE_SPI_IO(m)					__CONNECT(m, _spi_io)

// Delay
#define CORE_DELAY_DELAYMS(m)			__CONNECT(m, _delay_delayms)
#define CORE_DELAY_DELAYUS(m)			__CONNECT(m, _delay_delayus)

// TIMER
#define CORE_TIMER_INIT(m)				__CONNECT(m, _timer_init)
#define CORE_TIMER_FINI(m)				__CONNECT(m, _timer_fini)
#define CORE_TIMER_CONFIG(m)			__CONNECT(m, _timer_config)
#define CORE_TIMER_START(m)				__CONNECT(m, _timer_start)
#define CORE_TIMER_STOP(m)				__CONNECT(m, _timer_stop)
#define CORE_TIMER_GET_COUNT(m)			__CONNECT(m, _timer_get_count)
#define CORE_TIMER_SET_COUNT(m)			__CONNECT(m, _timer_set_count)
#define CORE_TIMER_CONFIG_CHANNEL(m)	__CONNECT(m, _timer_config_channel)
#define CORE_TIMER_GET_CHANNEL(m)		__CONNECT(m, _timer_get_channel)
#define CORE_TIMER_SET_CHANNEL(m)		__CONNECT(m, _timer_set_channel)

// EINT
#define CORE_EINT_INIT(m)				__CONNECT(m, _eint_init)
#define CORE_EINT_FINI(m)				__CONNECT(m, _eint_fini)
#define CORE_EINT_CONFIG(m)				__CONNECT(m, _eint_config)
#define CORE_EINT_ENABLE(m)				__CONNECT(m, _eint_enable)
#define CORE_EINT_DISABLE(m)			__CONNECT(m, _eint_disable)
#define CORE_EINT_TRIGGER(m)			__CONNECT(m, _eint_trigger)

// USB
#define CORE_USBD_INIT(m)				__CONNECT(m, _usbd_init)
#define CORE_USBD_FINI(m)				__CONNECT(m, _usbd_fini)
#define CORE_USBD_RESET(m)				__CONNECT(m, _usbd_reset)
#define CORE_USBD_POLL(m)				__CONNECT(m, _usbd_poll)
#define CORE_USBD_CONNECT(m)			__CONNECT(m, _usbd_connect)
#define CORE_USBD_DISCONNECT(m)			__CONNECT(m, _usbd_disconnect)
#define CORE_USBD_SET_ADDRESS(m)		__CONNECT(m, _usbd_set_address)
#define CORE_USBD_GET_ADDRESS(m)		__CONNECT(m, _usbd_get_address)
#define CORE_USBD_SUSPEND(m)			__CONNECT(m, _usbd_suspend)
#define CORE_USBD_RESUME(m)				__CONNECT(m, _usbd_resume)
#define CORE_USBD_LOWPOWER(m)			__CONNECT(m, _usbd_lowpower)
#define CORE_USBD_GET_FRAME_NUM(m)		__CONNECT(m, _usbd_get_frame_number)
#define CORE_USBD_EP_NUM(m)				__CONNECT(m, _usbd_ep_num)
#define CORE_USBD_EP_RESET(m)			__CONNECT(m, _usbd_ep_reset)
#define CORE_USBD_EP_SET_TYPE(m)		__CONNECT(m, _usbd_ep_set_type)
#define CORE_USBD_EP_GET_TYPE(m)		__CONNECT(m, _usbd_ep_get_type)
#define CORE_USBD_EP_SET_IN_HANDLER(m)	__CONNECT(m, _usbd_ep_set_IN_handler)
#define CORE_USBD_EP_SET_IN_DBUFFER(m)	__CONNECT(m, _usbd_ep_set_IN_dbuffer)
#define CORE_USBD_EP_IS_IN_DBUFFER(m)	__CONNECT(m, _usbd_ep_is_IN_dbuffer)
#define CORE_USBD_EP_SWITCH_IN_BUFFER(m)\
										__CONNECT(m, _usbd_ep_switch_IN_buffer)
#define CORE_USBD_EP_SET_IN_EPSIZE(m)	__CONNECT(m, _usbd_ep_set_IN_epsize)
#define CORE_USBD_EP_GET_IN_EPSIZE(m)	__CONNECT(m, _usbd_ep_get_IN_epsize)
#define CORE_USBD_EP_RESET_IN_TOGGLE(m)	__CONNECT(m, _usbd_ep_reset_IN_toggle)
#define CORE_USBD_EP_TOGGLE_IN_TOGGLE(m)\
										__CONNECT(m, _usbd_ep_toggle_IN_toggle)
#define CORE_USBD_EP_GET_IN_STATE(m)	__CONNECT(m, _usbd_ep_get_IN_state)
#define CORE_USBD_EP_SET_IN_STATE(m)	__CONNECT(m, _usbd_ep_set_IN_state)
#define CORE_USBD_EP_SET_IN_COUNT(m)	__CONNECT(m, _usbd_ep_set_IN_count)
#define CORE_USBD_EP_WRITE_IN_BUFFER(m)	__CONNECT(m, _usbd_ep_write_IN_buffer)
#define CORE_USBD_EP_SET_OUT_HANDLER(m)	__CONNECT(m, _usbd_ep_set_OUT_handler)
#define CORE_USBD_EP_SET_OUT_DBUFFER(m)	__CONNECT(m, _usbd_ep_set_OUT_dbuffer)
#define CORE_USBD_EP_IS_OUT_DBUFFER(m)	__CONNECT(m, _usbd_ep_is_OUT_dbuffer)
#define CORE_USBD_EP_SWITCH_OUT_BUFFER(m)\
										__CONNECT(m, _usbd_ep_switch_OUT_buffer)
#define CORE_USBD_EP_SET_OUT_EPSIZE(m)	__CONNECT(m, _usbd_ep_set_OUT_epsize)
#define CORE_USBD_EP_GET_OUT_EPSIZE(m)	__CONNECT(m, _usbd_ep_get_OUT_epsize)
#define CORE_USBD_EP_RESET_OUT_TOGGLE(m)\
										__CONNECT(m, _usbd_ep_reset_OUT_toggle)
#define CORE_USBD_EP_TOGGLE_OUT_TOGGLE(m)\
										__CONNECT(m, _usbd_ep_toggle_OUT_toggle)
#define CORE_USBD_EP_GET_OUT_STATE(m)	__CONNECT(m, _usbd_ep_get_OUT_state)
#define CORE_USBD_EP_SET_OUT_STATE(m)	__CONNECT(m, _usbd_ep_set_OUT_state)
#define CORE_USBD_EP_GET_OUT_COUNT(m)	__CONNECT(m, _usbd_ep_get_OUT_count)
#define CORE_USBD_EP_READ_OUT_BUFFER(m)	__CONNECT(m, _usbd_ep_read_OUT_buffer)

// extern drivers
extern const struct CORE_INFO_TYPE(__TARGET_CHIP__) CORE_INFO(__TARGET_CHIP__);
RESULT CORE_INIT(__TARGET_CHIP__)(void *p);
RESULT CORE_FINI(__TARGET_CHIP__)(void);
// GPIO
RESULT CORE_GPIO_INIT(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_GPIO_FINI(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_GPIO_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask, 
		uint32_t io, uint32_t pull_en_mask, uint32_t input_pull_mask);
RESULT CORE_GPIO_IN(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask, 
		uint32_t *value);
RESULT CORE_GPIO_OUT(__TARGET_CHIP__)(uint8_t index, uint32_t pin_mask, 
		uint32_t value);

// SPI
RESULT CORE_SPI_INIT(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_SPI_FINI(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_SPI_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t kHz, 
								uint8_t cpol, uint8_t cpha, uint8_t first_bit);
RESULT CORE_SPI_IO(__TARGET_CHIP__)(uint8_t index, uint8_t *out, uint8_t *in, 
									uint16_t len);

// Delay
RESULT CORE_DELAY_DELAYMS(__TARGET_CHIP__)(uint16_t ms);
RESULT CORE_DELAY_DELAYUS(__TARGET_CHIP__)(uint16_t us);

// TIMER
RESULT CORE_TIMER_INIT(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_TIMER_FINI(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_TIMER_CONFIG(__TARGET_CHIP__)(uint8_t index, uint32_t kHz, 
										uint32_t mode, void (*overflow)(void));
RESULT CORE_TIMER_START(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_TIMER_STOP(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_TIMER_GET_COUNT(__TARGET_CHIP__)(uint8_t index, uint32_t *count);
RESULT CORE_TIMER_SET_COUNT(__TARGET_CHIP__)(uint8_t index, uint32_t count);
RESULT CORE_TIMER_CONFIG_CHANNEL(__TARGET_CHIP__)(uint8_t index, 
						uint8_t channel, uint32_t mode, void (*callback)(void));
RESULT CORE_TIMER_GET_CHANNEL(__TARGET_CHIP__)(uint8_t index, uint8_t channel, 
												uint32_t *count);
RESULT CORE_TIMER_SET_CHANNEL(__TARGET_CHIP__)(uint8_t index, uint8_t channel, 
												uint32_t count);

// EINT
RESULT CORE_EINT_INIT(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_EINT_FINI(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_EINT_CONFIG(__TARGET_CHIP__)(uint8_t index, bool on_fall, 
										bool on_rise, void (*callback)(void));
RESULT CORE_EINT_ENABLE(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_EINT_DISABLE(__TARGET_CHIP__)(uint8_t index);
RESULT CORE_EINT_TRIGGER(__TARGET_CHIP__)(uint8_t index);

// USB
RESULT CORE_USBD_INIT(__TARGET_CHIP__)(void *device);
RESULT CORE_USBD_FINI(__TARGET_CHIP__)(void);
RESULT CORE_USBD_RESET(__TARGET_CHIP__)(void);
RESULT CORE_USBD_POLL(__TARGET_CHIP__)(void);
RESULT CORE_USBD_CONNECT(__TARGET_CHIP__)(void);
RESULT CORE_USBD_DISCONNECT(__TARGET_CHIP__)(void);
RESULT CORE_USBD_SET_ADDRESS(__TARGET_CHIP__)(uint8_t addr);
uint8_t CORE_USBD_GET_ADDRESS(__TARGET_CHIP__)(void);
RESULT CORE_USBD_SUSPEND(__TARGET_CHIP__)(void);
RESULT CORE_USBD_RESUME(__TARGET_CHIP__)(void);
RESULT CORE_USBD_LOWPOWER(__TARGET_CHIP__)(uint8_t level);
uint32_t CORE_USBD_GET_FRAME_NUM(__TARGET_CHIP__)(void);
extern const uint8_t CORE_USBD_EP_NUM(__TARGET_CHIP__);
RESULT CORE_USBD_EP_RESET(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SET_TYPE(__TARGET_CHIP__)(uint8_t idx, 
		enum usb_ep_type_t type);
enum usb_ep_type_t CORE_USBD_EP_GET_TYPE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SET_IN_HANDLER(__TARGET_CHIP__)(uint8_t idx, 
		vsfusbd_IN_hanlder_t handler);
RESULT CORE_USBD_EP_SET_IN_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
bool CORE_USBD_EP_IS_IN_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SWITCH_IN_BUFFER(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SET_IN_EPSIZE(__TARGET_CHIP__)(uint8_t idx, uint16_t size);
uint16_t CORE_USBD_EP_GET_IN_EPSIZE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_RESET_IN_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_TOGGLE_IN_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
enum usb_ep_state_t CORE_USBD_EP_GET_IN_STATE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SET_IN_STATE(__TARGET_CHIP__)(uint8_t idx, 
		enum usb_ep_state_t state);
RESULT CORE_USBD_EP_SET_IN_COUNT(__TARGET_CHIP__)(uint8_t idx, uint16_t size);
RESULT CORE_USBD_EP_WRITE_IN_BUFFER(__TARGET_CHIP__)(uint8_t idx, 
		uint8_t *buffer, uint16_t size);
RESULT CORE_USBD_EP_SET_OUT_HANDLER(__TARGET_CHIP__)(uint8_t idx, 
		vsfusbd_OUT_hanlder_t handler);
RESULT CORE_USBD_EP_SET_OUT_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
bool CORE_USBD_EP_IS_OUT_DBUFFER(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SWITCH_OUT_BUFFER(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SET_OUT_EPSIZE(__TARGET_CHIP__)(uint8_t idx, uint16_t size);
uint16_t CORE_USBD_EP_GET_OUT_EPSIZE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_RESET_OUT_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_TOGGLE_OUT_TOGGLE(__TARGET_CHIP__)(uint8_t idx);
enum usb_ep_state_t CORE_USBD_EP_GET_OUT_STATE(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_SET_OUT_STATE(__TARGET_CHIP__)(uint8_t idx, 
		enum usb_ep_state_t state);
uint16_t CORE_USBD_EP_GET_OUT_COUNT(__TARGET_CHIP__)(uint8_t idx);
RESULT CORE_USBD_EP_READ_OUT_BUFFER(__TARGET_CHIP__)(uint8_t idx, 
		uint8_t *buffer, uint16_t size);

struct interfaces_info_t
{
	void *info;
	
	RESULT (*init)(void *p);
	RESULT (*fini)(void);
	
	struct interface_gpio_t gpio;
	struct interface_timer_t timer;
	struct interface_eint_t eint;
	struct interface_usart_t usart;
	struct interface_spi_t spi;
	struct interface_i2c_t i2c;
	struct interface_usbd_t usbd;
	struct interface_pwm_t pwm;
	struct interface_microwire_t microwire;
	struct interface_delay_t delay;
	RESULT (*peripheral_commit)(void);
};

extern const struct interfaces_info_t core_interfaces;
extern struct interfaces_info_t *interfaces;

#endif	// __INTERFACES_H_INCLUDED__

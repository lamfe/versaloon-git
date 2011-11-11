#ifndef __VSFUSBD_CDC_H_INCLUDED__
#define __VSFUSBD_CDC_H_INCLUDED__

#include "dal/usart_stream/usart_stream.h"

struct vsfusbd_CDC_line_coding_t
{
	uint32_t bitrate;
	uint8_t stopbittype;
	uint8_t paritytype;
	uint8_t datatype;
};

#define USBCDC_CONTROLLINE_RTS			0x02
#define USBCDC_CONTROLLINE_DTR			0x01
#define USBCDC_CONTROLLINE_MASK			0x03

enum usb_CDC_req_t
{
	USB_CDCREQ_SEND_ENCAPSULATED_COMMAND	= 0x00,
	USB_CDCREQ_GET_ENCAPSULATED_RESPONSE	= 0x01,
	USB_CDCREQ_SET_COMM_FEATURE				= 0x02,
	USB_CDCREQ_GET_COMM_FEATURE				= 0x03,
	USB_CDCREQ_CLEAR_COMM_FEATURE			= 0x04,
	USB_CDCREQ_SET_LINE_CODING				= 0x20,
	USB_CDCREQ_GET_LINE_CODING				= 0x21,
	USB_CDCREQ_SET_CONTROL_LINE_STATE		= 0x22,
	USB_CDCREQ_SEND_BREAK					= 0x23,
};

extern const struct vsfusbd_class_protocol_t vsfusbd_CDCMaster_class;
extern const struct vsfusbd_class_protocol_t vsfusbd_CDCData_class;

struct vsfusbd_CDC_param_t
{
	struct usart_stream_info_t *usart_stream;
	bool gpio_rts_enable;
	uint8_t gpio_rts_port;
	uint32_t gpio_rts_pin;
	bool gpio_dtr_enable;
	uint8_t gpio_dtr_port;
	uint32_t gpio_dtr_pin;
	
	uint8_t master_iface;
	uint8_t slave_iface;
	uint8_t ep_out;
	uint8_t ep_in;
	
	struct vsfusbd_CDC_line_coding_t line_coding;
	
	// no need to initialize below by user
	uint8_t line_coding_buffer[7];
	volatile bool cdc_out_enable;
};

#endif	// __VSFUSBD_CDC_H_INCLUDED__

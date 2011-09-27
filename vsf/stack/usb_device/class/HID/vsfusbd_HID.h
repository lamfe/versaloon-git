#ifndef __VSFUSBD_HID_H_INCLUDED__
#define __VSFUSBD_HID_H_INCLUDED__

#include "tool/list/list.h"

enum usb_HID_description_type_t
{
	USB_HIDDESC_TYPE_HID					= 0x21,
	USB_HIDDESC_TYPE_REPORT					= 0x22,
};

enum usb_HID_req_t
{
	USB_HIDREQ_GET_REPORT					= 0x01,
	USB_HIDREQ_GET_IDLE						= 0x02,
	USB_HIDREQ_GET_PROTOCOL					= 0x03,
	USB_HIDREQ_SET_REPORT					= 0x09,
	USB_HIDREQ_SET_IDLE						= 0x0A,
	USB_HIDREQ_SET_PROTOCOL					= 0x0B,
};

#define USB_HID_REPORT_TYPE_INPUT			1
#define USB_HID_REPORT_TYPE_OUTPUT			2
#define USB_HID_REPORT_TYPE_FEATURE			3

#define USB_HID_PROTOCOL_BOOT				0
#define USB_HID_PROTOCOL_REPORT				1

extern const struct vsfusbd_class_protocol_t vsfusbd_HID_class;

enum usb_HID_report_type_t
{
	USB_HID_REPORT_OUTPUT,
	USB_HID_REPORT_INPUT,
};

struct vsfusbd_HID_report_t
{
	uint8_t type;
	uint8_t id;
	uint8_t idle;
	struct vsf_buffer_t buffer;
	struct vsf_buffer_t stable_buffer;
	vsf_err_t (*on_set_get_report)(uint8_t id, struct vsf_buffer_t *buffer);
	// no need to initialize below by user
	bool lock;
};

#define VSFUSBD_DESC_HID_REPORT(ptr, size, func)			\
	{USB_HIDDESC_TYPE_REPORT, 0, 0, {(uint8_t*)(ptr), (size)}, (func)}

struct vsfusbd_HID_param_t
{
	uint8_t iface;
	uint8_t ep_out;
	uint8_t ep_in;
	
	struct vsfusbd_desc_filter_t *desc;
	
	uint8_t num_of_report;
	struct vsfusbd_HID_report_t *reports;
	
	// no need to initialize below by user
	uint8_t protocol;
	
	struct sllist list;
};
vsf_err_t vsfusbd_HID_set_param(struct vsfusbd_HID_param_t *param);

#endif	// __VSFUSBD_HID_H_INCLUDED__

#include "tool/list/list.h"

enum usb_HID_req_t
{
	USB_HIDREQ_GET_REPORT					= 0x01,
	USB_HIDREQ_GET_IDLE						= 0x02,
	USB_HIDREQ_GET_PROTOCOL					= 0x03,
	USB_HIDREQ_SET_REPORT					= 0x09,
	USB_HIDREQ_SET_IDLE						= 0x0A,
	USB_HIDREQ_SET_PROTOCOL					= 0x0B,
};

enum usb_HID_protocol_t
{
	USB_HID_PROTOCOL_BOOT					= 0,
	USB_HID_PROTOCOL_REPORT					= 1,
};

extern const struct vsfusbd_class_protocol_t vsfusbd_HID_class;

struct vsfusbd_HID_report_t
{
	uint8_t report_id;
	uint8_t idle;
};

struct vsfusbd_HID_param_t
{
	uint8_t iface;
	uint8_t ep_out;
	uint8_t ep_in;
	
	struct vsfusbd_desc_filter_t *desc;
	
	uint8_t num_of_report;
	struct vsfusbd_HID_report_t *reports;
	RESULT (*get_report)(uint8_t id, struct vsf_buffer_t *buffer);
	
	// no need to initialize below by user
	enum usb_HID_protocol_t protocol;
	
	struct sllist list;
};
RESULT vsfusbd_HID_set_param(struct vsfusbd_HID_param_t *param);

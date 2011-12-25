#ifndef __USB_PROTOCOL_H_INCLUDED__
#define __USB_PROTOCOL_H_INCLUDED__

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "stack/usb_device/class/CDC/vsfusbd_CDC.h"
#include "stack/usb_device/class/HID/vsfusbd_HID.h"
#include "stack/usb_device/class/MSC/vsfusbd_MSC_BOT.h"

extern struct vsfusbd_device_t usb_device;
extern volatile uint32_t rep_len;

vsf_err_t usb_protocol_init(void);
vsf_err_t usb_protocol_poll(void);

#endif	// __USB_PROTOCOL_H_INCLUDED__

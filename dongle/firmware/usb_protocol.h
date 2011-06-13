#ifndef __USB_PROTOCOL_H_INCLUDED__
#define __USB_PROTOCOL_H_INCLUDED__

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "stack/usb_device/class/CDC/vsfusbd_CDC.h"
#include "stack/usb_device/class/HID/vsfusbd_HID.h"
#include "stack/usb_device/class/MSC/vsfusbd_MSC_BOT.h"

extern struct vsfusbd_device_t usb_device;
RESULT usb_protocol_init();

#endif	// __USB_PROTOCOL_H_INCLUDED__

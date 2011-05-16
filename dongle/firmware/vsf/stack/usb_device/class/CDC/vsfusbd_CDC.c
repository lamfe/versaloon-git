#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_CDC.h"

struct vsfusbd_CDC_line_coding_t vsfusbd_CDC_LineCoding = 
{
	115200, 0, 0, 8
};
uint8_t vsfusbd_CDC_LineCoding_buffer[7];

static RESULT vsfusbd_CDCData_class_init(struct vsfusbd_device_t *device)
{
	return ERROR_OK;
}

static RESULT vsfusbd_CDCMaster_GetLineCoding_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	SET_LE_U32(&vsfusbd_CDC_LineCoding_buffer[0], 
				vsfusbd_CDC_LineCoding.bitrate);
	vsfusbd_CDC_LineCoding_buffer[4] = vsfusbd_CDC_LineCoding.stopbittype;
	vsfusbd_CDC_LineCoding_buffer[5] = vsfusbd_CDC_LineCoding.paritytype;
	vsfusbd_CDC_LineCoding_buffer[6] = vsfusbd_CDC_LineCoding.datatype;
	buffer->buffer = vsfusbd_CDC_LineCoding_buffer;
	buffer->size = 7;
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_GetLineCoding_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_CDCMaster_SetControlLineState_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}
static RESULT vsfusbd_CDCMaster_SetControlLineState_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static struct vsfusbd_setup_filter_t vsfusbd_CDCDATA_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_GET_LINE_CODING,
		vsfusbd_CDCMaster_GetLineCoding_prepare,
		vsfusbd_CDCMaster_GetLineCoding_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_CDCREQ_SET_CONTROL_LINE_STATE,
		vsfusbd_CDCMaster_SetControlLineState_prepare,
		vsfusbd_CDCMaster_SetControlLineState_process
	}
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCMaster_class = 
{
	NULL,
	vsfusbd_CDCDATA_class_setup,
	
	NULL,
	NULL, NULL
};

const struct vsfusbd_class_protocol_t vsfusbd_CDCData_class = 
{
	NULL,
	NULL,
	
	vsfusbd_CDCData_class_init,
	NULL, NULL
};

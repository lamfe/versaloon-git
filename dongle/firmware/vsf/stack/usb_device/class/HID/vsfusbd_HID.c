#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_HID.h"

struct vsfusbd_HID_param_t *vsfusbd_HID_param_list = NULL;

static struct vsfusbd_HID_param_t* vsfusbd_HID_find_param(uint8_t iface)
{
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_param_list;
	
	while (tmp != NULL)
	{
		if (tmp->iface == iface)
		{
			break;
		}
		tmp = sllist_get_container(tmp->list.next, struct vsfusbd_HID_param_t, 
									list);
	}
	return tmp;
}

static RESULT vsfusbd_HID_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(iface);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

static RESULT vsfusbd_HID_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(iface);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

static RESULT vsfusbd_HID_GetReport_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = NULL;
	buffer->size = 0;
	
	return ERROR_OK;
}
static RESULT vsfusbd_HID_GetReport_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_HID_GetIdle_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = NULL;
	buffer->size = 0;
	
	return ERROR_OK;
}
static RESULT vsfusbd_HID_GetIdle_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_HID_GetProtocol_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = NULL;
	buffer->size = 0;
	
	return ERROR_OK;
}
static RESULT vsfusbd_HID_GetProtocol_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_HID_SetReport_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = NULL;
	buffer->size = 0;
	
	return ERROR_OK;
}
static RESULT vsfusbd_HID_SetReport_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_HID_SetIdle_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = NULL;
	buffer->size = 0;
	
	return ERROR_OK;
}
static RESULT vsfusbd_HID_SetIdle_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_HID_SetProtocol_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = NULL;
	buffer->size = 0;
	
	return ERROR_OK;
}
static RESULT vsfusbd_HID_SetProtocol_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static const struct vsfusbd_setup_filter_t vsfusbd_HID_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_REPORT,
		vsfusbd_HID_GetReport_prepare,
		vsfusbd_HID_GetReport_process
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_IDLE,
		vsfusbd_HID_GetIdle_prepare,
		vsfusbd_HID_GetIdle_process
	},
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_PROTOCOL,
		vsfusbd_HID_GetProtocol_prepare,
		vsfusbd_HID_GetProtocol_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_SET_REPORT,
		vsfusbd_HID_SetReport_prepare,
		vsfusbd_HID_SetReport_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_IDLE,
		vsfusbd_HID_SetIdle_prepare,
		vsfusbd_HID_SetIdle_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_GET_PROTOCOL,
		vsfusbd_HID_SetProtocol_prepare,
		vsfusbd_HID_SetProtocol_process
	},
};

RESULT vsfusbd_HID_get_desc(struct vsfusbd_device_t *device, uint8_t type, 
			uint8_t index, uint16_t lanid, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	
	if ((NULL == tmp) || (NULL == tmp->desc))
	{
		return ERROR_FAIL;
	}
	
	return vsfusbd_device_get_descriptor(device, tmp->desc, type, index, lanid, 
											buffer);
}

const struct vsfusbd_class_protocol_t vsfusbd_HID_class = 
{
	vsfusbd_HID_get_desc, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_HID_class_setup,
	
	vsfusbd_HID_class_init, NULL, vsfusbd_HID_class_poll
};

RESULT vsfusbd_HID_set_param(struct vsfusbd_HID_param_t *param)
{
	if (vsfusbd_HID_find_param(param->iface) != NULL)
	{
		return ERROR_FAIL;
	}
	
	if (NULL == vsfusbd_HID_param_list)
	{
		sllist_init_node(param->list);
		vsfusbd_HID_param_list = param;
	}
	else
	{
		sllint_insert(param->list, vsfusbd_HID_param_list->list);
	}
	
	return ERROR_OK;
}

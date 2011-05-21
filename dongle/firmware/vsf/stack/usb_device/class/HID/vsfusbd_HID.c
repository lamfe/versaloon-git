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

static struct vsfusbd_HID_report_t* vsfusbd_HID_find_report_by_id(
		struct vsfusbd_HID_param_t *param, uint8_t id)
{
	struct vsfusbd_HID_report_t *report = param->reports;
	uint8_t i;
	
	if (NULL == param)
	{
		return NULL;
	}
	
	for(i = 0; i < param->num_of_report; i++)
	{
		if (param->reports[i].id == id)
		{
			return report;
		}
	}
	
	return NULL;
}

static struct vsfusbd_HID_report_t* vsfusbd_HID_find_report_by_type_id(
		struct vsfusbd_HID_param_t *param, uint8_t type, uint8_t id)
{
	struct vsfusbd_HID_report_t *report = param->reports;
	uint8_t i;
	
	if (NULL == param)
	{
		return NULL;
	}
	
	for(i = 0; i < param->num_of_report; i++)
	{
		if ((param->reports[i].type == type) && (param->reports[i].id == id))
		{
			return report;
		}
	}
	
	return NULL;
}

static RESULT vsfusbd_HID_class_update_report(
		struct vsfusbd_HID_report_t *report)
{
	if (report->type == USB_HID_REPORT_TYPE_OUTPUT)
	{
		uint32_t size = report->buffer.size;
		
		if (NULL == report->on_set_get_report)
		{
			return ERROR_FAIL;
		}
		
		report->lock = true;
		if (ERROR_OK != 
			report->on_set_get_report(report->type, &report->buffer))
		{
			report->lock = false;
			return ERROR_FAIL;
		}
		report->lock = false;
		memcpy(report->stable_buffer.buffer, report->buffer.buffer, size);
	}
	return ERROR_OK;
}

static RESULT vsfusbd_HID_class_poll(uint8_t iface, 
										struct vsfusbd_device_t *device)
{
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(iface);
	uint8_t i;
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < tmp->num_of_report; i++)
	{
		struct vsfusbd_HID_report_t *report = &tmp->reports[i];
		uint8_t ep = tmp->ep_in;
		struct vsf_buffer_t *buffer = &report->buffer;
		bool r;
		
		if ((tmp->reports[i].type == USB_HID_REPORT_TYPE_OUTPUT) && 
			(	((NULL == report->on_set_get_report) || 
				(report->buffer.size != report->stable_buffer.size) || 
				(ERROR_OK != vsfusbd_HID_class_update_report(report)) || 
				(ERROR_OK != vsfusbd_ep_out_nb_isready(device, ep, &r)) || 
				(r && (ERROR_OK != vsfusbd_ep_out_nb(device, ep, buffer))))
			))
		{
			return ERROR_FAIL;
		}
	}
	
	return ERROR_OK;
}

static RESULT vsfusbd_HID_class_init(uint8_t iface, 
										struct vsfusbd_device_t *device)
{
	return vsfusbd_HID_class_poll(iface, device);
}

static RESULT vsfusbd_HID_GetReport_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	uint8_t type = request->value >> 8, id = request->value;
	struct vsfusbd_HID_report_t *report = 
							vsfusbd_HID_find_report_by_type_id(tmp, type, id);
	
	if ((NULL == tmp) || (NULL == report) || (type != report->type))
	{
		return ERROR_FAIL;
	}
	
	if (report->lock)
	{
		buffer->size = report->stable_buffer.size;
		buffer->buffer = report->stable_buffer.buffer;
	}
	else
	{
		buffer->size = report->buffer.size;
		buffer->buffer = report->buffer.buffer;
	}
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
	uint8_t id = request->value;
	struct vsfusbd_HID_report_t *report = 
										vsfusbd_HID_find_report_by_id(tmp, id);
	
	if ((NULL == tmp) || (NULL == report) || (request->length != 1))
	{
		return ERROR_FAIL;
	}
	
	buffer->size = 1;
	buffer->buffer = &report->idle;
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
	
	if ((NULL == tmp) || (request->value != 0) || (request->length != 1))
	{
		return ERROR_FAIL;
	}
	
	buffer->size = 1;
	buffer->buffer = &tmp->protocol;
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
	uint8_t type = request->value >> 8, id = request->value;
	struct vsfusbd_HID_report_t *report = 
							vsfusbd_HID_find_report_by_type_id(tmp, type, id);
	
	if ((NULL == tmp) || (NULL == report) || (type != report->type))
	{
		return ERROR_FAIL;
	}
	
	buffer->size = report->buffer.size;
	buffer->buffer = report->buffer.buffer;
	return ERROR_OK;
}
static RESULT vsfusbd_HID_SetReport_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	uint8_t type = request->value >> 8, id = request->value;
	struct vsfusbd_HID_report_t *report = 
							vsfusbd_HID_find_report_by_type_id(tmp, type, id);
	
	if ((NULL == tmp) || (NULL == report) || (type != report->type))
	{
		return ERROR_FAIL;
	}
	
	if (report->on_set_get_report != NULL)
	{
		return report->on_set_get_report(id, buffer);
	}
	return ERROR_OK;
}

static RESULT vsfusbd_HID_SetIdle_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_HID_param_t *tmp = vsfusbd_HID_find_param(request->index);
	uint8_t id = request->value;
	struct vsfusbd_HID_report_t *report = 
										vsfusbd_HID_find_report_by_id(tmp, id);
	
	if ((NULL == tmp) || (NULL == report) || (request->length != 1))
	{
		return ERROR_FAIL;
	}
	
	buffer->size = 1;
	buffer->buffer = &report->idle;
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
	
	if ((NULL == tmp) || (request->length != 1) || 
		((request->value != USB_HID_PROTOCOL_BOOT) && 
		 	(request->value != USB_HID_PROTOCOL_REPORT)))
	{
		return ERROR_FAIL;
	}
	
	tmp->protocol = request->value;
	return vsfusbd_request_prepare_0(device, buffer);
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
		USB_HIDREQ_SET_IDLE,
		vsfusbd_HID_SetIdle_prepare,
		vsfusbd_HID_SetIdle_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_HIDREQ_SET_PROTOCOL,
		vsfusbd_HID_SetProtocol_prepare,
		vsfusbd_HID_SetProtocol_process
	},
	{0, 0, NULL, NULL}
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
	}
	else
	{
		sllint_insert(param->list, vsfusbd_HID_param_list->list);
	}
	vsfusbd_HID_param_list = param;
	
	return ERROR_OK;
}

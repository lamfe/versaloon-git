#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_MSC_BOT.h"

struct vsfusbd_MSCBOT_param_t *vsfusbd_MSCBOT_param_list = NULL;

static struct vsfusbd_MSCBOT_param_t* vsfusbd_MSCBOT_find_param(uint8_t iface)
{
	struct vsfusbd_MSCBOT_param_t *tmp = vsfusbd_MSCBOT_param_list;
	
	while (tmp != NULL)
	{
		if (tmp->iface == iface)
		{
			break;
		}
		tmp = sllist_get_container(tmp->list.next, struct vsfusbd_MSCBOT_param_t, 
									list);
	}
	return tmp;
}

static RESULT vsfusbd_MSCBOT_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_MSCBOT_param_t *tmp = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64];
	
	if (iface < 0)
	{
		return ERROR_FAIL;
	}
	tmp = vsfusbd_MSCBOT_find_param(iface);
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	pkg_size = device->drv->ep.get_OUT_count(ep);
	if (pkg_size > 64)
	{
		return ERROR_FAIL;
	}
	device->drv->ep.read_OUT_buffer(ep, buffer, pkg_size);
	
	switch (tmp->status)
	{
	case VSFUSBD_MSCBOT_STATUS_IDLE:
		if (pkg_size != 31)
		{
			return ERROR_FAIL;
		}
		
		tmp->CBW.dCBWSignature = GET_LE_U32(&buffer[0]);
		tmp->CBW.dCBWTag = GET_LE_U32(&buffer[4]);
		tmp->CBW.dCBWDataTransferLength = GET_LE_U32(&buffer[8]);
		tmp->CBW.bmCBWFlags = buffer[12];
		tmp->CBW.bCBWLUN = buffer[13] & 0x0F;
		tmp->CBW.bCBWCBLength = buffer[14] & 0x1F;
		memcpy(tmp->CBW.CBWCB, &buffer[15], 16);
		
		return ERROR_OK;
	case VSFUSBD_MSCBOT_STATUS_OUT:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

static RESULT vsfusbd_MSCBOT_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_MSCBOT_param_t *tmp = NULL;
	
	if (iface < 0)
	{
		return ERROR_FAIL;
	}
	tmp = vsfusbd_MSCBOT_find_param(iface);
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	switch (tmp->status)
	{
	default:
		return ERROR_FAIL;
	}
}

static RESULT vsfusbd_MSCBOT_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_MSCBOT_param_t *tmp = vsfusbd_MSCBOT_find_param(iface);
	
	if ((NULL == tmp) || 
		// minium ep size of MSCBOT is 32 bytes, 
		// so that CBW and CSW can be transfered in one package
		(device->drv->ep.get_IN_epsize(tmp->ep_in) < 32) || 
		(device->drv->ep.get_OUT_epsize(tmp->ep_in) < 32) || 
		(ERROR_OK != device->drv->ep.set_IN_handler(tmp->ep_in, 
												vsfusbd_MSCBOT_IN_hanlder)) || 
		(ERROR_OK != device->drv->ep.set_IN_count(tmp->ep_in, 0)) || 
		(ERROR_OK != device->drv->ep.set_IN_state(tmp->ep_in, 
														USB_EP_STAT_NACK)) || 
		(ERROR_OK != device->drv->ep.set_OUT_handler(tmp->ep_out, 
												vsfusbd_MSCBOT_OUT_hanlder)))
	{
		return ERROR_FAIL;
	}
	tmp->status = VSFUSBD_MSCBOT_STATUS_IDLE;
	
	return ERROR_OK;
}

static RESULT vsfusbd_MSCBOT_class_poll(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct vsfusbd_MSCBOT_param_t *tmp = vsfusbd_MSCBOT_find_param(iface);
	
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

static RESULT vsfusbd_MSCBOT_GetMaxLun_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_MSCBOT_param_t *tmp = vsfusbd_MSCBOT_find_param(request->index);
	
	if ((NULL == tmp) || (request->length != 1) || (request->value != 0))
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = &tmp->max_lun;
	buffer->size = 1;
	
	return ERROR_OK;
}
static RESULT vsfusbd_MSCBOT_GetMaxLun_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static RESULT vsfusbd_MSCBOT_Reset_prepare(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_MSCBOT_param_t *tmp = vsfusbd_MSCBOT_find_param(request->index);
	
	if ((NULL == tmp) || (request->length != 0) || (request->value != 0) || 
		(ERROR_OK != device->drv->ep.set_type(tmp->ep_in, USB_EP_TYPE_BULK)) || 
		(ERROR_OK != device->drv->ep.set_type(tmp->ep_out, USB_EP_TYPE_BULK)) || 
		(ERROR_OK != vsfusbd_MSCBOT_class_init(tmp->iface, device)))
	{
		return ERROR_FAIL;
	}
	
	return vsfusbd_request_prepare_0(device, buffer);
}
static RESULT vsfusbd_MSCBOT_Reset_process(
	struct vsfusbd_device_t *device, struct vsf_buffer_t *buffer)
{
	return ERROR_OK;
}

static const struct vsfusbd_setup_filter_t vsfusbd_MSCBOT_class_setup[] = 
{
	{
		USB_REQ_DIR_DTOH | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_MSCBOTREQ_GET_MAX_LUN,
		vsfusbd_MSCBOT_GetMaxLun_prepare,
		vsfusbd_MSCBOT_GetMaxLun_process
	},
	{
		USB_REQ_DIR_HTOD | USB_REQ_TYPE_CLASS | USB_REQ_RECP_INTERFACE,
		USB_MSCBOTREQ_RESET,
		vsfusbd_MSCBOT_Reset_prepare,
		vsfusbd_MSCBOT_Reset_process
	}
};

const struct vsfusbd_class_protocol_t vsfusbd_MSCBOT_class = 
{
	NULL, NULL,
	(struct vsfusbd_setup_filter_t *)vsfusbd_MSCBOT_class_setup,
	
	vsfusbd_MSCBOT_class_init, NULL, vsfusbd_MSCBOT_class_poll
};

RESULT vsfusbd_MSCBOT_set_param(struct vsfusbd_MSCBOT_param_t *param)
{
	if (vsfusbd_MSCBOT_find_param(param->iface) != NULL)
	{
		return ERROR_FAIL;
	}
	
	if (NULL == vsfusbd_MSCBOT_param_list)
	{
		sllist_init_node(param->list);
	}
	else
	{
		sllint_insert(param->list, vsfusbd_MSCBOT_param_list->list);
	}
	vsfusbd_MSCBOT_param_list = param;
	
	return ERROR_OK;
}


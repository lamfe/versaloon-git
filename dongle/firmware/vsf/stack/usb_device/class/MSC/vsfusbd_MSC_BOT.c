#include "app_cfg.h"
#include "interfaces.h"

#include "stack/usb_device/vsf_usbd_const.h"
#include "stack/usb_device/vsf_usbd.h"
#include "stack/usb_device/vsf_usbd_drv_callback.h"

#include "vsfusbd_MSC_BOT.h"

#define VSFUSBD_MSCBOT_STALL_IN					(1 << 0)
#define VSFUSBD_MSCBOT_STALL_OUT				(1 << 1)
#define VSFUSBD_MSCBOT_STALL_BOTH				\
							VSFUSBD_MSCBOT_STALL_IN | VSFUSBD_MSCBOT_STALL_OUT

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
		tmp = sllist_get_container(tmp->list.next, 
									struct vsfusbd_MSCBOT_param_t, list);
	}
	return tmp;
}

static RESULT vsfusbd_MSCBOT_SetError(struct vsfusbd_device_t *device, 
		struct vsfusbd_MSCBOT_param_t *param, uint8_t error, uint8_t stall)
{
	if (stall & VSFUSBD_MSCBOT_STALL_IN)
	{
		device->drv->ep.set_IN_count(param->ep_in, 0);
		device->drv->ep.set_IN_state(param->ep_in, USB_EP_STAT_STALL);
	}
	if (stall & VSFUSBD_MSCBOT_STALL_OUT)
	{
		device->drv->ep.set_OUT_state(param->ep_out, USB_EP_STAT_STALL);
	}
	param->dCSWStatus = error;
	return ERROR_OK;
}

static struct vsf_buffer_t *vsfusbd_MSCBOT_GetBuffer(
		struct vsfusbd_MSCBOT_param_t *param)
{
	return &param->page_buffer[++param->tick_tock & 1];
}

static uint16_t vsfusbd_MSCBOT_GetInPkgSize(struct interface_usbd_t *drv, 
											uint8_t ep, uint32_t size)
{
	uint16_t epsize = drv->ep.get_IN_epsize(ep);
	
	if (epsize > size)
	{
		return size;
	}
	else
	{
		return epsize;
	}
}

static RESULT vsfusbd_MSCBOT_IN_hanlder(void *p, uint8_t ep);
static RESULT vsfusbd_MSCBOT_SendCSW(struct vsfusbd_device_t *device, 
							struct vsfusbd_MSCBOT_param_t *param, bool now)
{
	struct interface_usbd_t *drv = device->drv;
	uint8_t CSW_buffer[USBMSC_CSW_SIZE];
	
	SET_LE_U32(&CSW_buffer[0], USBMSC_CSW_SIGNATURE);
	SET_LE_U32(&CSW_buffer[4], param->CBW.dCBWTag);
	SET_LE_U32(&CSW_buffer[8], 
		param->CBW.dCBWDataTransferLength - 
		(param->page_size * param->cur_page + param->tbuffer.position));
	CSW_buffer[12] = param->dCSWStatus;
	param->cur_page = param->tbuffer.position = 0;
	param->page_size = USBMSC_CSW_SIZE;
	param->page_num = 1;
	param->tbuffer.buffer.buffer = CSW_buffer;
	param->bot_status = VSFUSBD_MSCBOT_STATUS_CSW;
	
	drv->ep.write_IN_buffer(param->ep_in, CSW_buffer, USBMSC_CSW_SIZE);
	drv->ep.set_IN_count(param->ep_in, USBMSC_CSW_SIZE);
	if (now)
	{
		drv->ep.set_IN_state(param->ep_in, USB_EP_STAT_ACK);
	}
	return ERROR_OK;
}

static RESULT vsfusbd_MSCBOT_ErrCode(struct vsfusbd_device_t *device, 
			struct vsfusbd_MSCBOT_param_t *param, enum SCSI_errcode_t code)
{
	switch (code)
	{
	case SCSI_ERRCODE_INVALID_PARAM:
		vsfusbd_MSCBOT_SetError(device, param, USBMSC_CSW_FAIL, 
								VSFUSBD_MSCBOT_STALL_BOTH);
		break;
	case SCSI_ERRCODE_FAIL:
		vsfusbd_MSCBOT_SetError(device, param, USBMSC_CSW_FAIL, 
								VSFUSBD_MSCBOT_STALL_IN);
		vsfusbd_MSCBOT_SendCSW(device, param, false);
		break;
	default:
		break;
	}
	return ERROR_OK;
}

static RESULT vsfusbd_MSCBOT_IN_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_MSCBOT_param_t *tmp = NULL;
	struct SCSI_LUN_info_t *lun_info = NULL;
	uint16_t pkg_size;
	uint32_t remain_size;
	uint8_t *pbuffer;
	
	if (iface < 0)
	{
		return ERROR_FAIL;
	}
	tmp = vsfusbd_MSCBOT_find_param(iface);
	if (NULL == tmp)
	{
		return ERROR_FAIL;
	}
	
	switch (tmp->bot_status)
	{
	case VSFUSBD_MSCBOT_STATUS_ERROR:
	case VSFUSBD_MSCBOT_STATUS_CSW:
		tmp->bot_status = VSFUSBD_MSCBOT_STATUS_IDLE;
		device->drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK);
		break;
	case VSFUSBD_MSCBOT_STATUS_IN:
		lun_info = &tmp->lun_info[tmp->CBW.bCBWLUN];
		remain_size = tmp->page_size - tmp->tbuffer.position;
		pkg_size = vsfusbd_MSCBOT_GetInPkgSize(device->drv, ep, remain_size);
		pbuffer = &tmp->tbuffer.buffer.buffer[tmp->tbuffer.position];
		if (remain_size)
		{
			device->drv->ep.write_IN_buffer(tmp->ep_in, pbuffer, pkg_size);
			device->drv->ep.set_IN_count(tmp->ep_in, pkg_size);
			device->drv->ep.set_IN_state(tmp->ep_in, USB_EP_STAT_ACK);
			tmp->tbuffer.position += pkg_size;
			return ERROR_OK;
		}
		
		tmp->tbuffer.position = 0;
		if (++tmp->cur_page >= tmp->page_num)
		{
			return vsfusbd_MSCBOT_SendCSW(device, tmp, true);
		}
		
		tmp->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(tmp);
		if (ERROR_OK != SCSI_IO(tmp->cur_handlers, lun_info, tmp->CBW.CBWCB, 
					&tmp->tbuffer.buffer, tmp->cur_page))
		{
			return vsfusbd_MSCBOT_ErrCode(device, tmp, SCSI_GetErrorCode());
		}
		return vsfusbd_MSCBOT_IN_hanlder(p, ep);
	default:
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT vsfusbd_MSCBOT_OUT_hanlder(void *p, uint8_t ep)
{
	struct vsfusbd_device_t *device = p;
	struct vsfusbd_config_t *config = &device->config[device->configuration];
	int8_t iface = config->ep_OUT_iface_map[ep];
	struct vsfusbd_MSCBOT_param_t *tmp = NULL;
	struct SCSI_LUN_info_t *lun_info = NULL;
	uint16_t pkg_size;
	uint8_t buffer[64], *pbuffer;
	
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
	
	switch (tmp->bot_status)
	{
	case VSFUSBD_MSCBOT_STATUS_IDLE:
		tmp->CBW.dCBWSignature = GET_LE_U32(&buffer[0]);
		tmp->CBW.dCBWTag = GET_LE_U32(&buffer[4]);
		tmp->CBW.dCBWDataTransferLength = GET_LE_U32(&buffer[8]);
		tmp->CBW.bmCBWFlags = buffer[12];
		tmp->CBW.bCBWLUN = buffer[13] & 0x0F;
		tmp->CBW.bCBWCBLength = buffer[14] & 0x1F;
		memcpy(tmp->CBW.CBWCB, &buffer[15], 16);
		
		if ((tmp->CBW.dCBWSignature != USBMSC_CBW_SIGNATURE) || 
			(tmp->CBW.bCBWLUN > tmp->max_lun) || (pkg_size != 31))
		{
			vsfusbd_MSCBOT_SetError(device, tmp, USBMSC_CSW_FAIL, 
									VSFUSBD_MSCBOT_STALL_BOTH);
			return ERROR_FAIL;
		}
		lun_info = &tmp->lun_info[tmp->CBW.bCBWLUN];
		if ((tmp->CBW.bCBWCBLength < 1) || (tmp->CBW.bCBWCBLength > 16))
		{
			vsfusbd_MSCBOT_SetError(device, tmp, USBMSC_CSW_FAIL, 
									VSFUSBD_MSCBOT_STALL_BOTH);
			lun_info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			lun_info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			return ERROR_FAIL;
		}
		tmp->page_size = tmp->page_num = 0;
		tmp->cur_handlers = NULL;
		tmp->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(tmp);
		tmp->dCSWStatus = USBMSC_CSW_OK;
		if (ERROR_OK != SCSI_Handle(tmp->cur_handlers, lun_info, 
					tmp->CBW.CBWCB, &tmp->tbuffer.buffer, &tmp->page_size, 
					&tmp->page_num))
		{
			enum SCSI_errcode_t errcode = SCSI_GetErrorCode();
			if (SCSI_ERRCODE_INVALID_COMMAND == errcode)
			{
				tmp->cur_handlers = tmp->user_handlers;
				if (NULL == tmp->cur_handlers)
				{
					vsfusbd_MSCBOT_SetError(device, tmp, USBMSC_CSW_FAIL, 
											VSFUSBD_MSCBOT_STALL_BOTH);
					return ERROR_FAIL;
				}
				else if (ERROR_OK != SCSI_Handle(tmp->cur_handlers, lun_info, 
						tmp->CBW.CBWCB, &tmp->tbuffer.buffer, &tmp->page_size, 
						&tmp->page_num))
				{
					errcode = SCSI_GetErrorCode();
					if (SCSI_ERRCODE_INVALID_COMMAND == errcode)
					{
						vsfusbd_MSCBOT_SetError(device, tmp, USBMSC_CSW_FAIL, 
												VSFUSBD_MSCBOT_STALL_BOTH);
						return ERROR_FAIL;
					}
					return vsfusbd_MSCBOT_ErrCode(device, tmp, errcode);
				}
			}
			else
			{
				return vsfusbd_MSCBOT_ErrCode(device, tmp, errcode);
			}
		}
		
		tmp->tbuffer.position = tmp->cur_page = 0;
		if (tmp->CBW.bCBWCBLength)
		{
			if ((tmp->CBW.bmCBWFlags & USBMSC_CBWFLAGS_DIR_MASK) == 
						USBMSC_CBWFLAGS_DIR_IN)
			{
				tmp->bot_status = VSFUSBD_MSCBOT_STATUS_IN;
				return vsfusbd_MSCBOT_IN_hanlder(p, tmp->ep_in);
			}
			else
			{
				tmp->bot_status = VSFUSBD_MSCBOT_STATUS_OUT;
				device->drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK);
				return ERROR_OK;
			}
		}
		else
		{
			return vsfusbd_MSCBOT_SendCSW(device, tmp, true);
		}
	case VSFUSBD_MSCBOT_STATUS_OUT:
		lun_info = &tmp->lun_info[tmp->CBW.bCBWLUN];
		pbuffer = &tmp->tbuffer.buffer.buffer[tmp->tbuffer.position];
		if ((pkg_size + tmp->tbuffer.position) <= tmp->page_size)
		{
			memcpy(pbuffer, buffer, pkg_size);
			tmp->tbuffer.position += pkg_size;
			if (tmp->tbuffer.position < tmp->page_size)
			{
				device->drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK);
				return ERROR_OK;
			}
			
			tmp->tbuffer.position = 0;
			if (ERROR_OK != SCSI_IO(tmp->cur_handlers, lun_info, 
						tmp->CBW.CBWCB, &tmp->tbuffer.buffer, tmp->cur_page))
			{
				return vsfusbd_MSCBOT_ErrCode(device, tmp, SCSI_GetErrorCode());
			}
			tmp->tbuffer.buffer = *vsfusbd_MSCBOT_GetBuffer(tmp);
			if (++tmp->cur_page >= tmp->page_num)
			{
				return vsfusbd_MSCBOT_SendCSW(device, tmp, true);
			}
			return device->drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK);
		}
		else
		{
			vsfusbd_MSCBOT_SetError(device, tmp, USBMSC_CSW_PHASE_ERROR, 
									VSFUSBD_MSCBOT_STALL_BOTH);
			lun_info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			lun_info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			return ERROR_FAIL;
		}
	default:
		vsfusbd_MSCBOT_SetError(device, tmp, USBMSC_CSW_PHASE_ERROR, 
								VSFUSBD_MSCBOT_STALL_BOTH);
		lun_info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
		lun_info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
		return ERROR_FAIL;
	}
}

static RESULT vsfusbd_MSCBOT_class_init(uint8_t iface, 
											struct vsfusbd_device_t *device)
{
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_MSCBOT_param_t *tmp = vsfusbd_MSCBOT_find_param(iface);
	
	if ((NULL == tmp) || 
		// minium ep size of MSCBOT is 32 bytes, 
		// so that CBW and CSW can be transfered in one package
		(drv->ep.get_IN_epsize(tmp->ep_in) < 32) || 
		(drv->ep.get_OUT_epsize(tmp->ep_in) < 32) || 
		(ERROR_OK != drv->ep.set_IN_handler(tmp->ep_in, 
												vsfusbd_MSCBOT_IN_hanlder)) || 
		(ERROR_OK != drv->ep.set_IN_count(tmp->ep_in, 0)) || 
		(ERROR_OK != drv->ep.set_IN_state(tmp->ep_in, USB_EP_STAT_ACK)) || 
		(ERROR_OK != drv->ep.set_OUT_handler(tmp->ep_out, 
												vsfusbd_MSCBOT_OUT_hanlder)) || 
		(ERROR_OK != drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK)))
	{
		return ERROR_FAIL;
	}
	
	tmp->bot_status = VSFUSBD_MSCBOT_STATUS_IDLE;
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
	struct vsfusbd_MSCBOT_param_t *tmp = 
									vsfusbd_MSCBOT_find_param(request->index);
	
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
	struct interface_usbd_t *drv = device->drv;
	struct vsfusbd_ctrl_request_t *request = &device->ctrl_handler.request;
	struct vsfusbd_MSCBOT_param_t *tmp = 
									vsfusbd_MSCBOT_find_param(request->index);
	
	if ((NULL == tmp) || (request->length != 0) || (request->value != 0) || 
		(ERROR_OK != drv->ep.reset_IN_toggle(tmp->ep_in)) || 
		(ERROR_OK != drv->ep.set_IN_state(tmp->ep_in, USB_EP_STAT_ACK)) || 
		(ERROR_OK != drv->ep.reset_OUT_toggle(tmp->ep_out)) || 
		(ERROR_OK != drv->ep.set_OUT_state(tmp->ep_out, USB_EP_STAT_ACK)))
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


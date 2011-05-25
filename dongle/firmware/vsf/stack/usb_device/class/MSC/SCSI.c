#include "app_cfg.h"

#include "SCSI.h"

static enum SCSI_errcode_t SCSI_errcode = SCSI_ERRCODE_OK;

static RESULT SCSI_handler_TEST_UNIT_READY(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_hadler_REQUEST_SENSE(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	uint8_t *pbuffer = buffer->buffer;
	uint32_t size;
	
	memset(pbuffer, 0, 18);
	pbuffer[0] = 0x70;
	pbuffer[2] = info->status.sense_key;
	pbuffer[7] = 0x0A;
	pbuffer[12] = info->status.asc;
	size = CB[4] <= 18 ? CB[4] : 18;
	buffer->size = size;
	*page_size = size;
	*page_num = 1;
	SCSI_errcode = SCSI_ERRCODE_OK;
	
	return ERROR_OK;
}

static RESULT SCSI_handler_FORMAT_UNIT(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_INQUIRY(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	uint8_t *pbuffer = buffer->buffer;
	
	if (CB[1] & 1)
	{
		// When the EVPD bit is set to one, 
		// the PAGE CODE field specifies which page of 
		// vital product data information the device server shall return
		if (0 == CB[2])
		{
			// 0x00: Supported VPD Pages
			memset(pbuffer, 0, 5);
			buffer->size = 5;
			*page_size = 5;
			*page_num = 1;
			SCSI_errcode = SCSI_ERRCODE_OK;
		}
	}
	else
	{
		if (CB[2] != 0)
		{
			// If the PAGE CODE field is not set to zero 
			// when the EVPD bit is set to zero, 
			// the command shall be terminated with CHECK CONDITION status, 
			// with the sense key set to ILLEGAL REQUEST, 
			// and the additional sense code set to INVALID FIELD IN CDB.
			info->status.sense_key = SCSI_SENSEKEY_ILLEGAL_REQUEST;
			info->status.asc = SCSI_ASC_INVALID_FIELED_IN_COMMAND;
			SCSI_errcode = SCSI_ERRCODE_INVALID_PARAM;
			return ERROR_FAIL;
		}
		// If the EVPD bit is set to zero, 
		// the device server shall return the standard INQUIRY data.
		memset(pbuffer, 0, 36);
		pbuffer[0] = info->param.type;
		if (info->param.removable)
		{
			pbuffer[1] = 0x80;
		}
		pbuffer[3] = 2;
		pbuffer[4] = 31;
		pbuffer += 8;
		memcpy(pbuffer, info->param.vendor, sizeof(info->param.vendor));
		pbuffer += sizeof(info->param.vendor);
		memcpy(pbuffer, info->param.product, sizeof(info->param.product));
		pbuffer += sizeof(info->param.product);
		memcpy(pbuffer, info->param.revision, sizeof(info->param.revision));
		buffer->size = 36;
		*page_size = 36;
		*page_num = 1;
		SCSI_errcode = SCSI_ERRCODE_OK;
	}
	
	return ERROR_OK;
}

static RESULT SCSI_handler_MODE_SENSE6(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_START_STOP_UNIT(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_ALLOW_MEDIUM_REMOVAL(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_READ_FORMAT_CAPACITIES(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_READ_CAPACITY10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_WRITE10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_READ10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_VERIFY10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_FAIL;
	return ERROR_FAIL;
}

static RESULT SCSI_handler_READ_TOC(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return ERROR_OK;
}

static RESULT SCSI_handler_GET_EVENT_STATUS_NOTIFICATION(
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return ERROR_OK;
}

static RESULT SCSI_handler_MODE_SENSE10(struct SCSI_LUN_info_t *info, 
		uint8_t CB[16], struct vsf_buffer_t *buffer, uint32_t *page_size, 
		uint32_t *page_num)
{
	info->status.sense_key = SCSI_SENSEKEY_NOT_READY;
	info->status.asc = SCSI_ASC_MEDIUM_NOT_PRESENT;
	SCSI_errcode = SCSI_ERRCODE_OK;
	return ERROR_OK;
}

static struct SCSI_handler_t SCSI_handlers[] = 
{
	{
		SCSI_CMD_TEST_UNIT_READY,
		SCSI_handler_TEST_UNIT_READY,
		NULL
	},
	{
		SCSI_CMD_REQUEST_SENSE,
		SCSI_hadler_REQUEST_SENSE,
		NULL
	},
	{
		SCSI_CMD_FORMAT_UNIT,
		SCSI_handler_FORMAT_UNIT,
		NULL
	},
	{
		SCSI_CMD_INQUIRY,
		SCSI_handler_INQUIRY,
		NULL
	},
	{
		SCSI_CMD_MODE_SENSE6,
		SCSI_handler_MODE_SENSE6,
		NULL
	},
	{
		SCSI_CMD_START_STOP_UNIT,
		SCSI_handler_START_STOP_UNIT,
		NULL
	},
	{
		SCSI_CMD_ALLOW_MEDIUM_REMOVAL,
		SCSI_handler_ALLOW_MEDIUM_REMOVAL,
		NULL
	},
	{
		SCSI_CMD_READ_FORMAT_CAPACITIES,
		SCSI_handler_READ_FORMAT_CAPACITIES,
		NULL
	},
	{
		SCSI_CMD_READ_CAPACITY10,
		SCSI_handler_READ_CAPACITY10,
		NULL
	},
	{
		SCSI_CMD_WRITE10,
		SCSI_handler_WRITE10,
		NULL
	},
	{
		SCSI_CMD_READ10,
		SCSI_handler_READ10,
		NULL
	},
	{
		SCSI_CMD_VERIFY10,
		SCSI_handler_VERIFY10,
		NULL
	},
	{
		SCSI_CMD_READ_TOC,
		SCSI_handler_READ_TOC,
		NULL
	},
	{
		SCSI_CMD_GET_EVENT_STATUS_NOTIFICATION,
		SCSI_handler_GET_EVENT_STATUS_NOTIFICATION,
		NULL
	},
	{
		SCSI_CMD_MODE_SENSE10,
		SCSI_handler_MODE_SENSE10,
		NULL
	},
	SCSI_HANDLER_NULL
};

static struct SCSI_handler_t* SCSI_get_handler(struct SCSI_handler_t *handlers, 
												uint8_t operation_code)
{
	while (handlers->handler != NULL)
	{
		if (handlers->operation_code == operation_code)
		{
			return handlers;
		}
		handlers++;
	}
	return NULL;
}

RESULT SCSI_Handle(struct SCSI_handler_t *handlers, 
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num)
{
	if (NULL == handlers)
	{
		handlers = SCSI_handlers;
	}
	handlers = SCSI_get_handler(handlers, CB[0]);
	if (NULL == handlers)
	{
		SCSI_errcode = SCSI_ERRCODE_INVALID_COMMAND;
		return ERROR_FAIL;
	}
	
	SCSI_errcode = SCSI_ERRCODE_OK;
	return handlers->handler(info, CB, buffer, page_size, page_num);
}

RESULT SCSI_IO(struct SCSI_handler_t *handlers, 
		struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page)
{
	if (NULL == handlers)
	{
		handlers = SCSI_handlers;
	}
	handlers = SCSI_get_handler(handlers, CB[0]);
	if ((NULL == handlers) || (NULL == handlers->io))
	{
		SCSI_errcode = SCSI_ERRCODE_INVALID_COMMAND;
		return ERROR_FAIL;
	}
	
	SCSI_errcode = SCSI_ERRCODE_OK;
	return handlers->io(info, CB, buffer, cur_page);
}

enum SCSI_errcode_t SCSI_GetErrorCode(void)
{
	return SCSI_errcode;
}

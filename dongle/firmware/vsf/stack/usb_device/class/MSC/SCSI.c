#include "app_cfg.h"

#include "SCSI.h"

static enum SCSI_errcode_t SCSI_errcode = SCSI_ERRCODE_OK;

RESULT SCSI_handler_INQUIRY(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num)
{
	return ERROR_OK;
}

static struct SCSI_handler_t SCSI_handlers[] = 
{
	{
		SCSI_CMD_INQUIRY,
		SCSI_handler_INQUIRY,
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
		return ERROR_FAIL;
	}
	
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
	if (NULL == handlers)
	{
		return ERROR_FAIL;
	}
	
	return handlers->io(info, CB, buffer, cur_page);
}

enum SCSI_errcode_t SCSI_GetErrorCode(void)
{
	return SCSI_errcode;
}

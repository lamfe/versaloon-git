#include "app_cfg.h"

#include "SCSI.h"

RESULT SCSI_Process(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t *page_size, uint32_t *page_num)
{
	return ERROR_OK;
}

RESULT SCSI_IO(struct SCSI_LUN_info_t *info, uint8_t CB[16], 
		struct vsf_buffer_t *buffer, uint32_t cur_page)
{
	return ERROR_OK;
}

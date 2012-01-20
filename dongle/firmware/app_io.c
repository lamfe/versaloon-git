/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       main.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    main.c file                                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include <stdlib.h>
#include <stdarg.h>

#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "scripts.h"
#include "interfaces.h"
#include "tool/buffer/buffer.h"
#include "dal/usart_stream/usart_stream.h"

#include "usb_protocol.h"

VSS_HANDLER(appio_set_dummy);

static const struct vss_cmd_t appio_cmd[] =
{
	VSS_CMD(	"dummy",
				"set dummy mode of appio, format: appio.dummy DUMMY",
				appio_set_dummy,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t appio_cmd_list =
							VSS_CMD_LIST("appio", appio_cmd);

static bool appio_dummy = false;
VSS_HANDLER(appio_set_dummy)
{
	VSS_CHECK_ARGC(2);
	
	appio_dummy = (strtoul(argv[1], NULL, 0) != 0);
	return VSFERR_NONE;
}

static uint8_t shell_buff_tx[512], shell_buff_rx[512];
struct usart_stream_info_t shell_stream =
{
	IFS_DUMMY_PORT,								// usart_index
	{{shell_buff_tx, sizeof(shell_buff_tx)}},	// fifo_tx
	{{shell_buff_rx, sizeof(shell_buff_rx)}}	// fifo_rx
};

static char app_io_local_buff[APPIO_BUFFER_SIZE];

#if !APPIO_DUMMY
static void app_io_out_sync(void)
{
	int free_space;
	
	do
	{
		usb_protocol_poll();
		free_space = vsf_fifo_get_data_length(&shell_stream.fifo_rx);
	} while (free_space);
}
#endif

void APP_IO_INIT(void)
{
	
}

FILE *FOPEN(const char *filename, const char *mode)
{
	return NULL;
}

int FCLOSE(FILE *f)
{
	if ((f != stdin) && (f != stdout) && (f != stderr))
	{
	}
	
	return 0;
}

int FEOF(FILE *f)
{
	if ((stdin == f) || (stdout == f) || (stderr == f))
	{
		return 0;
	}
	else
	{
	}
	
	return 0;
}

void REWIND(FILE *f)
{
	if ((f != stdin) && (f != stdout) && (f != stderr))
	{
	}
}

int FFLUSH(FILE *f)
{
	if ((stdout == f) || (stderr == f))
	{
#if !APPIO_DUMMY
		if (!appio_dummy)
		{
			app_io_out_sync();
		}
#endif
		return 0;
	}
	else if (stdin == f)
	{
#if !APPIO_DUMMY
		if (!appio_dummy)
		{
			uint32_t i, size = vsf_fifo_get_data_length(&shell_stream.fifo_tx);
			for (i = 0; i < size; i++)
			{
				vsf_fifo_pop8(&shell_stream.fifo_tx);
			}
		}
#endif
		return 0;
	}
	else
	{
	}
	
	return 0;
}

int FGETC(FILE *f)
{
	if ((stdout == f) || (stderr == f))
	{
		return 0;
	}
	else if (stdin == f)
	{
#if !APPIO_DUMMY
		if (!appio_dummy)
		{
			uint32_t size;
			do
			{
				usb_protocol_poll();
				size = vsf_fifo_get_data_length(&shell_stream.fifo_tx);
			} while (!size);
			return vsf_fifo_pop8(&shell_stream.fifo_tx);
		}
#endif
	}
	else
	{
	}
	
	return 0;
}

int GETCHAR(void)
{
	return FGETC(stdin);
}

char* FGETS(char *buf, int count, FILE *f)
{
	char cur_char, *result = buf;
	int size = 0, cur_size, pos;
	
	if ((NULL == buf) || (NULL == f) || (stdout == f) || (stderr == f))
	{
		return NULL;
	}
	
	if (stdin == f)
	{
#if !APPIO_DUMMY
		if (!appio_dummy)
		{
			pos = 0;
			cur_char = '\0';
			while ((size < count) && (cur_char != '\r'))
			{
				usb_protocol_poll();
				cur_size = vsf_fifo_get_data_length(&shell_stream.fifo_tx);
				
				while (cur_size && (size < count) && (cur_char != '\r'))
				{
					cur_char = (char)vsf_fifo_pop8(&shell_stream.fifo_tx);
					if ('\r' == cur_char)
					{
						vsf_fifo_push8(&shell_stream.fifo_rx, '\n');
					}
					else if ('\b' == cur_char)
					{
						if (pos)
						{
							vsf_fifo_push8(&shell_stream.fifo_rx, '\b');
							vsf_fifo_push8(&shell_stream.fifo_rx, ' ');
							vsf_fifo_push8(&shell_stream.fifo_rx, '\b');
							pos--;
						}
						cur_size--;
						continue;
					}
					vsf_fifo_push8(&shell_stream.fifo_rx, (uint8_t)cur_char);
					
					buf[pos++] = cur_char;
					size++;
					cur_size--;
				}
			}
			buf[pos] = '\0';
			app_io_out_sync();
		}
#else
		return NULL;
#endif
	}
	else
	{
	}
	return result;
}

static void APPIO_OUTBUFF(uint8_t *buff, uint32_t size)
{
	uint32_t free_space, cur_size;
	
	while (size > 0)
	{
		do
		{
			usb_protocol_poll();
			free_space = vsf_fifo_get_avail_length(&shell_stream.fifo_rx);
		} while (!free_space);
		
		if (free_space > size)
		{
			cur_size = size;
		}
		else
		{
			cur_size = free_space;
		}
		
		vsf_fifo_push(&shell_stream.fifo_rx, cur_size, buff);
		
		size -= cur_size;
		buff += cur_size;
	}
	
	app_io_out_sync();
}

int FPRINTF(FILE *f, const char *format, ...)
{
	int number = 0;
	char *pbuff = app_io_local_buff;
	va_list ap;
	
	if ((NULL == f) || (stdin == f))
	{
		return 0;
	}
	
	va_start(ap, format);
	number = vsprintf(app_io_local_buff, format, ap);
	va_end(ap);
	
	if ((stdout == f) || (stderr == f))
	{
#if !APPIO_DUMMY
		if (!appio_dummy)
		{
			APPIO_OUTBUFF((uint8_t *)pbuff, (uint32_t)number);
		}
#endif
	}
	else
	{
	}
	return number;
}

int PRINTF(const char *format, ...)
{
	int number = 0;
	char *pbuff = app_io_local_buff;
	va_list ap;
	
#if !APPIO_DUMMY
	if (!appio_dummy)
	{
		va_start(ap, format);
		number = vsprintf(app_io_local_buff, format, ap);
		va_end(ap);
	
		APPIO_OUTBUFF((uint8_t *)pbuff, (uint32_t)number);
	}
#endif
	return number;
}

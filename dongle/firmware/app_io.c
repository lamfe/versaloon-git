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

#include <stdarg.h>
#include "app_io.h"

#include "interfaces.h"
#include "tool/buffer/buffer.h"
#include "dal/usart_stream/usart_stream.h"

#include "usb_protocol.h"

extern struct usart_stream_info_t usart_stream_p0;

static char app_io_local_buff[256];

static void app_io_out_sync(void)
{
	int free_space;
	
	do
	{
		usb_protocol_poll();
		free_space = vsf_fifo_get_avail_length(&usart_stream_p0.fifo_rx);
	} while (free_space < usart_stream_p0.fifo_rx.buffer.size);
}

void APP_IO_INIT(void)
{
	
}

FILE *FOPEN(const char *filename, const char *mode)
{
}

int FCLOSE(FILE *f)
{
	if ((f != stdin) && (f != stdout) && (f != stderr))
	{
	}
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
		app_io_out_sync();
		return 0;
	}
	else if (stdin == f)
	{
		uint32_t i, size = vsf_fifo_get_data_length(&usart_stream_p0.fifo_tx);
		for (i = 0; i < size; i++)
		{
			vsf_fifo_pop8(&usart_stream_p0.fifo_tx);
		}
		return 0;
	}
	else
	{
	}
}

int FGETC(FILE *f)
{
	if ((stdout == f) || (stderr == f))
	{
		return 0;
	}
	else if (stdin == f)
	{
		uint32_t size;
		do
		{
			usb_protocol_poll();
			size = vsf_fifo_get_data_length(&usart_stream_p0.fifo_tx);
		} while (!size);
		return vsf_fifo_pop8(&usart_stream_p0.fifo_tx);
	}
	else
	{
	}
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
		pos = 0;
		cur_char = '\0';
		while ((size < count) && (cur_char != '\r'))
		{
			usb_protocol_poll();
			cur_size = vsf_fifo_get_data_length(&usart_stream_p0.fifo_tx);
			
			while (cur_size && (size < count) && (cur_char != '\r'))
			{
				cur_char = (char)vsf_fifo_pop8(&usart_stream_p0.fifo_tx);
				if ('\r' == cur_char)
				{
					vsf_fifo_push8(&usart_stream_p0.fifo_rx, '\n');
				}
				else if ('\b' == cur_char)
				{
					if (pos)
					{
						vsf_fifo_push8(&usart_stream_p0.fifo_rx, '\b');
						vsf_fifo_push8(&usart_stream_p0.fifo_rx, ' ');
						vsf_fifo_push8(&usart_stream_p0.fifo_rx, '\b');
						pos--;
					}
					cur_size--;
					continue;
				}
				vsf_fifo_push8(&usart_stream_p0.fifo_rx, (uint8_t)cur_char);
				
				buf[pos++] = cur_char;
				size++;
				cur_size--;
			}
		}
		buf[pos] = '\0';
		app_io_out_sync();
	}
	else
	{
	}
	return result;
}

int FPRINTF(FILE *f, const char *format, ...)
{
	int number, i;
	int free_space, cur_size;
	char *pbuff = app_io_local_buff;
	va_list ap;
	
	if ((NULL == f) || (stdout == f) || (stderr == f))
	{
		return 0;
	}
	
	va_start(ap, format);
	number = vsprintf(app_io_local_buff, format, ap);
	va_end(ap);
	
	if (stdin == f)
	{
		i = number;
		while (i > 0)
		{
			do
			{
				usb_protocol_poll();
				free_space = vsf_fifo_get_avail_length(&usart_stream_p0.fifo_rx);
			} while (!free_space);
			
			if (free_space > i)
			{
				cur_size = i;
			}
			else
			{
				cur_size = free_space;
			}
			
			vsf_fifo_push(&usart_stream_p0.fifo_rx, cur_size, (uint8_t *)pbuff);
			
			i -= cur_size;
			pbuff += cur_size;
		}
		
		app_io_out_sync();
	}
	else
	{
	}
	return number;
}

int PRINTF(const char *format, ...)
{
	int number, i;
	int free_space, cur_size;
	char *pbuff = app_io_local_buff;
	va_list ap;
	
	va_start(ap, format);
	number = vsprintf(app_io_local_buff, format, ap);
	va_end(ap);
	
	i = number;
	while (i > 0)
	{
		do
		{
			usb_protocol_poll();
			free_space = vsf_fifo_get_avail_length(&usart_stream_p0.fifo_rx);
		} while (!free_space);
		
		if (free_space > i)
		{
			cur_size = i;
		}
		else
		{
			cur_size = free_space;
		}
		
		vsf_fifo_push(&usart_stream_p0.fifo_rx, cur_size, (uint8_t *)pbuff);
		
		i -= cur_size;
		pbuff += cur_size;
	}
	
	app_io_out_sync();
	return number;
}

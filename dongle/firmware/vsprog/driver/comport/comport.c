/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <time.h>

#include "compiler.h"

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"

#include "port.h"
#include "comport.h"

void comm_close_usbtocomm(void);
vsf_err_t comm_open_usbtocomm(char *comport, uint32_t baudrate,
			uint8_t datalength, char paritybit, char stopbit, char handshake);
int32_t comm_read_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_write_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_ctrl_usbtocomm(uint8_t dtr, uint8_t rts);
int32_t comm_flush_usbtocomm(void);

struct comm_func_t comm_func[] =
{
	{
		comm_open_usbtocomm,
		comm_close_usbtocomm,
		comm_read_usbtocomm,
		comm_write_usbtocomm,
		comm_ctrl_usbtocomm,
		comm_flush_usbtocomm
	}
};

uint32_t comm_idx = 0;

static uint8_t usbtocomm_open = 0;
static struct INTERFACES_INFO_T *prog = NULL;
void comm_close_usbtocomm(void)
{
	if (!usbtocomm_open)
	{
		return;
	}
	
	if (prog != NULL)
	{
		prog->usart.fini(0);
		prog->peripheral_commit();
	}
	usbtocomm_open = 0;
}

vsf_err_t comm_open_usbtocomm(char *comport, uint32_t baudrate,
			uint8_t datalength, char paritybit, char stopbit, char handshake)
{
	struct INTERFACES_INFO_T *prog_temp = NULL;
	uint8_t mode;
	
	REFERENCE_PARAMETER(comport);
	REFERENCE_PARAMETER(handshake);
	
	if (interface_assert(&prog_temp) || (NULL == prog_temp))
	{
		return VSFERR_FAIL;
	}
	prog = prog_temp;
	
	mode = 0;
	// paritybit
	switch (paritybit)
	{
	case COMM_PARITYBIT_NONE:
		mode |= USART_PARITY_NONE;
		break;
	case COMM_PARITYBIT_ODD:
		mode |= USART_PARITY_ODD;
		break;
	case COMM_PARITYBIT_EVEN:
		mode |= USART_PARITY_EVEN;
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID_VALUE, paritybit, "comm parity");
		return ERRCODE_INVALID;
		break;
	}
	
	// stopbit
	switch (stopbit)
	{
	case COMM_STOPBIT_1:
		mode |= USART_STOPBITS_1;
		break;
	case COMM_STOPBIT_1P5:
		mode |= USART_STOPBITS_1P5;
		break;
	case COMM_STOPBIT_2:
		mode |= USART_STOPBITS_2;
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID_VALUE, stopbit, "comm stopbit");
		return ERRCODE_INVALID;
		
		break;
	}
	
	// initialize usbtocomm
	if (prog->usart.init(0)
		|| prog->usart.config(0, baudrate, datalength, mode)
		|| prog->peripheral_commit())
	{
		return VSFERR_FAIL;
	}
	
	usbtocomm_open = 1;
	return VSFERR_NONE;
}

int32_t comm_read_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes)
{
	uint32_t start, end;
	struct usart_status_t status;
	
	if (!usbtocomm_open)
	{
		return -1;
	}
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	do
	{
		LOG_PUSH();
		LOG_MUTE();
		if (prog->usart.receive(0, buffer, (uint16_t)num_of_bytes)
			|| prog->peripheral_commit())
		{
			LOG_POP();
		}
		else
		{
			LOG_POP();
			return (int32_t)num_of_bytes;
		}
		end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	} while ((end - start) < (100 + 5 * num_of_bytes));
	
	// fail to receive data
	// if usart_status is available
	//   try send data according to buffer_size available
	// else
	//    return 0
	if (NULL == prog->usart.status)
	{
		return 0;
	}
	
	LOG_PUSH();
	LOG_MUTE();
	if (prog->usart.status(0, &status)
		|| prog->peripheral_commit())
	{
		// error
		LOG_POP();
		return -1;
	}
	else if ((status.rx_buff_size != 0)
		&& (!prog->usart.receive(0, buffer, (uint16_t)status.rx_buff_size)
			&& !prog->peripheral_commit()))
	{
		LOG_POP();
		return (int32_t)status.rx_buff_size;
	}
	else
	{
		LOG_POP();
		return 0;
	}
}

int32_t comm_write_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes)
{
	int32_t data_sent;
	
	if (!usbtocomm_open)
	{
		return -1;
	}
	
	data_sent = 0;
	if (!prog->usart.send(0, buffer, (uint16_t)num_of_bytes)
		&& !prog->peripheral_commit())
	{
		data_sent = (int32_t)num_of_bytes;
	}
	
#if 0
	LOG_PUSH();
	LOG_MUTE();
	if (!prog->usart.send(buffer, (uint16_t)num_of_bytes)
		&& !prog->peripheral_commit())
	{
		data_sent = num_of_bytes;
	}
	LOG_POP();
	
	// fail to send data at firt try
	// if usart_status is available
	//   try wait for buffer_size is available and send again
	// else
	//    return 0
	if (NULL == prog->usart.status)
	{
		return (int32_t)data_sent;
	}
	
	if (data_sent != (int32_t)num_of_bytes)
	{
		struct usart_status_t status;
		uint32_t start, end;
		uint16_t size;
		
		// get current time
		start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
		// get buffer_size available for tx
		LOG_PUSH();
		LOG_MUTE();
		if (prog->usart.status(&status)
			|| prog->peripheral_commit())
		{
			// error
			LOG_POP();
			return -1;
		}
		LOG_POP();
		
		do
		{
			LOG_PUSH();
			LOG_MUTE();
			size = (uint16_t)(num_of_bytes - data_sent);
			if (prog->usart.send(buffer, size)
				|| prog->peripheral_commit())
			{
				LOG_POP();
			}
			else
			{
				LOG_POP();
				data_sent += num_of_bytes - data_sent;
				break;
			}
			end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
		} while ((end - start) < 50 + 5 * (num_of_bytes - status.tx_buff_avail));
		
		// time out
		return 0;
	}
	
	// poll ready
	while (1)
	{
		struct usart_status_t status;
		
		// get buffer_size for tx
		LOG_PUSH();
		LOG_MUTE();
		if (prog->usart.status(&status)
			|| prog->peripheral_commit())
		{
			// error
			LOG_POP();
			return -1;
		}
		LOG_POP();
		
		if (!status.tx_buff_size)
		{
			break;
		}
	}
#endif
	
	return data_sent;
}

int32_t comm_flush_usbtocomm(void)
{
	uint8_t buff;
	int32_t result;
	
	if (!usbtocomm_open)
	{
		return -1;
	}
	
	while (1)
	{
		result = comm_read_usbtocomm(&buff, 1);
		if (result < 0)
		{
			return result;
		}
		if (0 == result)
		{
			break;
		}
	}
	return 0;
}

int32_t comm_ctrl_usbtocomm(uint8_t dtr, uint8_t rts)
{
	REFERENCE_PARAMETER(dtr);
	REFERENCE_PARAMETER(rts);
	
	if ((NULL == prog) || !usbtocomm_open)
	{
		return -1;
	}
	
	return 0;
}

void comm_close(void)
{
	comm_func[comm_idx].comm_close();
}

vsf_err_t comm_open(char *comport, uint32_t baudrate, uint8_t datalength,
				 char paritybit, char stopbit, char handshake)
{
	comm_idx = 0;
	
	return comm_func[comm_idx].comm_open(comport, baudrate, datalength, paritybit,
											stopbit, handshake);
}

int32_t comm_read(uint8_t *buffer, uint32_t num_of_bytes)
{
	return comm_func[comm_idx].comm_read(buffer, num_of_bytes);
}

int32_t comm_write(uint8_t *buffer, uint32_t num_of_bytes)
{
	return comm_func[comm_idx].comm_write(buffer, num_of_bytes);
}

int32_t comm_ctrl(uint8_t dtr, uint8_t rts)
{
	return comm_func[comm_idx].comm_ctrl(dtr, rts);
}

int32_t comm_flush(void)
{
	return comm_func[comm_idx].comm_flush();
}


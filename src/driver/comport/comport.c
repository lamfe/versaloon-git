/***************************************************************************
 *   Copyright (C) 2009 by Simon Qian <SimonQian@SimonQian.com>            *
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

#include <stdio.h>
#include <string.h>

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "port.h"
#include "comport.h"
#include "timer.h"

#include "programmer.h"

void comm_close_hw(void);
RESULT comm_open_hw(char *comport, uint32_t baudrate, uint8_t datalength, 
				 char paritybit, char stopbit, char handshake);
int32_t comm_read_hw(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_write_hw(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_ctrl_hw(uint8_t dtr, uint8_t rts);

void comm_close_usbtocomm(void);
RESULT comm_open_usbtocomm(char *comport, uint32_t baudrate, 
			uint8_t datalength, char paritybit, char stopbit, char handshake);
int32_t comm_read_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_write_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes);
int32_t comm_ctrl_usbtocomm(uint8_t dtr, uint8_t rts);

#define COMM_HW					0
#define COMM_USBTOCOMM			1

struct comm_func_t comm_func[] = 
{
	{
		comm_open_hw,
		comm_close_hw,
		comm_read_hw,
		comm_write_hw,
		comm_ctrl_hw,
	},
	{
		comm_open_usbtocomm,
		comm_close_usbtocomm,
		comm_read_usbtocomm,
		comm_write_usbtocomm,
		comm_ctrl_usbtocomm,
	}
};

uint32_t comm_idx = 0;

#if IS_WIN32
static HANDLE hComm = INVALID_HANDLE_VALUE;

void comm_close_hw(void)
{
	if (hComm != INVALID_HANDLE_VALUE)
	{
		CloseHandle(hComm);
		hComm = INVALID_HANDLE_VALUE;
	}
}

RESULT comm_open_hw(char *comport, uint32_t baudrate, uint8_t datalength, 
						char paritybit, char stopbit, char handshake)
{
	DCB dcb; // device control block for serial port
	COMMTIMEOUTS timeouts; // serial port timeout values
	
	if ((NULL == comport) || (datalength < 5) || (datalength > 8) 
		|| ((paritybit != COMM_PARITYBIT_NONE) 
			&& (paritybit != COMM_PARITYBIT_ODD) 
			&& (paritybit != COMM_PARITYBIT_EVEN)) 
		|| ((stopbit != COMM_STOPBIT_1) && (stopbit != COMM_STOPBIT_1P5) 
			&& (stopbit != COMM_STOPBIT_2)) 
		|| ((handshake != COMM_HANDSHAKE_NONE) 
			&& (handshake != COMM_HANDSHAKE_HARDWARE) 
			&& (handshake != COMM_HANDSHAKE_SOFTWARE)))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	
	hComm = CreateFile(comport, GENERIC_READ | GENERIC_WRITE, 0, 0, 
					   OPEN_EXISTING, 0, 0);
	if (hComm == INVALID_HANDLE_VALUE)
	{
		switch (GetLastError())
		{
			case ERROR_FILE_NOT_FOUND:
				LOG_ERROR(_GETTEXT("Serial port %s does not exist.\n"), 
						  comport);
				break;
			case ERROR_ACCESS_DENIED:
				LOG_ERROR(_GETTEXT("Access denied.\n"));
				break;
			default:
				LOG_ERROR(_GETTEXT("Fail to open comport %s: %i\n"), 
						  comport, (int)GetLastError());
				break;
		}
		return ERROR_FAIL;
	}
	
	// COM port timeout values
	// TO DO:  evaluate timeouts correctly in terms of baud rate
	timeouts.ReadIntervalTimeout = 500; // 50 ms between received characters
	timeouts.ReadTotalTimeoutMultiplier = 50;
	timeouts.ReadTotalTimeoutConstant = 100;
	timeouts.WriteTotalTimeoutMultiplier = 50; // not used
	timeouts.WriteTotalTimeoutConstant = 100; // not used
	if (!SetCommTimeouts(hComm, &timeouts))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "set new serial port timeouts");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// configure serial port for correct communication parameters
	if (!GetCommState(hComm, &dcb))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "get existing communication parameters");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// baudrate
	dcb.BaudRate = baudrate;
	// datalength
	dcb.ByteSize = datalength;
	
	// paritybit
	switch (paritybit)
	{
	case COMM_PARITYBIT_NONE:
		dcb.fParity = FALSE;
		dcb.Parity = 0;
		
		break;
	case COMM_PARITYBIT_ODD:
		dcb.fParity = TRUE;
		dcb.Parity = 1;
		
		break;
	case COMM_PARITYBIT_EVEN:
		dcb.fParity = TRUE;
		dcb.Parity = 2;
		
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), paritybit, "comm parity");
		return ERRCODE_INVALID;
		
		break;
	}
	
	// stopbit
	switch (stopbit)
	{
	case COMM_STOPBIT_1:
		dcb.StopBits = 0;
		
		break;
	case COMM_STOPBIT_1P5:
		dcb.StopBits = 1;
		
		break;
	case COMM_STOPBIT_2:
		dcb.StopBits = 2;
		
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), stopbit, "comm stopbit");
		return ERRCODE_INVALID;
		
		break;
	}

	switch (handshake)
	{
	default:
	case COMM_HANDSHAKE_NONE:
		dcb.fOutxCtsFlow = FALSE; // no
		dcb.fOutxDsrFlow = FALSE; // no
		dcb.fDtrControl = DTR_CONTROL_DISABLE; // no
		dcb.fDsrSensitivity = FALSE; // no
		dcb.fOutX = FALSE; // no
		dcb.fInX = FALSE; // no
		dcb.fErrorChar = FALSE; //no
		dcb.fNull = FALSE; // no
		dcb.fRtsControl = RTS_CONTROL_DISABLE; // no
		dcb.ErrorChar = '?'; // parity error substitution character
		break;
	}
	
	if (!SetCommState(hComm, &dcb))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "set communication parameters");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// configure serial port for correct communication parameters
	if (!GetCommState(hComm, &dcb))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "get existing communication parameters");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// clear buffer
	if (!PurgeComm(hComm, PURGE_RXABORT | PURGE_RXCLEAR 
						  | PURGE_TXABORT | PURGE_TXCLEAR))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "purge comm port");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

int32_t comm_read_hw(uint8_t *buffer, uint32_t num_of_bytes)
{
	DWORD number;
	
	if (ReadFile(hComm, buffer, num_of_bytes, &number, NULL))
	{
		return number;
	}
	else
	{
		return -1;
	}
}

int32_t comm_write_hw(uint8_t *buffer, uint32_t num_of_bytes)
{
	DWORD number;
	
	if (WriteFile(hComm, buffer, num_of_bytes, &number, NULL))
	{
		return number;
	}
	else
	{
		return -1;
	}
}

int32_t comm_ctrl_hw(uint8_t dtr, uint8_t rts)
{
	if (dtr)
	{
		EscapeCommFunction(hComm, SETDTR);
	}
	else
	{
		EscapeCommFunction(hComm, CLRDTR);
	}
	if (rts)
	{
		EscapeCommFunction(hComm, SETRTS);
	}
	else
	{
		EscapeCommFunction(hComm, CLRRTS);
	}
	
	return 0;
}

#else

static int hComm = -1;

const int speed_arr[] = {	B50, B75, B110, B134, B150, B200, B300, B600, 
							B1200, B1800, B2400, B4800, B9600, B19200, B38400, 
							B57600, B115200, B230400, B460800, B500000, 
							B576000, B921600, B1000000, B1152000, B1500000, 
							B2000000, B2500000, B3000000, B3500000, B4000000};
const uint32_t name_arr[] = {	50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 
							2400, 4800, 9600, 19200, 38400, 57600, 115200, 
							230400, 460800, 500000, 576000, 921600, 1000000, 
							1152000, 1500000, 2000000, 2500000, 3000000, 
							3500000, 4000000};

void comm_close_hw(void)
{
	if (hComm != -1)
	{
		close(hComm);
		hComm = -1;
	}
}

RESULT comm_open_hw(char *comport, uint32_t baudrate, uint8_t datalength, 
				 char paritybit, char stopbit, char handshake)
{
	struct termios opt;
	uint32_t i;
	
	if ((NULL == comport) || (datalength < 5) || (datalength > 8) 
		|| ((paritybit != COMM_PARITYBIT_NONE) 
			&& (paritybit != COMM_PARITYBIT_ODD) 
			&& (paritybit != COMM_PARITYBIT_EVEN)) 
		|| ((stopbit != COMM_STOPBIT_1) && (stopbit != COMM_STOPBIT_1P5) 
			&& (stopbit != COMM_STOPBIT_2)) 
		|| ((handshake != COMM_HANDSHAKE_NONE) 
			&& (handshake != COMM_HANDSHAKE_HARDWARE) 
			&& (handshake != COMM_HANDSHAKE_SOFTWARE)))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	
	// open comport
	hComm = open(comport, O_RDWR);
	if (-1 == hComm)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPEN), comport);
		return ERRCODE_FAILURE_OPEN;
	}
	
	// get current settings
	if (tcgetattr(hComm, &opt) != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "get existing communication parameters");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	for (i = 0; i < dimof(speed_arr); i++)
	{
		if (baudrate == name_arr[i])
		{
			cfsetispeed(&opt, speed_arr[i]);
			cfsetospeed(&opt, speed_arr[i]);
		}
	}
	
	// set data size
	opt.c_cflag &= ~CSIZE;
	switch (datalength)
	{
	case 5:
		opt.c_cflag |= CS5;
		break;
	case 6:
		opt.c_cflag |= CS6;
		break;
	case 7:
		opt.c_cflag |= CS7;
		break;
	case 8:
		opt.c_cflag |= CS8;
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), datalength, 
				  "comm datalength");
		return ERRCODE_INVALID;
		break;
	}
	
	// set parity
	switch (paritybit)
	{
	case COMM_PARITYBIT_NONE:
		opt.c_cflag &= ~PARENB;
		opt.c_iflag &= ~INPCK;
		break;
	case COMM_PARITYBIT_ODD:
		opt.c_cflag |= PARENB | PARODD;
		opt.c_iflag |= INPCK;
		break;
	case COMM_PARITYBIT_EVEN:
		opt.c_cflag |= PARENB;
		opt.c_cflag &= ~PARODD;
		opt.c_iflag |= INPCK;
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), paritybit, "comm parity");
		return ERRCODE_INVALID;
		break;
	}
	
	// set stopbit
	switch (stopbit)
	{
	case COMM_STOPBIT_1:
	case COMM_STOPBIT_1P5:
		opt.c_cflag &= ~CSTOPB;
		break;
	case COMM_STOPBIT_2:
		opt.c_cflag |= CSTOPB;
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), stopbit, "comm stopbit");
		return ERRCODE_INVALID;
		break;
	}
	
	// raw data
	opt.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
	opt.c_oflag &= ~OPOST;
	
	tcflush(hComm, TCIFLUSH);
	
	// set timeout
	opt.c_cc[VTIME] = 1;		// 100ms delay
	opt.c_cc[VMIN] = 0;
	if (tcsetattr(hComm, TCSANOW, &opt)!= 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "set new serial port timeouts");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

int32_t comm_read_hw(uint8_t *buffer, uint32_t num_of_bytes)
{
	uint32_t current_length = 0;
	int32_t tmp;
	
	while (current_length < num_of_bytes)
	{
		tmp = read(hComm, buffer + current_length, num_of_bytes - current_length);
		if (tmp < 0)
		{
			// failure
			return tmp;
		}
		if (0 == tmp)
		{
			return current_length;
		}
		current_length += tmp;
	}
	
	return current_length;
}

int32_t comm_write_hw(uint8_t *buffer, uint32_t num_of_bytes)
{
	return write(hComm, buffer, num_of_bytes);
}

int32_t comm_ctrl_hw(uint8_t dtr, uint8_t rts)
{
	REFERENCE_PARAMETER(dtr);
	REFERENCE_PARAMETER(rts);
	
	return 0;
}

#endif

static uint8_t usbtocomm_open = 0;
void comm_close_usbtocomm(void)
{
	if (!usbtocomm_open)
	{
		return;
	}
	
	if (cur_programmer != NULL)
	{
		cur_programmer->usart_fini();
		cur_programmer->peripheral_commit();
		
		if (cur_programmer->fini != NULL)
		{
			cur_programmer->fini();
		}
	}
	usbtocomm_open = 0;
}

RESULT comm_open_usbtocomm(char *comport, uint32_t baudrate, 
			uint8_t datalength, char paritybit, char stopbit, char handshake)
{
	REFERENCE_PARAMETER(comport);
	
	if (NULL == cur_programmer)
	{
		return ERROR_FAIL;
	}
	
	if ((cur_programmer->init != NULL) 
		&& (ERROR_OK != cur_programmer->init()))
	{
		return ERROR_FAIL;
	}
	
	// paritybit
	switch (paritybit)
	{
	case COMM_PARITYBIT_NONE:
		paritybit = 0;
		
		break;
	case COMM_PARITYBIT_ODD:
		paritybit = 1;
		
		break;
	case COMM_PARITYBIT_EVEN:
		paritybit = 2;
		
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), paritybit, "comm parity");
		return ERRCODE_INVALID;
		
		break;
	}
	
	// stopbit
	switch (stopbit)
	{
	case COMM_STOPBIT_1:
		stopbit = 0;
		
		break;
	case COMM_STOPBIT_1P5:
		stopbit = 1;
		
		break;
	case COMM_STOPBIT_2:
		stopbit = 2;
		
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), stopbit, "comm stopbit");
		return ERRCODE_INVALID;
		
		break;
	}
	
	// handshake
	handshake = 0;
	
	// initialize usbtocomm
	if ((ERROR_OK != cur_programmer->usart_init())
		|| (ERROR_OK != cur_programmer->usart_config(baudrate, datalength, 
												paritybit, stopbit, handshake)) 
		|| (ERROR_OK != cur_programmer->peripheral_commit()))
	{
		return ERROR_FAIL;
	}
	
	usbtocomm_open = 1;
	return ERROR_OK;
}

int32_t comm_read_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes)
{
	uint32_t start;
	uint32_t buffer_len[2];
	
	if ((NULL == cur_programmer) || (!usbtocomm_open))
	{
		return ERROR_FAIL;
	}
	
	start = get_time_in_ms();
	while ((get_time_in_ms() - start) < (50 + 5 * num_of_bytes))
	{
		LOG_PUSH();
		LOG_MUTE();
		if ((ERROR_OK != cur_programmer->usart_receive(buffer, (uint16_t)num_of_bytes)) 
			|| (ERROR_OK != cur_programmer->peripheral_commit()))
		{
			LOG_POP();
		}
		else
		{
			LOG_POP();
			return (int32_t)num_of_bytes;
		}
	}
	
	if (cur_programmer->usart_status != NULL)
	{
		memset(buffer_len, 0, sizeof(buffer_len));
		if ((ERROR_OK != cur_programmer->usart_status(buffer_len)) 
			|| (ERROR_OK != cur_programmer->peripheral_commit()))
		{
			// error
			LOG_POP();
			return -1;
		}
		else if ((buffer_len[1] != 0) 
			&& ((ERROR_OK != cur_programmer->usart_receive(buffer, (uint16_t)buffer_len[1])) 
				|| (ERROR_OK != cur_programmer->peripheral_commit())))
		{
			LOG_POP();
			return (int32_t)buffer_len[1];
		}
		else
		{
			LOG_POP();
			return 0;
		}
	}
	
	return 0;
}

int32_t comm_write_usbtocomm(uint8_t *buffer, uint32_t num_of_bytes)
{
	if ((NULL == cur_programmer) || !usbtocomm_open)
	{
		return ERROR_FAIL;
	}
	
	if ((ERROR_OK != cur_programmer->usart_send(buffer, (uint16_t)num_of_bytes)) 
		|| (ERROR_OK != cur_programmer->peripheral_commit()))
	{
		return 0;
	}
	else
	{
		return (int32_t)num_of_bytes;
	}
}

int32_t comm_ctrl_usbtocomm(uint8_t dtr, uint8_t rts)
{
	REFERENCE_PARAMETER(dtr);
	REFERENCE_PARAMETER(rts);
	
	if ((NULL == cur_programmer) || !usbtocomm_open)
	{
		return ERROR_FAIL;
	}
	
	return 0;
}

void comm_close(void)
{
	comm_func[comm_idx].comm_close();
}

RESULT comm_open(char *comport, uint32_t baudrate, uint8_t datalength, 
				 char paritybit, char stopbit, char handshake)
{
	if (!strcmp(comport, "usbtocomm"))
	{
		comm_idx = COMM_USBTOCOMM;
	}
	else
	{
		comm_idx = COMM_HW;
	}
	
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

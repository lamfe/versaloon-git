/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USART.h                                                   *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    USART interface header file                               *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

RESULT usart_init(uint8_t index);
RESULT usart_fini(uint8_t index);
RESULT usart_config(uint8_t index, uint32_t baudrate, uint8_t datalength, 
					char paritybit, char stopbit, char handshake);
RESULT usart_send(uint8_t index, uint8_t *buf, uint16_t len);
RESULT usart_receive(uint8_t index, uint8_t *buf, uint16_t len);
RESULT usart_status(uint8_t index, struct usart_status_t *status);
RESULT usart_poll(uint8_t index);

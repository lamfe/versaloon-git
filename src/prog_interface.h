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
#ifndef __PROG_INTERFACE_H_INCLUDED__
#define __PROG_INTERFACE_H_INCLUDED__

// interfaces
#define USART					(1ULL << 0)
#define SPI						(1ULL << 1)
#define I2C						(1ULL << 2)
#define GPIO					(1ULL << 3)
#define CAN						(1ULL << 4)
#define CLOCK					(1ULL << 5)
#define ADC						(1ULL << 6)
#define DAC						(1ULL << 7)
#define POWER					(1ULL << 8)
#define ISSP					(1ULL << 16)
#define JTAG_LL					(1ULL << 17)
#define JTAG_HL					(1ULL << 18)
#define MSP430_SBW				(1ULL << 19)
#define C2						(1ULL << 20)
#define MSP430_JTAG				(1ULL << 21)
#define LPC_ICP					(1ULL << 22)
#define SWD						(1ULL << 23)
#define SWIM					(1ULL << 24)
#define HV						(1ULL << 25)
#define PDI						(1ULL << 26)
#define JTAG_RAW				(1ULL << 27)
#define BDM						(1ULL << 28)
#define POLL					(1ULL << 29)
#define DUSI					(1ULL << 30)
#define INVALID_INTERFACE		(1ULL << 63)
#define INTERFACES_MASK			(USART | SPI | I2C | GPIO | CAN | CLOCK | ADC \
								 | DAC | POWER | ISSP | JTAG | MSP430_JTAG \
								 | LPC_ICP | MSP430_SBW | SWD | SWIM | HV | BDM)


// GPIO pins
#define GPIO_SRST				(1 << 0)
#define GPIO_TRST				(1 << 1)
#define GPIO_USR1				(1 << 2)
#define GPIO_USR2				(1 << 3)
#define GPIO_TCK				(1 << 4)
#define GPIO_TDO				(1 << 5)
#define GPIO_TDI				(1 << 6)
#define GPIO_RTCK				(1 << 7)
#define GPIO_TMS				(1 << 8)

// SWD
#define SWD_SUCCESS				0x00
#define SWD_RETRY_OUT			0x01
#define SWD_FAULT				0x02
#define SWD_ACK_ERROR			0x03
#define SWD_PARITY_ERROR		0x04

// JTAG
struct jtag_pos_t
{
	uint8_t ub;		// units before
	uint8_t ua;		// bits before
	uint16_t bb;		// units after
	uint16_t ba;		// bits after
};

#define JTAG_SRST				GPIO_SRST
#define JTAG_TRST				GPIO_TRST
#define JTAG_USR1				GPIO_USR1
#define JTAG_USR2				GPIO_USR2

// SWIM
#define SWIM_PIN				GPIO_TMS
#define SWIM_RST_PIN			GPIO_SRST

// BDM
#define BDM_PIN					GPIO_TMS

// SPI
#define SPI_CPOL_HIGH			0x20
#define SPI_CPOL_LOW			0x00
#define SPI_CPHA_2EDGE			0x40
#define SPI_CPHA_1EDGE			0x00
#define SPI_MSB_FIRST			0x80
#define SPI_LSB_FIRST			0x00

// DUSI
#define DUSI_CPOL_HIGH			0x20
#define DUSI_CPOL_LOW			0x00
#define DUSI_CPHA_2EDGE			0x40
#define DUSI_CPHA_1EDGE			0x00
#define DUSI_MSB_FIRST			0x80
#define DUSI_LSB_FIRST			0x00

// ISSP for PSoC
#define ISSP_PM_RESET			(1 << 0)
#define ISSP_PM_POWER_ON		(0 << 0)

#define ISSP_VECTOR_END_BIT_1	0x04
#define ISSP_VECTOR_END_BIT_0	0x00
#define ISSP_VECTOR_ATTR_BANK	(1 << 0)
#define ISSP_VECTOR_ATTR_READ	(1 << 1)
#define ISSP_VECTOR_ATTR_0s		(1 << 3)

#define ISSP_VECTOR_0S			(ISSP_VECTOR_ATTR_0s | ISSP_VECTOR_END_BIT_0)
#define ISSP_VECTOR_READ_SRAM	(ISSP_VECTOR_ATTR_READ | ISSP_VECTOR_END_BIT_1)
#define ISSP_VECTOR_WRITE_SRAM	(ISSP_VECTOR_END_BIT_1)
#define ISSP_VECTOR_WRITE_REG	(ISSP_VECTOR_ATTR_BANK | ISSP_VECTOR_END_BIT_1)

#define ISSP_WAP_OK				0x00
#define ISSP_WAP_TIMEOUT		0x01

#endif /* __PROG_INTERFACE_H_INCLUDED__ */


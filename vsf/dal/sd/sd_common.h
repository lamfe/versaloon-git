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

// 7-bit checksum polynomial: x7 + x3 + 1.
#define SD_CRC7_POLY						0x09

// Resets all cards to idle state.
#define SD_CMD(cmd)							(cmd)
#define SD_ACMD(acmd)						(acmd)
#define  SD_CMD_GO_IDLE_STATE				SD_CMD(0)
// Sends host cap supp info & act card's init.
#define  SD_CMD_SEND_OP_COND				SD_CMD(1)
// Asks any card to send CID numbers on CMD line.
#define  SD_CMD_ALL_SEND_CID				SD_CMD(2)
// Asks card to publish new RCA.
#define  SD_CMD_SEND_RELATIVE_ADDR			SD_CMD(3)
// Programs the DSR of all cards.
#define  SD_CMD_SET_DSR						SD_CMD(4)
// Switches card function.
#define  SD_CMD_SWITCH_FUNC					SD_CMD(6)
// Toggle card between stand-by & transfer states.
#define  SD_CMD_SEL_DESEL_CARD				SD_CMD(7)
// Sends SD interface conditions.
#define  SD_CMD_SEND_IF_COND				SD_CMD(8)
// Addr'd card sends Card-Specific Data (CSD).
#define  SD_CMD_SEND_CSD					SD_CMD(9)
// Addr'd card sends Card Identification (CID).
#define  SD_CMD_SEND_CID					SD_CMD(10)
// Read data until stop transmission.
#define  SD_CMD_READ_DAT_UNTIL_STOP			SD_CMD(11)
// Forces the card to stop transmission.
#define  SD_CMD_STOP_TRANSMISSION			SD_CMD(12)
// Addressed card sends its status register.
#define  SD_CMD_SEND_STATUS					SD_CMD(13)
// Read the reversed bus testing data pattern.
#define  SD_CMD_BUSTEST_R					SD_CMD(14)
// Sends an addressed card into the inactive state.
#define  SD_CMD_GO_INACTIVE_STATE			SD_CMD(15)
// Sets the block length in block (std cap SD).
#define  SD_CMD_SET_BLOCKLEN				SD_CMD(16)
// Reads a block the size sel'd by SET_BLOCKLEN.
#define  SD_CMD_READ_SINGLE_BLOCK			SD_CMD(17)
// Continuously xfers data blks until STOP_TRAN.
#define  SD_CMD_READ_MULTIPLE_BLOCK			SD_CMD(18)
// Send the bus test data pattern to a card.
#define  SD_CMD_BUSTEST_W					SD_CMD(19)
// Write a data stream until stop transmission.
#define  SD_CMD_WRITE_DAT_UNTIL_STOP		SD_CMD(20)
// Define the number of blocks to be transferred.
#define  SD_CMD_SET_BLOCK_COUNT				SD_CMD(23)
// Writes a block the size sel'd by SET_BLOCKLEN.
#define  SD_CMD_WRITE_BLOCK					SD_CMD(24)
// Continuously writes data blks until STOP_TRAN.
#define  SD_CMD_WRITE_MULTIPLE_BLOCK		SD_CMD(25)
// Programming of the CID.
#define  SD_CMD_PROGRAM_CID					SD_CMD(26)
// Programming of the programmable bits of the CSD.
#define  SD_CMD_PROGRAM_CSD					SD_CMD(27)
// Sets the write protection bit of addr'd group.
#define  SD_CMD_SET_WRITE_PROT				SD_CMD(28)
// Clrs the write protection bit of addr'd group.
#define  SD_CMD_CLR_WRITE_PROT				SD_CMD(29)
// Asks card to send status of wr protection bits.
#define  SD_CMD_SEND_WRITE_PROT				SD_CMD(30)
// Sets addr of 1st wr blk to be erased.
#define  SD_CMD_ERASE_WR_BLK_START			SD_CMD(32)
// Sets addr of last wr blk to be erased.
#define  SD_CMD_ERASE_WR_BLK_END			SD_CMD(33)
// Sets address of first erase group within a range.
#define  SD_CMD_ERASE_GROUP_START			SD_CMD(35)
// Sets address of last  erase group within a range.
#define  SD_CMD_ERASE_GROUP_END				SD_CMD(36)
// Erases all prev sel'd wr blks.
#define  SD_CMD_ERASE						SD_CMD(38)
// Used to write & read 8-bit data fields.
#define  SD_CMD_FAST_IO						SD_CMD(39)
// Sets the system into interrupt mode.
#define  SD_CMD_GO_IRQ_STATE				SD_CMD(40)
// Used to set/reset password or lock/unlock card.
#define  SD_CMD_LOCK_UNLOCK					SD_CMD(42)
// Indicates that next cmd is app cmd.
#define  SD_CMD_APP_CMD						SD_CMD(55)
// Gets/sends data blk from app cmd.
#define  SD_CMD_GEN_CMD						SD_CMD(56)
// Reads OCR register of card.
#define  SD_CMD_READ_OCR					SD_CMD(58)
// Turns the CRC option on or off.
#define  SD_CMD_CRC_ON_OFF					SD_CMD(59)
// Define the data bus width for data transfer.
#define  SD_ACMD_BUS_WIDTH					SD_ACMD(6)
// Send the SD status.
#define  SD_ACMD_SD_STATUS					SD_ACMD(13)
// Send the nbr of wr'n wr blks.
#define  SD_ACMD_SEND_NUM_WR_BLOCKS			SD_ACMD(22)
// Send the nbr of wr blks to be pre-erased.
#define  SD_ACMD_SET_WR_BLK_ERASE_COUNT		SD_ACMD(23)
// Sends host capacity support & gets OCR.
#define  SD_ACMD_SD_SEND_OP_COND			SD_ACMD(41)
// Conn/disconn 50 kOhm res on DAT3.
#define  SD_ACMD_SET_CLR_CARD_DETECT		SD_ACMD(42)
// Reads the SD configuration register.
#define  SD_ACMD_SEND_SCR					SD_ACMD(51)

// Command arguments
#define SD_ACMD41_HCS						0x40000000

#define SD_CMD8_VHS_27_36_V					0x0100
#define SD_CMD8_VHS_LOW						0x0200

#define SD_CMD8_CHK_PATTERN					0xA5

#define SD_CMD59_CRC_OPT					0x01

// OCR regist
#define  SD_OCR_LVR							(1UL << 7)
#define  SD_OCR_20_21V						(1UL << 8)
#define  SD_OCR_21_22V						(1UL << 9)
#define  SD_OCR_22_33V						(1UL << 10)
#define  SD_OCR_23_24V						(1UL << 11)
#define  SD_OCR_24_25V						(1UL << 12)
#define  SD_OCR_25_26V						(1UL << 13)
#define  SD_OCR_26_27V						(1UL << 14)
#define  SD_OCR_27_28V						(1UL << 15)
#define  SD_OCR_28_29V						(1UL << 16)
#define  SD_OCR_29_30V						(1UL << 17)
#define  SD_OCR_30_31V						(1UL << 18)
#define  SD_OCR_31_32V						(1UL << 19)
#define  SD_OCR_32_33V						(1UL << 20)
#define  SD_OCR_33_34V						(1UL << 21)
#define  SD_OCR_34_35V						(1UL << 22)
#define  SD_OCR_35_36V						(1UL << 23)
#define  SD_OCR_CCS							(1UL << 30)
#define  SD_OCR_BUSY						(1UL << 31)

// RESP
#define  SD_RESP_EMPTY						0xFF

#define  SD_DATATOKEN_RESP_ACCEPTED			0x05
#define  SD_DATATOKEN_RESP_CRC_REJECTED		0x0B
#define  SD_DATATOKEN_RESP_WR_REJECTED		0x0D
#define  SD_DATATOKEN_RESP_MASK				0x1F

#define  SD_DATATOKEN_START_BLK_MULT		0xFC
#define  SD_DATATOKEN_STOP_TRAN				0xFD
#define  SD_DATATOKEN_START_BLK				0xFE

#define  SD_DATATOKEN_ERR_ERROR				0x01
#define  SD_DATATOKEN_ERR_CC_ERROR			0x02
#define  SD_DATATOKEN_ERR_CARD_ECC_ERROR	0x04
#define  SD_DATATOKEN_ERR_OUT_OF_RANGE		0x08

// Card Status
#define SD_CS8_NONE							0x00
#define SD_CS8_IN_IDLE_STATE				0x01
#define SD_CS8_ERASE_RESET					0x02
#define SD_CS8_ILLEGAL_COMMAND				0x04
#define SD_CS8_COM_CRC_ERROR				0x08
#define SD_CS8_ERASE_SEQUENCE_ERROR			0x10
#define SD_CS8_ADDRESS_ERROR				0x20
#define SD_CS8_PARAMETER_ERROR				0x40
#define SD_CS8_ERROR_MASK					\
	(SD_CS8_ERASE_RESET | SD_CS8_ILLEGAL_COMMAND | SD_CS8_COM_CRC_ERROR | \
	SD_CS8_ERASE_SEQUENCE_ERROR | SD_CS8_ADDRESS_ERROR | SD_CS8_PARAMETER_ERROR)

#define SD_TRANSTOKEN_DATA_DIR				0x8000
#define SD_TRANSTOKEN_DATA_OUT				0x8000
#define SD_TRANSTOKEN_DATA_IN				0x0000

#define SD_TRANSTOKEN_RESP_MASK				0x00FF
#define SD_TRANSTOKEN_RESP_CHECKBUSY		0x0080
#define SD_TRANSTOKEN_RESP_CMD				0x0040
#define SD_TRANSTOKEN_RESP_CRC7				0x0020
#define SD_TRANSTOKEN_RESP_LONG				0x0010
#define SD_TRANSTOKEN_RESP_SHORT			0x0000

#define SD_TRANSTOKEN_RESP_NONE				0x0000
#define SD_TRANSTOKEN_RESP_R1				0x0061
#define SD_TRANSTOKEN_RESP_R1B				0x00E1
#define SD_TRANSTOKEN_RESP_R2				0x0012
#define SD_TRANSTOKEN_RESP_R3				0x0003
#define SD_TRANSTOKEN_RESP_R4				0x0004
#define SD_TRANSTOKEN_RESP_R5				0x0005
#define SD_TRANSTOKEN_RESP_R6				0x0066
#define SD_TRANSTOKEN_RESP_R7				0x0067

enum sd_cardtype_t
{
	SD_CARDTYPE_NONE,
	SD_CARDTYPE_SD_V1,
	SD_CARDTYPE_SD_V2,
	SD_CARDTYPE_SD_V2HC,
	SD_CARDTYPE_MMC,
	SD_CARDTYPE_MMC_HC
};

struct sd_info_t
{
	struct mal_capacity_t capacity;
	enum sd_cardtype_t cardtype;
	uint16_t frequency_kHz;
	uint8_t cid[16];
};

uint8_t sd_spi_cmd_chksum(uint8_t *data, uint32_t num);
RESULT sd_parse_csd(uint8_t *csd, struct sd_info_t *info);


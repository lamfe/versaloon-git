
#include "app_cfg.h"
#include "BDM.h"
#include "Common.h"

/*
** The following macros perform individual BDM command
** Naming convention: bdm_cmd_x_y, 
**     where x in number of input parameters,
**           y is number of output paramreters
** suffxes indicate Word or Byte width
** HC/S12(X) wait times (in case ACKN is not used) are longer than those required for HCS08,
** so HCS08 can use the same macros 
** HCS08 should support ACKN anyway
*/

//============================================================
// The following commands DO NOT expect an ACK & do not delay
// They leave the interface in the Tx condition (ready to drive )
//

//  Write command byte, truncated sequence
U8 BDM_CMD_0_0_T(U8 cmd) {
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact(sizeof(data), data, 0);
}

//  Special for Software Reset HCS08, truncated sequence
U8 BDM_CMD_1W1B_0_T(U8 cmd, U16 parameter1, U8 parameter2) {
	U8 data[4];
	data[0] = cmd;
	data[1] = (U8)(((parameter1)>>8)&0xFF);
	data[2] = (U8)((parameter1)&0xFF);
	data[3] = (U8)((parameter2)&0xFF);
	return BDM_Transact(sizeof(data), data, 0);
}

//  Special truncated sequence
U8 BDM_CMD_1B_0_T(U8 cmd, U8 parameter) {
	U8 data[2];
	data[0] = cmd;
	data[1] = (U8)((parameter)&0xFF);
	return BDM_Transact(sizeof(data), data, 0);
}
   
//============================================================
// The following commands DO NOT expect an ACK & do not delay
//
// Write cmd without ACK (HCS08/RS08/CFV1)
U8 BDM_CMD_0_0_NOACK(U8 cmd) {
   return BDM_CMD_0_0_T(cmd);
}
   
// Write cmd & read byte without ACK (HCS08)
U8 BDM_CMD_0_1B_NOACK(U8 cmd, void *result) {
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact((1 << 8) | sizeof(data), data, (U8*)result);
}

// Write cmd & byte without ACK (HCS08)
U8 BDM_CMD_1B_0_NOACK(U8 cmd, U8 parameter)	{
	U8 data[2];
	data[0] = cmd;
	data[1] = (U8)((parameter)&0xFF);
	return BDM_Transact(sizeof(data), data, 0);
}

// Write cmd & read word without ACK
U8 BDM_CMD_0_1W_NOACK(U8 cmd, void *parameter) {
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact((2 << 8) | sizeof(data), data, (U8*)parameter);
}

// Write cmd & word without ACK
U8 BDM_CMD_1W_0_NOACK(U8 cmd, U16 parameter) {
	U8 data[3];
	data[0] = cmd;
	data[1] = (U8)(((parameter)>>8)&0xFF);
	data[2] = (U8)((parameter)&0xFF);
	return BDM_Transact(sizeof(data), data, 0);
}

//====================================================================
// The following DO expect an ACK or wait at end of the command phase

// Write command - No parameter
U8 BDM_CMD_0_0(U8 cmd) {
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}
   
// Write command + word
U8 BDM_CMD_1W_0(U8 cmd, U16 parameter) {
	U8 data[3];
	data[0] = cmd;
	data[1] = (U8)(((parameter)>>8)&0xFF);
	data[2] = (U8)((parameter)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}
   
/* read word */
U8 BDM_CMD_0_1W(U8 cmd, void *parameter) {
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact(0x8040 | (2 << 8) | sizeof(data), data, (U8*)parameter);
}

/* write longword */
U8 BDM_CMD_1L_0(U8 cmd, U32 parameter) {
	U8 data[5];
	data[0] = cmd;
	data[1] = (U8)(((parameter)>>24)&0xFF);
	data[2] = (U8)(((parameter)>>16)&0xFF);
	data[3] = (U8)(((parameter)>>8)&0xFF);
	data[4] = (U8)((parameter)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}
   
/* read longword */
U8 BDM_CMD_0_1L(U8 cmd, void *parameter) {
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact(0x8040 | (4 << 8) | sizeof(data), data, (U8*)parameter);
}

/* write word & read byte (read word but return byte - HC/S12(x)) */
U8 BDM_CMD_1W_1WB(U8 cmd, U16 parameter, void *result)  {
	U8 data[3], in[2], ret;
	data[0] = cmd;
	data[1] = (U8)(((parameter)>>8)&0xFF);
	data[2] = (U8)((parameter)&0xFF);
	ret = BDM_Transact(0x8040 | (2 << 8) | sizeof(data), data, in);
	if (!ret)
	{
		*(U8*)result = in[parameter & 1];
	}
	return ret;
}

/* write 2 words */
U8 BDM_CMD_2W_0(U8 cmd, U16 parameter1, U16 parameter2) {
	U8 data[5];
	data[0] = cmd;
	data[1] = (U8)(((parameter1)>>8)&0xFF);
	data[2] = (U8)((parameter1)&0xFF);
	data[3] = (U8)(((parameter2)>>8)&0xFF);
	data[4] = (U8)((parameter2)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write word and a byte (sends 2 words, the byte in both high and low byte of the 16-bit value) */
U8 BDM_CMD_2WB_0(U8 cmd, U16 parameter1, U8 parameter2) {
	U8 data[5];
	data[0] = cmd;
	data[1] = (U8)(((parameter1)>>8)&0xFF);
	data[2] = (U8)((parameter1)&0xFF);
	data[3] = (U8)((parameter2)&0xFF);
	data[4] = (U8)((parameter2)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write word & read word */
U8 BDM_CMD_1W_1W(U8 cmd, U16 parameter, void *result) {
	U8 data[3];
	data[0] = cmd;
	data[1] = (U8)(((parameter)>>8)&0xFF);
	data[2] = (U8)((parameter)&0xFF);
	return BDM_Transact(0x8040 | (2 << 8) | sizeof(data), data, (U8*)result);
}

/* write word & read word */
U8 BDM_CMD_1Wle_1Wle(U8 cmd, U16 parameter, void *result) {
	U8 data[3], ret;
	data[0] = cmd;
	data[1] = (U8)((parameter)&0xFF);
	data[2] = (U8)(((parameter)>>8)&0xFF);
	ret = BDM_Transact(0x8040 | (2 << 8) | sizeof(data), data, (U8*)result);
	if (!ret)
	{
		*(U16*)result = ((*(U16*)result << 8) & 0xFF00) + ((*(U16*)result >> 8) & 0x00FF);
	}
	return ret;
}

/* read word HCS08 */
U8 BDM_CMD_0_1B(U8 cmd, void *result)	{
	U8 data[1];
	data[0] = cmd;
	return BDM_Transact(0x8040 | (1 << 8) | sizeof(data), data, (U8*)result);
}

/* write byte HCS08 */
U8 BDM_CMD_1B_0(U8 cmd, U8 parameter) {
	U8 data[2];
	data[0] = cmd;
	data[1] = (U8)((parameter)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write word & read byte (read word but return byte - HCS08 */
U8 BDM_CMD_1W_1B(U8 cmd, U16 parameter, void *result) {
	U8 data[3];
	data[0] = cmd;
	data[1] = (U8)(((parameter)>>8)&0xFF);
	data[2] = (U8)(((parameter)>>0)&0xFF);
	return BDM_Transact(0x8040 | (1 << 8) | sizeof(data), data, (U8*)result);
}
                                                         
/* write word & read byte (read word but return byte - HCS08 */
U8 BDM_CMD_1Wle_1B(U8 cmd, U16 parameter, void *result) {
	U8 data[3];
	data[0] = cmd;
	data[1] = (U8)((parameter)&0xFF);
	data[2] = (U8)(((parameter)>>8)&0xFF);
	return BDM_Transact(0x8040 | (1 << 8) | sizeof(data), data, (U8*)result);
}
                                                         
/* write 1 words then delay then 1byte HCS08 */
U8 BDM_CMD_1W1B_0(U8 cmd, U16 parameter1, U8 parameter2) {
	U8 data[4];
	data[0] = cmd;
	data[1] = (U8)(((parameter1)>>8)&0xFF);
	data[2] = (U8)((parameter1)&0xFF);
	data[3] = (U8)((parameter2))&0xFF;
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write 1 24-bit value then 1 byte  */
U8 BDM_CMD_1A1B_0(U8 cmd, U32 addr, U8 value) {
	U8 data[5];
	data[0] = cmd;
	data[1] = (U8)(((addr)>>16)&0xFF);
	data[2] = (U8)(((addr)>>8)&0xFF);
	data[3] = (U8)((addr)&0xFF);
	data[4] = (U8)((value)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write 1 24-bit value then 1 word  */
U8 BDM_CMD_1A1W_0(U8 cmd, U32 addr, U16 value) {
	U8 data[6];
	data[0] = cmd;
	data[1] = (U8)(((addr)>>16)&0xFF);
	data[2] = (U8)(((addr)>>8)&0xFF);
	data[3] = (U8)((addr)&0xFF);
	data[4] = (U8)(((value)>>8)&0xFF);
	data[5] = (U8)((value)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write 1 24-bit value then 1 longword  */
U8 BDM_CMD_1A1L_0(U8 cmd, U32 addr, U32 value) {
	U8 data[8];
	data[0] = cmd;
	data[1] = (U8)(((addr)>>16)&0xFF);
	data[2] = (U8)(((addr)>>8)&0xFF);
	data[3] = (U8)((addr)&0xFF);
	data[4] = (U8)(((value)>>24)&0xFF);
	data[5] = (U8)(((value)>>16)&0xFF);
	data[6] = (U8)(((value)>>8)&0xFF);
	data[7] = (U8)((value)&0xFF);
	return BDM_Transact(0x8040 | sizeof(data), data, 0);
}

/* write 24-bit address & read byte */
U8 BDM_CMD_1A_1B(U8 cmd, U32 addr, void *result) {
	U8 data[4];
	data[0] = cmd;
	data[1] = (U8)(((addr)>>16)&0xFF);
	data[2] = (U8)(((addr)>>8)&0xFF);
	data[3] = (U8)((addr)&0xFF);
	return BDM_Transact(0x8040 | (1 << 8) | sizeof(data), data, (U8*)result);
}

/* write 24-bit address & read word */
U8 BDM_CMD_1A_1W(U8 cmd, U32 addr, void *result) {
	U8 data[4];
	data[0] = cmd;
	data[1] = (U8)(((addr)>>16)&0xFF);
	data[2] = (U8)(((addr)>>8)&0xFF);
	data[3] = (U8)((addr)&0xFF);
	return BDM_Transact(0x8040 | (2 << 8) | sizeof(data), data, (U8*)result);
}

/* write 24-bit address & read longword */
U8 BDM_CMD_1A_1L(U8 cmd, U32 addr, void *result) {
	U8 data[4];
	data[0] = cmd;
	data[1] = (U8)(((addr)>>16)&0xFF);
	data[2] = (U8)(((addr)>>8)&0xFF);
	data[3] = (U8)((addr)&0xFF);
	return BDM_Transact(0x8040 | (4 << 8) | sizeof(data), data, (U8*)result);
}

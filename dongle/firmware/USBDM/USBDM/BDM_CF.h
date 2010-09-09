/*! \file
    \brief Header file for BDM routines.

 */

#ifndef _BDMCF_H_
#define _BDMCF_H_

#if (CAPABILITY&CAP_CFVx)

//==============================================================
// Shared interface
void  bdmcf_init(void);
void  jtag_init(void);

void bdm_suspend(void);
void bdmCF_off(void);
U16  bdm_targetVddMeasure(void);
void bdmcf_interfaceIdle(void);
U8   bdmCF_powerOnReset(void);
void bdmCF_suspend(void);
void bdmcf_interfaceIdle(void);

//===============================================================
// Coldfire BDM mode interface
U8   bdmcf_resync(void);
U8   bdmcf_halt(void);
U8   bdmcf_reset(U8 bkpt);
U8   bdmcf_ta(U8 time_10us);
U8   bdmcf_tx_msg(U16 data);
U8   bdmcf_rx_msg(U16 *data);
U8   bdmcf_txrx_msg(U16 *data);
void bdmcf_tx(U8 count, U8 *data);
U8   bdmcf_complete_chk(U16 next_cmd);
U8   bdmcf_complete_chk_rx(void);
U8   bdmcf_tx_msg_half_rx(U16 data);
U8   bdmcf_rx(U8 count, U8 *data);
U8   bdmcf_rxtx(U8 count, U8 *data, U16 next_cmd);
U16  bdmcf_txRx16(U16 data);
U8   bdmcf_setSpeed(U16 freq);

// Prototypes for the Rx and Tx functions
void bdmcf_tx8_1(U8 data);
U8   bdmcf_rx8_1(void);
U8   bdmcf_txrx8_1(U8 data);
U8   bdmcf_txrx_start(void);

//=================================================================
// JTAG mode interface
void jtag_transition_shift(U8 mode);
void jtag_write(U8 tap_transition, U8 bit_count, U8 *writePtr);
void jtag_read(U8 tap_transition, U8 bit_count, U8 *readPtr);
void jtag_transition_reset(void);
void jtag_transition_idle(void);
void jtag_read_write(U8 transitionToIdle, U8 bitCount, U8 *readWritePtr);

#endif // (CAPABILITY&CAP_CFVx)

#endif // _BDMCF_H_


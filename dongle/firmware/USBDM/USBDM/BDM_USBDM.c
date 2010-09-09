/*! \file
    \brief USBDM - HCS12, HCS08, RS08 & CFV1 low level BDM communication.

   \verbatim
   This software was modified from \e TBDML software

   USBDM
   Copyright (C) 2007  Peter O'Donoghue

   Turbo BDM Light (TBDML)
   Copyright (C) 2005  Daniel Malik

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   \endverbatim

\verbatim
Change History
-=======================================================================================
| 14 Apr 2010 | bdmHC12_confirmSpeed() - added FDATA method                        - pgo
| 11 Dec 2009 | Increased size of BDM_SYNC_REQ to cope with 32.5kHz clock          - pgo
| 19 Oct 2009 | Modified Timer code - Folded together with JS16 code               - pgo
| 22 Sep 2009 | Made ACK enabling/disabling explicitly after Connect{}             - pgo
| 15 Sep 2009 | Corrected clearing of ACKN in bdm_physicalConnect{}                - pgo
| 15 Sep 2009 | Increased size of BDM_SYNC_REQ                                     - pgo
| 15 Sep 2009 | Moved bdm_aknInit{} from bdm_physicalConnect{}to bdm_connect{}     - pgo
|    Sep 2009 | Major changes for V2                                               - pgo
-=======================================================================================
|  2 Jun 2009 | Merged USBDM/USBDM_CF versions                                     - pgo
| 15 Apr 2009 | Updated BRSET offsets to be correct with compiler fix MTWX31284    - pgo
|  3 Apr 2009 | Made Rx/TxEmpty empty (were indicating failure even if retry OK!)  - pgo
|  3 Apr 2009 | Minor change to bdmConnect{}- no connection reported if too fast   - pgo
|  1 Apr 2009 | Changes to bdmConnect{} & bdm_enableBDM{} to improve Alt CLK       - pgo
| 30 Mar 2009 | Changes to bdm_setInterfaceLevel{} to remove glitch                - pgo
|  3 Feb 2009 | Extended bdm_setInterfaceLevel{}                                   - pgo
| 27 Jan 2009 | Introduced doACKN_WAIT64{} & doACKN_WAIT150{}                      - pgo
| 21 Jan 2009 | Minor changes to bdm_writeBDMControl{}                             - pgo
| 15 Jan 2009 | Moved shared routines to BDMCommon.c [TBLCF version]               - pgo
| 14 Jan 2009 | Moved timer routines & macros to Timer.c/Timer.h                   - pgo
| 27 Dec 2008 | Added bdm_setInterfaceLevel{}                                      - pgo
|  9 Dec 2008 | Added hack for MC51AC256                                           - pgo
|  9 Dec 2008 | Removed BKGD check in bdm_cycleTargetVddOn{}                       - pgo
| 11 Nov 2008 | Unified Timer MACROS                                               - pgo
|  5 Nov 2008 | Re-arrangement of Target Vdd control routines                      - pgo
|  1 Nov 2008 | Minor mods & corrected comments in bdm_syncMeasure{}               - pgo
|  1 Nov 2008 | Removed glitch in bdm_syncMeasure{} (LVC125 only)                  - pgo
| 23 Oct 2008 | Reviewed stack size                                                - pgo
| 22 Oct 2008 | Increased cable_status.waitX size                                  - pgo
| 22 Oct 2008 | Changed RESET macros - more consistent                             - pgo
|  4 Oct 2008 | Renamed some MACROs for consistency                                - pgo
|  1 Sep 2008 | Fixed several bugs related use of 1T45 buffers                     - pgo
| 29 Aug 2008 | Disabled ACMP0 as it clashes with Vdd5_En* on CLC                  - pgo
| 10 Aug 2008 | Fixed bug in multi-line MACROs                                     - pgo
| 10 Jul 2008 | Modified Speed Guessing routine (now smarter!)                     - pgo
| 10 Jul 2008 | Modified Reset code slightly                                       - pgo
|  9 Jul 2008 | Modified Rx routine mask to remove glitch                          - pgo
|  7 Jul 2008 | Modified Reset sequence to properly Cycle Vdd                      - pgo
|  2 Jul 2008 | More thorough checking for target Vdd                              - pgo
|  2 Jul 2008 | Modified using Alt BDM clock handling                              - pgo
| 19 Jun 2008 | Expanded Flash programming options of RS08                         - pgo
| 14 Jun 2008 | Added RS08 Trim value calculation                                  - pgo
| 10 Jun 2008 | Added Colfire V1 interface                                         - pgo
|  6 May 2008 | Ported to JB16 from JM60 - lotsa changes                           - pgo
| 12 Apr 2008 | Added reconnect before software reset                              - pgo
|  7 Apr 2008 | Fixed WAIT_WITH_TIMEOUT.. macros                                   - pgo
|  7 Apr 2008 | Fixed glitch in bdmHCS_init                                        - pgo
|  6 Apr 2008 | Added Vdd change detect handler & POR                              - pgo
| 23 Mar 2008 | Fixed several BMD commands not using ACKN                          - pgo
|  3 Mar 2008 | Started changes for JM60 - lots                                    - pgo
+=======================================================================================

ToDo / Issues
+=========================================================================
|  * The use of LVC45 driver ICs produces glitches and/or bus conflicts.  
|    These are unavoidable unless external resistors are used.             
+=========================================================================

\endverbatim
*/

#if TARGET_HARDWARE!=H_VERSALOON
#include <hidef.h>          /* common defines and macros */
#endif
#include "Derivative.h"
#include "Common.h"
#include "Configure.h"
#include "Commands.h"
#if TARGET_HARDWARE!=H_VERSALOON
#include "bdm.h"
#else
#include "bdm_usbdm.h"
#endif
#include "bdmMacros.h"
#include "TargetDefines.h"
#include "CmdProcessing.h"
#include "BDMCommon.h"
#include "BDM_RS08.h"

#if (TARGET_HARDWARE!=H_VERSALOON)
#if (__VERSION__ < 5029)
#error "BDM.c requires a Compiler fix (MTWX31284) that is only available in Versions later than 5.0.29"
#endif

void enInterrupts(void) {
   asm("cli");
}
#endif

//=============================================================================================================================================
#define BDM_SYNC_REQ      960U //!< us - length of the longest possible SYNC REQUEST pulse (128 BDM cycles @ 400kHz = 320us plus some extra time)
#define SYNC_TIMEOUT      460U //!< us - longest time for the target to completed a SYNC pulse (16+128+margin cycles @ 400kHz = 375us)
#define ACKN_TIMEOUT      375U //!< us - longest time after which the target should produce ACKN pulse (150 cycles @ 400kHz = 375us)
#define SOFT_RESET      10000U //!< us - longest time needed for soft reset of the BDM interface (512 BDM cycles @ 400kHz = 1280us)
#define RESET_LENGTH      100U //!< ms - time of RESET assertion
#define RESET_WAIT        270U //!< ms - max time to wait for the RESET pin to come high after release

/* Function prototypes */
       U8   bdm_syncMeasure(void);
       void bdmHCS_interfaceIdle(void);
#if TARGET_HARDWARE!=H_VERSALOON
static U8   bdmHC12_alt_speed_detect(void);
#endif

//========================================================
//
#pragma DATA_SEG __SHORT_SEG Z_PAGE
// MUST be placed into the direct segment (assumed in ASM code).
       U8 bitCount;  //!< Used as a general purpose variable in the bdm_Tx{} & bdm_Rx{}etc.
#if TARGET_HARDWARE!=H_VERSALOON
static U8 rxTiming1; //!< bdm_Rx timing constant #1
static U8 rxTiming2; //!< bdm_Rx timing constant #2
static U8 rxTiming3; //!< bdm_Rx timing constant #3
static U8 txTiming1; //!< bdm_Tx timing constant #1
static U8 txTiming2; //!< bdm_Tx timing constant #2
static U8 txTiming3; //!< bdm_Tx timing constant #3

// pointers to current bdm_Rx & bdm_Tx routines
U8   (*bdm_rx_ptr)(void) = bdm_rxEmpty; //!< pointers to current bdm_Rx routine
void (*bdm_tx_ptr)(U8)   = bdm_txEmpty; //!< pointers to current bdm_Tx routine
#endif

//========================================================
//
#pragma DATA_SEG DEFAULT

#pragma MESSAGE DISABLE C5909 // Disable warnings about Assignment in condition
#pragma MESSAGE DISABLE C4000 // Disable warnings about Condition always true

//! Read Target BDM status
//!
//!  Depending on target architecture this reads from
//!  - BDCSC,
//!  - BDMSTS,
//!  - XCSR.
//!
//! @param bdm_sts value read
//! @return
//!   \ref BDM_RC_OK             => success  \n
//!   \ref BDM_RC_UNKNOWN_TARGET => unknown target
//!
U8 bdm_readBDMStatus(U8 *bdm_sts) {

   switch (cable_status.target_type) {
      case T_HC12:
         BDM12_CMD_BDREADB(HC12_BDMSTS,bdm_sts);
         return BDM_RC_OK;
      case T_HCS08:
      case T_RS08:
         BDM08_CMD_READSTATUS(bdm_sts);
         return BDM_RC_OK;
      case T_CFV1:
         BDMCF_CMD_READ_XCSR(bdm_sts);
         return BDM_RC_OK;
      default:
         return BDM_RC_UNKNOWN_TARGET; // Don't know how to check status on this one!
   }
}

//! Write Target BDM control register
//!
//!  Depending on target architecture this writes to \n
//!   - BDCSC,  \n
//!   - BDMSTS, \n
//!   - XCSR.
//!
//!  @param value => value to write
//!
static void writeBDMControl(U8 value) {

   switch (cable_status.target_type) {
      case T_HC12:
         BDM12_CMD_BDWRITEB(HC12_BDMSTS,value);
         break;
      case T_HCS08:
      case T_RS08:
         BDM08_CMD_WRITECONTROL(value);
         break;
      case T_CFV1:
         BDMCF_CMD_WRITE_XCSR(value);
         break;
   }
}

//! Write Target BDM control register [masked value]
//!
//!  Depending on target architecture this writes to \n
//!   - BDCSC,  \n
//!   - BDMSTS, \n
//!   - XCSR.   \n
//!
//! @note - This routine may modify the value written if bdm_option.useAltBDMClock is active
//!
//! @return
//!   \ref BDM_RC_OK             => success  \n
//!   \ref BDM_RC_UNKNOWN_TARGET => unknown target
//!
U8 bdm_writeBDMControl(U8 bdm_ctrl) {
U8 statusClearMask; // Bits to clear in control value
U8 statusSetMask;   // Bits to set in control value
U8 statusClkMask;   // The position of the CLKSW bit in control register

   // Get clock select mask for this target (CLKSW bit in BDM control register)
   switch (cable_status.target_type) {
      case T_HC12:
         statusClkMask = HC12_BDMSTS_CLKSW;
         break;
      case T_HCS08:
      case T_RS08:
         statusClkMask = HC08_BDCSCR_CLKSW;
         break;
      case T_CFV1:
         statusClkMask = CFV1_XCSR_CLKSW;
         break;
      default:
         return BDM_RC_UNKNOWN_TARGET; // Don't know how to check status on this one!
   }

   // default - no modification of value given
   statusClearMask = 0xFF; // Clear no bits
   statusSetMask   = 0x00; // Set no bits

   // Construct the masks to modify control value
   switch (bdm_option.useAltBDMClock) {
      case CS_ALT:     statusClearMask = ~statusClkMask;  break; // Force CLKSW = 0
      case CS_NORMAL:  statusSetMask   =  statusClkMask;  break; // Force CLKSW = 1
   }

   bdm_ctrl &= statusClearMask;
   bdm_ctrl |= statusSetMask;

   writeBDMControl(bdm_ctrl);

   return BDM_RC_OK;
}

//! If BDM mode is not enabled in target yet, enable it so it can be made active
//!
//! @return
//!   \ref BDM_RC_OK             => success  \n
//!   \ref BDM_RC_UNKNOWN_TARGET => unknown target \n
//!   \ref BDM_RC_BDM_EN_FAILED  => enabling BDM failed (target not connected or wrong speed ?)
//!
U8 bdm_enableBDM() {
U8 bdm_sts;
U8 enbdmMask;
U8 rc;

   if (cable_status.target_type==T_CFV1) // Choose appropriate mask
      enbdmMask = CFV1_XCSR_ENBDM;
   else
      enbdmMask = HC12_BDMSTS_ENBDM;

   rc = bdm_readBDMStatus(&bdm_sts); // Get current status
   if (rc != BDM_RC_OK)
      return rc;

   if ((bdm_sts & enbdmMask) != 0) // BDM already enabled?
      return BDM_RC_OK;

   bdm_sts |= enbdmMask;

   // Try to enable
   writeBDMControl(bdm_sts);

   rc = bdm_readBDMStatus(&bdm_sts); // Enabled now?
   if (rc != BDM_RC_OK)
      return rc;

   return ((bdm_sts & enbdmMask)!=0)? BDM_RC_OK : BDM_RC_BDM_EN_FAILED;
}
#pragma MESSAGE DEFAULT C4000 // Restore warnings about Condition always true
#pragma MESSAGE DEFAULT C5909 // Restore warnings about Assignment in condition

#if 0
//! Used before some commands to ensure there is a useful connection to target available.
//!
//! Some non-intrusive commands still require a running processor e.g. READ_BYTE
//! This function will force a stopped CPU into active BDM mode
//!
//! @return
//!   \ref BDM_RC_OK  => success  \n
//!   otherwise       => various errors
//!
U8 bdm_makeActiveIfStopped(void) {

U8 rc = BDM_RC_OK;
U8 bdm_sts;

   if (cable_status.target_type == T_HC12) // Not supported on HC12
      return BDM_RC_OK; // not considered an error

   if (bdm_option.autoReconnect &&            // Auto re-connect enabled &
      (cable_status.speed==SPEED_SYNC)) { // Target supports Sync
      rc = bdm_syncMeasure(); // Check connection speed
#if TARGET_HARDWARE!=H_VERSALOON
      if (rc == 0)
         rc = bdm_RxTxSelect();
#endif
      }
   if (rc != BDM_RC_OK)
      return rc;

   rc = bdm_readBDMStatus(&bdm_sts);
   if (rc != BDM_RC_OK)
      return rc;

   // If HC08/RS08 processor is WAITing or STOPped then force background
   if (bdm_sts&HC08_BDCSCR_WS)
      (void)bdm_halt();

   return BDM_RC_OK;
}
#endif

//!  Tries to connect to target - doesn't try other strategies such as reset.
//!  This function does a basic connect sequence.  It doesn't configure the BDM
//!  registers on the target or enable ACKN
//!
//! @return
//!    == \ref BDM_RC_OK                  => success                             \n
//!    == \ref BDM_RC_VDD_NOT_PRESENT     => no target power present             \n
//!    == \ref BDM_RC_RESET_TIMEOUT_RISE  => RESET signal timeout - remained low \n
//!    == \ref BDM_RC_BKGD_TIMEOUT        => BKGD signal timeout - remained low  \n
//!    != \ref BDM_RC_OK                  => other failures
//!
U8 bdm_physicalConnect(void){
U8 rc;

//   cable_status.reset = NO_RESET_ACTIVITY; // Clear the reset flag
   cable_status.speed   = SPEED_NO_INFO;   // No connection
#if TARGET_HARDWARE!=H_VERSALOON
   bdm_rx_ptr           = bdm_rxEmpty;     // Clear the Tx/Rx pointers
   bdm_tx_ptr           = bdm_txEmpty;     //    i.e. no com. routines found
#endif

   bdmHCS_interfaceIdle(); // Make sure interface is idle

   // Target has power?
   if ((cable_status.power == BDM_TARGET_VDD_NONE) ||
       (cable_status.power == BDM_TARGET_VDD_ERR))
      return BDM_RC_VDD_NOT_PRESENT;

#if (CAPABILITY&CAP_RESET)
   // Wait with timeout until both RESET and BKGD  are high
   if (bdm_option.useResetSignal) {
      WAIT_WITH_TIMEOUT_MS(RESET_WAIT,(RESET_IN!=0)&&(BDM_IN!=0));
      if (RESET_IN==0)
         return(BDM_RC_RESET_TIMEOUT_RISE);  // RESET timeout
   }
#else
   WAIT_WITH_TIMEOUT_MS(RESET_WAIT,(BDM_IN!=0));
#endif

   if (BDM_IN==0)
      return(BDM_RC_BKGD_TIMEOUT);  // BKGD timeout

#if TARGET_HARDWARE==H_VERSALOON
   BDM_Init();
#endif

   rc = bdm_syncMeasure();
#if TARGET_HARDWARE!=H_VERSALOON
   if (rc != BDM_RC_OK) // try again
      rc = bdm_syncMeasure();
   if ((rc != BDM_RC_OK) &&                // Trying to measure SYNC was not successful
       (bdm_option.guessSpeed) &&          // Try alternative method if enabled
       (cable_status.target_type == T_HC12)) { // and HC12 target
      rc = bdmHC12_alt_speed_detect();     // Try alternative method (guessing!)
   }
   if (rc != BDM_RC_OK)
      return(rc);

   // If at least one of the two methods succeeded, we can select
   //  the right Rx and Tx routines
   rc = bdm_RxTxSelect();
   if (rc != BDM_RC_OK) {
      cable_status.speed = SPEED_NO_INFO;  // Indicate that we do not have a connection
      }
#endif
   return(rc);
}

//! Connect to target
//!
//!  This function may cycle the target power in attempting to connect. \n
//!  It enables BDM on the target if connection is successful.
//!
//! @return
//!    == \ref BDM_RC_OK               => success  \n
//!    == \ref BDM_RC_VDD_NOT_PRESENT  => no target power present \n
//!    == \ref BDM_RC_RESET_TIMEOUT_RISE    => RESET signal timeout - remained low \n
//!    == \ref BDM_RC_BKGD_TIMEOUT     => BKGD signal timeout - remained low \n
//!    != \ref BDM_RC_OK               => other failures \n
//!
U8 bdm_connect(void) {
U8 rc;

   rc = bdm_physicalConnect();

   if ((rc != BDM_RC_OK) && bdm_option.cycleVddOnConnect) {
      // No connection to target - cycle power if allowed
      (void)bdm_cycleTargetVdd(RESET_SPECIAL); // Ignore errors
      rc = bdm_physicalConnect(); // Try connect again
   }

   if (rc != BDM_RC_OK)
      return rc;

#if TARGET_HARDWARE!=H_VERSALOON
   bdm_acknInit();  // Try the ACKN feature
#else
   rc = bdm_acknInit();
   if (rc != BDM_RC_OK)
      return rc;
#endif

   // Try to enable BDM
   rc = bdm_enableBDM();
   return rc;
}

#if (CAPABILITY&CAP_RESET)

//! Resets the target using the BDM reset line.
//! @note
//!   Not all targets have a reset pin or support reset to BDM mode with a reset pin.
//!
//! @param mode
//!    - \ref RESET_SPECIAL => Reset to special mode,
//!    - \ref RESET_NORMAL  => Reset to normal mode
//!
//! @return
//!   \ref BDM_RC_OK  => Success \n
//!   \ref BDM_RC_BKGD_TIMEOUT     => BKGD pin stuck low \n
//!   \ref BDM_RC_RESET_TIMEOUT_RISE    => RESET pin stuck low \n
//!
U8 bdm_hardwareReset(U8 mode) {

   if (!bdm_option.useResetSignal)
      return BDM_RC_ILLEGAL_PARAMS;
      // Doesn't return BDM_RC_ILLEGAL_COMMAND as method is controlled by a parameter

   DISABLE_RESET_SENSE_INT(); // Mask RESET IC interrupts

   bdmHCS_interfaceIdle();  // Make sure BDM interface is idle

   mode &= RESET_MODE_MASK;

   // Wait with timeout until both RESET and BKGD are high
//   WAIT_WITH_TIMEOUT_MS(RESET_WAIT, (RESET_IN!=0)&&(BDM_IN!=0));
   WAIT_WITH_TIMEOUT_S(2, (RESET_IN!=0)&&(BDM_IN!=0));

   if (RESET_IN==0) return(BDM_RC_RESET_TIMEOUT_RISE);
   if (BDM_IN==0)   return(BDM_RC_BKGD_TIMEOUT);

#if TARGET_HARDWARE!=H_VERSALOON
   bdm_txPrepare(); // BKGD & RESET 3-state control on DIR_PORT
#endif

   if (mode==RESET_SPECIAL) {
#if TARGET_HARDWARE!=H_VERSALOON
      BDM_OUT = 0;        // Drive BKGD low
      BDM_ENABLE_TX();
#else
      BDM_CLR();
#endif

      WAIT_US(RESET_SETTLE); // Wait for signals to settle
   }

   RESET_LOW();

   WAIT_MS(RESET_LENGTH);  // Wait for reset pulse duration

   RESET_3STATE();

   // Wait with timeout until RESET is high
   WAIT_WITH_TIMEOUT_MS(RESET_WAIT, (RESET_IN!=0));

   // Assume RESET risen - check later after cleanUp

#if (DEBUG&RESET_DEBUG)
   DEBUG_PIN   = 0;
   DEBUG_PIN   = 1;
#endif

   // Wait 1 ms before releasing BKGD
   WAIT_MS(1);  // Wait for Target to start up after reset

   bdmHCS_interfaceIdle(); // Place interface in idle state

   // Wait recovery time before allowing anything else to happen on the BDM
   WAIT_MS(RESET_RECOVERY);  // Wait for Target to start up after reset

#if (DEBUG&RESET_DEBUG)
   DEBUG_PIN   = 1;
   DEBUG_PIN   = 0;
#endif

   if (RESET_IN==0)      // RESET failed to rise
      return(BDM_RC_RESET_TIMEOUT_RISE);

   CLEAR_RESET_SENSE_FLAG(); // Clear RESET IC Event
   ENABLE_RESET_SENSE_INT(); // Enable RESET IC interrupts

   return(BDM_RC_OK);
}

#endif //(CAPABILITY&CAP_RESET)

//! Resets the target using BDM commands
//!
//! @note
//!    Not all targets support reset using BDM commands
//!
//! @param mode
//!    - \ref RESET_SPECIAL => Reset to special mode,
//!    - \ref RESET_NORMAL  => Reset to normal mode
//!
//!
//! @return
//!    \ref BDM_RC_OK                     => Success \n
//!    \ref BDM_RC_BKGD_TIMEOUT           => BKGD pin stuck low \n
//!    \ref BDM_RC_RESET_TIMEOUT_RISE     => RESET pin stuck low \n
//!    \ref BDM_RC_UNKNOWN_TARGET         => Don't know how to reset this type of target! \n
//!
U8 bdm_softwareReset(U8 mode) {
U8 rc;

   if (cable_status.target_type == T_HC12) // Doesn't support s/w reset
      return BDM_RC_ILLEGAL_PARAMS; // HC12 doesn't have s/w reset
      // Doesn't return BDM_RC_ILLEGAL_COMMAND as method is controlled by a parameter

   mode &= RESET_MODE_MASK;

   // Make sure of connection
   rc = bdm_connect();

   // Make sure Active background mode (in case target is stopped!)
   if (bdm_halt() != BDM_RC_OK)
      rc = bdm_connect();

   switch (cable_status.target_type) {
      case T_HCS08:
         BDM08_CMD_RESET();
         break;
      case T_RS08:
         BDMRS08_CMD_RESET();
         break;
      case T_CFV1:
         // Force reset (& set up to halt in BDM mode on various errors)
         if (mode == RESET_SPECIAL) {
            BDM_CMD_1B_0_T(_BDMCF_WRITE_CSR2_BYTE,
                  CFV1_CSR2_BDFR|CFV1_CSR2_COPHR|CFV1_CSR2_IOPHR|CFV1_CSR2_IADHR|CFV1_CSR2_BFHBR);
         }
         else {
            BDM_CMD_1B_0_T(_BDMCF_WRITE_CSR2_BYTE,
                  CFV1_CSR2_BDFR|CFV1_CSR2_COPHR|CFV1_CSR2_IOPHR|CFV1_CSR2_IADHR);
         }
         break;
      default:
         return BDM_RC_UNKNOWN_TARGET; // Don't know how to reset this one!
   }

   // BDM Interface is left in Tx mode by the above reset command

#if TARGET_HARDWARE==H_VERSALOON
   bdmHCS_interfaceIdle();
#endif

   if (mode == RESET_SPECIAL) {  // Special mode - need BKGD held low out of reset
#if TARGET_HARDWARE!=H_VERSALOON
      BDM_OUT  = 0;      // drive BKGD low (out of reset)
      BDM_ENABLE_TX();
#else
      BDM_CLR();
#endif
   }

#if (DEBUG&RESET_DEBUG)
   DEBUG_PIN     = 0;
   DEBUG_PIN     = 1;
#endif

   WAIT_US(RESET_SETTLE);   // Wait for target to start reset (and possibly assert reset)

#if (CAPABILITY&CAP_RESET)
   if (bdm_option.useResetSignal) {
      // Wait with timeout until RESET is high (may be held low by processor)
      WAIT_WITH_TIMEOUT_MS(RESET_WAIT, (RESET_IN!=0));
      // Assume RESET risen - check later after cleanup
      }
#endif // (CAPABILITY&CAP_RESET)

#if (DEBUG&RESET_DEBUG)
   DEBUG_PIN   = 1;
   DEBUG_PIN   = 0;
#endif

   if (mode == 0) {   // Special mode - release BKGD
      WAIT_US(BKGD_WAIT);        // Wait for BKGD assertion time after reset rise
      BDM_3STATE_TX();
      WAIT_US(RESET_SETTLE);     // Time to wait for signals to settle
   }

#if TARGET_HARDWARE!=H_VERSALOON
   bdm_txFinish();
#endif

#if (DEBUG&RESET_DEBUG)
   DEBUG_PIN   = 0;
   DEBUG_PIN   = 1;
#endif

   // Wait recovery time before allowing anything else to happen on the BDM
   WAIT_MS(RESET_RECOVERY);     // Wait for Target to start up after reset

   bdmHCS_interfaceIdle();      // Place interface in idle state

#if (DEBUG&RESET_DEBUG)
   DEBUG_PIN   = 1;
   DEBUG_PIN   = 0;
#endif

   //  bdm_halt();  // For RS08?
   return BDM_RC_OK;
}

//! Resets the target
//!
//! Resets the target using any of the following as needed:
//! - Vdd power cycle - POR,
//! - Software BDM command or
//! - Hardware reset.
//!
//! @param mode
//!    - \ref RESET_SPECIAL => Reset to special mode,
//!    - \ref RESET_NORMAL  => Reset to normal mode
//!
//! @return
//!    == \ref BDM_RC_OK  => Success \n
//!    != \ref BDM_RC_OK  => various errors
//
U8 bdm_targetReset( U8 mode ) {
U8 rc = BDM_RC_OK;

   // Power-cycle-reset - applies to all chips
   if (bdm_option.cycleVddOnReset)
      rc = bdm_cycleTargetVdd(mode);

   if (rc != BDM_RC_OK)
      return rc;

   // Software (BDM Command) reset - HCS08, RS08 & Coldfire
   rc = bdm_softwareReset(mode);

#if (CAPABILITY&CAP_RESET)
   // Hardware (RESET pin) reset
   // HC12s and some HCS08s/RS08s & CFv1 (may not result in BDM mode) support this
   if ((rc != BDM_RC_OK) && bdm_option.useResetSignal)
      rc = bdm_hardwareReset(mode);
#endif //(CAPABILITY&CAP_RESET)

#if 0
   // Assume we have lost connection after reset
   cable_status.ackn   = WAIT;              // Turn off ACKN feature
   cable_status.reset  = NO_RESET_ACTIVITY; // BDM resetting the target doesn't count as a reset!
   bdm_rx_ptr          = bdm_rxEmpty;       // Clear the Tx/Rx pointers
   bdm_tx_ptr          = bdm_txEmpty;       //    i.e. no routines found

   if (cable_status.speed == SPEED_USER_SUPPLIED) {          // User specified speed?
      (void) bdmHC12_confirmSpeed(cable_status.sync_length); // Confirm we can still operate at that speed
      rc = bdm_enableBDM();                                  //  & enable BDM mode
      }
   else if (bdm_option.autoReconnect)           // Re-connect if Auto re-connect enabled
      (void)bdm_connect();                      //    Done even if no SYNC feature - may be slow!
   else     
     cable_status.speed  = SPEED_NO_INFO;             // Indicate we no longer have a connection
#endif      
   return rc;
}

//!  Called when target power has been externally cycled.
//!  Holds RESET & BKGD low while waiting for Vdd to rise.
//!
//! @return
//!    == \ref BDM_RC_OK              => Success               \n
//!    == \ref BDM_RC_VDD_NOT_PRESENT => Vdd failed to rise    \n
//!    == \ref BDM_RC_RESET_TIMEOUT_RISE   => Reset failed to rise  \n
//!    == \ref BDM_RC_BKGD_TIMEOUT    => BKGD failed to rise
//!
U8 bdmHCS_powerOnReset(void) {
U8 rc = BDM_RC_OK;

#if (CAPABILITY&CAP_VDDSENSE)

   bdmHCS_interfaceIdle();  // Make sure BDM interface is idle

   // BKGD pin=L, wait...
#if TARGET_HARDWARE!=H_VERSALOON
   DATA_PORT      = BDM_DIR_Rx_WR;
   DATA_PORT_DDR  = BDM_DIR_Rx_MASK|BDM_OUT_MASK;
#else
   BDM_CLR();
#endif
   RESET_LOW();
   WAIT_US(RESET_SETTLE);

   // Release reset
   RESET_3STATE();

   // Wait for Vdd to rise within 50% of 3V and RESET to return high
   // RESET rise may be delayed by target POR
   WAIT_WITH_TIMEOUT_MS( 250 /* ms */, (bdm_targetVddMeasure()>75)&&
                                       (!bdm_option.useResetSignal)||(RESET_IN!=0));

   // Let signals settle & CPU to finish reset (with BKGD held low)
   WAIT_US(BKGD_WAIT);

   if (bdm_targetVddMeasure()<=70) // Vdd didn't turn on!
      rc = BDM_RC_VDD_NOT_PRESENT;

   if (bdm_option.useResetSignal && (RESET_IN==0)) // RESET didn't rise
      rc = BDM_RC_RESET_TIMEOUT_RISE;

   bdmHCS_interfaceIdle();  // Make sure BDM interface is idle (BKGD now high)

   // Let signals settle
   WAIT_US(RESET_SETTLE);

   if (BDM_IN==0) // BKGD didn't rise!
      rc = BDM_RC_BKGD_TIMEOUT;

   cable_status.reset = RESET_DETECTED;    // Record the fact that reset was asserted

#endif // (CAPABILITY&CAP_VDDSENSE)
   return(rc);
}

//!  Directly set the interface levels
//!
//! @param level see \ref InterfaceLevelMasks_t
//!
//! @return
//!    == \ref BDM_RC_OK     => Success \n
//!    != \ref BDM_RC_OK     => various errors
//
U8  bdm_setInterfaceLevel(U8 level) {

#if TARGET_HARDWARE!=H_VERSALOON
   BDM_DIR_DDR &= BDM_DIR; // Disable BKGD control from BDM_DIR
#else
   BDM_Fini();
#endif

   switch (level&SI_BKGD) {
      case SI_BKGD_LOW :  // BKGD pin=L
#if TARGET_HARDWARE!=H_VERSALOON
         DATA_PORT      = BDM_DIR_Rx_WR;                // Enable BKGD buffer,  BKGD = 0
         DATA_PORT_DDR  = BDM_DIR_Rx_MASK|BDM_OUT_MASK; // Enable port pins
#else
         BDM_CLR();
#endif
         break;
      case SI_BKGD_HIGH : // BKGD pin=H
#if TARGET_HARDWARE!=H_VERSALOON
         DATA_PORT      = BDM_DIR_Rx_WR  |BDM_OUT_MASK; // Enable BKGD buffer,  BKGD = 1
         DATA_PORT_DDR  = BDM_DIR_Rx_MASK|BDM_OUT_MASK; // Enable port pins
#else
         BDM_SET();
#endif
         break;
      default :           // BKGD pin=3-state
#if TARGET_HARDWARE!=H_VERSALOON
         DATA_PORT      = BDM_DIR_Rx_RD;                // Disable BKGD buffer, BKGD = X
         DATA_PORT_DDR  = BDM_DIR_Rx_MASK;              // Enable port pins
#else
         BDM_SET();
#endif
         break;
   }

#if (CAPABILITY & CAP_RESET)
   switch (level&SI_RESET) {
      case SI_RESET_LOW : // RESET pin=L
         RESET_LOW();
         break;
      default :
         RESET_3STATE();
         break;
   }
#endif

#if (CAPABILITY & CAP_RESET)
   return (RESET_IN?SI_RESET_3STATE:SI_RESET_LOW)|(BDM_IN?SI_BKGD_3STATE:SI_BKGD_LOW);
#else
   return (BDM_IN?SI_BKGD:0);
#endif
}

//! \brief Measures the SYNC length and writes the result into cable_status structure
//!
//! Method :
//!   - One timer channel is set up for input capture on BKGD.
//!   - A second timer channel is used for timeout.
//!   - Trigger SYNC response from target by manually taking BKGD low and then pulsed briefly high.
//!   - The timer channel captures the falling edge and rising edges from the target.
//!   - SYNC duration is calculated from above times.
//!
//! @return
//!   \ref BDM_RC_OK               => Success \n
//!   \ref BDM_RC_BKGD_TIMEOUT     => BKGD pin stuck low or no response
//!
U8 bdm_syncMeasure(void) {
U16 time;

   cable_status.speed = SPEED_NO_INFO;  // Indicate that we do not have a clue about target speed at the moment...

#if TARGET_HARDWARE!=H_VERSALOON
   bdmHCS_interfaceIdle();        // Make sure BDM interface is idle

   // Wait with timeout until BKGD is high
   WAIT_WITH_TIMEOUT_US(BDM_SYNC_REQ, (BDM_IN!=0));

   if (BDM_IN==0)
      return(BDM_RC_BKGD_TIMEOUT); // Timeout !

#if (DRIVER == LVC45)
   // This sequence briefly drives BKGD high (670 ns) before driving low
   // Can't be avoided if using LVC45 buffer (conflict on BDM_OUT pin otherwise)
   // BKGD pin=L, RESET pin=3S (RESET 3-state & pulled high)
   DATA_PORT     = 0           |BDM_DIR_Rx_WR;
   DATA_PORT_DDR = BDM_OUT_MASK|BDM_DIR_Rx_MASK;
#else
   // This version directly drives BKGD low - no glitches!
   // BKGD pin=L, RESET pin=3S (RESET 3-state & pulled high)
   DATA_PORT     = 0           |BDM_DIR_Rx_RD;
   DATA_PORT_DDR = BDM_OUT_MASK|BDM_DIR_Rx_MASK;
   DATA_PORT     = 0           |BDM_DIR_Rx_WR;
#endif

   WAIT_US(BDM_SYNC_REQ);          // Wait SYNC request time

#if (DEBUG&SYNC_DEBUG)
   DEBUG_PIN   = 0;
   DEBUG_PIN   = 1;
#endif

   TPMSC      = ACKN_TPMxSC_VALUE;   // Set timer tick rate

   // Set up Input capture & timeout timers
   SYNC_TPMxCnSC       = SYNC_TPMxCnSC_FALLING_EDGE_MASK;  // TPMx.CHb : Input capture, falling edge on pin

   TPMCNTH              = 0;                               // Restart timer counter
   SYNC_TPMxCnSC_CHF    = 0;                               // TPMx.CHa : Clear capture flag
   TIMEOUT_TPMxCnSC_CHF = 0;                               // TPMx.CHb : Clear timeout flag
   TIMEOUT_TPMxCnVALUE  = ACKN_MICROSECOND(SYNC_TIMEOUT);  // Set Timeout value

   asm {
      LDX   #BDM_OUT_MASK|BDM_DIR_Rx_WR            // Mask to Drive BKGD high
      LDA   #BDM_OUT_MASK|BDM_DIR_Rx_RD            // Mask to 3-state BKGD
      STX   DATA_PORT                              // [3   wpp]  Drive BKGD high (for 500 ns)
      STA   DATA_PORT                              // [3   wpp]  3-state BKGD
      // It took 125 ns cycles from bringing BKGD high to 3-state (3 cycles/24 MHz)
      // Target drives BKGD line after 16 BDM clock cycles
      // Fast enough up to approx 125ns/(16 BDM cycles) = 128 MHz BDM Frequency
   }

   while ((SYNC_TPMxCnSC_CHF==0)&&(TIMEOUT_TPMxCnSC_CHF==0)){   // Wait for capture or timeout
   }
   time          = SYNC_TPMxCnVALUE;                 // TPM1.Ch1 : Save time of start of the SYNC interval
   SYNC_TPMxCnSC = SYNC_TPMxCnSC_RISING_EDGE_MASK;   // TPM1.Ch1 : Clear IC flag, config for IC rising edge

   // It takes 23 cycles to re-enable capture (worst case) which is good enough up to 128*24/23 = 130 MHz!
   while ((SYNC_TPMxCnSC_CHF==0)&&(TIMEOUT_TPMxCnSC_CHF==0)){   // Wait for capture or timeout
   }
   time = SYNC_TPMxCnVALUE-time;                     // Calculate length of the SYNC pulse

#if (DEBUG&SYNC_DEBUG)
   DEBUG_PIN   = 1;
   DEBUG_PIN   = 0;
#endif

   if (TIMEOUT_TPMxCnSC_CHF==1) {
#else
   if (BDM_Sync(&time)) {
#endif
      return(BDM_RC_SYNC_TIMEOUT);         // Timeout !
   }

#if TARGET_HARDWARE!=H_VERSALOON
#if (ACKN_TIMER_FREQ==3000000UL)
   cable_status.sync_length=(time<<2)+(time<<4);  // multiply by 20 to get the time in 60MHz ticks
#elif (ACKN_TIMER_FREQ==6000000UL)
   cable_status.sync_length=(time<<1)+(time<<3);  // multiply by 10 to get the time in 60MHz ticks
#elif (ACKN_TIMER_FREQ==24000000UL)
   cable_status.sync_length=(time<<1)+(time>>1);  // multiply by 2.5 to get the time in 60MHz ticks
#elif (ACKN_TIMER_FREQ==30000000UL)
   cable_status.sync_length=(time<<1);            // multiply by 2 to get the time in 60MHz ticks
#elif (ACKN_TIMER_FREQ==15000000UL)
   cable_status.sync_length=(time<<2);            // multiply by 4 to get the time in 60MHz ticks
#else
   #error "Please fix SYNC length calculation for new Bus Frequency"
#endif
#else
   cable_status.sync_length=128 * 60000 / time;
#endif
   cable_status.speed = SPEED_SYNC;           // SYNC feature is supported by the target

   return(0);
}

//! Enables ACKN and prepares the timer for easy ACKN timeout use
//!
#if TARGET_HARDWARE!=H_VERSALOON
void bdm_acknInit(void) {
#else
U8 bdm_acknInit(void) {
#endif
U8 rc;

#if TARGET_HARDWARE!=H_VERSALOON
   // Set up Input capture & timeout timers
   ACKN_TPMxCnSC       = ACKN_TPMxCnSC_RISING_EDGE_MASK;  // TPMx.CHb : Input capture, falling edge on pin
#endif

#if (DEBUG&ACK_DEBUG)
   DEBUG_PIN     = 0;
   DEBUG_PIN     = 1;
   DEBUG_PIN     = 0;
#endif

   cable_status.ackn = ACKN;              // Switch ACKN on

   // Send the ACK enable command to the target
   if (cable_status.target_type==T_CFV1)
      rc = BDMCF_CMD_ACK_ENABLE();
   else
      rc = BDM_CMD_ACK_ENABLE();

#if TARGET_HARDWARE!=H_VERSALOON
   // If ACKN fails turn off ACKN (early HCS12 or RS08 target)
   if (rc == BDM_RC_ACK_TIMEOUT)
      cable_status.ackn = WAIT;  // Switch the ackn feature off
#else
   return rc;
#endif
}

#if TARGET_HARDWARE!=H_VERSALOON
//! Depending on ACKN mode this function:       \n
//!   - Waits for ACKN pulse with timeout.
//!      OR
//!   - Busy waits for 64 target CPU clocks
//!
//! @return
//!   \ref BDM_RC_OK           => Success \n
//!   \ref BDM_RC_ACK_TIMEOUT  => No ACKN detected [timeout]
//!
U8 doACKN_WAIT64(void) {
   bdm_txFinish();
   if (cable_status.ackn==ACKN) {
      TPMCNTH              = 0;                               // Restart timer counter
      TIMEOUT_TPMxCnVALUE  = ACKN_MICROSECOND(ACKN_TIMEOUT);  // Set ACKN Timeout value
      TIMEOUT_TPMxCnSC_CHF = 0;                               // TPMx.CHb : Clear timeout flag

      // Wait for pin capture or timeout
      while ((ACKN_TPMxCnSC_CHF==0)&&(TIMEOUT_TPMxCnSC_CHF==0)) {
      }
      if (TIMEOUT_TPMxCnSC_CHF) {    // Timeout -
         return BDM_RC_ACK_TIMEOUT;  //   Return timeout error
      }
   }
   else asm {
      // Wait for 64 target clock cycles
         ldhx  cable_status.wait64_cnt   // Number of loop iterations to wait
      loop:
         aix   #-1      ; [2]
         cphx  #0       ; [3]
         bne   loop     ; [3] 8 cycles / iteration
   }
   return BDM_RC_OK;
}

//! Depending on ACKN mode this function:       \n
//!   - Waits for ACKN pulse with timeout.
//!      OR
//!   - Busy waits for 150 target CPU clocks
//!
//! @return
//!   \ref BDM_RC_OK           => Success \n
//!   \ref BDM_RC_ACK_TIMEOUT  => No ACKN detected [timeout]
//!
U8 doACKN_WAIT150(void) {
   bdm_txFinish();
   if (cable_status.ackn==ACKN) {
      TPMCNTH              = 0;                               // Restart timer counter
      TIMEOUT_TPMxCnVALUE  = ACKN_MICROSECOND(ACKN_TIMEOUT);  // Set ACKN Timeout value
      TIMEOUT_TPMxCnSC_CHF = 0;                               // TPMx.CHb : Clear timeout flag

      // Wait for pin capture or timeout
      while ((ACKN_TPMxCnSC_CHF==0)&&(TIMEOUT_TPMxCnSC_CHF==0)) {
      }
      if (TIMEOUT_TPMxCnSC_CHF) {    // Timeout -
         return BDM_RC_ACK_TIMEOUT;  //   Return timeout error
      }
   }
   else asm {
      // Wait for 150 target clock cycles
         ldhx  cable_status.wait150_cnt    // Number of loop iterations to wait
      loop:
         aix   #-1      ; [2]
         cphx  #0       ; [3]
         bne   loop     ; [3] 8 cycles / iteration
   }
   return BDM_RC_OK;
}
#endif

//!  Halts the processor - places in background mode
//!
U8 bdm_halt(void) {

   if (cable_status.target_type==T_CFV1)
      return BDMCF_CMD_BACKGROUND();
   else
      return BDM_CMD_BACKGROUND();
}

//! Commences full-speed execution on the target
//!
U8 bdm_go(void) {
U32 csr;

   if (cable_status.target_type == T_CFV1) {
      // Clear Single-step mode
      BDMCF_CMD_READ_DREG(CFV1_CSR, &csr);
      csr &= ~CFV1_CSR_SSM;
#ifdef MC51AC256_HACK
      csr |= CFV1_CSR_VBD; // Hack for MC51AC256 - turn off Bus visibility
#endif
      BDMCF_CMD_WRITE_DREG(CFV1_CSR,csr);

      return BDMCF_CMD_GO();
      }
   else
      return BDM_CMD_GO();
}

//!  Executes a single instruction on the target
//!
U8 bdm_step(void) {
U32 csr;

   if (cable_status.target_type == T_CFV1) {
      // Set Single-step mode
      BDMCF_CMD_READ_DREG(CFV1_CSR, &csr);
      csr |= CFV1_CSR_SSM;
#ifdef MC51AC256_HACK
      csr |= CFV1_CSR_VBD; // Hack for MC51AC256 - turn off Bus visibility
#endif
      BDMCF_CMD_WRITE_DREG(CFV1_CSR,csr);

      return BDMCF_CMD_GO();
      }
   else
      return BDM_CMD_TRACE1();
}

//!  Turns off the BDM interface
//!
//!  Depending upon settings, may leave target power on.
//!
void bdmHCS_off( void ) {
#if ((CAPABILITY & CAP_FLASH) != 0)
   (void)bdmSetVpp(BDM_TARGET_VPP_OFF);
#endif
   if (!bdm_option.leaveTargetPowered)
      VDD_OFF();
#if TARGET_HARDWARE==H_VERSALOON
   BDM_Fini();
#endif
   bdmHCS_interfaceIdle();
}

//!  Sets the BDM interface to a suspended state
//!
//!  - All signals undriven \n
//!  - All voltages off.
//!
void bdmHCS_suspend(void) {
   bdmHCS_off();
   VDD_OFF();
}

//!  Sets the BDM hardware interface to an idle condition
//!
//!  BKGD   signal undriven, PUP \n
//!  RESET  signal undriven, PUP \n
//!  VPP,Flash voltage etc unchanged. \n
//!
//! @note Assumes that \ref bdmHCS_init{} has been previously called at least once.
//!
void bdmHCS_interfaceIdle(void) {

   RESET_3STATE();
   BDM_3STATE_TX();

#if TARGET_HARDWARE!=H_VERSALOON
   DATA_PORT      = BDM_DIR_Rx_RD;      //
   DATA_PORT_DDR  = BDM_DIR_Rx_MASK;    // BDM_DIR_Rx enabled on DATA_PORT
   DIR_PORT_DDR  &= ~BDM_DIR_MASK;      // Transfer BKGD 3-state control to BDM_DIR_Rx on DATA_PORT
#endif

//   - BKGD & RESET pins on BDM cable are 3-state and may be read through BDM_IN & RESET_IN
//   - BDM_OUT is 3-state
//   - BDM_DIR_Rx is enabled and BDM_DIR is 3-state so BKGD direction is controlled by BDM_DIR_Rx
//   - BDM_DIR_Rx is BDM_DIR_Rx_RD (and hence BKGD pin is 3-state)
//   - RESET_OUT is undriven (but has PUP so is high) (needed if O/C buffer or FET is used as driver on RESET pin)
//   - RESET_DIR is inactive (high or low as required) i.e. external 3-state buffer/transceiver is disabled/input.
}

//!  Initialises the BDM hardware interface
//!
//!  Initialises required PUPs on port pins
//!  BKGD   signal undriven, PUP \n
//!  RESET  signal undriven, PUP \n
//!  VPP,Flash voltage etc unchanged. \n
//!
void bdmHCS_init(void) {

#if TARGET_HARDWARE!=H_VERSALOON
   // Enable pull-ups on I/O lines (RST_IO & BKGD have double PUPs)
#ifdef DATA_PORT_PEBIT
   // A single bit used to control PUPs on entire port
   DATA_PORT_PEBIT = 1;
#else
   // Individually controlled PUPs
   BDM_OUT_PER     = 1;     // Prevent drive
   BDM_DIR_Rx_PER  = 1;     // Keep driver idle
   RESET_IN_PER    = 1;     // Needed for input level translation to 5V
   RESET_OUT_PER   = 1;     // Holds RESET_OUT inactive when unused
#endif
#endif
   bdmHCS_interfaceIdle();
}

#if TARGET_HARDWARE!=H_VERSALOON
//! Prepares for transmission of BDM data
//!
//! Interrupts need to be disabled for the duration of all BDM commands. \n
//! It is up to the caller to see to this.
//!
void bdm_txPrepare(void) {
   disableInterrupts();

   // Hand over BKGD direction control from BDM_DIR_Rx (on BDM_DATA_PORT) to BDM_DIR (on BDM_DIR_PORT)
   // Note: direction of the BKGD driver must be controlled during transition to avoid glitches on BDM
                                                // Keep BKGD 3-state until transfer
   DATA_PORT     = BDM_OUT_MASK|BDM_DIR_Rx_RD;  // Make sure BKGD is currently 3-state &
                                                //    will be driven high once the driver is enabled
   BDM_3STATE_TX();                             // Make sure BKGD is 3-state when control is transferred to DIR_PORT

   DIR_PORT_DDR  |= BDM_DIR_MASK;               // Transfer 3-state control to DIR_PORT
   DATA_PORT_DDR = BDM_OUT_MASK;                // BDM_OUT active others 3-state

   // BKGD direction controlled by BDM_DIR in DIR_PORT
   // BDM_DIR is set to disable the BKGD driver
   //    Macros used to enable/disable BKGD drive
   //      BDM_ENABLE_ASM_TX/BDM_ENABLE_TX()            // Enable BKGD
   //      BDM_3STATE_ASM_TX/BDM_3STATE_TX()            // 3-state BKGD
   // BKGD is driven via BDM_OUT in DATA_PORT - hard coded into the assembly code
   // Other bits of DATA_PORT are configured as inputs and have no effect when written
   // DATA_PORT = 0xxxxxxxx => BKGD driven low  (if enabled by BDM_DIR)
   // DATA_PORT = 1xxxxxxxx => BKGD driven high (if enabled by BDM_DIR)
   // RESET state is unchanged
}

//! Finishes transmission of BDM data and sets up for reception.
//!
void bdm_txFinish(void) {
   // Hand over BKGD direction control from BDM_DIR (on BDM_DIR_PORT) to BDM_DIR_Rx (on BDM_DATA_PORT)
   // Note: direction of the BKGD driver must be controlled during transition to avoid glitches on BDM
   BDM_3STATE_TX();                              // Make sure BKGD is currently 3-state
   DATA_PORT     = BDM_OUT_MASK|BDM_DIR_Rx_RD;   // Make sure BKGD will remain 3-state after handover

   DATA_PORT_DDR  = BDM_OUT_MASK|BDM_DIR_Rx_MASK; // Transfer BDM_DIR control from DIR_PORT to DATA_PORT
   DIR_PORT_DDR  &= ~BDM_DIR_MASK;
   enableInterrupts();

   // BKGD is now driven via BDM_OUT (DATA_PORT.7) with 3-state control on BDM_DIR_Rx (DATA_PORT.6).
   // Other bits of DATA_PORT are configured as inputs and have no effect when written.
   // The following examples assume active low enables of the buffer
   // DATA_PORT = 00xxxxxxx => BKGD driven low
   // DATA_PORT = 11xxxxxxx => BKGD 3-state
   // DATA_PORT = 10xxxxxxx => BKGD driven high (not used by Rx routines)
   // DATA_PORT = 01xxxxxxx => BKGD 3-state (not used by Rx routines?)
}
#endif

#pragma MESSAGE DISABLE C1404 // Disable warnings about no return statement

#if TARGET_HARDWARE!=H_VERSALOON
//==============================================================
// Tx Routines bdm_tx..
//==============================================================
// Transmit 8 bits of data, MSB first
// These routines assume the following:
//    BKGD direction controlled via BDM_DIR in DIR_PORT
//      [use BDM_3STATE_TX/BDM_3STATE_ASM_TX or BDM_ENABLE_TX/BDM_ENABLE_ASM_TX macros]
//    BKGD output value may be controlled by DATA_PORT.7
//    The rest of DATA_PORT must be disabled call bdm_txPrepare
// Leaves BDM_OUT 3-state via BDM_DIR call bdm_txFinish to clean up
//
#if (BDM_OUT_BIT != 7)
#error "The bdm_txN routines requires BDM_OUT=PTA.7"
#endif

//=========================================================================
#pragma MESSAGE DISABLE C5703 // Disable warnings about unused parameter
//! Dummy BDM Tx routine
//!
void bdm_txEmpty(U8 data) {
   // If BDM command is executed with this routine set command failed...
//   commandBuffer[0] = BDM_RC_NO_CONNECTION;
}

//=========================================================================
// 2,4,6
void bdm_tx1(U8 data) {
   asm {
      LDHX  @DATA_PORT     // Point HX at DATA_PORT
      BDM_ENABLE_ASM_TX    // Enable BKGD (high)

      /* bit 7 (MSB) */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
                           // --- 6
      STA   ,X             // [2    wp]   Drive data to BDM
                           // --- 2
      ROR   ,X             // [4  rfwp]   Drive BKGD high
                           // --- 4
      LSLA                 // [1  p   ]   Next bit

      /* bit 6 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 5 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 4 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 3 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 2 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 1 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 0 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high

      BDM_3STATE_ASM_TX    // [5      ]   3-state BDM
      ACKN_SETUP_ASM       // [5 rfwpp]   Set up for ACKN
      // ACKN is ready within about 10 bus cycles <500ns
      // Required to be ready 32+16 BDC cycles
      // Adequate for 100MHz+ !
      }
}

//=========================================================================
// 3,4,6
void bdm_tx2(U8 data) {
   asm {
      LDHX  @DATA_PORT     // Point HX at DATA_PORT
      BDM_ENABLE_ASM_TX    // Enable BKGD (high)

      /* bit 7 (MSB) */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
                           // --- 6
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
                           // --- 3
      ROR   ,X             // [4  rfwp]   Drive BKGD high
                           // --- 4
      LSLA                 // [1  p   ]   Next bit

      /* bit 6 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 5 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 4 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 3 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 2 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 1 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 0 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high

      BDM_3STATE_ASM_TX    // [5      ]   3-state BDM
      ACKN_SETUP_ASM       // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 4,4,6
void bdm_tx3(U8 data) {
   asm {
      LDHX  @DATA_PORT     // Point HX at DATA_PORT
      BDM_ENABLE_ASM_TX    // Enable BKGD (high)

      /* bit 7 (MSB) */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
                           // --- 6
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
                           // --- 4
      ROR   ,X             // [4  rfwp]   Drive BKGD high
                           // --- 4
      LSLA                 // [1  p   ]   Next bit

      /* bit 6 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 5 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 4 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 3 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 2 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 1 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high
      LSLA                 // [1  p   ]   Next bit

      /* bit 0 */
      SEC                  // [1  p   ]   Used by ROR
      CLR   ,X             // [4  rfwp]   Drive BKGD low
      NOP                  // [1      ]
      NOP                  // [1      ]
      STA   ,X             // [2    wp]   Drive data to BDM
      ROR   ,X             // [4  rfwp]   Drive BKGD high

      BDM_3STATE_ASM_TX    // [5      ]   3-state BDM
      ACKN_SETUP_ASM       // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 4,5,8
void bdm_tx4(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 8
      NOP                           // [1      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 4
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 5
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 5,5,8
void bdm_tx5(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 8
      NOP                           // [1      ]
      NOP                           // [1      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 5
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 5
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 5,6,8
void bdm_tx6(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 8
      NOP                           // [1      ]
      NOP                           // [1      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 5
      NOP                           // [1      ]
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 6
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 6,7,8
void bdm_tx7(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 8
      BRN   *+0                     // [3      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 6
      NOP                           // [1      ]
      NOP                           // [1      ]
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 7
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 7,8,8
void bdm_tx8(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 8
      NOP                           // [1      ]
      BRN   *+0                     // [3      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 7
      NOP                           // [1      ]
      NOP                           // [1      ]
      NOP                           // [1      ]
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 8
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 8,9,8
void bdm_tx9(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 8
      NOP                           // [1      ]
      NOP                           // [1      ]
      BRN   *+0                     // [3      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 8
      NOP                           // [1      ]
      BRN   *+0                     // [3      ]
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 9
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// 9,10,9
void bdm_tx10(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      LDX   #8

   Loop:
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 9
      BRN   *+0                     // [3      ]
      BRN   *+0                     // [3      ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // --- 9
      BRN   *+0                     // [3      ]
      NOP                           // [1      ]
      NOP                           // [1      ]
      LSLA                          // [1      ]   Set up next data bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // --- 10
      NOP                           // [1      ]
      DBNZX Loop                    // [4      ]   Count bit, finished? no - loop

      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}

//=========================================================================
// >=10,>=12,>=18
//! Generic BDM Tx routine - used for a range of speeds
//!
void bdm_txGeneric(U8 data) {
   asm {
      BDM_ENABLE_ASM_TX             // Enable BKGD (high)
      MOV  #8,bitCount              // # of bits to send

   Loop:
      LDX   txTiming3               // [3      ]
      DBNZX *+0                     // [4n     ]
      MOV   #0,DATA_PORT            // [4  pwpp]   Drive BKGD low
                                    // --- 14+4n   (>=18)
      LDX   txTiming1               // [3      ]
      DBNZX *+0                     // [4n     ]
      STA   DATA_PORT               // [3   wpp]   Drive data to BDM
                                    // ---  6+4n   (>=10)
      LDX   txTiming2               // [3      ]
      DBNZX *+0                     // [4n     ]
      LSLA                          // [1      ]   Next bit
      MOV   #BDM_OUT_MASK,DATA_PORT // [4  pwpp]   Drive BKGD high
                                    // ---  8+4n   (>=12)
      DBNZ  bitCount,Loop           // [7      ]
                                    // ===========
      BDM_3STATE_ASM_TX             // [5      ]   3-state BDM
      ACKN_SETUP_ASM                // [5 rfwpp]   Set up for ACKN
      }
}
#pragma MESSAGE DEFAULT C5703 // Restore warnings about unused parameter


//=================================================================================
// DATA_PORT values for Rx Routines - assumes BDM_DIR_Rx controls BKGD enable/direction
#define RxBDM_LOW         (BDM_DIR_Rx_WR|0)  // BDM_OUT=1 and enabled
#define RxBDM_3_STATE     (BDM_DIR_Rx_RD|0)  // BDM_OUT=0 but 3-state


//==============================================================
//  Rx Routines bdm_rx..
//==============================================================
//  Receive 8 bit of data, MSB first
//  These routines assume the following call bdm_txFinish:
//     BDM direction to be controlled by BDM_DIR_Rx in DATA_PORT
//     BDM output value may be controlled by DATA_PORT.7 hard coded in bdm_rxN
//     BDM input value may be read from DATA_PORT.0 hard coded in rxStackDecode
//     The rest of PTA must be disabled - configured as inputs
//  Leaves BDM_OUT 3-state via BDM_DIR_Rx
//  Note: These routines have been re-timed to allow for the phase difference b/w read and write instructions.

#pragma MESSAGE DISABLE C20001 // Disable warnings about stackpointer

//!  Decodes values recorded by RX functions
//!
//!  Expects LSB data in X and remaining 7 bytes on stack. \n
//!  It is expected that caller will JUMP into this routine.
//!
static void rxStackDecode(void) {
   asm {
      MOV   #8,bitCount       // # of bits
decode:
#if (BDM_IN_BIT >= 4)         // Rotate left
#if (BDM_IN_BIT <= 4)
      ROLX                    // Get the interesting bit into C
#endif
#if (BDM_IN_BIT <= 5)
      ROLX
#endif
#if (BDM_IN_BIT <= 6)
      ROLX
#endif
      ROLX
#else // Rotate right
#if (BDM_IN_BIT >= 3)
      RORX                     // Get the interesting bit into C
#endif
#if (BDM_IN_BIT >= 2)
      RORX
#endif
#if (BDM_IN_BIT >= 1)
      RORX
#endif
      RORX
#endif
      RORA                     // and rotate it into A from the top
      PULX                     // get the next value from stack
      DBNZ  bitCount,decode
      PSHX                     // that was one pop too many, so push the value back
   }
}
#pragma MESSAGE DEFAULT C20001 // Restore warnings about stackpointer

//! Dummy BDM Rx routine
//!
//! When no function appropriate for the target speed can be found the following
//! routine is selected.  This is just to make things safe and to make sure
//! there is a place to jump to in such a case
//!
U8 bdm_rxEmpty( void ) {
   // if BDM command is executed with this routine set command failed...
//   commandBuffer[0] = BDM_RC_NO_CONNECTION;
   return(0);
}

#pragma MESSAGE DISABLE C1404  // Disable warnings about no return statement

#if (CAPABILITY&CAP_CFVx)  // CF Version in JM16 pressed for space - Smaller code
//=======================================================================
// 3,5,15
#pragma MESSAGE DISABLE C20001 // Disable warnings about different stack ptr
U8 bdm_rx1(void) {
#pragma NO_RETURN
   asm {
      SEI
      LDHX  @DATA_PORT           // Point HX at DATA_PORT
      MOV   #7,bitCount          // [3  pwp]   Drive BKGD low

      LDA   #RxBDM_3_STATE       // Writing A to DATA_PORT 3-states BKGD
      STA   ,X                   // 3-state BKGD pin

   Loop:
      /* bit 7..1 */
      MOV   #RxBDM_LOW,DATA_PORT // [3  pwp]   Drive BKGD low
                                 // ------- 15
                                 // [1+   p]
      STA   ,X                   // [2   wp]   3-state BKGD pin
                                 // ------- 3
      BRN   *+2                  // [3     ]
      LDA   ,X                   // [2   rf]   Sample BDM_IN
                                 // ------- 5
                                 // [1+   p]
      PSHA                       // [2     ]   Save sample on stack
      LDA   #RxBDM_3_STATE       // [2     ]   Restore A value
      DBNZ  bitCount,Loop        // [7     ]

      /* bit 0 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDX   ,X                   // Last sample in X

      CLI
      // now get the bit values (last value is in X, previous 7 on stack)
      JMP   rxStackDecode
   }
}
#pragma MESSAGE DEFAULT C20001 // Restore warnings about different stack ptr
                       
#else

//=======================================================================
// 3,5,8
U8 bdm_rx1(void) {
#pragma NO_RETURN
   asm {
      SEI
      LDHX  @DATA_PORT           // Point HX at DATA_PORT

      LDA   #RxBDM_3_STATE       // Writing A to DATA_PORT 3-states BDM
      STA   ,X                   // 3-state BKGD pin

      /* bit 7 (MSB) */
      MOV   #RxBDM_LOW,DATA_PORT // [3  pwp]   Drive BKGD low
                                 // ------- 8
                                 // [+1   p]
      STA   ,X                   // [2   wp]   3-state BKGD pin
                                 // ------- 3
      BRN   *+2                  // [3     ]
      LDA   ,X                   // [2   rf]   Sample BDM_IN
                                 // ------- 5
                                 // [1+   p]
      PSHA                       // [2     ]   Save sample on stack
      LDA   #RxBDM_3_STATE       // [2     ]   Restore A value

      /* bit 6 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 5 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 4 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 3 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 2 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 1 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 0 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      LDX   ,X

      CLI
      // now get the bit values (last value is in X, previous 7 on stack)
      JMP   rxStackDecode
   }
}
#endif

#if (CAPABILITY&CAP_CFVx)  // CF Version in JM16 pressed for space - Smaller code
//=======================================================================
// 3,6,15
#pragma MESSAGE DISABLE C20001 // Disable warnings about different stack ptr
U8 bdm_rx2(void) {
#pragma NO_RETURN
   asm {
      SEI
      LDHX  @DATA_PORT           // Point HX at DATA_PORT
      MOV   #7,bitCount          // [3  pwp]   Drive BKGD low

      LDA   #RxBDM_3_STATE       // Writing A to DATA_PORT 3-states BKGD
      STA   ,X                   // 3-state BKGD pin

   Loop:
      /* bit 7..1 */
      MOV   #RxBDM_LOW,DATA_PORT // [3  pwp]   Drive BKGD low
                                 // ------- 15
                                 // [1+   p]
      STA   ,X                   // [2   wp]   3-state BKGD pin
                                 // ------- 3
      BRN   *+2                  // [3     ]
      NOP                        // [1     ]
      LDA   ,X                   // [2   rf]   Sample BDM_IN
                                 // ------- 6
                                 // [1+   p]
      PSHA                       // [2     ]   Save sample on stack
      LDA   #RxBDM_3_STATE       // [2     ]   Restore A value
      DBNZ  bitCount,Loop        // [7     ]

      /* bit 0 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDX   ,X                   // Last sample in X

      CLI
      // now get the bit values (last value is in X, previous 7 on stack)
      JMP   rxStackDecode
   }
}
#pragma MESSAGE DEFAULT C20001 // Restore warnings about different stack ptr
#else

//=======================================================================
// 3,6,8
U8 bdm_rx2(void) {
#pragma NO_RETURN
   asm {
      SEI
      LDHX  @DATA_PORT           // Point HX at DATA_PORT

      LDA   #RxBDM_3_STATE       // Writing A to DATA_PORT 3-states BKGD
      STA   ,X                   // 3-state BKGD pin

      /* bit 7 (MSB) */
      MOV   #RxBDM_LOW,DATA_PORT // [3  pwp]   Drive BKGD low
                                 // ------- 8
                                 // [1+   p]
      STA   ,X                   // [2   wp]   3-state BKGD pin
                                 // ------- 3
      BRN   *+2                  // [3     ]
      NOP                        // [1     ]
      LDA   ,X                   // [2   rf]   Sample BDM_IN
                                 // ------- 6
                                 // [1+   p]
      PSHA                       // [2     ]   Save sample on stack
      LDA   #RxBDM_3_STATE       // [2     ]   Restore A value

      /* bit 6 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 5 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 4 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 3 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 2 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 1 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDA   ,X
      PSHA
      LDA   #RxBDM_3_STATE

      /* bit 0 */
      MOV   #RxBDM_LOW,DATA_PORT
      STA   ,X
      BRN   *+2
      NOP
      LDX   ,X

      CLI
      // now get the bit values (last value is in X, previous 7 on stack)
      JMP   rxStackDecode
   }
}
#endif

//=======================================================================
// 4,6,10
U8 bdm_rx3(void) {
   asm {
      SEI
      LDA   #0x01                    // Value used as sentinel

   Loop:
      MOV   #RxBDM_LOW,DATA_PORT     // [4  pwpp]   Drive BKGD low
                                     // --- 10
      MOV   #RxBDM_3_STATE,DATA_PORT // [4  pwpp]   3-state BKGD pin
                                     // ---  4
      BRN   *+2                      // [3     ]
      BRSET BDM_IN_BIT,DATA_PORT,*+3 // [3   rpp]   Sample BDM_IN
                                     // ---  6
                                     // [2+   pp]
      ROLA                           // [1      ]   Save sample
      BCC   Loop                     // [3      ]
      CLI
   }
}

//=======================================================================
// 5,6,10
U8 bdm_rx4(void) {
   asm {
      SEI
      LDA   #0x01                    // Value used as sentinel

   Loop:
      MOV   #RxBDM_LOW,DATA_PORT     // [4  pwpp]   Drive BKGD low
                                     // --- 10
      NOP                            // [1     ]
      MOV   #RxBDM_3_STATE,DATA_PORT // [4  pwpp]   3-state BKGD pin
                                     // ---  5
      BRN   *+2                      // [3     ]
      BRSET BDM_IN_BIT,DATA_PORT,*+3 // [3   rpp]   Sample BDM_IN
                                     // ---  6
                                     // [2+   pp]
      ROLA                           // [1      ]   Save sample
      BCC   Loop                     // [3      ]
      CLI
   }
}

//=======================================================================
// 5,8,10
U8 bdm_rx5(void) {
   asm {
      SEI
      LDA   #0x01                    // Value used as sentinel

   Loop:
      MOV   #RxBDM_LOW,DATA_PORT     // [4  pwpp]   Drive BKGD low
                                     // --- 10
      NOP                            // [1     ]
      MOV   #RxBDM_3_STATE,DATA_PORT // [4  pwpp]   3-state BKGD pin
                                     // ---  5
      BRN   *+2                      // [3     ]
      NOP                            // [1     ]
      NOP                            // [1     ]
      BRSET BDM_IN_BIT,DATA_PORT,*+3 // [3   rpp]   Sample BDM_IN
                                     // ---  8
                                     // [2+   pp]
      ROLA                           // [1      ]   Save sample
      BCC   Loop                     // [3      ]
      CLI
   }
}

//=======================================================================
// 6,9,10
U8 bdm_rx6(void) {
   asm {
      SEI
      LDA   #0x01                    // Value used as sentinel

   Loop:
      MOV   #RxBDM_LOW,DATA_PORT     // [4  pwpp]   Drive BKGD low
                                     // --- 10
      NOP                            // [1     ]
      NOP                            // [1     ]
      MOV   #RxBDM_3_STATE,DATA_PORT // [4  pwpp]   3-state BKGD pin
                                     // ---  6
      BRN   *+2                      // [3     ]
      BRN   *+2                      // [3     ]
      BRSET BDM_IN_BIT,DATA_PORT,*+3 // [3   rpp]   Sample BDM_IN
                                     // ---  9
                                     // [2+   pp]
      ROLA                           // [1      ]   Save sample
      BCC   Loop                     // [3      ]
      CLI
   }
}

//=======================================================================
// >8,>10,>20
//! Generic BDM Rx routine - used for a range of speeds
//!
U8 bdm_rxGeneric(void) {
   asm {
      SEI
      LDA   #0x01                    // Value used as sentinel

   Loop:
      LDX   rxTiming1                // [3      ]
      MOV   #RxBDM_LOW,DATA_PORT     // [4  pwpp]   Drive BKGD low
                                     // ---  16+4n
      DBNZX *+0                      // [4n     ]
      MOV   #RxBDM_3_STATE,DATA_PORT // [4  pwpp]   3-state BKGD pin
                                     // ---  4+4n
      LDX   rxTiming2                // [3      ]
      DBNZX *+0                      // [4n     ]
      BRSET BDM_IN_BIT,DATA_PORT,*+3 // [3   rpp]   Sample BDM_IN
                                     // ---  6+4n
                                     // [2+   pp]
      LDX   rxTiming3                // [3      ]
      DBNZX *+0                      // [4n     ]
      ROLA                           // [1      ]   Save sample
      BCC   Loop                     // [3      ]
      CLI
   }
}

#pragma MESSAGE DEFAULT C1404  // Restore warnings about no return statement

//===============================================================================
//  When the SYNC length expressed in 60MHz ticks is ABOVE OR EQUAL to the value
//  in the table, the corresponding pointer is selected
//  If SYNC is shorter than the second entry, the target runs too fast
//  If SYNC is longer or equal to the last entry, the target runs too slow
//
//! Structure describing Tx configuration
typedef struct {
   U16   syncThreshold;       //!< Threshold to use this function
   void  (*txFunc)(U8 data);  //!< Ptr to selected function
   U8    time1,time2,time3;   //!< Timing Parameters for function use
} TxConfiguration;

//! Information for each Tx configuration
//!
const TxConfiguration txConfiguration[] =
{
{ 0,     bdm_txEmpty,   0, 0, 0 },        //>56 MHz - Max Fequency
{ 137,   bdm_tx1,       1, 0, 0 },        //44 - 56 MHz,       (2,4,6)
{ 167,   bdm_tx2,       2, 0, 0 },        //37.71 - 48 MHz,    (3,4,6)
{ 193,   bdm_tx3,       3, 0, 0 },        //33 - 42 MHz,       (4,4,6)
{ 219,   bdm_tx4,       4, 0, 0 },        //29.33 - 37.33 MHz, (4,5,6)
{ 245,   bdm_tx5,       5, 0, 0 },        //26.4 - 33.6 MHz,   (5,5,6)
{ 270,   bdm_tx6,       6, 0, 0 },        //24 - 30.55 MHz,    (5,6,6)
{ 308,   bdm_tx7,       7, 0, 0 },        //20.31 - 25.85 MHz, (6,7,7)
{ 360,   bdm_tx8,       8, 0, 0 },        //17.6 - 22.4 MHz,   (7,8,7)
{ 412,   bdm_tx9,       9, 0, 0 },        //15.53 - 19.76 MHz, (8,9,8)
{ 463,   bdm_tx10,      10, 0, 0 },       //13.89 - 17.68 MHz, (9,10,9)
{ 527,   bdm_txGeneric, 1, 1, 1 },        //12 - 15.27 MHz,    (10,12,18)
{ 617,   bdm_txGeneric, 2, 1, 1 },        //10.15 - 12.92 MHz, (14,12,18)
{ 720,   bdm_txGeneric, 2, 2, 1 },        //8.8 - 11.2 MHz,    (14,16,18)
{ 824,   bdm_txGeneric, 3, 2, 1 },        //7.76 - 9.88 MHz,   (18,16,18)
{ 927,   bdm_txGeneric, 3, 3, 1 },        //6.95 - 8.84 MHz,   (18,20,18)
{ 1030,  bdm_txGeneric, 4, 3, 2 },        //6.29 - 8 MHz,      (22,20,22)
{ 1181,  bdm_txGeneric, 4, 5, 3 },        //5.28 - 6.72 MHz,   (22,28,26)
{ 1389,  bdm_txGeneric, 5, 6, 4 },        //4.55 - 5.79 MHz,   (26,32,30)
{ 1596,  bdm_txGeneric, 6, 7, 4 },        //4 - 5.09 MHz,      (30,36,30)
{ 1850,  bdm_txGeneric, 7, 9, 6 },        //3.38 - 4.31 MHz,   (34,44,38)
{ 2163,  bdm_txGeneric, 9, 10, 7 },       //2.93 - 3.73 MHz,   (42,48,42)
{ 2520,  bdm_txGeneric, 11, 12, 9 },      //2.49 - 3.17 MHz,   (50,56,50)
{ 2979,  bdm_txGeneric, 12, 16, 11 },     //2.1 - 2.67 MHz,    (54,72,58)
{ 3495,  bdm_txGeneric, 14, 19, 14 },     //1.81 - 2.3 MHz,    (62,84,70)
{ 4057,  bdm_txGeneric, 17, 22, 16 },     //1.55 - 1.98 MHz,   (74,96,78)
{ 4731,  bdm_txGeneric, 22, 24, 19 },     //1.33 - 1.7 MHz,    (94,104,90)
{ 5550,  bdm_txGeneric, 26, 29, 24 },     //1.13 - 1.44 MHz,   (110,124,110)
{ 6514,  bdm_txGeneric, 31, 34, 29 },     //0.96 - 1.23 MHz,   (130,144,130)
{ 7686,  bdm_txGeneric, 37, 40, 34 },     //0.82 - 1.04 MHz,   (154,168,150)
{ 8990,  bdm_txGeneric, 44, 47, 40 },     //0.7 - 0.89 MHz,    (182,196,174)
{ 10529, bdm_txGeneric, 52, 55, 47 },     //0.6 - 0.76 MHz,    (214,228,202)
{ 12298, bdm_txGeneric, 61, 65, 56 },     //0.51 - 0.65 MHz,   (250,268,238)
{ 14501, bdm_txGeneric, 71, 78, 67 },     //0.43 - 0.55 MHz,   (290,320,282)
{ 17084, bdm_txGeneric, 84, 93, 79 },     //0.37 - 0.47 MHz,   (342,380,330)
{ 19718, bdm_txGeneric, 98, 105, 92 },    //0.32 - 0.41 MHz,   (398,428,382)
{ 22627, bdm_txGeneric, 113, 120, 105 },  //0.28 - 0.36 MHz,   (458,488,434)
{ 26068, bdm_txGeneric, 128, 140, 120 },  //0.24 - 0.31 MHz,   (518,568,494)
{ 30170, bdm_txGeneric, 146, 166, 140 },  //0.21 - 0.27 MHz,   (590,672,574)
{ 36571, bdm_txEmpty,   0, 0 ,0  },       //<0.21 MHz - Min. Frequency
};

//! Structure describing Rx configuration
typedef struct {
   U16   syncThreshold;       //!< Threshold to use this function
   U8    (*txFunc)(void);     //!< Ptr to selected function
   U8    time1,time2,time3;   //!< Timing Parameters for function use
} RxConfiguration;

//! Information for each Rx configuration
//!
const RxConfiguration rxConfiguration[] =
{
{ 0, bdm_rxEmpty, 0, 0, 0 },//>56 MHz - Max Fequency
{ 137, bdm_rx1, 1, 0, 0 },//48.56 - 56 MHz, (3,5,8)
{ 152, bdm_rx2, 2, 0, 0 },//39.65 - 52.86 MHz, (3,6,8)
{ 188, bdm_rx3, 3, 0, 0 },//33.5 - 42 MHz, (4,6,10)
{ 229, bdm_rx4, 4, 0, 0 },//29 - 33.6 MHz, (5,6,10)
{ 258, bdm_rx5, 5, 0, 0 },//22.86 - 30.48 MHz, (5,8,10)
{ 320, bdm_rx6, 6, 0, 0 },//18.87 - 25.16 MHz, (6,9,10)
{ 396, bdm_rxGeneric, 1, 1, 1 },//14.95 - 19.93 MHz, (8,10,20)
{ 503, bdm_rxGeneric, 1, 2, 1 },//11.71 - 15.61 MHz, (8,14,20)
{ 627, bdm_rxGeneric, 1, 3, 1 },//9.62 - 12.83 MHz, (8,18,20)
{ 750, bdm_rxGeneric, 2, 3, 1 },//8.17 - 10.89 MHz, (12,18,20)
{ 874, bdm_rxGeneric, 2, 4, 2 },//7.09 - 9.46 MHz, (12,22,24)
{ 998, bdm_rxGeneric, 3, 4, 2 },//6.27 - 8.36 MHz, (16,22,24)
{ 1121, bdm_rxGeneric, 3, 5, 3 },//5.62 - 7.49 MHz, (16,26,28)
{ 1301, bdm_rxGeneric, 5, 5, 5 },//4.65 - 6.2 MHz, (24,26,36)
{ 1548, bdm_rxGeneric, 5, 7, 6 },//3.97 - 5.29 MHz, (24,34,40)
{ 1852, bdm_rxGeneric, 6, 9, 9 },//3.25 - 4.33 MHz, (28,42,52)
{ 2224, bdm_rxGeneric, 8, 10, 11 },//2.75 - 3.67 MHz, (36,46,60)
{ 2652, bdm_rxGeneric, 10, 12, 14 },//2.29 - 3.05 MHz, (44,54,72)
{ 3197, bdm_rxGeneric, 12, 15, 18 },//1.89 - 2.52 MHz, (52,66,88)
{ 3873, bdm_rxGeneric, 15, 18, 23 },//1.56 - 2.08 MHz, (64,78,108)
{ 4675, bdm_rxGeneric, 18, 22, 28 },//1.3 - 1.73 MHz, (76,94,128)
{ 5594, bdm_rxGeneric, 22, 26, 34 },//1.09 - 1.45 MHz, (92,110,152)
{ 6687, bdm_rxGeneric, 27, 31, 42 },//0.91 - 1.21 MHz, (112,130,184)
{ 8011, bdm_rxGeneric, 32, 38, 51 },//0.75 - 1.01 MHz, (132,158,220)
{ 9734, bdm_rxGeneric, 38, 47, 63 },//0.62 - 0.83 MHz, (156,194,268)
{ 11742, bdm_rxGeneric, 45, 58, 77 },//0.52 - 0.69 MHz, (184,238,324)
{ 13984, bdm_rxGeneric, 53, 70, 93 },//0.43 - 0.58 MHz, (216,286,388)
{ 16905, bdm_rxGeneric, 61, 88, 115 },//0.36 - 0.48 MHz, (248,358,476)
{ 20239, bdm_rxGeneric, 72, 108, 140 },//0.3 - 0.4 MHz, (292,438,576)
{ 24409, bdm_rxGeneric, 84, 130, 165 },//0.25 - 0.33 MHz, (340,526,676)
{ 29028, bdm_rxGeneric, 100, 160, 200 },//0.21 - 0.28 MHz, (404,646,816)
{ 36571, bdm_rxEmpty, 0, 0 ,0  },//<0.21 MHz - Min. Frequency
};

//! Selects Rx and Tx routine to be used according to SYNC length in \ref cable_status structure.
//! Sets up the soft wait delays
//!
//! @return
//!   \ref BDM_RC_OK             => Success \n
//!   \ref BDM_RC_NO_TX_ROUTINE  => No suitable Tx routine found \n
//!   \ref BDM_RC_NO_RX_ROUTINE  => No suitable Rx routine found \n
//!
U8 bdm_RxTxSelect(void) {
   const TxConfiguration  * far txConfigPtr;
   const RxConfiguration  * far rxConfigPtr;

   bdm_rx_ptr = bdm_rxEmpty; // clear the Tx/Rx pointers
   bdm_tx_ptr = bdm_txEmpty; // i.e. no routines found

   for (  txConfigPtr  = txConfiguration+sizeof(txConfiguration)/sizeof(txConfiguration[0]);
        --txConfigPtr >= txConfiguration; ) { // Search the table

      if (cable_status.sync_length >= txConfigPtr->syncThreshold) { // SYNC is >=
         bdm_tx_ptr    = txConfigPtr->txFunc; // Select this routine
         txTiming1     = txConfigPtr->time1;  // Save timing parameters
         txTiming2     = txConfigPtr->time2;
         txTiming3     = txConfigPtr->time3;
         break;                               // Quit search
      }
   }
   if (bdm_tx_ptr==bdm_txEmpty) // Return if no function found
      return(BDM_RC_NO_TX_ROUTINE);

   for (  rxConfigPtr  = rxConfiguration+sizeof(rxConfiguration)/sizeof(rxConfiguration[0]);
        --rxConfigPtr >= rxConfiguration; ) { // Search the table

      if (cable_status.sync_length >= rxConfigPtr->syncThreshold) { // SYNC is >=
         bdm_rx_ptr    = rxConfigPtr->txFunc; // Select this routine
         rxTiming1     = rxConfigPtr->time1;  // Save timing parameters
         rxTiming2     = rxConfigPtr->time2;
         rxTiming3     = rxConfigPtr->time3;
         break;                               // Quit search
      }
   }
   if (bdm_rx_ptr==bdm_rxEmpty) // Return if no function found
      return(BDM_RC_NO_RX_ROUTINE);

   // Calculate number of iterations for manual delay (each iteration is 8 cycles)
   cable_status.wait64_cnt  = cable_status.sync_length/(U16)(((8*60*128UL)/(BUS_FREQ/1000000)/64));
   cable_status.wait150_cnt = cable_status.sync_length/(U16)(((8*60*128UL)/(BUS_FREQ/1000000)/150));
   // Correct for overhead in calling function etc. (JSR+RTS+JSR) = (5+4+5) ~ 2 iterations
   if (cable_status.wait64_cnt<=2)
      cable_status.wait64_cnt = 1; // minimum of 1 iteration
   else
      cable_status.wait64_cnt -= 2;
   if (cable_status.wait150_cnt<=2)
      cable_status.wait150_cnt = 1; // minimum of 1 iteration
   else
      cable_status.wait150_cnt -= 2;
   return(0);
}
#endif

//! Confirm communication at given Sync value.
//! Only works on HC12 (and maybe only 1 of 'em!)
//!
//! @return
//!   == \ref BDM_RC_OK  => Success \n
//!   != \ref BDM_RC_OK  => Various errors
//!
#pragma MESSAGE DISABLE C4001 // Disable warnings about Condition always true
U8 bdmHC12_confirmSpeed(U16 syncValue) {
U8 rc;

   cable_status.sync_length = syncValue;

#if TARGET_HARDWARE!=H_VERSALOON
   rc = bdm_RxTxSelect(); // Drivers available for this frequency?
   if (rc != BDM_RC_OK)
      goto tidyUp;
#endif

   rc = BDM_RC_BDM_EN_FAILED; // Assume probing failed

   do {
      U16 probe;
      // This method works for secured or unsecured devices
      // in special mode
      
      // Set FDATA to 0xAA55 & read back
      BDM12_CMD_WRITEW(0x10A,0xAA55);
      BDM12_CMD_READW(0x10A,&probe);

      // Read back correctly?
      if (probe != 0xAA55)
         break;

      // Set location to 0x55AA & read back
      BDM12_CMD_WRITEW(0x10A,0x55AA);
      BDM12_CMD_READW(0x10A,&probe);

      // Read back correctly?
      if (probe != 0x55AA)
         break;

      return BDM_RC_OK; // Success!

   } while (0);

   do {
      U8 probe;
      U8 originalValue;
      // This method works for unsecured devices
      // in special or non-special modes

      // Get current BDMCCR
      BDM12_CMD_BDREADB(HC12_BDMCCR,&originalValue);

      // Set location to 0xAA & read back
      BDM12_CMD_BDWRITEB(HC12_BDMCCR,0xAA);
      BDM12_CMD_BDREADB(HC12_BDMCCR,&probe);

      // Read back correctly?
      if (probe != 0xAA)
         break;

      // set location to 0x55 & read back
      BDM12_CMD_BDWRITEB(HC12_BDMCCR,0x55);
      BDM12_CMD_BDREADB(HC12_BDMCCR,&probe);

      // Read back correctly?
      if (probe != 0x55)
         break;

      // Restore BDMCCR
      BDM12_CMD_BDWRITEB(HC12_BDMCCR,originalValue);
      return BDM_RC_OK; // Success!
      
   } while (0);
   
#if 0
   do {
      
   // Get current BDMSTS
   BDM12_CMD_BDREADB(HC12_BDMSTS,&originalValue);

   // Try to clear BDMSTS.ENBDM
   BDM12_CMD_BDWRITEB(HC12_BDMSTS,originalValue&~HC12_BDMSTS_ENBDM);
   BDM12_CMD_BDREADB(HC12_BDMSTS,&probe);

   if ((probe & HC12_BDMSTS_ENBDM) != 0) // Not clear now? - Try next speed
      goto tidyUp;

   // Try to set BDMSTS.ENBDM
   BDM12_CMD_BDWRITEB(HC12_BDMSTS,originalValue|HC12_BDMSTS_ENBDM);
   BDM12_CMD_BDREADB(HC12_BDMSTS,&probe);

   if ((probe & HC12_BDMSTS_ENBDM) == 0) // Not set now? - Try next speed
      goto tidyUp;

   return BDM_RC_OK; // Success!
      
   } while false;
#endif

tidyUp:
   cable_status.sync_length  = 1;
   cable_status.speed        = SPEED_NO_INFO; // Connection cannot be established at this speed
   cable_status.ackn         = WAIT;    // Clear indication of ACKN feature
   return rc;
}
#pragma MESSAGE DEFAULT C4001 // Disable warnings about Condition always true

#if TARGET_HARDWARE!=H_VERSALOON
//! Attempt to determine target speed by trial and error
//!
//! Basic process used to check for communication is:
//!   -  Attempt to modify the BDM Status register [BDMSTS] or BDM CCR Save Register [BDMCCR]
//!
//! The above is attempted for a range of 'nice' frequencies and then every Tx driver frequency. \n
//! To improve performance the last two successful frequencies are remembered.  This covers the \n
//! common case of alternating between two frequencies [reset & clock configured] with a minimum \n
//! number of probes.
//!
static U8 bdmHC12_alt_speed_detect(void) {
static const U16 typicalSpeeds[] = { // Table of 'nice' BDM speeds to try
   SYNC_MULTIPLE( 4000000UL),  //  4 MHz
   SYNC_MULTIPLE( 8000000UL),  //  8 MHz
   SYNC_MULTIPLE(16000000UL),  // 16 MHz
   SYNC_MULTIPLE(32000000UL),  // 32 MHz
   SYNC_MULTIPLE(24000000UL),  // 24 MHz
   SYNC_MULTIPLE(48000000UL),  // 48 MHz
   SYNC_MULTIPLE(20000000UL),  // 20 MHz
   SYNC_MULTIPLE( 2000000UL),  //  2 MHz
   SYNC_MULTIPLE(10000000UL),  // 10 MHz
   SYNC_MULTIPLE( 1000000UL),  //  1 MHz
   SYNC_MULTIPLE(  500000UL),  // 500kHz
   0
   };
#pragma DATA_SEG __SHORT_SEG Z_PAGE
static U16 lastGuess1 = SYNC_MULTIPLE(8000000UL);  // Used to remember last 2 guesses
static U16 lastGuess2 = SYNC_MULTIPLE(16000000UL); // Common situation to change between 2 speeds (reset,running)
#pragma DATA_SEG DEFAULT
const TxConfiguration  * far txConfigPtr;
int sub;
U16 currentGuess;
U8  rc;

   // Try last used speed #1
   if (bdmHC12_confirmSpeed(lastGuess1) == BDM_RC_OK) {
      cable_status.speed = SPEED_GUESSED;  // Speed found by trial and error
      return BDM_RC_OK;
   }

   // Try last used speed #2
   currentGuess = lastGuess2;
   rc = bdmHC12_confirmSpeed(lastGuess2);

   // Try some likely numbers!
   for (sub=0; typicalSpeeds[sub]>0; sub++) {
      if (rc == BDM_RC_OK)
         break;
      currentGuess = typicalSpeeds[sub];
      rc           = bdmHC12_confirmSpeed(currentGuess);
      }

   // Try each Tx driver BDM frequency
   for (  txConfigPtr  = txConfiguration+(sizeof(txConfiguration)/sizeof(txConfiguration[0])-1);
        --txConfigPtr >= txConfiguration; ) { // Search the table
      if (rc == BDM_RC_OK)
         break;
      currentGuess = (txConfigPtr->syncThreshold+(txConfigPtr+1)->syncThreshold)/2;
      rc           = bdmHC12_confirmSpeed(currentGuess);
      }


   if (rc == BDM_RC_OK) {
      // Update speed cache (LRU)
      lastGuess2       = lastGuess1;
      lastGuess1       = currentGuess;
      cable_status.speed = SPEED_GUESSED;  // Speed found by trial and error
      return BDM_RC_OK;
   }

   cable_status.speed = SPEED_NO_INFO;  // Speed not found
   return rc;
}

#if (DEBUG&DEBUG_COMMANDS) // Debug commands enabled

//! Used to check the phase relationship between port reads and writes.
//!
//!  The higher speed BDM communication routines have very critical timing.  This routine
//!  allowed the timing delays between instructions accessing the ports and port pin changes to be measured.
//!  More precisely, the difference in delays between port reads and writes was measured.
//!  This may vary a bit with a particular chip but it's better than ignoring it.
//!
//! @warning This function will hang the BDM
//!
void bdm_checkTiming(void) {
#if ((TARGET_HARDWARE==H_USBDM) && (CAPABILITY&CAP_RESET)) // Requires RESET driver!!
//! ToDO - Broken in USBDM_CF!!!!

   DIR_PORT_DDR  = 0;     // BKGD direction controlled by DATA_PORT
   DATA_PORT_DDR = BDM_OUT_MASK|BDM_DIR_Rx_MASK;     // BKGD pin is driven, Reset pin is driven
   RESET_OUT_DDR = 1;

   asm {
      LDHX   @DATA_PORT                                // Point X at DATA_PORT

   Loop1:
      LDA   #BDM_OUT_MASK|BDM_DIR_Rx_WR                // [2      ] Mask for BDM high
      BRN   *+0                                        // [3      ] Waste some time
   Loop:
      BRN   *+0                                        // [3      ] Waste some time
      BRN   *+0                                        // [3      ]
      BRN   *+0                                        // [3      ]
      BRN   *+0                                        // [3      ]

      ASM_RESET_LOW
      CLR   ,X                                         // [4  rfwp] Drive BKGD low
                                                       // ---------
      STA   ,X                                         // [2    wp] Drive BKGD high
                                                       // --------- 2 cycles low
      BSET  BDM_DIR_Rx_BIT,DATA_PORT                   // [4  rfwp] 3-state BKGD pin
                                                       // [1     p]
      BRN   *+0                                        // [3      ]
      LDA   ,X                                         // [2    rf] Sample BDM_IN
                                                       // --------- 12 cycles from BKGD low
                                                       // [1     p]
      AND   #BDM_IN_MASK                               // [2      ] Input high?
      ASM_RESET_HIGH
      BEQ   Loop1                                      // [3      ] No - set RESET low

      LDA   #BDM_OUT_MASK|BDM_DIR_Rx_WR                // [2      ] Mask for BDM high, RESET High
      BRA   Loop                                       // [3      ]
      }
#endif// (CAPABILITY&CAP_RESET)
}

#endif // (DEBUG&DEBUG_COMMANDS) // Debug commands enabled

#endif
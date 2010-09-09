/*! \file
    \brief USBDM - Main command procedure for executing BDM commands.

   This file processes the commands received over the USB link from the host

   \verbatim
   This software was modified from \e TBLCF software
   This software was modified from \e TBDML software

   USBDM
   Copyright (C) 2007  Peter O'Donoghue

   Turbo BDM Light
   Copyright (C) 2005  Daniel Malik

   Turbo BDM Light ColdFire
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
   +=======================================================================================
   | 10 May 2010 | Many changes in Vpp/RS08 code (deleted!)                           - pgo
   |  1 May 2010 | Action option changes immediately in f_CMD_SET_OPTIONS()           - pgo
   |    Oct 2009 | Extended status information available                              - pgo
   |    Sep 2009 | Major changes for V2                                               - pgo
   -=======================================================================================
   |  3 Feb 2009 | Extended bdm_setInterfaceLeve[]                                    - pgo
   | 20 Jan 2009 | Merged HCS/CFV1 code from USBDM                                    - pgo
   | 13 Jan 2009 | Moved CFVx commands to separate file                               - pgo
   | 12 Jan 2009 | Changed to dispatch table & individual functions                   - pgo
   |  1 Jan 2009 | Ported to JMxx from JB16 TBLCF code                                - pgo
   +=======================================================================================
   \endverbatim
*/
#include "app_cfg.h"
#include "Derivative.h"
#include "Common.h"
#include "Configure.h"
#include "Commands.h"
#include "bdmCommon.h"
#include "BDM_CF.h"
#include "BDM_RS08.h"
#include "BDM.h"
#if (TARGET_HARDWARE!=H_VERSALOON)
#include "usb.h"
#endif
#include "icp.h"
#include "CmdProcessing.h"
#include "CmdProcessingHCS.h"
#include "CmdProcessingCFVx.h"
#include "CmdProcessingCFV1.h"

#ifdef __HC08__
#pragma DATA_SEG __SHORT_SEG Z_PAGE
#endif // __HC08__
//! Status of the BDM
//!
//! see \ref CableStatus_t
CableStatus_t cable_status = { T_OFF };

//! Options for the BDM
//!
//! see \ref BDM_Option_t
BDM_Option_t bdm_option = {
   BDM_TARGET_VDD_OFF,  //!< Target Vdd (off, 3.3V or 5V)
   FALSE,               //!< Cycle target Power when resetting
   FALSE,               //!< Cycle target Power if connection problems (when resetting?)
   FALSE,               //!< Leave target power on when exiting
   TRUE,                //!< Automatically re-connect to target (for speed change)
   TRUE,                //!< Guess speed for target w/o ACKN
   CS_DEFAULT,          //!< Use alternative BDM clock source in target
   FALSE,               //!< Use RESET signal on BDM interface
   {0,0,0,0}            //   Reserved
};
#pragma DATA_SEG DEFAULT

//! Buffer for Rx and Tx of commands & results
U8  commandBuffer[MAX_PACKET_SIZE];

#ifdef __HC08__
#pragma DATA_SEG __SHORT_SEG Z_PAGE
#endif // __HC08__
static U8  commandStatus;      // Error code from last/current command
U8  returnSize;                // Size of result
#pragma DATA_SEG DEFAULT

//==========================================================================
// Modeless commands
//==========================================================================

//! Creates status byte
//!
//! @return 8-bit status byte \ref StatusBitMasks_t
//!
U16 makeStatusWord(void) {
U16 status = 0;

#if TARGET_HARDWARE==H_USBDM_CF_JMxxCLD
   if  (cable_status.target_type == T_CFVx) { // Coldfire V2, V3 & V4
      if (ALLPST_IS_HIGH)
         status |= S_HALT;          // Target is halted
   }
   else
#endif //TARGET_HARDWARE==H_USBDM_CF_JMxxCLD
   { // HCS12, HCS08, RS08 or CFV1
      if (cable_status.ackn==ACKN)   // Target supports ACKN and the feature is enabled ?
         status |= S_ACKN;

      if (cable_status.speed==SPEED_USER_SUPPLIED) // Do not blame me if it does not work...
         status |= S_USER_DONE;
      else if (cable_status.speed==SPEED_SYNC) // SYNC feature used to detect speed
         status |= S_SYNC_DONE;
      else if (cable_status.speed==SPEED_GUESSED)  // Speed guessed by trial & error
         status |= S_GUESS_DONE;
   }

#if (CAPABILITY&CAP_RESET)
   if (RESET_IS_HIGH)
      status |= S_RESET_STATE;   // The RSTO pin is currently high
   if (cable_status.reset==RESET_DETECTED) {
      status |= S_RESET_DETECT;                    // The target was recently reset externally
      if (RESET_IS_HIGH)
         cable_status.reset = NO_RESET_ACTIVITY;   // Clear the flag if reset pin has returned high
   }
#endif

#if (CAPABILITY&CAP_VDDSENSE)
   switch (cable_status.power) {    // Target has power ?
//    case BDM_TARGET_VDD_NONE : status |= S_POWER_NONE; break;
      case BDM_TARGET_VDD_ERR  : status |= S_POWER_ERR;  break;
      case BDM_TARGET_VDD_INT  : status |= S_POWER_INT;  break;
      case BDM_TARGET_VDD_EXT  : status |= S_POWER_EXT;  break;
   }
#else
   // Assume power present
   status |= S_POWER_EXT;
#endif

#if (CAPABILITY&CAP_FLASH)
   switch (cable_status.flashState) {
//    case BDM_TARGET_VPP_OFF     : status |= S_VPP_OFF;      break;
      case BDM_TARGET_VPP_STANDBY : status |= S_VPP_STANDBY;  break;
      case BDM_TARGET_VPP_ON      : status |= S_VPP_ON;       break;
      case BDM_TARGET_VPP_ERROR   : status |= S_VPP_ERR;      break;
   }
#endif

   return status;
}

//! Dummy routine for unused slots in command table
//!
//! @return
//!    BDM_RC_ILLEGAL_COMMAND => illegal command
//!
U8 f_CMD_ILLEGAL(void) {
   return BDM_RC_ILLEGAL_COMMAND;
}

//! Return status from last command received
//!
//! @return
//!   status from last command
//!
U8 f_CMD_GET_COMMAND_STATUS(void) {
   return commandStatus;
}

//! Various debugging & testing commands
//!
//! @note
//!   commandBuffer           \n
//!    - [1..N] = various commands
//!
//! @return
//!    error code
//!
U8 f_CMD_DEBUG(void) {

DebugSubCommands subCommand = commandBuffer[2];
U8 rc;

   switch ((U8)subCommand) {
      case BDM_DBG_ACKN: // try the ACKN feature
         bdm_acknInit();
         commandBuffer[1] = (U8) makeStatusWord(); // return the status byte
         returnSize = 2;
         return BDM_RC_OK;

      case BDM_DBG_SYNC: // try the sync feature
         rc = bdm_syncMeasure();
         if (rc != BDM_RC_OK)
            return rc;
         commandBuffer[1] = (U8)makeStatusWord(); // return the status byte
         SET_BE_U16(&commandBuffer[2], cable_status.sync_length);
         returnSize = 4;
         return BDM_RC_OK;
#if (DEBUG&DEBUG_COMMANDS)
      case BDM_DBG_TESTPORT: // Check port I/O timing - hangs USB interface
         bdm_checkTiming();
         return BDM_RC_OK;
#endif
#if (CAPABILITY & CAP_FLASH)
      case BDM_DBG_VPP_OFF: // Programming voltage off
         return bdmSetVpp(BDM_TARGET_VPP_OFF );

      case BDM_DBG_VPP_ON:
         // Programming voltage on (requires FLASH12V on first)
         return bdmSetVpp(BDM_TARGET_VPP_ON );

      case BDM_DBG_FLASH12V_OFF: // 12V charge pump off
         return bdmSetVpp(BDM_TARGET_VPP_OFF );

      case BDM_DBG_FLASH12V_ON:  // 12V charge pump on
         return bdmSetVpp(BDM_TARGET_VPP_STANDBY );
#endif // (CAPABILITY & CAP_FLASH)

      case BDM_DBG_VDD_OFF: // Target Vdd voltage off
         VDD_OFF();
         return BDM_RC_OK;

      case BDM_DBG_VDD3_ON: // Target Vdd voltage on
         VDD3_ON();
         return BDM_RC_OK;

      case BDM_DBG_VDD5_ON: // Target Vdd voltage on
         VDD5_ON();
         return BDM_RC_OK;

      case BDM_DBG_CYCLE_POWER: // Cycle power to target
         return bdm_cycleTargetVdd(RESET_SPECIAL);

      case BDM_DBG_MEASURE_VDD: // Measure Target Vdd
         SET_BE_U16(&commandBuffer[1], bdm_targetVddMeasure()); // return the value
         returnSize = 3;
         return BDM_RC_OK;

#if defined(INLINE_ACKN)
      case BDM_DBG_TESTWAITS:
         bdm_checkWaitTiming();
         return BDM_RC_OK;
#endif

      case BDM_DBG_TESTALTSPEED:
         return BDM_RC_OK;

#if (DEBUG&STACK_DEBUG)
      case BDM_DBG_STACKSIZE: // Measure stack size
         {
         extern char  __SEG_START_SSTACK[];     // Bottom of stack space
         extern char  __SEG_END_SSTACK[];       // Top of stack space
         char *stackProbe = __SEG_START_SSTACK; // Probe for stack RAM

         while (*++stackProbe == 0) { // Find 1st used (non-zero) byte on stack
         }
         SET_BE_U16(&commandBuffer[1], (U16) __SEG_END_SSTACK - (U16) stackProbe);
         returnSize = 3;
         return BDM_RC_OK;
         }
#endif // (DEBUG&STACK_DEBUG)
   } // switch
   return BDM_RC_ILLEGAL_PARAMS;

}

//! Set various options
//!
//! @note
//!   commandBuffer\n
//!   - [2..N] = options (image of \ref BDM_Option_t)
//!
//!  @return
//!    BDM_RC_OK => success
//!
U8 f_CMD_SET_OPTIONS(void) {
U8 rc = BDM_RC_OK;

   // Save BDM Options
   (void)memcpy((U8*)&bdm_option, &commandBuffer[2], sizeof(bdm_option));
   if ((U8)cable_status.target_type != T_OFF) {
      // Action options changes if already open
      
#if (CAPABILITY&CAP_VDDCONTROL)
      rc = bdm_setTargetVdd(); // Update Target Vdd
#endif
   }
   return rc;
}

//! Returns capability vector for hardware
//! @return
//!  commandBuffer                                                \n
//!   - [1..2] = BDM capability, see \ref HardwareCapabilities_t
//!
U8 f_CMD_GET_CAPABILITIES(void)
{
   SET_BE_U16(&commandBuffer[1], CAPABILITY);  // Returns 16-bit value
   returnSize = 3;
   return BDM_RC_OK;
}

//! Return status of BDM communication
//!
//! @note
//!   commandBuffer\n
//!   - [1..2] = BDM status
//!
U8 f_CMD_GET_BDM_STATUS(void) {
U16 word;

   word = makeStatusWord();

   commandBuffer[1] = (U8) (word>>8);
   commandBuffer[2] = (U8) word;
   returnSize  = 3;

   return BDM_RC_OK;
}

#if 0
//! Return status of BDM settings
//!
//! @note
//!   commandBuffer\n
//!   - [1..2] = Measured target voltage
//!
U8 f_CMD_GET_SETTINGS(void) {
U16 word;

   word = bdm_targetVddMeasure();

   commandBuffer[1] = (U8) word;
   commandBuffer[2] = (U8) (word>>8);
   returnSize  = 3;

   return BDM_RC_OK;
}
#endif

//=================================================
// Command Dispatch code
//=================================================
typedef U8 (*FunctionPtr)(void);

extern U8 f_CMD_SET_TARGET(void);

static const FunctionPtr commonFunctionPtrs[] =
{
   // Common to all targets
   f_CMD_GET_COMMAND_STATUS         ,//= 0,  CMD_USBDM_GET_COMMAND_STATUS
   f_CMD_SET_TARGET                 ,//= 1,  CMD_USBDM_SET_TARGET
   f_CMD_ILLEGAL                    ,//= 2,  Reserved
#if (DEBUG&DEBUG_COMMANDS)
   f_CMD_DEBUG                      ,//= 3,  CMD_USBDM_DEBUG
#else
   f_CMD_ILLEGAL                    ,//= 3,  CMD_USBDM_DEBUG - disabled
#endif
   f_CMD_GET_BDM_STATUS             ,//= 4,  CMD_USBDM_GET_BDM_STATUS
   f_CMD_GET_CAPABILITIES           ,//= 5,  CMD_USBDM_GET_CAPABILITIES
   f_CMD_SET_OPTIONS                ,//= 6,  CMD_USBDM_SET_OPTIONS
//   f_CMD_GET_SETTINGS               ,//= 7,  CMD_USBDM_GET_SETTINGS
   f_CMD_ILLEGAL                    ,//= 7,  Reserved
   f_CMD_ILLEGAL                    ,//= 8,  Reserved
   f_CMD_ILLEGAL                    ,//= 9,  Reserved
   f_CMD_ILLEGAL                    ,//= 10, Reserved
   f_CMD_ILLEGAL                    ,//= 11, Reserved
   f_CMD_ILLEGAL                    ,//= 12, CMD_USBDM_GET_VER (EP0)
   f_CMD_ILLEGAL                    ,//= 13, Reserved
   f_CMD_ILLEGAL                    ,//= 14, CMD_USBDM_ICP_BOOT (EP0)
};

static const FunctionPtr HCS12functionPtrs[] =
{
   // Target specific versions
   f_CMD_CONNECT                    ,//= 15, CMD_USBDM_CONNECT
   f_CMD_SET_SPEED                  ,//= 16, CMD_USBDM_SET_SPEED
   f_CMD_GET_SPEED                  ,//= 17, CMD_USBDM_GET_SPEED

   f_CMD_CONTROL_INTERFACE          ,//= 18, CMD_USBDM_CONTROL_INTERFACE
   f_CMD_ILLEGAL                    ,//= 19, RESERVED

   f_CMD_READ_STATUS_REG            ,//= 20, CMD_USBDM_READ_STATUS_REG
   f_CMD_WRITE_CONTROL_REG          ,//= 21, CMD_USBDM_WRITE_CONTROL_REG

   f_CMD_RESET                      ,//= 22, CMD_USBDM_TARGET_RESET
   f_CMD_STEP                       ,//= 23, CMD_USBDM_TARGET_STEP
   f_CMD_GO                         ,//= 24, CMD_USBDM_TARGET_GO
   f_CMD_HALT                       ,//= 25, CMD_USBDM_TARGET_HALT

   f_CMD_HCS12_WRITE_REG            ,//= 26, CMD_USBDM_WRITE_REG
   f_CMD_HCS12_READ_REG             ,//= 27, CMD_USBDM_READ_REG

   f_CMD_ILLEGAL                    ,//= 28, CMD_USBDM_WRITE_CREG
   f_CMD_ILLEGAL                    ,//= 29, CMD_USBDM_READ_CREG

   f_CMD_WRITE_BD                   ,//= 30, CMD_USBDM_WRITE_DREG
   f_CMD_READ_BD                    ,//= 31, CMD_USBDM_READ_DREG

   f_CMD_HCS12_WRITE_MEM            ,//= 32, CMD_USBDM_WRITE_MEM
   f_CMD_HCS12_READ_MEM             ,//= 33, CMD_USBDM_READ_MEM
   };

static const FunctionPtr HCS08functionPtrs[] =
{
   // Target specific versions
   f_CMD_CONNECT                    ,//= 15, CMD_USBDM_CONNECT
   f_CMD_SET_SPEED                  ,//= 16, CMD_USBDM_SET_SPEED
   f_CMD_GET_SPEED                  ,//= 17, CMD_USBDM_GET_SPEED

   f_CMD_CONTROL_INTERFACE          ,//= 18, CMD_USBDM_CONTROL_INTERFACE
   f_CMD_ILLEGAL                    ,//= 19, RESERVED

   f_CMD_READ_STATUS_REG            ,//= 20, CMD_USBDM_READ_STATUS_REG
   f_CMD_WRITE_CONTROL_REG          ,//= 21, CMD_USBDM_WRITE_CONTROL_REG

   f_CMD_RESET                      ,//= 22, CMD_USBDM_TARGET_RESET
   f_CMD_STEP                       ,//= 23, CMD_USBDM_TARGET_STEP
   f_CMD_GO                         ,//= 24, CMD_USBDM_TARGET_GO
   f_CMD_HALT                       ,//= 25, CMD_USBDM_TARGET_HALT

   f_CMD_HCS08_WRITE_REG            ,//= 26, CMD_USBDM_WRITE_REG
   f_CMD_HCS08_READ_REG             ,//= 27, CMD_USBDM_READ_REG

   f_CMD_ILLEGAL                    ,//= 28, CMD_USBDM_WRITE_CREG
   f_CMD_ILLEGAL                    ,//= 29, CMD_USBDM_READ_CREG

   f_CMD_WRITE_BKPT                 ,//= 30, CMD_USBDM_WRITE_DREG
   f_CMD_READ_BKPT                  ,//= 31, CMD_USBDM_READ_DREG

   f_CMD_HCS08_WRITE_MEM            ,//= 32, CMD_USBDM_WRITE_MEM
   f_CMD_HCS08_READ_MEM             ,//= 33, CMD_USBDM_READ_MEM

#if ((CAPABILITY & CAP_FLASH) != 0)
   f_CMD_ILLEGAL                    ,//= 34, CMD_USBDM_TRIM_CLOCK - obsolete
   f_CMD_ILLEGAL                    ,//= 35, CMD_USBDM_RS08_FLASH_ENABLE - obsolete
   f_CMD_ILLEGAL                    ,//= 36, CMD_USBDM_RS08_FLASH_STATUS - obsolete
   f_CMD_ILLEGAL                    ,//= 37, CMD_USBDM_RS08_FLASH_DISABLE - obsolete
   f_CMD_ILLEGAL                    ,//= 38, CMD_USBDM_JTAG_GOTORESET
   f_CMD_ILLEGAL                    ,//= 39, CMD_USBDM_JTAG_GOTOSHIFT
   f_CMD_ILLEGAL                    ,//= 40, CMD_USBDM_JTAG_WRITE
   f_CMD_ILLEGAL                    ,//= 41, CMD_USBDM_JTAG_READ
   f_CMD_SET_VPP                    ,//= 42, CMD_USBDM_SET_VPP
#endif
   };

static const FunctionPtr CFV1functionPtrs[] =
{
   // Target specific versions
   f_CMD_CONNECT                    ,//= 15, CMD_USBDM_CONNECT
   f_CMD_SET_SPEED                  ,//= 16, CMD_USBDM_SET_SPEED
   f_CMD_GET_SPEED                  ,//= 17, CMD_USBDM_GET_SPEED

   f_CMD_CONTROL_INTERFACE          ,//= 18, CMD_USBDM_CONTROL_INTERFACE
   f_CMD_ILLEGAL                    ,//= 19, RESERVED

   f_CMD_READ_STATUS_REG            ,//= 20, CMD_USBDM_READ_STATUS_REG
   f_CMD_WRITE_CONTROL_REG          ,//= 21, CMD_USBDM_WRITE_CONTROL_REG

   f_CMD_RESET                      ,//= 22, CMD_USBDM_TARGET_RESET
   f_CMD_STEP                       ,//= 23, CMD_USBDM_TARGET_STEP
   f_CMD_GO                         ,//= 24, CMD_USBDM_TARGET_GO
   f_CMD_HALT                       ,//= 25, CMD_USBDM_TARGET_HALT

   f_CMD_CF_WRITE_REG               ,//= 26, CMD_USBDM_WRITE_REG
   f_CMD_CF_READ_REG                ,//= 27  CMD_USBDM_READ_REG

   f_CMD_CF_WRITE_CREG              ,//= 28  CMD_USBDM_WRITE_CREG
   f_CMD_CF_READ_CREG               ,//= 29  CMD_USBDM_READ_CREG

   f_CMD_CF_WRITE_DREG              ,//= 30  CMD_USBDM_WRITE_DREG
   f_CMD_CF_READ_DREG               ,//= 31  CMD_USBDM_READ_DREG

   f_CMD_CF_WRITE_MEM               ,//= 32  CMD_USBDM_WRITE_MEM
   f_CMD_CF_READ_MEM                ,//= 33  CMD_USBDM_READ_MEM
   };

#if (CAPABILITY&CAP_CFVx)
static const FunctionPtr CFVxfunctionPtrs[] =
{
   // Target specific versions
   f_CMD_CFVx_RESYNC                ,//= 15, CMD_USBDM_CONNECT
   f_CMD_CFVx_SET_SPEED             ,//= 16, CMD_USBDM_SET_SPEED
   f_CMD_ILLEGAL                    ,//= 17, CMD_USBDM_GET_SPEED

   f_CMD_CFVx_CONTROL_INTERFACE     ,//= 18, CMD_USBDM_CONTROL_INTERFACE
   f_CMD_ILLEGAL                    ,//= 19, RESERVED

   f_CMD_CFVx_READ_STATUS_REG       ,//= 20, CMD_USBDM_READ_STATUS_REG
   f_CMD_ILLEGAL                    ,//= 21, CMD_USBDM_WRITE_CONTROL_REG

   f_CMD_CFVx_RESET                 ,//= 22, CMD_USBDM_TARGET_RESET
   f_CMD_CFVx_STEP                  ,//= 23, CMD_USBDM_TARGET_STEP
   f_CMD_CFVx_GO                    ,//= 24, CMD_USBDM_TARGET_GO
   f_CMD_CFVx_HALT                  ,//= 25, CMD_USBDM_TARGET_HALT

   f_CMD_CFVx_WRITE_REG             ,//= 26, CMD_USBDM_WRITE_REG
   f_CMD_CFVx_READ_REG              ,//= 27  CMD_USBDM_READ_REG

   f_CMD_CFVx_WRITE_CREG            ,//= 28  CMD_USBDM_WRITE_CREG
   f_CMD_CFVx_READ_CREG             ,//= 29  CMD_USBDM_READ_CREG

   f_CMD_CFVx_WRITE_DREG            ,//= 30  CMD_USBDM_WRITE_DREG
   f_CMD_CFVx_READ_DREG             ,//= 31  CMD_USBDM_READ_DREG

   f_CMD_CFVx_WRITE_MEM             ,//= 32  CMD_USBDM_WRITE_MEM
   f_CMD_CFVx_READ_MEM              ,//= 33  CMD_USBDM_READ_MEM

   f_CMD_ILLEGAL                    ,//= 34, CMD_USBDM_TRIM_CLOCK
   f_CMD_ILLEGAL                    ,//= 35, CMD_USBDM_RS08_FLASH_ENABLE
   f_CMD_ILLEGAL                    ,//= 36, CMD_USBDM_RS08_FLASH_STATUS
   f_CMD_ILLEGAL                    ,//= 37, CMD_USBDM_RS08_FLASH_DISABLE

   f_CMD_JTAG_GOTORESET             ,//= 38, CMD_USBDM_JTAG_GOTORESET
   f_CMD_JTAG_GOTOSHIFT             ,//= 39, CMD_USBDM_JTAG_GOTOSHIFT
   f_CMD_JTAG_WRITE                 ,//= 40, CMD_USBDM_JTAG_WRITE
   f_CMD_JTAG_READ                  ,//= 41, CMD_USBDM_JTAG_READ
   //f_CMD_JTAG_READ_WRITE            ,//= 42, CMD_USBDM_JTAG_READ
   };
#endif // TARGET_HARDWARE==H_USBDM_CF_JMxxCLD

typedef struct {
   U8 size;
   const FunctionPtr *functions;
} FunctionPtrs;

static const FunctionPtrs Functions[] = {
  {0,                                                NULL},
  {sizeof(HCS12functionPtrs)/sizeof(FunctionPtr),    HCS12functionPtrs},
  {sizeof(HCS08functionPtrs)/sizeof(FunctionPtr),    HCS08functionPtrs},
  {sizeof(CFV1functionPtrs)/sizeof(FunctionPtr),     CFV1functionPtrs},
#if (CAPABILITY&CAP_CFVx)
  {sizeof(CFVxfunctionPtrs)/sizeof(FunctionPtr),     CFVxfunctionPtrs},
#endif // TARGET_HARDWARE==H_USBDM_CF_JMxxCLD
};

//! Ptr to function table for current target type
static const FunctionPtrs *currentFunctions = NULL; // default to empty

//! Set target type
//! Initialise interface for given target
//! @note
//!   commandBuffer        \n
//!   - [2] = target type
//!
U8 f_CMD_SET_TARGET(void) {
U8 target = commandBuffer[2];

   switch (target) {
      case T_HC12:
         currentFunctions = &Functions[1];
         break;
#if ((CAPABILITY & CAP_FLASH) != 0)
      case T_RS08:
#endif
      case T_HCS08:
         currentFunctions = &Functions[2];
         break;
      case T_CFV1:
         currentFunctions = &Functions[3];
         break;
#if (CAPABILITY&CAP_CFVx)
      case T_CFVx:
      case T_JTAG:
         currentFunctions = &Functions[4];
         break;
#endif // TARGET_HARDWARE==H_USBDM_CF_JMxxCLD
      default:
         currentFunctions = &Functions[0];
         break;
   }
   return bdm_setTarget(target);
}

//!  Processes all commands received over USB
//!
//!  The command is expected to be in \ref commandBuffer[1..N]
//!
//!  @return Number of bytes left in commandBuffer to be sent back as response.\n
//!         commandBuffer[0]    = result code, BDM_RC_OK => success, else failure error code\n
//!         commandBuffer[1..N] = command results
//!
U8 commandExec(void) {
BDMCommands command    = commandBuffer[1];  // Command is 1st byte
FunctionPtr commandPtr = f_CMD_ILLEGAL;     // Default to illegal command

#ifdef DEBUG_PIN
   DEBUG_PIN_DDR = 1;
   DEBUG_PIN = 1;
   DEBUG_PIN = 0;
#endif

   // Check if modeless command
   if ((U8)command < sizeof(commonFunctionPtrs)/sizeof(FunctionPtr)) {
      // Modeless command
      commandPtr = commonFunctionPtrs[(U8)command];
   }
   else {
      // Target specific command
      command -= sizeof(commonFunctionPtrs)/sizeof(FunctionPtr);

      if ((currentFunctions!= NULL) && (U8)command < currentFunctions->size) {
         commandPtr = currentFunctions->functions[(U8)command];
      }
   }

   // Execute the command
   // Note: returnSize & commandBuffer may be updated by command
   //       returnSize has a default value of 1
   //       On error, returnSize is forced to 1

   returnSize       = 1;              // Default to returning 1 byte
   commandStatus    = commandPtr();   // Execute command & update command status
   commandBuffer[0] = commandStatus;  // return command status

   if (commandStatus != BDM_RC_OK) {
      returnSize = 1;  // Return a single byte error code
      // Do any common cleanup here
#if TARGET_HARDWARE==H_USBDM_CF_JMxxCLD
      if (cable_status.target_type == T_CFVx) {
         (void)bdmcf_complete_chk_rx(); //  Send at least 2 nops to purge the BDM
         (void)bdmcf_complete_chk_rx(); //  of the offending command
      }
#endif
   }
   return returnSize;
}

#if TARGET_HARDWARE!=H_VERSALOON
#if (VERSION_HW!=(HW_JB+TARGET_HARDWARE))
void commandLoop(void) {
U8 size;

   for(;;) {
      enableInterrupts();
      size = receiveUSBCommand( MAX_PACKET_SIZE, commandBuffer );
      commandBuffer[0] = size;
      size = commandExec();
      sendUSBResponse( size, commandBuffer );
   }
}
#endif
#endif

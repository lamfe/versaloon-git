/*! \file
    \brief Relocated User vector table
    
    The vector table on the HCS08JM processor can be relocated to just
    under the protected region of Flash memory.
    
    This file contains the relocated table.
       
   \verbatim
    JMxx ICP Code
    
    Copyright (C) 2008  Peter O'Donoghue

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
+=======================================================================
|  22 May  2009 | Extended ICP structure with Flash start address - pgo
|  24 Oct  2008 | Added ICP structure & changes (still compat.)   - pgo
|  23 July 2008 | Started coding                                  - pgo
+=======================================================================

\endverbatim
*/
#include "Common.h"
#include "Configure.h"
#include "ICP.h"
#include "Commands.h"
#include "bdmCommon.h"
#include "usb.h"

/*! Location of user vector table in User Flash
**
**  See UserVectors.c
**
*/
#define USER_VECTORTABLE_LOCATION  (FLASH_PROTECT_START-sizeof(userVectorTable)-2)

/*! The following structure may be found through \ref versionOffset so
**  it may be accessed from ICP.
*/
#define ICP_DATA_LOCATION (USER_VECTORTABLE_LOCATION-sizeof(ICP_dataType))

/* The following structure is at a fixed location in the ROM
**  so it may be accessed from ICP.
*/
/*! Various ICP data
**
*/
extern char far __SEG_START__PRESTART[];  // located at bottom of Flash

//! Structure to describe ICP data
const ICP_dataType ICP_data @ICP_DATA_LOCATION =
{
   __SEG_START__PRESTART,     //!< Start of User flash area
   VERSION_HW,                //!< Hardware version
   VERSION_SW,                //!< Software version
   userDetectICP,             //!< Check if User code wants ICP on boot
};

/*! Dummy ISR for unexpected interrupts
**
**  It is good practice to set unused vectors to a dummy routine.
*/
static interrupt void dummyISR(void) {

   asm {
   loop:
      bgnd
      bra   loop
   }
}

/*
** External interrupt routines
*/
extern void _Startup(void); // Low-level C startup entry point

/*! User vector table
**
**  The vector table is relocated to the top of User Flash
**
*/
const vector userVectorTable[30] @USER_VECTORTABLE_LOCATION = {
   _Startup,               // 30. pseudo-Reset - Real reset vector is not relocated.
   dummyISR,               // 29. I2C
   dummyISR,               // 28. RTC
   bdm_targetVddSense,     // 27. ACMP
   dummyISR,               // 26. ADC
   bdm_resetSense,         // 25. KBI
   dummyISR,               // 24. SCI2 Tx
   dummyISR,               // 23. SCI2 Rx
   dummyISR,               // 22. SCI2 Error
   dummyISR,               // 21. SCI1 Tx
   dummyISR,               // 20. SCI1 Rx
   dummyISR,               // 19. SCI1 Error
   dummyISR,               // 18. TPM2 Overflow
   dummyISR,               // 17. TPM2 Ch 1
   dummyISR,               // 16. TPM2 Ch 0
   dummyISR,               // 15. TPM1 Overflow
   bdm_resetSense,         // 10. TPM1 Ch 5 // One of these channels is used!
   bdm_resetSense,         // 10. TPM1 Ch 4
   bdm_resetSense,         // 10. TPM1 Ch 3
   bdm_resetSense,         // 10. TPM1 Ch 2
   bdm_resetSense,         // 10. TPM1 Ch 1
   bdm_resetSense,         //  9. TPM1 Ch 0
   dummyISR,               //  8. Reserved
   USBInterruptHandler,    //  7. USB Status
   dummyISR,               //  6. SPI2
   dummyISR,               //  5. SPI1
   dummyISR,               //  4. MCG loss of lock
   dummyISR,               //  3. Low voltage detect
   dummyISR,               //  2. IRQ
   dummyISR,               //  1. SWI
                           //  0. Used for offset to Version# & Flash checksum
                           //     Real reset vector is not relocated.
};

// This offset allows the ICP information structure to be located by the interface DLL
const U8 versionOffset@(FLASH_PROTECT_START-2) = sizeof(userVectorTable)+2;


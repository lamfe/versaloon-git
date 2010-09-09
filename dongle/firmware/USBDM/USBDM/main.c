/*! \file
    \brief Contains startup code and main loop.
    
    This file contains the main entry point for the program.
    It initialises the system and then remains in the main idle loop.
   
   The basic program flow:
   \li   All BDM activity is driven by commands received from the USB
   \li   All BDM commands are executed from within the ISRs servicing the USB IRQ
              (i.e. with interrupts disabled)
   \li   The only asynchronous activity is:
        \n      * Detection of external resets on RESET_IN pin through KBD interrupts
        \n      * Detection of Target Vdd changes on ?? pin through Timer input capture events
           
    \verbatim
    Open Source BDM/Turbo BMD Light
    
    Original TBDML Copyright (C) 2005  Daniel Malik
     **  Prominent Notice-This software was modified from TBDML software - 12/05  **

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

   Change History

   +====================================================================
   |  1 Jan 2009 | Ported from USBDM                              - pgo
   | 17 Apr 2009 | Removed hardware ICP from user code            - pgo
   | 23 Jul 2008 | Added ICP code                                 - pgo
   | 23 Jul 2008 | Added Vector re-location                       - pgo
   |  3 Mar 2008 | Started changes - lots                         - pgo
   +====================================================================
    \endverbatim
*/

#include <hidef.h> /* for EnableInterrupts macro */
#include "derivative.h" /* include peripheral declarations */
#include "Common.h"
#include "ICP.h"
#include "Configure.h"
#include "Commands.h"
#include "USB.h"
#include "BDM.h"
#include "main.h"
#include "BDMCommon.h"
#include "CmdProcessing.h"

/*! \brief Detect In-Circuit Programming (ICP) mode.
 *
 * ICP mode is indicated by a loopback from BDM to RST pins of the BDM cable.
 * For this to work the interface IC must be powered.
 * This routine is called DIRECTLY from the ICP boot code - minimal C setup (stack only)!
 *
 * @return 0 => ICP not required, \n
 *         1 => ICP required
 */
U8 userDetectICP(void) {

   VDD_OFF(); // Turn off Vdd as early as possible
   return 0;  // ICP not required (rely on bootstrap in ICP code)
}

//==========================================
// Sets up the clock for USB operation
// MGCOUT = 48MHz, BUS_CLOCK = 24MHz, (PEE mode)
//
// Assumes 12 MHz external crystal
//
// Modes: FEI (FLL engaged internal) -> 
//        FBE (FLL bypassed external) ->
//        PBE (PLL bypassed external) ->
//        PEE (PLL engaged external)
//
// Refer 12.5.2.1 of MC9S08JM60 Reference
//
static void init_clock(void)
{
   // Out of reset MCG is in FEI mode
   
   // 1. Switch from FEI (FLL engaged internal) to FBE (FLL bypassed external)
   
   // 1 a) Set up crystal 
   MCGC2 =                       // BDIV = 0, divide by 1
            MCGC2_HGO_MASK       // oscillator in high gain mode
          | MCGC2_EREFS_MASK     // because crystal is being used
          | MCGC2_RANGE_MASK     // 12 MHz is in high freq range
          | MCGC2_ERCLKEN_MASK;  // activate external reference clock
  
   // 1 b) Wait for crystal to start up        
   while (MCGSC_OSCINIT == 0) {
   }
  
   // 1 c) Select clock mode
   MCGC1 =   (2<<6)   // CLKS = 10 -> External reference clock
           | (3<<3);  // RDIV = 3  -> 12MHz/8=1.5 MHz
  
   // 1 d) Wait for mode change
   while (MCGSC_IREFST != 0) {
   }
   
   // 1 e) Wait for MCGOUT indicating that the external reference to be fed to MCGOUT
   while (MCGSC_CLKST != 2) {
   }
   
   // 2. Switch FBE (FLL bypassed external) to PBE (PLL Bypassed External) mode
   
   // 2 b) Set RDIV for correct range
   // 2 c) Enable the PLL & set VDIV value
   MCGC3 = MCGC3_PLLS_MASK
         | (8<<0);        // VDIV=6 -> multiply by 32 -> 1.5MHz * 32 = 48MHz */
   
   // 2 e) Wait until PLLS clock source changes to the PLL
   while(MCGSC_PLLST != 1) {
   }

   // 2 f)  Wait for PLL to acquired lock
   while(MCGSC_LOCK != 1) {
   }
   
   // 3. PBE mode transitions into PEE mode:

   // 3 a) Set RDIV for correct range and select PLL.FLL clock
   MCGC1 =   (0<<6)       // CLKS = 0 -> PLL or FLL output clock
           | (3<<3);      // RDIV = 3 -> 12MHz/8=1.5 MHz (req. 1 < f < 2)

   // 3 c)  Wait for clock stable
   while(MCGSC_CLKST!=3) {
   }

    
  /* Now MCGOUT=48MHz, BUS_CLOCK=24MHz */  
}

#if (DEBUG&STACK_DEBUG)
/*! \brief Clear stack to allows probing to determine the used stack area.
 *
 *  Clear the stack before first use.  This allows later probing to determine how much
 *  stack space has been used.
 */
void clearStack(void) {
extern char far __SEG_START_SSTACK[];  // bottom of stack space

   asm {
      ais   #-2                  // Allows space for value to be pushed
      tsx                        // Current TOS
      sthx  1,sp                 // Save on stack for comparison
      ldhx  #@__SEG_START_SSTACK // Clear from bottom of stack
   loop:    
      clr   ,x                   // Clear next byte
      aix   #1 
      cphx  1,sp                 // Reached TOS?
      bne   loop                 // No - loop
      
      ais   #2                   // Clean up stack
   }
}
#else
#define clearStack() ;
#endif

/*! \brief Initialise the system.
 *
 *  Initialisation of the following:
 *  \li  Default port values
 *  \li  Watchdog (off),
 *  \li  Stack,
 *  \li  BDM interface,
 *  \li  USB interface.
 *  \li  Configure Clock for 48MHz operation
 */
void init(void) {

   // Default ports to inputs
   PTADD = 0x00;
   PTBDD = 0x00;
#if TARGET_HARDWARE!=H_USBDM_JS16CWJ
   PTCDD = 0x00;
   PTDDD = 0x00;
   PTEDD = 0x00;
   PTFDD = 0x00;
   PTGDD = 0x00;
#endif

   // Turn off important things
   VPP_OFF();
   VDD_OFF();
   FLASH12V_OFF();
   
   // Default to Ports have PUPs
   // Note - this doesn't affect outputs
   PTAPE = 0xFF;
   PTBPE = 0xFF;
#if TARGET_HARDWARE!=H_USBDM_JS16CWJ
   PTCPE = 0xFF;
   PTDPE = 0xFF;
   PTEPE = 0xFF;
   PTFPE = 0xFF;
   PTGPE = 0xFF;
#endif

   EnableInterrupts;

#if TARGET_HARDWARE==H_USBDM_JS16CWJ
   SOPT1       = SOPT1_STOPE_MASK|SOPT1_BKGDPE_MASK; // Disable COP, enable STOP instr. & BKGD pin
#else
   SOPT1       = SOPT1_STOPE_MASK; // Disable COP & enable STOP instruction
#endif

#if (CAPABILITY&CAP_VDDSENSE)
   SPMSC1_BGBE = 1;                // Enable Bandgap Reference
#endif
   
   LED_INIT();
   
   init_clock();

   clearStack();

   initUSB();

   (void)bdm_init();
}
void initdebugMessageBuffer(void);

/*! \brief Main loop.
 *
 *  Most of the action is interrupt driven.  
 *
 * @return Never exits

*/
void main(void) {
   EnableInterrupts; /* enable interrupts */
   
   //TERMIO_Init();
   //initdebugMessageBuffer();
   
   //dprint("main():Starting\r\n");
   
   init();
   
   commandLoop(); // doesn't return
   
//   for(;;){
//      dprint("main():Waiting\r\n");
//      wait();
//   }
}

#if DEBUG&DEBUG_MESSAGES
static char debugMessageBuffer[450] = {0};

void initdebugMessageBuffer(void) {
int i;

   for (i=0; i<sizeof(debugMessageBuffer)/sizeof(debugMessageBuffer[0]); i++)
      debugMessageBuffer[i] = 0;   
}

void dputs(char *msg) {
static char *msgPtr = debugMessageBuffer;

   if (msgPtr >= debugMessageBuffer+sizeof(debugMessageBuffer)-30) {
      *msgPtr = '\0';
      msgPtr = debugMessageBuffer;
   }
   while (*msg != '\0')
      *msgPtr++ = *msg++;
  *msgPtr++ = ' ';
  *msgPtr++ = ' ';
  *msgPtr++ = ' ';
}

#endif // DEBUG&DEBUG_MESSAGES


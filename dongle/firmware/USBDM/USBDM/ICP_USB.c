/*! \file
    \brief Minimum USB stack & Flash programing routines for BOOT area
   
    \warning
    It is very important that this code does not use any library
    routines as they will end up in the USER ROM area which may be replaced. \n
    This code must be stand-alone.

    \warning
    This code allows the Flash memory to be read!  It is not suitable for
    devices where security of the Flash memory is of concern.  

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
+====================================================================
|  17 Apr  2009 | Added USB std req. GET_INTERFACE  [ICP v1.4]  - pgo
|   4 Feb  2009 | Made GET_VERSION more compatible  [v1.4]      - pgo
|  23 July 2008 | Started coding                                - pgo
+====================================================================
 \endverbatim
*/
#include <hidef.h>      // For EnableInterrupts macro
#include "Derivative.h" // Include peripheral declarations

#include "Common.h"
#include "Configure.h"
#include "Commands.h"
#include "USBDefs.h"
#include "ICP.h"
#include "ICP_USB.h"

#pragma CONST_SEG BOOT_CONST
#pragma CODE_SEG  BOOT_ROM
#pragma DATA_SEG __SHORT_SEG BOOT_RAM

// Return codes
// Note - can't be an enum as used in ASM code
#define ICP_RC_OK            (0)
#define ICP_RC_ILLEGAL       (1)
#define ICP_RC_FLASH_ERR     (2)
#define ICP_RC_VERIFY_ERR    (3)

//=================================================================================
// LED Port bit masks
// This supports LEDs on pins PTF.4 and PTF.5
//
#ifndef GREEN_LED_MASK
#define GREEN_LED_MASK  (PTFD_PTFD4_MASK)
#define RED_LED_MASK    (PTFD_PTFD5_MASK)
#define LED_PORT_DATA   (PTFD)
#define LED_PORT_DDR    (PTFDD)
#endif

#pragma MESSAGE DISABLE C4301 // Disable warnings about Inline functions

//======================================================================
// Structure representing a BDT entry
typedef struct {
   union {
      struct {
         U8           :2;  
         U8 bdtkpid   :4;
         U8 data0_1   :1;
         U8 own       :1;
      } a;
      struct {
         U8           :2;  
         U8 bdtstall  :1;  
         U8 dts       :1;
         U8           :2;  
         U8 data0_1   :1;
         U8 own       :1;
      } b;
      U8 bits;
   } control;
   U8 byteCount;
   U8 epAddr;  // Offset by two bits 
} BDTEntry;

#define BDTEntry_OWN_MASK        (1<<7)
#define BDTEntry_DATA0_MASK      (0<<6) // dummy
#define BDTEntry_DATA1_MASK      (1<<6)
#define BDTEntry_DTS_MASK        (1<<3)
#define BDTEntry_BDTSTALL_MASK   (1<<2)
//======================================================================
// USB RAM usage
//
#define USB_RAM_START (0x1860U)

#define USB_MAP_ADDRESS(x)  ((((x) - USB_RAM_START)>>2)&0xFC)
#define ADDRESS_ROUND16(x)  ((x+0xF)&0xFFF0)

//======================================================================
// BDTs for the endpoints (in USB RAM)
//
static BDTEntry bdts[10]    @USB_RAM_START;
#define  ep0BDTIn    bdts[0]
#define  ep0BDTOut   bdts[1]

//======================================================================
// Maximum packet sizes for each endpoint
//
#define ENDPT0MAXSIZE    (64)    

//======================================================================
// Buffers for endpoint data packets (in USB RAM)
//
#define EP0InDataBufferAddress ADDRESS_ROUND16(USB_RAM_START+0x20)
static U8 ep0InDataBuffer[ENDPT0MAXSIZE]  @EP0InDataBufferAddress;
#define EP0OutDataBufferAddress ADDRESS_ROUND16(EP0InDataBufferAddress+ENDPT0MAXSIZE)
static U8 ep0OutDataBuffer[ENDPT0MAXSIZE] @EP0OutDataBufferAddress;

//======================================================================
// All of the USB 2-port RAM
static volatile U8 usbRam[256] @USB_RAM_START;


//======================================================================
// Data packet odd/even indicator
enum {DATA0=0, DATA1=1};

//======================================================================
// Descriptors
//
static const DeviceDescriptor deviceDescriptor = {
   sizeof(DeviceDescriptor),               // Length [18 bytes]
   DT_DEVICE,                              // Type [device]
   {0x10, 0x01},                           // USB spec rel. No. [BCD = 1.10]
   0,                                      // Class code [none]
   0,                                      // Sub Class code [none]
   0,                                      // Protocol [none]
   ENDPT0MAXSIZE,                          // EndPt 0 max packet size
   {VendorID&0xFF,  (VendorID>>8)&0xFF},   // Vendor ID
   {ProductID&0xFF, (ProductID>>8)&0xFF},  // Product Id
   {0x00, 0x01},                           // Device Release         [BCD = 1.00]
   0,                                      // String index of Manufacturer name
   0,                                      // String index of product desc.
   0,                                      // String index desc. serial #
   1                                       // Number of configurations
   };

static const struct {
   ConfigurationDescriptor configDescriptor;
   InterfaceDescriptor     interfaceDescriptor;
   } otherDescriptors = 
{
   { // configDescriptor
      sizeof(ConfigurationDescriptor),   // bLength = 9
      DT_CONFIGURATION,                  // bDescriptorType = 2
      {sizeof(otherDescriptors),0},      // wTotalLength = 25
      1,                                 // bNumInterfaces = 1
      1,                                 // bConfigurationValue
      0,                                 // iConfiguration = ""
      0x80,                              // bmAttributes = Bus powered, no wakeup
      150                                // MaxPower = 300 mA
   },
   { // interfaceDescriptor
      sizeof(InterfaceDescriptor),  // bLength = 9
      DT_INTERFACE,                 // bDescriptorType = 4
      0,                            // bInterfaceNumber = 0
      0,                            // bAlternateSetting = 0
      0,                            // bNumEndpoints = 0
      0xFF,                         // bInterfaceClass = [Vendor specific]
      0xFF,                         // bInterfaceSubClass = [Vendor specific]
      0xFF,                         // bInterfaceProtocol = [Vendor specific]
      0                             // iInterface desc = ""
   },
};

//===============================================================================
// Device Status
//       
typedef struct {
   int selfPowered  : 1;
   int remoteWakeup : 1;
   int portTest     : 1;
   int res1         : 5;
   int res2         : 8;
} DeviceStatus; 

static const DeviceStatus deviceStatus = {0,0,0,0,0};

typedef struct {
   U8           configuration;
   U8           newUSBAddress;
} DeviceState;

//===============================================================================
// Endpoint Status
//       
typedef struct {
   int stall  : 1;
   int res1   : 7;
   int res2   : 8;
} EPStatus;

// Endpoint state values
typedef enum { 
   EPIdle = 0,       // Idle (Tx complete)
   EPLastIn,         // Doing the last IN packet
   EPStatusIn,       // Doing an IN packet as a status handshake
   EPLastOut,        // Doing the last OUT packet
   EPStatusOut,      // Doing an OUT packet as a status handshake
} EPModes;

typedef struct {
   EPModes   state;
   U8       (*callback)(void);
} EPState;

static const EPState initialEPState = {EPIdle, NULL};

static SetupPacket ep0SetupBuffer;   //!< Buffer for EP0 Setup packet (copied from USB RAM)
static DeviceState deviceState;      //!< State of USB state machine
static EPState     ep0State;         //!< State of Endpoint \#0

// Flash Programming variables
static U8  dataLength;      //!< Length of data portion of USB pkt (also \#of bytes to Flash)
static U16 startAddress;    //!< Start address of area in Flash to program or verify
static U16 sourceAddress;   //!< Address in buffer for programming/verifying
static U8  flashCommand;    //!< Flash command code
static U8  flashData;       //!< Byte to program to Flash
static U8  commandStatus;   //!< Status of last command executed

/*! Copy a range of memory
 *
 * @param from source ptr
 * @param to destination ptr
 * @param size number of bytes to copy
 */
static void memcpy(U8 *to, const U8 *from, U8 size) {
   while (size-->0)
      *to++ = *from++;
}

/*!  Sets up the clock for USB operation
 *
 * MGCOUT = 48MHz, BUS_CLOCK = 24MHz, (PEE mode)
 *
 * Assumes 12 MHz external crystal
 *
 * Modes: FEI [FLL engaged internal] -> 
 *        FBE [FLL bypassed external] ->
 *        PBE [PLL bypassed external] ->
 *        PEE [PLL engaged external]
 *
 * Refer 12.5.2.1 of MC9S08JM60 Reference
 */
static void initClock(void)
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

//======================================================================
/*! Configure EP0 for an OUT transaction [Rx, device <- host, DATA0/1]
 *
 * @param bufSize - Size of data to transfer
 * @param data0_1 - DATA0/DATA1 toggle value
 */
static void ep0StartOutTransaction( U8 bufSize, U8 data0_1 ){
   ep0State.state = EPLastOut;       // Assume one and only data pkt
   
   if (bufSize == 0)
      ep0State.state = EPStatusOut;  // Actually it's a status handshake
   
   // Set up to Rx packet
   ep0BDTOut.epAddr      = USB_MAP_ADDRESS(EP0OutDataBufferAddress);
   ep0BDTOut.byteCount   = ENDPT0MAXSIZE;
   if (data0_1) 
      ep0BDTOut.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA1_MASK|BDTEntry_DTS_MASK;
   else
      ep0BDTOut.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA0_MASK|BDTEntry_DTS_MASK;
}

//======================================================================
// Configure EP0 for a SETUP transaction [Rx, device<-host, DATA0]
//
static void ep0StartSetupTransaction( void ){
   ep0StartOutTransaction(sizeof(ep0SetupBuffer), DATA0);
   ep0State.state = EPIdle;         // Now Idle
}

//======================================================================
// Configure EP0 for an IN transaction (Rx, device -> host, DATA1 [assuming single DATA pkt]
//
static void ep0StartInTransaction( U8 bufSize, const U8 *bufPtr ){

   ep0State.state = EPStatusIn;   // Assume status handshake
   
   if (bufSize != 0) {
      ep0State.state = EPLastIn;     // Actually it's the one and only data pkt
      memcpy(ep0InDataBuffer, bufPtr, bufSize);
   }
   // Enable endpoint
   EPCTL0  = EPCTL0_EPRXEN_MASK|EPCTL0_EPTXEN_MASK|EPCTL0_EPHSHK_MASK;

   // Set up to Tx packet
   ep0BDTIn.epAddr        = USB_MAP_ADDRESS(EP0InDataBufferAddress);
   ep0BDTIn.byteCount     = bufSize;
   // All IN pkts are DATA1
   ep0BDTIn.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA1_MASK|BDTEntry_DTS_MASK;
   }

//======================================================================
// Configure EP0 for an Empty [Status] IN transaction [Tx, device->host, DATA1]
//
static void ep0StartStatusInTransaction( void ){
   ep0StartInTransaction( 0, NULL); // Do status Pkt transmission
}

//======================================================================
// Stall control for endpoints
//

// Stall ep0
#pragma INLINE
static void ep0Stall(void) {
   EPCTL0 |= EPCTL0_EPSTALL_MASK; 
}

// Clear stall ep0
#pragma INLINE
static void ep0ClearStall(void) {
   EPCTL0 &= ~EPCTL0_EPSTALL_MASK; 
}

//========================================================================================
//
#pragma INLINE
static void setUSBdefaultState( void ){
   ADDR                       = 0;
   deviceState.configuration  = 0;
}

#pragma INLINE
static void setUSBaddressedState( U8 address ){
   ADDR                       = address;
   deviceState.configuration  = 0;
}

#pragma INLINE
static void setUSBconfiguredState( U8 config ){
   deviceState.configuration        = config;
   if (config == 0) // Unconfigure?
      GREEN_LED_OFF();
   else
      GREEN_LED_ON();
}

//======================================================================
// Initialise the USB interface
//
void initICP_USB( void ){

   // Set up clock for USB operation (48MHz)
   initClock();
   
   // Set initial USB state
   setUSBdefaultState();
   ep0State    = initialEPState;

   // Clear USB RAM (includes BDTs)
   asm {
      ldhx  @usbRam
      clra  // 256 bytes
   loop:
      clr   ,x
      aix   #1
      dbnza  loop
   }
   LED_INIT();
   
   // Reset USB   
   USBCTL0_USBRESET = 1;
   while (USBCTL0_USBRESET) {
   }
   
   // Enable USB module.
   CTL = CTL_USBEN_MASK;
   
   // Internal PUP, Internal 3V3 reg, Enable Txvr 
   USBCTL0 = USBCTL0_USBPU_MASK|USBCTL0_USBVREN_MASK|USBCTL0_USBPHYEN_MASK;

   // Clear all pending interrupts except reset.
   INTSTAT = ~INTSTAT_USBRSTF_MASK;
   
   // Disable error interrupts.   
   ERRENB = 0;

   // Disable all interrupts
   INTENB  = 0;
   
   ep0ClearStall();

   // Set up to receive 1st SETUP packet
   ep0StartSetupTransaction();
 
   // Enable endpoint #0
   EPCTL0  = EPCTL0_EPRXEN_MASK|EPCTL0_EPTXEN_MASK|EPCTL0_EPHSHK_MASK;
   }

//===============================================================================
// Get Descriptor - Device Req 0x06
//       
static void handleGetDescriptor( void ) {
U16         wlength         = (ep0SetupBuffer.wLength.le.hi<<8) | ep0SetupBuffer.wLength.le.lo;
U8          descriptorIndex = ep0SetupBuffer.wValue.le.lo;
U8          dataSize;
const char *dataPtr = NULL;

   switch (ep0SetupBuffer.wValue.le.hi) {
      case DT_DEVICE: // Get Device Desc. - 1
         dataSize = (U8)deviceDescriptor.bLength;
         dataPtr  = (U8 *) &deviceDescriptor;
         break;
      case DT_CONFIGURATION: // Get Configuration Desc. - 2
         dataSize = (U8)otherDescriptors.configDescriptor.wTotalLength.le.lo;
         dataPtr  = (U8 *) &otherDescriptors;
         break;
      case DT_STRING: // Get String Desc.- 3
      default:        // shouldn't happen
         ep0Stall();
         return;
      } // switch

   if (dataSize > wlength) // Truncate if more bytes available than asked for
      dataSize = (U8)wlength;
      
   ep0StartInTransaction( dataSize, dataPtr ); // Set up Tx
}

//===============================================================================
// Set device Address Callback
//       
static U8 setAddressCallback(void) {
   ADDR = deviceState.newUSBAddress;
   setUSBaddressedState(deviceState.newUSBAddress);
   return 0;
}

#pragma MESSAGE DISABLE C1404  // Disable warnings about Return expected
#pragma MESSAGE DISABLE C20001 // Disable warnings about stackpointer
#pragma NO_ENTRY
#pragma NO_EXIT
#pragma NO_RETURN
/*!   Low-level Flash programming code

      This code is copied to the stack [RAM] for execution as Flash memory
      cannot be used while being programmed.
      
      @entry 
       - dataLength       =>   Number of bytes to process (must be >0!)
       - startAddress     =>   Start address of area in Flash to process 
       - sourceAddress    =>   Data to use

      @return Final value of FSTAT
      
      @warning - If the size of this routine changes then the constant 
                  ONSTACK_SIZE must be corrected
*/
static U8 onStack(void) {
   asm {
   loop:
      ldhx  sourceAddress        // Get next byte to program
      mov   x+,flashData         //    & increment source address
      sthx  sourceAddress
      
   chkBusy:
      lda   FSTAT                // Loop if not ready to buffer next command
      bit   #FSTAT_FCBEF_MASK
      beq   chkBusy

      bit   #FSTAT_FACCERR_MASK|FSTAT_FPVIOL_MASK;   // Check for any errors
      bne   flashExit
      
   intoLoop:      
      ldhx  startAddress         // Write to flash, latch addr and data
      mov   flashData,x+         //    & increment Flash write address
      sthx  startAddress
      
      lda   flashCommand         // Write the flash command
      sta   FCMD 
      
      lda   #FSTAT_FCBEF_MASK    // Initiate command
      sta   FSTAT
      
      dbnz  dataLength,loop      // More bytes? - yes - loop
      
   chkDone: 
      lda   FSTAT                // Loop if command not complete
      bit   #FSTAT_FCCF_MASK
      beq   chkDone              
      
   flashExit:   
      rts
   }
}

#pragma NO_ENTRY
#pragma NO_EXIT
#pragma NO_RETURN
/*!   Low-level Flash programming code entry point

      Initialises sourceAddress 
      Clears any current Flash errors
      Sets Flash Clock divider
      Then copies the \ref onStack[] routine to the stack and executes it.
            
      @return \n
         - \ref ICP_RC_OK        - Success \n
         - \ref ICP_RC_FLASH_ERR - Programming or erasing failed.
*/
static U8 doOnStack(void) {
#define ONSTACK_SIZE (0x2C)  // #@doOnStack-@onStack 
   RED_LED_ON();
   asm {
      lda   #FCDIV_PRDIV8_MASK|14  // Initialise Flash clock divider
      sta   FCDIV                  // Assumes 24MHz bus clock (req. for USB!)

      lda   #FSTAT_FACCERR_MASK|FSTAT_FPVIOL_MASK;   // Clear any errors
      sta   FSTAT
      
      ldhx  @ep0OutDataBuffer // Set up data source address
      sthx  sourceAddress

      // Copy routine onto stack (RAM)
      //
      ldhx  #@doOnStack       // End of range+1
   pshLoop:
      aix   #-1               // push byte on stack
      lda   ,x
      psha                    
      cphx  #@onStack         // c.f. Start of range
      bne   pshLoop
      
      tsx                     // Execute the routine on the stack
      jsr   ,x
      ais   #ONSTACK_SIZE     // Clean up stack
      
      bit   #FSTAT_FACCERR_MASK|FSTAT_FPVIOL_MASK;   // Check for any errors
      bne   errorExit

   okExit:      
      clra  // lda #ICP_RC_OK
      rts
      
   errorExit:
      lda   #ICP_RC_FLASH_ERR
      rts
   }
   RED_LED_OFF();
}

#pragma MESSAGE DEFAULT C1404  // Restore warnings about Return expected
#pragma MESSAGE DEFAULT C20001 // Restore warnings about stackpointer

/*!   Erase a Page in Flash Memory
   @entry startAddress     Any address within Flash Page to be erased
*/
static U8 erasePageCommand(void) {
   flashCommand = mPageErase;    // Page/sector erase command
   dataLength   = 1;             // A single command
   return doOnStack();
}

/*!   Program a Row in Flash memory
   @entry 
   - dataLength        =>  Number of bytes to program [must be >0]
   - startAddress      =>  Start address of area in Flash to program 
   - ep0OutDataBuffer  =>  Data to program
*/
static U8 programRowCommand(void) {
   flashCommand  = mBurstProg;   // Burst program command
   return doOnStack();
}

#pragma MESSAGE DISABLE C1404  // Restore warnings about Return expected
#pragma NO_ENTRY
#pragma NO_EXIT
#pragma NO_RETURN
/*!   Verify a Row in Flash memory

   @entry 
   - dataLength        =>  Number of bytes to verify [must be >0]
   - startAddress      =>  Start address of area in Flash
   - ep0OutDataBuffer  =>  Data to verify against
*/
static U8 verifyRowCommand(void) {
   asm {
      ldhx  #@ep0OutDataBuffer
      sthx  sourceAddress
      
   Loop:
      ldhx  sourceAddress
      lda   ,x
      aix   #1
      sthx  sourceAddress
      ldhx  startAddress
      cbeq  ,x+,okByte
      lda   #ICP_RC_VERIFY_ERR
      rts

   okByte:
      sthx  startAddress
      
   IntoLoop:   
      dbnz  dataLength,Loop
      lda   #ICP_RC_OK
      rts
   }
}

//! Constant describing the Hardware and Software
static const U8 versionConstant[] = {
   BDM_RC_OK,                   //!< Success transaction
   0xFF, VERSION_HW,            //!< BDM code version unknown
   ICP_VERSION_SW, VERSION_HW   //!< ICP SW/HW version
};                                     


//==================================================================
// Handlers for Token Complete USB interrupt
//

//================
// ep0 - IN
// 
static void handleEp0InToken(void){

   switch (ep0State.state) {
      case EPLastIn:    // Just done the last IN packet
         ep0State.state = EPStatusOut;     // Receiving an OUT status pkt
         ep0StartOutTransaction(0, DATA1); // Do status Pkt reception
         break;
         
      case EPStatusIn:  // Just done an IN packet as a status handshake for an OUT Data transfer
         if (ep0State.callback != NULL)
            commandStatus = ep0State.callback();   // Execute callback function to process OUT data
         ep0State.callback = NULL;
         ep0State.state    = EPIdle;
         break;

      // We don't expect an IN token while in the following states
      default:
         break;
   }
}

//================
// ep0 - OUT
// 
static void handleEp0OutToken(void){

   switch (ep0State.state) {
      case EPLastOut:        // Done the last OUT packet
         ep0StartSetupTransaction();    // Make ready for a SETUP pkt
         ep0StartStatusInTransaction(); // Do status Pkt transmission
         break;

      default:
         ep0StartSetupTransaction();            // Make ready for a SETUP pkt
         break;
   }
}

//===============================================================================
// Handles SETUP Packet
//       
static void handleSetupToken( void ) {
   // Save SETUP pkt
   memcpy((U8*)&ep0SetupBuffer, ep0OutDataBuffer, sizeof(ep0SetupBuffer));
   
   ep0StartSetupTransaction();   // In case another SETUP pkt
   
   ep0State.callback    = NULL;
   dataLength           = ep0SetupBuffer.wLength.le.lo;
   startAddress         = (ep0SetupBuffer.wValue.le.hi<<8)+ep0SetupBuffer.wValue.le.lo;
  
   if (IS_VENDOR_REQ(ep0SetupBuffer.bmRequestType)) {
      // Handle Vendor requests
      if (ep0SetupBuffer.bRequest == ICP_PROGRAM_ROW) {
         ep0StartOutTransaction( dataLength, DATA1 ); // Set up to get rest of command (data portion)
         ep0State.callback = programRowCommand;       // Set callback to execute when data arrives
      } 
      else if (ep0SetupBuffer.bRequest == ICP_VERIFY_ROW) {
         ep0StartOutTransaction( dataLength, DATA1 ); // Set up to get rest of command (data portion)
         ep0State.callback = verifyRowCommand;        // Set callback to execute when data arrives
      } 
      else if (ep0SetupBuffer.bRequest == ICP_ERASE_PAGE) {
         commandStatus = erasePageCommand();          // Execute the command - no response
         ep0StartStatusInTransaction();               // Tx empty Status packet
      } 
      else if (ep0SetupBuffer.bRequest == ICP_GET_RESULT)  {
         ep0StartInTransaction( sizeof(commandStatus),  &commandStatus );
      } 
      else if (ep0SetupBuffer.bRequest == ICP_GET_VER)  {
         ep0StartInTransaction( sizeof(versionConstant),  versionConstant );
      } 
      else if (ep0SetupBuffer.bRequest == ICP_REBOOT)  {
         CTL = 0x00;   // Disable USB
         reset();      // Illegal opcode - causes cpu reset!
      } 
      else
        ep0Stall();
   }
   else  {
      // Handle standard USB requests
      if (ep0SetupBuffer.bRequest == GET_STATUS) {
         ep0StartInTransaction( sizeof(DeviceStatus), (U8 *) &deviceStatus );
      }
      else if (ep0SetupBuffer.bRequest == SET_ADDRESS) {
         // Save address for change after status transaction
         deviceState.newUSBAddress  = ep0SetupBuffer.wValue.le.lo; 
         ep0State.callback          = setAddressCallback;
         ep0StartStatusInTransaction(); // Tx empty Status packet
      }
      else if (ep0SetupBuffer.bRequest == GET_DESCRIPTOR) {
         handleGetDescriptor();
      }
      else if (ep0SetupBuffer.bRequest == GET_INTERFACE) {
         // No eror checking!
         ep0StartInTransaction( 1, &otherDescriptors.interfaceDescriptor.bAlternateSetting);
      }
      else if (ep0SetupBuffer.bRequest == GET_CONFIGURATION) {
         ep0StartInTransaction( 1, &deviceState.configuration );
      }
      else if (ep0SetupBuffer.bRequest == SET_CONFIGURATION) {
         setUSBconfiguredState(ep0SetupBuffer.wValue.le.lo);
         ep0StartStatusInTransaction(); // Tx empty Status packet
      }
      else if (ep0SetupBuffer.bRequest == SET_INTERFACE) {
         ep0StartStatusInTransaction(); // Tx empty Status packet
      }
      else
         ep0Stall();
   }

   // Allow transactions post SETUP
   CTL_TSUSPEND = 0;
}

//==================================================================
// Handler for Token Complete USB interrupt
//
// Only handles ep0 [In & Out]
// 
static void handleTokenComplete(void){
   if (ep0BDTOut.control.a.bdtkpid == SETUPToken) // SETUP transaction complete
      handleSetupToken();
   else if (STAT&STAT_IN_MASK)  // IN Transaction complete
      handleEp0InToken();
   else                         // OUT Transaction complete
      handleEp0OutToken();
   return;
}

//==================================================================
// Handler for USB Bus reset
// 
static void handleUSBReset(void) {
   ERRSTAT = 0xFF;                 // Clear USB error flags

   // Clear all USB interrupt flags
   INTSTAT = INTSTAT_STALLF_MASK|INTSTAT_RESUMEF_MASK|INTSTAT_SLEEPF_MASK| 
             INTSTAT_TOKDNEF_MASK|INTSTAT_SOFTOKF_MASK|INTSTAT_ERRORF_MASK|INTSTAT_USBRSTF_MASK;

   setUSBdefaultState();
}

//==================================================================
// Polling Loop for USB events
// 
// Determines source and dispatches to appropriate routine.
// Never returns
//
void USBEventPollingLoop( void ) {
U8 interruptFlags;

   for(;;) {
      interruptFlags = INTSTAT; // Take snapshot
      if ((interruptFlags&INTSTAT_USBRSTF_MASK) != 0) { // USB Reset
         handleUSBReset();
      }
      if ((interruptFlags&INTSTAT_TOKDNEF_MASK) != 0) { // Token complete int?
         handleTokenComplete();
      }
      INTSTAT = interruptFlags; // Clear all flags
   }
}

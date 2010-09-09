/*! \file
    \brief Simple USB Stack for JM60
    
   \verbatim
   UF32 USB Code
    
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
+=======================================================================================
|  4 Feb 2010 | Changed endpoint sizes to ep0In/ep0Out/ep1out/ep2In = 32/32/64/64  - pgo
|  5 Sep 2009 | Moved SET_BOOT and GET_VERSION to USB module                       - pgo
-=======================================================================================
|    Sep 2009 | Major changes for V2                                               - pgo
-=======================================================================================
| 30 Jul 2009 | Changed USB command/response structure - uses EP1/EP2 - pgo
| 17 May 2009 | Tested with USBCV13.exe from USB.ORG - now passes     - pgo
| 16 May 2009 | Increased validation on handleGetInterface[]          - pgo
| 10 May 2009 | Changed String language to EN_AUS from GREEK!         - pgo
|  7 May 2009 | Changed ep0ConfigureSetupTransaction[] & related      - pgo
| 27 Jan 2009 | Changed SETUP pkt handling (I hate little-endian!)    - pgo
| 27 Jan 2009 | Changed undersize IN transaction handling             - pgo
| 24 Sep 2008 | Fixed possible ptr error in ep0SaveOutData            - pgo
|  3 Mar 2008 | JM60 - USB code written from scratch                  - pgo
+==========================================================================
   \endverbatim
*/
#include <string.h>
#include <hidef.h>      // For EnableInterrupts macro
#include "Derivative.h" // Include peripheral declarations
#include "Common.h"
#include "Configure.h"
#include "Commands.h"
#include "BDM.h"
#include "BDMCommon.h"
#include "CmdProcessing.h"
#include "USBDefs.h"
#include "USB.h"
#include "icp.h"

#if (DEBUG&COMMAND_DEBUG)
#define commandExec() debugCommandExec()
#endif

// Endpoint direction
enum {EP_OUT=0x00, EP_IN=0x80};

//======================================================================
// Data packet odd/even indicator
enum {DATA0=0,DATA1=1};

//======================================================================
// Structure representing a BDT entry
#pragma MESSAGE DISABLE C1106 // Disable warnings about Non-standard bitfield types
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
#pragma MESSAGE DEFAULT C1106 // Restore warnings about Non-standard bitfield types

#define USB_INTMASKS (INTENB_STALL_MASK|INTENB_TOKDNE_MASK|\
                      INTENB_SOFTOK_MASK|INTENB_USBRST_MASK|\
                      INTENB_STALL_MASK)

#define BDTEntry_OWN_MASK        (1<<7)
#define BDTEntry_DATA0_MASK      (0<<6) // dummy
#define BDTEntry_DATA1_MASK      (1<<6)
#define BDTEntry_DTS_MASK        (1<<3)
#define BDTEntry_BDTSTALL_MASK   (1<<2)

//======================================================================
// Maximum packet sizes for each endpoint
//
#define ENDPT0MAXSIZE    (32)    
#define ENDPT1MAXSIZE    (64)
#define ENDPT2MAXSIZE    (64)

//======================================================================
// Descriptors
//
static const DeviceDescriptor deviceDescriptor = {
   sizeof(DeviceDescriptor),               // Length
   DT_DEVICE,                              // Type [device]          [0x01]
   {0x10, 0x01},                           // USB spec rel. No.      [BCD = 1.10]
   0xFF,                                   // Class code             [none]
   0xFF,                                   // Sub Class code         [none]
   0xFF,                                   // Protocol               [none]
   ENDPT0MAXSIZE,                          // EndPt 0 max packet size
   {VendorID&0xFF,  (VendorID>>8)&0xFF},   // Vendor ID
   {ProductID&0xFF, (ProductID>>8)&0xFF},  // Product Id
   {0x00, 0x01},                           // Device Release         [BCD = 1.00]
   1,                                      // String index of Manufacturer name
   2,                                      // String index of product desc.
   3,                                      // String index desc. serial #
   1                                       // Number of configurations
   };

static const struct {
   ConfigurationDescriptor configDescriptor;
   InterfaceDescriptor     interfaceDescriptor;
   EndpointDescriptor      endpointDescriptor5;
   EndpointDescriptor      endpointDescriptor6;
} otherDescriptors =     
{
   { // configDescriptor
      sizeof(ConfigurationDescriptor),    // bLength              = 9
      DT_CONFIGURATION,                   // bDescriptorType      = 2
      {sizeof(otherDescriptors),0},       // wTotalLength         = 25
      1,                                  // bNumInterfaces       = 1
      1,                                  // bConfigurationValue  = 1
      0,                                  // iConfiguration       = ""
      0x80,                               // bmAttributes         = Bus powered, no wakeup (yet?)
      150                                 // MaxPower             = 300 mA
   },
   { // interfaceDescriptor
      sizeof(InterfaceDescriptor),  // bLength              = 9
      DT_INTERFACE,                 // bDescriptorType      = 4
      0,                            // bInterfaceNumber     = 0
      0,                            // bAlternateSetting    = 0
      2,                            // bNumEndpoints        = 2
      0xFF,                         // bInterfaceClass      = (Vendor specific)
      0xFF,                         // bInterfaceSubClass   = (Vendor specific)
      0xFF,                         // bInterfaceProtocol   = (Vendor specific)
      0                             // iInterface desc      = ""
   },
   { // endpointDescriptor1 - OUT
     sizeof(EndpointDescriptor), // bLength           = 7
     DT_ENDPOINT,                // bDescriptorType   = 5
     EP_OUT|1,                   // bEndpointAddress  = (OUT+#1)
     0x02,                       // bmAttributes      = Bulk
     {ENDPT2MAXSIZE,0},          // wMaxPacketSize    = 16
     1                           // bInterval         = 1 ms
   },
   { // endpointDescriptor2 - IN
     sizeof(EndpointDescriptor), // bLength           = 7
     DT_ENDPOINT,                // bDescriptorType   = 5
     EP_IN|2,                    // bEndpointAddress  = (IN+#2)
     0x02,                       // bmAttributes      = Bulk
     {ENDPT1MAXSIZE,0},          // wMaxPacketSize
     1                           // bInterval         = 1 ms
   }
};

static const U8 sd0[] = {4,  DT_STRING, 0x09, 0x0C};  // Language IDs
static const U8 sd1[] = "pgo";                        // Manufacturer
static const U8 sd2[] = ProductDescription;           // Description
static const U8 sd3[] = "0001";                       // Serial #
static const U8 * const stringDescriptors[] = {sd0, sd1, sd2, sd3};

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

typedef enum { USBpowered, USBattached, USBdefault, USBaddressed,
               USBconfigured, USBsuspended } USBstateType;

#pragma DATA_SEG __SHORT_SEG Z_PAGE
struct {
   U8           state;
   U8           configuration;
   U8           interfaceAltSetting;
   DeviceStatus status;
   U8           newUSBAddress;
} deviceState = {USBattached, 0, 0, {0,0,0,0,0}, 0};
#pragma DATA_SEG DEFAULT

//===============================================================================
// Endpoint Status
//       
typedef struct {
   int stall  : 1;
   int res1   : 7;
   int res2   : 8;
} EPStatus;

// Endpoint state
typedef enum { 
   EPIdle = 0,       // Idle (Tx complete)
   EPSetup,          // Expecting SETUP pkt
   EPDataIn,         // Doing a sequence of IN packets (until data count <= EPSIZE)
   EPDataOut,        // Doing a sequence of OUT packets (until data count <= EPSIZE)
   EPLastIn,         // Doing the last IN packet
   EPLastOut,        // Doing the last OUT packet
   EPStatusIn,       // Doing an IN packet as a status handshake
   EPStatusOut,      // Doing an OUT packet as a status handshake
} EPModes;

typedef struct {
   U8       state;
   U8       data0_1;
   EPStatus status;
   U8*      dataPtr;             // Pointer to data buffer 
   U8       dataRemaining;       // Count of remaining bytes to Rx/Tx
   U8       dataCount;           // Count of bytes Rx/Tx so far
   U8       shortInTransaction;  // Indicates that the IN transaction is undersized 
   void     (*callback)(void);
} EPState;


#pragma DATA_SEG __SHORT_SEG Z_PAGE
static volatile EPState ep0State = {EPIdle, DATA0, {0,0,0}, NULL, 0, 0, 0, NULL};
static volatile EPState ep1State = {EPIdle, DATA0, {0,0,0}, NULL, 0, 0, 0, NULL};

static U8  usbActivityFlag = 0;
static U16 frameNum        = 0;

//======================================================================
// Buffer for EP0 Setup packet (copied from USB RAM)
volatile SetupPacket ep0SetupBuffer;

#pragma DATA_SEG DEFAULT

static volatile EPState ep2State = {EPIdle, DATA0, {0,0,0}, NULL, 0, 0, 0, NULL};

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
#define  ep1BDT      bdts[2]
#define  ep2BDT      bdts[3]
#define  ep3BDT      bdts[4]
#define  ep4BDT      bdts[5]
#define  ep5BDTEven  bdts[6]
#define  ep5BDTOdd   bdts[7]
#define  ep6BDTEven  bdts[8]
#define  ep6BDTOdd   bdts[9]

//======================================================================
// Buffers for endpoint data packets (in USB RAM)
//
#define EP0InDataBufferAddress ADDRESS_ROUND16(USB_RAM_START+0x20)
static volatile U8 ep0InDataBuffer[ENDPT0MAXSIZE]  @EP0InDataBufferAddress;

#define EP0OutDataBufferAddress ADDRESS_ROUND16(EP0InDataBufferAddress+ENDPT0MAXSIZE)
static volatile U8 ep0OutDataBuffer[ENDPT0MAXSIZE] @EP0OutDataBufferAddress;

#define EP1DataBufferAddress ADDRESS_ROUND16(EP0OutDataBufferAddress+ENDPT0MAXSIZE)
static volatile U8 ep1DataBuffer[ENDPT1MAXSIZE]    @EP1DataBufferAddress;

#define EP2DataBufferAddress ADDRESS_ROUND16(EP1DataBufferAddress+ENDPT1MAXSIZE)
static volatile U8 ep2DataBuffer[ENDPT2MAXSIZE]    @EP2DataBufferAddress;

#if (EP2DataBufferAddress+ENDPT2MAXSIZE) > USB_RAM_START+256
#error USB endpoint buffers too large
#endif

//======================================================================
// All of the USB RAM
//
static volatile U8 usbRam[256] @USB_RAM_START;

#pragma DATA_SEG DEFAULT
//======================================================================
// Configure the BDT for EP0 Out [Rx, device <- host, DATA0/1]
//
void ep0InitialiseBDTOut(void){
   // Set up to Rx packet
   ep0BDTOut.epAddr      = USB_MAP_ADDRESS(EP0OutDataBufferAddress);
   ep0BDTOut.byteCount   = (U8)ENDPT0MAXSIZE;
   if (ep0State.data0_1) 
      ep0BDTOut.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA1_MASK|BDTEntry_DTS_MASK;
   else
      ep0BDTOut.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA0_MASK|BDTEntry_DTS_MASK;
}

//=========================================================================
// Save the data from an EP0 OUT pkt and advance ptrs etc.
//
U8 ep0SaveOutData( void ) {
U8 size = ep0BDTOut.byteCount;

   if (size > 0) {
      if (size > ep0State.dataRemaining)
         size = ep0State.dataRemaining;
      if (ep0State.dataPtr != NULL) {
         // Copy the data from the Rx buffer
         (void)memcpy(ep0State.dataPtr, ep0OutDataBuffer, size);
      }
      ep0State.dataRemaining -= size;   // Count down bytes to go
      ep0State.dataCount     += size;   // Count bytes so far
      ep0State.dataPtr       += size;   // Advance buffer ptr
      }
   ep0State.data0_1        = !ep0State.data0_1;        // Toggle DATA0/1
   return size;
}

//======================================================================
/*! Configure EP0 for an OUT transaction [Rx, device <- host, DATA0/1]
 *
 * @param bufSize - Size of data to transfer
 * @param bufPtr  - Buffer for data
 * @param data0_1 - DATA0/DATA1 toggle value
 */
void ep0StartOutTransaction( U8 bufSize, U8 *bufPtr, U8 data0_1 ){

   ep0State.dataRemaining  = bufSize; // Total bytes to Rx
   ep0State.dataCount      = 0;       // Reset count of bytes so far
   ep0State.dataPtr        = bufPtr;  // Where to (eventually) place data
   ep0State.data0_1        = data0_1; // Initial data toggle
   
   if (bufSize == 0) 
      ep0State.state = EPStatusOut;  // Assume status handshake
   else if (bufSize <= ENDPT0MAXSIZE) 
      ep0State.state = EPLastOut;    // Assume one and only data pkt
   else
      ep0State.state = EPDataOut;    // Assume first of several data pkts

   ep0InitialiseBDTOut(); // Configure the BDT for transfer
}

//======================================================================
// Configure EP0-out for a SETUP transaction [Rx, device<-host, DATA0]
// Only done if endpoint is not already configured for some other OUT transaction
//
void ep0ConfigureSetupTransaction( void ){

   switch (ep0State.state) {
      case EPDataOut:        // Doing a sequence of OUT packets (until data count <= EPSIZE)
      case EPLastOut:        // Doing the last OUT packet
      case EPStatusOut:      // Doing an OUT packet as a status handshake
         // EP0-OUT is already set up for an OUT pkt
         break;
         
      case EPIdle:           // Idle
      case EPDataIn:         // Doing a sequence of IN packets
      case EPLastIn:         // Doing the last IN packet
      case EPStatusIn:       // Doing an IN packet as a status handshake for an OUT Data transfer
      default:
         // Set up EP0-OUT to Rx SETUP packets
         ep0BDTOut.epAddr        = USB_MAP_ADDRESS(EP0OutDataBufferAddress);
         ep0BDTOut.byteCount     = (U8)ENDPT0MAXSIZE;
         ep0BDTOut.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA0_MASK|BDTEntry_DTS_MASK;
         break;
   }
}

//======================================================================
// Configure the BDT for EP0 In [Tx, device -> host]
//
void ep0InitialiseBDTIn( void ){
U16 size;

   size = ep0State.dataRemaining;
   if (size > ENDPT0MAXSIZE)
      size = ENDPT0MAXSIZE;
   
   // Copy the Tx data to Tx buffer
   (void)memcpy(ep0InDataBuffer, ep0State.dataPtr, size);
      
   ep0State.dataPtr         += size;  // Ptr to _next_ data
   ep0State.dataRemaining   -= size;  // Count of remaining bytes
   ep0State.dataCount       += size;  // Count of bytes so far
   
   // Set up to Tx packet
   ep0BDTIn.epAddr        = USB_MAP_ADDRESS(EP0InDataBufferAddress);
   ep0BDTIn.byteCount     = (U8)size;
   if (ep0State.data0_1) 
      ep0BDTIn.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA1_MASK|BDTEntry_DTS_MASK;
   else
      ep0BDTIn.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA0_MASK|BDTEntry_DTS_MASK;

   ep0State.data0_1         = !ep0State.data0_1;   // Toggle DATA0/1 for next pkt
}

//======================================================================
// Configure EP0 for an IN transaction [Tx, device -> host, DATA0/1]
//
void ep0StartInTransaction( U8 bufSize, const U8 *bufPtr, U8 data0_1 ){

   if (bufSize > ep0SetupBuffer.wLength.word) // More data than requested - truncate
      bufSize = (U8)ep0SetupBuffer.wLength.word;

   ep0State.dataPtr            = (U8*)bufPtr;   // Ptr to _next_ data
   ep0State.dataRemaining      = bufSize;       // Count of remaining bytes
   ep0State.dataCount          = 0;             // Reset count of bytes so far
   ep0State.data0_1            = data0_1;       // Initial data toggle
   ep0State.shortInTransaction = bufSize < ep0SetupBuffer.wLength.word; // Short transaction?

   if (bufSize == 0)
      ep0State.state = EPStatusIn;   // Assume status handshake
   else if ((bufSize < ENDPT0MAXSIZE) ||    // Undersize pkt OR
            ((bufSize == ENDPT0MAXSIZE) &&  // Full size AND
             !ep0State.shortInTransaction)) //   Don't need to flag undersize transaction 
      ep0State.state = EPLastIn;    // Sending one and only pkt
   else 
      ep0State.state = EPDataIn;    // Sending first of several pkts
      
   ep0InitialiseBDTIn(); // Configure the BDT for transfer
}

//======================================================================
// Configure the BDT for EP1 Out [Rx, device <- host, DATA0/1]
//
void ep1InitialiseBDTOut(void){
   // Set up to Rx packet
   ep1BDT.epAddr      = USB_MAP_ADDRESS(EP1DataBufferAddress);
   ep1BDT.byteCount   = (U8)ENDPT1MAXSIZE;
   if (ep1State.data0_1) 
      ep1BDT.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA1_MASK|BDTEntry_DTS_MASK;
   else
      ep1BDT.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA0_MASK|BDTEntry_DTS_MASK;
}

//=========================================================================
// Save the data from an EP1 OUT pkt and advance ptrs etc.
//
U8 ep1SaveOutData( void ) {
U8 size; 

   size = ep1BDT.byteCount;
   if (size > 0) {
      if (size > ep1State.dataRemaining)
         size = ep1State.dataRemaining;
      if (ep1State.dataPtr != NULL)
         // Copy the data from the Rx buffer
         (void)memcpy(ep1State.dataPtr, ep1DataBuffer, ep1BDT.byteCount);
      ep1State.dataRemaining -= size;   // Count down bytes to go
      ep1State.dataCount     += size;   // Count bytes so far
      ep1State.dataPtr       += size;   // Advance buffer ptr
      }
   ep1State.data0_1        = !ep1State.data0_1;        // Toggle DATA0/1
   return size;
}

//======================================================================
/*! Configure EP1-out for an OUT transaction [Rx, device <- host, DATA0/1]
 *
 * @param bufSize - Size of data to transfer
 * @param bufPtr  - Buffer for data
 */
void ep1StartOutTransaction( U8 bufSize, U8 *bufPtr ){

   ep1State.dataRemaining  = bufSize; // Total bytes to Rx
   ep1State.dataCount      = 0;       // Reset count of bytes so far
   ep1State.dataPtr        = bufPtr;  // Where to (eventually) place data
   
   ep1State.state = EPDataOut;    // Assume first of several data pkts

   ep1InitialiseBDTOut(); // Configure the BDT for transfer
}

//======================================================================
// Configure the BDT for EP2 In [Tx, device -> host]
//
void ep2InitialiseBDTIn( void ){
U16 size;

   // Enable endpoint
   // EPCTL2  = EPCTL2_EPTXEN_MASK|EPCTL2_EPHSHK_MASK;

   size = ep2State.dataRemaining;
   if (size > ENDPT2MAXSIZE)
      size = ENDPT2MAXSIZE;
   
   // Copy the Tx data to Tx buffer
   (void)memcpy(ep2DataBuffer, ep2State.dataPtr, size);
      
   ep2State.dataPtr         += size;  // Ptr to _next_ data
   ep2State.dataRemaining   -= size;  // Count of remaining bytes
   ep2State.dataCount       += size;  // Count of bytes so far
   
   // Set up to Tx packet
   ep2BDT.epAddr        = USB_MAP_ADDRESS(EP2DataBufferAddress);
   ep2BDT.byteCount     = (U8)size;
   if (ep2State.data0_1) 
      ep2BDT.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA1_MASK|BDTEntry_DTS_MASK;
   else
      ep2BDT.control.bits  = BDTEntry_OWN_MASK|BDTEntry_DATA0_MASK|BDTEntry_DTS_MASK;

   ep2State.data0_1         = !ep2State.data0_1;   // Toggle DATA0/1 for next pkt
}

//======================================================================
// Configure EP2 for an IN transaction [Tx, device -> host, DATA0/1]
//
void ep2StartInTransaction( U8 bufSize, const U8 *bufPtr ){

   ep2State.dataPtr            = (U8*)bufPtr;   // Ptr to _next_ data
   ep2State.dataRemaining      = bufSize;       // Count of remaining bytes
   ep2State.dataCount          = 0;             // Reset count of bytes so far
   
   // Note - Always terminates transfers with a truncated/zero pkt
   if (bufSize < ENDPT2MAXSIZE)    // Undersize pkt OR
      ep2State.state = EPLastIn;    // Sending one and only pkt
   else 
      ep2State.state = EPDataIn;    // Sending first of several pkts
      
   ep2InitialiseBDTIn(); // Configure the BDT for transfer
}

//======================================================================
//! Receive a command over EP1
//!
//! @param maxSize  = max # of bytes to receive
//! @param buffer   = ptr to buffer for bytes received
//!
//! @note : Doesn't return until command has been received.
//!
U8 receiveUSBCommand(U8 maxSize, U8 *buffer) {
U8 size;

   // Size of first (command) transaction
   size = ENDPT1MAXSIZE;   
   if (size > maxSize)
      size = maxSize;
   
   // Receive Command transaction
   ep1StartOutTransaction( size, buffer );
   while (ep1State.state != EPIdle) 
      wait();

   // Size for entire command 
   size = buffer[0];  
   if (size > maxSize)
      size = maxSize;
         
   // Receive rest of data if present
   if (size > ep1State.dataCount) {
      ep1StartOutTransaction( size-ep1State.dataCount, buffer+ep1State.dataCount );
      while (ep1State.state != EPIdle) 
         wait();
   }
   return buffer[0];
}

//======================================================================
//! Set a command response over EP2
//!
//! @param size   = # of bytes to send
//! @param buffer = ptr to bytes to send
//!
//! @note : Returns before the command has been sent.
//!
void sendUSBResponse( U8 size, U8 *buffer) {
   ep2StartInTransaction( size, buffer);
   //while (ep2State.state != EPIdle) 
   //   wait();
   //return ep2State.dataCount;
}

//======================================================================
// Stall control for endpoints
//

// Stall ep0
static void ep0Stall(void) {
   EPCTL0 |= EPCTL0_EPSTALL_MASK; 
   ep0State.status.stall = 1;
   //dprint("epSt()");
}

// Clear stall ep0
static void ep0ClearStall(void) {
   EPCTL0 &= ~EPCTL0_EPSTALL_MASK; 
   ep0State.status.stall = 0;
   //dprint("epCSt()\r\n");
}

// Stall ep1
void ep1Stall(void) {
   EPCTL1 |= EPCTL1_EPSTALL_MASK; 
   ep1State.status.stall = 1;
   //dprint("epSt()");
}

// Clear stall ep0
void ep1ClearStall(void) {
   EPCTL1 &= ~EPCTL1_EPSTALL_MASK; 
   ep1State.status.stall = 0;
   //dprint("epCSt()\r\n");
}

// Stall ep2
void ep2Stall(void) {
   EPCTL2 |= EPCTL2_EPSTALL_MASK; 
   ep2State.status.stall = 1;
   //dprint("epSt()");
}

// Clear stall ep2
void ep2ClearStall(void) {
   EPCTL2 &= ~EPCTL2_EPSTALL_MASK; 
   ep2State.status.stall = 0;
   //dprint("epCSt()\r\n");
}

//========================================================================================
//
void setUSBdefaultState( void ){
   GREEN_LED_OFF();
   deviceState.state                = USBdefault;
   ADDR                             = 0;
   deviceState.configuration        = 0;
   deviceState.interfaceAltSetting  = 0; 
}

void setUSBaddressedState( U8 address ){
   if (address == 0) // Unaddress??
      setUSBdefaultState();
   else {
      GREEN_LED_OFF();
      deviceState.state                = USBaddressed;
      ADDR                             = address;
      deviceState.configuration        = 0;
      deviceState.interfaceAltSetting  = 0; 
   }
}

void setUSBconfiguredState( U8 config ){
   if (config == 0) // unconfigure
      setUSBaddressedState(ADDR);
   else {
      GREEN_LED_ON();
      deviceState.state                = USBconfigured;
      deviceState.configuration        = config;
      deviceState.interfaceAltSetting  = otherDescriptors.interfaceDescriptor.bAlternateSetting; // xxx check
   }
}

//==================================================================
// Handler for USB Bus reset
// 
void handleUSBReset(void) {
   ERRSTAT = 0xFF;                 // Clear USB error flags
   // Clear all USB interrupt flags
   INTSTAT = INTSTAT_STALLF_MASK|INTSTAT_RESUMEF_MASK|INTSTAT_SLEEPF_MASK| 
             INTSTAT_TOKDNEF_MASK|INTSTAT_SOFTOKF_MASK|INTSTAT_ERRORF_MASK|
             INTSTAT_USBRSTF_MASK;

   ERRENB  = 0x00;                 // Disable all USB error interrupt sources

   // Enable various interrupts
   INTENB  = USB_INTMASKS;

   // Set up to receive setup packet
   ep0State.state = EPIdle;
   ep0ConfigureSetupTransaction();

   setUSBdefaultState();

   deviceState.state = USBdefault;

   setUSBdefaultState();

   ep0ClearStall();
   ep1ClearStall();
   ep2ClearStall();
}

//======================================================================
// Initialise the USB interface
//
void initUSB( void ){

   // Clear USB RAM (includes BDTs)
   (void)memset(usbRam, 0x00, sizeof(usbRam));
   
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
   
   // Enable endpoints
   EPCTL0  = EPCTL0_EPRXEN_MASK|EPCTL0_EPTXEN_MASK|EPCTL0_EPHSHK_MASK; // IN/OUT/SEUP
   EPCTL1  = EPCTL1_EPRXEN_MASK|                   EPCTL1_EPHSHK_MASK; // OUT
   EPCTL2  =                    EPCTL2_EPTXEN_MASK|EPCTL2_EPHSHK_MASK; // IN

   // Enable usb reset interrupt
   INTENB_USBRST=1;    // check
  
   handleUSBReset();   
}

//===============================================================================
// Get Status - Device Req 0x00
//       
void handleGetStatus( void ) {
static const U8 zeroReturn[] = {0,0};

   //dprint("hGS()");
   switch(ep0SetupBuffer.bmRequestType) {
      case (EP_IN|RT_DEVICE) : // Device Status
         ep0StartInTransaction( sizeof(deviceState.status), 
                               (char *) &deviceState.status, DATA1 );
         break;
      case (EP_IN|RT_INTERFACE) : // Interface Status - reserved
         ep0StartInTransaction( sizeof(zeroReturn), 
                               (char *) &zeroReturn, DATA1 );
         break;
      case (EP_IN|RT_ENDPOINT) : // Endpoint Status
         if (ep0SetupBuffer.wIndex.word == (EP_OUT|1)) { // endpt 1 - out
            ep0StartInTransaction( sizeof(ep1State.status), 
                                  (char *) &ep1State.status, DATA1 );
            }
         else if (ep0SetupBuffer.wIndex.word == (EP_IN|2)) { // endpt 2 - in
            ep0StartInTransaction( sizeof(ep2State.status), 
                                  (char *) &ep2State.status, DATA1 );
            }
         break;
      
      default : // Illegal
         ep0Stall();
         break;
      }
}    

//===============================================================================
// Clear Feature - Device Req 0x01
//       
void handleClearFeature( void ) {
int okStatus         = 0;

   switch(ep0SetupBuffer.bmRequestType) {
      case (RT_DEVICE) : // Device Feature
         if ((ep0SetupBuffer.wValue.word != DEVICE_REMOTE_WAKEUP) || // Device remote wakeup
             (ep0SetupBuffer.wIndex.word != 0))   {                  // Device index must be 0
            break;
            }
         deviceState.status.remoteWakeup = 0;
         okStatus                        = 1;
         break;

      case (RT_INTERFACE) : // Interface Feature
         break;

      case (RT_ENDPOINT) : // Endpoint Feature ( Out,Standard,Endpoint )
         if (ep0SetupBuffer.wValue.word != ENDPOINT_HALT) // Not Endpoint Stall ?
            break;
         if (ep0SetupBuffer.wIndex.word == (EP_OUT|1)) {   // endpt 1 - out
            ep1ClearStall();    // clear stall on Endpt 1
            okStatus = 1;
            }
         else if (ep0SetupBuffer.wIndex.word == (EP_IN|2)) { // endpt 2 - in
            ep2ClearStall();    // clear stall on Endpt 2
            okStatus = 1;
            }
         break;

      default : // Illegal
         break;
      }

   if (okStatus)
      ep0StartInTransaction( 0, NULL, DATA1 ); // Tx empty Status packet
   else
      ep0Stall();
}

//===============================================================================
// Set Feature - Device Req 0x03
//       
void handleSetFeature( void ) {
int okStatus        = 0;

   switch(ep0SetupBuffer.bmRequestType) {
      case (RT_DEVICE) : // Device Feature
         if ((ep0SetupBuffer.wValue.word != DEVICE_REMOTE_WAKEUP) || // Device remote wakeup
             (ep0SetupBuffer.wIndex.word != 0)) {                    // device wIndex must be 0
            break;
            }
         deviceState.status.remoteWakeup = 1;
         okStatus                        = 1;
         break;

      case (RT_INTERFACE) : // Interface Feature
         break;

      case (RT_ENDPOINT) : // Endpoint Feature ( Out,Standard,Endpoint )
         if (ep0SetupBuffer.wValue.word != ENDPOINT_HALT) // Not Endpoint Stall ?
            break;
         if (ep0SetupBuffer.wIndex.word == (EP_OUT|1)) { // endpt 1 - out
            ep1Stall();
            okStatus              = 1;
            }
         else if (ep0SetupBuffer.wIndex.word == (EP_IN|2)) { // endpt 2 - in
            ep2Stall();
            okStatus              = 1;
            }
         break;

      default : // Illegal
         break;
      }
      
   if (okStatus)
      ep0StartInTransaction( 0, NULL, DATA1 ); // Tx empty Status packet
   else
      ep0Stall();
}

//===============================================================================

//! Creates a valid string descriptor in Unicode from a C string
//!
//! @param source - C string
//! @param dest   - Where to place descriptor
//!
void unicodeExpand(const U8 *source, U8 *dest) {
U8 *size = dest; // 1st byte is descriptor size

    *dest++ = 2;         // 1st byte = descriptor size (2 bytes so far)
    *dest++ = DT_STRING; // 2nd byte = DT_STRING;
    while (*source != '\0') {
       *dest++ = *source++; // Copy ASCII byte
       *dest++ = 0;         // Pad with zero for Unicode (little-endian!)
       *size  += 2;         // Update size
    }
}

//===============================================================================
// Get Descriptor - Device Req 0x06
//       
void handleGetDescriptor( void ) {
int         descriptorIndex = ep0SetupBuffer.wValue.be.lo;
int         dataSize = 0;
const U8   *dataPtr = NULL;

   if (ep0SetupBuffer.bmRequestType != (EP_IN|RT_DEVICE)) {// In,Standard,Device
      ep0Stall();
      return;
   }

   switch (ep0SetupBuffer.wValue.be.hi) {

      case DT_DEVICE: // Get Device Desc. - 1
         dataPtr  = (U8 *) &deviceDescriptor;
         dataSize = sizeof(DeviceDescriptor); //deviceDescriptor.bLength;
         break;
      case DT_CONFIGURATION: // Get Configuration Desc. - 2
         //dprint("hGDconf()\r\n");
         if (ep0SetupBuffer.wValue.be.lo != 0) {
            ep0Stall();
            return;
         }
         dataPtr  = (U8 *) &otherDescriptors;
         dataSize = sizeof(otherDescriptors); // otherDescriptors.configDescriptor.wTotalLength.le.lo;
         break;
      case DT_STRING: // Get String Desc.- 3
         if (descriptorIndex >= sizeof(stringDescriptors)/sizeof(stringDescriptors[0])) {
            ep0Stall(); // Illegal string index - stall
            return;
         }
         if (descriptorIndex == 0) { // Language bytes
            dataPtr  = stringDescriptors[0];
         } 
         else { // string
            dataPtr = commandBuffer;
            unicodeExpand(stringDescriptors[descriptorIndex], commandBuffer);
         }
         dataSize = *dataPtr;
         break;
      case DT_DEVICEQUALIFIER: // Get Device Qualifier Descr.
      default: // shouldn't happen
         ep0Stall();
         return;
      } // switch

   ep0StartInTransaction( (U8)dataSize, dataPtr, DATA1 ); // Set up Tx
}

//===============================================================================
// Set device Address Callback
//       
static void setAddressCallback(void) {
   //dprint("setACB()");
   ADDR = deviceState.newUSBAddress;
   if ((deviceState.state == USBdefault) && (deviceState.newUSBAddress != 0))
      setUSBaddressedState(deviceState.newUSBAddress);
   else if ((deviceState.state == USBaddressed) && (deviceState.newUSBAddress == 0))
      setUSBdefaultState();
}

//===============================================================================
// Set device Address - Device Req 0x05
//       
void handleSetAddress( void ) {

   if (ep0SetupBuffer.bmRequestType != (EP_OUT|RT_DEVICE)) {// Out,Standard,Device
      //dprint("hSA():inv. bmR");
      ep0Stall(); // Illegal format - stall ep0
      return;
      }
   // Save address for change after status transaction
   deviceState.newUSBAddress  = ep0SetupBuffer.wValue.be.lo; 
   ep0State.callback          = setAddressCallback;
   
   ep0StartInTransaction( 0, NULL, DATA1 ); // Tx empty Status packet
}

//===============================================================================
// Get Configuration - Device Req 0x08
//       
void   handleGetConfiguration( void ) {

   ep0StartInTransaction( 1, (U8 *) &deviceState.configuration, DATA1 );
}

//===============================================================================
// Set Configuration - Device Req 0x09
//       
void handleSetConfiguration( void ) {

   //dprint("hSC()\r\n");
   if ((ep0SetupBuffer.bmRequestType != (EP_OUT|RT_DEVICE)) || // Out,Standard,Device
       ((ep0SetupBuffer.wValue.be.lo != 0) &&       // Only support 1 configuration
        (ep0SetupBuffer.wValue.be.lo != otherDescriptors.configDescriptor.bConfigurationValue))) { 
      ep0Stall();
      return;
      }
   
   setUSBconfiguredState(ep0SetupBuffer.wValue.be.lo);

   ep1ClearStall();
   ep2ClearStall();;
   
   ep0StartInTransaction( 0, NULL, DATA1 ); // Tx empty Status packet
}

//===============================================================================
// Get interface - Device Req 0x0A
//       
void handleGetInterface( void ) {
U8 interfaceBuffer;

   if ((ep0SetupBuffer.bmRequestType != (EP_IN|RT_INTERFACE)) || // NOT In,Standard,Interface
       (ep0SetupBuffer.wLength.word != 1) ||                     // NOT correct length
       (deviceState.state != USBaddressed)) {                    // NOT in addressed state
      ep0Stall(); // Error
      return;
      }

   // Only support one interface
   if (ep0SetupBuffer.wValue.word != otherDescriptors.interfaceDescriptor.bInterfaceNumber) {
      ep0Stall(); // Error
      return;
      }

   interfaceBuffer = (U8)deviceState.interfaceAltSetting;  // set up buffer for transfer
   ep0StartInTransaction( 1, (char *) &interfaceBuffer, DATA1 ); // Send packet
}

//=================================================
// Illegal request in SETUP pkt
//
void handleUnexpected(void) {
   //dprint("hUx()\r\n");
   ep0Stall();
}


//=================================================
//! Swaps words between \e big-endian and \e little-endian
//!
#define SWAP(value)       \
   { asm (ldx  value:0);  \
     asm (lda  value:1);  \
     asm (stx  value:1);  \
     asm (sta  value:0);  \
   }
         
//===============================================================================
// Handles SETUP Packet
//       
void handleSetupToken( void ) {
static void (* const(funcs[]))(void) = {
   handleGetStatus,        //   0 : GET_STATUS 
   handleClearFeature,     //   1 : CLEAR_FEATURE
   handleUnexpected,       //   2 : Reserved
   handleSetFeature,       //   3 : SET_FEATURE
   handleUnexpected,       //   4 : Reserved
   handleSetAddress,       //   5 : SET_ADDRESS
   handleGetDescriptor,    //   6 : GET_DESCRIPTOR
   handleUnexpected,       //   7 : SET_DESCRIPTOR - not supported
   handleGetConfiguration, //   8 : GET_CONFIGURATION
   handleSetConfiguration, //   9 : SET_CONFIGURATION
   handleGetInterface,     //  10 : GET_INTERFACE
   handleUnexpected,       //  11 : SET_INTERFACE - not supported
   handleUnexpected,       //  12 : SYNCH_FRAME
   };

   // Save data from SETUP pkt
   ep0SetupBuffer = *(SetupPacket *)ep0OutDataBuffer;

   ep0State.state    = EPIdle;
   ep0State.callback = NULL;

   if (IS_VENDOR_REQ(ep0SetupBuffer.bmRequestType)) {
      // Handle special commands here
      if (ep0SetupBuffer.bRequest == ICP_GET_VER) {
         U8 versionConstant[5];
         versionConstant[0] = BDM_RC_OK; 
         versionConstant[1] = VERSION_SW;      // BDM SW/HW version
         versionConstant[2] = VERSION_HW;
         versionConstant[3] = ICP_Version_SW;  // ICP SW/HW version
         versionConstant[4] = ICP_Version_HW;
         ep0StartInTransaction( sizeof(versionConstant),  versionConstant, DATA1 );
      }
      else if (ep0SetupBuffer.bRequest == CMD_USBDM_ICP_BOOT) {
         
         // Turn on bootloader & reset CPU!
         forceICPReset();  // Reboots to ICP mode - doesn't return
      }
   }   
   else if (ep0SetupBuffer.bRequest <= sizeof(funcs)/sizeof(funcs[0])) { // Valid request?
      SWAP(ep0SetupBuffer.wLength);     // Convert SETUP values to big-endian
      SWAP(ep0SetupBuffer.wValue);
      SWAP(ep0SetupBuffer.wIndex);
      funcs[ep0SetupBuffer.bRequest](); // Dispatch to service routine
   }
   else
      handleUnexpected();

   ep0ConfigureSetupTransaction();   // In case another SETUP pkt
      
   // Allow transactions post SETUP
   CTL_TSUSPEND = 0;
}

//==================================================================
// Handlers for Token Complete USB interrupt
//

//================
// ep0 - IN
// 
void handleEp0InToken(void){

   switch (ep0State.state) {
      case EPDataIn:    // Doing a sequence of IN packets (until data count <= EPSIZE)
         if ((ep0State.dataRemaining < ENDPT0MAXSIZE) ||   // Undersize pkt OR
             ((ep0State.dataRemaining == ENDPT0MAXSIZE) && // Full size AND
               !ep0State.shortInTransaction))              //   Don't need to flag undersize transaction 
            ep0State.state = EPLastIn;    // Sending last pkt
         else 
            ep0State.state = EPDataIn;    // Sending full pkt
         ep0InitialiseBDTIn(); // Set up next IN pkt
         break;
         
      case EPLastIn:    // Just done the last IN packet
         ep0State.state = EPStatusOut;   // Receiving an OUT status pkt
         ep0StartOutTransaction(0, NULL, DATA1); // Do status Pkt reception
         break;
         
      case EPStatusIn:  // Just done an IN packet as a status handshake for an OUT Data transfer
         if (ep0State.callback != NULL)
            ep0State.callback(); // Execute callback function to process OUT data
         ep0State.callback = NULL;
         break;

      // We don't expect an IN token while in the following states
      case EPIdle:           // Idle (Tx complete)
      case EPDataOut:        // Doing a sequence of OUT packets (until data count <= EPSIZE)
      case EPLastOut:        // Doing the last OUT packet
      case EPStatusOut:      // Doing an OUT packet as a status handshake
      default:
         break;
   }
}

//================
// ep0 - OUT
// 
void handleEp0OutToken(void){
U8 transferSize;

   switch (ep0State.state) {
      case EPDataOut:        // Receiving a sequence of OUT packets
         transferSize = ep0SaveOutData();          // Save the data from the Rx buffer
         // Completed on undersize pkt or received expected number of bytes
         if ((transferSize < ENDPT0MAXSIZE) || (ep0State.dataRemaining == 0)) { // Last pkt?
            ep0State.state = EPIdle;
            ep0StartInTransaction(0, NULL, DATA1); // Do status Pkt transmission
            }
         else
            ep0InitialiseBDTOut(); // Set up next OUT pkt
         break;

      case EPStatusOut:      // Done an OUT packet as a status handshake
         ep0State.state = EPIdle;
         break;
        
      // We don't expect an OUT token while in the following states
      case EPLastIn:          // Just done the last IN packet
      case EPDataIn:          // Doing a sequence of IN packets (until data count <= EPSIZE)
      case EPStatusIn:        // Just done an IN packet as a status handshake
      case EPIdle:            // Idle (Tx complete)
         break;
   }
   ep0ConfigureSetupTransaction();  // Make ready for a SETUP pkt
}

//=================================================
// ep0 - STALL completed - re-enable ep0 for SETUP
// 
void handleEp0StallComplete(void){
   if (EPCTL0 & EPCTL0_EPSTALL_MASK) {// ep0 stalled? 
      ep0ClearStall();
      ep0ConfigureSetupTransaction(); // yes - re-enable
      }
}

//================
// ep1 - OUT
// 
void handleEp1OutToken(void){
U8 transferSize;

   switch (ep1State.state) {
      case EPDataOut:        // Doing a sequence of OUT packets
         transferSize = ep1SaveOutData();          // Save the data from the Rx buffer
         // Completed transfer on undersize pkt or received expected number of bytes
         if ((transferSize < ENDPT1MAXSIZE) || (ep1State.dataRemaining == 0)) // Last pkt?
            ep1State.state = EPIdle;
         else
            ep1InitialiseBDTOut(); // Set up next OUT pkt
         break;
        
      // We don't expect an OUT token while in the following states
      case EPLastOut:         
      case EPLastIn:          // Just done the last IN packet
      case EPDataIn:          // Doing a sequence of IN packets (until data count <= EPSIZE)
      case EPStatusIn:        // Just done an IN packet as a status handshake
      case EPIdle:            // Idle (Tx complete)
         break;
   }
}

//================
// ep2 - IN
// 
void handleEp2InToken(void){

   switch (ep2State.state) {
      case EPDataIn:    // Doing a sequence of IN packets (until data count <= EPSIZE)
         if (ep2State.dataRemaining < ENDPT2MAXSIZE) 
            ep2State.state = EPLastIn;    // Sending last pkt (may be empty)
         else 
            ep2State.state = EPDataIn;    // Sending full pkt
         ep2InitialiseBDTIn(); // Set up next IN pkt
         break;
         
      case EPLastIn:    // Just done the last IN packet
         ep2State.state = EPIdle;   // Receiving an OUT status pkt
         break;
         
      // We don't expect an IN token while in the following states
      case EPIdle:           // Idle (Tx complete)
      case EPDataOut:        // Doing a sequence of OUT packets (until data count <= EPSIZE)
      case EPLastOut:        // Doing the last OUT packet
      case EPStatusOut:      // Doing an OUT packet as a status handshake
      default:
         break;
   }
}

//==================================================================
// Handler for Token Complete USB interrupt
//
// Only handles ep0 [In & Out]
// 
void handleTokenComplete(U8 status){
   
   U8 endPoint      = ((U8)status)>>4;             // Endpoint number
   U8 directionIsIn = status&STAT_IN_MASK;         // Direction of T/F 0=>OUT, (!=0)=>IN
   U8 receivedPID   = ep0BDTOut.control.a.bdtkpid; // PID 

   if (endPoint == 0) { // Accept IN, OUT or SETUP token
      if (receivedPID == SETUPToken) // SETUP transaction complete
         handleSetupToken();
      else if (directionIsIn)  // IN Transaction complete
         handleEp0InToken();
      else // OUT Transaction
         handleEp0OutToken();
      return;
   }
   else if (endPoint == 1) { // Accept OUT token
      if (!directionIsIn)  // OUT Transaction complete
         handleEp1OutToken();
      return;
   }
   else if (endPoint == 2) { // Accept IN token
      if (directionIsIn)  // IN Transaction complete
         handleEp2InToken();
      return;
   }
}

#pragma MESSAGE DISABLE C4003
//==================================================================
// Handler for Start of Frame Token interrupt
//
void handleSOFToken(void) {
static U8 flashCount = 0;

   asm {
      lda FRMNUMH
      sta frameNum
      lda FRMNUML
      sta frameNum:1
   }
   // Green LED
   // Off                     - no USB activity, not connected
   // On                      - no USB activity, connected
   // Off, flash briefly on   - USB activity, not connected
   // On,  flash briefly off  - USB activity, connected
   if (FRMNUML==0) { // Every ~256 ms
      switch (flashCount++) {
         case 0:
            if (deviceState.state == USBconfigured)
               GREEN_LED_ON(); // Green LED on when USB connection established
            else
               GREEN_LED_OFF(); // Green LED off when no USB connection
            break;
         case 1:
         case 2:
            break;
         case 3:
            if (usbActivityFlag) { // Greeen LED flashes on USB activity
               GREEN_LED_TOGGLE();
               usbActivityFlag = 0;
            }
            break;            
         default :
            flashCount = 0;
            break;
         }
   }
}
#pragma MESSAGE DEFAULT C4003

//==================================================================
// Handler for USB Suspend
//
// * Enables the USB module to wakeup the CPU 
// * Stops the CPU
// On wakeup
// * Re-checks the USB after a small delay to avoid wakeups by noise
//
void handleUSBSuspend(void) {

   INTSTAT_SLEEPF    = 1;   // Clear the sleep int flag                    
   // Need to disable BDM interface & everything else to reduce power
   bdm_suspend();
   deviceState.state = USBsuspended;

   // A re-check loop is used here to ensure USB bus noise doesn't wakeup the CPU
   for(;;) {
      INTSTAT = INTSTAT_RESUMEF_MASK;  // Clear resume int flag
      INTENB_RESUME     = 1;           // Enable resume detection interrupts on the USB bus
      USBCTL0_USBRESMEN = 1;           // Allow the USB module to wakeup the CPU
     
      asm ("stop");                    // Processor stop for low power
      // The CPU has woken up!

      INTSTAT = INTSTAT_RESUMEF_MASK;  // Clear resume int flag
      asm { // Wait 3 us
         ldhx     #(3*BUS_FREQ)/(4*1000)
      Loop:
         dbnzx    Loop  ; [4]
      }
      // We should have another resume int by now
      if (INTSTAT&INTSTAT_RESUMEF_MASK)
         break;
   }
   USBCTL0_USBRESMEN = 0;  

   return;
}

//==================================================================
// Handler for USB Resume
// 
// Disables further USB module wakeups
void handleUSBResume(void){
   INTENB_RESUME = 0;       // Mask further resume ints
   CTL_TSUSPEND  = 0;	    // Enable the transmit or receive of packets
   deviceState.state = USBconfigured;

   // Set up to receive setup packet
   ep0State.state = EPIdle;
   ep0ConfigureSetupTransaction();
 
   // power up BDM interface?
}

//==================================================================
// Handler for USB interrupt
// 
// Determines source and dispatches to appropriate routine.
//
interrupt //VectorNumber_Vusb 
void USBInterruptHandler( void ) {
U8 interruptFlags = INTSTAT;

   if((USBCTL0_LPRESF) && (deviceState.state==USBsuspended)) {
      USBCTL0_USBRESMEN = 0;
   }

   if ((interruptFlags&INTSTAT_RESUMEF_MASK) != 0) { // Resume signalled on Bus?
      handleUSBResume();
      INTSTAT = INTSTAT_RESUMEF_MASK; // Clear source
   }
   else if ((interruptFlags&INTSTAT_USBRSTF_MASK) != 0) {
      handleUSBReset();
      INTSTAT = INTSTAT_USBRSTF_MASK; // Clear source
   }
   else if ((interruptFlags&INTSTAT_TOKDNEF_MASK) != 0) { // Token complete int?
      U8 status = STAT;               // Snapshot status register
      handleTokenComplete(status);
      usbActivityFlag = 1;
      INTSTAT = INTSTAT_TOKDNEF_MASK; // Clear source
   }
   else if ((interruptFlags&INTSTAT_STALLF_MASK) != 0) { // Stall sent?
      handleEp0StallComplete();
      INTSTAT = INTSTAT_STALLF_MASK; // Clear source
   }
   else if ((interruptFlags&INTSTAT_SOFTOKF_MASK) != 0) { // SOF Token?
      handleSOFToken();
      INTSTAT = INTSTAT_SOFTOKF_MASK; // Clear source
   }
   else if ((interruptFlags&INTSTAT_SLEEPF_MASK) != 0) { // Bus Idle 3ms? => sleep
      handleUSBSuspend();
      INTSTAT = INTSTAT_SLEEPF_MASK; // Clear source
   }
   else  // unexpected int
      INTSTAT = interruptFlags; // Clear & ignore
}

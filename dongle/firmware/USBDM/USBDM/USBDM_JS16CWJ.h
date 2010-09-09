/*! @file
    @brief This file contains hardware specific information and configuration.
    
    USBDM - Universal BDM, JMxx Version \n
    Combined TBDML/OSBDM + extensions, see 
    @htmlonly <a href="USBDM_JMxxCLD.pdf">schematic</a> @endhtmlonly \n
    Supports HC12, HCS08, RS08 and Coldfire V1 targets \n

    @note DO NOT CHANGE THIS FILE \n
    If you need to create another configuration make a copy of this file
    under a new name and change Configure.h appropropriately.
*/

#ifndef _CONFIGURE_H_
#define _CONFIGURE_H_

#ifndef PLATFORM
#define PLATFORM USBDM   // Choose BDM emulation
#endif

//=================================================================================
// Debug pin - used to check timing and hardware sequences etc.
//
#if (DEBUG != 0)
#define DEBUG_PIN_DDR PTBDD_PTBDD1
#define DEBUG_PIN_PER PTBPE_PTBPE1
#define DEBUG_PIN     PTBD_PTBD1
#endif

//=================================================================================
// ICP pin - used to force ICP in bootstrap code
//
#define ICP_PIN_DDR DEBUG_PIN_DDR
#define ICP_PIN_PER DEBUG_PIN_PER
#define ICP_PIN     DEBUG_PIN

//==========================================================================================
// Capabilities of the hardware - used to enable/disable appropriate code
//
#define CAPABILITY (CAP_RESET)


//===========================================================================================
// Type of BDM interface chips are supported

#define DRIVER  LVC125  // Choose driver IC being used

#ifndef DRIVER
#error "Please define DRIVER in Configure.h"
#define DRIVER LVC125
#endif

#if ((DRIVER!=LVC125) && (DRIVER!=LVC45))
#error "Please correctly define DRIVER in Configure.h"
#define DRIVER LVC125
#endif


//=================================================================================
// Port Pin assignments
// Please note: some pin assignments cannot be changed freely
// RX AND TX ROUTINES ARE DEPENDENT ON THIS SPECIFIC ASSIGNMENTS
//


#define ALLPST_IS_HIGH     (0)
#define BKPT_LOW()         ; // Dummy 

//=================================================================================
// HCS08, HCS12, RS08 & Coldfire V1 BDM Mode Port Bit masks
//
#define DATA_PORT          PTAD
#define DATA_PORT_DDR      PTADD
#define DATA_PORT_PER      PTAPE

// BDM data out pin - hard coded in Rx/Tx routines
#define BDM_OUT           PTAD_PTAD7
#define BDM_OUT_BIT       (7)  // Bit number!
#define BDM_OUT_MASK      (1<<BDM_OUT_BIT)

#define BDM_OUT_PER       PTAPE_PTAPE7

// BDM data in pin
#define BDM_IN            PTAD_PTAD0
#define BDM_IN_BIT        (0)  // Bit number!
#define BDM_IN_MASK       (1<<BDM_IN_BIT)

// Primary BDM data direction pin - only disabled during Tx routines
#define BDM_DIR_Rx        PTAD_PTAD1
#define BDM_DIR_Rx_BIT    (1)  // Bit number!
#define BDM_DIR_Rx_MASK   (1<<BDM_DIR_Rx_BIT)

#define BDM_DIR_Rx_PER    PTAPE_PTAPE1

// Polarity of BDM buffer enable/direction varies with driver IC
#if (DRIVER == LVC125)
#define BDM_DIR_Rx_RD     BDM_DIR_Rx_MASK
#define BDM_DIR_Rx_WR     0
#define BDM_ENABLE_ASM_RX BCLR BDM_DIR_Rx_BIT,DATA_PORT
#define BDM_3STATE_ASM_RX BSET BDM_DIR_Rx_BIT,DATA_PORT

#elif (DRIVER == LVC45)
#define BDM_DIR_Rx_RD     0
#define BDM_DIR_Rx_WR     BDM_DIR_Rx_MASK
#define BDM_ENABLE_ASM_RX BSET BDM_DIR_Rx_BIT,DATA_PORT
#define BDM_3STATE_ASM_RX BCLR BDM_DIR_Rx_BIT,DATA_PORT
#endif

//=================================================================================
// Direction Port bit masks
//
#define DIR_PORT          PTBD
#define DIR_PORT_DDR      PTBDD
#define DIR_PORT_PER      PTBPE

// Secondary BDM data direction pin - only enabled during Tx routines
#define BDM_DIR           PTBD_PTBD0
#define BDM_DIR_BIT       (0) // Bit #
#define BDM_DIR_MASK      (1<<BDM_DIR_BIT)

#define BDM_DIR_DDR       PTBDD_PTBDD0
#define BDM_DIR_PER       PTBPE_PTBPE0

// Polarity of BDM buffer enable/direction varies with driver IC
// Following MACROs only used by Tx related routines
// i.e. when BKGD is controlled through DIR_PORT
#if (DRIVER == LVC125)
#define BDM_DIR_RD        BDM_DIR_MASK
#define BDM_DIR_WR        0
#define BDM_ENABLE_TX()   (BDM_DIR=0)
#define BDM_3STATE_TX()   (BDM_DIR=1)
#define BDM_ENABLE_ASM_TX BCLR BDM_DIR_BIT,DIR_PORT
#define BDM_3STATE_ASM_TX BSET BDM_DIR_BIT,DIR_PORT
#elif (DRIVER == LVC45)
#define BDM_DIR_RD        0
#define BDM_DIR_WR        BDM_DIR_MASK
#define BDM_ENABLE_TX()   (BDM_DIR=1)
#define BDM_3STATE_TX()   (BDM_DIR=0)
#define BDM_ENABLE_ASM_TX BSET BDM_DIR_BIT,DIR_PORT
#define BDM_3STATE_ASM_TX BCLR BDM_DIR_BIT,DIR_PORT
#endif

//=================================================================================
// RESET control & sensing
//
#if (CAPABILITY&CAP_RESET)

// RESET output pin
#define RESET_OUT           PTAD_PTAD4
#define RESET_OUT_DDR       PTADD_PTADD4
#define RESET_OUT_PER       PTAPE_PTAPE4
#define RESET_LOW()         (RESET_OUT=0,RESET_OUT_DDR=1)
#define RESET_3STATE()      (RESET_OUT=1,RESET_OUT_DDR=0)

// RESET input pin
#define RESET_IN            PTAD_PTAD3
#define RESET_IN_DDR        PTADD_PTADD3
#define RESET_IN_PER        PTAPE_PTAPE3
#define RESET_IN_MASK       (1<<3)

#define RESET_IS_HIGH       (RESET_IN!=0)

#endif // CAP_RESET

//=================================================================================
// LED Port bit masks
//
#define GREEN_LED_MASK  (PTBD_PTBD3_MASK)
#define RED_LED_MASK    (0) // No red LED!
#define LED_PORT_DATA   (PTBD)
#define LED_PORT_DDR    (PTBDD)

// LEDs off, LED pins are outputs 
#define LED_INIT()         ((LED_PORT_DATA |= RED_LED_MASK|GREEN_LED_MASK), \
                            (LED_PORT_DDR  |= RED_LED_MASK|GREEN_LED_MASK))
#define GREEN_LED_ON()     (LED_PORT_DATA &=~GREEN_LED_MASK)
#define GREEN_LED_OFF()    (LED_PORT_DATA |= GREEN_LED_MASK)
#define GREEN_LED_TOGGLE() (LED_PORT_DATA ^= GREEN_LED_MASK)
#if RED_LED_MASK == 0
#define RED_LED_ON()       ;
#define RED_LED_OFF()      ;
#define RED_LED_TOGGLE()   ;
#else
#define RED_LED_ON()       (LED_PORT_DATA &=~RED_LED_MASK)
#define RED_LED_OFF()      (LED_PORT_DATA |= RED_LED_MASK)
#define RED_LED_TOGGLE()   (LED_PORT_DATA ^= RED_LED_MASK)
#endif
//=================================================================================
// Flash programming control
//
#if (CAPABILITY&CAP_FLASH)

#else // !CAP_FLASH
// No Flash programming supply
#define FLASH12V_ON()   ;
#define FLASH12V_OFF()  ;

#define VPP_ON()        ;
#define VPP_OFF()       ;

#endif //CAP_FLASH

//=================================================================================
// Target Vdd control

#if (CAPABILITY&CAP_VDDCONTROL)

#else // !CAP_VDDCONTROL
// No Vdd control
#define VDD_OFF()       ; // Vdd Off
#define VDD3_ON()       ; // Vdd = 3.3V
#define VDD5_ON()       ; // Vdd = 5V

#endif // CAP_VDDCONTROL

//=================================================================================
// Use of 1k5 resistor on USB D+ line
//
//  The JMxx has a programmable 1k5 pull-up resistor on the USB D+ line.
//
#define USBPUP_ON    (0) // Turn on internal PUP
#define USBPUP_OFF   (1) // Turn off internal PUP

#define USBPUP USBPUP_ON // Internal 1k5 PUP present on D+

#ifndef USBPUP
#error "Please define USBPUP in Configure.h"
#define USBPUP USBPUP_OFF
#endif

#if ((USBPUP != USBPUP_ON) && (USBPUP != USBPUP_OFF))
#error "Please correctly define USBPUP in Configure.h"
#endif


//================================================================================
// Timer Channel use
//
//     TPM-CH0 - BKGD_I pin, SYNC measuring, ACKN detection (Input Capture)
//     TPM-CH1 - Timeouts (Output compare, no pin used)
//

// SYNC Detection TPM.Ch0 : Input capture  (PTA.0)
#define SYNC_TPMxCnSC_CHF                 TPMC0SC_CH0F       // Event Flag
#define SYNC_TPMxCnSC                     TPMC0SC            // TPM Channel Status & Configuration
#define SYNC_TPMxCnSC_RISING_EDGE_MASK    TPMC0SC_ELS0A_MASK // TPMxCnSC value for rising edge
#define SYNC_TPMxCnSC_FALLING_EDGE_MASK   TPMC0SC_ELS0B_MASK // TPMxCnSC value for falling edge
#define SYNC_TPMxCnVALUE                  TPMC0V             // IC Event time

// ACKN uses same channel as SYNC
#define ACKN_TPMxCnSC_CHF                 SYNC_TPMxCnSC_CHF
#define ACKN_TPMxCnSC                     SYNC_TPMxCnSC
#define ACKN_TPMxCnSC_RISING_EDGE_MASK    SYNC_TPMxCnSC_RISING_EDGE_MASK
#define ACKN_TPMxCnSC_FALLING_EDGE_MASK   SYNC_TPMxCnSC_FALLING_EDGE_MASK
#define ACKN_SETUP_ASM                    BCLR 7,ACKN_TPMxCnSC

// Timeout TPM.Ch1 : Output Compare (no pin)
#define TIMEOUT_TPMxCnSC_CHF              TPMC1SC_CH1F       // Event Flag
#define TIMEOUT_TPMxCnSC                  TPMC1SC            // TPM Status & Configuration
#define TIMEOUT_TPMxCnSC_OC_MASK          TPMC1SC_MS1A_MASK  // TPMxCnSC value for OC event
#define TIMEOUT_TPMxCnVALUE               TPMC1V             // OC Event time

#if (CAPABILITY&CAP_VDDSENSE)
// Target Vdd Present
//#define VDD_SENSE                 (ACMPSC_ACO == 0)

#else // !CAP_VDDSENSE

#define VDD_SENSE                 (1) // Assume VDD present
#endif

//================================================================================
// RESET Detection - falling edge using KBI inputs
//
//     KBI     - RESET_IN pin, Reset detection (Keypress falling edge detection)
//
// Configure RESET change sensing (Falling edges)
#define CONFIGURE_RESET_SENSE()   (KBIES &= ~RESET_IN_MASK)
// Enable & Configure RESET Change interrupts
#define ENABLE_RESET_SENSE_INT()  (KBIPE |= RESET_IN_MASK,  \
                                   KBISC = KBISC_KBACK_MASK,\
                                   KBISC = KBISC_KBIE_MASK )
// Clear Reset Change Event
#define CLEAR_RESET_SENSE_FLAG()  (KBISC = KBISC_KBIE_MASK|KBISC_KBACK_MASK)
// Disable  RESET Change interrupts
#define DISABLE_RESET_SENSE_INT() (KBIPE &= ~RESET_IN_MASK)

// Enable & Configure Vdd Change interrupts (Using Analogue comparator, rising or falling edges)
// NOT AVAILABLE
#endif

/*! @file
    @brief This file contains hardware specific information and configuration.
    
    USBDM_CF - Universal BDM/CF, JMxx Version \n
    Combined TBDML/OSBDM/TBLCF + extensions,
             
    @htmlonly <a href="USBDM_CF_JMxxCLD.pdf">schematic</a> @endhtmlonly \n
    Supports HC12, HCS08, RS08 and Coldfire V1, V2, V3 & V4 targets \n

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
#define DEBUG_PIN_DDR PTGDD_PTGDD2
#define DEBUG_PIN_PER PTGPE_PTGPE2
#define DEBUG_PIN     PTGD_PTGD2
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
#define CAPABILITY (CAP_RESET|CAP_FLASH|CAP_VDDCONTROL|CAP_VDDSENSE|CAP_CFVx)


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

//=================================================================================
// Coldfire V2,3,4 BDM Mode Port Bit masks
//
// DSO in pin (MISO1)
#define DSO_IN             PTED_PTED4
#define DSO_IN_BITNUM      (4)
#define DSO_IN_MASK        (1<<DSO_IN_BITNUM)
#define DSO_IN_DDR         PTEDD_PTEDD4
#define DSO_IN_PORT        PTED

// DSI out pin (MOSI1) - DSI, DSCLK must be on the same port 
#define DSI_OUT            PTED_PTED5
#define DSI_OUT_BITNUM     (5) 
#define DSI_OUT_MASK       (1<<DSI_OUT_BITNUM)
#define DSI_OUT_PER        PTEPE_PTEPE5

// DSCLK out pin (SPSCK1) - DSI, DSCLK must be on the same port 
#define DSCLK_OUT          PTED_PTED6
#define DSCLK_OUT_BITNUM   (6)
#define DSCLK_OUT_MASK     (1<<DSCLK_OUT_BITNUM)
#define DSCLK_OUT_PORT     PTED
#define DSCLK_OUT_PER      PTEPE_PTEPE6

#define ASM_DSCLK_HIGH     bset  DSCLK_OUT_BITNUM,DSCLK_OUT_PORT
#define ASM_DSCLK_LOW      bclr  DSCLK_OUT_BITNUM,DSCLK_OUT_PORT

// DSCLK 3-state control
#define DSCLK_DRV            PTFD_PTFD5
#define DSCLK_DRV_DDR        PTFDD_PTFDD5
#define DSCLK_DRV_PER        PTFPE_PTFPE5

#define DSCLK_DRV_ENABLE()   (DSCLK_DRV = 0, DSCLK_DRV_DDR=1)
#define DSCLK_DRV_DISABLE()  (DSCLK_DRV = 1, DSCLK_DRV_DDR=0)

// BKPT out pin
#define BKPT_OUT           PTBD_PTBD0
#define BKPT_OUT_DDR       PTBDD_PTBDD0
#define BKPT_OUT_PER       PTBPE_PTBPE0

#define BKPT_LOW()         (BKPT_OUT=0,BKPT_OUT_DDR=1)
#define BKPT_HIGH()        (BKPT_OUT=1,BKPT_OUT_DDR=0)

// TA out pin 
#define TA_OUT             PTFD_PTFD4
#define TA_OUT_DDR         PTFDD_PTFDD4
#define TA_OUT_PER         PTFPE_PTFPE4

#define TA_LOW()           (TA_OUT=0,TA_OUT_DDR=1)
#define TA_3STATE()        (TA_OUT=1,TA_OUT_DDR=0)
   
// ALLPST in pin 
#define ALLPST_IN          PTCD_PTCD2
#define ALLPST_IN_DDR      PTCDD_PTCDD2
#define ALLPST_IN_PER      PTCPE_PTCPE2

#define ALLPST_IS_HIGH     (ALLPST_IN!=0)

// Masks to make BDM interface idle @ref bdmcf_init
// DSI=0, DSCLK=0 all others undriven (PUPs) 
#define BDMCF_IDLE         (0)
#define BDMCF_IDLE_DDR     (DSI_OUT_MASK|DSCLK_OUT_MASK)

//=================================================================================
// JTAG Mode Port Bit masks
//
// TDI out pin (MOSI1)
#define TDI_OUT             DSI_OUT
#define TDI_OUT_BITNUM	    DSI_OUT_BITNUM
#define TDI_OUT_MASK        DSI_OUT_MASK

#define TDI_HIGH()          TDI_OUT = 1    // Assumes direction already set
#define TDI_LOW()           TDI_OUT = 0

// TDO in pin (MISO)
#define TDO_IN              DSO_IN
#define TDO_IN_BITNUM       DSO_IN_BITNUM
#define TDO_IN_MASK         DSO_IN_MASK
#define TDO_IN_DDR          DSO_IN_DDR

#define TDO_IS_HIGH         (TDO_IN!=0)

// TCLK out pin (SPICK1)
#define TCLK_OUT            DSCLK_OUT
#define TCLK_OUT_BITNUM     DSCLK_OUT_BITNUM
#define TCLK_OUT_MASK       DSCLK_OUT_MASK

#define TCLK_HIGH()         TCLK_OUT = 1   // Assumes direction already set
#define TCLK_LOW()          TCLK_OUT = 0

// TMS out pin (MISO2)
#define TMS_OUT             BKPT_OUT    
#define TMS_OUT_DDR         BKPT_OUT_DDR

#define TMS_HIGH()          (TMS_OUT = 1, TMS_OUT_DDR = 1)
#define TMS_LOW()           (TMS_OUT = 0, TMS_OUT_DDR = 1)

// TRST* out pin
#define TRST_OUT            PTED_PTED3
#define TRST_OUT_DDR        PTFDD_PTFDD3
#define TRST_OUT_PER        PTFPE_PTFPE3

#define TRST_LOW()          (TRST_OUT=0,TRST_OUT_DDR=1)
#define TRST_3STATE()       (TRST_OUT=1,TRST_OUT_DDR=0)

//! Masks to make JTAG interface idle @ref jtag_init 
//! TDI held low, TCLK undriven with PUP
#define JTAG_IDLE           (    0       )
#define JTAG_IDLE_DDR       (TDI_OUT_MASK)

//=================================================================================
// HCS08, HCS12, RS08 & Coldfire V1 BDM Mode Port Bit masks
//
#define DATA_PORT          PTED
#define DATA_PORT_DDR      PTEDD
#define DATA_PORT_PER      PTEPE
// BDM data out pin - hard coded in Rx/Tx routines
#define BDM_OUT           PTED_PTED7
#define BDM_OUT_BIT       (7)  // Bit number!
#define BDM_OUT_MASK      (1<<BDM_OUT_BIT)

#define BDM_OUT_PER       PTEPE_PTEPE7

// BDM data in pin
#define BDM_IN            PTED_PTED0
#define BDM_IN_BIT        (0)  // Bit number!
#define BDM_IN_MASK       (1<<BDM_IN_BIT)

// Primary BDM data direction pin - only disabled during Tx routines
#define BDM_DIR_Rx        PTED_PTED1
#define BDM_DIR_Rx_BIT    (1)  // Bit number!
#define BDM_DIR_Rx_MASK   (1<<BDM_DIR_Rx_BIT)

#define BDM_DIR_Rx_PER    PTEPE_PTEPE1

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
#define DIR_PORT          PTFD
#define DIR_PORT_DDR      PTFDD
#define DIR_PORT_PER      PTFPE

// Secondary BDM data direction pin - only enabled during Tx routines
#define BDM_DIR           PTFD_PTFD5
#define BDM_DIR_BIT       (5) // Bit #
#define BDM_DIR_MASK      (1<<BDM_DIR_BIT)

#define BDM_DIR_DDR       PTFDD_PTFDD5
#define BDM_DIR_PER       PTFPE_PTFPE5

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
#define RESET_OUT           PTCD_PTCD4
#define RESET_OUT_DDR       PTCDD_PTCDD4
#define RESET_OUT_PER       PTCPE_PTCPE4
#define RESET_LOW()         (RESET_OUT=0,RESET_OUT_DDR=1)
#define RESET_3STATE()      (RESET_OUT=1,RESET_OUT_DDR=0)

// RST_IN in pin
#define RESET_IN            PTFD_PTFD1
#define RESET_IN_DDR        PTFDD_PTFDD3
#define RESET_IN_PER        PTFPE_PTFPE3

#define RESET_IS_HIGH       (RESET_IN!=0)

#endif // CAP_RESET

//=================================================================================
// LED Port bit masks
//
#define GREEN_LED_MASK  (PTGD_PTGD0_MASK)
#define RED_LED_MASK    (PTGD_PTGD1_MASK)
#define LED_PORT_DATA   (PTGD)
#define LED_PORT_DDR    (PTGDD)

// LEDs off, LED pins are outputs 
#define LED_INIT()         ((LED_PORT_DATA |= RED_LED_MASK|GREEN_LED_MASK), \
                            (LED_PORT_DDR  |= RED_LED_MASK|GREEN_LED_MASK))
#define GREEN_LED_ON()     (LED_PORT_DATA &=~GREEN_LED_MASK)
#define GREEN_LED_OFF()    (LED_PORT_DATA |= GREEN_LED_MASK)
#define GREEN_LED_TOGGLE() (LED_PORT_DATA ^= GREEN_LED_MASK)
#define RED_LED_ON()       (LED_PORT_DATA &=~RED_LED_MASK)
#define RED_LED_OFF()      (LED_PORT_DATA |= RED_LED_MASK)
#define RED_LED_TOGGLE()   (LED_PORT_DATA ^= RED_LED_MASK)

//=================================================================================
// Flash programming control
//
#if (CAPABILITY&CAP_FLASH)

// Flash 12V charge pump control
#define FLASH12_ENn      (PTBD_PTBD5)
#define FLASH12V_DDR     (PTBDD_PTBDD5)

// Drive transistor for VPP on/off
#define VPP_EN           (PTFD_PTFD0)
#define VPP_EN_DDR       (PTFDD_PTFDD0)

// Order of statements within the following macros may be important 
#define FLASH12V_ON()   (FLASH12_ENn=0, FLASH12V_DDR=1)
#define FLASH12V_OFF()  (FLASH12_ENn=1, FLASH12V_DDR=1)

#define VPP_ON()        (VPP_EN=1,  VPP_EN_DDR=1) // VPP Control pin is output, VPP on
#define VPP_OFF()       (VPP_EN=0,  VPP_EN_DDR=1) // VPP Control pin is output, VPP off 

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

//  Vdd on/off
#define VDD3_EN          (PTBD_PTBD1)
#define VDD3_EN_DDR      (PTBDD_PTBDD1)
#define VDD5_EN          (PTBD_PTBD4)
#define VDD5_EN_DDR      (PTBDD_PTBDD4)

#define VDD_OFF()        (VDD5_EN=0, VDD5_EN_DDR=1, VDD3_EN=0, VDD3_EN_DDR=1) // Vdd Off
#define VDD3_ON()        (VDD5_EN=0, VDD5_EN_DDR=1, VDD3_EN=1, VDD3_EN_DDR=1) // Vdd = 3.3V
#define VDD5_ON()        (VDD3_EN=0, VDD3_EN_DDR=1, VDD5_EN=1, VDD5_EN_DDR=1) // Vdd = 5V

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
//     TPM1-CH3 - RST_I pin, Reset detection (IC falling edge detection)
//     TPM1-CH0 - BKGD_I pin, SYNC measuring, ACKN detection (Input Capture)
//     TPM1-CH2 - Timeouts (Output compare, no pin used)
//
// Use TPM1 as Timer
#define TPMSC              TPM1SC
#define TPMCNTH            TPM1CNTH
#define TPMSC_CLKSA_MASK   TPM1SC_CLKSA_MASK

// RESET Detection TPM1.Ch3 : Interrupt, Input capture, falling edge on pin 
#define RESET_TPMxCnSC_CHIE               TPM1C3SC_CH3IE      // Mask/enable RESET detection
#define RESET_TPMxCnSC_CHF                TPM1C3SC_CH3F       // Event Interrupt Flag
#define RESET_TPMxCnSC                    TPM1C3SC            // TPM Status & Configuration
#define RESET_TPMxCnSC_FALLING_EDGE_MASK  TPM1C3SC_ELS3B_MASK // Falling-edge input capture  

// SYNC Detection TPM1.Ch0 : Input capture
#define SYNC_TPMxCnSC_CHF                 TPM1C0SC_CH0F       // Event Flag
#define SYNC_TPMxCnSC                     TPM1C0SC            // TPM Channel Status & Configuration
#define SYNC_TPMxCnSC_RISING_EDGE_MASK    TPM1C0SC_ELS0A_MASK // TPMxCnSC value for rising edge
#define SYNC_TPMxCnSC_FALLING_EDGE_MASK   TPM1C0SC_ELS0B_MASK // TPMxCnSC value for falling edge
#define SYNC_TPMxCnVALUE                  TPM1C0V             // IC Event time

// ACKN uses same channel as SYNC
#define ACKN_TPMxCnSC_CHF                 SYNC_TPMxCnSC_CHF
#define ACKN_TPMxCnSC                     SYNC_TPMxCnSC
#define ACKN_TPMxCnSC_RISING_EDGE_MASK    SYNC_TPMxCnSC_RISING_EDGE_MASK
#define ACKN_TPMxCnSC_FALLING_EDGE_MASK   SYNC_TPMxCnSC_FALLING_EDGE_MASK
#define ACKN_SETUP_ASM                    BCLR 7,ACKN_TPMxCnSC

// Timeout TPM1.Ch2 : Output Compare (no pin)
#define TIMEOUT_TPMxCnSC_CHF              TPM1C2SC_CH2F       // Event Flag
#define TIMEOUT_TPMxCnSC                  TPM1C2SC            // TPM Status & Configuration
#define TIMEOUT_TPMxCnSC_OC_MASK          TPM1C2SC_MS2A_MASK  // TIMEOUT_TPMxCnSC value for OC event
#define TIMEOUT_TPMxCnVALUE               TPM1C2V             // OC Event time


// Target Vdd Present
#define VDD_SENSE                 (ACMPSC_ACO == 0)

// Configure RESET change sensing (Falling edges)
#define CONFIGURE_RESET_SENSE()   (RESET_TPMxCnSC = RESET_TPMxCnSC_FALLING_EDGE_MASK)
// Enable & Configure RESET Change interrupts
#define ENABLE_RESET_SENSE_INT()  (RESET_TPMxCnSC_CHIE = 1)
// Clear Reset Change Event
#define CLEAR_RESET_SENSE_FLAG()  (RESET_TPMxCnSC_CHF  = 0)
// Disable  RESET Change interrupts
#define DISABLE_RESET_SENSE_INT() (RESET_TPMxCnSC_CHIE = 0)

// Enable & Configure Vdd Change interrupts (Using Analogue comparator, rising or falling edges)
#define CONFIGURE_VDD_SENSE()     (ACMPSC = ACMPSC_ACME_MASK|ACMPSC_ACBGS_MASK|ACMPSC_ACMOD1_MASK|ACMPSC_ACMOD0_MASK)
// Enable Vdd Change interrupts
#define ENABLE_VDD_SENSE_INT()    (ACMPSC_ACIE = 1) 
// Clear Vdd Change Event
#define CLEAR_VDD_SENSE_FLAG()    (ACMPSC_ACF = 1)
// Disable Vdd Change interrupts
#define DISABLE_VDD_SENSE_INT()   (ACMPSC_ACIE = 0) 

#endif

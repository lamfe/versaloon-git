#include "app_cfg.h"

#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#include "BDM.h"

#include "Commands.h"
#include "BDM_USBDM.h"

//==========================================================================================
// Capabilities of the hardware - used to enable/disable appropriate code
//
#define CAPABILITY (CAP_RESET|CAP_VDDCONTROL|CAP_VDDSENSE)

//=================================================================================
// RESET control & sensing
//
// RESET output pin
#define RESET_LOW()         do{\
								RST_CLR();\
								RST_SETOUTPUT();\
							} while (0)
#define RESET_3STATE()      RST_SETINPUT()

#define RESET_IN			RST_GET()
#define RESET_IS_HIGH       (RESET_IN!=0)

#define VDD_OFF()           PWREXT_ForceRelease() // Vdd Off
#define VDD3_ON()           PWREXT_Acquire() // Vdd = 3.3V
#define VDD5_ON()           // Vdd = 5V

#define CONFIGURE_VDD_SENSE()
#define DISABLE_VDD_SENSE_INT()
#define CLEAR_VDD_SENSE_FLAG()
#define ENABLE_VDD_SENSE_INT()

#define CONFIGURE_RESET_SENSE()
#define CLEAR_RESET_SENSE_FLAG()
#define ENABLE_RESET_SENSE_INT()
#define DISABLE_RESET_SENSE_INT()

//#define BKPT_LOW()         (JTAG_TAP_TMS_CLR(),JTAG_TAP_TMS_SETOUTPUT())
//#define BKPT_HIGH()        (JTAG_TAP_TMS_SETINPUT())

#define BDM_3STATE_TX()		BDM_SET()
#define BDM_IN				BDM_GET()

#define RED_LED_ON()		
#define RED_LED_OFF()		



// BDM functions
U8 BDM_CMD_0_0_T(U8 cmd);
U8 BDM_CMD_1W1B_0_T(U8 cmd, U16 parameter1, U8 parameter2);
U8 BDM_CMD_1B_0_T(U8 cmd, U8 parameter);
U8 BDM_CMD_0_0_NOACK(U8 cmd);
U8 BDM_CMD_0_1B_NOACK(U8 cmd, void *result);
U8 BDM_CMD_1B_0_NOACK(U8 cmd, U8 parameter);
U8 BDM_CMD_0_1W_NOACK(U8 cmd, void *parameter);
U8 BDM_CMD_1W_0_NOACK(U8 cmd, U16 parameter);
U8 BDM_CMD_0_0(U8 cmd);
U8 BDM_CMD_1W_0(U8 cmd, U16 parameter);
U8 BDM_CMD_0_1W(U8 cmd, void *parameter);
U8 BDM_CMD_1L_0(U8 cmd, U32 parameter);
U8 BDM_CMD_0_1L(U8 cmd, void *parameter);
U8 BDM_CMD_1W_1WB(U8 cmd, U16 parameter, void *result);
U8 BDM_CMD_2W_0(U8 cmd, U16 parameter1, U16 parameter2);
U8 BDM_CMD_2WB_0(U8 cmd, U16 parameter1, U8 parameter2);
U8 BDM_CMD_1W_1W(U8 cmd, U16 parameter, void *result);
U8 BDM_CMD_1Wle_1Wle(U8 cmd, U16 parameter, void *result);
U8 BDM_CMD_0_1B(U8 cmd, void *result);
U8 BDM_CMD_1B_0(U8 cmd, U8 parameter);
U8 BDM_CMD_1W_1B(U8 cmd, U16 parameter, void *result);
U8 BDM_CMD_1Wle_1B(U8 cmd, U16 parameter, void *result);
U8 BDM_CMD_1W1B_0(U8 cmd, U16 parameter1, U8 parameter2);
U8 BDM_CMD_1A1B_0(U8 cmd, U32 addr, U8 value);
U8 BDM_CMD_1A1W_0(U8 cmd, U32 addr, U16 value);
U8 BDM_CMD_1A1L_0(U8 cmd, U32 addr, U32 value);
U8 BDM_CMD_1A_1B(U8 cmd, U32 addr, void *result);
U8 BDM_CMD_1A_1W(U8 cmd, U32 addr, void *result);
U8 BDM_CMD_1A_1L(U8 cmd, U32 addr, void *result);

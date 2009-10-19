#ifndef __CM3_COMMON_H_INCLUDED__
#define __CM3_COMMON_H_INCLUDED__

#define CM3_CPUID					0xE000ED00
/* Debug Control Block */
#define CM3_DCB_DHCSR				0xE000EDF0
#define CM3_DCB_DCRSR				0xE000EDF4
#define CM3_DCB_DCRDR				0xE000EDF8
#define CM3_DCB_DEMCR				0xE000EDFC

/* DCB_DHCSR bit and field definitions */
#define CM3_DCB_DHCSR_DBGKEY		(0xA05F << 16)
#define CM3_DCB_DHCSR_C_DEBUGEN		(1 << 0)
#define CM3_DCB_DHCSR_C_HALT		(1 << 1)
#define CM3_DCB_DHCSR_C_STEP		(1 << 2)
#define CM3_DCB_DHCSR_C_MASKINTS	(1 << 3)
#define CM3_DCB_DHCSR_S_REGRDY		(1 << 16)
#define CM3_DCB_DHCSR_S_HALT		(1 << 17)
#define CM3_DCB_DHCSR_S_SLEEP		(1 << 18)
#define CM3_DCB_DHCSR_S_LOCKUP		(1 << 19)
#define CM3_DCB_DHCSR_S_RETIRE_ST	(1 << 24)
#define CM3_DCB_DHCSR_S_RESET_ST	(1 << 25)

RESULT cm3_dp_parameter_init(adi_dp_if_t *dp);
RESULT cm3_dp_fini(void);
RESULT cm3_dp_init(programmer_info_t *prog, adi_dp_if_t *interf);

RESULT cm3_dp_halt(void);
RESULT cm3_dp_run(void);

uint32 cm3_get_max_block_size(uint32 address);

#endif	// __CM3_COMMON_H_INCLUDED__


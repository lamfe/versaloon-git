#ifndef __STM32_CORE_H_INCLUDED__
#define __STM32_CORE_H_INCLUDED__

enum stm32_clksrc_t
{
	STM32_CLKSRC_HSE = 1,
	STM32_CLKSRC_HSI = 0,
	STM32_CLKSRC_PLL = 2
};
enum stm32_pllsrc_t
{
	STM32_PLLSRC_HSE,
	STM32_PLLSRC_HSEd2,
	STM32_PLLSRC_HSId2
};
enum stm32_rtcsrc_t
{
	STM32_RTCSRC_HSEd128,
	STM32_RTCSRC_LSE,
	STM32_RTCSRC_LSI
};
enum stm32_hse_type_t
{
	STM32_HSE_TYPE_CLOCK,
	STM32_HSE_TYPE_CRYSTAL
};
enum debug_setting_t
{
	STM32_DBG_JTAG_SWD = 0,
	STM32_DBG_SWD = 2,
	STM32_DBG_NONE = 4
};
struct stm32_info_t
{
	enum stm32_clksrc_t clksrc;
	enum stm32_pllsrc_t pllsrc;
	enum stm32_rtcsrc_t rtcsrc;
	enum stm32_hse_type_t hse_type;
	
	uint32_t osc_freq_hz;
	uint32_t pll_freq_hz;
	uint32_t ahb_freq_hz;
	uint32_t apb1_freq_hz;
	uint32_t apb2_freq_hz;
	
	uint8_t flash_latency;
	uint32_t vector_table;
	enum debug_setting_t debug_setting;
	
	// calculated internally
	uint32_t sys_freq_hz;
};

RESULT stm32_interface_init(void *p);
RESULT stm32_interface_fini(void);
void stm32_interface_reset(void);
RESULT stm32_interface_get_info(struct stm32_info_t **info);
RESULT stm32_delay_delayms(uint16_t ms);
RESULT stm32_delay_delayus(uint16_t us);

#endif	// __STM32_CORE_H_INCLUDED__

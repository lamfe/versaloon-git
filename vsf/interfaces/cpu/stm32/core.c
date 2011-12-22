#include "vsf_err.h"

#include "app_cfg.h"
#include "app_type.h"

#include "interfaces_cfg.h"
#include "interfaces_const.h"
#include "core.h"

#define STM32_RCC_CR_HSEON				(1 << 16)
#define STM32_RCC_CR_HSERDY				(1 << 17)
#define STM32_RCC_CR_HSEBYP				(1 << 18)
#define STM32_RCC_CR_PLLON				(1 << 24)
#define STM32_RCC_CR_PLLRDY				(1 << 25)

#define STM32_RCC_CFGR_SW_MSK			0x00000003
#define STM32_RCC_CFGR_HPRE_SFT			4
#define STM32_RCC_CFGR_HPRE_MSK			(0x0F << STM32_RCC_CFGR_HPRE_SFT)
#define STM32_RCC_CFGR_PPRE1_SFT		8
#define STM32_RCC_CFGR_PPRE1_MSK		(0x07 << STM32_RCC_CFGR_PPRE1_SFT)
#define STM32_RCC_CFGR_PPRE2_SFT		11
#define STM32_RCC_CFGR_PPRE2_MSK		(0x07 << STM32_RCC_CFGR_PPRE2_SFT)
#define STM32_RCC_CFGR_PLLSRC			(1 << 16)
#define STM32_RCC_CFGR_PLLXTPRE			(1 << 17)
#define STM32_RCC_CFGR_PLLMUL_SFT		18
#define STM32_RCC_CFGR_PLLMUL_MSK		(0x0F << STM32_RCC_CFGR_PLLMUL_SFT)

#define STM32_FLASH_ACR_PRFTBE			(1 << 4)

#define STM32_RCC_APB2ENR_AFIO			(1 << 0)

#define STM32_AFIO_MAPR_SWJCFG_SFT		24

#define STM32_HSI_FREQ_HZ				(8 * 1000 * 1000)

static struct stm32_info_t stm32_info = 
{
	CORE_CLKSRC, CORE_PLLSRC, CORE_RTCSRC, CORE_HSE_TYPE, OSC0_FREQ_HZ, 
	CORE_PLL_FREQ_HZ, CORE_AHB_FREQ_HZ, CORE_APB1_FREQ_HZ, CORE_APB2_FREQ_HZ, 
	CORE_FLASH_LATENCY, CORE_VECTOR_TABLE, CORE_DEBUG
};

vsf_err_t stm32_interface_get_info(struct stm32_info_t **info)
{
	*info = &stm32_info;
	return VSFERR_NONE;
}

vsf_err_t stm32_interface_fini(void)
{
	return VSFERR_NONE;
}

vsf_err_t stm32_interface_reset(void)
{
	NVIC_SystemReset();
	return VSFERR_NONE;
}

static uint32_t __log2__(uint32_t n)
{
	uint32_t i, value = 1;
	
	for (i = 0; i < 31; i++)
	{
		if (value == n)
		{
			return i;
		}
		value <<= 1;
	}
	return 0;
}

vsf_err_t stm32_interface_init(void *p)
{
	uint32_t tmp32;
	
	if (p != NULL)
	{
		stm32_info = *(struct stm32_info_t *)p;
	}
	
	switch (stm32_info.clksrc)
	{
	case STM32_CLKSRC_HSI:
		stm32_info.sys_freq_hz = STM32_HSI_FREQ_HZ;
		break;
	case STM32_CLKSRC_HSE:
		stm32_info.sys_freq_hz = OSC0_FREQ_HZ;
		break;
	case STM32_CLKSRC_PLL:
		stm32_info.sys_freq_hz = CORE_PLL_FREQ_HZ;
		break;
	}
	
	RCC_DeInit();
	if ((STM32_CLKSRC_HSE == stm32_info.clksrc) || 
		(STM32_PLLSRC_HSE == stm32_info.pllsrc) || 
		(STM32_PLLSRC_HSEd2 == stm32_info.pllsrc) || 
		(STM32_RTCSRC_HSEd128 == stm32_info.rtcsrc))
	{
		RCC->CR |= STM32_RCC_CR_HSEON;
		
		if (STM32_HSE_TYPE_CLOCK == stm32_info.hse_type)
		{
			RCC->CR |= STM32_RCC_CR_HSEBYP;
		}
		else
		{
			RCC->CR &= ~STM32_RCC_CR_HSEBYP;
		}
		
		while (!(RCC->CR & STM32_RCC_CR_HSERDY));
	}
	else
	{
		RCC->CR &= ~STM32_RCC_CR_HSEON;
	}
	
	FLASH->ACR = STM32_FLASH_ACR_PRFTBE | CORE_FLASH_LATENCY;
	RCC->CFGR &= ~(STM32_RCC_CFGR_HPRE_MSK | STM32_RCC_CFGR_PPRE1_MSK | 
					STM32_RCC_CFGR_PPRE2_MSK);
	
	tmp32 = __log2__(stm32_info.sys_freq_hz / stm32_info.ahb_freq_hz);
	if (tmp32)
	{
		RCC->CFGR |= (0x08 | (tmp32 - 1)) << STM32_RCC_CFGR_HPRE_SFT;
	}
	tmp32 = __log2__(stm32_info.sys_freq_hz / stm32_info.apb1_freq_hz);
	if (tmp32)
	{
		RCC->CFGR |= (0x04 | (tmp32 - 1)) << STM32_RCC_CFGR_PPRE1_SFT;
	}
	tmp32 = __log2__(stm32_info.sys_freq_hz / stm32_info.apb2_freq_hz);
	if (tmp32)
	{
		RCC->CFGR |= (0x04 | (tmp32 - 1)) << STM32_RCC_CFGR_PPRE2_SFT;
	}
	
	if (stm32_info.pll_freq_hz)
	{
		RCC->CFGR &= ~(STM32_RCC_CFGR_PLLMUL_MSK | STM32_RCC_CFGR_PLLSRC | 
						STM32_RCC_CFGR_PLLXTPRE);
		switch (stm32_info.pllsrc)
		{
		case STM32_PLLSRC_HSE:
			tmp32 = stm32_info.osc_freq_hz;
			RCC->CFGR |= STM32_RCC_CFGR_PLLSRC;
			break;
		case STM32_PLLSRC_HSEd2:
			tmp32 = stm32_info.osc_freq_hz / 2;
			RCC->CFGR |= STM32_RCC_CFGR_PLLSRC | STM32_RCC_CFGR_PLLXTPRE;
			break;
		case STM32_PLLSRC_HSId2:
			tmp32 = STM32_HSI_FREQ_HZ / 2;
			break;
		}
		tmp32 = stm32_info.pll_freq_hz / tmp32;
#if __VSF_DEBUG__
		if ((tmp32 < 2) || (tmp32 > 16))
		{
			return VSFERR_INVALID_PARAMETER;
		}
#endif
		RCC->CFGR |= ((tmp32 - 2) << STM32_RCC_CFGR_PLLMUL_SFT);
		
		RCC->CR |= STM32_RCC_CR_PLLON;
		while (!(RCC->CR & STM32_RCC_CR_PLLRDY));
	}
	
	RCC->CFGR &= ~STM32_RCC_CFGR_SW_MSK;
	RCC->CFGR |= CORE_CLKSRC;
	while (((RCC->CFGR >> 2) & STM32_RCC_CFGR_SW_MSK) != CORE_CLKSRC);
	
	RCC->APB2ENR |= STM32_RCC_APB2ENR_AFIO;
	AFIO->MAPR |= stm32_info.debug_setting << STM32_AFIO_MAPR_SWJCFG_SFT;
	
	SCB->VTOR = stm32_info.vector_table;
	return VSFERR_NONE;
}



#define CM3_SYSTICK_ENABLE				(1 << 0)
#define CM3_SYSTICK_CLKSOURCE			(1 << 2)
#define CM3_SYSTICK_COUNTFLAG			(1 << 16)

vsf_err_t stm32_delay_init(void)
{
	SysTick->CTRL = CM3_SYSTICK_CLKSOURCE;
	SysTick->VAL = 0;
	return VSFERR_NONE;
}

static vsf_err_t stm32_delay_delayus_do(uint32_t tick)
{
	uint32_t dly_tmp;
	
	while (tick)
	{
		dly_tmp = (tick > ((1 << 24) - 1)) ? ((1 << 24) - 1) : tick;
		SysTick->LOAD = dly_tmp;
		SysTick->CTRL |= CM3_SYSTICK_ENABLE;
		while (!(SysTick->CTRL & CM3_SYSTICK_COUNTFLAG));
		stm32_delay_init();
		tick -= dly_tmp;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_delay_delayus(uint16_t us)
{
	stm32_delay_delayus_do(us * (stm32_info.sys_freq_hz / (1000 * 1000)));
	return VSFERR_NONE;
}

vsf_err_t stm32_delay_delayms(uint16_t ms)
{
	stm32_delay_delayus_do(ms * (stm32_info.sys_freq_hz / 1000));
	return VSFERR_NONE;
}

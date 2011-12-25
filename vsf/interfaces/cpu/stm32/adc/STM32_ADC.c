
#include "app_type.h"
#include "interfaces.h"

// TODO: remove MACROs below to stm32_reg.h
#define STM32_RCC_APB2RSTR_ADC1RST		((uint32_t)1 << 9)
#define STM32_RCC_APB2RSTR_ADC2RST		((uint32_t)1 << 10)

#define STM32_RCC_APB2ENR_ADC1EN		((uint32_t)1 << 9)
#define STM32_RCC_APB2ENR_ADC2EN		((uint32_t)1 << 10)
#define STM32_RCC_APB2ENR_IOPAEN		((uint32_t)1 << 2)
#define STM32_RCC_APB2ENR_IOPBEN		((uint32_t)1 << 3)
#define STM32_RCC_APB2ENR_IOPCEN		((uint32_t)1 << 4)
#define STM32_RCC_APB2ENR_IOPDEN		((uint32_t)1 << 5)

#define STM32_RCC_CFGR_ADCPRE_SFT		14
#define STM32_RCC_CFGR_ADCPRE_MSK		((uint32_t)0x03 << \
											STM32_RCC_CFGR_ADCPRE_SFT)

#define STM32_ADC_SR_STRT				((uint32_t)1 << 4)
#define STM32_ADC_SR_EOC				((uint32_t)1 << 1)

#define STM32_ADC_CR1_DISCEN			((uint32_t)1 << 11)

#define STM32_ADC_CR2_ADON				((uint32_t)1 << 0)
#define STM32_ADC_CR2_CAL				((uint32_t)1 << 2)
#define STM32_ADC_CR2_RSTCAL			((uint32_t)1 << 3)
#define STM32_ADC_CR2_EXTSEL_SWSTART	((uint32_t)3 << 17)
#define STM32_ADC_CR2_SWSTART			((uint32_t)1 << 22)

#define STM32_ADC_SQR1_L_SFT			20
#define STM32_ADC_SQR1_L_MASK			((uint32_t)0x0F << STM32_ADC_SQR1_L_SFT)

#if IFS_ADC_EN

#include "STM32_ADC.h"

#define STM32_ADC_NUM					3

static const ADC_TypeDef *stm32_adcs[STM32_ADC_NUM] = {ADC1, ADC2, ADC3};

vsf_err_t stm32_adc_init(uint8_t index)
{
	ADC_TypeDef *adc;
	
#if __VSF_DEBUG__
	if (index >= STM32_ADC_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	switch (index)
	{
	case 0:
		RCC->APB2RSTR |= STM32_RCC_APB2RSTR_ADC1RST;
		RCC->APB2RSTR &= ~STM32_RCC_APB2RSTR_ADC1RST;
		RCC->APB2ENR |= STM32_RCC_APB2ENR_ADC1EN;
		break;
	case 1:
		RCC->APB2RSTR |= STM32_RCC_APB2RSTR_ADC2RST;
		RCC->APB2RSTR &= ~STM32_RCC_APB2RSTR_ADC2RST;
		RCC->APB2ENR |= STM32_RCC_APB2ENR_ADC2EN;
		break;
	default:
		return VSFERR_NOT_SUPPORT;
	}
	
	adc->CR1 = STM32_ADC_CR1_DISCEN;
	adc->CR2 = 0;
	adc->SQR1 = 0;
	
	return VSFERR_NONE;
}

vsf_err_t stm32_adc_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		RCC->APB2ENR &= ~STM32_RCC_APB2ENR_ADC1EN;
		break;
	case 1:
		RCC->APB2ENR &= ~STM32_RCC_APB2ENR_ADC2EN;
		break;
	default:
		return VSFERR_NOT_SUPPORT;
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_adc_config(uint8_t index, uint32_t clock_hz, uint8_t mode)
{
	ADC_TypeDef *adc;
	struct stm32_info_t *info;
	
	if (stm32_interface_get_info(&info))
	{
		return VSFERR_FAIL;
	}
	
#if __VSF_DEBUG__
	if ((index >= STM32_ADC_NUM) || 
		(	((info->apb2_freq_hz / clock_hz) != 2) && 
		 	((info->apb2_freq_hz / clock_hz) != 4) && 
			((info->apb2_freq_hz / clock_hz) != 6) && 
			((info->apb2_freq_hz / clock_hz) != 8)))
	{
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	RCC->CFGR &= ~STM32_RCC_CFGR_ADCPRE_MSK;
	RCC->CFGR |= (((info->apb2_freq_hz / clock_hz) / 2) - 1) << 
					STM32_RCC_CFGR_ADCPRE_SFT;
	
	adc->CR2 = ((uint32_t)mode << 8) | STM32_ADC_CR2_EXTSEL_SWSTART;
	return VSFERR_NONE;
}

vsf_err_t stm32_adc_config_channel(uint8_t index, uint8_t channel, 
								uint8_t cycles)
{
	ADC_TypeDef *adc;
	uint32_t tmpreg;
	
#if __VSF_DEBUG__
	if (index >= STM32_ADC_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	// channel 0  --  7 : PA0 -- PA7
	// channel 8  --  9 : PB0 -- PB1
	// channel 10 --  15: PC0 -- PC5
	if (channel <= 7)
	{
		RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPAEN;
		GPIOA->CRL = (GPIOA->CRL & ~(0x0F << (channel * 4))) | 
						(uint32_t)stm32_GPIO_ANALOG << (channel * 4);
	}
	else if (channel <= 9)
	{
		RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPBEN;
		GPIOB->CRL = (GPIOB->CRL & ~(0x0F << ((channel - 8) * 4))) | 
						(uint32_t)stm32_GPIO_ANALOG << ((channel - 8) * 4);
	}
	else if (channel <= 15)
	{
		RCC->APB2ENR |= STM32_RCC_APB2ENR_IOPCEN;
		GPIOC->CRL = (GPIOC->CRL & ~(0x0F << ((channel - 10) * 4))) | 
						(uint32_t)stm32_GPIO_ANALOG << ((channel - 10) * 4);
	}
	
	if (channel > 9)
	{
		tmpreg = adc->SMPR1;
		tmpreg = (tmpreg & ~(((uint32_t) 0x07) << (3 * (channel - 10)))) | 
					((cycles >> 5) << (3 * (channel - 10)));
		adc->SMPR1 = tmpreg;
	}
	else
	{
		tmpreg = adc->SMPR2;
		tmpreg = (tmpreg & ~(((uint32_t) 0x07) << (3 * channel))) | 
					((cycles >> 5) << (3 * channel));
		adc->SMPR2 = tmpreg;
	}
	
	return VSFERR_NONE;
}

vsf_err_t stm32_adc_calibrate(uint8_t index, uint8_t channel)
{
	ADC_TypeDef *adc;
	
#if __VSF_DEBUG__
	if (index >= STM32_ADC_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	adc->SQR3 = channel;
	
	adc->CR2 |= STM32_ADC_CR2_ADON;
	adc->CR2 |= STM32_ADC_CR2_RSTCAL;
	while (adc->CR2 & STM32_ADC_CR2_RSTCAL);
	adc->CR2 |= STM32_ADC_CR2_CAL;
	while (adc->CR2 & STM32_ADC_CR2_CAL);
	
	return VSFERR_NONE;
}

vsf_err_t stm32_adc_start(uint8_t index, uint8_t channel)
{
	ADC_TypeDef *adc;
	
#if __VSF_DEBUG__
	if (index >= STM32_ADC_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	adc->SQR3 = channel;
	adc->SR &= ~STM32_ADC_SR_EOC;
	adc->CR2 |= STM32_ADC_CR2_ADON;
	return VSFERR_NONE;
}

vsf_err_t stm32_adc_isready(uint8_t index, uint8_t channel)
{
	ADC_TypeDef *adc;
	
	REFERENCE_PARAMETER(channel);
#if __VSF_DEBUG__
	if (index >= STM32_ADC_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	return (adc->SR & STM32_ADC_SR_EOC) ? VSFERR_NONE : VSFERR_NOT_READY;
}

uint32_t stm32_adc_get(uint8_t index, uint8_t channel)
{
	ADC_TypeDef *adc;
	
	REFERENCE_PARAMETER(channel);
#if __VSF_DEBUG__
	if (index >= STM32_ADC_NUM)
	{
		return 0;
	}
#endif
	adc = (ADC_TypeDef *)stm32_adcs[index];
	
	return adc->DR;
}

#endif

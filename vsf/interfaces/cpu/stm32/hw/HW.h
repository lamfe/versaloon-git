struct stm32_info_t
{
	uint32_t quartz_khz;
	uint32_t kernel_khz;
};
extern const struct stm32_info_t stm32_info;

#define GPIO_CNF_IN_ANALOG			(0x00 << 2)
#define GPIO_CNF_IN_FLOAT			(0x01 << 2)
#define GPIO_CNF_IN_PULL			(0x02 << 2)
#define GPIO_CNF_OUT_PUSHPULL		(0x00 << 2)
#define GPIO_CNF_OUT_OPENDRAIN		(0x01 << 2)
#define GPIO_CNF_OUT_AF_PUSHPULL	(0x02 << 2)
#define GPIO_CNF_OUT_AF_OPENDRAIN	(0x03 << 2)
#define GPIO_MODE_IN				(0x00 << 0)
#define GPIO_MODE_OUT_10M			(0x01 << 0)
#define GPIO_MODE_OUT_2M			(0x02 << 0)
#define GPIO_MODE_OUT_50M			(0x03 << 0)
#define GPIO_INPUT_PULLUP			0x30
#define GPIO_INPUT_PULLDOWN			0x20

#define GPIO_MODE_IN_FLOATING		(GPIO_MODE_IN | GPIO_CNF_IN_FLOAT)
#define GPIO_MODE_AIN				(GPIO_MODE_IN | GPIO_CNF_IN_ANALOG)
#define GPIO_MODE_IPD				(GPIO_MODE_IN | GPIO_CNF_IN_PULL | GPIO_INPUT_PULLDOWN)
#define GPIO_MODE_IPU				(GPIO_MODE_IN | GPIO_CNF_IN_PULL | GPIO_INPUT_PULLUP)
#define GPIO_MODE_OUT_OD			(GPIO_CNF_OUT_OPENDRAIN | GPIO_MODE_OUT_50M)
#define GPIO_MODE_OUT_PP			(GPIO_CNF_OUT_PUSHPULL | GPIO_MODE_OUT_50M)
#define GPIO_MODE_AF_OD				(GPIO_CNF_OUT_AF_OPENDRAIN | GPIO_MODE_OUT_50M)
#define GPIO_MODE_AF_PP				(GPIO_CNF_OUT_AF_PUSHPULL | GPIO_MODE_OUT_50M)

#define GPIO_PIN_0					0
#define GPIO_PIN_1					1
#define GPIO_PIN_2					2
#define GPIO_PIN_3					3
#define GPIO_PIN_4					4
#define GPIO_PIN_5					5
#define GPIO_PIN_6					6
#define GPIO_PIN_7					7
#define GPIO_PIN_8					8
#define GPIO_PIN_9					9
#define GPIO_PIN_10					10
#define GPIO_PIN_11					11
#define GPIO_PIN_12					12
#define GPIO_PIN_13					13
#define GPIO_PIN_14					14
#define GPIO_PIN_15					15
#define GPIO_PIN_GetMask(p)			(((uint32_t)1) << (p))

#define GPIO_SetPins(port, pin)		(port)->BSRR = GPIO_PIN_GetMask(pin)
#define GPIO_ClrPins(port, pin)		(port)->BRR = GPIO_PIN_GetMask(pin)
#define GPIO_GetOutPins(port, pin)	((port)->ODR & GPIO_PIN_GetMask(pin))
#define GPIO_GetInPins(port, pin)	((port)->IDR & GPIO_PIN_GetMask(pin))
void GPIO_SetMode(GPIO_TypeDef* GPIOx, uint8_t pin, uint8_t mode);


void RCC_Configuration(void);
void NVIC_Configuration(void);
void GPIO_Configuration(void);
void ADC_Configuration(void);

extern SPI_InitTypeDef   SPI_InitStructure;
void SPI_Configuration(SPI_TypeDef* SPIx, uint16_t mode, uint16_t brp, 
						uint16_t fb, uint16_t cpol, uint16_t cpha);

void DOVR_Callback(void);
void ERR_Callback(void);
void WKUP_Callback(void);
void SUSP_Callback(void);

s32 USB_Out_IsReady(void);
s32 USB_Out_PollReady(void);
void USB_Out(uint8_t *data, uint32_t len);

extern uint8_t asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
extern uint16_t Vtarget;

void GLOBAL_OUTPUT_Acquire(void);
void GLOBAL_OUTPUT_Release(void);

uint16_t SampleVtarget(void);
void PWREXT_Check(uint8_t b_control_led);

// Delay
void DelayUS(uint32_t);
void DelayMS(uint32_t);

// USART
void USART_IF_Init(void);
void USART_IF_Fini(void);
void USART_IF_Setup(uint32_t baudrate, uint8_t datatype, uint8_t paritytype, uint8_t stopbittype);
void USART_IF_RX_Int(uint8_t dat);

RESULT stm32_interface_init(void *p);
RESULT stm32_interface_fini(void);

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
#define GPIO_PIN_GetMask(p)			(((uint32)1) << (p))

#define GPIO_SetPins(port, msk)		(port)->BSRR = (msk)
#define GPIO_ClrPins(port, msk)		(port)->BRR = (msk)
#define GPIO_GetOutPins(port, msk)	((port)->ODR & (msk))
#define GPIO_GetInPins(port, msk)	((port)->IDR & (msk))
void GPIO_Dir(GPIO_TypeDef* GPIOx, uint8 mode, uint8 pin);


void Sys_Init(void);
void RCC_Configuration(void);
void NVIC_Configuration(void);
void GPIO_Configuration(void);
void ADC_Configuration(void);

void USB_Init_SerialString(uint8 *strSerial, uint16 len);
void CDC_IF_Setup(uint32 baudrate, uint8 datatype, uint8 paritytype, uint8 stopbittype);
void CDC_IF_Fini(void);
void CDC_Process(void);
void CDC_IF_Enable_Int(void);
void CDC_IF_Disable_Int(void);
void CDC_IF_RX_Int(uint8 dat);
void CDC_IF_TX_Int(void);

#if INTERFACE_SPI_EN
extern SPI_InitTypeDef   SPI_InitStructure;
void SPI_Configuration(SPI_TypeDef* SPIx,u16 mode,u16 brp,u16 fb,u16 cpol,u16 cpha);
#endif

void DOVR_Callback(void);
void ERR_Callback(void);
void WKUP_Callback(void);
void SUSP_Callback(void);

s32 USB_Out_IsReady(void);
s32 USB_Out_PollReady(void);
void USB_Out(u8 *data, u32 len);

extern uint8 asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
extern uint16 Vtarget;

void GLOBAL_OUTPUT_Acquire(void);
void GLOBAL_OUTPUT_Release(void);

void PWREXT_Check(uint8 b_control_led);

extern __IO uint32_t rep_len, cmd_len;
extern uint8_t buffer_out[USB_DATA_BUFF_SIZE], *buffer_in;
extern __IO uint32_t count_out, usb_ovf;
extern __IO uint32_t usb_in_data_remain, usb_in_numofpackage;

// Delay
void DelayUS(uint32);
void DelayMS(uint32);

#include "app_cfg.h"
#if USB_PROTOCOL == USB_ST_VCOM

extern __IO uint8_t CDC_enable;
extern FIFO CDC_OUT_fifo;
extern FIFO CDC_IN_fifo;

extern __IO uint8_t CDC_USART_IsBusy;
extern __IO uint8_t CDC_USB_IsBusy;

#define CDC_USART_Out(b)		do{\
									USART_SendData(USART_DEF_PORT, (b));\
								}while(0)

#if USB_TX_DOUBLEBUFFER_EN
#define CDC_USB_Out(b, l)		do{\
									if(GetENDPOINT(ENDP2) & EP_DTOG_RX)\
									{\
										UserToPMABufferCopy((b), ENDP2_TXADDR1, (l));\
										SetEPDblBuf1Count(ENDP2, EP_DBUF_IN, (l));\
									}\
									else\
									{\
										UserToPMABufferCopy((b), ENDP2_TXADDR0, (l));\
										SetEPDblBuf0Count(ENDP2, EP_DBUF_IN, (l));\
									}\
									FreeUserBuffer(ENDP2, EP_DBUF_IN);\
									SetEPTxValid(ENDP2);\
								}while(0)
#else
#define CDC_USB_Out(b, l)		do{\
									UserToPMABufferCopy((b), ENDP2_TXADDR, (l));\
									SetEPTxCount(ENDP2, (l));\
									SetEPTxValid(ENDP2);\
								}while(0)
#endif

#endif  // #if USB_PROTOCOL == USB_ST_VCOM

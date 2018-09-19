# Connecting programmer to target

## VersaloonMini Pinout

STM32|Versaloon
:----|:--------
PB10 |JTAG_TRST
PB15 |JTAG_TDI/SPI_MOSI
PB4/6(/7)*|JTAG_TMS/SWD_SWDIO
PB6  |SWIM/BDM
PB13/3*|JTAG_TCK/SPI_SCK/SWD_SWCLK/C2_C2CK/ICP_PCL/ISSP_SCLK
PA8  |JTAG_RTCK
PB14 |JTAG_TDO/SPI_MISO
PB11 |JTAG_SRST/C2_C2D/ICP_PDA/ISSP_SDATA
PA9  |UART_TXD
PA10 |UART_RXD

> \* Pins are connected

> () Pin probably not used

# Supported interfaces
## JTAG
*    JTAG_TRST
*    JTAG_SRST
*    JTAG_TCK
*    JTAG_TDO
*    JTAG_TDI
*    JTAG_TMS
*    JTAG_RTCK
## SWD
*    SWD_SWCLK
*    SWD_SWDIO
*    Reset: JTAG_SRST
## STM8_SWIM
*    SWIM
*    Reset: JTAG_SRST
## AVR8/S51_ISP
*    SPI_SCK
*    SPI_MOSI
*    SPI_MISO
*    Reset: JTAG_SRST
## C8051F_C2
*    C2_C2CK
*    C2_C2D
## LPC900_ICP
*    ICP_PCL
*    ICP_PDA
*    Reset: JTAG_TRST
## PSoC1_ISSP
*    ISSP_SCLK
*    ISSP_SDATA
*    Reset: JTAG_TRST
## HCS08/HCS12(X)_BDM
*    BDM
*    Reset: JTAG_SRST

-----------------------------
Source: [Versaloon platform - archived](https://web.archive.org/web/20151025183950/http://www.versaloon.com:80/doc/versaloon/doc_versaloon_programmer_platform.html)

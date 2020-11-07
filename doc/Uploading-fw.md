# Uploading the Versaloon firmware to the STM32 Blue Pill

## Requirements
You will need a STM32 programmer (STlink is used here), a Versaloon compatible
board (Blue Pill is used) and a PC.

## Software
Download, build and install [stlink](https://github.com/stlink-org/stlink).

## Connection
STLink|Blue Pill
:-----|:--------
3.3V  |3.3
GND   |GND
SWDIO |DIO
SWCLK |CLK

## Uploading
On the Blue Pill board, set jumpers `BOOT1 = 0`, `BOOT0 = 1`.

Download or compile the Versaloon firmware hex file. `cd` to the directory which contains the firmware file.

```bash
st-flash erase
st-flash --reset --format ihex --flash=128k write Versaloon_GCC-BluePill-0x0.hex
```

If upload succeeds, you can now disconnect the STlink, set `BOOT1 = 0`, `BOOT0 = 0`
and connect the board via USB to your PC to use the Versaloon.



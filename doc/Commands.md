# Using vsprog

## General usage

List connected Versaloon programmers (in debug mode)  
`vsprog -d 2 -L`

## Target Area Char and Target Area String

Every target MCU area (fuse, lock, flash, eeprom, calibration byte...) can be defined as one single character.
A string with these characters is Target Area String.

Target Area Char:

* _u_: Fuse
* _l_: Lock
* _f_: Flash
* _e_: Eeprom or the 2nd flash bank
* _c_: Calibration

## vsprog options

* `-s`: define the series of the target chip

		-s avr8
* `-c`: define the module of the target chip

		-c atmega128
		-c stm32f103c8

* `-t TARGET_AREA_CHAR VALUE`: define the value of the specified target area, no space between TARGET_AREA_CHAR and VALUE

		-t u0xFF8090
* `-F SPEED`: define the speed of the programming interface

		-F 4500
* `-m MODE`: define the mode of the programming interface

		-m i
* `-x ADDR`: define the address to execute after programming

		-x 0x08002000
* `-C COM_ATTR`: define the comm port attribute, see below

		-C COM4
		-C COM4_115200
		-C COM4_115200_8N1
		-C /dev/ttyACM0_115200_8O1   
		-C usbtocomm_115200_8E1

* `-J UNITS_BEFORE UNITS_AFTER BITS_BEFORE BITS_AFTER`: define the daisy chain of JTAG interface

		-J 1 0 4 0
* `-K KERNEL_FREQ`: define the frequency of kernel

		-K 60
* `-W WAIT_STATE`: define the wait state.

		-W 2
* `-A`: define the auto-adjust feature.

		-A
* `-I INPUT_FILE_NAME[@SEG,ADDR]`: define the input file.

		-I test.hex
		-I /home/project_avr_eeprom.bin@2,0
		-I cpld_programming.svf

* `-O OUTPUT_FILE_NAME[@SEG,ADDR]`: define the output file.

		-O readout.hex
		-O avr_eeprom.bin@2,0
* `-o <e|w|v|r>[TARGET_AREA_STRING]`: define the operation to perform on target.
	* e - erase operation
	* w - write operation
	* v - verify operation
	* r - read operation

	If `TARGET_AREA_STRING` is not defined, all valid target areas are 			sellected.

		-o e -o wful -o vful

-----------------------------------------
## STM32 support (ISP/JTAG/SWD)
- Vsprog can support STM32 in JTAG/SWD/COM ISP interfaces.
- Auto-detect feature is supported on STM32.
- Supported target area: flash('f').

### Available options for STM32:
* `-s stm32f1, stm32f2, stm32f4, stm32l1`
* `-c <stm32_ld | stm32_md | stm32_hd | stm32_conn | stm32_vl>`
* `-m <i | j | s>`: i for ISP, j for JTAG(default), s for SWD.
* `-C`: valid under ISP mode
* `-F`: valid under JTAG mode, defines the frequency of JTAG in kHz, default is 8000 / 6 kHz
* `-W`: valid under SWD mode, defines the delay of SWD signal, default is 0
* `-x`

### examples:
	vsprog -sstm32f2 -mj -F9000 -I flash.hex -oe -owf -ovf
	vsprog -cstm32f1_ld -ms -W0 -I flash.hex -oe -owf -ovf
	vsprog -sstm32f1 -mi -CCOM4 -I flash.hex -oe -owf -ovf


## LPC1000 support (ISP/JTAG/SWD)
- Vsprog can support LPC1000 in JTAG/SWD interfaces, including LPC11xx CortexM0.
- Auto-detect feature is supported on LPC1000.
- Supported target area: flash('f').

### Available options for LPC1000:
*    `-s lpc1000`
*    `-c`
*    `-m <i | j | s>`: i for ISP, j for JTAG(default), s for SWD.
*    `-K`: defines the kernel frequency in kHz, default is 12000.
*    `-C`: valid under ISP mode
*    `-F`: valid under JTAG mode, defines the frequency of JTAG in kHz, default is 4000 / 6 kHz
*    `-W`: valid under SWD mode, defines the delay of SWD signal, default is 0
*    `-A`: calculate the checksum for 7 vectors.
*    `-x`

### examples:
	vsprog -slpc1000 -A -K 60000 -mj -F9000 -I flash.hex -oe -owf -ovf
	vsprog -clpc1766 -A -ms -W0 -I flash.hex -oe -owf -ovf
	vsprog -clpc1114 -A -mi -CCOM4 -I flash.hex -oe -owf -ovf

## LM3S support (JTAG/SWD)
- Vsprog can support LM3S in JTAG/SWD interfaces. This support needs further test.
- Auto-detect feature is supported on LM3S.
- Supported target area: flash('f').

### Available options for LM3S:
*    `-s lm3s`
*    `-c`
*    `-m <j | s>`: j for JTAG(default), s for SWD.
*    `-F`: valid under JTAG mode, defines the frequency of JTAG in kHz, default is 12000 / 6 kHz
*    `-W`: valid under SWD mode, defines the delay of SWD signal, default is 0
*    `-x`

### examples:
	vsprog -slm3s -mj -F9000 -I flash.hex -oe -owf -ovf
	vsprog -clm3s101 -ms -W0 -I flash.hex -oe -owf -ovf

## AT91SAM3 support (JTAG/SWD)
- Vsprog can support AT91SAM3 in JTAG/SWD interfaces.
- Auto-detect feature is supported on AT91SAM3.
- Supported target area: flash plane 0('f') and 1('e').

### Available options for AT91SAM3:
*    `-s at91sam3`
*    `-c`
*    `-m <j | s>`: j for JTAG(default), s for SWD.
*    `-F`: valid under JTAG mode, defines the frequency of JTAG in kHz, default is 12000 / 6 kHz
*    `-W`: valid under SWD mode, defines the delay of SWD signal, default is 0
*    `-x`

### examples:
	vsprog -sat91sam3 -mj -F9000 -I flash.hex -oe -owfe -ovfe

## STM8 support (SWIM)
- Vsprog can support STM8 in SWIM interface.
- Auto-detect feature is NOT supported on STM8
- Supported target area: flash('f'), fuse('u'). Note, for STM8, lock(ROP) is defined in fuse. MTU of STM8A is also supported.

### Available options for STM8:
*    `-c`
*    `-m <s>`: s for SWIM(default).
*    `-t uFUSE`: define fuse of STM8.

### examples:
	vsprog -cstm8s103f2 -tu0x00000000080000AA -I flash.hex -oe -owfu -ovfu

## AT89S5X support (ISP)
- Vsprog can support AT89S5X in ISP interface.
- Auto-detect feature is supported on AT89S5X.
- Supported target area: flash('f'), lock('l').

### Available options for AT89S5X:
*    `-s at89s5x`
*    `-c`
*    `-m <b | p>`: p for page mode(default), b for byte mode.
*    `-F`: defines the frequency of ISP in kHz, default is 560 kHz
*    `-t lLOCK`: defines lock(from 1 to 4).

### examples:
	vsprog -sat89s5x -tl4 -I flash.hex -oe -owfl -ovfl

## PSOC1 support (ISSP)
- Vsprog can support PSoC1 in ISSP interface.
- Auto-detect feature is supported on PSoC1.
- Supported target area: flash('f'), secure('l').

### Available options for PSOC1:
*    `-s psoc1`
*    `-c`
*    `-m <r | p>`: r for reset mode(default), p for power-on mode.

### examples:
	vsprog -spsoc1 -I flash.hex -oe -owfl -ovfl

## MSP430 (without TEST) support  (JTAG)
- Vsprog can support MSP430 (without TEST pin) in JTAG interface.
- Auto-detect feature is supported on MSP430.
- Supported target area: flash('f').

### Available options for MSP430:
*    `-s msp430`
*    `-c`
*    `-m <j>`: j for JTAG mode(default).

### examples:
	vsprog -smsp430 -I flash.hex -oe -owf -ovf

## C8051F support (C2/JTAG)
- Vsprog can support C8051F in JTAG/C2 interfaces.
- Auto-detect feature is supported on C8051F.
- Supported target area: flash('f').

### Available options for C8051F:
*    `-s c8051f`
*    `-c`
*    `-m <c | j>`: c for C2 mode, j for JTAG mode(default).
*    `-F`: valid under JTAG mode, defines the frequency of JTAG  in kHz, default is 4500

### examples:
	vsprog -sc8051f -mc -I flash.hex -oe -owf -ovf
	vsprog -sc8051f -mj -F2000 -I flash.hex -oe -owf -ovf

## AVR8 support (ISP/JTAG)
- Vsprog can support AVR8 in ISP/JTAG interfaces.
- Auto-detect feature is supported on AVR8.
- Supported target area: flash('f'), lock('l'), fuse('u'), calibration('c', read-only).

### Available options for AVR8:
*    `-s avr8`
*    `-c`
*    `-m <i | j>`: i for ISP mode(default), j for JTAG mode.
*    `-F`: valid under JTAG mode, defines the frequency of ISP/JTAG  in kHz, default for ISP is 560, default for JTAG is 4500
*    `-t`: defines the lock or fuse

### examples:
	vsprog -savr8 -mi -F 1000 -tl0xC0 -I flash.hex -oe -owfl -ovfl
	vsprog -catmega128 -mj -F2000 -tu0xFF80FF -I flash.hex -oe -owfu -ovfu

## LPC900 support (ICP)
- Vsprog can support LPC900 in ICP interfaces.
- Auto-detect feature is supported on LPC900.
- Supported target area: flash('f').

### Available options for LPC900:
*    `-s lpc900`
*    `-c`

### examples:
	vsprog -slpc900 -I flash.hex -oe -owf -ovf

## HCS08 support (BDM)
- Vsprog can support HCS08 in BDM interface.
- Auto-detect feature is supported on HCS08.
- Supported target area: flash('f').

### Available options for HCS08:
*    `-s hcs08`
*    `-c`
*    `-m <b>`: b for BDM mode(default)

### examples:
	vsprog -shcs08 -I flash.hex -oe -owf -ovf

## HCS12(X) support (BDM)
- Vsprog can support HCS12(X) in BDM interfaces.
- Auto-detect feature is *NOT* supported on HCS12(X).
- Supported target area: flash('f').

### Available options for HCS12(X):
*    `-c`
*    `-m <b>`: b for BDM mode(default)

### examples:
	vsprog -cmc9s12xs128 -I flash.hex -oe -owf -ovf

## SVF support (JTAG)
- Vsprog can support SVF player.

### Available options for SVF_PLAYER:
*    `-s svf_player`
*    `-F`: defines the frequency of JTAG  in kHz
*    `-I`: defines the svf file

### examples:
	vsprog -ssvf_player -F18000 -I test.svf

-----------------------------------------
## Sources
[Versaloon script - archived](https://web.archive.org/web/20151107061107/http://www.versaloon.com:80/doc/versaloon/doc_versaloon_script.html)

[Versaloon platform - archived](https://web.archive.org/web/20151025183950/http://www.versaloon.com:80/doc/versaloon/doc_versaloon_programmer_platform.html)

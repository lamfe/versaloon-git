# Versaloon GIT

## Origin
_This repository is unofficial!_ But it is documented better. The official one on github is [here](https://github.com/versaloon/versaloon) (uses SVN).

This repository was copied from 
[Google code archive](https://code.google.com/archive/p/vsprog/source/default/source) and converted from SVN to GIT. All line endings were converted to Unix type (LF).

## About
The Versaloon project is a universal interface/programmer based on STM32F microcontrollers.
It supports several programming protocols, such as *JTAG*, *STM8 SWIM*, *AVR ISP*, *STM32 SWD* and *Silabs C2*.

From official site (now dead, accessible using 
[internet archive](https://web.archive.org/web/20151025183950/http://www.versaloon.com:80/doc/versaloon/doc_versaloon_programmer_platform.html)):

>  Versaloon Programmer Platform is a generic framework for MCU programming, which can support now more than 10 kinds of target MCUs.
> And it can be simply expanded to support more chips based on USB_TO_XXX protocol.
> There are xml configure files for each MCU series, so for newly released chip, it's very easy to add into this Programmer Platform.
> Vsprog is the command line of the platform compiled using GCC toolchain, it can also support scripts. Vsgui is the GUI compiled using Lazarus.

## Structure
This project has several parts (***folder of the part***):

- the computer-programmer interface
	- `vsprog` - command line tool for controlling the programmer (***src***)
	- `vsgui` - graphical interface for vsprog (***vsgui***)
- documentation (***doc***)
- the programmer firmware and hardware (***dongle***)
- built programmer firmware [outdated] (***release***)
- Versaloon framework - _unfinished_ (***vsf***)
- Versaloon framework for android - only .apk file (***Vsfand***)
- files for build environment (***codelite***)


## Compiling
Compilation was tested on Linux Mint 20, it should work on Ubuntu too.

### vsprog
#### Requirements
To compile `vsprog`, you need `build-essential, autoconf, automake, libusb-1.0-0-dev, libxml2-dev, texinfo`

`sudo apt-get install build-essential autoconf automake libusb-1.0-0-dev libxml2-dev texinfo`

#### Compilation
Execute the commands in the repository's root.
```
./bootstrap
./configure --enable-maintainer-mode
make
```

#### Installation
`sudo make install-strip`

#### Adding udev rules for Versaloon
```
sudo cp -v dongle/driver/linux/60-versaloon.rules /etc/udev/rules.d
sudo udevadm control --reload-rules
```

#### Cleaning
`make maintainer-clean`

For more information about `vsprog`, see [doc/vsprog.texi](doc/vsprog.texi)

--------------------------------------------------------------------------------
### vsgui
#### Requirements
To compile `vsgui`, you will need a Pascal compiler for Lazarus IDE `lcl` (~140MB) or [Lazarus IDE](http://www.lazarus-ide.org/) alone.

`sudo apt-get install lcl`

#### Compilation
```
cd vsgui
lazbuild vsgui.lpr
```

This will generate `vsgui` executable in the *vsgui* folder.

--------------------------------------------------------------------------------
### Versaloon programmer firmware
If you want to use a STM32F103 Blue Pill board, you can get the compiled firmware
[here](https://github.com/zoobab/versaloon).

#### Building from source
You will need a gcc-arm commpiler toolchain to build the firmware.

##### Installing the compiler
Download the official
[GNU Embedded Toolchain for ARM](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-rm/downloads)  
Unpack the archive to your desired location, and add it to your `PATH`.

On Ubuntu based linux distributions you can install it this way:
```
sudo add-apt-repository ppa:team-gcc-arm-embedded/ppa
sudo apt-get update
sudo apt-get install gcc-arm-embedded
```

#### Make
Default build target is STM32F103C8 (Blue Pill).
```
cd dongle/firmware/Projects/Versaloon/GCC
make
```

The built binary .hex file will be `/dongle/firmware/Projects/Versaloon/GCC/Versaloon_GCC-BluePill-0x0.hex`

To clean the directory after build (deletes the hex file):  
`make clean`

#### Uploading the firmware
Upload the firmware to your board using STlink (both 
[software](https://github.com/stlink-org/stlink) and hardware) or Versaloon itself.

For more information about uploading, see [doc/Uploading-fw.md](doc/Uploading-fw.md)

## Connecting to target microcontroller
See [doc/Connecting.md](doc/Connecting.md)

## Using Versaloon with vsprog
See [doc/Commands.md](doc/Commands.md)

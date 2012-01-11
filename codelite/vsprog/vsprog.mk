##
## Auto Generated makefile by CodeLite IDE
## any manual changes will be erased      
##
## Debug
ProjectName            :=vsprog
ConfigurationName      :=Debug
IntermediateDirectory  :=./Debug
OutDir                 := $(IntermediateDirectory)
WorkspacePath          := "F:\MyProject\versaloon\vsprog\codelite"
ProjectPath            := "F:\MyProject\versaloon\vsprog\codelite\vsprog"
CurrentFileName        :=
CurrentFilePath        :=
CurrentFileFullPath    :=
User                   :=Simon
Date                   :=2012/1/11
CodeLitePath           :="C:\Program Files (x86)\CodeLite"
LinkerName             :=gcc
ArchiveTool            :=ar rcus
SharedObjectLinkerName :=gcc -shared -fPIC
ObjectSuffix           :=.o
DependSuffix           :=.o.d
PreprocessSuffix       :=.o.i
DebugSwitch            :=-g 
IncludeSwitch          :=-I
LibrarySwitch          :=-l
OutputSwitch           :=-o 
LibraryPathSwitch      :=-L
PreprocessorSwitch     :=-D
SourceSwitch           :=-c 
CompilerName           :=gcc
C_CompilerName         :=gcc
OutputFile             :=$(IntermediateDirectory)/$(ProjectName)
Preprocessors          :=$(PreprocessorSwitch)HAVE_CONFIG_H 
ObjectSwitch           :=-o 
ArchiveOutputSwitch    := 
PreprocessOnlySwitch   :=-E 
ObjectsFileList        :="F:\MyProject\versaloon\vsprog\codelite\vsprog\vsprog.txt"
PCHCompileFlags        :=
MakeDirCommand         :=makedir
CmpOptions             := -g -O0 -Wall $(Preprocessors)
C_CmpOptions           := -g -O0 -Wall $(Preprocessors)
LinkOptions            :=  -lusb -lxml2 -lm
IncludePath            :=  $(IncludeSwitch). $(IncludeSwitch). $(IncludeSwitch)include $(IncludeSwitch)../../src/vsf_cfg $(IncludeSwitch)../../vsf $(IncludeSwitch)../../vsf/interfaces/cpu/pc $(IncludeSwitch)../../vsf/tool/list $(IncludeSwitch)../../src $(IncludeSwitch)../../src/scripts $(IncludeSwitch)../../src/tools/pgbar $(IncludeSwitch)../../src/tools/hex $(IncludeSwitch)../../src/tools/s19 $(IncludeSwitch)../../src/tools/filelist $(IncludeSwitch)../../src/tools/fileparser $(IncludeSwitch)../../src/tools/memlist $(IncludeSwitch)../../src/tools/strparser $(IncludeSwitch)../../src/driver $(IncludeSwitch)../../src/driver/usb $(IncludeSwitch)../../src/driver/comport $(IncludeSwitch)../../src/programmer $(IncludeSwitch)../../src/programmer/interfaces $(IncludeSwitch)../../src/target $(IncludeSwitch)../../src/target/cortex-m3 $(IncludeSwitch)../../src/target/core/arm_adi $(IncludeSwitch)../../src/target/comisp $(IncludeSwitch)../../src/target/lpc1000 $(IncludeSwitch)../../src/target/stm32f1 $(IncludeSwitch)../../src/target/stm32f2 $(IncludeSwitch)../../src/target/stm32l1 $(IncludeSwitch)../../src/target/at91sam3 $(IncludeSwitch)../../src/target/lm3s 
IncludePCH             := 
RcIncludePath          := 
Libs                   := 
LibPath                := $(LibraryPathSwitch). $(LibraryPathSwitch)./lib 


##
## User defined environment variables
##
CodeLiteDir:=C:\Program Files (x86)\CodeLite
WXWIN:=C:\wxWidgets-2.9.2
PATH:=$(WXWIN)\lib\gcc_dll;$(PATH)
WXCFG:=gcc_dll\mswu
UNIT_TEST_PP_SRC_DIR:=C:\UnitTest++-1.3
Objects=$(IntermediateDirectory)/src_vsprog$(ObjectSuffix) $(IntermediateDirectory)/programmer_programmer$(ObjectSuffix) $(IntermediateDirectory)/interfaces_interfaces$(ObjectSuffix) $(IntermediateDirectory)/versaloon_versaloon$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoadc$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtobdm$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoc2$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtodusi$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoebi$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtogpio$(ObjectSuffix) \
	$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoissp$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtojtagll$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtopwm$(ObjectSuffix) \
	$(IntermediateDirectory)/usbtoxxx_usbtopwr$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtospi$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoswd$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoswim$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtousart$(ObjectSuffix) $(IntermediateDirectory)/usbtoxxx_usbtoxxx$(ObjectSuffix) $(IntermediateDirectory)/vi_stm32_vi_stm32$(ObjectSuffix) $(IntermediateDirectory)/scripts_app_scripts$(ObjectSuffix) $(IntermediateDirectory)/scripts_interfaces_script$(ObjectSuffix) $(IntermediateDirectory)/scripts_scripts$(ObjectSuffix) \
	$(IntermediateDirectory)/mic2826_script_mic2826_script$(ObjectSuffix) $(IntermediateDirectory)/driver_port$(ObjectSuffix) $(IntermediateDirectory)/usb_usbapi$(ObjectSuffix) $(IntermediateDirectory)/comport_comport$(ObjectSuffix) $(IntermediateDirectory)/filelist_filelist$(ObjectSuffix) $(IntermediateDirectory)/fileparser_fileparser$(ObjectSuffix) $(IntermediateDirectory)/hex_hex$(ObjectSuffix) $(IntermediateDirectory)/memlist_memlist$(ObjectSuffix) $(IntermediateDirectory)/pgbar_pgbar$(ObjectSuffix) $(IntermediateDirectory)/s19_s19$(ObjectSuffix) \
	$(IntermediateDirectory)/strparser_strparser$(ObjectSuffix) $(IntermediateDirectory)/target_target$(ObjectSuffix) $(IntermediateDirectory)/at89s5x_at89s5x$(ObjectSuffix) $(IntermediateDirectory)/at91sam3_at91sam3$(ObjectSuffix) $(IntermediateDirectory)/avr8_avr8$(ObjectSuffix) $(IntermediateDirectory)/avr8_avr8_hvpp$(ObjectSuffix) $(IntermediateDirectory)/avr8_avr8_hvsp$(ObjectSuffix) $(IntermediateDirectory)/avr8_avr8_isp$(ObjectSuffix) $(IntermediateDirectory)/avr8_avr8_jtag$(ObjectSuffix) $(IntermediateDirectory)/avr32_avr32$(ObjectSuffix) \
	$(IntermediateDirectory)/avrxmega_avrxmega$(ObjectSuffix) $(IntermediateDirectory)/c8051f_c8051f$(ObjectSuffix) $(IntermediateDirectory)/c8051f_c8051f_c2$(ObjectSuffix) $(IntermediateDirectory)/c8051f_c8051f_jtag$(ObjectSuffix) $(IntermediateDirectory)/cfi_cfi$(ObjectSuffix) $(IntermediateDirectory)/comisp_comisp$(ObjectSuffix) $(IntermediateDirectory)/comisp_lpcarmisp$(ObjectSuffix) $(IntermediateDirectory)/comisp_stm32isp$(ObjectSuffix) $(IntermediateDirectory)/arm_adi_adi_v5p1$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3$(ObjectSuffix) \
	$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_common$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_lm3s$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(ObjectSuffix) $(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(ObjectSuffix) $(IntermediateDirectory)/df25xx_df25xx$(ObjectSuffix) $(IntermediateDirectory)/ee24cxx_ee24cxx$(ObjectSuffix) \
	$(IntermediateDirectory)/ee93cx6_ee93cx6$(ObjectSuffix) $(IntermediateDirectory)/hcs08_hcs08$(ObjectSuffix) $(IntermediateDirectory)/hcs12_hcs12$(ObjectSuffix) $(IntermediateDirectory)/lm3s_lm3s$(ObjectSuffix) $(IntermediateDirectory)/lpc900_lpc900$(ObjectSuffix) $(IntermediateDirectory)/lpc1000_lpc1000$(ObjectSuffix) $(IntermediateDirectory)/msp430_JTAGfunc$(ObjectSuffix) $(IntermediateDirectory)/msp430_msp430$(ObjectSuffix) $(IntermediateDirectory)/msp430_msp430_bsl$(ObjectSuffix) $(IntermediateDirectory)/msp430_msp430_jtagsbw$(ObjectSuffix) \
	$(IntermediateDirectory)/nand_nand$(ObjectSuffix) $(IntermediateDirectory)/psoc1_psoc1$(ObjectSuffix) $(IntermediateDirectory)/sd_sd$(ObjectSuffix) $(IntermediateDirectory)/stm8_stm8$(ObjectSuffix) $(IntermediateDirectory)/stm32f1_stm32f1$(ObjectSuffix) $(IntermediateDirectory)/stm32f2_stm32f2$(ObjectSuffix) $(IntermediateDirectory)/stm32l1_stm32l1$(ObjectSuffix) $(IntermediateDirectory)/svf_player_byte_tap$(ObjectSuffix) $(IntermediateDirectory)/svf_player_svf_parser$(ObjectSuffix) $(IntermediateDirectory)/svf_player_svf_player$(ObjectSuffix) \
	$(IntermediateDirectory)/target_target_readconf$(ObjectSuffix) $(IntermediateDirectory)/dal_dal$(ObjectSuffix) $(IntermediateDirectory)/mal_mal$(ObjectSuffix) $(IntermediateDirectory)/cfi_cfi_drv$(ObjectSuffix) $(IntermediateDirectory)/df25xx_df25xx_drv$(ObjectSuffix) $(IntermediateDirectory)/df45xx_df45xx_drv$(ObjectSuffix) $(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(ObjectSuffix) $(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(ObjectSuffix) $(IntermediateDirectory)/mic2826_mic2826_drv$(ObjectSuffix) $(IntermediateDirectory)/nand_nand_drv$(ObjectSuffix) \
	$(IntermediateDirectory)/sd_sd_common$(ObjectSuffix) $(IntermediateDirectory)/sd_sd_spi_drv$(ObjectSuffix) $(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(ObjectSuffix) 

##
## Main Build Targets 
##
.PHONY: all clean PreBuild PrePreBuild PostBuild
all: $(OutputFile)

$(OutputFile): $(IntermediateDirectory)/.d $(Objects) 
	@$(MakeDirCommand) $(@D)
	@echo "" > $(IntermediateDirectory)/.d
	@echo $(Objects) > $(ObjectsFileList)
	$(LinkerName) $(OutputSwitch)$(OutputFile) @$(ObjectsFileList) $(LibPath) $(Libs) $(LinkOptions)

$(IntermediateDirectory)/.d:
	@$(MakeDirCommand) "./Debug"

PreBuild:


##
## Objects
##
$(IntermediateDirectory)/src_vsprog$(ObjectSuffix): ../../src/vsprog.c $(IntermediateDirectory)/src_vsprog$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/vsprog.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/src_vsprog$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/src_vsprog$(DependSuffix): ../../src/vsprog.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/src_vsprog$(ObjectSuffix) -MF$(IntermediateDirectory)/src_vsprog$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/vsprog.c"

$(IntermediateDirectory)/src_vsprog$(PreprocessSuffix): ../../src/vsprog.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/src_vsprog$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/vsprog.c"

$(IntermediateDirectory)/programmer_programmer$(ObjectSuffix): ../../src/programmer/programmer.c $(IntermediateDirectory)/programmer_programmer$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/programmer.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/programmer_programmer$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/programmer_programmer$(DependSuffix): ../../src/programmer/programmer.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/programmer_programmer$(ObjectSuffix) -MF$(IntermediateDirectory)/programmer_programmer$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/programmer.c"

$(IntermediateDirectory)/programmer_programmer$(PreprocessSuffix): ../../src/programmer/programmer.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/programmer_programmer$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/programmer.c"

$(IntermediateDirectory)/interfaces_interfaces$(ObjectSuffix): ../../src/programmer/interfaces/interfaces.c $(IntermediateDirectory)/interfaces_interfaces$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/interfaces.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/interfaces_interfaces$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/interfaces_interfaces$(DependSuffix): ../../src/programmer/interfaces/interfaces.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/interfaces_interfaces$(ObjectSuffix) -MF$(IntermediateDirectory)/interfaces_interfaces$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/interfaces.c"

$(IntermediateDirectory)/interfaces_interfaces$(PreprocessSuffix): ../../src/programmer/interfaces/interfaces.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/interfaces_interfaces$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/interfaces.c"

$(IntermediateDirectory)/versaloon_versaloon$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/versaloon.c $(IntermediateDirectory)/versaloon_versaloon$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/versaloon.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/versaloon_versaloon$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/versaloon_versaloon$(DependSuffix): ../../src/programmer/interfaces/versaloon/versaloon.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/versaloon_versaloon$(ObjectSuffix) -MF$(IntermediateDirectory)/versaloon_versaloon$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/versaloon.c"

$(IntermediateDirectory)/versaloon_versaloon$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/versaloon.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/versaloon_versaloon$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/versaloon.c"

$(IntermediateDirectory)/usbtoxxx_usbtoadc$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoadc.c $(IntermediateDirectory)/usbtoxxx_usbtoadc$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoadc.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoadc$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoadc$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoadc.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoadc$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoadc$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoadc.c"

$(IntermediateDirectory)/usbtoxxx_usbtoadc$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoadc.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoadc$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoadc.c"

$(IntermediateDirectory)/usbtoxxx_usbtobdm$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtobdm.c $(IntermediateDirectory)/usbtoxxx_usbtobdm$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtobdm.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtobdm$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtobdm$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtobdm.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtobdm$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtobdm$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtobdm.c"

$(IntermediateDirectory)/usbtoxxx_usbtobdm$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtobdm.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtobdm$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtobdm.c"

$(IntermediateDirectory)/usbtoxxx_usbtoc2$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoc2.c $(IntermediateDirectory)/usbtoxxx_usbtoc2$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoc2.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoc2$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoc2$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoc2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoc2$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoc2$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoc2.c"

$(IntermediateDirectory)/usbtoxxx_usbtoc2$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoc2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoc2$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoc2.c"

$(IntermediateDirectory)/usbtoxxx_usbtodusi$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtodusi.c $(IntermediateDirectory)/usbtoxxx_usbtodusi$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtodusi.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtodusi$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtodusi$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtodusi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtodusi$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtodusi$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtodusi.c"

$(IntermediateDirectory)/usbtoxxx_usbtodusi$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtodusi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtodusi$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtodusi.c"

$(IntermediateDirectory)/usbtoxxx_usbtoebi$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoebi.c $(IntermediateDirectory)/usbtoxxx_usbtoebi$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoebi.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoebi$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoebi$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoebi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoebi$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoebi$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoebi.c"

$(IntermediateDirectory)/usbtoxxx_usbtoebi$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoebi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoebi$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoebi.c"

$(IntermediateDirectory)/usbtoxxx_usbtogpio$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtogpio.c $(IntermediateDirectory)/usbtoxxx_usbtogpio$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtogpio.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtogpio$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtogpio$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtogpio.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtogpio$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtogpio$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtogpio.c"

$(IntermediateDirectory)/usbtoxxx_usbtogpio$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtogpio.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtogpio$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtogpio.c"

$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoi2c.c $(IntermediateDirectory)/usbtoxxx_usbtoi2c$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoi2c.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoi2c.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoi2c.c"

$(IntermediateDirectory)/usbtoxxx_usbtoi2c$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoi2c.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoi2c$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoi2c.c"

$(IntermediateDirectory)/usbtoxxx_usbtoissp$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoissp.c $(IntermediateDirectory)/usbtoxxx_usbtoissp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoissp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoissp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoissp$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoissp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoissp$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoissp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoissp.c"

$(IntermediateDirectory)/usbtoxxx_usbtoissp$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoissp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoissp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoissp.c"

$(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtaghl.c $(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtaghl.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtaghl.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtaghl.c"

$(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtaghl.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtaghl.c"

$(IntermediateDirectory)/usbtoxxx_usbtojtagll$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagll.c $(IntermediateDirectory)/usbtoxxx_usbtojtagll$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagll.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtojtagll$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtojtagll$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagll.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtojtagll$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtojtagll$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagll.c"

$(IntermediateDirectory)/usbtoxxx_usbtojtagll$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagll.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtojtagll$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagll.c"

$(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagraw.c $(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagraw.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagraw.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagraw.c"

$(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagraw.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtojtagraw.c"

$(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtolpcicp.c $(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtolpcicp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtolpcicp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtolpcicp.c"

$(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtolpcicp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtolpcicp.c"

$(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomicrowire.c $(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomicrowire.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomicrowire.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomicrowire.c"

$(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomicrowire.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomicrowire.c"

$(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c $(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c"

$(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430jtag.c"

$(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c $(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c"

$(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtomsp430sbw.c"

$(IntermediateDirectory)/usbtoxxx_usbtopwm$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtopwm.c $(IntermediateDirectory)/usbtoxxx_usbtopwm$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtopwm.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtopwm$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtopwm$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtopwm.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtopwm$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtopwm$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtopwm.c"

$(IntermediateDirectory)/usbtoxxx_usbtopwm$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtopwm.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtopwm$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtopwm.c"

$(IntermediateDirectory)/usbtoxxx_usbtopwr$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtopwr.c $(IntermediateDirectory)/usbtoxxx_usbtopwr$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtopwr.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtopwr$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtopwr$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtopwr.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtopwr$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtopwr$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtopwr.c"

$(IntermediateDirectory)/usbtoxxx_usbtopwr$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtopwr.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtopwr$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtopwr.c"

$(IntermediateDirectory)/usbtoxxx_usbtospi$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtospi.c $(IntermediateDirectory)/usbtoxxx_usbtospi$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtospi.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtospi$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtospi$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtospi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtospi$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtospi$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtospi.c"

$(IntermediateDirectory)/usbtoxxx_usbtospi$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtospi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtospi$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtospi.c"

$(IntermediateDirectory)/usbtoxxx_usbtoswd$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoswd.c $(IntermediateDirectory)/usbtoxxx_usbtoswd$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoswd.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoswd$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoswd$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoswd.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoswd$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoswd$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoswd.c"

$(IntermediateDirectory)/usbtoxxx_usbtoswd$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoswd.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoswd$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoswd.c"

$(IntermediateDirectory)/usbtoxxx_usbtoswim$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoswim.c $(IntermediateDirectory)/usbtoxxx_usbtoswim$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoswim.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoswim$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoswim$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoswim.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoswim$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoswim$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoswim.c"

$(IntermediateDirectory)/usbtoxxx_usbtoswim$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoswim.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoswim$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoswim.c"

$(IntermediateDirectory)/usbtoxxx_usbtousart$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtousart.c $(IntermediateDirectory)/usbtoxxx_usbtousart$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtousart.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtousart$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtousart$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtousart.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtousart$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtousart$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtousart.c"

$(IntermediateDirectory)/usbtoxxx_usbtousart$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtousart.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtousart$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtousart.c"

$(IntermediateDirectory)/usbtoxxx_usbtoxxx$(ObjectSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoxxx.c $(IntermediateDirectory)/usbtoxxx_usbtoxxx$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoxxx.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usbtoxxx_usbtoxxx$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usbtoxxx_usbtoxxx$(DependSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoxxx.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usbtoxxx_usbtoxxx$(ObjectSuffix) -MF$(IntermediateDirectory)/usbtoxxx_usbtoxxx$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoxxx.c"

$(IntermediateDirectory)/usbtoxxx_usbtoxxx$(PreprocessSuffix): ../../src/programmer/interfaces/versaloon/usbtoxxx/usbtoxxx.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usbtoxxx_usbtoxxx$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/versaloon/usbtoxxx/usbtoxxx.c"

$(IntermediateDirectory)/vi_stm32_vi_stm32$(ObjectSuffix): ../../src/programmer/interfaces/virtualinterface/vi_stm32/vi_stm32.c $(IntermediateDirectory)/vi_stm32_vi_stm32$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/virtualinterface/vi_stm32/vi_stm32.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/vi_stm32_vi_stm32$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/vi_stm32_vi_stm32$(DependSuffix): ../../src/programmer/interfaces/virtualinterface/vi_stm32/vi_stm32.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/vi_stm32_vi_stm32$(ObjectSuffix) -MF$(IntermediateDirectory)/vi_stm32_vi_stm32$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/virtualinterface/vi_stm32/vi_stm32.c"

$(IntermediateDirectory)/vi_stm32_vi_stm32$(PreprocessSuffix): ../../src/programmer/interfaces/virtualinterface/vi_stm32/vi_stm32.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/vi_stm32_vi_stm32$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/programmer/interfaces/virtualinterface/vi_stm32/vi_stm32.c"

$(IntermediateDirectory)/scripts_app_scripts$(ObjectSuffix): ../../src/scripts/app_scripts.c $(IntermediateDirectory)/scripts_app_scripts$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/scripts/app_scripts.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/scripts_app_scripts$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/scripts_app_scripts$(DependSuffix): ../../src/scripts/app_scripts.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/scripts_app_scripts$(ObjectSuffix) -MF$(IntermediateDirectory)/scripts_app_scripts$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/scripts/app_scripts.c"

$(IntermediateDirectory)/scripts_app_scripts$(PreprocessSuffix): ../../src/scripts/app_scripts.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/scripts_app_scripts$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/scripts/app_scripts.c"

$(IntermediateDirectory)/scripts_interfaces_script$(ObjectSuffix): ../../src/scripts/interfaces_script.c $(IntermediateDirectory)/scripts_interfaces_script$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/scripts/interfaces_script.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/scripts_interfaces_script$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/scripts_interfaces_script$(DependSuffix): ../../src/scripts/interfaces_script.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/scripts_interfaces_script$(ObjectSuffix) -MF$(IntermediateDirectory)/scripts_interfaces_script$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/scripts/interfaces_script.c"

$(IntermediateDirectory)/scripts_interfaces_script$(PreprocessSuffix): ../../src/scripts/interfaces_script.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/scripts_interfaces_script$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/scripts/interfaces_script.c"

$(IntermediateDirectory)/scripts_scripts$(ObjectSuffix): ../../src/scripts/scripts.c $(IntermediateDirectory)/scripts_scripts$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/scripts/scripts.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/scripts_scripts$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/scripts_scripts$(DependSuffix): ../../src/scripts/scripts.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/scripts_scripts$(ObjectSuffix) -MF$(IntermediateDirectory)/scripts_scripts$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/scripts/scripts.c"

$(IntermediateDirectory)/scripts_scripts$(PreprocessSuffix): ../../src/scripts/scripts.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/scripts_scripts$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/scripts/scripts.c"

$(IntermediateDirectory)/mic2826_script_mic2826_script$(ObjectSuffix): ../../src/scripts/mic2826_script/mic2826_script.c $(IntermediateDirectory)/mic2826_script_mic2826_script$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/scripts/mic2826_script/mic2826_script.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/mic2826_script_mic2826_script$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/mic2826_script_mic2826_script$(DependSuffix): ../../src/scripts/mic2826_script/mic2826_script.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/mic2826_script_mic2826_script$(ObjectSuffix) -MF$(IntermediateDirectory)/mic2826_script_mic2826_script$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/scripts/mic2826_script/mic2826_script.c"

$(IntermediateDirectory)/mic2826_script_mic2826_script$(PreprocessSuffix): ../../src/scripts/mic2826_script/mic2826_script.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/mic2826_script_mic2826_script$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/scripts/mic2826_script/mic2826_script.c"

$(IntermediateDirectory)/driver_port$(ObjectSuffix): ../../src/driver/port.c $(IntermediateDirectory)/driver_port$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/driver/port.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/driver_port$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/driver_port$(DependSuffix): ../../src/driver/port.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/driver_port$(ObjectSuffix) -MF$(IntermediateDirectory)/driver_port$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/driver/port.c"

$(IntermediateDirectory)/driver_port$(PreprocessSuffix): ../../src/driver/port.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/driver_port$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/driver/port.c"

$(IntermediateDirectory)/usb_usbapi$(ObjectSuffix): ../../src/driver/usb/usbapi.c $(IntermediateDirectory)/usb_usbapi$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/driver/usb/usbapi.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/usb_usbapi$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/usb_usbapi$(DependSuffix): ../../src/driver/usb/usbapi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/usb_usbapi$(ObjectSuffix) -MF$(IntermediateDirectory)/usb_usbapi$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/driver/usb/usbapi.c"

$(IntermediateDirectory)/usb_usbapi$(PreprocessSuffix): ../../src/driver/usb/usbapi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/usb_usbapi$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/driver/usb/usbapi.c"

$(IntermediateDirectory)/comport_comport$(ObjectSuffix): ../../src/driver/comport/comport.c $(IntermediateDirectory)/comport_comport$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/driver/comport/comport.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/comport_comport$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/comport_comport$(DependSuffix): ../../src/driver/comport/comport.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/comport_comport$(ObjectSuffix) -MF$(IntermediateDirectory)/comport_comport$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/driver/comport/comport.c"

$(IntermediateDirectory)/comport_comport$(PreprocessSuffix): ../../src/driver/comport/comport.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/comport_comport$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/driver/comport/comport.c"

$(IntermediateDirectory)/filelist_filelist$(ObjectSuffix): ../../src/tools/filelist/filelist.c $(IntermediateDirectory)/filelist_filelist$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/filelist/filelist.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/filelist_filelist$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/filelist_filelist$(DependSuffix): ../../src/tools/filelist/filelist.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/filelist_filelist$(ObjectSuffix) -MF$(IntermediateDirectory)/filelist_filelist$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/filelist/filelist.c"

$(IntermediateDirectory)/filelist_filelist$(PreprocessSuffix): ../../src/tools/filelist/filelist.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/filelist_filelist$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/filelist/filelist.c"

$(IntermediateDirectory)/fileparser_fileparser$(ObjectSuffix): ../../src/tools/fileparser/fileparser.c $(IntermediateDirectory)/fileparser_fileparser$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/fileparser/fileparser.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/fileparser_fileparser$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/fileparser_fileparser$(DependSuffix): ../../src/tools/fileparser/fileparser.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/fileparser_fileparser$(ObjectSuffix) -MF$(IntermediateDirectory)/fileparser_fileparser$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/fileparser/fileparser.c"

$(IntermediateDirectory)/fileparser_fileparser$(PreprocessSuffix): ../../src/tools/fileparser/fileparser.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/fileparser_fileparser$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/fileparser/fileparser.c"

$(IntermediateDirectory)/hex_hex$(ObjectSuffix): ../../src/tools/hex/hex.c $(IntermediateDirectory)/hex_hex$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/hex/hex.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/hex_hex$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/hex_hex$(DependSuffix): ../../src/tools/hex/hex.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/hex_hex$(ObjectSuffix) -MF$(IntermediateDirectory)/hex_hex$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/hex/hex.c"

$(IntermediateDirectory)/hex_hex$(PreprocessSuffix): ../../src/tools/hex/hex.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/hex_hex$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/hex/hex.c"

$(IntermediateDirectory)/memlist_memlist$(ObjectSuffix): ../../src/tools/memlist/memlist.c $(IntermediateDirectory)/memlist_memlist$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/memlist/memlist.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/memlist_memlist$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/memlist_memlist$(DependSuffix): ../../src/tools/memlist/memlist.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/memlist_memlist$(ObjectSuffix) -MF$(IntermediateDirectory)/memlist_memlist$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/memlist/memlist.c"

$(IntermediateDirectory)/memlist_memlist$(PreprocessSuffix): ../../src/tools/memlist/memlist.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/memlist_memlist$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/memlist/memlist.c"

$(IntermediateDirectory)/pgbar_pgbar$(ObjectSuffix): ../../src/tools/pgbar/pgbar.c $(IntermediateDirectory)/pgbar_pgbar$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/pgbar/pgbar.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/pgbar_pgbar$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/pgbar_pgbar$(DependSuffix): ../../src/tools/pgbar/pgbar.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/pgbar_pgbar$(ObjectSuffix) -MF$(IntermediateDirectory)/pgbar_pgbar$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/pgbar/pgbar.c"

$(IntermediateDirectory)/pgbar_pgbar$(PreprocessSuffix): ../../src/tools/pgbar/pgbar.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/pgbar_pgbar$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/pgbar/pgbar.c"

$(IntermediateDirectory)/s19_s19$(ObjectSuffix): ../../src/tools/s19/s19.c $(IntermediateDirectory)/s19_s19$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/s19/s19.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/s19_s19$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/s19_s19$(DependSuffix): ../../src/tools/s19/s19.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/s19_s19$(ObjectSuffix) -MF$(IntermediateDirectory)/s19_s19$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/s19/s19.c"

$(IntermediateDirectory)/s19_s19$(PreprocessSuffix): ../../src/tools/s19/s19.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/s19_s19$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/s19/s19.c"

$(IntermediateDirectory)/strparser_strparser$(ObjectSuffix): ../../src/tools/strparser/strparser.c $(IntermediateDirectory)/strparser_strparser$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/tools/strparser/strparser.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/strparser_strparser$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/strparser_strparser$(DependSuffix): ../../src/tools/strparser/strparser.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/strparser_strparser$(ObjectSuffix) -MF$(IntermediateDirectory)/strparser_strparser$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/tools/strparser/strparser.c"

$(IntermediateDirectory)/strparser_strparser$(PreprocessSuffix): ../../src/tools/strparser/strparser.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/strparser_strparser$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/tools/strparser/strparser.c"

$(IntermediateDirectory)/target_target$(ObjectSuffix): ../../src/target/target.c $(IntermediateDirectory)/target_target$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/target.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/target_target$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/target_target$(DependSuffix): ../../src/target/target.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/target_target$(ObjectSuffix) -MF$(IntermediateDirectory)/target_target$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/target.c"

$(IntermediateDirectory)/target_target$(PreprocessSuffix): ../../src/target/target.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/target_target$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/target.c"

$(IntermediateDirectory)/at89s5x_at89s5x$(ObjectSuffix): ../../src/target/at89s5x/at89s5x.c $(IntermediateDirectory)/at89s5x_at89s5x$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/at89s5x/at89s5x.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/at89s5x_at89s5x$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/at89s5x_at89s5x$(DependSuffix): ../../src/target/at89s5x/at89s5x.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/at89s5x_at89s5x$(ObjectSuffix) -MF$(IntermediateDirectory)/at89s5x_at89s5x$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/at89s5x/at89s5x.c"

$(IntermediateDirectory)/at89s5x_at89s5x$(PreprocessSuffix): ../../src/target/at89s5x/at89s5x.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/at89s5x_at89s5x$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/at89s5x/at89s5x.c"

$(IntermediateDirectory)/at91sam3_at91sam3$(ObjectSuffix): ../../src/target/at91sam3/at91sam3.c $(IntermediateDirectory)/at91sam3_at91sam3$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/at91sam3/at91sam3.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/at91sam3_at91sam3$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/at91sam3_at91sam3$(DependSuffix): ../../src/target/at91sam3/at91sam3.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/at91sam3_at91sam3$(ObjectSuffix) -MF$(IntermediateDirectory)/at91sam3_at91sam3$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/at91sam3/at91sam3.c"

$(IntermediateDirectory)/at91sam3_at91sam3$(PreprocessSuffix): ../../src/target/at91sam3/at91sam3.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/at91sam3_at91sam3$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/at91sam3/at91sam3.c"

$(IntermediateDirectory)/avr8_avr8$(ObjectSuffix): ../../src/target/avr8/avr8.c $(IntermediateDirectory)/avr8_avr8$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avr8_avr8$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avr8_avr8$(DependSuffix): ../../src/target/avr8/avr8.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avr8_avr8$(ObjectSuffix) -MF$(IntermediateDirectory)/avr8_avr8$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8.c"

$(IntermediateDirectory)/avr8_avr8$(PreprocessSuffix): ../../src/target/avr8/avr8.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avr8_avr8$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8.c"

$(IntermediateDirectory)/avr8_avr8_hvpp$(ObjectSuffix): ../../src/target/avr8/avr8_hvpp.c $(IntermediateDirectory)/avr8_avr8_hvpp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_hvpp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avr8_avr8_hvpp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avr8_avr8_hvpp$(DependSuffix): ../../src/target/avr8/avr8_hvpp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avr8_avr8_hvpp$(ObjectSuffix) -MF$(IntermediateDirectory)/avr8_avr8_hvpp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_hvpp.c"

$(IntermediateDirectory)/avr8_avr8_hvpp$(PreprocessSuffix): ../../src/target/avr8/avr8_hvpp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avr8_avr8_hvpp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_hvpp.c"

$(IntermediateDirectory)/avr8_avr8_hvsp$(ObjectSuffix): ../../src/target/avr8/avr8_hvsp.c $(IntermediateDirectory)/avr8_avr8_hvsp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_hvsp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avr8_avr8_hvsp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avr8_avr8_hvsp$(DependSuffix): ../../src/target/avr8/avr8_hvsp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avr8_avr8_hvsp$(ObjectSuffix) -MF$(IntermediateDirectory)/avr8_avr8_hvsp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_hvsp.c"

$(IntermediateDirectory)/avr8_avr8_hvsp$(PreprocessSuffix): ../../src/target/avr8/avr8_hvsp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avr8_avr8_hvsp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_hvsp.c"

$(IntermediateDirectory)/avr8_avr8_isp$(ObjectSuffix): ../../src/target/avr8/avr8_isp.c $(IntermediateDirectory)/avr8_avr8_isp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_isp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avr8_avr8_isp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avr8_avr8_isp$(DependSuffix): ../../src/target/avr8/avr8_isp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avr8_avr8_isp$(ObjectSuffix) -MF$(IntermediateDirectory)/avr8_avr8_isp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_isp.c"

$(IntermediateDirectory)/avr8_avr8_isp$(PreprocessSuffix): ../../src/target/avr8/avr8_isp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avr8_avr8_isp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_isp.c"

$(IntermediateDirectory)/avr8_avr8_jtag$(ObjectSuffix): ../../src/target/avr8/avr8_jtag.c $(IntermediateDirectory)/avr8_avr8_jtag$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_jtag.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avr8_avr8_jtag$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avr8_avr8_jtag$(DependSuffix): ../../src/target/avr8/avr8_jtag.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avr8_avr8_jtag$(ObjectSuffix) -MF$(IntermediateDirectory)/avr8_avr8_jtag$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_jtag.c"

$(IntermediateDirectory)/avr8_avr8_jtag$(PreprocessSuffix): ../../src/target/avr8/avr8_jtag.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avr8_avr8_jtag$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avr8/avr8_jtag.c"

$(IntermediateDirectory)/avr32_avr32$(ObjectSuffix): ../../src/target/avr32/avr32.c $(IntermediateDirectory)/avr32_avr32$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avr32/avr32.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avr32_avr32$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avr32_avr32$(DependSuffix): ../../src/target/avr32/avr32.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avr32_avr32$(ObjectSuffix) -MF$(IntermediateDirectory)/avr32_avr32$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avr32/avr32.c"

$(IntermediateDirectory)/avr32_avr32$(PreprocessSuffix): ../../src/target/avr32/avr32.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avr32_avr32$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avr32/avr32.c"

$(IntermediateDirectory)/avrxmega_avrxmega$(ObjectSuffix): ../../src/target/avrxmega/avrxmega.c $(IntermediateDirectory)/avrxmega_avrxmega$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/avrxmega/avrxmega.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/avrxmega_avrxmega$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/avrxmega_avrxmega$(DependSuffix): ../../src/target/avrxmega/avrxmega.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/avrxmega_avrxmega$(ObjectSuffix) -MF$(IntermediateDirectory)/avrxmega_avrxmega$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/avrxmega/avrxmega.c"

$(IntermediateDirectory)/avrxmega_avrxmega$(PreprocessSuffix): ../../src/target/avrxmega/avrxmega.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/avrxmega_avrxmega$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/avrxmega/avrxmega.c"

$(IntermediateDirectory)/c8051f_c8051f$(ObjectSuffix): ../../src/target/c8051f/c8051f.c $(IntermediateDirectory)/c8051f_c8051f$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/c8051f_c8051f$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/c8051f_c8051f$(DependSuffix): ../../src/target/c8051f/c8051f.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/c8051f_c8051f$(ObjectSuffix) -MF$(IntermediateDirectory)/c8051f_c8051f$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f.c"

$(IntermediateDirectory)/c8051f_c8051f$(PreprocessSuffix): ../../src/target/c8051f/c8051f.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/c8051f_c8051f$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f.c"

$(IntermediateDirectory)/c8051f_c8051f_c2$(ObjectSuffix): ../../src/target/c8051f/c8051f_c2.c $(IntermediateDirectory)/c8051f_c8051f_c2$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f_c2.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/c8051f_c8051f_c2$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/c8051f_c8051f_c2$(DependSuffix): ../../src/target/c8051f/c8051f_c2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/c8051f_c8051f_c2$(ObjectSuffix) -MF$(IntermediateDirectory)/c8051f_c8051f_c2$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f_c2.c"

$(IntermediateDirectory)/c8051f_c8051f_c2$(PreprocessSuffix): ../../src/target/c8051f/c8051f_c2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/c8051f_c8051f_c2$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f_c2.c"

$(IntermediateDirectory)/c8051f_c8051f_jtag$(ObjectSuffix): ../../src/target/c8051f/c8051f_jtag.c $(IntermediateDirectory)/c8051f_c8051f_jtag$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f_jtag.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/c8051f_c8051f_jtag$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/c8051f_c8051f_jtag$(DependSuffix): ../../src/target/c8051f/c8051f_jtag.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/c8051f_c8051f_jtag$(ObjectSuffix) -MF$(IntermediateDirectory)/c8051f_c8051f_jtag$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f_jtag.c"

$(IntermediateDirectory)/c8051f_c8051f_jtag$(PreprocessSuffix): ../../src/target/c8051f/c8051f_jtag.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/c8051f_c8051f_jtag$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/c8051f/c8051f_jtag.c"

$(IntermediateDirectory)/cfi_cfi$(ObjectSuffix): ../../src/target/cfi/cfi.c $(IntermediateDirectory)/cfi_cfi$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cfi/cfi.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cfi_cfi$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cfi_cfi$(DependSuffix): ../../src/target/cfi/cfi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cfi_cfi$(ObjectSuffix) -MF$(IntermediateDirectory)/cfi_cfi$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cfi/cfi.c"

$(IntermediateDirectory)/cfi_cfi$(PreprocessSuffix): ../../src/target/cfi/cfi.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cfi_cfi$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cfi/cfi.c"

$(IntermediateDirectory)/comisp_comisp$(ObjectSuffix): ../../src/target/comisp/comisp.c $(IntermediateDirectory)/comisp_comisp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/comisp/comisp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/comisp_comisp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/comisp_comisp$(DependSuffix): ../../src/target/comisp/comisp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/comisp_comisp$(ObjectSuffix) -MF$(IntermediateDirectory)/comisp_comisp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/comisp/comisp.c"

$(IntermediateDirectory)/comisp_comisp$(PreprocessSuffix): ../../src/target/comisp/comisp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/comisp_comisp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/comisp/comisp.c"

$(IntermediateDirectory)/comisp_lpcarmisp$(ObjectSuffix): ../../src/target/comisp/lpcarmisp.c $(IntermediateDirectory)/comisp_lpcarmisp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/comisp/lpcarmisp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/comisp_lpcarmisp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/comisp_lpcarmisp$(DependSuffix): ../../src/target/comisp/lpcarmisp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/comisp_lpcarmisp$(ObjectSuffix) -MF$(IntermediateDirectory)/comisp_lpcarmisp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/comisp/lpcarmisp.c"

$(IntermediateDirectory)/comisp_lpcarmisp$(PreprocessSuffix): ../../src/target/comisp/lpcarmisp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/comisp_lpcarmisp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/comisp/lpcarmisp.c"

$(IntermediateDirectory)/comisp_stm32isp$(ObjectSuffix): ../../src/target/comisp/stm32isp.c $(IntermediateDirectory)/comisp_stm32isp$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/comisp/stm32isp.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/comisp_stm32isp$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/comisp_stm32isp$(DependSuffix): ../../src/target/comisp/stm32isp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/comisp_stm32isp$(ObjectSuffix) -MF$(IntermediateDirectory)/comisp_stm32isp$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/comisp/stm32isp.c"

$(IntermediateDirectory)/comisp_stm32isp$(PreprocessSuffix): ../../src/target/comisp/stm32isp.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/comisp_stm32isp$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/comisp/stm32isp.c"

$(IntermediateDirectory)/arm_adi_adi_v5p1$(ObjectSuffix): ../../src/target/core/arm_adi/adi_v5p1.c $(IntermediateDirectory)/arm_adi_adi_v5p1$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/core/arm_adi/adi_v5p1.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/arm_adi_adi_v5p1$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/arm_adi_adi_v5p1$(DependSuffix): ../../src/target/core/arm_adi/adi_v5p1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/arm_adi_adi_v5p1$(ObjectSuffix) -MF$(IntermediateDirectory)/arm_adi_adi_v5p1$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/core/arm_adi/adi_v5p1.c"

$(IntermediateDirectory)/arm_adi_adi_v5p1$(PreprocessSuffix): ../../src/target/core/arm_adi/adi_v5p1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/arm_adi_adi_v5p1$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/core/arm_adi/adi_v5p1.c"

$(IntermediateDirectory)/cortex-m3_cm3$(ObjectSuffix): ../../src/target/cortex-m3/cm3.c $(IntermediateDirectory)/cortex-m3_cm3$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3$(DependSuffix): ../../src/target/cortex-m3/cm3.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3.c"

$(IntermediateDirectory)/cortex-m3_cm3$(PreprocessSuffix): ../../src/target/cortex-m3/cm3.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3.c"

$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(ObjectSuffix): ../../src/target/cortex-m3/cm3_at91sam3.c $(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_at91sam3.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(DependSuffix): ../../src/target/cortex-m3/cm3_at91sam3.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_at91sam3.c"

$(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_at91sam3.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_at91sam3.c"

$(IntermediateDirectory)/cortex-m3_cm3_common$(ObjectSuffix): ../../src/target/cortex-m3/cm3_common.c $(IntermediateDirectory)/cortex-m3_cm3_common$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_common.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_common$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_common$(DependSuffix): ../../src/target/cortex-m3/cm3_common.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_common$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_common$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_common.c"

$(IntermediateDirectory)/cortex-m3_cm3_common$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_common.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_common$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_common.c"

$(IntermediateDirectory)/cortex-m3_cm3_lm3s$(ObjectSuffix): ../../src/target/cortex-m3/cm3_lm3s.c $(IntermediateDirectory)/cortex-m3_cm3_lm3s$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_lm3s.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_lm3s$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_lm3s$(DependSuffix): ../../src/target/cortex-m3/cm3_lm3s.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_lm3s$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_lm3s$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_lm3s.c"

$(IntermediateDirectory)/cortex-m3_cm3_lm3s$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_lm3s.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_lm3s$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_lm3s.c"

$(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(ObjectSuffix): ../../src/target/cortex-m3/cm3_lpc1000.c $(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_lpc1000.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(DependSuffix): ../../src/target/cortex-m3/cm3_lpc1000.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_lpc1000.c"

$(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_lpc1000.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_lpc1000.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(ObjectSuffix): ../../src/target/cortex-m3/cm3_stm32_fl.c $(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32_fl.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(DependSuffix): ../../src/target/cortex-m3/cm3_stm32_fl.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32_fl.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_stm32_fl.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32_fl.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(ObjectSuffix): ../../src/target/cortex-m3/cm3_stm32f1.c $(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32f1.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(DependSuffix): ../../src/target/cortex-m3/cm3_stm32f1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32f1.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_stm32f1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32f1.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(ObjectSuffix): ../../src/target/cortex-m3/cm3_stm32f2.c $(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32f2.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(DependSuffix): ../../src/target/cortex-m3/cm3_stm32f2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32f2.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_stm32f2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32f2.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(ObjectSuffix): ../../src/target/cortex-m3/cm3_stm32l1.c $(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32l1.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(DependSuffix): ../../src/target/cortex-m3/cm3_stm32l1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(ObjectSuffix) -MF$(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32l1.c"

$(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(PreprocessSuffix): ../../src/target/cortex-m3/cm3_stm32l1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/cortex-m3/cm3_stm32l1.c"

$(IntermediateDirectory)/df25xx_df25xx$(ObjectSuffix): ../../src/target/df25xx/df25xx.c $(IntermediateDirectory)/df25xx_df25xx$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/df25xx/df25xx.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/df25xx_df25xx$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/df25xx_df25xx$(DependSuffix): ../../src/target/df25xx/df25xx.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/df25xx_df25xx$(ObjectSuffix) -MF$(IntermediateDirectory)/df25xx_df25xx$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/df25xx/df25xx.c"

$(IntermediateDirectory)/df25xx_df25xx$(PreprocessSuffix): ../../src/target/df25xx/df25xx.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/df25xx_df25xx$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/df25xx/df25xx.c"

$(IntermediateDirectory)/ee24cxx_ee24cxx$(ObjectSuffix): ../../src/target/ee24cxx/ee24cxx.c $(IntermediateDirectory)/ee24cxx_ee24cxx$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/ee24cxx/ee24cxx.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/ee24cxx_ee24cxx$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/ee24cxx_ee24cxx$(DependSuffix): ../../src/target/ee24cxx/ee24cxx.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/ee24cxx_ee24cxx$(ObjectSuffix) -MF$(IntermediateDirectory)/ee24cxx_ee24cxx$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/ee24cxx/ee24cxx.c"

$(IntermediateDirectory)/ee24cxx_ee24cxx$(PreprocessSuffix): ../../src/target/ee24cxx/ee24cxx.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/ee24cxx_ee24cxx$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/ee24cxx/ee24cxx.c"

$(IntermediateDirectory)/ee93cx6_ee93cx6$(ObjectSuffix): ../../src/target/ee93cx6/ee93cx6.c $(IntermediateDirectory)/ee93cx6_ee93cx6$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/ee93cx6/ee93cx6.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/ee93cx6_ee93cx6$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/ee93cx6_ee93cx6$(DependSuffix): ../../src/target/ee93cx6/ee93cx6.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/ee93cx6_ee93cx6$(ObjectSuffix) -MF$(IntermediateDirectory)/ee93cx6_ee93cx6$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/ee93cx6/ee93cx6.c"

$(IntermediateDirectory)/ee93cx6_ee93cx6$(PreprocessSuffix): ../../src/target/ee93cx6/ee93cx6.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/ee93cx6_ee93cx6$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/ee93cx6/ee93cx6.c"

$(IntermediateDirectory)/hcs08_hcs08$(ObjectSuffix): ../../src/target/hcs08/hcs08.c $(IntermediateDirectory)/hcs08_hcs08$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/hcs08/hcs08.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/hcs08_hcs08$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/hcs08_hcs08$(DependSuffix): ../../src/target/hcs08/hcs08.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/hcs08_hcs08$(ObjectSuffix) -MF$(IntermediateDirectory)/hcs08_hcs08$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/hcs08/hcs08.c"

$(IntermediateDirectory)/hcs08_hcs08$(PreprocessSuffix): ../../src/target/hcs08/hcs08.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/hcs08_hcs08$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/hcs08/hcs08.c"

$(IntermediateDirectory)/hcs12_hcs12$(ObjectSuffix): ../../src/target/hcs12/hcs12.c $(IntermediateDirectory)/hcs12_hcs12$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/hcs12/hcs12.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/hcs12_hcs12$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/hcs12_hcs12$(DependSuffix): ../../src/target/hcs12/hcs12.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/hcs12_hcs12$(ObjectSuffix) -MF$(IntermediateDirectory)/hcs12_hcs12$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/hcs12/hcs12.c"

$(IntermediateDirectory)/hcs12_hcs12$(PreprocessSuffix): ../../src/target/hcs12/hcs12.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/hcs12_hcs12$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/hcs12/hcs12.c"

$(IntermediateDirectory)/lm3s_lm3s$(ObjectSuffix): ../../src/target/lm3s/lm3s.c $(IntermediateDirectory)/lm3s_lm3s$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/lm3s/lm3s.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/lm3s_lm3s$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/lm3s_lm3s$(DependSuffix): ../../src/target/lm3s/lm3s.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/lm3s_lm3s$(ObjectSuffix) -MF$(IntermediateDirectory)/lm3s_lm3s$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/lm3s/lm3s.c"

$(IntermediateDirectory)/lm3s_lm3s$(PreprocessSuffix): ../../src/target/lm3s/lm3s.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/lm3s_lm3s$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/lm3s/lm3s.c"

$(IntermediateDirectory)/lpc900_lpc900$(ObjectSuffix): ../../src/target/lpc900/lpc900.c $(IntermediateDirectory)/lpc900_lpc900$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/lpc900/lpc900.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/lpc900_lpc900$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/lpc900_lpc900$(DependSuffix): ../../src/target/lpc900/lpc900.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/lpc900_lpc900$(ObjectSuffix) -MF$(IntermediateDirectory)/lpc900_lpc900$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/lpc900/lpc900.c"

$(IntermediateDirectory)/lpc900_lpc900$(PreprocessSuffix): ../../src/target/lpc900/lpc900.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/lpc900_lpc900$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/lpc900/lpc900.c"

$(IntermediateDirectory)/lpc1000_lpc1000$(ObjectSuffix): ../../src/target/lpc1000/lpc1000.c $(IntermediateDirectory)/lpc1000_lpc1000$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/lpc1000/lpc1000.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/lpc1000_lpc1000$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/lpc1000_lpc1000$(DependSuffix): ../../src/target/lpc1000/lpc1000.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/lpc1000_lpc1000$(ObjectSuffix) -MF$(IntermediateDirectory)/lpc1000_lpc1000$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/lpc1000/lpc1000.c"

$(IntermediateDirectory)/lpc1000_lpc1000$(PreprocessSuffix): ../../src/target/lpc1000/lpc1000.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/lpc1000_lpc1000$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/lpc1000/lpc1000.c"

$(IntermediateDirectory)/msp430_JTAGfunc$(ObjectSuffix): ../../src/target/msp430/JTAGfunc.c $(IntermediateDirectory)/msp430_JTAGfunc$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/msp430/JTAGfunc.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/msp430_JTAGfunc$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/msp430_JTAGfunc$(DependSuffix): ../../src/target/msp430/JTAGfunc.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/msp430_JTAGfunc$(ObjectSuffix) -MF$(IntermediateDirectory)/msp430_JTAGfunc$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/msp430/JTAGfunc.c"

$(IntermediateDirectory)/msp430_JTAGfunc$(PreprocessSuffix): ../../src/target/msp430/JTAGfunc.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/msp430_JTAGfunc$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/msp430/JTAGfunc.c"

$(IntermediateDirectory)/msp430_msp430$(ObjectSuffix): ../../src/target/msp430/msp430.c $(IntermediateDirectory)/msp430_msp430$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/msp430_msp430$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/msp430_msp430$(DependSuffix): ../../src/target/msp430/msp430.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/msp430_msp430$(ObjectSuffix) -MF$(IntermediateDirectory)/msp430_msp430$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430.c"

$(IntermediateDirectory)/msp430_msp430$(PreprocessSuffix): ../../src/target/msp430/msp430.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/msp430_msp430$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430.c"

$(IntermediateDirectory)/msp430_msp430_bsl$(ObjectSuffix): ../../src/target/msp430/msp430_bsl.c $(IntermediateDirectory)/msp430_msp430_bsl$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430_bsl.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/msp430_msp430_bsl$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/msp430_msp430_bsl$(DependSuffix): ../../src/target/msp430/msp430_bsl.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/msp430_msp430_bsl$(ObjectSuffix) -MF$(IntermediateDirectory)/msp430_msp430_bsl$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430_bsl.c"

$(IntermediateDirectory)/msp430_msp430_bsl$(PreprocessSuffix): ../../src/target/msp430/msp430_bsl.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/msp430_msp430_bsl$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430_bsl.c"

$(IntermediateDirectory)/msp430_msp430_jtagsbw$(ObjectSuffix): ../../src/target/msp430/msp430_jtagsbw.c $(IntermediateDirectory)/msp430_msp430_jtagsbw$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430_jtagsbw.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/msp430_msp430_jtagsbw$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/msp430_msp430_jtagsbw$(DependSuffix): ../../src/target/msp430/msp430_jtagsbw.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/msp430_msp430_jtagsbw$(ObjectSuffix) -MF$(IntermediateDirectory)/msp430_msp430_jtagsbw$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430_jtagsbw.c"

$(IntermediateDirectory)/msp430_msp430_jtagsbw$(PreprocessSuffix): ../../src/target/msp430/msp430_jtagsbw.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/msp430_msp430_jtagsbw$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/msp430/msp430_jtagsbw.c"

$(IntermediateDirectory)/nand_nand$(ObjectSuffix): ../../src/target/nand/nand.c $(IntermediateDirectory)/nand_nand$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/nand/nand.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/nand_nand$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/nand_nand$(DependSuffix): ../../src/target/nand/nand.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/nand_nand$(ObjectSuffix) -MF$(IntermediateDirectory)/nand_nand$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/nand/nand.c"

$(IntermediateDirectory)/nand_nand$(PreprocessSuffix): ../../src/target/nand/nand.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/nand_nand$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/nand/nand.c"

$(IntermediateDirectory)/psoc1_psoc1$(ObjectSuffix): ../../src/target/psoc1/psoc1.c $(IntermediateDirectory)/psoc1_psoc1$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/psoc1/psoc1.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/psoc1_psoc1$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/psoc1_psoc1$(DependSuffix): ../../src/target/psoc1/psoc1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/psoc1_psoc1$(ObjectSuffix) -MF$(IntermediateDirectory)/psoc1_psoc1$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/psoc1/psoc1.c"

$(IntermediateDirectory)/psoc1_psoc1$(PreprocessSuffix): ../../src/target/psoc1/psoc1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/psoc1_psoc1$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/psoc1/psoc1.c"

$(IntermediateDirectory)/sd_sd$(ObjectSuffix): ../../src/target/sd/sd.c $(IntermediateDirectory)/sd_sd$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/sd/sd.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/sd_sd$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/sd_sd$(DependSuffix): ../../src/target/sd/sd.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/sd_sd$(ObjectSuffix) -MF$(IntermediateDirectory)/sd_sd$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/sd/sd.c"

$(IntermediateDirectory)/sd_sd$(PreprocessSuffix): ../../src/target/sd/sd.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/sd_sd$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/sd/sd.c"

$(IntermediateDirectory)/stm8_stm8$(ObjectSuffix): ../../src/target/stm8/stm8.c $(IntermediateDirectory)/stm8_stm8$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/stm8/stm8.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/stm8_stm8$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/stm8_stm8$(DependSuffix): ../../src/target/stm8/stm8.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/stm8_stm8$(ObjectSuffix) -MF$(IntermediateDirectory)/stm8_stm8$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/stm8/stm8.c"

$(IntermediateDirectory)/stm8_stm8$(PreprocessSuffix): ../../src/target/stm8/stm8.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/stm8_stm8$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/stm8/stm8.c"

$(IntermediateDirectory)/stm32f1_stm32f1$(ObjectSuffix): ../../src/target/stm32f1/stm32f1.c $(IntermediateDirectory)/stm32f1_stm32f1$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/stm32f1/stm32f1.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/stm32f1_stm32f1$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/stm32f1_stm32f1$(DependSuffix): ../../src/target/stm32f1/stm32f1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/stm32f1_stm32f1$(ObjectSuffix) -MF$(IntermediateDirectory)/stm32f1_stm32f1$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/stm32f1/stm32f1.c"

$(IntermediateDirectory)/stm32f1_stm32f1$(PreprocessSuffix): ../../src/target/stm32f1/stm32f1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/stm32f1_stm32f1$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/stm32f1/stm32f1.c"

$(IntermediateDirectory)/stm32f2_stm32f2$(ObjectSuffix): ../../src/target/stm32f2/stm32f2.c $(IntermediateDirectory)/stm32f2_stm32f2$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/stm32f2/stm32f2.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/stm32f2_stm32f2$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/stm32f2_stm32f2$(DependSuffix): ../../src/target/stm32f2/stm32f2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/stm32f2_stm32f2$(ObjectSuffix) -MF$(IntermediateDirectory)/stm32f2_stm32f2$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/stm32f2/stm32f2.c"

$(IntermediateDirectory)/stm32f2_stm32f2$(PreprocessSuffix): ../../src/target/stm32f2/stm32f2.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/stm32f2_stm32f2$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/stm32f2/stm32f2.c"

$(IntermediateDirectory)/stm32l1_stm32l1$(ObjectSuffix): ../../src/target/stm32l1/stm32l1.c $(IntermediateDirectory)/stm32l1_stm32l1$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/stm32l1/stm32l1.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/stm32l1_stm32l1$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/stm32l1_stm32l1$(DependSuffix): ../../src/target/stm32l1/stm32l1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/stm32l1_stm32l1$(ObjectSuffix) -MF$(IntermediateDirectory)/stm32l1_stm32l1$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/stm32l1/stm32l1.c"

$(IntermediateDirectory)/stm32l1_stm32l1$(PreprocessSuffix): ../../src/target/stm32l1/stm32l1.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/stm32l1_stm32l1$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/stm32l1/stm32l1.c"

$(IntermediateDirectory)/svf_player_byte_tap$(ObjectSuffix): ../../src/target/svf_player/byte_tap.c $(IntermediateDirectory)/svf_player_byte_tap$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/svf_player/byte_tap.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/svf_player_byte_tap$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/svf_player_byte_tap$(DependSuffix): ../../src/target/svf_player/byte_tap.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/svf_player_byte_tap$(ObjectSuffix) -MF$(IntermediateDirectory)/svf_player_byte_tap$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/svf_player/byte_tap.c"

$(IntermediateDirectory)/svf_player_byte_tap$(PreprocessSuffix): ../../src/target/svf_player/byte_tap.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/svf_player_byte_tap$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/svf_player/byte_tap.c"

$(IntermediateDirectory)/svf_player_svf_parser$(ObjectSuffix): ../../src/target/svf_player/svf_parser.c $(IntermediateDirectory)/svf_player_svf_parser$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/svf_player/svf_parser.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/svf_player_svf_parser$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/svf_player_svf_parser$(DependSuffix): ../../src/target/svf_player/svf_parser.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/svf_player_svf_parser$(ObjectSuffix) -MF$(IntermediateDirectory)/svf_player_svf_parser$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/svf_player/svf_parser.c"

$(IntermediateDirectory)/svf_player_svf_parser$(PreprocessSuffix): ../../src/target/svf_player/svf_parser.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/svf_player_svf_parser$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/svf_player/svf_parser.c"

$(IntermediateDirectory)/svf_player_svf_player$(ObjectSuffix): ../../src/target/svf_player/svf_player.c $(IntermediateDirectory)/svf_player_svf_player$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/svf_player/svf_player.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/svf_player_svf_player$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/svf_player_svf_player$(DependSuffix): ../../src/target/svf_player/svf_player.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/svf_player_svf_player$(ObjectSuffix) -MF$(IntermediateDirectory)/svf_player_svf_player$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/svf_player/svf_player.c"

$(IntermediateDirectory)/svf_player_svf_player$(PreprocessSuffix): ../../src/target/svf_player/svf_player.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/svf_player_svf_player$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/svf_player/svf_player.c"

$(IntermediateDirectory)/target_target_readconf$(ObjectSuffix): ../../src/target/target_readconf.c $(IntermediateDirectory)/target_target_readconf$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/src/target/target_readconf.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/target_target_readconf$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/target_target_readconf$(DependSuffix): ../../src/target/target_readconf.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/target_target_readconf$(ObjectSuffix) -MF$(IntermediateDirectory)/target_target_readconf$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/src/target/target_readconf.c"

$(IntermediateDirectory)/target_target_readconf$(PreprocessSuffix): ../../src/target/target_readconf.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/target_target_readconf$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/src/target/target_readconf.c"

$(IntermediateDirectory)/dal_dal$(ObjectSuffix): ../../vsf/dal/dal.c $(IntermediateDirectory)/dal_dal$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/dal.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/dal_dal$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/dal_dal$(DependSuffix): ../../vsf/dal/dal.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/dal_dal$(ObjectSuffix) -MF$(IntermediateDirectory)/dal_dal$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/dal.c"

$(IntermediateDirectory)/dal_dal$(PreprocessSuffix): ../../vsf/dal/dal.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/dal_dal$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/dal.c"

$(IntermediateDirectory)/mal_mal$(ObjectSuffix): ../../vsf/dal/mal/mal.c $(IntermediateDirectory)/mal_mal$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/mal/mal.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/mal_mal$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/mal_mal$(DependSuffix): ../../vsf/dal/mal/mal.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/mal_mal$(ObjectSuffix) -MF$(IntermediateDirectory)/mal_mal$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/mal/mal.c"

$(IntermediateDirectory)/mal_mal$(PreprocessSuffix): ../../vsf/dal/mal/mal.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/mal_mal$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/mal/mal.c"

$(IntermediateDirectory)/cfi_cfi_drv$(ObjectSuffix): ../../vsf/dal/cfi/cfi_drv.c $(IntermediateDirectory)/cfi_cfi_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/cfi/cfi_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/cfi_cfi_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/cfi_cfi_drv$(DependSuffix): ../../vsf/dal/cfi/cfi_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/cfi_cfi_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/cfi_cfi_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/cfi/cfi_drv.c"

$(IntermediateDirectory)/cfi_cfi_drv$(PreprocessSuffix): ../../vsf/dal/cfi/cfi_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/cfi_cfi_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/cfi/cfi_drv.c"

$(IntermediateDirectory)/df25xx_df25xx_drv$(ObjectSuffix): ../../vsf/dal/df25xx/df25xx_drv.c $(IntermediateDirectory)/df25xx_df25xx_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/df25xx/df25xx_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/df25xx_df25xx_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/df25xx_df25xx_drv$(DependSuffix): ../../vsf/dal/df25xx/df25xx_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/df25xx_df25xx_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/df25xx_df25xx_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/df25xx/df25xx_drv.c"

$(IntermediateDirectory)/df25xx_df25xx_drv$(PreprocessSuffix): ../../vsf/dal/df25xx/df25xx_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/df25xx_df25xx_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/df25xx/df25xx_drv.c"

$(IntermediateDirectory)/df45xx_df45xx_drv$(ObjectSuffix): ../../vsf/dal/df45xx/df45xx_drv.c $(IntermediateDirectory)/df45xx_df45xx_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/df45xx/df45xx_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/df45xx_df45xx_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/df45xx_df45xx_drv$(DependSuffix): ../../vsf/dal/df45xx/df45xx_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/df45xx_df45xx_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/df45xx_df45xx_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/df45xx/df45xx_drv.c"

$(IntermediateDirectory)/df45xx_df45xx_drv$(PreprocessSuffix): ../../vsf/dal/df45xx/df45xx_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/df45xx_df45xx_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/df45xx/df45xx_drv.c"

$(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(ObjectSuffix): ../../vsf/dal/ee24cxx/ee24cxx_drv.c $(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/ee24cxx/ee24cxx_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(DependSuffix): ../../vsf/dal/ee24cxx/ee24cxx_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/ee24cxx/ee24cxx_drv.c"

$(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(PreprocessSuffix): ../../vsf/dal/ee24cxx/ee24cxx_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/ee24cxx/ee24cxx_drv.c"

$(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(ObjectSuffix): ../../vsf/dal/ee93cx6/ee93cx6_drv.c $(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/ee93cx6/ee93cx6_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(DependSuffix): ../../vsf/dal/ee93cx6/ee93cx6_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/ee93cx6/ee93cx6_drv.c"

$(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(PreprocessSuffix): ../../vsf/dal/ee93cx6/ee93cx6_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/ee93cx6/ee93cx6_drv.c"

$(IntermediateDirectory)/mic2826_mic2826_drv$(ObjectSuffix): ../../vsf/dal/mic2826/mic2826_drv.c $(IntermediateDirectory)/mic2826_mic2826_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/mic2826/mic2826_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/mic2826_mic2826_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/mic2826_mic2826_drv$(DependSuffix): ../../vsf/dal/mic2826/mic2826_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/mic2826_mic2826_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/mic2826_mic2826_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/mic2826/mic2826_drv.c"

$(IntermediateDirectory)/mic2826_mic2826_drv$(PreprocessSuffix): ../../vsf/dal/mic2826/mic2826_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/mic2826_mic2826_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/mic2826/mic2826_drv.c"

$(IntermediateDirectory)/nand_nand_drv$(ObjectSuffix): ../../vsf/dal/nand/nand_drv.c $(IntermediateDirectory)/nand_nand_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/nand/nand_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/nand_nand_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/nand_nand_drv$(DependSuffix): ../../vsf/dal/nand/nand_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/nand_nand_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/nand_nand_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/nand/nand_drv.c"

$(IntermediateDirectory)/nand_nand_drv$(PreprocessSuffix): ../../vsf/dal/nand/nand_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/nand_nand_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/nand/nand_drv.c"

$(IntermediateDirectory)/sd_sd_common$(ObjectSuffix): ../../vsf/dal/sd/sd_common.c $(IntermediateDirectory)/sd_sd_common$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/sd/sd_common.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/sd_sd_common$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/sd_sd_common$(DependSuffix): ../../vsf/dal/sd/sd_common.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/sd_sd_common$(ObjectSuffix) -MF$(IntermediateDirectory)/sd_sd_common$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/sd/sd_common.c"

$(IntermediateDirectory)/sd_sd_common$(PreprocessSuffix): ../../vsf/dal/sd/sd_common.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/sd_sd_common$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/sd/sd_common.c"

$(IntermediateDirectory)/sd_sd_spi_drv$(ObjectSuffix): ../../vsf/dal/sd/sd_spi_drv.c $(IntermediateDirectory)/sd_sd_spi_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/sd/sd_spi_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/sd_sd_spi_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/sd_sd_spi_drv$(DependSuffix): ../../vsf/dal/sd/sd_spi_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/sd_sd_spi_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/sd_sd_spi_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/sd/sd_spi_drv.c"

$(IntermediateDirectory)/sd_sd_spi_drv$(PreprocessSuffix): ../../vsf/dal/sd/sd_spi_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/sd_sd_spi_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/sd/sd_spi_drv.c"

$(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(ObjectSuffix): ../../vsf/dal/nrf24l01/nrf24l01_drv.c $(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "F:/MyProject/versaloon/vsprog/vsf/dal/nrf24l01/nrf24l01_drv.c" $(C_CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(DependSuffix): ../../vsf/dal/nrf24l01/nrf24l01_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(ObjectSuffix) -MF$(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(DependSuffix) -MM "F:/MyProject/versaloon/vsprog/vsf/dal/nrf24l01/nrf24l01_drv.c"

$(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(PreprocessSuffix): ../../vsf/dal/nrf24l01/nrf24l01_drv.c
	@$(C_CompilerName) $(C_CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(PreprocessSuffix) "F:/MyProject/versaloon/vsprog/vsf/dal/nrf24l01/nrf24l01_drv.c"


-include $(IntermediateDirectory)/*$(DependSuffix)
##
## Clean
##
clean:
	$(RM) $(IntermediateDirectory)/src_vsprog$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/src_vsprog$(DependSuffix)
	$(RM) $(IntermediateDirectory)/src_vsprog$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/programmer_programmer$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/programmer_programmer$(DependSuffix)
	$(RM) $(IntermediateDirectory)/programmer_programmer$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/interfaces_interfaces$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/interfaces_interfaces$(DependSuffix)
	$(RM) $(IntermediateDirectory)/interfaces_interfaces$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/versaloon_versaloon$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/versaloon_versaloon$(DependSuffix)
	$(RM) $(IntermediateDirectory)/versaloon_versaloon$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoadc$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoadc$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoadc$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtobdm$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtobdm$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtobdm$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoc2$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoc2$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoc2$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtodusi$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtodusi$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtodusi$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoebi$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoebi$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoebi$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtogpio$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtogpio$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtogpio$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoi2c$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoi2c$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoi2c$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoissp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoissp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoissp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtaghl$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtagll$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtagll$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtagll$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtojtagraw$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtolpcicp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomicrowire$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomsp430jtag$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtomsp430sbw$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtopwm$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtopwm$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtopwm$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtopwr$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtopwr$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtopwr$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtospi$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtospi$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtospi$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoswd$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoswd$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoswd$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoswim$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoswim$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoswim$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtousart$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtousart$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtousart$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoxxx$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoxxx$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usbtoxxx_usbtoxxx$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/vi_stm32_vi_stm32$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/vi_stm32_vi_stm32$(DependSuffix)
	$(RM) $(IntermediateDirectory)/vi_stm32_vi_stm32$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/scripts_app_scripts$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/scripts_app_scripts$(DependSuffix)
	$(RM) $(IntermediateDirectory)/scripts_app_scripts$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/scripts_interfaces_script$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/scripts_interfaces_script$(DependSuffix)
	$(RM) $(IntermediateDirectory)/scripts_interfaces_script$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/scripts_scripts$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/scripts_scripts$(DependSuffix)
	$(RM) $(IntermediateDirectory)/scripts_scripts$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/mic2826_script_mic2826_script$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/mic2826_script_mic2826_script$(DependSuffix)
	$(RM) $(IntermediateDirectory)/mic2826_script_mic2826_script$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/driver_port$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/driver_port$(DependSuffix)
	$(RM) $(IntermediateDirectory)/driver_port$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/usb_usbapi$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/usb_usbapi$(DependSuffix)
	$(RM) $(IntermediateDirectory)/usb_usbapi$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/comport_comport$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/comport_comport$(DependSuffix)
	$(RM) $(IntermediateDirectory)/comport_comport$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/filelist_filelist$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/filelist_filelist$(DependSuffix)
	$(RM) $(IntermediateDirectory)/filelist_filelist$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/fileparser_fileparser$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/fileparser_fileparser$(DependSuffix)
	$(RM) $(IntermediateDirectory)/fileparser_fileparser$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/hex_hex$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/hex_hex$(DependSuffix)
	$(RM) $(IntermediateDirectory)/hex_hex$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/memlist_memlist$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/memlist_memlist$(DependSuffix)
	$(RM) $(IntermediateDirectory)/memlist_memlist$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/pgbar_pgbar$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/pgbar_pgbar$(DependSuffix)
	$(RM) $(IntermediateDirectory)/pgbar_pgbar$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/s19_s19$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/s19_s19$(DependSuffix)
	$(RM) $(IntermediateDirectory)/s19_s19$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/strparser_strparser$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/strparser_strparser$(DependSuffix)
	$(RM) $(IntermediateDirectory)/strparser_strparser$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/target_target$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/target_target$(DependSuffix)
	$(RM) $(IntermediateDirectory)/target_target$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/at89s5x_at89s5x$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/at89s5x_at89s5x$(DependSuffix)
	$(RM) $(IntermediateDirectory)/at89s5x_at89s5x$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/at91sam3_at91sam3$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/at91sam3_at91sam3$(DependSuffix)
	$(RM) $(IntermediateDirectory)/at91sam3_at91sam3$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_hvpp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_hvpp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_hvpp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_hvsp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_hvsp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_hvsp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_isp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_isp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_isp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_jtag$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_jtag$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avr8_avr8_jtag$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avr32_avr32$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avr32_avr32$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avr32_avr32$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/avrxmega_avrxmega$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/avrxmega_avrxmega$(DependSuffix)
	$(RM) $(IntermediateDirectory)/avrxmega_avrxmega$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f$(DependSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f_c2$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f_c2$(DependSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f_c2$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f_jtag$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f_jtag$(DependSuffix)
	$(RM) $(IntermediateDirectory)/c8051f_c8051f_jtag$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cfi_cfi$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cfi_cfi$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cfi_cfi$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/comisp_comisp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/comisp_comisp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/comisp_comisp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/comisp_lpcarmisp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/comisp_lpcarmisp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/comisp_lpcarmisp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/comisp_stm32isp$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/comisp_stm32isp$(DependSuffix)
	$(RM) $(IntermediateDirectory)/comisp_stm32isp$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/arm_adi_adi_v5p1$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/arm_adi_adi_v5p1$(DependSuffix)
	$(RM) $(IntermediateDirectory)/arm_adi_adi_v5p1$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_at91sam3$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_common$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_common$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_common$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_lm3s$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_lm3s$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_lm3s$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_lpc1000$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32_fl$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32f1$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32f2$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cortex-m3_cm3_stm32l1$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/df25xx_df25xx$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/df25xx_df25xx$(DependSuffix)
	$(RM) $(IntermediateDirectory)/df25xx_df25xx$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/ee24cxx_ee24cxx$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/ee24cxx_ee24cxx$(DependSuffix)
	$(RM) $(IntermediateDirectory)/ee24cxx_ee24cxx$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/ee93cx6_ee93cx6$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/ee93cx6_ee93cx6$(DependSuffix)
	$(RM) $(IntermediateDirectory)/ee93cx6_ee93cx6$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/hcs08_hcs08$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/hcs08_hcs08$(DependSuffix)
	$(RM) $(IntermediateDirectory)/hcs08_hcs08$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/hcs12_hcs12$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/hcs12_hcs12$(DependSuffix)
	$(RM) $(IntermediateDirectory)/hcs12_hcs12$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/lm3s_lm3s$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/lm3s_lm3s$(DependSuffix)
	$(RM) $(IntermediateDirectory)/lm3s_lm3s$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/lpc900_lpc900$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/lpc900_lpc900$(DependSuffix)
	$(RM) $(IntermediateDirectory)/lpc900_lpc900$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/lpc1000_lpc1000$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/lpc1000_lpc1000$(DependSuffix)
	$(RM) $(IntermediateDirectory)/lpc1000_lpc1000$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/msp430_JTAGfunc$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/msp430_JTAGfunc$(DependSuffix)
	$(RM) $(IntermediateDirectory)/msp430_JTAGfunc$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430$(DependSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430_bsl$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430_bsl$(DependSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430_bsl$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430_jtagsbw$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430_jtagsbw$(DependSuffix)
	$(RM) $(IntermediateDirectory)/msp430_msp430_jtagsbw$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/nand_nand$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/nand_nand$(DependSuffix)
	$(RM) $(IntermediateDirectory)/nand_nand$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/psoc1_psoc1$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/psoc1_psoc1$(DependSuffix)
	$(RM) $(IntermediateDirectory)/psoc1_psoc1$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd$(DependSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/stm8_stm8$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/stm8_stm8$(DependSuffix)
	$(RM) $(IntermediateDirectory)/stm8_stm8$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/stm32f1_stm32f1$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/stm32f1_stm32f1$(DependSuffix)
	$(RM) $(IntermediateDirectory)/stm32f1_stm32f1$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/stm32f2_stm32f2$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/stm32f2_stm32f2$(DependSuffix)
	$(RM) $(IntermediateDirectory)/stm32f2_stm32f2$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/stm32l1_stm32l1$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/stm32l1_stm32l1$(DependSuffix)
	$(RM) $(IntermediateDirectory)/stm32l1_stm32l1$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_byte_tap$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_byte_tap$(DependSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_byte_tap$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_svf_parser$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_svf_parser$(DependSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_svf_parser$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_svf_player$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_svf_player$(DependSuffix)
	$(RM) $(IntermediateDirectory)/svf_player_svf_player$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/target_target_readconf$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/target_target_readconf$(DependSuffix)
	$(RM) $(IntermediateDirectory)/target_target_readconf$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/dal_dal$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/dal_dal$(DependSuffix)
	$(RM) $(IntermediateDirectory)/dal_dal$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/mal_mal$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/mal_mal$(DependSuffix)
	$(RM) $(IntermediateDirectory)/mal_mal$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/cfi_cfi_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/cfi_cfi_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/cfi_cfi_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/df25xx_df25xx_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/df25xx_df25xx_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/df25xx_df25xx_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/df45xx_df45xx_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/df45xx_df45xx_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/df45xx_df45xx_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/ee24cxx_ee24cxx_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/ee93cx6_ee93cx6_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/mic2826_mic2826_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/mic2826_mic2826_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/mic2826_mic2826_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/nand_nand_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/nand_nand_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/nand_nand_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd_common$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd_common$(DependSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd_common$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd_spi_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd_spi_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/sd_sd_spi_drv$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(DependSuffix)
	$(RM) $(IntermediateDirectory)/nrf24l01_nrf24l01_drv$(PreprocessSuffix)
	$(RM) $(OutputFile)
	$(RM) $(OutputFile).exe
	$(RM) "F:\MyProject\versaloon\vsprog\codelite\.build-debug\vsprog"



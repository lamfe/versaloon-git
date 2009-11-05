unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ExtCtrls, cli_caller{, hexeditor, checkeditor}, Menus,
  Synaser, com_setup;

type

  TTargetType = (TT_NONE, TT_PSOC, TT_AT89S5X, TT_C8051F, TT_MSP430, TT_STM8,
                 TT_EEPROM, TT_JTAG, TT_AVR8, TT_PIC8, TT_COMISP, TT_LPCICP,
                 TT_CORTEXM3);
  { TFormMain }

  TFormMain = class(TForm)
    btnEditFuse: TButton;
    btnEditEE: TButton;
    btnEditUsrSig: TButton;
    btnOpenOCDRun: TButton;
    btnOpenOCDStop: TButton;
    btnUpdate: TButton;
    btnSVFPlayerRun: TButton;
    cbboxOpenOCDInterface: TComboBox;
    cbboxOpenOCDScript: TComboBox;
    cbboxOpenOCDTarget: TComboBox;
    chkboxUsrSig: TCheckBox;
    chkboxMP: TCheckBox;
    chkboxFuse: TCheckBox;
    chkboxEE: TCheckBox;
    cbboxMode: TComboBox;
    cbboxCOM: TComboBox;
    edtSVFOption: TEdit;
    edtOpenOCDOption: TEdit;
    fneditSVFFile: TFileNameEdit;
    fnFW: TFileNameEdit;
    gbChipName: TGroupBox;
    btnEditApp: TButton;
    btnEditLock: TButton;
    btnErase: TButton;
    btnRead: TButton;
    btnTargetDetect: TButton;
    btnVerify: TButton;
    btnWrite: TButton;
    cbboxTarget: TComboBox;
    chkboxApp: TCheckBox;
    chkboxEraseBeforeWrite: TCheckBox;
    chkboxLock: TCheckBox;
    chkboxVerifyAfterWrite: TCheckBox;
    fnEdit: TFileNameEdit;
    gbInputFile: TGroupBox;
    gbOperation: TGroupBox;
    gbOption: TGroupBox;
    gbUpdate: TGroupBox;
    gbOpenOCD: TGroupBox;
    gbSVFPlayer: TGroupBox;
    lblSVFOption: TLabel;
    lblSVFPlayerSelectFile: TLabel;
    lblOpenOCDOption: TLabel;
    lblOpenOCDInterface: TLabel;
    lblOpenOCDTarget: TLabel;
    lblOpenOCDScript: TLabel;
    lbledtFreq: TLabeledEdit;
    lbledtExtraPara: TLabeledEdit;
    lbledtLock: TLabeledEdit;
    lbledtFuse: TLabeledEdit;
    lblMode: TLabel;
    lblInputFile: TLabel;
    lblTarget: TLabel;
    memoAbout: TMemo;
    memoInfo: TMemo;
    memoLog: TMemo;
    miExit: TMenuItem;
    pgbarMain: TProgressBar;
    pnlMain: TPanel;
    pcMain: TPageControl;
    pmTray: TPopupMenu;
    sbMain: TStatusBar;
    tsCortexM3: TTabSheet;
    tsLPCICP: TTabSheet;
    tDelay: TTimer;
    tsCOMISP: TTabSheet;
    tsJTAG: TTabSheet;
    tsPIC8: TTabSheet;
    tsAVR8: TTabSheet;
    tsEEPROM: TTabSheet;
    tiMain: TTrayIcon;
    tsSTM8: TTabSheet;
    tsMSP430: TTabSheet;
    tsAbout: TTabSheet;
    tsAT89S5X: TTabSheet;
    tsC8051F: TTabSheet;
    tsPSoC: TTabSheet;
    procedure btnEditAppClick(Sender: TObject);
    procedure btnEditEEClick(Sender: TObject);
    procedure btnEditFuseClick(Sender: TObject);
    procedure btnEditLockClick(Sender: TObject);
    procedure btnEditUsrSigClick(Sender: TObject);
    procedure btnEraseClick(Sender: TObject);
    procedure btnOpenOCDRunClick(Sender: TObject);
    procedure btnOpenOCDStopClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnSVFPlayerRunClick(Sender: TObject);
    procedure btnTargetDetectClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure cbboxModeChange(Sender: TObject);
    procedure cbboxTargetChange(Sender: TObject);
    procedure edtExtraParaKeyPress(Sender: TObject; var Key: char);
    procedure edtFuseKeyPress(Sender: TObject; var Key: char);
    procedure edtLockKeyPress(Sender: TObject; var Key: char);
    procedure fnEditKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pcMainPageChanged(Sender: TObject);
    procedure AdjustComponentColor(Sender: TObject);
    procedure ShowDebugLog();
    procedure HideDebugLog();
    procedure ToggleIF(Sender: TObject);

    procedure LogInfo(info: string);

    function GetIntegerParameter(line, para_name: string; var value: integer): boolean;
    function GetStringParameter(line, para_name: string; var value: string): boolean;
    function CheckFatalError(line: string): boolean;
    function PrepareToRunCLI(): boolean;
    procedure PrepareCommonParameters();
    procedure tDelayTimer(Sender: TObject);
    procedure tiMainClick(Sender: TObject);
    { VSProg common declarations }
    function VSProg_CommonCallback(line: string): boolean;
    function VSProg_CommonParseSupportCallback(line: string): boolean;
    function VSProg_CommonParseAboutCallback(line: string): boolean;
    function VSProg_CommonParseChipIDCallback(line: string): boolean;
    procedure VSProg_CommonInit(para: string);
    { PSoC declarations }
    function PSoC_Init(): boolean;
    function PSoC_Init_Para(line: string): string;
    procedure PSoC_Update_Chip(p_str: string);
    function PSoC_AddWriteParameters(): boolean;
    function PSoC_AddReadParameters(): boolean;
    function PSoC_AddVerifyParameters(): boolean;
    { C8051F declarations }
    function C8051F_Init(): boolean;
    function C8051F_Init_Para(line: string): string;
    procedure C8051F_Update_Chip(p_str: string);
    function C8051F_AddWriteParameters(): boolean;
    function C8051F_AddReadParameters(): boolean;
    function C8051F_AddVerifyParameters(): boolean;
    { AT89S5X declarations }
    function AT89S5X_Init(): boolean;
    function AT89S5X_Init_Para(line: string): string;
    procedure AT89S5X_Update_Chip(p_str: string);
    function AT89S5X_AddWriteParameters(): boolean;
    function AT89S5X_AddReadParameters(): boolean;
    function AT89S5X_AddVerifyParameters(): boolean;
    { MSP430 declarations }
    function MSP430_Init(): boolean;
    function MSP430_Init_Para(line: string): string;
    procedure MSP430_Update_Chip(p_str: string);
    function MSP430_AddWriteParameters(): boolean;
    function MSP430_AddReadParameters(): boolean;
    function MSP430_AddVerifyParameters(): boolean;
    { STM8 declarations }
    function STM8_Init(): boolean;
    { EEPROM declarations }
    function EEPROM_Init(): boolean;
    { ARM declarations }
    function ARM_Init(): boolean;
    { AVR8 declarations }
    function AVR8_Init(): boolean;
    function AVR8_Init_Para(line: string): string;
    procedure AVR8_Update_Chip(p_str: string);
    procedure AVR8_Update_Mode(m_str: string);
    function AVR8_AddWriteParameters(): boolean;
    function AVR8_AddReadParameters(): boolean;
    function AVR8_AddVerifyParameters(): boolean;
    { PIC8 declarations }
    function PIC8_Init(): boolean;
    { COMISP declarations }
    function COMISP_Init(): boolean;
    function COMISP_Init_Para(line: string): string;
    procedure COMISP_Update_Chip(p_str: string);
    function COMISP_AddWriteParameters(): boolean;
    function COMISP_AddReadParameters(): boolean;
    function COMISP_AddVerifyParameters(): boolean;
    { LPCICP declarations }
    function LPCICP_Init(): boolean;
    function LPCICP_Init_Para(line: string): string;
    procedure LPCICP_Update_Chip(p_str: string);
    function LPCICP_AddWriteParameters(): boolean;
    function LPCICP_AddReadParameters(): boolean;
    function LPCICP_AddVerifyParameters(): boolean;
    { CortexM3 declarations }
    function CortexM3_Init(): boolean;
    function CortexM3_Init_Para(line: string): string;
    procedure CortexM3_Update_chip(p_str: string);
    procedure CortexM3_Update_Mode(m_str: string);
    function CortexM3_AddWriteParameters(): boolean;
    function CortexM3_AddReadParameters(): boolean;
    function CortexM3_AddVerifyParameters(): boolean;
  private
    { private declarations }
    bFatalError: boolean;
    TargetType: TTargetType;
    ParaString: TStringList;
    bPageLock: boolean;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;
  caller: TCLI_Caller;
  OpenOCD_Caller: TCLI_Caller;
  chip_module_header: string;
  ComMode: TComMode;
  ComModeInit: TComMode;

const
  DEBUG_LOG_SHOW: boolean = FALSE;
  DISPLAY_ALL_COMPORT_WHEN_UPDATE = TRUE;
  APP_STR: string = 'vsgui';
  VERSION_STR: string = 'Alpha';
  VSPROG_STR: string = {$IFDEF UNIX}'vsprog'{$ELSE}'vsprog.exe'{$ENDIF};
  OPENOCD_APP_STR: string = {$IFDEF UNIX}'openocd'{$ELSE}'openocd.exe'{$ENDIF};
  EQUAL_STR: string = ' = ';
  SLASH_STR: string = {$IFDEF UNIX}'/'{$ELSE}'\'{$ENDIF};
  COMSETUP_STR: string = 'COM Setup';
  AUTODETECT_STR: string = 'AutoDetect';
  FREQ_STR:string = 'Freq(KHz):';
  EXECUTE_ADDR_STR: string = 'execute:';
  ST_PROG_STR: string = '-U "0x0483 0x5740 0x82 0x03 2"';
  ATMEL_PROG_STR: string = '-U "0x03eb 0x2103 0x82 0x02 0"';
  LOGMEMO_WIDTH: integer = 400;

implementation

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  btnRead.Enabled := FALSE;
  bPageLock := FALSE;

  HideDebugLog();
  chip_module_header := '';
  ParaString := TStringList.Create;

  // caller init
  caller := TCLI_Caller.Create();
  caller.SetApplication(Application.Location + VSPROG_STR);
  caller.SetDelimiter('-');

  OpenOCD_Caller := TCLI_Caller.Create();
  OpenOCD_Caller.SetApplication(Application.Location + OPENOCD_APP_STR);
  OpenOCD_Caller.SetDelimiter('-');

  lblTarget.Top := cbboxTarget.Top + (cbboxTarget.Height - lblTarget.Height) div 2;
  btnTargetDetect.Top := cbboxTarget.Top + (cbboxTarget.Height - btnTargetDetect.Height) div 2;
  lblInputFile.Top := fnEdit.Top + (fnEdit.Height - lblInputFile.Height) div 2;
  lblMode.Top := cbboxMode.Top + (cbboxMode.Height - lblMode.Height) div 2;
  Caption := APP_STR + ' ' + VERSION_STR;

  FormMain.Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
  memoLog.Width := LOGMEMO_WIDTH;

  tiMain.Icon := Application.Icon;
  tiMain.PopUpMenu := pmTray;
  tiMain.Show;

  tsPIC8.TabVisible := FALSE;
  tsSTM8.TabVisible := FALSE;
  tsEEPROM.TabVisible := FALSE;

  pcMain.ActivePage := tsJTAG;
  pcMainPageChanged(Sender);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ParaString.Destroy();
  caller.Destroy();
  OpenOCD_Caller.Destroy();
  tiMain.Hide;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pgbarMain.Height := sbMain.Height - 1;
  pgbarMain.Top := sbMain.Top + 1;
  pgbarMain.Left := sbMain.Panels.Items[0].Width + 2;
  pgbarMain.Width := Width - sbMain.Panels.Items[0].Width - 2;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFormMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ShowDebugLog();
begin
  Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
end;

procedure TFormMain.HideDebugLog();
begin
  if not DEBUG_LOG_SHOW then
  begin
    Width := pnlMain.Width + 1;
  end;
end;

procedure TFormMain.ToggleIF(Sender: TObject);
begin
  if (sender is TEdit) then
  begin
    if ((Sender as TEdit).Text = ST_PROG_STR) or ((Sender as TEdit).Text = '') then
    begin
      (Sender as TEdit).Text := ATMEL_PROG_STR;
    end
    else if (Sender as TEdit).Text = ATMEL_PROG_STR then
    begin
      (Sender as TEdit).Text := ST_PROG_STR;
    end;
  end
  else if (sender is TLabeledEdit) then
  begin
    if ((Sender as TLabeledEdit).Text = ST_PROG_STR) or ((Sender as TLabeledEdit).Text = '') then
    begin
      (Sender as TLabeledEdit).Text := ATMEL_PROG_STR;
    end
    else if (Sender as TLabeledEdit).Text = ATMEL_PROG_STR then
    begin
      (Sender as TLabeledEdit).Text := ST_PROG_STR;
    end;
  end;
end;

procedure TFormMain.pcMainChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if (caller <> nil) and (OpenOCD_Caller <> nil) then
  begin
    AllowChange := (not caller.IsRunning()) and (not OpenOCD_Caller.IsRunning()) and (not tDelay.Enabled) and (not bPageLock);
    if Allowchange then
    begin
      bPageLock := TRUE;
    end;
  end;
end;

procedure TFormMain.pcMainPageChanged(Sender: TObject);
var
  index: integer;
  ser: TBlockSerial;
begin
  if not pcMain.ActivePage.Enabled then
  begin
    HideDebugLog();
    bPageLock := FALSE;
    exit;
  end;

  // initialize GUI
  if pcMain.ActivePage = tsAbout then
  begin
    HideDebugLog();
    TargetType := TT_NONE;

    // Init serial port combobox
    cbboxCOM.Clear;

    for index := 0 to Length(COMPORTS) - 1 do
    begin
      if DISPLAY_ALL_COMPORT_WHEN_UPDATE then
      begin
        cbboxCOM.Items.Add(COMPORTS[index]);
      end
      else
      begin
        ser := TBlockSerial.Create;
        try
          ser.Connect(COMPORTS[index]);
          if ser.LastError = 0 then
          begin
            cbboxCOM.Items.Add(COMPORTS[index]);
          end;
        finally
          ser.Free;
        end;
      end;
    end;

    if cbboxCOM.Items.Count > 0 then
    begin
      cbboxCOM.ItemIndex := 0;
    end;

    bPageLock := FALSE;
    exit;
  end;

  // initialize target
  if pcMain.ActivePage = tsJTAG then
  begin
    TargetType := TT_JTAG;
    ARM_Init();

    memoLog.Clear;
    ShowDebugLog();

    bPageLock := FALSE;
    exit;
  end
  else
  begin
    HideDebugLog();

    if pcMain.ActivePage = tsPSoC then
    begin
      TargetType := TT_PSOC;
    end
    else if pcMain.ActivePage = tsC8051F then
    begin
      TargetType := TT_C8051F;
    end
    else if pcMain.ActivePage = tsAT89S5X then
    begin
      TargetType := TT_AT89S5X;
    end
    else if pcMain.ActivePage = tsMSP430 then
    begin
      TargetType := TT_MSP430;
    end
    else if pcMain.ActivePage = tsSTM8 then
    begin
      TargetType := TT_STM8;
    end
    else if pcMain.ActivePage = tsEEPROM then
    begin
      TargetType := TT_EEPROM;
    end
    else if pcMain.ActivePage = tsAVR8 then
    begin
      TargetType := TT_AVR8;
    end
    else if pcMain.ActivePage = tsPIC8 then
    begin
      TargetType := TT_PIC8;
    end
    else if pcMain.ActivePage = tsCOMISP then
    begin
      TargetType := TT_COMISP;
    end
    else if pcMain.ActivePage = tsLPCICP then
    begin
      TargetType := TT_LPCICP;
    end
    else if pcMain.ActivePage = tsCortexM3 then
    begin
      TargetType := TT_CORTEXM3;
    end
    else
    begin
      // what page? does it exists?
      TargetType := TT_NONE;
    end;
    tDelay.Enabled := TRUE;
  end;
end;

procedure TFormMain.btnTargetDetectClick(Sender: TObject);
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;

  if btnTargetDetect.Caption = COMSETUP_STR then
  begin
    FormComSetup.ShowModal;
    FormComSetup.GetComMode(ComMode);
  end
  else if btnTargetDetect.Caption = AUTODETECT_STR then
  begin
    caller.Take;
    if not PrepareToRunCLI() then
    begin
      caller.UnTake;
      exit;
    end;

    caller.AddParameter('s' + cbboxTarget.Items.Strings[0]);
    if cbboxMode.Enabled and (cbboxMode.Text <> '') then
    begin
      caller.AddParameter('m' + cbboxMode.Text[1]);
    end;
    if lbledtFreq.Enabled and (lbledtFreq.Text <> '') then
    begin
      caller.AddParameter('F' + lbledtFreq.Text);
    end;
    if lbledtExtraPara.Text <> '' then
    begin
      caller.AddParametersString(lbledtExtraPara.Text);
    end;

    LogInfo('Running...');
    caller.Run(@VSProg_CommonParseChipIDCallback, FALSE, TRUE);
    LogInfo('Idle');
  end;
end;

procedure TFormMain.btnUpdateClick(Sender: TObject);
var
  caller_tmp: TCLI_Caller;
const
  update_lock: boolean = FALSE;
begin
  if update_lock then
  begin
    exit;
  end;
  update_lock := TRUE;
  if not FileExists(Application.Location + VSPROG_STR) then
  begin
    MessageDlg('Error, missing vsprog', 'Opps, Where is my vsprog? I cannot work without her.', mtError, [mbOK], 0);
    exit;
  end;
  if cbboxCOM.Text = '' then
  begin
    MessageDlg('Error, no comm port found?', '......', mtError, [mbOK], 0);
    exit;
  end;

  if fnFW.FileName <> '' then
  begin
    caller_tmp := TCLI_Caller.Create;
    caller_tmp.SetApplication(Application.Location + VSPROG_STR);
    caller.SetDelimiter('-');
    caller_tmp.AddParametersString('-G -ccomisp_stm32 -C' + cbboxCOM.Text + ' ' +
                                   ' -x0x08002000 -oe -owf -i"' + fnFW.FileName + '"');

    memoLog.Clear;
    memoInfo.Clear;
    bFatalError := FALSE;
    LogInfo('Running...');
    caller_tmp.Run(@VSProg_CommonCallback, FALSE, TRUE);
    LogInfo('Idle');
    if not bFatalError then
    begin
      MessageDlg('OK', 'FW Updated OK.', mtInformation, [mbOK], 0);
    end;

    caller_tmp.Free;
  end
  else
  begin
    MessageDlg('Error', 'Please sellect FW Hex file.', mtError, [mbOK], 0);
  end;
  update_lock := FALSE;
end;

procedure TFormMain.btnVerifyClick(Sender: TObject);
var
  success: boolean;
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;
  if not PrepareToRunCLI() then
  begin
    caller.UnTake;
    exit;
  end;

  PrepareCommonParameters();

  // operations
  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC:
      success := PSoC_AddVerifyParameters();
    TT_AT89S5X:
      success := AT89S5X_AddVerifyParameters();
    TT_C8051F:
      success := C8051F_AddVerifyParameters();
    TT_AVR8:
      success := AVR8_AddVerifyParameters();
    TT_MSP430:
      success := MSP430_AddVerifyParameters();
    TT_COMISP:
      success := COMISP_AddVerifyParameters();
    TT_LPCICP:
      success := LPCICP_AddVerifyParameters();
    TT_CORTEXM3:
      success := CortexM3_AddVerifyParameters();
  end;

  if not success then
  begin
    MessageDlg('Error', 'Option Error!', mtError, [mbOK], 0);
    caller.UnTake;
    exit;
  end;
  
  LogInfo('Running...');
  caller.Run(@VSProg_CommonCallback, FALSE, TRUE);
  LogInfo('Idle');
end;

procedure TFormMain.btnEraseClick(Sender: TObject);
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;
  if not PrepareToRunCLI() then
  begin
    caller.UnTake;
    exit;
  end;

  PrepareCommonParameters();
  caller.AddParameter('oe');

  LogInfo('Running...');
  caller.Run(@VSProg_CommonCallback, FALSE, TRUE);
  LogInfo('Idle');
end;

procedure TFormMain.btnOpenOCDRunClick(Sender: TObject);
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;

  OpenOCD_Caller.Take;
  if not FileExists(Application.Location + OPENOCD_APP_STR) then
  begin
    MessageDlg('Error, missing OpenOCD', 'Opps, Where is OpenOCD? I cannot work without her.', mtError, [mbOK], 0);
    OpenOCD_Caller.UnTake;
    exit;
  end;

  OpenOCD_Caller.RemoveAllParameters();

  if edtOpenOCDOption.Text <> '' then
  begin
    OpenOCD_Caller.AddParametersString(edtOpenOCDOption.Text);
  end;
  if cbboxOpenOCDInterface.Text <> '' then
  begin
    OpenOCD_Caller.AddParameter('f "' + AnsiToUtf8(Application.Location) + cbboxOpenOCDInterface.Text + '"');
  end;
  if cbboxOpenOCDTarget.Text <> '' then
  begin
    OpenOCD_Caller.AddParameter('f "' + AnsiToUtf8(Application.Location) + cbboxOpenOCDTarget.Text + '"');
  end;
  if cbboxOpenOCDScript.Text <> '' then
  begin
    OpenOCD_Caller.AddParameter('f "' + AnsiToUtf8(Application.Location) + cbboxOpenOCDScript.Text + '"');
  end;

  memoInfo.Clear;
  memoLog.Clear;
  LogInfo('Running...');
  OpenOCD_Caller.Run(@VSProg_CommonCallback, FALSE, FALSE);
  LogInfo('Idle');
  bFatalError := FALSE;
end;

procedure TFormMain.btnOpenOCDStopClick(Sender: TObject);
begin
  OpenOCD_Caller.Stop();
end;

procedure TFormMain.btnEditAppClick(Sender: TObject);
begin
//  FormHexEditor.Caption := (Sender as TButton).Caption;
//  FormHexEditor.ShowModal;
end;

procedure TFormMain.btnEditEEClick(Sender: TObject);
begin
//  FormHexEditor.Caption := (Sender as TButton).Caption;
//  FormHexEditor.ShowModal;
end;

procedure TFormMain.btnEditFuseClick(Sender: TObject);
begin
//  FormCheckEditor.Caption := (Sender as TButton).Caption;
//  FormCheckEditor.ShowModal;
end;

procedure TFormMain.btnEditLockClick(Sender: TObject);
begin
//  FormCheckEditor.Caption := (Sender as TButton).Caption;
//  FormCheckEditor.ShowModal;
end;

procedure TFormMain.btnEditUsrSigClick(Sender: TObject);
begin
//  FormHexEditor.Caption := (Sender as TButton).Caption;
//  FormHexEditor.ShowModal;
end;

procedure TFormMain.btnReadClick(Sender: TObject);
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;
  if not PrepareToRunCLI() then
  begin
    caller.UnTake;
    exit;
  end;
end;

procedure TFormMain.btnSVFPlayerRunClick(Sender: TObject);
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;

  caller.Take;
  if fneditSVFFile.FileName <> '' then
  begin
    if not PrepareToRunCLI() then
    begin
      caller.UnTake;
      exit;
    end;
    caller.AddParametersString('-G -ssvf_player -I"' + fneditSVFFile.FileName + '"');
    if edtSVFOption.Text <> '' then
    begin
      caller.AddParametersString(edtSVFOption.Text);
    end;

    LogInfo('Running...');
    caller.Run(@VSProg_CommonCallback, FALSE, TRUE);
    LogInfo('Idle');
    bFatalError := FALSE;
  end
  else
  begin
    MessageDlg('Error', 'Please input SVF file.', mtError, [mbOK], 0);
    caller.UnTake;
  end;
end;

procedure TFormMain.btnWriteClick(Sender: TObject);
var
  success: boolean;
begin
  if OpenOCD_Caller.IsRunning() or caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;
  if not PrepareToRunCLI() then
  begin
    caller.UnTake;
    exit;
  end;

  PrepareCommonParameters();

  // operations
  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC:
      success := PSoC_AddWriteParameters();
    TT_AT89S5X:
      success := AT89S5X_AddWriteParameters();
    TT_C8051F:
      success := C8051F_AddWriteParameters();
    TT_AVR8:
      success := AVR8_AddWriteParameters();
    TT_MSP430:
      success := MSP430_AddWriteParameters();
    TT_COMISP:
      success := COMISP_AddWriteParameters();
    TT_LPCICP:
      success := LPCICP_AddWriteParameters();
    TT_CORTEXM3:
      success := CortexM3_AddWriteParameters();
  end;

  if not success then
  begin
    MessageDlg('Error', 'Option Error!', mtError, [mbOK], 0);
    caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  caller.Run(@VSProg_CommonCallback, FALSE, TRUE);
  LogInfo('Idle');
end;

procedure TFormMain.cbboxModeChange(Sender: TObject);
begin
  // to do: change the GUI according to different mode
  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC:
      exit;
    TT_AT89S5X:
      exit;
    TT_C8051F:
      exit;
    TT_AVR8:
      AVR8_Update_Mode(cbboxMode.Text);
    TT_CortexM3:
      CortexM3_Update_Mode(cbboxMode.Text);
  end;

  AdjustComponentColor(cbboxMode);
  AdjustComponentColor(lbledtFuse);
  AdjustComponentColor(lbledtLock);
  AdjustComponentColor(lbledtFreq);
end;

procedure TFormMain.cbboxTargetChange(Sender: TObject);
begin
  // to do: change the GUI according to different chip
  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC:
      PSoC_Update_Chip(ParaString.Strings[cbboxTarget.ItemIndex]);
    TT_AT89S5X:
      AT89S5X_Update_Chip(ParaString.Strings[cbboxTarget.ItemIndex]);
    TT_C8051F:
      C8051F_Update_Chip(ParaString.Strings[cbboxTarget.ItemIndex]);
    TT_AVR8:
      AVR8_Update_Chip(ParaString.Strings[cbboxTarget.ItemIndex]);
    TT_MSP430:
      MSP430_Update_Chip(ParaString.Strings[cbboxTarget.ItemIndex]);
    TT_COMISP:
      COMISP_Update_Chip(ParaString.Strings[cbboxTarget.ItemIndex]);
//    TT_LPCICP:
//      LPCICP_Update_chip(ParaString.Strings[cbboxTarget.ItemIndex]);
    TT_CORTEXM3:
      CortexM3_Update_chip(ParaString.Strings[cbboxTarget.ItemIndex]);
  end;

  AdjustComponentColor(cbboxMode);
  AdjustComponentColor(lbledtFuse);
  AdjustComponentColor(lbledtLock);
  AdjustComponentColor(lbledtFreq);
end;

procedure TFormMain.edtExtraParaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    fnEditKeyPress(fnEdit, Key);
  end;
end;

procedure TFormMain.edtFuseKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    fnEditKeyPress(fnEdit, Key);
  end;
end;

procedure TFormMain.edtLockKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    fnEditKeyPress(fnEdit, Key);
  end;
end;

procedure TFormMain.fnEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Pos('show all support', fnEdit.Text) = 1 then
    begin
      tsPSoC.TabVisible := TRUE;
      tsC8051F.TabVisible := TRUE;
      tsAT89S5X.TabVisible := TRUE;
      tsMSP430.TabVisible := TRUE;
      tsSTM8.TabVisible := TRUE;
      fnEdit.Text := '';
    end
    else if Pos('show debug log', fnEdit.Text) = 1 then
    begin
      ShowDebugLog();
      fnEdit.Text := '';
    end
    else if fnEdit.Text = 'exit' then
    begin
      Close;
    end
    else
    begin
      // write the file
      fnEdit.SelectAll;
      btnWrite.Click;
    end;

    Key := Char(0);
  end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  caller.Stop();
  OpenOCD_Caller.Stop();

  CloseAction := caFree;
end;

procedure TFormMain.AdjustComponentColor(Sender: TObject);
begin
  if Sender is TLabeledEdit then
  begin
    if (Sender as TLabeledEdit).Enabled then
    begin
      (Sender as TLabeledEdit).Color := clWindow;
    end
    else
    begin
      (Sender as TLabeledEdit).Color := clBtnFace;
    end;
  end;

  if Sender is TComboBox then
  begin
    if (Sender as TComboBox).Enabled then
    begin
      (Sender as TComboBox).Color := clWindow;
    end
    else
    begin
      (Sender as TComboBox).Color := clBtnFace;
    end;
  end;
end;

procedure TFormMain.LogInfo(info: string);
begin
  memoInfo.Lines.Add(info);
  sbMain.Panels.Items[0].Text := info;

  if info = 'Idle' then
  begin
    sbMain.Panels.Items[1].text := '';
    pgbarmain.Position := 0;
  end;
end;






function TFormMain.GetStringParameter(line, para_name: string; var value: string): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  if Pos(#13 + '', line) > 0 then
  begin
    SetLength(line, Length(line) - 1);
  end;

  pos_start := Pos(para_name + EQUAL_STR, line);
  if pos_start > 0 then
  begin
    str_tmp := Copy(line, pos_start + Length(para_name + EQUAL_STR), Length(line) - pos_start);

    pos_end := Pos(',', str_tmp);
    if pos_end > 1 then
    begin
      str_tmp := Copy(str_tmp, 1, pos_end - 1);
    end;

    value := str_tmp;
    result := TRUE;
  end
  else
  begin
    value := '';
    result := FALSE;
  end;
end;

function TFormMain.GetIntegerParameter(line, para_name: string; var value: integer): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  if Pos(#13 + '', line) > 0 then
  begin
    SetLength(line, Length(line) - 1);
  end;

  pos_start := Pos(para_name + EQUAL_STR, line);
  if pos_start > 0 then
  begin
    str_tmp := Copy(line, pos_start + Length(para_name + EQUAL_STR), Length(line) - pos_start);

    pos_end := Pos(',', str_tmp);
    if pos_end > 1 then
    begin
      str_tmp := Copy(str_tmp, 1, pos_end - 1);
    end;

    value := StrToInt(str_tmp);
    result := TRUE;
  end
  else
  begin
    value := 0;
    result := FALSE;
  end;
end;

function TFormMain.CheckFatalError(line: string): boolean;
begin
  memoLog.Lines.Add(line);

  if (Pos('Error:', line) > 0) and not bFatalError then
  begin
    MessageDlg('Error', line, mtError, [mbOK], 0);
    bFatalError := TRUE;
  end;
  if (Pos('/****Bug****/:', line) = 1) and not bFatalError  then
  begin
    MessageDlg('Bug', line, mtError, [mbOK], 0);
    bFatalError := TRUE;
  end;
  if (Pos('fail', line) <> 0) and not bFatalError then
  begin
    MessageDlg('Error', line, mtError, [mbOK], 0);
    bFatalError := TRUE;
  end;

  result := bFatalError;
end;

function TFormMain.PrepareToRunCLI(): boolean;
begin
  if not FileExists(caller.GetApplication()) then
  begin
    MessageDlg('Error, missing vsprog', 'Opps, Where is my vsprog? I cannot work without her.', mtError, [mbOK], 0);
    result := FALSE;
  end
  else
  begin
    bFatalError := FALSE;
    caller.RemoveAllParameters();
    result := TRUE;
  end;
  memoLog.Clear;
  memoInfo.Clear;
end;

procedure TFormMain.PrepareCommonParameters();
begin
  // enable GUI mode
  caller.AddParameter('G');

  // target series_name or chip_name
  if ((TargetType <> TT_COMISP) and (TargetType <> TT_CORTEXM3)) and (cbboxTarget.ItemIndex = 0) then
  begin
    caller.AddParameter('s' + cbboxTarget.Items.Strings[cbboxTarget.ItemIndex]);
  end
  else
  begin
    caller.AddParameter('c' + cbboxTarget.Items.Strings[cbboxTarget.ItemIndex]);
  end;

  // COM Mode
  if btnTargetDetect.Caption = COMSETUP_STR then
  begin
    if ComMode.comstr = '' then
    begin
      FormComSetup.GetComMode(ComMode);
    end;
    caller.AddParameter('C "' + ComMode.comstr + ':' + IntToStr(ComMode.baudrate) +
                        ' ' + IntToStr(ComMode.datalength) + ComMode.paritybit +
                        ComMode.stopbit + ' ' + ComMode.handshake +
                        ComMode.auxpin + '"');
  end;

  // input file
  if fnEdit.Text <> '' then
  begin
    caller.AddParameter('i"' + fnEdit.Text + '"');
  end;
  
  // extra parameters
  if lbledtExtraPara.Text <> '' then
  begin
    caller.AddParametersString(lbledtExtraPara.Text);
  end;

  // Mode
  if cbboxMode.Enabled and (cbboxMode.Text <> '') then
  begin
    caller.AddParameter('m' + cbboxMode.Text[1]);
  end;

  // Fuse
  if lbledtFuse.Enabled and chkboxFuse.Enabled and chkboxFuse.Checked and (lbledtFuse.Text <> '') then
  begin
    caller.AddParameter('f' + lbledtFuse.Text);
  end;
  
  // Lock
  if lbledtLock.Enabled and chkboxLock.Enabled and chkboxLock.Checked and (lbledtLock.Text <> '') then
  begin
    caller.AddParameter('l' + lbledtLock.Text);
  end;

  // Frequency
  if lbledtFreq.Enabled and (lbledtFreq.Text <> '') then
  begin
    if lbledtFreq.EditLabel.Caption = EXECUTE_ADDR_STR then
    begin
      caller.AddParameter('x' + lbledtFreq.Text);
    end
    else
    begin
      caller.AddParameter('F' + lbledtFreq.Text);
    end;
  end;

  // Mass-product support
  if chkboxMP.Enabled and chkboxMP.Checked then
  begin
    caller.AddParameter('M');
  end;
end;

procedure TFormMain.tDelayTimer(Sender: TObject);
var
  success: boolean;
begin
  tDelay.Enabled := FALSE;

  case TargetType of
    TT_PSOC:
      success := PSoC_Init();
    TT_C8051F:
      success := C8051F_Init();
    TT_AT89S5X:
      success := AT89S5X_Init();
    TT_MSP430:
      success := MSP430_Init();
    TT_STM8:
      success := STM8_Init();
    TT_EEPROM:
      success := EEPROM_Init();
    TT_AVR8:
      success := AVR8_Init();
    TT_PIC8:
      success := PIC8_Init();
    TT_COMISP:
      success := COMISP_Init();
    TT_LPCICP:
      success := LPCICP_Init();
    TT_CORTEXM3:
      success := CortexM3_Init();
  else
      bPageLock := FALSE;
      exit;
  end;

  if success and (cbboxTarget.ItemIndex >= 0) then
  begin
    gbChipName.Parent := pcMain.ActivePage;
    gbInputFile.Parent := pcMain.ActivePage;
    gbOption.Parent := pcMain.ActivePage;
    gbOperation.Parent := pcMain.ActivePage;
    ActiveControl := fnEdit;
    fnEdit.SelectAll;

    cbboxTargetChange(cbboxTarget);

    AdjustComponentColor(cbboxMode);
    AdjustComponentColor(lbledtFuse);
    AdjustComponentColor(lbledtLock);
    AdjustComponentColor(lbledtFreq);
  end
  else
  begin
    TargetType := TT_NONE;
    pcMain.ActivePage.Enabled := FALSE;
  end;
  bPageLock := FALSE;
end;

procedure TFormMain.tiMainClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
end;

function TFormMain.VSProg_CommonCallback(line: string): boolean;
var
  i: integer;
const
  operating: boolean = FALSE;
begin
  if CheckFatalError(line) then
  begin
    result := FALSE;
    exit;
  end;
  result := TRUE;

  if ((Pos('writing', line)  = 1) or
      (Pos('reading', line)  = 1) or
      (Pos('verifying', line) = 1) or
      (Pos('erasing', line)  = 1) or
      (Pos('checking', line) = 1) or
      (Pos('executing', line) = 1)) and
     (Length(line) > 9) then
  begin
    operating := TRUE;
    LogInfo(Copy(line, 1, Length(line) - 8) + '...');
    sbMain.Panels.Items[1].text := '';
    pgbarMain.Position := 0;
  end;

  i := Pos('| ', line);
  if operating and (i > 0) then
  begin
    memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] := memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] + Copy(line, i + 1, Length(line) - i - 1);
    operating := FALSE;
  end;

  if Pos('=', line) = 1 then
  begin
    i := 1;
    while line[i] = PChar('=') do
    begin
      sbMain.Panels.Items[1].text := sbMain.Panels.Items[1].text + '=';
      pgbarMain.Position := pgbarMain.Position + 1;
      Inc(i);
    end;
  end;
end;

function TFormMain.VSProg_CommonParseSupportCallback(line: string): boolean;
var
  chip_name: string;
  str_tmp: string;
begin
  if CheckFatalError(line) then
  begin
    result := FALSE;
    exit;
  end;

  // parse chip name
  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC:
      chip_module_header := 'cy8c';
    TT_AT89S5X:
      chip_module_header := 'at89s';
    TT_C8051F:
      chip_module_header := 'c8051f';
    TT_AVR8:
      chip_module_header := 'at';
    TT_PIC8:
      exit;
    TT_MSP430:
      chip_module_header := 'msp430';
    TT_COMISP:
      chip_module_header := 'comisp_';
    TT_LPCICP:
      chip_module_header := 'p89lpc';
    TT_CORTEXM3:
      chip_module_header := 'cm3_';
  end;

  if Pos(chip_module_header, line) = 1 then
  begin
    chip_name := Copy(line, 1, Pos(':', line) - 1);
  end;

  if chip_name <> '' then
  begin
    // Add special parameter
    str_tmp := '';
    case TargetType of
      TT_NONE:
        exit;
      TT_PSOC:
        str_tmp := PSoC_Init_Para(line);
      TT_AT89S5X:
        str_tmp := AT89S5X_Init_Para(line);
      TT_C8051F:
        str_tmp := C8051F_Init_Para(line);
      TT_AVR8:
        str_tmp := AVR8_Init_Para(line);
      TT_MSP430:
        str_tmp := MSP430_Init_Para(line);
      TT_COMISP:
        str_tmp := COMISP_Init_Para(line);
      TT_LPCICP:
        str_tmp := LPCICP_Init_Para(line);
      TT_CORTEXM3:
        str_tmp := CortexM3_Init_Para(line);
    end;

    cbboxTarget.Items.Add(chip_name);
    ParaString.Add(str_tmp);
  end;
  
  result := TRUE;
end;

function TFormMain.VSProg_CommonParseAboutCallback(line: string): boolean;
begin
  if CheckFatalError(line) then
  begin
    result := FALSE;
    exit;
  end;

  memoAbout.Lines.Add(line);
  result := TRUE;
end;

function TFormMain.VSProg_CommonParseChipIDCallback(line: string): boolean;
var
  chip_name: string;
  chip_name_pos, i: integer;
begin
  if CheckFatalError(line) then
  begin
    result := FALSE;
    exit;
  end;

  chip_name_pos := Pos(' found', line);
  if chip_name_pos >= 8 then
  begin
    // chip found
    chip_name := Copy(line, 9, chip_name_pos - 9);
    for i := 0 to cbboxTarget.Items.Count - 1 do
    begin
      if cbboxTarget.Items.Strings[i] = chip_name then
      begin
        cbboxTarget.ItemIndex := i;
        cbboxTargetChange(cbboxTarget);
        break;
      end;
    end;
    result := TRUE;
  end
  else
  begin
    // chip not found
    result := FALSE;
  end;
end;

procedure TFormMain.VSProg_CommonInit(para: string);
var
  str_tmp: string;
begin
  btnEditApp.Caption := 'Flash';
  chkboxApp.Caption := 'Flash';
  if Pos('f', para) > 0 then
  begin
    btnEditApp.Enabled := TRUE;
    chkboxApp.Enabled := TRUE;
    chkboxApp.Checked := TRUE;
  end
  else
  begin
    btnEditApp.Enabled := FALSE;
    chkboxApp.Enabled := FALSE;
    chkboxApp.Checked := FALSE;
  end;

  btnEditEE.Caption := 'EE';
  chkboxEE.Caption := 'EE';
  if Pos('e', para) > 0 then
  begin
    btnEditEE.Enabled := TRUE;
    chkboxEE.Enabled := TRUE;
    chkboxEE.Checked := TRUE;
  end
  else
  begin
    btnEditEE.Enabled := FALSE;
    chkboxEE.Enabled := FALSE;
    chkboxEE.Checked := FALSE;
  end;

  btnEditLock.Caption := 'Lock';
  chkboxLock.Caption := 'Lock';
  lbledtLock.Text := '';
  if Pos('l', para) > 0 then
  begin
    btnEditLock.Enabled := TRUE;
    lbledtLock.Enabled := TRUE;
    chkboxLock.Enabled := TRUE;
    chkboxLock.Checked := TRUE;
  end
  else
  begin
    btnEditLock.Enabled := FALSE;
    lbledtLock.Enabled := FALSE;
    chkboxLock.Enabled := FALSE;
    chkboxLock.Checked := FALSE;
  end;

  btnEditFuse.Caption := 'Fuse';
  chkboxFuse.Caption := 'Fuse';
  lbledtFuse.Text := '';
  if Pos('u', para) > 0 then
  begin
    btnEditFuse.Enabled := TRUE;
    lbledtFuse.Enabled := TRUE;
    chkboxFuse.Enabled := TRUE;
    chkboxFuse.Checked := TRUE;
  end
  else
  begin
    btnEditFuse.Enabled := FALSE;
    lbledtFuse.Enabled := FALSE;
    chkboxFuse.Enabled := FALSE;
    chkboxFuse.Checked := FALSE;
  end;

  btnEditUsrSig.Caption := 'UsrSig';
  chkboxUsrSig.Caption := 'UsrSig';
  if Pos('s', para) > 0 then
  begin
    btnEditUsrSig.Enabled := TRUE;
    chkboxUsrSig.Enabled := TRUE;
    chkboxUsrSig.Checked := TRUE;
  end
  else
  begin
    btnEditUsrSig.Enabled := FALSE;
    chkboxUsrSig.Enabled := FALSE;
    chkboxUsrSig.Checked := FALSE;
  end;

  chkboxMP.Checked := FALSE;
  if Pos('M', para) > 0 then
  begin
    chkboxMP.Enabled := TRUE;
  end
  else
  begin
    chkboxMP.Enabled := FALSE;
  end;

  str_tmp := lbledtFreq.EditLabel.Caption;
  if (Pos('F', para) > 0) or (Pos('X', para) > 0) then
  begin
    if Pos('F', para) > 0 then
    begin
      lbledtFreq.EditLabel.Caption := FREQ_STR;
    end
    else
    begin
      lbledtFreq.EditLabel.Caption := EXECUTE_ADDR_STR;
    end;
    if lbledtFreq.EditLabel.Caption <> str_tmp then
    begin
      lbledtFreq.Text := '';
    end;
    lbledtFreq.Enabled := TRUE;
  end
  else
  begin
    lbledtFreq.Text := '';
    lbledtFreq.Enabled := FALSE;
  end;

  if (Pos('A', para) > 0) or (Pos('C', para) > 0) then
  begin
    if Pos('C', para) > 0 then
    begin
      btnTargetDetect.Caption := COMSETUP_STR;
    end
    else
    begin
      btnTargetDetect.Caption := AUTODETECT_STR;
    end;
    btnTargetDetect.Enabled := TRUE;
  end
  else
  begin
    btnTargetDetect.Caption := 'No use';
    btnTargetDetect.Enabled := FALSE;
  end;

  chkboxEraseBeforeWrite.Caption := 'Erase before write';
  chkboxEraseBeforeWrite.Enabled := TRUE;
  chkboxVerifyAfterWrite.Caption := 'Verify after write';
  chkboxVerifyAfterWrite.Enabled := TRUE;

  cbboxMode.Clear;
  cbboxMode.Enabled := FALSE;
end;

{ PSoC implementation }
function TFormMain.PSoC_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked or chkboxLock.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 's';
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked and chkboxApp.Checked then
  begin
    PSoC_AddVerifyParameters();
  end;
  
  result := TRUE;
end;

function TFormMain.PSoC_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.PSoC_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'fc';
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

procedure TFormMain.PSoC_Update_Chip(p_str: string);
var
  str_tmp: string;
begin
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('r', p_str) > 0 then
  begin
    cbboxMode.Items.Add('r:Reset');
  end;
  if Pos('p', p_str) > 0 then
  begin
    cbboxMode.Items.Add('p:Power-On');
  end;

  if cbboxMode.Items.IndexOf(str_tmp) >= 0 then
  begin
    cbboxMode.ItemIndex := cbboxMode.Items.IndexOf(str_tmp);
  end
  else
  begin
    cbboxMode.ItemIndex := 0;
  end;
end;

function TFormMain.PSoC_Init_Para(line: string): string;
var
  int_tmp: integer;
begin
  result := '';
  int_tmp := 0;
  GetIntegerParameter(line, 'init_mode', int_tmp);
  if (int_tmp and $01) = $01 then
  begin
    result := result + 'r';
  end;
  if (int_tmp and $02) = $02 then
  begin
    result := result + 'p';
  end;
end;

function TFormMain.PSoC_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('psoc');
  ParaString.Clear;
  ParaString.Add('rp');

  // call 'vsprog -Spsoc' to extract supported psoc targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Spsoc');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsPSoC.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, lock, Mass-product, Autodetect
//  VSProg_CommonInit('flAM');
  VSProg_CommonInit('flA');

  btnEditLock.Caption := 'Secure';
  lbledtLock.Enabled := FALSE;
  chkboxLock.Caption := 'Secure';
  cbboxMode.Enabled := TRUE;
end;

{ C8051F implementations }
function TFormMain.C8051F_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oef');
  end;

  if chkboxApp.Checked then
  begin
    para_tmp := 'owf';
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked and chkboxApp.Checked then
  begin
    C8051F_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.C8051F_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.C8051F_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

function TFormMain.C8051F_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('c8051f');
  ParaString.Clear;
  ParaString.Add('jc');

  // call 'vsprog -Sc8051f' to check support
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Sc8051f');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsAT89S5X.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, Autodetect
  VSProg_CommonInit('fA');
  cbboxMode.Enabled := TRUE;
end;

procedure TFormMain.C8051F_Update_Chip(p_str: string);
begin
  cbboxMode.Items.Clear;
  if Pos('j', p_str) > 0 then
  begin
    cbboxMode.Items.Add('j:JTAG');
  end;
  if Pos('c', p_str) > 0 then
  begin
    cbboxMode.Items.Add('c:C2');
  end;
  cbboxMode.ItemIndex := 0;
end;

function TFormMain.C8051F_Init_Para(line: string): string;
var
  int_tmp: integer;
begin
  result := '';
  int_tmp := 0;
  GetIntegerParameter(line, 'prog_mode', int_tmp);
  if int_tmp = 2 then
  begin
    result := result + 'c';
  end
  else
  begin
    result := result + 'j';
  end;
end;

{ AT89S5X implementations }
function TFormMain.AT89S5X_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked or chkboxLock.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 'l';
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked then
  begin
    AT89S5X_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.AT89S5X_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.AT89S5X_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked and not chkboxLock.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 'l'
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

procedure TFormMain.AT89S5X_Update_Chip(p_str: string);
begin
  if Pos('f', p_str) > 0 then
  begin
    lbledtFuse.Enabled := TRUE;
    btnEditFuse.Enabled := TRUE;
    chkboxFuse.Checked := FALSE;
    chkboxFuse.Enabled := TRUE;
  end
  else
  begin
    lbledtFuse.Enabled := FALSE;
    btnEditFuse.Enabled := FALSE;
    chkboxFuse.Checked := FALSE;
    chkboxFuse.Enabled := FALSE;
  end;
  if Pos('u', p_str) > 0 then
  begin
    btnEditUsrSig.Enabled := TRUE;
  end
  else
  begin
    btnEditUsrSig.Enabled := FALSE;
  end;
end;

function TFormMain.AT89S5X_Init_Para(line: string): string;
var
  int_tmp: integer;
begin
  result := '';
  int_tmp := 0;
  GetIntegerParameter(line, 'fuse_size', int_tmp);
  if int_tmp > 0 then
  begin
    result := result + 'f';
  end;
  GetIntegerParameter(line, 'usrsig_size', int_tmp);
  if int_tmp > 0 then
  begin
    result := result + 'u';
  end;
end;

function TFormMain.AT89S5X_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('at89s5x');
  ParaString.Clear;
  ParaString.Add('fu');

  // call 'vsprog -Sat89s5x' to extract supported at89s5x targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Sat89s5x');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsAT89S5X.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, lock, Frequency, Autodetect
  VSProg_CommonInit('flFA');

  lbledtLock.Text := '1';
  cbboxMode.Items.Add('p:Page');
  cbboxMode.Items.Add('b:Byte');
  cbboxMode.ItemIndex := 0;
  cbboxMode.Enabled := TRUE;
end;

{ MSP430 implementations }
function TFormMain.MSP430_Init_Para(line: string): string;
var
  int_tmp: integer;
begin
  result := '';
  int_tmp := 0;
  GetIntegerParameter(line, 'prog_mode', int_tmp);
  if (int_tmp and $01) = $01 then
  begin
    result := result + 'j';
  end;
  if (int_tmp and $02) = $02 then
  begin
    result := result + 's';
  end;
  if (int_tmp and $04) = $04 then
  begin
    result := result + 'b';
  end;
end;

function TFormMain.MSP430_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('msp430');
  ParaString.Clear;
  ParaString.Add('jsb');

  // call 'vsprog -Smsp430' to extract supported at89s5x targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Smsp430');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsMSP430.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, Autodetect
  VSProg_CommonInit('fA');

  cbboxMode.Enabled := TRUE;
end;

procedure TFormMain.MSP430_Update_Chip(p_str: string);
var
  str_tmp: string;
begin
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('j', p_str) > 0 then
  begin
    cbboxMode.Items.Add('j:JTAG');
  end;
  if Pos('s', p_str) > 0 then
  begin
    cbboxMode.Items.Add('s:SBW');
  end;
  if Pos('b', p_str) > 0 then
  begin
    cbboxMode.Items.Add('b:BSL');
  end;

  if cbboxMode.Items.IndexOf(str_tmp) >= 0 then
  begin
    cbboxMode.ItemIndex := cbboxMode.Items.IndexOf(str_tmp);
  end
  else
  begin
    cbboxMode.ItemIndex := 0;
  end;
end;

function TFormMain.MSP430_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked then
  begin
    MSP430_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.MSP430_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.MSP430_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

{ STM8 implementations }
function TFormMain.STM8_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('stm8');
  ParaString.Clear;

  // call 'vsprog -Smsp430' to extract supported at89s5x targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Sstm8');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsSTM8.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, Autodetect
  VSProg_CommonInit('fA');
end;

{ EEPROM implementations }
function TFormMain.EEPROM_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('eeprom_i2c');
  cbboxTarget.Items.Add('eeprom_spi');
  ParaString.Clear;

  // call 'vsprog -Smsp430' to extract supported at89s5x targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Seeprom_i2c');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsEEPROM.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, Autodetect
  VSProg_CommonInit('e');
end;

{ ARM functions }
function TFormMain.ARM_Init(): boolean;
var
  SearchResult : TSearchRec;
  index: integer;
begin
  if (cbboxOpenOCDInterface.Items.Count = 0) and DirectoryExists(Application.Location + 'interface') then
  begin
    if FindFirst(Application.Location + 'interface' + SLASH_STR + '*',
                 (faAnyFile and not faDirectory),
                 SearchResult) = 0 then
    begin
      if ExtractFileExt(SearchResult.Name) = '.cfg' then
      begin
        cbboxOpenOCDInterface.Items.Add('interface' + SLASH_STR + SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if ExtractFileExt(SearchResult.Name) = '.cfg' then
        begin
          cbboxOpenOCDInterface.Items.Add('interface' + SLASH_STR + SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDInterface.Items.Count > 0 then
    begin
      index := cbboxOpenOCDInterface.Items.IndexOf('interface' + SLASH_STR + 'vsllink.cfg');
      if index > 0 then
      begin
        cbboxOpenOCDInterface.ItemIndex := index;
      end
      else
      begin
        cbboxOpenOCDInterface.ItemIndex := 0;
      end;
    end;
  end;

  if (cbboxOpenOCDTarget.Items.Count = 0) and  DirectoryExists(Application.Location + 'target') then
  begin
    if FindFirst(Application.Location + 'target' + SLASH_STR + '*',
                 (faAnyFile and not faDirectory),
                 SearchResult) = 0 then
    begin
      if ExtractFileExt(SearchResult.Name) = '.cfg' then
      begin
        cbboxOpenOCDTarget.Items.Add('target' + SLASH_STR + SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if ExtractFileExt(SearchResult.Name) = '.cfg' then
        begin
          cbboxOpenOCDTarget.Items.Add('target' + SLASH_STR + SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDTarget.Items.Count > 0 then
    begin
      cbboxOpenOCDTarget.ItemIndex := 0;
    end;
  end;

  if (cbboxOpenOCDScript.Items.Count = 0) and  DirectoryExists(Application.Location + 'script') then
  begin
    if FindFirst(Application.Location + 'script' + SLASH_STR + '*',
                 (faAnyFile and not faDirectory),
                 SearchResult) = 0 then
    begin
      if ExtractFileExt(SearchResult.Name) = '.cfg' then
      begin
        cbboxOpenOCDScript.Items.Add('script' + SLASH_STR + SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if ExtractFileExt(SearchResult.Name) = '.cfg' then
        begin
          cbboxOpenOCDScript.Items.Add('script' + SLASH_STR + SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDScript.Items.Count > 0 then
    begin
      cbboxOpenOCDScript.ItemIndex := 0;
    end;
  end;

  Result := TRUE;
end;

{ AVR8 implementations }
function TFormMain.AVR8_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('avr8');
  ParaString.Clear;
  ParaString.Add('ijps');

  // call 'vsprog -Sat89s5x' to extract supported at89s5x targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Savr8');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsAVR8.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, eeprom, fuse, lock, Autodetect, Frequency
//  VSProg_CommonInit('feulAF');
  VSProg_CommonInit('fAF');

  lbledtFuse.Text := '0xFFFFFF';
  lbledtLock.Text := '0xFF';
  chkboxEE.Checked := FALSE;
  chkboxLock.Checked := FALSE;
  chkboxFuse.Checked := FALSE;
  cbboxMode.Enabled := TRUE;
end;

function TFormMain.AVR8_Init_Para(line: string): string;
var
  int_tmp: integer;
begin
  result := '';
  int_tmp := 0;
  GetIntegerParameter(line, 'prog_mode', int_tmp);
  if (int_tmp and $01) = $01 then
  begin
    result := result + 'i';
  end;
  if (int_tmp and $02) = $02 then
  begin
    result := result + 'j';
  end;
  if (int_tmp and $04) = $04 then
  begin
    result := result + 'p';
  end;
  if (int_tmp and $08) = $08 then
  begin
    result := result + 's';
  end;
end;

procedure TFormMain.AVR8_Update_Chip(p_str: string);
var
  str_tmp: string;
begin
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('i', p_str) > 0 then
  begin
    cbboxMode.Items.Add('i:ISP');
  end;
  if Pos('j', p_str) > 0 then
  begin
    cbboxMode.Items.Add('j:JTAG');
  end;
  if Pos('p', p_str) > 0 then
  begin
    cbboxMode.Items.Add('p:HVPP');
  end;
  if Pos('s', p_str) > 0 then
  begin
    cbboxMode.Items.Add('s:HVSP');
  end;

  if cbboxMode.Items.IndexOf(str_tmp) >= 0 then
  begin
    cbboxMode.ItemIndex := cbboxMode.Items.IndexOf(str_tmp);
  end
  else
  begin
    cbboxMode.ItemIndex := 0;
    AVR8_Update_Mode(cbboxMode.Text);
  end;
end;

procedure TFormMain.AVR8_Update_Mode(m_str: string);
begin
  if m_str = 'i:ISP' then
  begin
    lbledtFreq.Enabled := TRUE;
  end
  else
  begin
    lbledtFreq.Enabled := FALSE;
  end;
end;

function TFormMain.AVR8_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.AVR8_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked or chkboxLock.Checked or chkboxEE.Checked or chkboxFuse.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxEE.Checked then
    begin
      para_tmp := para_tmp + 'e';
    end;
    if chkboxFuse.Checked then
    begin
      para_tmp := para_tmp + 'u';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 'l'
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked then
  begin
    AVR8_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.AVR8_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked and not chkboxLock.Checked and not chkboxEE.Checked and not chkboxFuse.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxEE.Checked then
    begin
      para_tmp := para_tmp + 'e';
    end;
    if chkboxFuse.Checked then
    begin
      para_tmp := para_tmp + 'u';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 'l'
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

{ PIC8 implementations }
function TFormMain.PIC8_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('pic8');
  ParaString.Clear;
  ParaString.Add('');

  // call 'vsprog -Spic8' to extract supported pic8 targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Spic8');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsPIC8.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, Autodetect
  VSProg_CommonInit('fA');
end;

{ COMISP declarations }
function TFormMain.COMISP_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  ParaString.Clear;

  // call 'vsprog -Scomisp' to extract supported comisp targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Scomisp');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsCOMISP.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash
  VSProg_CommonInit('CflX');
  chkboxLock.Checked := FALSE;
  lbledtLock.Enabled := FALSE;
  btnEditLock.Enabled := FALSE;
end;

function TFormMain.COMISP_Init_Para(line: string): string;
begin
  result := line;
end;

procedure TFormMain.COMISP_Update_Chip(p_str: string);
var
  str_tmp: string;
begin
  str_tmp := '';

  GetIntegerParameter(p_str, 'baudrate', ComModeInit.baudrate);
  GetIntegerParameter(p_str, 'datalength', ComModeInit.datalength);
  GetStringParameter(p_str, 'paritybit', str_tmp);
  ComModeInit.paritybit := str_tmp[1];
  GetStringParameter(p_str, 'stopbit', str_tmp);
  ComModeInit.stopbit := str_tmp[1];
  GetStringParameter(p_str, 'handshake', str_tmp);
  ComModeInit.handshake := str_tmp[1];
  GetStringParameter(p_str, 'auxpin', str_tmp);
  ComModeInit.auxpin := str_tmp[1];
  FormComSetup.ComInitPara(ComModeInit);
end;

function TFormMain.COMISP_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.COMISP_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked or chkboxLock.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 'l'
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked then
  begin
    COMISP_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.COMISP_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked and not chkboxLock.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    if chkboxLock.Checked then
    begin
      para_tmp := para_tmp + 'l'
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

{ LPCICP implementations }
function TFormMain.LPCICP_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('lpc900');
  ParaString.Clear;
  ParaString.Add('');

  // call 'vsprog -Scomisp' to extract supported comisp targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Slpc900');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsLPCICP.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash, Autodetect
  VSProg_CommonInit('fA');
  chkboxLock.Checked := FALSE;
  lbledtLock.Enabled := FALSE;
  btnEditLock.Enabled := FALSE;
end;

function TFormMain.LPCICP_Init_Para(line: string): string;
begin
  result := line;
end;

procedure TFormMain.LPCICP_Update_Chip(p_str: string);
begin
  p_str := p_str;
end;

function TFormMain.LPCICP_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked then
  begin
    LPCICP_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.LPCICP_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.LPCICP_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

{ CortexM3 implementations }
function TFormMain.CortexM3_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
  ParaString.Clear;

  // call 'vsprog -Scm3' to extract supported comisp targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Scm3');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsCOMISP.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
//    cbboxTargetChange(cbboxTarget);
  end;

  // flash
  VSProg_CommonInit('fF');
  chkboxLock.Checked := FALSE;
  lbledtLock.Enabled := FALSE;
  btnEditLock.Enabled := FALSE;
  cbboxMode.Enabled := TRUE;
end;

function TFormMain.CortexM3_Init_Para(line: string): string;
begin
  line := line;
  result := 'js';
end;

procedure TFormMain.CortexM3_Update_chip(p_str: string);
var
  str_tmp: string;
begin
  p_str := p_str;

  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  cbboxMode.Items.Add('j:JTAG');
  cbboxMode.Items.Add('s:SWJ');

  if (cbboxMode.ItemIndex <> 0) and (cbboxMode.ItemIndex <> 1) then
  begin
    cbboxMode.ItemIndex := 0;
  end;

  if cbboxMode.Items.IndexOf(str_tmp) >= 0 then
  begin
    cbboxMode.ItemIndex := cbboxMode.Items.IndexOf(str_tmp);
  end
  else
  begin
    cbboxMode.ItemIndex := 0;
    CortexM3_Update_Mode(cbboxMode.Text);
  end;
end;

procedure TFormMain.CortexM3_Update_Mode(m_str: string);
begin
  if m_str = 'j:JTAG' then
  begin
    lbledtFreq.Enabled := TRUE;
  end
  else
  begin
    lbledtFreq.Enabled := FALSE;
  end;
end;

function TFormMain.CortexM3_AddWriteParameters(): boolean;
var
  para_tmp: string;
begin
  // Erase before write
  if chkboxEraseBeforeWrite.Checked then
  begin
    caller.AddParameter('oe');
  end;

  if chkboxApp.Checked then
  begin
    para_tmp := 'ow';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
  end
  else
  begin
    result := FALSE;
    exit;
  end;

  // Verify after write
  if chkboxVerifyAfterWrite.Checked then
  begin
    CortexM3_AddVerifyParameters();
  end;

  result := TRUE;
end;

function TFormMain.CortexM3_AddReadParameters(): boolean;
begin
  result := TRUE;
end;

function TFormMain.CortexM3_AddVerifyParameters(): boolean;
var
  para_tmp: string;
begin
  if not chkboxApp.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para_tmp := 'ov';
    if chkboxApp.Checked then
    begin
      para_tmp := para_tmp + 'f';
    end;
    caller.AddParameter(para_tmp);
    result := TRUE;
  end;
end;

initialization
  {$I main.lrs}

end.


unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ExtCtrls, cli_caller, parameditor, Menus, Buttons, Synaser,
  com_setup, fileselector;

type

  TTargetType = (TT_NONE, TT_PSOC1, TT_AT89S5X, TT_C8051F, TT_MSP430, TT_STM8,
                 TT_EEPROM, TT_JTAG, TT_AVR8, TT_PIC8, TT_COMISP, TT_LPCICP,
                 TT_CORTEXM3);

  TTargetSetting = record
    extra: string;
    target: string;
    mode: string;
    fuse_bytelen: integer;
    fuse_default: integer;
    lock_bytelen: integer;
    lock_default: integer;
    cali_bytelen: integer;
    cali_default: integer;
    usrsig_bytelen: integer;
    usrsig_default: integer;
  end;

  TTargetFileSetting = record
    target: string;
    filename: string;
    seg_offset: integer;
    addr_offset: integer;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    btnEditFuse: TButton;
    btnEditEE: TButton;
    btnEditUsrSig: TButton;
    btnOpenOCDRun: TButton;
    btnOpenOCDStop: TButton;
    btnUpdate: TButton;
    btnSVFPlayerRun: TButton;
    btnEditCali: TButton;
    btnOpenFile: TButton;
    cbboxOpenOCDInterface: TComboBox;
    cbboxOpenOCDScript: TComboBox;
    cbboxOpenOCDTarget: TComboBox;
    chkboxNowarning: TCheckBox;
    chkboxCali: TCheckBox;
    chkboxNoconnect: TCheckBox;
    chkboxUsrSig: TCheckBox;
    chkboxMP: TCheckBox;
    chkboxFuse: TCheckBox;
    chkboxEE: TCheckBox;
    cbboxMode: TComboBox;
    cbboxCOM: TComboBox;
    cbboxInputFile: TComboBox;
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
    gbInputFile: TGroupBox;
    gbOperation: TGroupBox;
    gbOption: TGroupBox;
    gbUpdate: TGroupBox;
    gbOpenOCD: TGroupBox;
    gbSVFPlayer: TGroupBox;
    lbledtCali: TLabeledEdit;
    lbledtUsrSig: TLabeledEdit;
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
    odInputFile: TOpenDialog;
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
    tsAVR8: TTabSheet;
    tsEEPROM: TTabSheet;
    tiMain: TTrayIcon;
    tsSTM8: TTabSheet;
    tsMSP430: TTabSheet;
    tsAbout: TTabSheet;
    tsAT89S5X: TTabSheet;
    tsC8051F: TTabSheet;
    tsPSoC1: TTabSheet;
    procedure btnEditAppClick(Sender: TObject);
    procedure btnEditCaliClick(Sender: TObject);
    procedure btnEditEEClick(Sender: TObject);
    procedure btnEditFuseClick(Sender: TObject);
    procedure btnEditLockClick(Sender: TObject);
    procedure btnEditUsrSigClick(Sender: TObject);
    procedure btnEraseClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenOCDRunClick(Sender: TObject);
    procedure btnOpenOCDStopClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnSVFPlayerRunClick(Sender: TObject);
    procedure btnTargetDetectClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure cbboxInputFileChange(Sender: TObject);
    procedure cbboxModeChange(Sender: TObject);
    procedure cbboxTargetChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbledtExtraParaKeyPress(Sender: TObject; var Key: char);
    procedure miExitClick(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pcMainPageChanged(Sender: TObject);
    procedure AdjustComponentColor(Sender: TObject);
    procedure ShowDebugLog();
    procedure HideDebugLog();
    procedure ToggleIF(Sender: TObject);

    procedure LogInfo(info: string);

    function CheckFatalError(line: string): boolean;
    function PrepareToRunCLI(): boolean;
    function GetTargetDefineParameters(): string;
    procedure PrepareBaseParameters();
    procedure PrepareCommonParameters();
    procedure tDelayTimer(Sender: TObject);
    procedure tiMainClick(Sender: TObject);
    { VSProg common declarations }
    function VSProg_CommonCallback(line: string): boolean;
    function VSProg_CommonParseParaCallback(line: string): boolean;
    function VSProg_CommonParseReadTargetCallback(line, target: string; var result_str: string): boolean;
    function VSProg_CommonParseReadFuseCallback(line: string): boolean;
    function VSProg_CommonParseReadLockCallback(line: string): boolean;
    function VSProg_CommonParseReadCaliCallback(line: string): boolean;
    procedure VSProg_TargetSettingInit(var setting: TTargetSetting);
    procedure VSProg_AddTargetSetting(setting: TTargetSetting);
    function VSProg_CommonParseSupportCallback(line: string): boolean;
    function VSProg_CommonParseAboutCallback(line: string): boolean;
    function VSProg_CommonParseChipIDCallback(line: string): boolean;
    procedure VSProg_CommonTargetInit(para: string);
    procedure VSProg_CommonInit(para: string);
    procedure VSProg_CommonUpdateSetting(value, bytelen: integer; edt: TLabeledEdit);
    procedure VSProg_CommonUpdateFuse(fuse, bytelen: integer);
    procedure VSProg_CommonUpdateLock(lock, bytelen: integer);
    procedure VSProg_CommonUpdateUsrSig(sig, bytelen: integer);
    procedure VSProg_CommonUpdateCali(cali, bytelen: integer);
    function VSProg_CommonGetEnabledOperationString(): string;
    function VSProg_CommonAddEraseOperation(): boolean;
    function VSProg_CommonAddWriteOperation(): boolean;
    function VSProg_CommonAddVerifyOperation(): boolean;
    function VSProg_CommonAddReadOperation(): boolean;
    { PSoC1 declarations }
    function PSoC1_Init(): boolean;
    function PSoC1_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure PSoC1_Update_Chip(setting: TTargetSetting);
    function PSoC1_AddVerifyParameters(var para: string): boolean;
    { C8051F declarations }
    function C8051F_Init(): boolean;
    function C8051F_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure C8051F_Update_Chip(setting: TTargetSetting);
    { AT89S5X declarations }
    function AT89S5X_Init(): boolean;
    function AT89S5X_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure AT89S5X_Update_Chip(setting: TTargetSetting);
    { MSP430 declarations }
    function MSP430_Init(): boolean;
    function MSP430_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure MSP430_Update_Chip(setting: TTargetSetting);
    { STM8 declarations }
    function STM8_Init(): boolean;
    { EEPROM declarations }
    function EEPROM_Init(): boolean;
    { ARM declarations }
    function ARM_Init(): boolean;
    { AVR8 declarations }
    function AVR8_Init(): boolean;
    function AVR8_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure AVR8_Update_Chip(setting: TTargetSetting);
    procedure AVR8_Update_Mode(m_str: string);
    { COMISP declarations }
    function COMISP_Init(): boolean;
    function COMISP_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure COMISP_Update_Chip(setting: TTargetSetting);
    { LPCICP declarations }
    function LPCICP_Init(): boolean;
    function LPCICP_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure LPCICP_Update_Chip(setting: TTargetSetting);
    { CortexM3 declarations }
    function CortexM3_Init(): boolean;
    function CortexM3_Init_Para(line: string; var setting: TTargetSetting): boolean;
    procedure CortexM3_Update_Chip(setting: TTargetSetting);
    procedure CortexM3_Update_Mode(m_str: string);
  private
    { private declarations }
    bFatalError: boolean;
    TargetType: TTargetType;
    TargetSetting: array of TTargetSetting;
    bPageLock: boolean;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;
  caller: TCLI_Caller;
  OpenOCD_Caller: TCLI_Caller;
  ComMode: TComMode;
  ComModeInit: TComMode;
  TargetFile: array of TTargetFileSetting;

const
  DEBUG_LOG_SHOW: boolean = FALSE;
  DISPLAY_ALL_COMPORT_WHEN_UPDATE = TRUE;
  APP_STR: string = 'vsgui';
  VERSION_STR: string = 'Alpha';
  VSPROG_STR: string = {$IFDEF UNIX}'vsprog'{$ELSE}'vsprog.exe'{$ENDIF};
  OPENOCD_APP_STR: string = {$IFDEF UNIX}'openocd'{$ELSE}'openocd.exe'{$ENDIF};
  SLASH_STR: string = {$IFDEF UNIX}'/'{$ELSE}'\'{$ENDIF};
  COMSETUP_STR: string = '&COM Setup';
  COMSETUP_HINT: string = 'Setup COM Port';
  AUTODETECT_STR: string = '&AutoDetect';
  AUTODETECT_HINT: string = 'Detect Target Module';
  FREQ_STR:string = 'Freq(KHz):';
  FREQ_HINT: string = 'Set operation frequency';
  EXECUTE_ADDR_STR: string = 'execute:';
  EXECUTE_ADDR_HINT: string = 'Set address to execute after operation';
  ST_PROG_STR: string = '-U "0x0483 0x5740 0x82 0x03 2"';
  ATMEL_PROG_STR: string = '-U "0x03eb 0x2103 0x82 0x02 0"';
  LOGMEMO_WIDTH: integer = 400;

  FLASH_CHAR: string = 'f';
  EEPROM_CHAR: string = 'e';
  FUSE_CHAR: string = 'u';
  LOCK_CHAR: string = 'l';
  CALI_CHAR: string = 'c';
  USRSIG_CHAR: string = 's';

implementation

procedure ClearTargetFile();
begin
  SetLength(TargetFile, 0);
end;

procedure RemoveTargetFile(name: string);
var
  i, j: integer;
  found: boolean;
begin
  found := FALSE;
  for i := 0 to Length(TargetFile) - 1 do
  begin
    if TargetFile[i].target = name then
    begin
      found := TRUE;
    end;
  end;
  if found then
  begin
    for j := i to Length(TargetFile) - 2 do
    begin
      TargetFile[j] := TargetFile[j + 1];
    end;
    SetLength(TargetFile, Length(TargetFile) - 1);
  end;
end;

procedure AddTargetFile(name: string; seg_offset, addr_offset: integer);
var
  i: integer;
  found: boolean;
begin
  found := FALSE;
  for i := 0 to Length(TargetFile) - 1 do
  begin
    if TargetFile[i].target = name then
    begin
      found := TRUE;
    end;
  end;
  if not found then
  begin
    SetLength(TargetFile, Length(TargetFile) + 1);
    i := Length(TargetFile) - 1;
    TargetFile[i].target := name;
    TargetFile[i].filename := '';
    TargetFile[i].seg_offset := seg_offset;
    TargetFile[i].addr_offset := addr_offset;
  end;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  btnRead.Enabled := FALSE;
  bPageLock := FALSE;

  HideDebugLog();

  // caller init
  caller := TCLI_Caller.Create();
  caller.SetApplication(Application.Location + VSPROG_STR);
  caller.SetDelimiter('-');

  OpenOCD_Caller := TCLI_Caller.Create();
  OpenOCD_Caller.SetApplication(Application.Location + OPENOCD_APP_STR);
  OpenOCD_Caller.SetDelimiter('-');

  lblTarget.Top := cbboxTarget.Top + (cbboxTarget.Height - lblTarget.Height) div 2;
  btnTargetDetect.Top := cbboxTarget.Top + (cbboxTarget.Height - btnTargetDetect.Height) div 2;
  lblInputFile.Top := cbboxInputFile.Top + (cbboxInputFile.Height - lblInputFile.Height) div 2;
  lblMode.Top := cbboxMode.Top + (cbboxMode.Height - lblMode.Height) div 2;
  Caption := APP_STR + ' ' + VERSION_STR;

  FormMain.Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
  memoLog.Width := LOGMEMO_WIDTH;

  tiMain.Icon := Application.Icon;
  tiMain.PopUpMenu := pmTray;
  tiMain.Show;

  tsSTM8.TabVisible := FALSE;
  tsEEPROM.TabVisible := FALSE;

  pcMain.ActivePage := tsJTAG;
  pcMainPageChanged(Sender);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FormParaEditor.FreeRecord();
  caller.Destroy();
  OpenOCD_Caller.Destroy();
  tiMain.Hide;
  SetLength(TargetSetting, 0);
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
  UpdateShowing;
end;

procedure TFormMain.lbledtExtraParaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Pos('show all support', lbledtExtraPara.Text) = 1 then
    begin
      tsSTM8.TabVisible := TRUE;
      tsEEPROM.TabVisible := TRUE;
      lbledtExtraPara.Text := '';
    end
    else if Pos('show debug log', lbledtExtraPara.Text) = 1 then
    begin
      ShowDebugLog();
      lbledtExtraPara.Text := '';
    end
    else if lbledtExtraPara.Text = 'exit' then
    begin
      Close;
    end;

    Key := #0;
  end;
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

  SetLength(TargetSetting, 0);
  ClearTargetFile();
  cbboxInputFile.Clear;

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

    if pcMain.ActivePage = tsPSoC1 then
    begin
      TargetType := TT_PSOC1;
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
    caller_tmp.AddParametersString('-G -Z -ccomisp_stm32 -C' + cbboxCOM.Text + ' ' +
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

function TFormMain.VSProg_CommonGetEnabledOperationString(): string;
begin
  result := '';
  if chkboxApp.Enabled and chkboxApp.Checked then
  begin
    result := result + FLASH_CHAR;
  end;
  if chkboxEE.Enabled and chkboxEE.Checked then
  begin
    result := result + EEPROM_CHAR;
  end;
  if chkboxFuse.Enabled and chkboxFuse.Checked then
  begin
    result := result + FUSE_CHAR;
  end;
  if chkboxLock.Enabled and chkboxLock.Checked then
  begin
    result := result + LOCK_CHAR;
  end;
  if chkboxUsrSig.Enabled and chkboxUsrSig.Checked then
  begin
    result := result + USRSIG_CHAR;
  end;
  if chkboxCali.Enabled and chkboxCali.Checked then
  begin
    result := result + CALI_CHAR;
  end;
end;

function TFormMain.VSProg_CommonAddEraseOperation(): boolean;
begin
  caller.AddParameter('oe');
  result := TRUE;
end;

function TFormMain.VSProg_CommonAddWriteOperation(): boolean;
var
  para: string;
begin
  para := VSProg_CommonGetEnabledOperationString();
  if para = '' then
  begin
    result := FALSE;
  end
  else
  begin
    result := TRUE;
    case TargetType of
      TT_PSOC1:
    end;
    para := 'ow' + para;
    if result then
    begin
      caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_CommonAddVerifyOperation(): boolean;
var
  para: string;
begin
  para := VSProg_CommonGetEnabledOperationString();
  if para = '' then
  begin
    result := FALSE;
  end
  else
  begin
    result := TRUE;
    case TargetType of
      TT_PSOC1:
        result := PSoC1_AddVerifyParameters(para);
    end;
    para := 'ov' + para;
    if result then
    begin
      caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_CommonAddReadOperation(): boolean;
var
  para: string;
begin
  para := VSProg_CommonGetEnabledOperationString();
  if para = '' then
  begin
    result := FALSE;
  end
  else
  begin
    result := TRUE;
    case TargetType of
      TT_PSOC1:
    end;
    para := 'or' + para;
    if result then
    begin
      caller.AddParameter(para);
    end;
  end;
end;

procedure TFormMain.btnVerifyClick(Sender: TObject);
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

  if not VSProg_CommonAddVerifyOperation() then
  begin
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
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

procedure TFormMain.btnOpenFileClick(Sender: TObject);
var
  i, file_num, valid_filename_num: integer;
begin
  file_num := Length(TargetFile);
  if file_num > 1 then
  begin
    FormFileSelector.Reset;
    for i := 0 to file_num - 1 do
    begin
      FormFileSelector.AddFileSetting(TargetFile[i].target, TargetFile[i].filename);
    end;
    if mrOK = FormFileSelector.ShowModal then
    begin
      cbboxInputFile.Clear;
      valid_filename_num := 0;
      for i := 0 to file_num - 1 do
      begin
        TargetFile[i].filename := FormFileSelector.GetFileNameByTargetName(TargetFile[i].target);
        if TargetFile[i].filename <> '' then
        begin
          cbboxInputFile.Items.Add(TargetFile[i].filename);
          valid_filename_num := valid_filename_num + 1;
        end;
      end;
      if valid_filename_num > 0 then
      begin
        cbboxInputFile.Items.Insert(0, 'ALL');
        cbboxInputFile.ItemIndex := 0;
        cbboxInputFile.Hint := 'ALL';
      end;
    end;
  end
  else
  begin
    if odInputFile.Execute then
    begin
      cbboxInputFile.Clear;
      cbboxInputFile.Items.Add(odInputFile.FileName);
      cbboxInputFile.ItemIndex := 0;
    end;
  end;
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

procedure TFormMain.btnEditCaliClick(Sender: TObject);
var
  targetdefine: string;
  init, bytelen: integer;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  if caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;

  if not chkboxNoconnect.Checked then
  begin
    // call 'vsprog -orc' to read calibration settings from target
    if not PrepareToRunCLI() then
    begin
      caller.UnTake;
      exit;
    end;
    PrepareBaseParameters();
    caller.AddParameter('orc');
    caller.Run(@VSProg_CommonParseReadCaliCallback, FALSE, TRUE);
    if bFatalError then
    begin
      exit;
    end;
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCLI() then
  begin
    exit;
  end;
  PrepareBaseParameters();
  caller.AddParameter('Pcalibration');
  FormParaEditor.FreeRecord();
  caller.Run(@VSProg_CommonParseParaCallback, FALSE, TRUE);
  if bFatalError then
  begin
    exit;
  end;

  init := TargetSetting[cbboxTarget.ItemIndex].cali_default;
  bytelen := TargetSetting[cbboxTarget.ItemIndex].cali_bytelen;
  FormParaEditor.SetParameter(init, bytelen, StrToInt(lbledtCali.Text), 'Calibration', not chkboxNowarning.Checked);
  if mrOK = FormParaEditor.ShowModal then
  begin
    // OK clicked, get value
    VSProg_CommonUpdateCali(FormParaEditor.GetResult(), bytelen);
  end;
end;

procedure TFormMain.btnEditEEClick(Sender: TObject);
begin
//  FormHexEditor.Caption := (Sender as TButton).Caption;
//  FormHexEditor.ShowModal;
end;

procedure TFormMain.btnEditFuseClick(Sender: TObject);
var
  targetdefine: string;
  init, bytelen: integer;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  if caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;

  if not chkboxNoconnect.Checked then
  begin
    // call 'vsprog -oru' to read fuse settings from target
    if not PrepareToRunCLI() then
    begin
      caller.UnTake;
      exit;
    end;
    PrepareBaseParameters();
    caller.AddParameter('oru');
    caller.Run(@VSProg_CommonParseReadFuseCallback, FALSE, TRUE);
    if bFatalError then
    begin
      exit;
    end;
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCLI() then
  begin
    exit;
  end;
  PrepareBaseParameters();
  caller.AddParameter('Pfuse');
  FormParaEditor.FreeRecord();
  caller.Run(@VSProg_CommonParseParaCallback, FALSE, TRUE);
  if bFatalError then
  begin
    exit;
  end;

  init := TargetSetting[cbboxTarget.ItemIndex].fuse_default;
  bytelen := TargetSetting[cbboxTarget.ItemIndex].fuse_bytelen;
  FormParaEditor.SetParameter(init, bytelen, StrToInt(lbledtFuse.Text), 'Fuse', not chkboxNowarning.Checked);
  if mrOK = FormParaEditor.ShowModal then
  begin
    // OK clicked, get value
    VSProg_CommonUpdateFuse(FormParaEditor.GetResult(), bytelen);
  end;
end;

procedure TFormMain.btnEditLockClick(Sender: TObject);
var
  targetdefine: string;
  init, bytelen: integer;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  if caller.IsRunning() then
  begin
    exit;
  end;
  caller.Take;

  if not chkboxNoconnect.Checked then
  begin
    // call 'vsprog -orl' to read lock settings from target
    if not PrepareToRunCLI() then
    begin
      caller.UnTake;
      exit;
    end;
    PrepareBaseParameters();
    caller.AddParameter('orl');
    caller.Run(@VSProg_CommonParseReadLockCallback, FALSE, TRUE);
    if bFatalError then
    begin
      exit;
    end;
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCLI() then
  begin
    caller.UnTake;
    exit;
  end;
  PrepareBaseParameters();
  caller.AddParameter('Plock');
  FormParaEditor.FreeRecord();
  caller.Run(@VSProg_CommonParseParaCallback, FALSE, TRUE);

  init := TargetSetting[cbboxTarget.ItemIndex].lock_default;
  bytelen := TargetSetting[cbboxTarget.ItemIndex].lock_bytelen;
  FormParaEditor.SetParameter(init, bytelen, StrToInt(lbledtLock.Text), 'Lock', not chkboxNowarning.Checked);
  if mrOK = FormParaEditor.ShowModal then
  begin
    // OK clicked, get value
    VSProg_CommonUpdateLock(FormParaEditor.GetResult(), bytelen);
  end;
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

  if chkboxEraseBeforeWrite.Checked and not VSProg_CommonAddEraseOperation() then
  begin
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    caller.UnTake;
    exit;
  end;

  if not VSProg_CommonAddWriteOperation() then
  begin
    MessageDlg('Error', 'Fail to add write operation', mtError, [mbOK], 0);
    caller.UnTake;
    exit;
  end;

  if chkboxVerifyAfterWrite.Checked and not VSProg_CommonAddVerifyOperation() then
  begin
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  caller.Run(@VSProg_CommonCallback, FALSE, TRUE);
  LogInfo('Idle');
end;

procedure TFormMain.cbboxInputFileChange(Sender: TObject);
begin
  cbboxInputFile.Hint := cbboxInputFile.Text;
end;

procedure TFormMain.cbboxModeChange(Sender: TObject);
var
  str_tmp: string;
begin
  // to do: change the GUI according to different mode
  str_tmp := cbboxMode.Text;
  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC1:
      exit;
    TT_AT89S5X:
      exit;
    TT_C8051F:
      exit;
    TT_AVR8:
      AVR8_Update_Mode(str_tmp);
    TT_CortexM3:
      CortexM3_Update_Mode(str_tmp);
  end;

  AdjustComponentColor(cbboxMode);
  AdjustComponentColor(lbledtFuse);
  AdjustComponentColor(lbledtLock);
  AdjustComponentColor(lbledtCali);
  AdjustComponentColor(lbledtUsrSig);
  AdjustComponentColor(lbledtFreq);
end;

procedure TFormMain.cbboxTargetChange(Sender: TObject);
var
  index: integer;
begin
  index := cbboxTarget.ItemIndex;

  VSProg_CommonTargetInit(TargetSetting[index].target);

  if TargetSetting[index].fuse_bytelen > 0 then
  begin
    lbledtFuse.Enabled := TRUE;
    btnEditFuse.Enabled := TRUE;
  end
  else
  begin
    lbledtFuse.Enabled := False;
    btnEditFuse.Enabled := False;
  end;
  VSProg_CommonUpdateFuse(TargetSetting[index].fuse_default,
                          TargetSetting[index].fuse_bytelen);

  if TargetSetting[index].lock_bytelen > 0 then
  begin
    lbledtLock.Enabled := TRUE;
    btnEditLock.Enabled := TRUE;
  end
  else
  begin
    lbledtLock.Enabled := FALSE;
    btnEditLock.Enabled := FALSE;
  end;
  VSProg_CommonUpdateLock(TargetSetting[index].lock_default,
                          TargetSetting[index].lock_bytelen);

  if TargetSetting[index].cali_bytelen > 0 then
  begin
    lbledtCali.Enabled := TRUE;
    btnEditCali.Enabled := TRUE;
  end
  else
  begin
    lbledtCali.Enabled := FALSE;
    btnEditCali.Enabled := FALSE;
  end;
  VSProg_CommonUpdateCali(TargetSetting[index].cali_default,
                          TargetSetting[index].cali_bytelen);

  if TargetSetting[index].usrsig_bytelen > 0 then
  begin
    lbledtUsrSig.Enabled := TRUE;
    btnEditUsrSig.Enabled := TRUE;
  end
  else
  begin
    lbledtUsrSig.Enabled := FALSE;
    btnEditUsrSig.Enabled := FALSE;
  end;
  VSProg_CommonUpdateUsrSig(TargetSetting[index].usrsig_default,
                            TargetSetting[index].usrsig_bytelen);

  if TargetSetting[index].mode = '' then
  begin
    cbboxMode.Clear;
    cbboxMode.Enabled := FALSE;
  end
  else
  begin
    cbboxMode.Enabled := TRUE;
  end;

  case TargetType of
    TT_NONE:
      exit;
    TT_PSOC1:
      PSoC1_Update_Chip(TargetSetting[index]);
    TT_AT89S5X:
      AT89S5X_Update_Chip(TargetSetting[index]);
    TT_C8051F:
      C8051F_Update_Chip(TargetSetting[index]);
    TT_AVR8:
      AVR8_Update_Chip(TargetSetting[index]);
    TT_MSP430:
      MSP430_Update_Chip(TargetSetting[index]);
    TT_COMISP:
      COMISP_Update_Chip(TargetSetting[index]);
    TT_LPCICP:
      LPCICP_Update_chip(TargetSetting[index]);
    TT_CORTEXM3:
      CortexM3_Update_chip(TargetSetting[index]);
  end;

  AdjustComponentColor(cbboxMode);
  AdjustComponentColor(lbledtFuse);
  AdjustComponentColor(lbledtLock);
  AdjustComponentColor(lbledtCali);
  AdjustComponentColor(lbledtUsrSig);
  AdjustComponentColor(lbledtFreq);
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








function TFormMain.CheckFatalError(line: string): boolean;
begin
  memoLog.Lines.Add(line);

  if ((Pos('Error:', line) > 0) or (Pos('fail', line) <> 0)) and not bFatalError then
  begin
    MessageDlg('Error', line, mtError, [mbOK], 0);
    bFatalError := TRUE;
  end;
  if (Pos('/****Bug****/:', line) = 1) and not bFatalError  then
  begin
    MessageDlg('Bug', line, mtError, [mbOK], 0);
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

function TFormMain.GetTargetDefineParameters(): string;
begin
  // target series_name or chip_name
  if ((TargetType <> TT_COMISP) and (TargetType <> TT_CORTEXM3)) and (cbboxTarget.ItemIndex = 0) then
  begin
    result := 's' + cbboxTarget.Items.Strings[cbboxTarget.ItemIndex];
  end
  else
  begin
    result := 'c' + cbboxTarget.Items.Strings[cbboxTarget.ItemIndex];
  end;
end;

procedure TFormMain.PrepareBaseParameters();
var
  i: integer;
begin
  caller.AddParameter(GetTargetDefineParameters());

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
  if cbboxInputFile.ItemIndex > 0 then
  begin
    // enable selected input file
    i := cbboxInputFile.ItemIndex - 1;
    caller.AddParameter('I"' + TargetFile[i].filename + '@'
                             + IntToStr(TargetFile[i].seg_offset) + ','
                             + IntToStr(TargetFile[i].addr_offset) + '"');
  end
  else
  begin
    // enable all input file
    for i := 0 to Length(TargetFile) - 1 do
    begin
      if TargetFile[i].filename <> '' then
      begin
        caller.AddParameter('I"' + TargetFile[i].filename + '@'
                                 + IntToStr(TargetFile[i].seg_offset) + ','
                                 + IntToStr(TargetFile[i].addr_offset) + '"');
      end;
    end;
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

  // Frequency
  if lbledtFreq.Enabled and (lbledtFreq.Text <> '') then
  begin
    if lbledtFreq.EditLabel.Caption <> EXECUTE_ADDR_STR then
    begin
      caller.AddParameter('F' + lbledtFreq.Text);
    end;
  end;
end;

procedure TFormMain.PrepareCommonParameters();
begin
  // enable GUI mode
  caller.AddParameter('G');
  PrepareBaseParameters();

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

  // Execute
  if lbledtFreq.Enabled and (lbledtFreq.Text <> '') then
  begin
    if lbledtFreq.EditLabel.Caption = EXECUTE_ADDR_STR then
    begin
      caller.AddParameter('x' + lbledtFreq.Text);
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
    TT_PSOC1:
      success := PSoC1_Init();
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

    cbboxTargetChange(cbboxTarget);

    AdjustComponentColor(cbboxMode);
    AdjustComponentColor(lbledtFuse);
    AdjustComponentColor(lbledtLock);
    AdjustComponentColor(lbledtCali);
    AdjustComponentColor(lbledtUsrSig);
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
      (Pos('verifying', line)= 1) or
      (Pos('erasing', line)  = 1) or
      (Pos('checking', line) = 1) or
      (Pos('executing', line)= 1)) and
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

function TFormMain.VSProg_CommonParseReadTargetCallback(line, target: string; var result_str: string): boolean;
var
  pos_start: integer;
begin
  if CheckFatalError(line) then
  begin
    result := FALSE;
    exit;
  end;
  result := TRUE;

  result_str := '';
  pos_start := Pos(target + ' read is ', line);
  if pos_start > 0 then
  begin
    result_str := Copy(line, pos_start + Length(target + ' read is '), Length(line) - pos_start);
    FormParaEditor.WipeTailEnter(result_str);
  end;
end;

function TFormMain.VSProg_CommonParseReadFuseCallback(line: string): boolean;
var
  fuse_str: string;
begin
  result := TRUE;
  fuse_str := '';
  if not VSProg_CommonParseReadTargetCallback(line, 'fuse', fuse_str) then
  begin
    result := FALSE;
  end;

  if fuse_str <> '' then
  begin
    VSProg_CommonUpdateFuse(StrToInt(fuse_str) and TargetSetting[cbboxTarget.ItemIndex].fuse_default,
                            TargetSetting[cbboxTarget.ItemIndex].fuse_bytelen);
  end;
end;

function TFormMain.VSProg_CommonParseReadCaliCallback(line: string): boolean;
var
  cali_str: string;
begin
  result := TRUE;
  cali_str := '';
  if not VSProg_CommonParseReadTargetCallback(line, 'calibration', cali_str) then
  begin
    result := FALSE;
  end;

  if cali_str <> '' then
  begin
    VSProg_CommonUpdateCali(StrToInt(cali_str) and TargetSetting[cbboxTarget.ItemIndex].cali_default,
                            TargetSetting[cbboxTarget.ItemIndex].cali_bytelen);
  end;
end;

function TFormMain.VSProg_CommonParseReadLockCallback(line: string): boolean;
var
  lock_str: string;
begin
  result := TRUE;
  lock_str := '';
  if not VSProg_CommonParseReadTargetCallback(line, 'lock', lock_str) then
  begin
    result := FALSE;
  end;

  if lock_str <> '' then
  begin
    VSProg_CommonUpdateLock(StrToInt(lock_str) and TargetSetting[cbboxTarget.ItemIndex].lock_default,
                            TargetSetting[cbboxTarget.ItemIndex].lock_bytelen);
  end;
end;

function TFormMain.VSProg_CommonParseParaCallback(line: string): boolean;
var
  dis: string;
begin
  if CheckFatalError(line) then
  begin
    result := FALSE;
    exit;
  end;
  result := TRUE;

  if Pos('setting: ', line) = 1 then
  begin
    // check disable
    dis := '';
    FormParaEditor.GetStringParameter(line, 'ban', dis);
    if (dis = '*') or (Pos(cbboxMode.Text[1], dis) > 0) then
    begin
      // current setting is disabled in current mode
      FormParaEditor.WipeTailEnter(line);
      line := line + ', disabled = 1'
    end;
  end;

  FormParaEditor.ParseLine(line);
end;

procedure TFormMain.VSProg_AddTargetSetting(setting: TTargetSetting);
var
  index: integer;
begin
  index := Length(TargetSetting);
  SetLength(TargetSetting, index + 1);

  TargetSetting[index] := setting;
end;

procedure TFormMain.VSProg_TargetSettingInit(var setting: TTargetSetting);
begin
  setting.mode := '';
  setting.target := '';
  setting.fuse_bytelen := 0;
  setting.fuse_default := 0;
  setting.lock_bytelen := 0;
  setting.lock_default := 0;
  setting.cali_bytelen := 0;
  setting.cali_default := 0;
  setting.usrsig_bytelen := 0;
  setting.usrsig_default := 0;
end;

function TFormMain.VSProg_CommonParseSupportCallback(line: string): boolean;
var
  chip_name, chip_module_header: string;
  setting: TTargetSetting;
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
    TT_PSOC1:
      chip_module_header := 'cy8c2';
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
    VSProg_TargetSettingInit(setting);

    case TargetType of
      TT_NONE:
        exit;
      TT_PSOC1:
        PSoC1_Init_Para(line, setting);
      TT_AT89S5X:
        AT89S5X_Init_Para(line, setting);
      TT_C8051F:
        C8051F_Init_Para(line, setting);
      TT_AVR8:
        AVR8_Init_Para(line, setting);
      TT_MSP430:
        MSP430_Init_Para(line, setting);
      TT_COMISP:
        COMISP_Init_Para(line, setting);
      TT_LPCICP:
        LPCICP_Init_Para(line, setting);
      TT_CORTEXM3:
        CortexM3_Init_Para(line, setting);
    end;

    FormParaEditor.GetIntegerParameter(line, 'fuse_bytelen', setting.fuse_bytelen);
    FormParaEditor.GetIntegerParameter(line, 'fuse_default', setting.fuse_default);
    FormParaEditor.GetIntegerParameter(line, 'lock_bytelen', setting.lock_bytelen);
    FormParaEditor.GetIntegerParameter(line, 'lock_default', setting.lock_default);
    FormParaEditor.GetIntegerParameter(line, 'cali_bytelen', setting.cali_bytelen);
    FormParaEditor.GetIntegerParameter(line, 'cali_default', setting.cali_default);
    FormParaEditor.GetIntegerParameter(line, 'usrsig_bytelen', setting.usrsig_bytelen);
    FormParaEditor.GetIntegerParameter(line, 'usrsig_default', setting.usrsig_default);

    cbboxTarget.Items.Add(chip_name);
    VSProg_AddTargetSetting(setting);
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

procedure TFormMain.VSProg_CommonUpdateSetting(value, bytelen: integer; edt: TLabeledEdit);
begin
  if bytelen > 0 then
  begin
    edt.Text := '0x' + IntToHex(value, bytelen * 2);
  end
  else
  begin
    edt.Text := '';
  end;
end;

procedure TFormMain.VSProg_CommonUpdateUsrSig(sig, bytelen: integer);
begin
  VSProg_CommonUpdateSetting(sig, bytelen, lbledtUsrSig);
end;

procedure TFormMain.VSProg_CommonUpdateCali(cali, bytelen: integer);
begin
  VSProg_CommonUpdateSetting(cali, bytelen, lbledtCali);
end;

procedure TFormMain.VSProg_CommonUpdateLock(lock, bytelen: integer);
begin
  VSProg_CommonUpdateSetting(lock, bytelen, lbledtLock);
end;

procedure TFormMain.VSProg_CommonUpdateFuse(fuse, bytelen: integer);
begin
  VSProg_CommonUpdateSetting(fuse, bytelen, lbledtFuse);
end;

// f: Flash, e: EEprom, l: Lock, u: Fuse, s: User Signature, c: Calibration Value
// M: Mass-Product, F: Frequency, X: Execute, A: AutoDetect, C: COM Port Setting
procedure TFormMain.VSProg_CommonTargetInit(para: string);
begin
  btnEditApp.Caption := 'Flash';
  chkboxApp.Caption := 'Flash';
  if Pos(FLASH_CHAR, para) > 0 then
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
  if Pos(EEPROM_CHAR, para) > 0 then
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
  if Pos(LOCK_CHAR, para) > 0 then
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
  if Pos(FUSE_CHAR, para) > 0 then
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
  if Pos(USRSIG_CHAR, para) > 0 then
  begin
    btnEditUsrSig.Enabled := TRUE;
    lbledtUsrSig.Enabled := TRUE;
    chkboxUsrSig.Enabled := TRUE;
    chkboxUsrSig.Checked := TRUE;
  end
  else
  begin
    btnEditUsrSig.Enabled := FALSE;
    lbledtUsrSig.Enabled := FALSE;
    chkboxUsrSig.Enabled := FALSE;
    chkboxUsrSig.Checked := FALSE;
  end;

  btnEditCali.Caption := 'Cali.';
  chkboxCali.Caption := 'Cali.';
  if Pos(CALI_CHAR, para) > 0 then
  begin
    btnEditCali.Enabled := TRUE;
    lbledtCali.Enabled := TRUE;
    chkboxCali.Enabled := TRUE;
    chkboxCali.Checked := TRUE;
  end
  else
  begin
    btnEditCali.Enabled := FALSE;
    lbledtCali.Enabled := FALSE;
    chkboxCali.Enabled := FALSE;
    chkboxCali.Checked := FALSE;
  end;
end;

procedure TFormMain.VSProg_CommonInit(para: string);
var
  str_tmp: string;
begin
  VSProg_CommonTargetInit(para);

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
      lbledtFreq.Hint := FREQ_HINT;
    end
    else
    begin
      lbledtFreq.EditLabel.Caption := EXECUTE_ADDR_STR;
      lbledtFreq.Hint := EXECUTE_ADDR_HINT;
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
    lbledtFreq.Hint := '';
    lbledtFreq.Enabled := FALSE;
  end;

  if (Pos('A', para) > 0) or (Pos('C', para) > 0) then
  begin
    if Pos('C', para) > 0 then
    begin
      btnTargetDetect.Caption := COMSETUP_STR;
      btnTargetDetect.Hint := COMSETUP_HINT;
    end
    else
    begin
      btnTargetDetect.Caption := AUTODETECT_STR;
      btnTargetDetect.Hint := AUTODETECT_HINT;
    end;
    btnTargetDetect.Enabled := TRUE;
  end
  else
  begin
    btnTargetDetect.Caption := 'No use';
    btnTargetDetect.Hint := 'No use';
    btnTargetDetect.Enabled := FALSE;
  end;

  chkboxEraseBeforeWrite.Caption := 'Erase before write';
  chkboxEraseBeforeWrite.Enabled := TRUE;
  chkboxVerifyAfterWrite.Caption := 'Verify after write';
  chkboxVerifyAfterWrite.Enabled := TRUE;

  cbboxMode.Clear;
  cbboxMode.Enabled := FALSE;
end;

{ PSoC1 implementation }
function TFormMain.PSoC1_AddVerifyParameters(var para: string): boolean;
begin
  if not chkboxApp.Checked then
  begin
    result := FALSE;
  end
  else
  begin
    para := 'fk';
    result := TRUE;
  end;
end;

procedure TFormMain.PSoC1_Update_Chip(setting: TTargetSetting);
var
  str_tmp: string;
  mode_str: string;
begin
  mode_str := setting.mode;
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('r', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('r:Reset');
  end;
  if Pos('p', mode_str) > 0 then
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

function TFormMain.PSoC1_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  FormParaEditor.GetStringParameter(line, 'init_mode', setting.mode);
  setting.target := FLASH_CHAR + LOCK_CHAR;
  result := TRUE;
end;

function TFormMain.PSoC1_Init(): boolean;
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('psoc1');

  VSProg_TargetSettingInit(setting);
  setting.mode := 'rp';
  setting.target := FLASH_CHAR + LOCK_CHAR;
  VSProg_AddTargetSetting(setting);

  // call 'vsprog -Spsoc1' to extract supported psoc1 targets
  if not PrepareToRunCLI() then
  begin
    result := FALSE;
    exit;
  end;
  caller.AddParameter('Spsoc1');
  LogInfo('Running...');
  caller.Run(@VSProg_CommonParseSupportCallback, FALSE, TRUE);
  LogInfo('Idle');

  if bFatalError then
  begin
    tsPSoC1.Enabled := FALSE;
    result := FALSE;
    exit;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

{ C8051F implementations }
function TFormMain.C8051F_Init(): boolean;
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('c8051f');

  VSProg_TargetSettingInit(setting);
  setting.mode := 'jc';
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);

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
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

procedure TFormMain.C8051F_Update_Chip(setting: TTargetSetting);
var
  mode_str: string;
begin
  mode_str := setting.mode;
  cbboxMode.Items.Clear;
  if Pos('j', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('j:JTAG');
  end;
  if Pos('c', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('c:C2');
  end;
  cbboxMode.ItemIndex := 0;
end;

function TFormMain.C8051F_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  FormParaEditor.GetStringParameter(line, 'prog_mode', setting.mode);
  setting.target := FLASH_CHAR;
  result := TRUE;
end;

{ AT89S5X implementations }
procedure TFormMain.AT89S5X_Update_Chip(setting: TTargetSetting);
var
  mode_str, str_tmp: string;
begin
  mode_str := setting.mode;
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('p', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('p:Page');
  end;
  if Pos('b', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('b:Byte');
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

function TFormMain.AT89S5X_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  line := line;

  setting.mode := 'pb';
  setting.target := FLASH_CHAR + LOCK_CHAR;
  setting.lock_bytelen := 1;
  setting.lock_default := 1;
  result := TRUE;
end;

function TFormMain.AT89S5X_Init(): boolean;
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('at89s5x');

  VSProg_TargetSettingInit(setting);
  setting.mode := 'pb';
  setting.target := FLASH_CHAR + LOCK_CHAR;
  setting.lock_bytelen := 1;
  setting.lock_default := 1;
  VSProg_AddTargetSetting(setting);

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
  end;

  // Frequency, Autodetect
  VSProg_CommonInit('FA');
end;

{ MSP430 implementations }
function TFormMain.MSP430_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  FormParaEditor.GetStringParameter(line, 'prog_mode', setting.mode);
  setting.target := FLASH_CHAR;
  result := TRUE;
end;

function TFormMain.MSP430_Init(): boolean;
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('msp430');

  VSProg_TargetSettingInit(setting);
  setting.mode := 'jsb';
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);

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
  end;

  // flash, Autodetect
  VSProg_CommonInit('A');
end;

procedure TFormMain.MSP430_Update_Chip(setting: TTargetSetting);
var
  str_tmp: string;
  mode_str: string;
begin
  mode_str := setting.mode;
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('j', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('j:JTAG');
  end;
  if Pos('s', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('s:SBW');
  end;
  if Pos('b', mode_str) > 0 then
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

{ STM8 implementations }
function TFormMain.STM8_Init(): boolean;
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('stm8');

  VSProg_TargetSettingInit(setting);
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);

  // call 'vsprog -Sstm8' to extract supported at89s5x targets
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
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

{ EEPROM implementations }
function TFormMain.EEPROM_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;
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
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('avr8');

  VSProg_TargetSettingInit(setting);
  setting.mode := 'ijps';
  setting.target := FLASH_CHAR + FUSE_CHAR + LOCK_CHAR + CALI_CHAR + EEPROM_CHAR;
  setting.fuse_bytelen := 3;
  setting.fuse_default := $FFFFFF;
  setting.lock_bytelen := 1;
  setting.lock_default := $FF;
  setting.cali_bytelen := 4;
  setting.cali_default := $FFFFFFFF;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('Flash', 0, 0);
  AddTargetFile('EEPROM', 2, 0);

  // call 'vsprog -Savr8' to extract supported avr8 targets
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
  end;

  // Autodetect, Frequency
  VSProg_CommonInit('AF');
end;

function TFormMain.AVR8_Init_Para(line: string; var setting: TTargetSetting): boolean;
var
  size: integer;
begin
  FormParaEditor.GetStringParameter(line, 'prog_mode', setting.mode);
  size := 0;
  FormParaEditor.GetIntegerParameter(line, 'eeprom_size', size);
  if size > 0 then
  begin
    setting.target := FLASH_CHAR + FUSE_CHAR + LOCK_CHAR + CALI_CHAR + EEPROM_CHAR;
  end
  else
  begin
    setting.target := FLASH_CHAR + FUSE_CHAR + LOCK_CHAR + CALI_CHAR;
  end;
  result := TRUE;
end;

procedure TFormMain.AVR8_Update_Chip(setting: TTargetSetting);
var
  str_tmp, mode_str: string;
begin
  mode_str := setting.mode;
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;
  if Pos('i', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('i:ISP');
  end;
  if Pos('j', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('j:JTAG');
  end;
  if Pos('p', mode_str) > 0 then
  begin
    cbboxMode.Items.Add('p:HVPP');
  end;
  if Pos('s', mode_str) > 0 then
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

  str_tmp := setting.target;
  if Pos(FLASH_CHAR, str_tmp) > 0 then
  begin
    AddTargetFile('Flash', 0, 0);
  end
  else
  begin
    RemoveTargetFile('Flash');
  end;
  if Pos(EEPROM_CHAR, str_tmp) > 0 then
  begin
    AddTargetFile('EEPROM', 2, 0);
  end
  else
  begin
    RemoveTargetFile('EEPROM');
  end;

  // Fuse, Lock and Cali are not enabled by default
  chkboxEE.Checked := FALSE;
  chkboxFuse.Checked := FALSE;
  chkboxLock.Checked := FALSE;
  chkboxCali.Checked := FALSE;
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

{ COMISP declarations }
function TFormMain.COMISP_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;

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
  end;

  VSProg_CommonInit('CX');
end;

function TFormMain.COMISP_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  setting.extra := line;
  setting.target := FLASH_CHAR;
  result := TRUE;
end;

procedure TFormMain.COMISP_Update_Chip(setting: TTargetSetting);
var
  str_tmp: string;
  setting_str: string;
begin
  str_tmp := '';

  setting_str := setting.extra;
  FormParaEditor.GetIntegerParameter(setting_str, 'baudrate', ComModeInit.baudrate);
  FormParaEditor.GetIntegerParameter(setting_str, 'datalength', ComModeInit.datalength);
  FormParaEditor.GetStringParameter(setting_str, 'paritybit', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.paritybit := str_tmp[1];
  end;
  FormParaEditor.GetStringParameter(setting_str, 'stopbit', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.stopbit := str_tmp[1];
  end;
  FormParaEditor.GetStringParameter(setting_str, 'handshake', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.handshake := str_tmp[1];
  end;
  FormParaEditor.GetStringParameter(setting_str, 'auxpin', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.auxpin := str_tmp[1];
  end;
  FormComSetup.ComInitPara(ComModeInit);
end;

{ LPCICP implementations }
function TFormMain.LPCICP_Init(): boolean;
var
  setting: TTargetSetting;
begin
  result := TRUE;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('lpc900');

  VSProg_TargetSettingInit(setting);
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);

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
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

function TFormMain.LPCICP_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  line := line;

  setting.target := FLASH_CHAR;
  result := TRUE;
end;

procedure TFormMain.LPCICP_Update_Chip(setting: TTargetSetting);
begin
  setting := setting;
end;

{ CortexM3 implementations }
function TFormMain.CortexM3_Init(): boolean;
begin
  result := TRUE;

  cbboxTarget.Clear;

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
  end;

  // Auto-detect
  VSProg_CommonInit('A');
end;

function TFormMain.CortexM3_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  line := line;

  setting.mode := 'js';
  setting.target := FLASH_CHAR;
  result := TRUE;
end;

procedure TFormMain.CortexM3_Update_Chip(setting: TTargetSetting);
var
  str_tmp: string;
begin
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;

  if setting.mode = '' then
  begin
    cbboxMode.Items.Add('j:JTAG');
    cbboxMode.Items.Add('s:SWJ');
  end
  else
  begin
    if Pos('j', setting.mode) > 0 then
    begin
      cbboxMode.Items.Add('j:JTAG');
    end;
    if Pos('s', setting.mode) > 0 then
    begin
      cbboxMode.Items.Add('s:SWJ');
    end;
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

initialization
  {$I main.lrs}

end.


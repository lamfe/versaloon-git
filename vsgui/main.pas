unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ExtCtrls, cli_caller, parameditor, Menus, Buttons, Spin,
  Synaser, com_setup, fileselector, hexeditor, XMLCfg, vsprogparser;

type

  TTargetType = (TT_NONE, TT_PSOC1, TT_AT89S5X, TT_C8051F, TT_MSP430, TT_STM8,
    TT_EEPROM, TT_JTAG, TT_AVR8, TT_PIC8, TT_COMISP, TT_LPCICP,
    TT_CORTEXM3);

  TTargetSetting = record
    extra:     string;
    target:    string;
    mode:      string;
    fuse_bytelen: integer;
    fuse_default: integer;
    lock_bytelen: integer;
    lock_default: integer;
    cali_bytelen: integer;
    cali_default: integer;
    usrsig_bytelen: integer;
    usrsig_default: integer;
    flash_start_addr: integer;
    flash_seg: integer;
    eeprom_start_addr: integer;
    eeprom_seg: integer;
  end;

  TTargetFileSetting = record
    target:      string;
    filename:    string;
    seg_offset:  integer;
    addr_offset: integer;
    def_start_addr: integer;
    offset_is_fake: boolean;
  end;

  TMemoryInfo = record
    start_addr:   integer;
    seg_addr:     integer;
    default_byte: byte;
    byte_size:    integer;
  end;

  { TPollThread }

  TPollThread = class(TThread)
  private
    TargetVoltage: integer;
    ConnectOK:     boolean;
    procedure Update;
  protected
    procedure Execute; override;
  public
    constructor Create();
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
    btnSetPower: TButton;
    cbboxOpenOCDInterface: TComboBox;
    cbboxOpenOCDScript: TComboBox;
    cbboxOpenOCDTarget: TComboBox;
    chkboxNowarning: TCheckBox;
    chkboxCali: TCheckBox;
    chkboxNoconnect: TCheckBox;
    chkboxUsrSig: TCheckBox;
    chkboxMP:  TCheckBox;
    chkboxFuse: TCheckBox;
    chkboxEE:  TCheckBox;
    cbboxMode: TComboBox;
    cbboxCOM:  TComboBox;
    cbboxInputFile: TComboBox;
    edtSVFOption: TEdit;
    edtOpenOCDOption: TEdit;
    fneditSVFFile: TFileNameEdit;
    fnFW:      TFileNameEdit;
    gbChipName: TGroupBox;
    btnEditApp: TButton;
    btnEditLock: TButton;
    btnErase:  TButton;
    btnRead:   TButton;
    btnTargetDetect: TButton;
    btnVerify: TButton;
    btnWrite:  TButton;
    cbboxTarget: TComboBox;
    chkboxApp: TCheckBox;
    chkboxEraseBeforeWrite: TCheckBox;
    chkboxLock: TCheckBox;
    chkboxVerifyAfterWrite: TCheckBox;
    gbInputFile: TGroupBox;
    gbOperation: TGroupBox;
    gbOption:  TGroupBox;
    gbUpdate:  TGroupBox;
    gbOpenOCD: TGroupBox;
    gbSVFPlayer: TGroupBox;
    gbPower:   TGroupBox;
    lblPowerUnit: TLabel;
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
    lblMode:   TLabel;
    lblInputFile: TLabel;
    lblTarget: TLabel;
    memoAbout: TMemo;
    memoInfo:  TMemo;
    memoLog:   TMemo;
    miExit:    TMenuItem;
    odInputFile: TOpenDialog;
    pgbarMain: TProgressBar;
    pnlMain:   TPanel;
    pcMain:    TPageControl;
    pmTray:    TPopupMenu;
    sbMain:    TStatusBar;
    sedtPower: TSpinEdit;
    tPollProgrammer: TTimer;
    tsCortexM3: TTabSheet;
    tsLPCICP:  TTabSheet;
    tDelay:    TTimer;
    tsCOMISP:  TTabSheet;
    tsJTAG:    TTabSheet;
    tsAVR8:    TTabSheet;
    tsEEPROM:  TTabSheet;
    tiMain:    TTrayIcon;
    tsSTM8:    TTabSheet;
    tsMSP430:  TTabSheet;
    tsAbout:   TTabSheet;
    tsAT89S5X: TTabSheet;
    tsC8051F:  TTabSheet;
    tsPSoC1:   TTabSheet;
    xmlcfgMain: TXMLConfig;
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
    procedure btnSetPowerClick(Sender: TObject);
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
    procedure pcMainChanging(Sender: TObject; var AllowChange: boolean);
    procedure pcMainPageChanged(Sender: TObject);
    procedure tbPowerChange(Sender: TObject);
    procedure tPollProgrammerTimer(Sender: TObject);
    procedure tDelayTimer(Sender: TObject);
    procedure tiMainClick(Sender: TObject);

    procedure AdjustComponentColor(Sender: TObject);
    procedure ShowDebugLog();
    procedure HideDebugLog();
    procedure ComboBoxSetText(var combo: TComboBox; aText: string);
    procedure UpdateTitle();
    procedure LogInfo(info: string);

    { VSProg common declarations }

    function VSProg_RunAlgorithm(var caller: TCLI_Caller; parser: TParserFunc;
      result_num: integer): boolean;
    function VSProg_MemoryTargetParserCallback(line: string): boolean;
    function VSProg_SettingTargetParserCallback(line: string): boolean;
    function VSProg_SupportParserCallback(line: string): boolean;
    function VSProg_LogProcess(ProgressInfo: TProgressInfoType; info: string): boolean;
    procedure VSProg_LogOutput(line: string);
    procedure VSProg_LogProgress(ProgressInfo: TProgressInfoType; info: string);

    procedure UpdateTargetFile();
    procedure VSProg_TargetSettingInit(var setting: TTargetSetting);
    procedure VSProg_AddTargetSetting(setting: TTargetSetting);
    function GetTargetDefineParameters(): string;
    procedure PrepareBaseParameters(var caller: TCLI_Caller);
    procedure PrepareOperationParameters(var caller: TCLI_Caller);
    function PrepareToRunCLI: boolean;
    function PrepareToRunOpenOCD: boolean;
    procedure VSProg_CommonTargetInit(para: string);
    procedure VSProg_CommonInit(para: string);
    procedure VSProg_CommonUpdateSetting(Value, bytelen: integer; edt: TLabeledEdit);
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
    TargetType:     TTargetType;
    TargetSetting:  array of TTargetSetting;
    bPageLock:      boolean;
    bReadOperation: boolean;
    bOpenFileOK:    boolean;
    JTAGPage_Init:  boolean;
    TargetPage_Init: boolean;
    AboutPage_Init: boolean;
    TargetVoltage:  integer;
    ShowCallerError: boolean;
  public
    { public declarations }
  end;

var
  FormMain:    TFormMain;
  PollThread:  TPollThread;
  VSProg_Caller: TCLI_Caller;
  VSProg_Parser: TVSprog_Parser;
  VSProg_Taken_By_Polling: boolean;
  ComMode:     TComMode;
  ComModeInit: TComMode;
  TargetFile:  array of TTargetFileSetting;
  VSProg_Version: string;
  MemoryInfo:  TMemoryInfo;
  VSProg_Exists: boolean;

const
  DEBUG_LOG_SHOW: boolean = False;
  DISPLAY_ALL_COMPORT_WHEN_UPDATE = True;
  APP_STR: string      = 'Vsgui';
  VERSION_STR: string  = 'RC1';
  {$IFDEF UNIX}
  VSPROG_STR: string   = 'vsprog';
  {$ELSE}
  VSPROG_STR: string   = 'vsprog.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  OPENOCD_APP_STR: string = 'openocd';
  {$ELSE}
  OPENOCD_APP_STR: string = 'openocd.exe';
  {$ENDIF}
  COMSETUP_STR: string = '&COM Setup';
  COMSETUP_HINT: string = 'Setup COM Port';
  AUTODETECT_STR: string = '&AutoDetect';
  AUTODETECT_HINT: string = 'Detect Target Module';
  FREQ_STR: string     = 'Freq(KHz):';
  FREQ_HINT: string    = 'Set operation frequency';
  EXECUTE_ADDR_STR: string = 'execute:';
  EXECUTE_ADDR_HINT: string = 'Set address to execute after operation';
  LOGMEMO_WIDTH: integer = 400;

  FLASH_CHAR: string  = 'f';
  EEPROM_CHAR: string = 'e';
  FUSE_CHAR: string   = 'u';
  LOCK_CHAR: string   = 'l';
  CALI_CHAR: string   = 'c';
  USRSIG_CHAR: string = 's';

implementation

procedure RemoveTargetFile(Name: string);
var
  i, j:  integer;
  found: boolean;
begin
  found := False;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].target = Name then
    begin
      found := True;
    end;
  end;
  if found then
  begin
    for j := i to high(TargetFile) - 1 do
    begin
      TargetFile[j] := TargetFile[j + 1];
    end;
    SetLength(TargetFile, Length(TargetFile) - 1);
  end;
end;

procedure AddTargetFile(Name: string; offset_is_fake: boolean;
  seg_offset, addr_offset, def_start_addr: integer);
var
  i:     integer;
  found: boolean;
begin
  found := False;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].target = Name then
    begin
      found := True;
    end;
  end;
  if not found then
  begin
    SetLength(TargetFile, Length(TargetFile) + 1);
    i := Length(TargetFile) - 1;
    TargetFile[i].target := Name;
    TargetFile[i].filename := '';
    TargetFile[i].seg_offset := seg_offset;
    TargetFile[i].addr_offset := addr_offset;
    TargetFile[i].offset_is_fake := offset_is_fake;
    TargetFile[i].def_start_addr := def_start_addr;
  end;
end;

function GetTargetFileIndex(target_str: string): integer;
var
  i: integer;
begin
  Result := -1;
  if Length(TargetFile) > 0 then
  begin
    for i := low(TargetFile) to high(TargetFile) do
    begin
      if TargetFile[i].target = target_str then
      begin
        Result := i;
        exit;
      end;
    end;
  end;
end;

{ TPollThread }

procedure TPollThread.Update;
begin
{
  if bFatalError then
  begin
    // programmer not available
  end
  else
  begin
    // programmer available
    // update target volage
    sedtPower.Value := TargetVoltage;
  end;
}
end;

procedure TPollThread.Execute;
begin
  if not VSProg_Caller.Take() then
  begin
    // not available now
    exit;
  end;
  VSProg_Taken_By_Polling := True;

  VSProg_Caller.Application := Application.Location + VSPROG_STR;
  VSProg_Caller.RemoveAllParameters();
  VSProg_Caller.AddParameter('V"voltage"');
  //VSProg_Caller.Run(, False, True);

  Synchronize(@Update);
  VSProg_Taken_By_Polling := False;
end;

constructor TPollThread.Create();
begin
  FreeOnTerminate := False;
  inherited Create(True);
end;

{ TFormMain }

procedure TFormMain.ComboBoxSetText(var combo: TComboBox; aText: string);
var
  i: integer;
begin
  for i := 0 to combo.Items.Count - 1 do
  begin
    if combo.Items.Strings[i] = aText then
    begin
      combo.ItemIndex := i;
      break;
    end;
  end;
end;

procedure TFormMain.tPollProgrammerTimer(Sender: TObject);
begin
  if (Sender as TTimer).Enabled and (PollThread <> nil) and PollThread.Suspended then
  begin
    //    PollThread.Resume;
  end;
end;

procedure TFormMain.UpdateTitle();
var
  enable_str: string;
begin
  if pcMain.ActivePage.Enabled then
  begin
    enable_str := '';
  end
  else
  begin
    enable_str := '(*)';
  end;
  Caption := APP_STR + VERSION_STR + '-' + pcMain.ActivePage.Caption +
    enable_str + ' (' + VSProg_Version + ')';
end;

function TFormMain.VSProg_RunAlgorithm(var caller: TCLI_Caller;
  parser: TParserFunc; result_num: integer): boolean;
begin
  Result := False;
  VSProg_Parser.Prepare();
  VSProg_Parser.ParserFunc := parser;
  caller.Run(@VSProg_Parser.CommonParser, False, True);
  VSProg_Parser.CallbackFunc    := nil;
  VSProg_Parser.LogOutputEnable := True;

  if VSProg_Parser.HasError then
  begin
    Beep();
    MessageDlg('Error', VSProg_Parser.ErrorStr, mtError, [mbOK], 0);
  end
  else if VSProg_Parser.ResultStrings.Count >= result_num then
  begin
    Result := True;
  end;
end;

procedure TFormMain.VSProg_LogOutput(line: string);
begin
  memoLog.Lines.Add(line);
end;

procedure TFormMain.VSProg_LogProgress(ProgressInfo: TProgressInfoType; info: string);
begin
  case ProgressInfo of
    liStartSection:
    begin
      LogInfo(info);
      sbMain.Panels.Items[1].Text := '';
      pgbarMain.Position := 0;
    end;
    liStep:
    begin
      sbMain.Panels.Items[1].Text := sbMain.Panels.Items[1].Text + info;
      pgbarMain.StepIt;
    end;
    liEndSection:
    begin
      memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] :=
        memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] + info;
    end;
  end;
end;

procedure TFormMain.UpdateTargetFile();
var
  valid_file_num, i: integer;
begin
  valid_file_num := 0;
  cbboxInputFile.Clear;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].filename <> '' then
    begin
      cbboxInputFile.Items.Add(TargetFile[i].target + ':' + TargetFile[i].filename);
      Inc(valid_file_num);
    end;
  end;
  if valid_file_num > 1 then
  begin
    cbboxInputFile.Items.Insert(0, 'ALL');
  end;
  cbboxInputFile.ItemIndex := 0;
  cbboxInputFileChange(cbboxInputFile);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
  str_tmp: string;
begin
  xmlcfgMain.FileName := GetUserDir + 'vsgui.xml';
  if FileExists(xmlcfgMain.FileName) then
  begin
    JTAGPage_Init   := False;
    TargetPage_Init := False;
    AboutPage_Init  := False;
  end
  else
  begin
    JTAGPage_Init   := True;
    TargetPage_Init := True;
    AboutPage_Init  := True;
  end;

  VSProg_Taken_By_Polling := False;
  ShowCallerError := True;
  VSProg_Exists := True;
  bOpenFileOK := False;
  bReadOperation := False;
  bPageLock := False;

  HideDebugLog();

  // caller init
  VSProg_Caller := TCLI_Caller.Create();
  VSProg_Caller.Delimiter := '-';

  // parser init
  VSProg_Parser := TVSProg_Parser.Create;
  VSProg_Parser.LogProgressFunc := @VSProg_LogProgress;
  VSProg_Parser.LogOutputFunc := @VSProg_LogOutput;
  VSProg_Parser.CallbackFunc := nil;

  // poll thread init
  PollThread := TPollThread.Create;
  if Assigned(PollThread.FatalException) then
  begin
    raise PollThread.FatalException;
  end;

  lblTarget.Top    := cbboxTarget.Top + (cbboxTarget.Height - lblTarget.Height) div 2;
  btnTargetDetect.Top := cbboxTarget.Top + (cbboxTarget.Height -
    btnTargetDetect.Height) div 2;
  lblInputFile.Top := cbboxInputFile.Top + (cbboxInputFile.Height -
    lblInputFile.Height) div 2;
  lblMode.Top      := cbboxMode.Top + (cbboxMode.Height - lblMode.Height) div 2;

  FormMain.Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
  memoLog.Width  := LOGMEMO_WIDTH;

  tiMain.Icon      := Application.Icon;
  tiMain.PopUpMenu := pmTray;
  tiMain.Show;

  tsSTM8.TabVisible   := False;
  tsEEPROM.TabVisible := False;

  // get VSProg_Version
  VSProg_Version := '';
  if not PrepareToRunCLI then
  begin
    VSProg_Version := 'No Exists';
    UpdateTitle();
  end
  else
  begin
    VSProg_Caller.AddParametersString('--version');
    VSProg_Parser.LogOutputEnable := False;
    if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.VersionParser, 1) then
    begin
      VSProg_Version := VSProg_Parser.ResultStrings.Strings[0];
    end;
    UpdateTitle();
  end;

  // load last settings
  str_tmp := xmlcfgMain.GetValue('activepage', 'JTAG');
  for i := 0 to pcMain.PageCount - 1 do
  begin
    if pcMain.Page[i].Caption = str_tmp then
    begin
      pcMain.ActivePage := (pcMain.Page[i] as TTabSheet);
      break;
    end;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  VSProg_Caller.Destroy;
  VSProg_Parser.Destroy;

  if PollThread <> nil then
  begin
    PollThread.Terminate;
    PollThread.Free;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pgbarMain.Height := sbMain.Height - 1;
  pgbarMain.Top    := sbMain.Top + 1;
  pgbarMain.Left   := sbMain.Panels.Items[0].Width + 2;
  pgbarMain.Width  := Width - sbMain.Panels.Items[0].Width - 2;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FormResize(Sender);
  UpdateShowing;
  pcMainPageChanged(pcMain);
  if VSProg_Exists and (PollThread <> nil) then
  begin
    //    tPollProgrammer.Enabled := True;
  end;
end;

procedure TFormMain.lbledtExtraParaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Pos('show all support', lbledtExtraPara.Text) = 1 then
    begin
      tsSTM8.TabVisible    := True;
      tsEEPROM.TabVisible  := True;
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
  memoLog.AdjustSize;
end;

procedure TFormMain.HideDebugLog();
begin
  if not DEBUG_LOG_SHOW then
  begin
    Width := pnlMain.Width + 1;
  end;
end;

procedure TFormMain.tbPowerChange(Sender: TObject);
begin
  sedtPower.Value := (Sender as TTrackBar).Position;
  sedtPower.Hint  := IntToStr(sedtPower.Value) + 'mV';
  (Sender as TTrackBar).Hint := sedtPower.Hint;
end;

procedure TFormMain.pcMainChanging(Sender: TObject; var AllowChange: boolean);
begin
  if (VSProg_Caller <> nil) then
  begin
    AllowChange := (not VSProg_Caller.IsRunning()) and (not tDelay.Enabled) and
      (not bPageLock);
    if Allowchange then
    begin
      bPageLock := True;
    end;
  end;
end;

procedure TFormMain.pcMainPageChanged(Sender: TObject);
var
  index: integer;
  ser:   TBlockSerial;
  Fails: integer;
begin
  if not pcMain.ActivePage.Enabled then
  begin
    UpdateTitle();
    bPageLock := False;
    exit;
  end;

  SetLength(TargetSetting, 0);
  cbboxInputFile.Clear;
  memoInfo.Clear;

  // initialize GUI
  if pcMain.ActivePage = tsAbout then
  begin
    HideDebugLog();
    TargetType := TT_NONE;

    // Init serial port combobox
    cbboxCOM.Clear;

    for index := low(COMPORTS) to high(COMPORTS) do
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

    if not PrepareToRunCli then
    begin
      pcMain.ActivePage.Enabled := False;
    end
    else
    begin
      VSProg_Caller.UnTake();
    end;

    // update settings
    if not AboutPage_Init then
    begin
      AboutPage_Init := True;
      fnFW.FileName  := xmlcfgMain.GetValue('fw/filename', '');
      ComboBoxSetText(cbboxCOM, xmlcfgMain.GetValue('fw/comm', ''));
    end;
    UpdateTitle();
    bPageLock := False;
    exit;
  end
  else if pcMain.ActivePage = tsJTAG then
  begin
    TargetType := TT_JTAG;
    ARM_Init();

    memoLog.Clear;
    ShowDebugLog();

    Fails := 0;
    if not PrepareToRunOpenOCD then
    begin
      Inc(Fails);
      gbOpenOCD.Enabled := False;
    end
    else
    begin
      VSProg_Caller.UnTake();
    end;
    if not PrepareToRunCli then
    begin
      Inc(Fails);
      gbSVFPlayer.Enabled := False;
      gbPower.Enabled     := False;
    end
    else
    begin
      VSProg_Caller.UnTake();
    end;
    if Fails = 2 then
    begin
      TargetType := TT_NONE;
      pcMain.ActivePage.Enabled := False;
    end;

    // update settings
    if not JTAGPage_Init then
    begin
      JTAGPage_Init := True;
      ComboBoxSetText(cbboxOpenOCDInterface, xmlcfgMain.GetValue(
        'openocd/interface', ''));
      ComboBoxSetText(cbboxOpenOCDTarget, xmlcfgMain.GetValue('openocd/target', ''));
      ComboBoxSetText(cbboxOpenOCDScript, xmlcfgMain.GetValue('openocd/script', ''));
      edtOpenOCDOption.Text := xmlcfgMain.GetValue('openocd/option', '');
      fneditSVFFile.FileName := xmlcfgMain.GetValue('svf/filename', '');
      edtSVFOption.Text := xmlcfgMain.GetValue('svf/option', '');
      sedtPower.Value := xmlcfgMain.GetValue('power/voltage', 0);
    end;

    UpdateTitle();
    bPageLock := False;
    exit;
  end
  else
  begin
    SetLength(TargetFile, 0);
    HideDebugLog();

    if not PrepareToRunCli then
    begin
      pcMain.ActivePage.Enabled := False;
      bPageLock := False;
      exit;
    end;

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
      TargetType := TT_NONE;
      VSProg_Caller.UnTake();
      exit;
    end;

    tDelay.Enabled := True;
  end;
end;

procedure TFormMain.btnTargetDetectClick(Sender: TObject);
begin
  if btnTargetDetect.Caption = COMSETUP_STR then
  begin
    FormComSetup.ShowModal;
    FormComSetup.GetComMode(ComMode);
  end
  else if btnTargetDetect.Caption = AUTODETECT_STR then
  begin
    if not PrepareToRunCli then
    begin
      exit;
    end;

    VSProg_Caller.AddParameter('s' + cbboxTarget.Items.Strings[0]);
    if cbboxMode.Enabled and (cbboxMode.Text <> '') then
    begin
      VSProg_Caller.AddParameter('m' + cbboxMode.Text[1]);
    end;
    if lbledtFreq.Enabled and (lbledtFreq.Text <> '') then
    begin
      VSProg_Caller.AddParameter('F' + lbledtFreq.Text);
    end;
    if lbledtExtraPara.Text <> '' then
    begin
      VSProg_Caller.AddParametersString(lbledtExtraPara.Text);
    end;

    LogInfo('Running...');
    if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.AutoDetectParser, 1) then
    begin
      ComboBoxSetText(cbboxTarget, VSProg_Parser.ResultStrings.Strings[0]);
      cbboxTargetChange(cbboxTarget);
    end;
    LogInfo('Idle');
  end;
end;

procedure TFormMain.btnUpdateClick(Sender: TObject);
begin
  if cbboxCOM.Text = '' then
  begin
    Beep();
    MessageDlg('Error, no comm port found?', '......', mtError, [mbOK], 0);
    exit;
  end;
  if fnFW.FileName = '' then
  begin
    Beep();
    MessageDlg('Error', 'Please sellect FW Hex file.', mtError, [mbOK], 0);
  end;

  if not PrepareToRunCli then
  begin
    exit;
  end;

  VSProg_Caller.AddParametersString('-G -Z -ccomisp_stm32 -C' +
    cbboxCOM.Text + ' ' + ' -x0x08002000 -oe -owf -I"' + fnFW.FileName + '"');
  LogInfo('Running...');
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0) then
  begin
    MessageDlg('OK', 'FW Updated OK.', mtInformation, [mbOK], 0);
  end;
  LogInfo('Idle');
end;

function TFormMain.VSProg_CommonGetEnabledOperationString(): string;
begin
  Result := '';
  if chkboxApp.Enabled and chkboxApp.Checked then
  begin
    Result := Result + FLASH_CHAR;
  end;
  if chkboxEE.Enabled and chkboxEE.Checked then
  begin
    Result := Result + EEPROM_CHAR;
  end;
  if chkboxFuse.Enabled and chkboxFuse.Checked then
  begin
    Result := Result + FUSE_CHAR;
  end;
  if chkboxLock.Enabled and chkboxLock.Checked then
  begin
    Result := Result + LOCK_CHAR;
  end;
  if chkboxUsrSig.Enabled and chkboxUsrSig.Checked then
  begin
    Result := Result + USRSIG_CHAR;
  end;
  if chkboxCali.Enabled and chkboxCali.Checked then
  begin
    Result := Result + CALI_CHAR;
  end;
end;

function TFormMain.VSProg_CommonAddEraseOperation(): boolean;
begin
  VSProg_Caller.AddParameter('oe');
  Result := True;
end;

function TFormMain.VSProg_CommonAddWriteOperation(): boolean;
var
  para: string;
begin
  para := VSProg_CommonGetEnabledOperationString();
  if para = '' then
  begin
    Result := False;
  end
  else
  begin
    Result := True;
    para   := 'ow' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
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
    Result := False;
  end
  else
  begin
    Result := True;
    para   := 'ov' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
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
    Result := False;
  end
  else
  begin
    Result := True;
    para   := 'or' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

procedure TFormMain.btnVerifyClick(Sender: TObject);
begin
  if not PrepareToRunCli then
  begin
    exit;
  end;

  PrepareOperationParameters(VSProg_Caller);
  if not VSProg_CommonAddVerifyOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0);
  LogInfo('Idle');
end;

procedure TFormMain.btnEraseClick(Sender: TObject);
begin
  if not PrepareToRunCli then
  begin
    exit;
  end;

  PrepareOperationParameters(VSProg_Caller);
  if not VSProg_CommonAddEraseOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0);
  LogInfo('Idle');
end;

procedure TFormMain.btnOpenFileClick(Sender: TObject);
var
  i, file_num: integer;
begin
  bOpenFileOK := False;
  file_num    := Length(TargetFile);
  if file_num = 0 then
  begin
    exit;
  end
  else if file_num > 1 then
  begin
    FormFileSelector.Reset;
    for i := 0 to file_num - 1 do
    begin
      FormFileSelector.AddFileSetting(TargetFile[i].target, TargetFile[i].filename);
    end;
    if mrOk = FormFileSelector.ShowModal then
    begin
      for i := 0 to file_num - 1 do
      begin
        TargetFile[i].filename :=
          FormFileSelector.GetFileNameByTargetName(TargetFile[i].target);
      end;
      UpdateTargetFile();
      bOpenFileOK := True;
    end;
  end
  else
  begin
    odInputFile.FileName := TargetFile[0].filename;
    if odInputFile.Execute then
    begin
      TargetFile[0].filename := odInputFile.FileName;
      UpdateTargetFile();
      bOpenFileOK := True;
    end;
  end;
end;

procedure TFormMain.btnOpenOCDRunClick(Sender: TObject);
begin
  if not PrepareToRunOpenOCD then
  begin
    exit;
  end;

  if edtOpenOCDOption.Text <> '' then
  begin
    VSProg_Caller.AddParametersString(edtOpenOCDOption.Text);
  end;
  if cbboxOpenOCDInterface.Text <> '' then
  begin
    VSProg_Caller.AddParameter('f "' + AnsiToUtf8(Application.Location) +
      cbboxOpenOCDInterface.Text + '"');
  end;
  if cbboxOpenOCDTarget.Text <> '' then
  begin
    VSProg_Caller.AddParameter('f "' + AnsiToUtf8(Application.Location) +
      cbboxOpenOCDTarget.Text + '"');
  end;
  if cbboxOpenOCDScript.Text <> '' then
  begin
    VSProg_Caller.AddParameter('f "' + AnsiToUtf8(Application.Location) +
      cbboxOpenOCDScript.Text + '"');
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0);
  LogInfo('Idle');
end;

procedure TFormMain.btnOpenOCDStopClick(Sender: TObject);
begin
  VSProg_Caller.Stop();
end;

procedure TFormMain.btnEditAppClick(Sender: TObject);
var
  index: integer;
  targetdefine: string;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    Beep();
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  index := GetTargetFileIndex('Flash');
  if (index < 0) or (TargetFile[index].filename = '') then
  begin
    index := GetTargetFileIndex('ALL');
    if (index < 0) or (TargetFile[index].filename = '') then
    begin
      Beep();
      MessageDlg('Error', 'No File Defined.', mtError, [mbOK], 0);
      exit;
    end;
  end;
  if not FileExists(TargetFile[index].filename) then
  begin
    Beep();
    MessageDlg('Error', TargetFile[index].filename + 'not exists.', mtError, [mbOK], 0);
    exit;
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCli then
  begin
    exit;
  end;
  PrepareBaseParameters(VSProg_Caller);
  VSProg_Caller.AddParameter('Pflash');
  FormParaEditor.FreeRecord();
  VSProg_Parser.CallbackFunc    := @VSProg_MemoryTargetParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if not VSProg_RunAlgorithm(VSProg_Caller,
    @VSProg_Parser.MemoryTargetInfoParser, 0) then
  begin
    exit;
  end;

  FormHexEditor.FileName := TargetFile[index].filename;
  FormHexEditor.StartAddress := MemoryInfo.start_addr;
  FormHexEditor.DataByteSize := MemoryInfo.byte_size;
  FormHexEditor.DefaultData := MemoryInfo.default_byte;
  FormHexEditor.Target := (Sender as TButton).Caption;
  if not TargetFile[index].offset_is_fake then
  begin
    FormHexEditor.SegOffset     := TargetFile[index].seg_offset;
    FormHexEditor.AddressOffset := TargetFile[index].addr_offset;
  end
  else
  begin
    FormHexEditor.SegOffset     := 0;
    FormHexEditor.AddressOffset := 0;
  end;
  FormHexEditor.ShowModal;
end;

procedure TFormMain.btnEditCaliClick(Sender: TObject);
var
  targetdefine:  string;
  init, bytelen: integer;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    Beep();
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  if not chkboxNoconnect.Checked then
  begin
    // call 'vsprog -orc' to read calibration settings from target
    if not PrepareToRunCli then
    begin
      exit;
    end;
    PrepareBaseParameters(VSProg_Caller);
    VSProg_Caller.AddParameter('orc');
    if not VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.CaliDataParser, 1) then
    begin
      exit;
    end;
    VSProg_CommonUpdateCali(StrToInt(VSProg_Parser.ResultStrings.Strings[0]) and
      TargetSetting[cbboxTarget.ItemIndex].cali_default,
      TargetSetting[cbboxTarget.ItemIndex].cali_bytelen);
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCli then
  begin
    exit;
  end;
  PrepareBaseParameters(VSProg_Caller);
  VSProg_Caller.AddParameter('Pcalibration');
  FormParaEditor.FreeRecord();
  VSProg_Parser.LogOutputEnable := False;
  VSProg_Parser.CallbackFunc    := @VSProg_SettingTargetParserCallback;
  if not VSProg_RunAlgorithm(VSProg_Caller,
    @VSProg_Parser.SettingTargetInfoParser, 0) then
  begin
    exit;
  end;

  init    := TargetSetting[cbboxTarget.ItemIndex].cali_default;
  bytelen := TargetSetting[cbboxTarget.ItemIndex].cali_bytelen;
  FormParaEditor.SetParameter(init, bytelen, StrToInt(lbledtCali.Text),
    'Calibration', not chkboxNowarning.Checked);
  if mrOk = FormParaEditor.ShowModal then
  begin
    // OK clicked, get value
    VSProg_CommonUpdateCali(FormParaEditor.GetResult(), bytelen);
  end;
end;

procedure TFormMain.btnEditEEClick(Sender: TObject);
var
  index: integer;
  targetdefine: string;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    Beep();
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  index := GetTargetFileIndex('EEPROM');
  if (index < 0) or (TargetFile[index].filename = '') then
  begin
    index := GetTargetFileIndex('ALL');
    if (index < 0) or (TargetFile[index].filename = '') then
    begin
      Beep();
      MessageDlg('Error', 'No File Defined.', mtError, [mbOK], 0);
      exit;
    end;
  end;
  if not FileExists(TargetFile[index].filename) then
  begin
    Beep();
    MessageDlg('Error', TargetFile[index].filename + 'not exists.', mtError, [mbOK], 0);
    exit;
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCli then
  begin
    exit;
  end;
  PrepareBaseParameters(VSProg_Caller);
  VSProg_Caller.AddParameter('Peeprom');
  FormParaEditor.FreeRecord();
  VSProg_Parser.CallbackFunc    := @VSProg_MemoryTargetParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if not VSProg_RunAlgorithm(VSProg_Caller,
    @VSProg_Parser.MemoryTargetInfoParser, 0) then
  begin
    exit;
  end;

  FormHexEditor.FileName := TargetFile[index].filename;
  FormHexEditor.StartAddress := MemoryInfo.start_addr;
  FormHexEditor.DataByteSize := MemoryInfo.byte_size;
  FormHexEditor.DefaultData := MemoryInfo.default_byte;
  FormHexEditor.Target := (Sender as TButton).Caption;
  if not TargetFile[index].offset_is_fake then
  begin
    FormHexEditor.SegOffset     := TargetFile[index].seg_offset;
    FormHexEditor.AddressOffset := TargetFile[index].addr_offset;
  end
  else
  begin
    FormHexEditor.SegOffset     := 0;
    FormHexEditor.AddressOffset := 0;
  end;
  FormHexEditor.ShowModal;
end;

procedure TFormMain.btnEditFuseClick(Sender: TObject);
var
  targetdefine:  string;
  init, bytelen: integer;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    Beep();
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  if not chkboxNoconnect.Checked then
  begin
    // call 'vsprog -oru' to read fuse settings from target
    if not PrepareToRunCli then
    begin
      exit;
    end;
    PrepareBaseParameters(VSProg_Caller);
    VSProg_Caller.AddParameter('oru');
    if not VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.FuseDataParser, 1) then
    begin
      exit;
    end;
    VSProg_CommonUpdateFuse(StrToInt(VSProg_Parser.ResultStrings.Strings[0]) and
      TargetSetting[cbboxTarget.ItemIndex].fuse_default,
      TargetSetting[cbboxTarget.ItemIndex].fuse_bytelen);
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCli then
  begin
    exit;
  end;
  PrepareBaseParameters(VSProg_Caller);
  VSProg_Caller.AddParameter('Pfuse');
  FormParaEditor.FreeRecord();
  VSProg_Parser.CallbackFunc    := @VSProg_SettingTargetParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if not VSProg_RunAlgorithm(VSProg_Caller,
    @VSProg_Parser.SettingTargetInfoParser, 0) then
  begin
    exit;
  end;

  init    := TargetSetting[cbboxTarget.ItemIndex].fuse_default;
  bytelen := TargetSetting[cbboxTarget.ItemIndex].fuse_bytelen;
  FormParaEditor.SetParameter(init, bytelen, StrToInt(lbledtFuse.Text),
    'Fuse', not chkboxNowarning.Checked);
  if mrOk = FormParaEditor.ShowModal then
  begin
    // OK clicked, get value
    VSProg_CommonUpdateFuse(FormParaEditor.GetResult(), bytelen);
  end;
end;

procedure TFormMain.btnEditLockClick(Sender: TObject);
var
  targetdefine:  string;
  init, bytelen: integer;
begin
  targetdefine := GetTargetDefineParameters();
  if targetdefine[1] = 's' then
  begin
    Beep();
    MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
    exit;
  end;

  if not chkboxNoconnect.Checked then
  begin
    // call 'vsprog -orl' to read lock settings from target
    if not PrepareToRunCli then
    begin
      exit;
    end;
    PrepareBaseParameters(VSProg_Caller);
    VSProg_Caller.AddParameter('orl');
    if not VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.LockDataParser, 1) then
    begin
      exit;
    end;
    VSProg_CommonUpdateLock(StrToInt(VSProg_Parser.ResultStrings.Strings[0]) and
      TargetSetting[cbboxTarget.ItemIndex].lock_default,
      TargetSetting[cbboxTarget.ItemIndex].lock_bytelen);
  end;

  // call 'vsprog -Ppara' to extract para settings
  if not PrepareToRunCli then
  begin
    exit;
  end;
  PrepareBaseParameters(VSProg_Caller);
  VSProg_Caller.AddParameter('Plock');
  FormParaEditor.FreeRecord();
  VSProg_Parser.CallbackFunc    := @VSProg_SettingTargetParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if not VSProg_RunAlgorithm(VSProg_Caller,
    @VSProg_Parser.SettingTargetInfoParser, 0) then
  begin
    exit;
  end;

  init    := TargetSetting[cbboxTarget.ItemIndex].lock_default;
  bytelen := TargetSetting[cbboxTarget.ItemIndex].lock_bytelen;
  FormParaEditor.SetParameter(init, bytelen, StrToInt(lbledtLock.Text),
    'Lock', not chkboxNowarning.Checked);
  if mrOk = FormParaEditor.ShowModal then
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
  // select output file
  btnOpenFile.Click;
  if not bOpenFileOK then
  begin
    exit;
  end;

  if not PrepareToRunCli then
  begin
    exit;
  end;
  bReadOperation := True;
  PrepareOperationParameters(VSProg_Caller);
  if not VSProg_CommonAddReadOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add read operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    bReadOperation := False;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0);
  LogInfo('Idle');
  bReadOperation := False;
end;

procedure TFormMain.btnSetPowerClick(Sender: TObject);
begin
  if not PrepareToRunCli then
  begin
    exit;
  end;

  VSProg_Caller.AddParameter('V"powerout ' + IntToStr(sedtPower.Value) + '"');
  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, nil, 0);
  LogInfo('Idle');
end;

procedure TFormMain.btnSVFPlayerRunClick(Sender: TObject);
begin
  if fneditSVFFile.FileName <> '' then
  begin
    if not PrepareToRunCli then
    begin
      exit;
    end;
    VSProg_Caller.AddParametersString('-G -ssvf_player -I"' +
      fneditSVFFile.FileName + '"');
    if edtSVFOption.Text <> '' then
    begin
      VSProg_Caller.AddParametersString(edtSVFOption.Text);
    end;

    LogInfo('Running...');
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0);
    LogInfo('Idle');
  end
  else
  begin
    Beep();
    MessageDlg('Error', 'Please input SVF file.', mtError, [mbOK], 0);
  end;
end;

procedure TFormMain.btnWriteClick(Sender: TObject);
begin
  if not PrepareToRunCli then
  begin
    exit;
  end;

  PrepareOperationParameters(VSProg_Caller);

  if chkboxEraseBeforeWrite.Checked and not VSProg_CommonAddEraseOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  if not VSProg_CommonAddWriteOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add write operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  if chkboxVerifyAfterWrite.Checked and not VSProg_CommonAddVerifyOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0);
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
    lbledtFuse.Enabled  := True;
    btnEditFuse.Enabled := True;
  end
  else
  begin
    lbledtFuse.Enabled  := False;
    btnEditFuse.Enabled := False;
  end;
  VSProg_CommonUpdateFuse(TargetSetting[index].fuse_default,
    TargetSetting[index].fuse_bytelen);

  if TargetSetting[index].lock_bytelen > 0 then
  begin
    lbledtLock.Enabled  := True;
    btnEditLock.Enabled := True;
  end
  else
  begin
    lbledtLock.Enabled  := False;
    btnEditLock.Enabled := False;
  end;
  VSProg_CommonUpdateLock(TargetSetting[index].lock_default,
    TargetSetting[index].lock_bytelen);

  if TargetSetting[index].cali_bytelen > 0 then
  begin
    lbledtCali.Enabled  := True;
    btnEditCali.Enabled := True;
  end
  else
  begin
    lbledtCali.Enabled  := False;
    btnEditCali.Enabled := False;
  end;
  VSProg_CommonUpdateCali(TargetSetting[index].cali_default,
    TargetSetting[index].cali_bytelen);

  if TargetSetting[index].usrsig_bytelen > 0 then
  begin
    lbledtUsrSig.Enabled  := True;
    btnEditUsrSig.Enabled := True;
  end
  else
  begin
    lbledtUsrSig.Enabled  := False;
    btnEditUsrSig.Enabled := False;
  end;
  VSProg_CommonUpdateUsrSig(TargetSetting[index].usrsig_default,
    TargetSetting[index].usrsig_bytelen);

  if TargetSetting[index].mode = '' then
  begin
    cbboxMode.Clear;
    cbboxMode.Enabled := False;
  end
  else
  begin
    cbboxMode.Enabled := True;
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
var
  i: integer;
begin
  VSProg_Caller.Stop();

  // save settings to config file
  xmlcfgMain.SetValue('activepage', pcMain.ActivePage.Caption);
  xmlcfgMain.SetValue('openocd/interface', cbboxOpenOCDInterface.Text);
  xmlcfgMain.SetValue('openocd/target', cbboxOpenOCDTarget.Text);
  xmlcfgMain.SetValue('openocd/script', cbboxOpenOCDScript.Text);
  xmlcfgMain.SetValue('openocd/option', edtOpenOCDOption.Text);
  xmlcfgMain.SetValue('svf/filename', fneditSVFFile.FileName);
  xmlcfgMain.SetValue('svf/option', edtSVFOption.Text);
  xmlcfgMain.SetValue('power/voltage', sedtPower.Value);
  xmlcfgMain.SetValue('fw/filename', fnFW.FileName);
  xmlcfgMain.SetValue('fw/comm', cbboxCOM.Text);
  xmlcfgMain.SetValue('target/chip', cbboxTarget.Text);
  xmlcfgMain.SetValue('target/mode', cbboxMode.Text);
  xmlcfgMain.SetValue('target/filename', cbboxInputFile.Text);
  xmlcfgMain.SetValue('target/freq', lbledtFreq.Text);
  if Length(TargetFile) > 0 then
  begin
    xmlcfgMain.SetValue('target/files/number', Length(TargetFile));
    for i := low(TargetFile) to high(TargetFile) do
    begin
      xmlcfgMain.SetValue('target/files/' + IntToStr(i) + '/target',
        TargetFile[i].target);
      xmlcfgMain.SetValue('target/files/' + IntToStr(i) + '/filename',
        TargetFile[i].filename);
    end;
  end;
  xmlcfgMain.SetValue('target/fuse', lbledtFuse.Text);
  xmlcfgMain.SetValue('target/lock', lbledtLock.Text);
  xmlcfgMain.SetValue('target/cali', lbledtCali.Text);
  xmlcfgMain.SetValue('target/usrsig', lbledtUsrSig.Text);
  xmlcfgMain.SetValue('target/nc', chkboxNoconnect.Checked);
  xmlcfgMain.SetValue('target/nw', chkboxNowarning.Checked);
  xmlcfgMain.SetValue('target/flashen', chkboxApp.Checked);
  xmlcfgMain.SetValue('target/eepromen', chkboxEE.Checked);
  xmlcfgMain.SetValue('target/fuseen', chkboxFuse.Checked);
  xmlcfgMain.SetValue('target/locken', chkboxLock.Checked);
  xmlcfgMain.SetValue('target/usrsigen', chkboxUsrSig.Checked);
  xmlcfgMain.SetValue('target/calien', chkboxCali.Checked);
  xmlcfgMain.SetValue('target/ebw', chkboxEraseBeforeWrite.Checked);
  xmlcfgMain.SetValue('target/vaw', chkboxVerifyAfterWrite.Checked);
  xmlcfgMain.SetValue('target/mass', chkboxMP.Checked);
  xmlcfgMain.SetValue('target/extraparam', lbledtExtraPara.Text);
  xmlcfgMain.Flush;

  tiMain.Hide;
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
    sbMain.Panels.Items[1].Text := '';
    pgbarmain.Position := 0;
  end;
end;

function TFormMain.PrepareToRunOpenOCD: boolean;
var
  strTmp: string;
begin
  Result := False;
  strTmp := Application.Location + OPENOCD_APP_STR;
  if not FileExists(strTmp) then
  begin
    exit;
  end;

  // take it first
  if not VSProg_Caller.Take() then
  begin
    if VSProg_Taken_By_Polling then
    begin
      // occupied by polling thread, wait it
//      PollThread.WaitFor;
      if not VSProg_Caller.Take() then
      begin
        // give up ......
        exit;
      end;
    end
    else
    begin
      // other operation is on the way
      exit;
    end;
  end;

  VSProg_Caller.Application := strTmp;
  VSProg_Caller.RemoveAllParameters();
  Result := True;
  memoLog.Clear;
  memoInfo.Clear;
end;

function TFormMain.PrepareToRunCLI: boolean;
var
  strTmp: string;
begin
  Result := False;
  strTmp := Application.Location + VSPROG_STR;
  if not FileExists(strTmp) then
  begin
    if VSProg_Exists then
    begin
      // For Once
      Beep();
      MessageDlg('Error, missing vsprog',
        'Opps, Where is my vsprog? I cannot work without her.', mtError, [mbOK], 0);
    end;
    VSProg_Exists := False;
    exit;
  end;

  // take it first
  if not VSProg_Caller.Take() then
  begin
    if VSProg_Taken_By_Polling then
    begin
      // occupied by polling thread, wait it
//      PollThread.WaitFor;
      if not VSProg_Caller.Take() then
      begin
        // give up ......
        exit;
      end;
    end
    else
    begin
      // other operation is on the way
      exit;
    end;
  end;

  VSProg_Caller.Application := strTmp;
  VSProg_Caller.RemoveAllParameters();
  Result := True;
  memoLog.Clear;
  memoInfo.Clear;
end;

function TFormMain.GetTargetDefineParameters(): string;
begin
  // target series_name or chip_name
  if ((TargetType <> TT_COMISP) and (TargetType <> TT_CORTEXM3)) and
    (cbboxTarget.ItemIndex = 0) then
  begin
    Result := 's' + cbboxTarget.Items.Strings[cbboxTarget.ItemIndex];
  end
  else
  begin
    Result := 'c' + cbboxTarget.Items.Strings[cbboxTarget.ItemIndex];
  end;
end;

procedure TFormMain.PrepareBaseParameters(var caller: TCLI_Caller);
var
  i: integer;
  io_file_opt, str: string;
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
      ComMode.stopbit + ' ' + ComMode.handshake + ComMode.auxpin + '"');
  end;

  // input/output file
  if bReadOperation then
  begin
    io_file_opt := 'O';
  end
  else
  begin
    io_file_opt := 'I';
  end;
  if Length(TargetFile) = 0 then
  begin
    if cbboxInputFile.Text <> '' then
    begin
      caller.AddParameter(io_file_opt + '"' + cbboxInputFile.Text + '@0,0"');
    end;
  end
  else if cbboxInputFile.Text <> 'ALL' then
  begin
    // enable selected input file
    for i := low(TargetFile) to high(TargetFile) do
    begin
      str := Copy(cbboxInputFile.Text, 1, Pos(':', cbboxInputFile.Text) - 1);
      if TargetFile[i].target = str then
      begin
        if TargetFile[i].filename <> '' then
        begin
          str := LowerCase(ExtractFileExt(TargetFile[i].filename));
          if str = '.hex' then
          begin
            caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
              '@' + IntToStr(TargetFile[i].seg_offset) + ',' +
              IntToStr(TargetFile[i].addr_offset) + '"');
          end
          else if str = '.bin' then
          begin
            caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
              '@' + IntToStr(TargetFile[i].seg_offset) + ',' +
              IntToStr(TargetFile[i].addr_offset + TargetFile[i].def_start_addr) + '"');
          end;
        end;
        break;
      end;
    end;
  end
  else
  begin
    // enable all input file
    for i := low(TargetFile) to high(TargetFile) do
    begin
      if TargetFile[i].filename <> '' then
      begin
        str := LowerCase(ExtractFileExt(TargetFile[i].filename));
        if str = '.hex' then
        begin
          caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
            '@' + IntToStr(TargetFile[i].seg_offset) + ',' +
            IntToStr(TargetFile[i].addr_offset) + '"');
        end
        else if str = '.bin' then
        begin
          caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
            '@' + IntToStr(TargetFile[i].seg_offset) + ',' +
            IntToStr(TargetFile[i].addr_offset + TargetFile[i].def_start_addr) + '"');
        end;
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

procedure TFormMain.PrepareOperationParameters(var caller: TCLI_Caller);
begin
  // enable GUI mode
  caller.AddParameter('G');
  PrepareBaseParameters(caller);

  // Fuse
  if lbledtFuse.Enabled and chkboxFuse.Enabled and chkboxFuse.Checked and
    (lbledtFuse.Text <> '') then
  begin
    caller.AddParameter('f' + lbledtFuse.Text);
  end;

  // Lock
  if lbledtLock.Enabled and chkboxLock.Enabled and chkboxLock.Checked and
    (lbledtLock.Text <> '') then
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
  i, j:    integer;
  str_tmp: string;
  int_tmp: integer;
begin
  tDelay.Enabled := False;

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
    // BUG IN SOFTWARE IF RUN HERE
  end;

  if success and (cbboxTarget.ItemIndex >= 0) then
  begin
    gbChipName.Parent  := pcMain.ActivePage;
    gbInputFile.Parent := pcMain.ActivePage;
    gbOption.Parent    := pcMain.ActivePage;
    gbOperation.Parent := pcMain.ActivePage;

    cbboxTargetChange(cbboxTarget);

    AdjustComponentColor(cbboxMode);
    AdjustComponentColor(lbledtFuse);
    AdjustComponentColor(lbledtLock);
    AdjustComponentColor(lbledtCali);
    AdjustComponentColor(lbledtUsrSig);
    AdjustComponentColor(lbledtFreq);

    if not TargetPage_Init then
    begin
      TargetPage_Init := True;
      str_tmp := xmlcfgMain.GetValue('target/chip', '');
      success := False;
      for i := 0 to cbboxTarget.Items.Count - 1 do
      begin
        if str_tmp = cbboxTarget.Items.Strings[i] then
        begin
          success := True;
          break;
        end;
      end;
      if success then
      begin
        ComboBoxSetText(cbboxTarget, xmlcfgMain.GetValue('target/chip', ''));
        cbboxTargetChange(cbboxTarget);
        ComboBoxSetText(cbboxMode, xmlcfgMain.GetValue('target/mode', ''));
        cbboxModeChange(cbboxMode);
        lbledtFreq.Text := xmlcfgMain.GetValue('target/freq', '');
        int_tmp := xmlcfgMain.GetValue('target/files/number', 0);
        if Length(TargetFile) < int_tmp then
        begin
          // Error here, try to recovery
          xmlcfgMain.DeletePath('target/files');
          xmlcfgMain.SetValue('target/files/number', 0);
          int_tmp := 0;
        end;
        if int_tmp > 0 then
        begin
          for i := 0 to int_tmp - 1 do
          begin
            j := GetTargetFileIndex(xmlcfgMain.GetValue('target/files/' +
              IntToStr(i) + '/target', ''));
            if j < 0 then
            begin
              // Error here, try to recovery
              xmlcfgMain.DeletePath('target/files');
              xmlcfgMain.SetValue('target/files/number', 0);
              int_tmp := 0;
              break;
            end;
            TargetFile[j].filename :=
              xmlcfgMain.GetValue('target/files/' + IntToStr(i) + '/filename', '');
          end;
        end;
        UpdateTargetFile();
        ComboBoxSetText(cbboxInputFile, xmlcfgMain.GetValue('target/filename', ''));
        lbledtFuse.Text      := xmlcfgMain.GetValue('target/fuse', '');
        lbledtLock.Text      := xmlcfgMain.GetValue('target/lock', '');
        lbledtCali.Text      := xmlcfgMain.GetValue('target/cali', '');
        lbledtUsrSig.Text    := xmlcfgMain.GetValue('target/usrsig', '');
        chkboxNoconnect.Checked := xmlcfgMain.GetValue('target/nc', False);
        chkboxNowarning.Checked := xmlcfgMain.GetValue('target/nw', False);
        chkboxApp.Checked    := xmlcfgMain.GetValue('target/flashen', False);
        chkboxEE.Checked     := xmlcfgMain.GetValue('target/eepromen', False);
        chkboxFuse.Checked   := xmlcfgMain.GetValue('target/fuseen', False);
        chkboxLock.Checked   := xmlcfgMain.GetValue('target/locken', False);
        chkboxUsrSig.Checked := xmlcfgMain.GetValue('target/usrsigen', False);
        chkboxCali.Checked   := xmlcfgMain.GetValue('target/calien', False);
        chkboxEraseBeforeWrite.Checked := xmlcfgMain.GetValue('target/ebw', False);
        chkboxVerifyAfterWrite.Checked := xmlcfgMain.GetValue('target/vaw', False);
        chkboxMP.Checked     := xmlcfgMain.GetValue('target/mass', False);
        lbledtExtraPara.Text := xmlcfgMain.GetValue('target/extraparam', '');
      end;
    end;
  end
  else
  begin
    TargetType := TT_NONE;
    pcMain.ActivePage.Enabled := False;
  end;
  UpdateTitle();
  bPageLock := False;
end;

procedure TFormMain.tiMainClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
end;

function TFormMain.VSProg_LogProcess(ProgressInfo: TProgressInfoType;
  info: string): boolean;
begin
  Result := True;

  case ProgressInfo of
    liStartSection:
    begin
      LogInfo(info + '...');
      sbMain.Panels.Items[1].Text := '';
      pgbarMain.Position := 0;
    end;
    liStep:
    begin
      sbMain.Panels.Items[1].Text := sbMain.Panels.Items[1].Text + info;
      pgbarMain.StepIt;
    end;
    liEndSection:
    begin
      memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] :=
        memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] + info;
    end;
  end;
end;

function TFormMain.VSProg_MemoryTargetParserCallback(line: string): boolean;
var
  seg_addr, start_addr, byte_size, default_byte: integer;
  parse_result: boolean;
begin
  Result := True;
  parse_result := GetIntegerParameter(line, 'seg_addr', seg_addr);
  parse_result := parse_result and GetIntegerParameter(line, 'start_addr', start_addr);
  parse_result := parse_result and GetIntegerParameter(line, 'default_byte',
    default_byte);
  parse_result := parse_result and GetIntegerParameter(line, 'byte_size', byte_size);
  if parse_result then
  begin
    MemoryInfo.seg_addr     := seg_addr;
    MemoryInfo.start_addr   := start_addr;
    MemoryInfo.default_byte := byte(default_byte);
    MemoryInfo.byte_size    := byte_size;
  end;
end;

function TFormMain.VSProg_SettingTargetParserCallback(line: string): boolean;
var
  dis: string;
begin
  Result := True;
  if Pos('setting: ', line) = 1 then
  begin
    // check disable
    dis := '';
    GetStringParameter(line, 'ban', dis);
    if (dis = '*') or (Pos(cbboxMode.Text[1], dis) > 0) then
    begin
      // current setting is disabled in current mode
      line := line + ', disabled = 1';
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
  setting.mode   := '';
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

function TFormMain.VSProg_SupportParserCallback(line: string): boolean;
var
  chip_name, chip_module_header: string;
  setting: TTargetSetting;
begin
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

    GetIntegerParameter(line, 'fuse_bytelen', setting.fuse_bytelen);
    GetIntegerParameter(line, 'fuse_default', setting.fuse_default);
    GetIntegerParameter(line, 'lock_bytelen', setting.lock_bytelen);
    GetIntegerParameter(line, 'lock_default', setting.lock_default);
    GetIntegerParameter(line, 'cali_bytelen', setting.cali_bytelen);
    GetIntegerParameter(line, 'cali_default', setting.cali_default);
    GetIntegerParameter(line, 'usrsig_bytelen', setting.usrsig_bytelen);
    GetIntegerParameter(line, 'usrsig_default', setting.usrsig_default);

    cbboxTarget.Items.Add(chip_name);
    VSProg_AddTargetSetting(setting);
  end;

  Result := True;
end;

procedure TFormMain.VSProg_CommonUpdateSetting(Value, bytelen: integer;
  edt: TLabeledEdit);
begin
  if bytelen > 0 then
  begin
    edt.Text := '0x' + IntToHex(Value, bytelen * 2);
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
  chkboxApp.Caption  := 'Flash';
  if Pos(FLASH_CHAR, para) > 0 then
  begin
    btnEditApp.Enabled := True;
    chkboxApp.Enabled  := True;
    chkboxApp.Checked  := True;
  end
  else
  begin
    btnEditApp.Enabled := False;
    chkboxApp.Enabled  := False;
    chkboxApp.Checked  := False;
  end;

  btnEditEE.Caption := 'EE';
  chkboxEE.Caption  := 'EE';
  if Pos(EEPROM_CHAR, para) > 0 then
  begin
    btnEditEE.Enabled := True;
    chkboxEE.Enabled  := True;
    chkboxEE.Checked  := True;
  end
  else
  begin
    btnEditEE.Enabled := False;
    chkboxEE.Enabled  := False;
    chkboxEE.Checked  := False;
  end;

  btnEditLock.Caption := 'Lock';
  chkboxLock.Caption  := 'Lock';
  lbledtLock.Text     := '';
  if Pos(LOCK_CHAR, para) > 0 then
  begin
    btnEditLock.Enabled := True;
    lbledtLock.Enabled  := True;
    chkboxLock.Enabled  := True;
    chkboxLock.Checked  := True;
  end
  else
  begin
    btnEditLock.Enabled := False;
    lbledtLock.Enabled  := False;
    chkboxLock.Enabled  := False;
    chkboxLock.Checked  := False;
  end;

  btnEditFuse.Caption := 'Fuse';
  chkboxFuse.Caption  := 'Fuse';
  lbledtFuse.Text     := '';
  if Pos(FUSE_CHAR, para) > 0 then
  begin
    btnEditFuse.Enabled := True;
    lbledtFuse.Enabled  := True;
    chkboxFuse.Enabled  := True;
    chkboxFuse.Checked  := True;
  end
  else
  begin
    btnEditFuse.Enabled := False;
    lbledtFuse.Enabled  := False;
    chkboxFuse.Enabled  := False;
    chkboxFuse.Checked  := False;
  end;

  btnEditUsrSig.Caption := 'UsrSig';
  chkboxUsrSig.Caption  := 'UsrSig';
  if Pos(USRSIG_CHAR, para) > 0 then
  begin
    btnEditUsrSig.Enabled := True;
    lbledtUsrSig.Enabled  := True;
    chkboxUsrSig.Enabled  := True;
    chkboxUsrSig.Checked  := True;
  end
  else
  begin
    btnEditUsrSig.Enabled := False;
    lbledtUsrSig.Enabled  := False;
    chkboxUsrSig.Enabled  := False;
    chkboxUsrSig.Checked  := False;
  end;

  btnEditCali.Caption := 'Cali.';
  chkboxCali.Caption  := 'Cali.';
  if Pos(CALI_CHAR, para) > 0 then
  begin
    btnEditCali.Enabled := True;
    lbledtCali.Enabled  := True;
    chkboxCali.Enabled  := True;
    chkboxCali.Checked  := True;
  end
  else
  begin
    btnEditCali.Enabled := False;
    lbledtCali.Enabled  := False;
    chkboxCali.Enabled  := False;
    chkboxCali.Checked  := False;
  end;
end;

procedure TFormMain.VSProg_CommonInit(para: string);
var
  str_tmp: string;
begin
  VSProg_CommonTargetInit(para);

  chkboxMP.Checked := False;
  if Pos('M', para) > 0 then
  begin
    chkboxMP.Enabled := True;
  end
  else
  begin
    chkboxMP.Enabled := False;
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
    lbledtFreq.Enabled := True;
  end
  else
  begin
    lbledtFreq.Text    := '';
    lbledtFreq.Hint    := '';
    lbledtFreq.Enabled := False;
  end;

  if (Pos('A', para) > 0) or (Pos('C', para) > 0) then
  begin
    if Pos('C', para) > 0 then
    begin
      btnTargetDetect.Caption := COMSETUP_STR;
      btnTargetDetect.Hint    := COMSETUP_HINT;
    end
    else
    begin
      btnTargetDetect.Caption := AUTODETECT_STR;
      btnTargetDetect.Hint    := AUTODETECT_HINT;
    end;
    btnTargetDetect.Enabled := True;
  end
  else
  begin
    btnTargetDetect.Caption := 'No use';
    btnTargetDetect.Hint    := 'No use';
    btnTargetDetect.Enabled := False;
  end;

  chkboxEraseBeforeWrite.Caption := 'Erase before write';
  chkboxEraseBeforeWrite.Enabled := True;
  chkboxVerifyAfterWrite.Caption := 'Verify after write';
  chkboxVerifyAfterWrite.Enabled := True;

  cbboxMode.Clear;
  cbboxMode.Enabled := False;
end;

{ PSoC1 implementation }

procedure TFormMain.PSoC1_Update_Chip(setting: TTargetSetting);
var
  str_tmp:  string;
  mode_str: string;
begin
  mode_str := setting.mode;
  str_tmp  := cbboxMode.Text;
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
  GetStringParameter(line, 'init_mode', setting.mode);
  setting.target := FLASH_CHAR + LOCK_CHAR;
  Result := True;
end;

function TFormMain.PSoC1_Init(): boolean;
var
  setting: TTargetSetting;
begin
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('psoc1');

  VSProg_TargetSettingInit(setting);
  setting.mode   := 'rp';
  setting.target := FLASH_CHAR + LOCK_CHAR;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('ALL', False, 0, 0, 0);

  // call 'vsprog -Spsoc1' to extract supported psoc1 targets
  VSProg_Caller.AddParameter('Spsoc1');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsPSoC1.Enabled := False;
    Result := False;
    exit;
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

{ C8051F implementations }
function TFormMain.C8051F_Init(): boolean;
var
  setting: TTargetSetting;
begin
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('c8051f');

  VSProg_TargetSettingInit(setting);
  setting.mode   := 'jc';
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('ALL', False, 0, 0, 0);

  // call 'vsprog -Sc8051f' to check support
  VSProg_Caller.AddParameter('Sc8051f');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsC8051F.Enabled := False;
    Result := False;
    exit;
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
  GetStringParameter(line, 'prog_mode', setting.mode);
  setting.target := FLASH_CHAR;
  Result := True;
end;

{ AT89S5X implementations }
procedure TFormMain.AT89S5X_Update_Chip(setting: TTargetSetting);
var
  mode_str, str_tmp: string;
begin
  mode_str := setting.mode;
  str_tmp  := cbboxMode.Text;
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
  Result := True;
end;

function TFormMain.AT89S5X_Init(): boolean;
var
  setting: TTargetSetting;
begin
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('at89s5x');

  VSProg_TargetSettingInit(setting);
  setting.mode   := 'pb';
  setting.target := FLASH_CHAR + LOCK_CHAR;
  setting.lock_bytelen := 1;
  setting.lock_default := 1;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('ALL', False, 0, 0, 0);

  // call 'vsprog -Sat89s5x' to extract supported at89s5x targets
  VSProg_Caller.AddParameter('Sat89s5x');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsAT89S5X.Enabled := False;
    Result := False;
    exit;
  end;

  // Frequency, Autodetect
  VSProg_CommonInit('FA');
end;

{ MSP430 implementations }
function TFormMain.MSP430_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  GetStringParameter(line, 'prog_mode', setting.mode);
  setting.target := FLASH_CHAR;
  Result := True;
end;

function TFormMain.MSP430_Init(): boolean;
var
  setting: TTargetSetting;
begin
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('msp430');

  VSProg_TargetSettingInit(setting);
  setting.mode   := 'jsb';
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('ALL', False, 0, 0, 0);

  // call 'vsprog -Smsp430' to extract supported at89s5x targets
  VSProg_Caller.AddParameter('Smsp430');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsMSP430.Enabled := False;
    Result := False;
    exit;
  end;

  // flash, Autodetect
  VSProg_CommonInit('A');
end;

procedure TFormMain.MSP430_Update_Chip(setting: TTargetSetting);
var
  str_tmp:  string;
  mode_str: string;
begin
  mode_str := setting.mode;
  str_tmp  := cbboxMode.Text;
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
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('stm8');

  VSProg_TargetSettingInit(setting);
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('ALL', False, 0, 0, 0);

  // call 'vsprog -Sstm8' to extract supported at89s5x targets
  VSProg_Caller.AddParameter('Sstm8');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsSTM8.Enabled := False;
    Result := False;
    exit;
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

{ EEPROM implementations }
function TFormMain.EEPROM_Init(): boolean;
begin
  Result := True;

  cbboxTarget.Clear;
end;

{ ARM functions }
function TFormMain.ARM_Init(): boolean;
var
  SearchResult: TSearchRec;
  index: integer;
begin
  if (cbboxOpenOCDInterface.Items.Count = 0) and
    DirectoryExists(Application.Location + 'interface') then
  begin
    if FindFirst(Application.Location + 'interface' + System.DirectorySeparator +
      '*', (faAnyFile and not faDirectory), SearchResult) = 0 then
    begin
      if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
      begin
        cbboxOpenOCDInterface.Items.Add('interface' + System.DirectorySeparator +
          SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
        begin
          cbboxOpenOCDInterface.Items.Add('interface' + System.DirectorySeparator +
            SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDInterface.Items.Count > 0 then
    begin
      index := cbboxOpenOCDInterface.Items.IndexOf('interface' +
        System.DirectorySeparator + 'vsllink.cfg');
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

  if (cbboxOpenOCDTarget.Items.Count = 0) and
    DirectoryExists(Application.Location + 'target') then
  begin
    if FindFirst(Application.Location + 'target' + System.DirectorySeparator +
      '*', (faAnyFile and not faDirectory), SearchResult) = 0 then
    begin
      if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
      begin
        cbboxOpenOCDTarget.Items.Add('target' + System.DirectorySeparator +
          SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
        begin
          cbboxOpenOCDTarget.Items.Add('target' + System.DirectorySeparator +
            SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDTarget.Items.Count > 0 then
    begin
      cbboxOpenOCDTarget.ItemIndex := 0;
    end;
  end;

  if (cbboxOpenOCDScript.Items.Count = 0) and
    DirectoryExists(Application.Location + 'script') then
  begin
    if FindFirst(Application.Location + 'script' + System.DirectorySeparator +
      '*', (faAnyFile and not faDirectory), SearchResult) = 0 then
    begin
      if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
      begin
        cbboxOpenOCDScript.Items.Add('script' + System.DirectorySeparator +
          SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
        begin
          cbboxOpenOCDScript.Items.Add('script' + System.DirectorySeparator +
            SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDScript.Items.Count > 0 then
    begin
      cbboxOpenOCDScript.ItemIndex := 0;
    end;
  end;

  Result := True;
end;

{ AVR8 implementations }
function TFormMain.AVR8_Init(): boolean;
var
  setting: TTargetSetting;
begin
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('avr8');

  VSProg_TargetSettingInit(setting);
  setting.mode   := 'ijps';
  setting.target := FLASH_CHAR + FUSE_CHAR + LOCK_CHAR + CALI_CHAR + EEPROM_CHAR;
  setting.fuse_bytelen := 3;
  setting.fuse_default := $FFFFFF;
  setting.lock_bytelen := 1;
  setting.lock_default := $FF;
  setting.cali_bytelen := 4;
  setting.cali_default := $FFFFFFFF;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('Flash', False, 0, 0, 0);
  AddTargetFile('EEPROM', True, 2, 0, 0);

  // call 'vsprog -Savr8' to extract supported avr8 targets
  VSProg_Caller.AddParameter('Savr8');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsAVR8.Enabled := False;
    Result := False;
    exit;
  end;

  // Autodetect, Frequency
  VSProg_CommonInit('AF');
end;

function TFormMain.AVR8_Init_Para(line: string; var setting: TTargetSetting): boolean;
var
  size: integer;
begin
  GetStringParameter(line, 'prog_mode', setting.mode);
  size := 0;
  GetIntegerParameter(line, 'eeprom_size', size);
  if size > 0 then
  begin
    setting.target := FLASH_CHAR + FUSE_CHAR + LOCK_CHAR + CALI_CHAR + EEPROM_CHAR;
  end
  else
  begin
    setting.target := FLASH_CHAR + FUSE_CHAR + LOCK_CHAR + CALI_CHAR;
  end;
  Result := True;
end;

procedure TFormMain.AVR8_Update_Chip(setting: TTargetSetting);
var
  str_tmp, mode_str: string;
begin
  mode_str := setting.mode;
  str_tmp  := cbboxMode.Text;
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
    AddTargetFile('Flash', False, 0, 0, 0);
  end
  else
  begin
    RemoveTargetFile('Flash');
  end;
  if Pos(EEPROM_CHAR, str_tmp) > 0 then
  begin
    AddTargetFile('EEPROM', True, 2, 0, 0);
  end
  else
  begin
    RemoveTargetFile('EEPROM');
  end;

  // Fuse, Lock and Cali are not enabled by default
  chkboxEE.Checked   := False;
  chkboxFuse.Checked := False;
  chkboxLock.Checked := False;
  chkboxCali.Checked := False;
end;

procedure TFormMain.AVR8_Update_Mode(m_str: string);
begin
  if m_str = 'i:ISP' then
  begin
    lbledtFreq.Hint    := FREQ_HINT;
    lbledtFreq.Enabled := True;
  end
  else
  begin
    lbledtFreq.Enabled := False;
  end;
end;

{ COMISP declarations }
function TFormMain.COMISP_Init(): boolean;
begin
  Result := True;

  cbboxTarget.Clear;

  // call 'vsprog -Scomisp' to extract supported comisp targets
  VSProg_Caller.AddParameter('Scomisp');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsCOMISP.Enabled := False;
    Result := False;
    exit;
  end;

  VSProg_CommonInit('CX');
end;

function TFormMain.COMISP_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  setting.extra  := line;
  setting.target := FLASH_CHAR;

  Result := True;
end;

procedure TFormMain.COMISP_Update_Chip(setting: TTargetSetting);
var
  str_tmp:     string;
  setting_str: string;
begin
  str_tmp := '';

  if cbboxTarget.Text = 'comisp_stm32' then
  begin
    // STM32
    AddTargetFile('Flash', False, 0, 0, $08000000);
  end
  else
  begin
    AddTargetFile('Flash', False, 0, 0, 0);
  end;

  setting_str := setting.extra;
  GetIntegerParameter(setting_str, 'baudrate', ComModeInit.baudrate);
  GetIntegerParameter(setting_str, 'datalength', ComModeInit.datalength);
  GetStringParameter(setting_str, 'paritybit', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.paritybit := str_tmp[1];
  end;
  GetStringParameter(setting_str, 'stopbit', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.stopbit := str_tmp[1];
  end;
  GetStringParameter(setting_str, 'handshake', str_tmp);
  if str_tmp <> '' then
  begin
    ComModeInit.handshake := str_tmp[1];
  end;
  GetStringParameter(setting_str, 'auxpin', str_tmp);
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
  Result := True;

  cbboxTarget.Clear;
  cbboxTarget.Items.Add('lpc900');

  VSProg_TargetSettingInit(setting);
  setting.target := FLASH_CHAR;
  VSProg_AddTargetSetting(setting);
  AddTargetFile('ALL', False, 0, 0, 0);

  // call 'vsprog -Scomisp' to extract supported comisp targets
  VSProg_Caller.AddParameter('Slpc900');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsLPCICP.Enabled := False;
    Result := False;
    exit;
  end;

  // Autodetect
  VSProg_CommonInit('A');
end;

function TFormMain.LPCICP_Init_Para(line: string; var setting: TTargetSetting): boolean;
begin
  line := line;

  setting.target := FLASH_CHAR;
  Result := True;
end;

procedure TFormMain.LPCICP_Update_Chip(setting: TTargetSetting);
begin
  setting := setting;
end;

{ CortexM3 implementations }
function TFormMain.CortexM3_Init(): boolean;
begin
  Result := True;

  cbboxTarget.Clear;

  // call 'vsprog -Scm3' to extract supported comisp targets
  VSProg_Caller.AddParameter('Scm3');
  VSProg_Parser.CallbackFunc    := @VSProg_SupportParserCallback;
  VSProg_Parser.LogOutputEnable := False;
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SupportParser, 0) then
  begin
    cbboxTarget.ItemIndex := 0;
  end
  else
  begin
    tsCortexM3.Enabled := False;
    Result := False;
    exit;
  end;

  VSProg_CommonInit('F');
end;

function TFormMain.CortexM3_Init_Para(line: string;
  var setting: TTargetSetting): boolean;
begin
  line := line;

  setting.mode := 'js';
  setting.target := FLASH_CHAR;
  Result := True;
end;

procedure TFormMain.CortexM3_Update_Chip(setting: TTargetSetting);
var
  str_tmp: string;
begin
  str_tmp := cbboxMode.Text;
  cbboxMode.Clear;

  if cbboxTarget.Text = 'cm3_stm32' then
  begin
    // STM32
    AddTargetFile('Flash', False, 0, 0, $08000000);
  end
  else
  begin
    AddTargetFile('Flash', False, 0, 0, 0);
  end;

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
    lbledtFreq.Hint    := FREQ_HINT;
    lbledtFreq.Enabled := True;
  end
  else
  begin
    lbledtFreq.Enabled := False;
  end;
end;

initialization
  {$I main.lrs}

end.


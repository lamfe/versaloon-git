unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ExtCtrls, cli_caller, parameditor, Menus, Buttons, Spin,
  Synaser, com_setup, fileselector, hexeditor, XMLCfg, vsprogparser, vsprogtarget,
  vsprogprogrammer;

type

  { TPollThread }

  TPollThread = class(TThread)
  private
    FTargetVoltage: integer;
    FConnectOK: boolean;
    FAppPath: string;
    procedure Update;
  protected
    procedure Execute; override;
  public
    constructor Create();
    property AppPath: string Read FAppPath Write FAppPath;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    btnEditFuse: TButton;
    btnEditEE: TButton;
    btnEditUsrSig: TButton;
    btnOpenOCDRun: TButton;
    btnOpenOCDStop: TButton;
    btnUpdate: TButton;
    btnSVFRun: TButton;
    btnEditCali: TButton;
    btnOpenFile: TButton;
    btnSetPower: TButton;
    btnModeSetup: TButton;
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
    dedtOpenOCD: TDirectoryEdit;
    dedtVSprog: TDirectoryEdit;
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
    gbSetting: TGroupBox;
    lblKHz:    TLabel;
    lblOpenOCDDir: TLabel;
    lblVSProgDir: TLabel;
    lblPowerUnit: TLabel;
    lbledtCali: TLabeledEdit;
    lbledtUsrSig: TLabeledEdit;
    lblSVFOption: TLabel;
    lblSVFFile: TLabel;
    lblOpenOCDOption: TLabel;
    lblOpenOCDInterface: TLabel;
    lblOpenOCDTarget: TLabel;
    lblOpenOCDScript: TLabel;
    lbledtAddr: TLabeledEdit;
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
    pnlAbout:  TPanel;
    pgbarMain: TProgressBar;
    pnlMain:   TPanel;
    pcMain:    TPageControl;
    pmTray:    TPopupMenu;
    sbMain:    TStatusBar;
    sedtPower: TSpinEdit;
    sedtFreq:  TSpinEdit;
    tInit: TTimer;
    tsVsprog:  TTabSheet;
    tsJTAG:    TTabSheet;
    tiMain:    TTrayIcon;
    tsAbout:   TTabSheet;
    xmlcfgMain: TXMLConfig;
    procedure btnEditAppClick(Sender: TObject);
    procedure btnEditCaliClick(Sender: TObject);
    procedure btnEditEEClick(Sender: TObject);
    procedure btnEditFuseClick(Sender: TObject);
    procedure btnEditLockClick(Sender: TObject);
    procedure btnEditUsrSigClick(Sender: TObject);
    procedure btnEraseClick(Sender: TObject);
    procedure btnModeSetupClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenOCDRunClick(Sender: TObject);
    procedure btnOpenOCDStopClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnSetPowerClick(Sender: TObject);
    procedure btnSVFRunClick(Sender: TObject);
    procedure btnTargetDetectClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure cbboxInputFileChange(Sender: TObject);
    procedure cbboxModeChange(Sender: TObject);
    procedure cbboxTargetChange(Sender: TObject);
    procedure dedtPathChange(Sender: TObject);
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
    procedure sedtFreqEditingDone(Sender: TObject);
    procedure tiMainClick(Sender: TObject);
    procedure tInitTimer(Sender: TObject);

    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure AdjustComponentColor(Sender: TControl);
    procedure ShowDebugLog();
    procedure HideDebugLog();
    procedure ComboBoxSetText(var combo: TComboBox; aText: string);
    function ComboBoxGetIdx(combo: TComboBox; aText: string): integer;
    procedure UpdateTitle();
    procedure LogInfo(info: string);
    procedure UpdateComboTargetFile();

    { VSProg declarations }

    procedure VSProg_GUIUpdateULCS(Value: QWord; bytelen: integer; var edt: TLabeledEdit);
    procedure VSProg_GUIUpdateFuse(fuse: QWord; bytelen: integer);
    procedure VSProg_GUIUpdateLock(lock: QWord; bytelen: integer);
    procedure VSProg_GUIUpdateUsrSig(sig: QWord; bytelen: integer);
    procedure VSProg_GUIUpdateCali(cali: QWord; bytelen: integer);
    procedure ShowTargetArea(AreaName: char; var Sender: TObject; parser: TParserFunc);
    function VSProg_PrepareToRun(aApplicationName: string): boolean;
    function VSProg_PrepareToRunCLI: boolean;
    function VSProg_PrepareToRunOpenOCD: boolean;
    function VSProg_RunAlgorithm(var caller: TCLI_Caller; parser: TParserFunc;
      result_num: integer; silent: boolean): boolean;
    function VSProg_SettingTargetParserCallback(var line: string): boolean;
    procedure VSProg_LogOutput(var line: string);
    procedure VSProg_LogProgress(ProgressInfo: TProgressInfoType; info: string);
    function VSProg_GUITargetSeriesPageInit(index: integer): boolean;
    procedure VSProg_GUITargetAreaInit;
    procedure VSProg_GUIModeInit;
    procedure VSProg_GUIFeatureInit(para: string; append: boolean);
    function VSProg_GetEnabledOperationString(): string;
    function VSProg_GetTargetDefineParameters(): string;
    function VSProg_AddEraseOperation(): boolean;
    function VSProg_AddWriteOperation(): boolean;
    function VSProg_AddVerifyOperation(): boolean;
    function VSProg_AddReadOperation(): boolean;
    procedure VSProg_PrepareMiscParameters(var caller: TCLI_Caller);
    procedure VSProg_PrepareBaseParameters(var caller: TCLI_Caller);
    procedure VSProg_PrepareOperationParameters(var caller: TCLI_Caller);
    { OpenOCD declarations }
    function OpenOCD_Init(): boolean;
  private
    { private declarations }
    bReadOperation: boolean;
    bOpenFileOK: boolean;
    JTAGPage_Init: boolean;
    TargetPage_Init: boolean;
    AboutPage_Init: boolean;
    Page_Init: boolean;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  PollThread: TPollThread;
  VSProg_Taken_By_Polling: boolean;
  VSProg_Version: string;
  VSProg_Caller: TCLI_Caller;
  VSProg_Parser: TVSprog_Parser;
  VSProg_Targets: TVSProg_Targets;
  VSProg_Exists: boolean;
  OpenOCD_Exists: boolean;
  CurTargetChip: TTargetChip;
  CurTargetSeries: TTargetSeries;
  ComMode: TComMode;
  CurProgrammerInfo: string;
  ProgrammerParameter: string;
  PreviousPage: string;

const
  DEBUG_LOG_SHOW: boolean = False;
  DISPLAY_ALL_COMPORT_WHEN_UPDATE = True;
  APP_STR: string     = 'Vsgui';
  VERSION_STR: string = 'RC1';
  {$IFDEF UNIX}
  VSPROG_STR: string  = 'vsprog';
  {$ELSE}
  VSPROG_STR: string  = 'vsprog.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  OPENOCD_STR: string = 'openocd';
  {$ELSE}
  OPENOCD_STR: string = 'openocd.exe';
  {$ENDIF}
  LOGMEMO_WIDTH: integer = 400;

implementation

{ TPollThread }

procedure TPollThread.Update;
begin
  if FConnectOK then
  begin
    CurProgrammerInfo := FormatFloat('0.0', FTargetVoltage / 1000) + 'V';
  end
  else
  begin
    CurProgrammerInfo := 'N.C.';
  end;
  FormMain.UpdateTitle();
end;

procedure TPollThread.Execute;
var
  i: integer;
begin
  i := 0;
  while not Terminated do
  begin
    Sleep(50);
    Inc(i);
    if i < 20 then
    begin
      continue;
    end;
    i := 0;

    if not VSProg_Caller.Take() then
    begin
      // not available now
      continue;
    end;
    VSProg_Taken_By_Polling := True;

    FTargetVoltage := 0;
    VSProg_Caller.Application := FAppPath;
    VSProg_Caller.RemoveAllParameters();
    if ProgrammerParameter <> '' then
    begin
      VSProg_Caller.AddParameter('U' + ProgrammerParameter);
    end;
    VSProg_Caller.AddParameter('V"voltage"');
    VSProg_Parser.Prepare();
    VSProg_Parser.ParserFunc      := @VSProg_Parser.TargetVoltageParser;
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Caller.bProcessMessage := False;
    VSProg_Caller.Run(@VSProg_Parser.CommonParser, False, True);
    VSProg_Caller.bProcessMessage := True;
    VSProg_Parser.LogOutputEnable := True;
    if (not VSProg_Parser.HasError) and (VSProg_Parser.ResultStrings.Count > 0) then
    begin
      FConnectOK     := True;
      FTargetVoltage := StrToInt(VSProg_Parser.ResultStrings.Strings[0]);
    end
    else
    begin
      FConnectOK     := False;
      FTargetVoltage := 0;
    end;

    if FConnectOK and (ProgrammerParameter = '') then
    begin
      if not VSProg_Caller.Take() then
      begin
        // not available now
        continue;
      end;
      VSProg_Caller.Application := FAppPath;
      VSProg_Caller.RemoveAllParameters();
      VSProg_Caller.AddParameter('L');
      VSProg_Parser.Prepare();
      VSProg_Parser.ParserFunc := @VSProg_Programmer.ProgrammerParser;
      VSProg_Parser.LogOutputEnable := False;
      VSProg_Caller.bProcessMessage := False;
      VSProg_Caller.Run(@VSProg_Parser.CommonParser, False, True);
      if not VSProg_Parser.HasError then
      begin
        ProgrammerParameter := VSProg_Programmer.SerialNumber;
      end;
    end;

    Synchronize(@Update);
    VSProg_Taken_By_Polling := False;
  end;
end;

constructor TPollThread.Create();
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

{ TFormMain }

function TFormMain.ComboBoxGetIdx(combo: TComboBox; aText: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to combo.Items.Count - 1 do
  begin
    if combo.Items.Strings[i] = aText then
    begin
      Result := i;
    end;
  end;
end;

procedure TFormMain.ComboBoxSetText(var combo: TComboBox; aText: string);
var
  i: integer;
begin
  i := ComboBoxGetIdx(combo, aText);
  if i >= 0 then
  begin
    combo.ItemIndex := i;
  end;
end;

procedure TFormMain.tInitTimer(Sender: TObject);
var
  ControlLeft: integer;
begin
  tInit.Enabled := False;
  ControlLeft      := (pcMain.ActivePage.Width - gbOpenOCD.Width) div 2;
  gbOpenOCD.Left   := ControlLeft;
  gbSVFPlayer.Left := ControlLeft;
  gbPower.Left     := ControlLeft;
  gbChipName.Left  := ControlLeft;
  gbInputFile.Left := ControlLeft;
  gbOption.Left    := ControlLeft;
  gbOperation.Left := ControlLeft;
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
    enable_str + ' (' + VSProg_Version + ') ' + CurProgrammerInfo;
end;

function TFormMain.VSProg_RunAlgorithm(var caller: TCLI_Caller;
  parser: TParserFunc; result_num: integer; silent: boolean): boolean;
begin
  Result := False;
  VSProg_Parser.Prepare();
  VSProg_Parser.ParserFunc := parser;
  caller.Run(@VSProg_Parser.CommonParser, False, True);
  VSProg_Parser.CallbackFunc    := nil;
  VSProg_Parser.LogOutputEnable := True;
  VSProg_Caller.Application     := '';

  if (VSProg_Parser.HasError) and (not silent) then
  begin
    Beep();
    MessageDlg('Error', VSProg_Parser.ErrorStr, mtError, [mbOK], 0);
  end
  else if VSProg_Parser.ResultStrings.Count >= result_num then
  begin
    Result := True;
  end;
end;

procedure TFormMain.VSProg_LogOutput(var line: string);
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

procedure TFormMain.VSProg_GUIUpdateULCS(Value: QWord; bytelen: integer;
  var edt: TLabeledEdit);
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

procedure TFormMain.VSProg_GUIUpdateUsrSig(sig: QWord; bytelen: integer);
begin
  VSProg_GUIUpdateULCS(sig, bytelen, lbledtUsrSig);
end;

procedure TFormMain.VSProg_GUIUpdateCali(cali: QWord; bytelen: integer);
begin
  VSProg_GUIUpdateULCS(cali, bytelen, lbledtCali);
end;

procedure TFormMain.VSProg_GUIUpdateLock(lock: QWord; bytelen: integer);
begin
  VSProg_GUIUpdateULCS(lock, bytelen, lbledtLock);
end;

procedure TFormMain.VSProg_GUIUpdateFuse(fuse: QWord; bytelen: integer);
begin
  VSProg_GUIUpdateULCS(fuse, bytelen, lbledtFuse);
end;

 // Update Series Features
 // C: Commport, X: eXecute, F, Frequency
procedure TFormMain.VSProg_GUIFeatureInit(para: string; append: boolean);
var
  strTmp: string;
begin
  chkboxMP.Checked := False;
  chkboxMP.Enabled := (Pos('M', para) > 0);

  if Pos('F', para) > 0 then
  begin
    sedtFreq.Visible     := True;
    btnModeSetup.Visible := False;
  end
  else
  begin
    sedtFreq.Visible := False;
  end;

  if Pos('C', para) > 0 then
  begin
    sedtFreq.Visible     := False;
    btnModeSetup.Visible := True;

    // Set COMM Settings
    strTmp := CurTargetChip.ExtraStr;
    if strTmp <> '' then
    begin
      FormComSetup.ComInitPara(strTmp);
    end
    else
    begin
      strTmp := CurTargetSeries.SeriesInfo;
      if strTmp <> '' then
      begin
        FormComSetup.ComInitPara(strTmp);
      end;
    end;
  end
  else
  begin
    btnModeSetup.Visible := False;
  end;

  lblKHz.Visible := sedtFreq.Visible;

  if Pos('X', para) > 0 then
  begin
    lbledtAddr.Enabled := True;
  end
  else if not append then
  begin
    lbledtAddr.Enabled := False;
    lbledtAddr.Text := '';
  end;

  if Pos('A', para) > 0 then
  begin
    btnTargetDetect.Enabled := True;
  end
  else if not append then
  begin
    btnTargetDetect.Enabled := False;
  end;
end;

// Update Mode and correspong features
procedure TFormMain.VSProg_GUIModeInit;
var
  OrigMode: string;
  i: integer;
begin
  OrigMode := cbboxMode.Text;
  cbboxMode.Clear;

  for i := 1 to Length(CurTargetChip.Mode) do
  begin
    cbboxMode.Items.Add(CurTargetSeries.GetModeStrByModeChar(CurTargetChip.Mode[i]));
  end;

  if cbboxMode.Items.Count > 0 then
  begin
    i := ComboBoxGetIdx(cbboxMode, OrigMode);
    if i >= 0 then
    begin
      cbboxMode.ItemIndex := i;
    end
    else
    begin
      cbboxMode.ItemIndex := 0;
    end;
    cbboxMode.Enabled := True;
    cbboxModeChange(cbboxMode);
  end
  else
  begin
    cbboxMode.Enabled := False;
  end;
end;

// f: Flash, e: EEprom, l: Lock, u: Fuse, s: User Signature, c: Calibration Value
procedure TFormMain.VSProg_GUITargetAreaInit;
var
  index: integer;
  para:  string;
begin
  para := CurTargetChip.Areas;

  if Pos(FLASH_CHAR, para) > 0 then
  begin
    btnEditApp.Enabled := True;
    chkboxApp.Enabled  := True;
    //chkboxApp.Checked  := True;
  end
  else
  begin
    btnEditApp.Enabled := False;
    chkboxApp.Enabled  := False;
    chkboxApp.Checked  := False;
  end;

  if Pos(EE_CHAR, para) > 0 then
  begin
    btnEditEE.Enabled := True;
    chkboxEE.Enabled  := True;
    //chkboxEE.Checked  := True;
  end
  else
  begin
    btnEditEE.Enabled := False;
    chkboxEE.Enabled  := False;
    chkboxEE.Checked  := False;
  end;

  lbledtLock.Text    := '';
  lbledtLock.Enabled := False;
  if Pos(LOCK_CHAR, para) > 0 then
  begin
    btnEditLock.Enabled := True;
    chkboxLock.Enabled := True;
    //chkboxLock.Checked := True;
    index := CurTargetChip.GetAreaIdx(LOCK_CHAR);
    if (index >= 0) and (not CurTargetChip.TargetAreas[index].InFile) and
      (CurTargetChip.TargetAreas[index].ByteLen <= 4) then
    begin
      lbledtLock.Enabled := True;
      VSProg_GUIUpdateLock(CurTargetChip.TargetAreas[index].DefaultValue,
        CurTargetChip.TargetAreas[index].ByteLen);
    end;
  end
  else
  begin
    btnEditLock.Enabled := False;
    chkboxLock.Enabled  := False;
    chkboxLock.Checked  := False;
  end;

  lbledtFuse.Text    := '';
  lbledtFuse.Enabled := False;
  if Pos(FUSE_CHAR, para) > 0 then
  begin
    btnEditFuse.Enabled := True;
    chkboxFuse.Enabled := True;
    //chkboxFuse.Checked := True;
    index := CurTargetChip.GetAreaIdx(FUSE_CHAR);
    if (index >= 0) and (not CurTargetChip.TargetAreas[index].InFile) and
      (CurTargetChip.TargetAreas[index].ByteLen <= 8) then
    begin
      lbledtFuse.Enabled := True;
      VSProg_GUIUpdateFuse(CurTargetChip.TargetAreas[index].DefaultValue,
        CurTargetChip.TargetAreas[index].ByteLen);
    end;
  end
  else
  begin
    btnEditFuse.Enabled := False;
    chkboxFuse.Enabled  := False;
    chkboxFuse.Checked  := False;
  end;

  lbledtUsrSig.Text    := '';
  lbledtUsrSig.Enabled := False;
  if Pos(USRSIG_CHAR, para) > 0 then
  begin
    btnEditUsrSig.Enabled := True;
    chkboxUsrSig.Enabled := True;
    //chkboxUsrSig.Checked := True;
    index := CurTargetChip.GetAreaIdx(USRSIG_CHAR);
    if (index >= 0) and (not CurTargetChip.TargetAreas[index].InFile) and
      (CurTargetChip.TargetAreas[index].ByteLen <= 8) then
    begin
      lbledtUsrSig.Enabled := True;
      VSProg_GUIUpdateUsrSig(CurTargetChip.TargetAreas[index].DefaultValue,
        CurTargetChip.TargetAreas[index].ByteLen);
    end;
  end
  else
  begin
    btnEditUsrSig.Enabled := False;
    chkboxUsrSig.Enabled  := False;
    chkboxUsrSig.Checked  := False;
  end;

  lbledtCali.Text    := '';
  lbledtCali.Enabled := False;
  if Pos(CALI_CHAR, para) > 0 then
  begin
    btnEditCali.Enabled := True;
    chkboxCali.Enabled := True;
    //chkboxCali.Checked := True;
    index := CurTargetChip.GetAreaIdx(CALI_CHAR);
    if (index >= 0) and (not CurTargetChip.TargetAreas[index].InFile) and
      (CurTargetChip.TargetAreas[index].ByteLen <= 4) then
    begin
      lbledtCali.Enabled := True;
      VSProg_GUIUpdateCali(CurTargetChip.TargetAreas[index].DefaultValue,
        CurTargetChip.TargetAreas[index].ByteLen);
    end;
  end
  else
  begin
    btnEditCali.Enabled := False;
    chkboxCali.Enabled  := False;
    chkboxCali.Checked  := False;
  end;
end;

function TFormMain.VSProg_GUITargetSeriesPageInit(index: integer): boolean;
var
  i: integer;
begin
  Result := True;

  VSProg_GUIFeatureInit(VSProg_Targets.TargetSeries[pcMain.ActivePage.Tag].Feature,
    False);

  cbboxTarget.Clear;
  for i := 0 to VSProg_Targets.TargetSeries[index].ChipCount - 1 do
  begin
    cbboxTarget.Items.Add(VSProg_Targets.TargetSeries[index].TargetChips[i].Name);
  end;
  cbboxTarget.ItemIndex := 0;

  gbChipName.Parent  := pcMain.ActivePage;
  gbInputFile.Parent := pcMain.ActivePage;
  gbOption.Parent    := pcMain.ActivePage;
  gbOperation.Parent := pcMain.ActivePage;

  cbboxTargetChange(cbboxTarget);
end;

procedure TFormMain.UpdateComboTargetFile();
var
  valid_file_num, i: integer;
begin
  valid_file_num := 0;
  cbboxInputFile.Clear;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].filename <> '' then
    begin
      cbboxInputFile.Items.Add(GetAreaFullName(TargetFile[i].target) +
        ':' + TargetFile[i].filename);
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
  i, j: integer;
  str_tmp: string;
  T:   TTabSheet;
  ser: TBlockSerial;
begin
  xmlcfgMain.FileName := GetUserDir + 'vsgui.xml';

  ProgrammerParameter := '';
  CurProgrammerInfo := 'N.C.';
  JTAGPage_Init   := True;
  TargetPage_Init := True;
  AboutPage_Init  := True;

  VSProg_Taken_By_Polling := False;
  bOpenFileOK    := False;
  bReadOperation := False;
  Page_Init      := False;

  // caller init
  VSProg_Caller := TCLI_Caller.Create();
  VSProg_Caller.Delimiter := '-';

  // parser init
  VSProg_Parser := TVSProg_Parser.Create;
  VSProg_Parser.LogProgressFunc := @VSProg_LogProgress;
  VSProg_Parser.LogOutputFunc := @VSProg_LogOutput;
  VSProg_Parser.CallbackFunc := nil;

  // target init
  VSProg_Targets := TVSProg_Targets.Create;

  // poll thread init
  PollThread := TPollThread.Create;
  if Assigned(PollThread.FatalException) then
  begin
    raise PollThread.FatalException;
  end;

  // TrayIcon
  tiMain.Icon      := Application.Icon;
  tiMain.PopUpMenu := pmTray;
  tiMain.Show;

  // locate executables(vsprog and openocd)
  dedtVSProg.Directory  := xmlcfgMain.GetValue('vsprog_dir', Application.Location);
  dedtOpenOCD.Directory := xmlcfgMain.GetValue('openocd_dir', Application.Location);
  dedtPathChange(dedtVSProg);
  dedtPathChange(dedtOpenOCD);
  if not VSProg_Exists then
  begin
    Beep();
    MessageDlg('Error, missing vsprog',
      'Opps, Where is my vsprog? I cannot work without her.', mtError, [mbOK], 0);
  end;

  // get VSProg_Version
  VSProg_Version := '';
  if not VSProg_PrepareToRunCLI then
  begin
    VSProg_Version := 'No Exists';
  end
  else
  begin
    VSProg_Caller.AddParametersString('--version');
    VSProg_Parser.LogOutputEnable := False;
    if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.VersionParser, 1, False) then
    begin
      VSProg_Version := VSProg_Parser.ResultStrings.Strings[0];
    end;
  end;
  UpdateTitle();

  // get VSProg_Targets
  if VSProg_PrepareToRunCLI then
  begin
    VSProg_Caller.AddParameter('Starget');
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Parser.CallbackFunc    := @VSProg_Targets.TargetParser;
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.TargetParser, 0, True);
  end;
  // Create Pages
  j := 1;
  for i := 0 to VSProg_Targets.SeriesCount - 1 do
  begin
    if Pos('svf', VSProg_Targets.TargetSeries[i].Name) = 0 then
    begin
      T := TTabSheet.Create(pcMain);
      with T do
      begin
        T.Tag     := i;
        T.Caption := UpperCase(VSProg_Targets.TargetSeries[i].Name);
        T.Visible := True;
        T.TabVisible := True;
        T.PageControl := pcMain;
        pcMain.PageList.Move(j + 1, j);
        T.PageIndex := j;
        Inc(j);
      end;
    end;
  end;
  Page_Init := True;

  // Init COM
  cbboxCOM.Clear;
  for i := low(COMPORTS) to high(COMPORTS) do
  begin
    if DISPLAY_ALL_COMPORT_WHEN_UPDATE then
    begin
      cbboxCOM.Items.Add(COMPORTS[i]);
    end
    else
    begin
      ser := TBlockSerial.Create;
      try
        ser.Connect(COMPORTS[i]);
        if ser.LastError = 0 then
        begin
          cbboxCOM.Items.Add(COMPORTS[i]);
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

  // load last settings
  str_tmp := xmlcfgMain.GetValue('activepage', 'About');
  for i := 0 to pcMain.PageCount - 1 do
  begin
    if pcMain.Page[i].Caption = str_tmp then
    begin
      pcMain.ActivePageIndex := i;
      break;
    end;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  VSProg_Caller.Stop();
  VSProg_Caller.Destroy;
  VSProg_Parser.Destroy;
  VSProg_Targets.Destroy;
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

  // adjust size and position
  CenterControl(lblTarget, cbboxTarget);
  CenterControl(btnTargetDetect, cbboxTarget);
  CenterControl(lblInputFile, cbboxInputFile);
  CenterControl(btnOpenFile, cbboxInputFile);
  CenterControl(lblMode, cbboxMode);
  CenterControl(btnModeSetup, cbboxMode);
  CenterControl(sedtFreq, cbboxMode);
  CenterControl(chkboxNoconnect, lbledtFuse);
  CenterControl(chkboxNowarning, lbledtCali);
  CenterControl(lblOpenOCDInterface, cbboxOpenOCDInterface);
  CenterControl(lblOpenOCDTarget, cbboxOpenOCDTarget);
  CenterControl(lblOpenOCDScript, cbboxOpenOCDScript);
  CenterControl(lblOpenOCDOption, edtOpenOCDOption);
  CenterControl(lblSVFFile, fneditSVFFile);
  CenterControl(lblSVFOption, edtSVFOption);
  CenterControl(btnSVFRun, edtSVFOption);
  CenterControl(btnSetPower, sedtPower);
  CenterControl(fnFW, cbboxCOM);
  CenterControl(btnUpdate, cbboxCOM);
  CenterControl(lblVSProgDir, dedtVSProg);
  CenterControl(lblOpenOCDDir, dedtOpenOCD);

  FormMain.Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
  memoLog.Width  := LOGMEMO_WIDTH;
  UpdateShowing;

  // get existing programmer
  if VSProg_PrepareToRunCLI then
  begin
    VSProg_Caller.AddParameter('L');
    VSProg_Parser.LogOutputEnable := False;
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Programmer.ProgrammerParser, 0, True);
  end;
  if VSProg_Programmer.ProgrammerCount > 1 then
  begin
    VSProg_Programmer.ShowModal;
  end;
  ProgrammerParameter := VSProg_Programmer.SerialNumber;

  if VSProg_Exists and (PollThread <> nil) then
  begin
    PollThread.AppPath := dedtVSProg.Directory + VSPROG_STR;
    PollThread.Resume;
  end;

  // Load Setting
  JTAGPage_Init   := False;
  TargetPage_Init := False;
  AboutPage_Init  := False;
  pcMainPageChanged(pcMain);
end;

procedure TFormMain.lbledtExtraParaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Pos('show debug log', lbledtExtraPara.Text) = 1 then
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

procedure TFormMain.pcMainChanging(Sender: TObject; var AllowChange: boolean);
begin
  AllowChange := (VSProg_Caller <> nil) and ((not VSProg_Caller.IsRunning) or
    VSProg_Taken_By_Polling);
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

procedure TFormMain.pcMainPageChanged(Sender: TObject);
var
  strTmp:  string;
  intTmp:  integer;
  success: boolean;
  i, j:    integer;
begin
  if not Page_Init then
  begin
    exit;
  end;
  if (not pcMain.ActivePage.Enabled) or (VSProg_Caller.IsRunning and
    (not VSProg_Taken_By_Polling)) then
  begin
    UpdateTitle();
    exit;
  end;

  memoInfo.Clear;
  memoLog.Clear;

  // initialize GUI
  if pcMain.ActivePage = tsAbout then
  begin
    HideDebugLog();

    gbUpdate.Enabled := VSProg_Exists;

    // update settings
    if not AboutPage_Init then
    begin
      AboutPage_Init := True;
      fnFW.FileName  := xmlcfgMain.GetValue('fw/filename', '');
      ComboBoxSetText(cbboxCOM, xmlcfgMain.GetValue('fw/comm', ''));
    end;
  end
  else if pcMain.ActivePage = tsJTAG then
  begin
    ShowDebugLog();
    OpenOCD_Init();

    gbSVFPlayer.Enabled := VSprog_Exists;
    gbPower.Enabled     := VSprog_Exists;
    gbOpenOCD.Enabled   := OpenOCD_Exists;

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
  end
  else
  begin
    if PreviousPage <> pcMain.ActivePage.Caption then
    begin
      SetLength(TargetFile, 0);
      cbboxInputFile.Clear;
    end;
    PreviousPage := pcMain.ActivePage.Caption;
    HideDebugLog();

    CurTargetSeries := VSProg_Targets.TargetSeries[pcMain.ActivePage.Tag];
    if VSProg_GUITargetSeriesPageInit(pcMain.ActivePage.Tag) and
      (cbboxTarget.ItemIndex >= 0) then
    begin
      // update settings
      if not TargetPage_Init then
      begin
        TargetPage_Init := True;
        strTmp  := xmlcfgMain.GetValue('target/chip', '');
        success := False;
        for i := 0 to cbboxTarget.Items.Count - 1 do
        begin
          if strTmp = cbboxTarget.Items.Strings[i] then
          begin
            success := True;
            break;
          end;
        end;
        if success then
        begin
          ComboBoxSetText(cbboxTarget, xmlcfgMain.GetValue('target/chip', ''));
          cbboxTargetChange(cbboxTarget);
          if cbboxMode.Items.Count > 0 then
          begin
            ComboBoxSetText(cbboxMode, xmlcfgMain.GetValue('target/mode', ''));
            cbboxModeChange(cbboxMode);
          end;
          sedtFreq.Value := xmlcfgMain.GetValue('target/freq', 1000);
          if lbledtAddr.Enabled then
          begin
            lbledtAddr.Text := xmlcfgMain.GetValue('target/exe_addr', '');
          end;
          intTmp := xmlcfgMain.GetValue('target/files/number', 0);
          if Length(TargetFile) < intTmp then
          begin
            // Error here, try to recovery
            xmlcfgMain.DeletePath('target/files');
            xmlcfgMain.SetValue('target/files/number', 0);
            intTmp := 0;
          end;
          if intTmp > 0 then
          begin
            for i := 0 to intTmp - 1 do
            begin
              j := GetTargetFileIdx(xmlcfgMain.GetValue('target/files/' +
                IntToStr(i) + '/target', '')[1]);
              if j < 0 then
              begin
                // Error here, try to recovery
                xmlcfgMain.DeletePath('target/files');
                xmlcfgMain.SetValue('target/files/number', 0);
                intTmp := 0;
                break;
              end;
              TargetFile[j].filename :=
                xmlcfgMain.GetValue('target/files/' + IntToStr(i) + '/filename', '');
            end;
          end;
          UpdateComboTargetFile();
          ComboBoxSetText(cbboxInputFile, xmlcfgMain.GetValue('target/filename', ''));
          if lbledtFuse.Enabled then
          begin
            lbledtFuse.Text      := xmlcfgMain.GetValue('target/fuse', '');
          end;
          if lbledtLock.Enabled then
          begin
            lbledtLock.Text      := xmlcfgMain.GetValue('target/lock', '');
          end;
          if lbledtCali.Enabled then
          begin
            lbledtCali.Text      := xmlcfgMain.GetValue('target/cali', '');
          end;
          if lbledtUsrSig.Enabled then
          begin
            lbledtUsrSig.Text    := xmlcfgMain.GetValue('target/usrsig', '');
          end;
          if chkboxNoconnect.Enabled then
          begin
            chkboxNoconnect.Checked := xmlcfgMain.GetValue('target/nc', False);
          end;
          if chkboxNowarning.Enabled then
          begin
            chkboxNowarning.Checked := xmlcfgMain.GetValue('target/nw', False);
          end;
          if chkboxApp.Enabled then
          begin
            chkboxApp.Checked    := xmlcfgMain.GetValue('target/flashen', False);
          end;
          if chkboxEE.Enabled then
          begin
            chkboxEE.Checked     := xmlcfgMain.GetValue('target/eepromen', False);
          end;
          if chkboxFuse.Enabled then
          begin
            chkboxFuse.Checked   := xmlcfgMain.GetValue('target/fuseen', False);
          end;
          if chkboxLock.Enabled then
          begin
            chkboxLock.Checked   := xmlcfgMain.GetValue('target/locken', False);
          end;
          if chkboxUsrSig.Enabled then
          begin
            chkboxUsrSig.Checked := xmlcfgMain.GetValue('target/usrsigen', False);
          end;
          if chkboxCali.Enabled then
          begin
            chkboxCali.Checked   := xmlcfgMain.GetValue('target/calien', False);
          end;
          if chkboxMP.Enabled then
          begin
            chkboxMP.Checked     := xmlcfgMain.GetValue('target/mass', False);
          end;
          chkboxEraseBeforeWrite.Checked := xmlcfgMain.GetValue('target/ebw', False);
          chkboxVerifyAfterWrite.Checked := xmlcfgMain.GetValue('target/vaw', False);
          lbledtExtraPara.Text := xmlcfgMain.GetValue('target/extraparam', '');
        end;
      end;
    end
    else
    begin
      pcMain.ActivePage.Enabled := False;
    end;
  end;
  UpdateTitle();
end;

procedure TFormMain.sedtFreqEditingDone(Sender: TObject);
begin
  if sedtFreq.Text = '' then
  begin
    sedtFreq.Value := 0;
  end;
end;

procedure TFormMain.btnTargetDetectClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_Caller.AddParameter('s' + cbboxTarget.Items.Strings[0]);
  VSProg_PrepareMiscParameters(VSProg_Caller);

  LogInfo('Running...');
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.AutoDetectParser, 1, False) then
  begin
    ComboBoxSetText(cbboxTarget, VSProg_Parser.ResultStrings.Strings[0]);
    cbboxTargetChange(cbboxTarget);
  end;
  LogInfo('Idle');
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
    exit;
  end;

  if not VSProg_PrepareToRunCLI then
  begin
    Beep;
    MessageDlg('Error', 'Fail to run.', mtError, [mbOK], 0);
    exit;
  end;

  VSProg_Caller.AddParametersString('-G -Z -sstm32 -mi -C' +
    cbboxCOM.Text + ' ' + ' -x0x08002000 -oe -owf -I"' + fnFW.FileName + '"');
  LogInfo('Running...');
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False) then
  begin
    MessageDlg('OK', 'FW Updated OK.', mtInformation, [mbOK], 0);
  end;
  LogInfo('Idle');
end;

function TFormMain.VSProg_GetTargetDefineParameters(): string;
begin
  if cbboxTarget.ItemIndex = 0 then
  begin
    Result := 's';
  end
  else
  begin
    Result := 'c';
  end;
  Result := Result + CurTargetChip.Name;
end;

function TFormMain.VSProg_GetEnabledOperationString(): string;
begin
  Result := '';
  if chkboxApp.Enabled and chkboxApp.Checked then
  begin
    Result := Result + FLASH_CHAR;
  end;
  if chkboxEE.Enabled and chkboxEE.Checked then
  begin
    Result := Result + EE_CHAR;
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

function TFormMain.VSProg_AddEraseOperation(): boolean;
begin
  VSProg_Caller.AddParameter('oe');
  Result := True;
end;

function TFormMain.VSProg_AddWriteOperation(): boolean;
var
  para: string;
begin
  Result := False;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
  begin
    Result := True;
    para   := 'ow' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_AddVerifyOperation(): boolean;
var
  para: string;
begin
  Result := True;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
  begin
    Result := True;
    para   := 'ov' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_AddReadOperation(): boolean;
var
  para: string;
begin
  Result := True;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
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
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_PrepareOperationParameters(VSProg_Caller);
  if not VSProg_AddVerifyOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnEraseClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_PrepareOperationParameters(VSProg_Caller);
  if not VSProg_AddEraseOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnModeSetupClick(Sender: TObject);
begin
  FormComSetup.ShowModal;
  FormComSetup.GetComMode(ComMode);
end;

procedure TFormMain.btnOpenFileClick(Sender: TObject);
var
  file_num: integer;
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
    if mrOk = FormFileSelector.ShowModal then
    begin
      UpdateComboTargetFile();
      bOpenFileOK := True;
    end;
  end
  else
  begin
    odInputFile.FileName := TargetFile[0].filename;
    if odInputFile.Execute then
    begin
      TargetFile[0].filename := odInputFile.FileName;
      UpdateComboTargetFile();
      bOpenFileOK := True;
    end;
  end;
end;

procedure TFormMain.btnOpenOCDRunClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunOpenOCD then
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
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnOpenOCDStopClick(Sender: TObject);
begin
  if (VSProg_Caller <> nil) and (VSProg_Caller.Application =
    dedtOpenOCD.Directory + OPENOCD_STR) then
  begin
    VSProg_Caller.Stop();
  end;
end;

procedure TFormMain.ShowTargetArea(AreaName: char; var Sender: TObject;
  parser: TParserFunc);
var
  areaidx, fileidx: integer;
  bytelen: integer;
  default: QWord;
  targetdefined:    string;
begin
  areaidx := CurTargetChip.GetAreaIdx(AreaName);
  if areaidx < 0 then
  begin
    exit;
  end;

  if CurTargetChip.TargetAreas[areaidx].InFile then
  begin
    fileidx := GetTargetFileIdx(AreaName);
    if (fileidx < 0) or (TargetFile[fileidx].filename = '') then
    begin
      Beep();
      MessageDlg('Error', 'No File Defined.', mtError, [mbOK], 0);
      exit;
    end;
    if not FileExists(TargetFile[fileidx].filename) then
    begin
      Beep();
      MessageDlg('Error', TargetFile[fileidx].filename + 'not exists.',
        mtError, [mbOK], 0);
      exit;
    end;

    FormHexEditor.FileName := TargetFile[fileidx].filename;
    FormHexEditor.StartAddress := CurTargetChip.GetArea(AreaName).StartAddr;
    FormHexEditor.DataByteSize := CurTargetChip.GetArea(AreaName).ByteLen;
    FormHexEditor.DefaultData := CurTargetChip.GetArea(AreaName).DefaultValue;
    FormHexEditor.SegAddr := CurTargetChip.GetArea(AreaName).SegAddr;
    FormHexEditor.AddressOffset := 0;
    FormHexEditor.Target := GetAreaFullName(AreaName);

    FormHexEditor.ShowModal;
  end
  else
  begin
    if not (Sender is TLabeledEdit) then
    begin
      exit;
    end;
    targetdefined := VSProg_GetTargetDefineParameters();
    if targetdefined[1] = 's' then
    begin
      Beep();
      MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
      exit;
    end;

    bytelen := CurTargetChip.TargetAreas[areaidx].ByteLen;
    default := CurTargetChip.TargetAreas[areaidx].DefaultValue;

    if not chkboxNoconnect.Checked then
    begin
      // call 'vsprog -or' to read settings from target
      if not VSProg_PrepareToRunCLI then
      begin
        exit;
      end;
      VSProg_PrepareBaseParameters(VSProg_Caller);
      VSProg_Caller.AddParameter('or' + AreaName);
      if not VSProg_RunAlgorithm(VSProg_Caller, parser, 1, False) then
      begin
        exit;
      end;
      VSProg_GUIUpdateULCS(StrToInt(VSProg_Parser.ResultStrings.Strings[0]) and
        default, bytelen, TLabeledEdit(Sender));
    end;

    // call 'vsprog -Ppara' to extract para settings
    if not VSProg_PrepareToRunCLI then
    begin
      exit;
    end;
    VSProg_PrepareBaseParameters(VSProg_Caller);
    VSProg_Caller.AddParameter('P' + AreaName);
    FormParaEditor.FreeRecord();
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Parser.CallbackFunc    := @VSProg_SettingTargetParserCallback;
    if not VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SettingTargetInfoParser,
      0, False) then
    begin
      exit;
    end;

    FormParaEditor.SetParameter(default, bytelen, StrToInt(
      (Sender as TLabeledEdit).Text),
      GetAreaFullName(AreaName), not chkboxNowarning.Checked);
    if mrOk = FormParaEditor.ShowModal then
    begin
      // OK clicked, get value
      VSProg_GUIUpdateULCS(FormParaEditor.GetResult(), bytelen, TLabeledEdit(Sender));
    end;
  end;
end;

procedure TFormMain.btnEditAppClick(Sender: TObject);
begin
  ShowTargetArea(FLASH_CHAR, Sender, nil);
end;

procedure TFormMain.btnEditCaliClick(Sender: TObject);
begin
  ShowTargetArea(CALI_CHAR, lbledtCali, @VSProg_Parser.CaliDataParser);
end;

procedure TFormMain.btnEditEEClick(Sender: TObject);
begin
  ShowTargetArea(EE_CHAR, Sender, nil);
end;

procedure TFormMain.btnEditFuseClick(Sender: TObject);
begin
  ShowTargetArea(FUSE_CHAR, lbledtFuse, @VSProg_Parser.FuseDataParser);
end;

procedure TFormMain.btnEditLockClick(Sender: TObject);
begin
  ShowTargetArea(LOCK_CHAR, lbledtLock, @VSProg_Parser.LockDataParser);
end;

procedure TFormMain.btnEditUsrSigClick(Sender: TObject);
begin
  ShowTargetArea(USRSIG_CHAR, lbledtUsrsig, @VSProg_Parser.UsrsigDataParser);
end;

procedure TFormMain.btnReadClick(Sender: TObject);
begin
  // select output file
  btnOpenFile.Click;
  if not bOpenFileOK then
  begin
    exit;
  end;

  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;
  bReadOperation := True;
  VSProg_PrepareOperationParameters(VSProg_Caller);
  if not VSProg_AddReadOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add read operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    bReadOperation := False;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
  bReadOperation := False;
end;

procedure TFormMain.btnSetPowerClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_Caller.AddParameter('V"powerout ' + IntToStr(sedtPower.Value) + '"');
  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, nil, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnSVFRunClick(Sender: TObject);
begin
  if fneditSVFFile.FileName <> '' then
  begin
    if not VSProg_PrepareToRunCLI then
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
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
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
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_PrepareOperationParameters(VSProg_Caller);

  if chkboxEraseBeforeWrite.Checked and not VSProg_AddEraseOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  if not VSProg_AddWriteOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add write operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  if chkboxVerifyAfterWrite.Checked and not VSProg_AddVerifyOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.cbboxInputFileChange(Sender: TObject);
begin
  cbboxInputFile.Hint := cbboxInputFile.Text;
end;

procedure TFormMain.cbboxModeChange(Sender: TObject);
begin
  if cbboxMode.Items.Count > 0 then
  begin
    VSProg_GUIFeatureInit(CurTargetSeries.Feature +
      CurTargetSeries.GetModeFeatureByModeChar(CurTargetChip.Mode[1 +
      cbboxMode.ItemIndex]), False);

    AdjustComponentColor(cbboxMode);
    AdjustComponentColor(lbledtFuse);
    AdjustComponentColor(lbledtLock);
    AdjustComponentColor(lbledtCali);
    AdjustComponentColor(lbledtUsrSig);
    AdjustComponentColor(lbledtAddr);
    AdjustComponentColor(sedtFreq);
  end;
end;

procedure TFormMain.cbboxTargetChange(Sender: TObject);
var
  i: integer;
begin
  CurTargetChip := CurTargetSeries.TargetChips[cbboxTarget.ItemIndex];

  if CurTargetChip.AreaCount = 0 then
  begin
    // Parse Memory Info using '-D' option
    if not VSProg_PrepareToRunCli then
    begin
      exit;
    end;
    VSProg_Caller.AddParameter(VSProg_GetTargetDefineParameters);
    VSProg_Caller.AddParameter('Dall');
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Parser.CallbackFunc    := @CurTargetChip.TargetAreaParser;
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.MemoryTargetInfoParser, 0, True);
  end;

  for i := 0 to CurTargetChip.AreaCount - 1 do
  begin
    if CurTargetChip.TargetAreas[i].InFile then
    begin
      AddTargetFile(CurTargetChip.TargetAreas[i].Name);
    end;
  end;

  VSProg_GUITargetAreaInit;
  VSProg_GUIModeInit;

  AdjustComponentColor(cbboxMode);
  AdjustComponentColor(lbledtFuse);
  AdjustComponentColor(lbledtLock);
  AdjustComponentColor(lbledtCali);
  AdjustComponentColor(lbledtUsrSig);
  AdjustComponentColor(lbledtAddr);
  AdjustComponentColor(sedtFreq);
end;

procedure TFormMain.dedtPathChange(Sender: TObject);
begin
  if (Sender as TDirectoryEdit).Directory[Length(
    (Sender as TDirectoryEdit).Directory)] <> System.DirectorySeparator then
  begin
    (Sender as TDirectoryEdit).Directory :=
      (Sender as TDirectoryEdit).Directory + System.DirectorySeparator;
  end;

  VSProg_Exists  := FileExists(dedtVSProg.Directory + VSPROG_STR);
  OpenOCD_Exists := FileExists(dedtOpenOCD.Directory + OPENOCD_STR);
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  if PollThread <> nil then
  begin
    PollThread.Terminate;
    Sleep(100);
  end;

  // save settings to config file
  xmlcfgMain.SetValue('vsprog_dir', dedtVSProg.Directory);
  xmlcfgMain.SetValue('openocd_dir', dedtOpenOCD.Directory);
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
  xmlcfgMain.SetValue('target/freq', sedtFreq.Value);
  if lbledtAddr.Enabled then
  begin
    xmlcfgMain.SetValue('target/exe_addr', lbledtAddr.Text);
  end;
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
  if lbledtFuse.Enabled then
  begin
    xmlcfgMain.SetValue('target/fuse', lbledtFuse.Text);
  end;
  if lbledtLock.Enabled then
  begin
    xmlcfgMain.SetValue('target/lock', lbledtLock.Text);
  end;
  if lbledtCali.Enabled then
  begin
    xmlcfgMain.SetValue('target/cali', lbledtCali.Text);
  end;
  if lbledtUsrSig.Enabled then
  begin
    xmlcfgMain.SetValue('target/usrsig', lbledtUsrSig.Text);
  end;
  if chkboxNoconnect.Enabled then
  begin
    xmlcfgMain.SetValue('target/nc', chkboxNoconnect.Checked);
  end;
  if chkboxNowarning.Enabled then
  begin
    xmlcfgMain.SetValue('target/nw', chkboxNowarning.Checked);
  end;
  if chkboxApp.Enabled then
  begin
    xmlcfgMain.SetValue('target/flashen', chkboxApp.Checked);
  end;
  if chkboxEE.Enabled then
  begin
    xmlcfgMain.SetValue('target/eepromen', chkboxEE.Checked);
  end;
  if chkboxFuse.Enabled then
  begin
    xmlcfgMain.SetValue('target/fuseen', chkboxFuse.Checked);
  end;
  if chkboxLock.Enabled then
  begin
    xmlcfgMain.SetValue('target/locken', chkboxLock.Checked);
  end;
  if chkboxUsrSig.Enabled then
  begin
    xmlcfgMain.SetValue('target/usrsigen', chkboxUsrSig.Checked);
  end;
  if chkboxCali.Enabled then
  begin
    xmlcfgMain.SetValue('target/calien', chkboxCali.Checked);
  end;
  if  chkboxMP.Enabled then
  begin
    xmlcfgMain.SetValue('target/mass', chkboxMP.Checked);
  end;
  xmlcfgMain.SetValue('target/ebw', chkboxEraseBeforeWrite.Checked);
  xmlcfgMain.SetValue('target/vaw', chkboxVerifyAfterWrite.Checked);
  xmlcfgMain.SetValue('target/extraparam', lbledtExtraPara.Text);
  xmlcfgMain.Flush;

  tiMain.Hide;
  CloseAction := caFree;
end;

procedure TFormMain.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormMain.AdjustComponentColor(Sender: TControl);
begin
  if Sender.Enabled then
  begin
    Sender.Color := clWindow;
  end
  else
  begin
    Sender.Color := clBtnFace;
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

function TFormMain.VSProg_PrepareToRun(aApplicationName: string): boolean;
begin
  Result := False;
  if not FileExists(aApplicationName) then
  begin
    exit;
  end;

  // take it first
  if not VSProg_Caller.Take() then
  begin
    if VSProg_Taken_By_Polling then
    begin
      // occupied by polling thread, wait a while
      Sleep(100);
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

  VSProg_Caller.Application := aApplicationName;
  VSProg_Caller.RemoveAllParameters();
  Result := True;
  memoLog.Clear;
  memoInfo.Clear;
end;

function TFormMain.VSProg_PrepareToRunOpenOCD: boolean;
begin
  Result := VSProg_PrepareToRun(dedtOpenOCD.Directory + OPENOCD_STR);
end;

function TFormMain.VSProg_PrepareToRunCLI: boolean;
begin
  Result := VSProg_PrepareToRun(dedtVSProg.Directory + VSPROG_STR);
end;

procedure TFormMain.VSProg_PrepareMiscParameters(var caller: TCLI_Caller);
var
  i: integer;
  io_file_opt, strTmp, strExt: string;
  areaTmp: TTargetArea;
  FakeArea: TFakeArea;
  addr, seg: cardinal;
  faddr, fseg: cardinal;
begin
  // COM Mode
  if btnModeSetup.Visible then
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

  if cbboxInputFile.Text = '' then
  begin

  end
  else if cbboxInputFile.Text <> 'ALL' then
  begin
    // enable selected input file
    i := Pos(':', cbboxInputFile.Text);
    areaTmp := CurTargetChip.GetArea(GetAreaShortName(
      Copy(cbboxInputFile.Text, 1, i - 1)));
    if areaTmp <> nil then
    begin
      addr     := areaTmp.StartAddr;
      seg      := areaTmp.SegAddr;
      faddr    := 0;
      fseg     := 0;
      FakeArea := CurTargetSeries.GetFakeArea(areaTmp.Name);
      if FakeArea <> nil then
      begin
        if FakeArea.FakeAddrEn then
        begin
          faddr := FakeArea.FakeAddr;
          Inc(addr, faddr);
        end;
        if FakeArea.FakeSegEn then
        begin
          fseg := FakeArea.FakeSeg;
          Inc(seg, fseg);
        end;
      end;

      strTmp := Copy(cbboxInputFile.Text, i + 1, Length(cbboxInputFile.Text) - i);
      strExt := LowerCase(ExtractFileExt(strTmp));
      if strExt = '.hex' then
      begin
        caller.AddParameter(io_file_opt + '"' + strTmp + '@' +
          IntToStr(fseg) + ',' + IntToStr(faddr) + '"');
      end
      else if strExt = '.bin' then
      begin
        caller.AddParameter(io_file_opt + '"' + strTmp + '@' +
          IntToStr(seg) + ',' + IntToStr(addr) + '"');
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
        areaTmp := CurTargetChip.GetArea(TargetFile[i].target);
        if areaTmp <> nil then
        begin
          addr     := areaTmp.StartAddr;
          seg      := areaTmp.SegAddr;
          faddr    := 0;
          fseg     := 0;
          FakeArea := CurTargetSeries.GetFakeArea(areaTmp.Name);
          if FakeArea <> nil then
          begin
            if FakeArea.FakeAddrEn then
            begin
              faddr := FakeArea.FakeAddr;
              Inc(addr, faddr);
            end;
            if FakeArea.FakeSegEn then
            begin
              fseg := FakeArea.FakeSeg;
              Inc(seg, fseg);
            end;
          end;

          strExt := LowerCase(ExtractFileExt(TargetFile[i].filename));
          if strExt = '.hex' then
          begin
            caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
              '@' + IntToStr(fseg) + ',' + IntToStr(faddr) + '"');
          end
          else if strExt = '.bin' then
          begin
            caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
              '@' + IntToStr(seg) + ',' + IntToStr(addr) + '"');
          end;
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
  if sedtFreq.Visible and sedtFreq.Enabled and (sedtFreq.Value > 0) then
  begin
    caller.AddParameter('F' + IntToStr(sedtFreq.Value));
  end;

  // ProgrammerParameter
  if ProgrammerParameter <> '' then
  begin
    caller.AddParameter('U' + ProgrammerParameter);
  end;
end;

procedure TFormMain.VSProg_PrepareBaseParameters(var caller: TCLI_Caller);
begin
  caller.AddParameter(VSProg_GetTargetDefineParameters());

  VSProg_PrepareMiscParameters(caller);
end;

procedure TFormMain.VSProg_PrepareOperationParameters(var caller: TCLI_Caller);
begin
  // enable GUI mode
  caller.AddParameter('G');
  VSProg_PrepareBaseParameters(caller);

  // Fuse
  if lbledtFuse.Enabled and chkboxFuse.Enabled and chkboxFuse.Checked and
    (lbledtFuse.Text <> '') then
  begin
    caller.AddParameter('tu' + lbledtFuse.Text);
  end;

  // Lock
  if lbledtLock.Enabled and chkboxLock.Enabled and chkboxLock.Checked and
    (lbledtLock.Text <> '') then
  begin
    caller.AddParameter('tl' + lbledtLock.Text);
  end;

  // Execute
  if lbledtAddr.Visible and lbledtAddr.Enabled and (lbledtAddr.Text <> '') then
  begin
    caller.AddParameter('x' + lbledtAddr.Text);
  end;

  // Mass-product support
  if chkboxMP.Enabled and chkboxMP.Checked then
  begin
    caller.AddParameter('M');
  end;
end;

procedure TFormMain.tiMainClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
end;

function TFormMain.VSProg_SettingTargetParserCallback(var line: string): boolean;
var
  dis: string;
begin
  Result := True;
  if Pos('setting: ', line) = 1 then
  begin
    // check disable
    dis := '';
    GetParameter(line, 'ban', dis);
    if (dis = '*') or (Pos(cbboxMode.Text[1], dis) > 0) then
    begin
      // current setting is disabled in current mode
      line := line + ', disabled = 1';
    end;
  end;

  FormParaEditor.ParseLine(line);
end;

{ OpenOCD functions }
function TFormMain.OpenOCD_Init(): boolean;
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

initialization
  {$I main.lrs}

end.


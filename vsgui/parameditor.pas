unit parameditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, inputdialog, vsprogparser;

type
  TParam_Warning = record
    mask:  integer;
    Value: integer;
    msg:   string;
  end;

  TParam_Choice = record
    Value: integer;
    Text:  string;
  end;

  TParam_Setting = record
    Name:      string;
    info:      string;
    mask:      integer;
    use_checkbox: boolean;
    use_edit:  boolean;
    radix:     integer;
    shift:     integer;
    Checked:   integer;
    unchecked: integer;
    Enabled:   boolean;
    bytelen:   integer;
    choices:   array of TParam_Choice;
  end;

  TParam_Record = record
    init_value: integer;
    warnings:   array of TParam_Warning;
    settings:   array of TParam_Setting;
  end;

  { TFormParaEditor }

  TFormParaEditor = class(TForm)
    btnOK:     TButton;
    btnCancel: TButton;
    pnlSettings: TPanel;
    pnlButton: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure pnlButtonClick(Sender: TObject);
    procedure pnlSettingsClick(Sender: TObject);
    procedure SettingChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CenterControl(ctl: TControl; ref: TControl);
  private
    { private declarations }
    Param_Record:    TParam_Record;
    ParaEdtNameArr:  array of TEdit;
    ParaComboArr:    array of TComboBox;
    ParaCheckArr:    array of TCheckBox;
    ParaEdtValueArr: array of TEdit;
    Param_name:      string;
    Init_Value, Param_Value, Value_ByteLen: integer;
    EnableWarning:   boolean;
    SettingParameter: boolean;
  public
    { public declarations }
    function GetResult(): integer;
    function ParseSettingMaskValue(Sender: TObject; var mask, Value: integer): boolean;
    procedure ValueToSetting();
    procedure UpdateTitle();
    procedure SetParameter(init, bytelen, Value: integer; title: string;
      warnEnabled: boolean);
    function ParseLine(line: string): boolean;
    procedure FreeRecord();
  end;

var
  FormParaEditor: TFormParaEditor;
  bCancel: boolean;

const
  LEFT_MARGIN: integer   = 10;
  RIGHT_MARGIN: integer  = 10;
  TOP_MARGIN: integer    = 10;
  BOTTOM_MARGIN: integer = 10;
  X_MARGIN: integer      = 4;
  Y_MARGIN: integer      = 4;
  ITEM_HEIGHT: integer   = 20;
  EDT_WIDTH: integer     = 100;
  COMBO_WIDTH: integer   = 400;
  SECTION_DIV_HEIGHT: integer = 20;

implementation

function GetStringLen_To_ByteLen(bytelen, radix: integer): integer;
begin
  if (radix < 2) or (radix > 16) or (BYTELEN_ACCORDING_TO_RADIX[radix] = 0) then
  begin
    Result := 0;
    exit;
  end;

  Result := bytelen * BYTELEN_ACCORDING_TO_RADIX[radix];
end;

procedure TFormParaEditor.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormParaEditor.SettingChange(Sender: TObject);
var
  mask, Value: integer;
begin
  if SettingParameter then
  begin
    exit;
  end;

  mask  := 0;
  Value := 0;
  if ParseSettingMaskValue(Sender, mask, Value) = False then
  begin
    exit;
  end;
  Param_Value := Param_Value and not mask;
  Param_Value := Param_Value or Value;

  SettingParameter := True;
  ValueToSetting();
  SettingParameter := False;

  UpdateTitle();
end;

procedure TFormParaEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormParaEditor.pnlButtonClick(Sender: TObject);
begin
  ActiveControl := btnOK;
end;

procedure TFormParaEditor.pnlSettingsClick(Sender: TObject);
begin
  ActiveControl := btnOK;
end;

procedure TFormParaEditor.btnOKClick(Sender: TObject);
begin
  bCancel := False;
end;

procedure TFormParaEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: integer;
begin
  FormParaEditor.ActiveControl := btnOK;

  CanClose := True;

  if not bCancel and EnableWarning then
  begin
    for i := low(Param_Record.warnings) to high(Param_Record.warnings) do
    begin
      if (Param_Value and Param_Record.warnings[i].mask) =
        Param_Record.warnings[i].Value then
      begin
        if mrNo = MessageDlg(Param_Record.warnings[i].msg, mtWarning,
          [mbYes, mbNo], 0) then
        begin
          bCancel  := True;
          CanClose := False;
          exit;
        end;
      end;
    end;
  end;
end;

procedure TFormParaEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  FreeRecord();

  for i := low(ParaEdtNameArr) to high(ParaEdtNameArr) do
  begin
    if Assigned(ParaEdtNameArr[i]) then
    begin
      ParaEdtNameArr[i].Destroy;
    end;
  end;
  for i := low(ParaComboArr) to high(ParaComboArr) do
  begin
    if Assigned(ParaComboArr[i]) then
    begin
      ParaComboArr[i].Destroy;
    end;
  end;
  for i := low(ParaCheckArr) to high(ParaCheckArr) do
  begin
    if Assigned(ParaCheckArr[i]) then
    begin
      ParaCheckArr[i].Destroy;
    end;
  end;
  for i := low(ParaEdtValueArr) to high(ParaEdtValueArr) do
  begin
    if Assigned(ParaEdtValueArr[i]) then
    begin
      ParaEdtValueArr[i].Destroy;
    end;
  end;
  SetLength(ParaEdtNameArr, 0);
  SetLength(ParaComboArr, 0);
  SetLength(ParaCheckArr, 0);
  SetLength(ParaEdtValueArr, 0);

  CloseAction := caHide;
end;

procedure TFormParaEditor.UpdateTitle();
begin
  Caption := Param_Name + ': 0x' + IntToHex(Param_Value, Value_ByteLen * 2);
end;

function TFormParaEditor.GetResult(): integer;
begin
  Result := Param_Value;
end;

procedure TFormParaEditor.SetParameter(init, bytelen, Value: integer;
  title: string; warnEnabled: boolean);
begin
  Init_Value    := init;
  Value_ByteLen := bytelen;
  Param_Value   := Value;
  Param_Name    := title;
  EnableWarning := warnEnabled;
end;

function TFormParaEditor.ParseSettingMaskValue(Sender: TObject;
  var mask, Value: integer): boolean;
var
  i: integer;
  str_tmp: string;
begin
  Result := True;

  if Sender is TComboBox then
  begin
    i := (Sender as TComboBox).Tag;
  end
  else if Sender is TCheckBox then
  begin
    i := (Sender as TCheckBox).Tag;
  end
  else if Sender is TEdit then
  begin
    i := (Sender as TEdit).Tag;
  end
  else
  begin
    // this component is not supported as a setting component
    Result := False;
    exit;
  end;

  mask := Param_Record.settings[i].mask;
  if Param_Record.settings[i].use_edit then
  begin
    str_tmp := Copy(ParaEdtValueArr[i].Text, 1,
      GetStringLen_To_ByteLen(Param_Record.settings[i].bytelen,
      Param_Record.settings[i].radix));
    Value   := StrToIntRadix(str_tmp, Param_Record.settings[i].radix);
    Value   := Value shl Param_Record.settings[i].shift;
    if (Value and not mask) > 0 then
    begin
      Value := Value and mask;
    end;
  end
  else if Param_Record.settings[i].use_checkbox then
  begin
    if ParaCheckArr[i].Checked then
    begin
      Value := Param_Record.settings[i].Checked;
    end
    else
    begin
      Value := Param_Record.settings[i].unchecked;
    end;
  end
  else
  begin
    Value := Param_Record.settings[i].choices[ParaComboArr[i].ItemIndex].Value;
  end;
end;

procedure TFormParaEditor.ValueToSetting();
var
  i, j:  integer;
  Value: integer;
  found: boolean;
begin
  SettingParameter := True;
  for i := low(Param_Record.settings) to high(Param_Record.settings) do
  begin
    Value := Param_Value and Param_Record.settings[i].mask;
    if Param_Record.settings[i].use_edit then
    begin
      Value := Value shr Param_Record.settings[i].shift;
      ParaEdtValueArr[i].Text := IntToStrRadix(Value, Param_Record.settings[i].radix);
      while Length(ParaEdtValueArr[i].Text) <
        GetStringLen_To_ByteLen(Param_Record.settings[i].bytelen,
          Param_Record.settings[i].radix) do
      begin
        ParaEdtValueArr[i].Text := '0' + ParaEdtValueArr[i].Text;
      end;
      ParaEdtValueArr[i].Hint := ParaEdtValueArr[i].Text;
    end
    else if Param_Record.settings[i].use_checkbox then
    begin
      if Value = Param_Record.settings[i].Checked then
      begin
        ParaCheckArr[i].Checked := True;
      end
      else if Value = Param_Record.settings[i].unchecked then
      begin
        ParaCheckArr[i].Checked := False;
      end
      else
      begin
        // unrecognized value
        ParaEdtNameArr[i].Color := clRed;
      end;
    end
    else
    begin
      found := False;
      for j := low(Param_Record.settings[i].choices)
        to high(Param_Record.settings[i].choices) do
      begin
        if Value = Param_Record.settings[i].choices[j].Value then
        begin
          ParaEdtNameArr[i].Color := clWindow;
          ParaComboArr[i].ItemIndex := j;
          found := True;
          break;
        end;
      end;
      if not found then
      begin
        // there is an error
        ParaEdtNameArr[i].Color := clRed;
      end;
      ParaComboArr[i].Hint := ParaComboArr[i].Text;
    end;
  end;
  SettingParameter := False;
end;

procedure TFormParaEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  settings_num, choices_num, section_num, mask: integer;
begin
  bCancel := True;

  // create components according to Param_Record
  settings_num := Length(Param_Record.settings);
  SetLength(ParaEdtNameArr, settings_num);
  SetLength(ParaComboArr, settings_num);
  SetLength(ParaCheckArr, settings_num);
  SetLength(ParaEdtValueArr, settings_num);

  section_num := 0;
  mask := 0;

  SettingParameter := False;
  for i := 0 to settings_num - 1 do
  begin
    if (Param_Record.settings[i].mask and mask) > 0 then
    begin
      Inc(section_num);
      mask := 0;
    end;
    mask := mask or Param_Record.settings[i].mask;

    ParaEdtNameArr[i]      := TEdit.Create(Self);
    ParaEdtNameArr[i].Parent := pnlSettings;
    ParaEdtNameArr[i].Top  := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) +
      section_num * SECTION_DIV_HEIGHT;
    ParaEdtNameArr[i].Left := LEFT_MARGIN;
    ParaEdtNameArr[i].Width := EDT_WIDTH;
    ParaEdtNameArr[i].Height := ITEM_HEIGHT;
    ParaEdtNameArr[i].Text := Param_Record.settings[i].Name;
    if Param_Record.settings[i].use_edit then
    begin
      ParaEdtNameArr[i].Text :=
        ParaEdtNameArr[i].Text + '(r:' + IntToStr(Param_Record.settings[i].radix) + ')';
    end;
    ParaEdtNameArr[i].Color    := clWindow;
    ParaEdtNameArr[i].Hint     := Param_Record.settings[i].info;
    ParaEdtNameArr[i].ShowHint := True;
    ParaEdtNameArr[i].ReadOnly := True;
    if Param_Record.settings[i].use_edit then
    begin
      ParaEdtValueArr[i]      := TEdit.Create(Self);
      ParaEdtValueArr[i].Parent := pnlSettings;
      ParaEdtValueArr[i].Top  :=
        TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) + section_num * SECTION_DIV_HEIGHT;
      ParaEdtValueArr[i].Width := COMBO_WIDTH;
      ParaEdtValueArr[i].Left := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaEdtValueArr[i].Height := ITEM_HEIGHT;
      ParaEdtValueArr[i].OnExit := @SettingChange;
      ParaEdtValueArr[i].Tag  := i;
      ParaEdtValueArr[i].ShowHint := True;
      ParaEdtValueArr[i].Enabled := Param_Record.settings[i].Enabled;
      CenterControl(ParaEdtValueArr[i], ParaEdtNameArr[i]);
    end
    else if Param_Record.settings[i].use_checkbox then
    begin
      ParaCheckArr[i]      := TCheckBox.Create(Self);
      ParaCheckArr[i].Parent := pnlSettings;
      ParaCheckArr[i].Top  := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) +
        section_num * SECTION_DIV_HEIGHT;
      ParaCheckArr[i].Left := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaCheckArr[i].Height := ITEM_HEIGHT;
      ParaCheckArr[i].OnChange := @SettingChange;
      ParaCheckArr[i].Tag  := i;
      ParaCheckArr[i].Enabled := Param_Record.settings[i].Enabled;
      ParaCheckArr[i].Caption := '';
      ParaCheckArr[i].Checked := False;
      CenterControl(ParaCheckArr[i], ParaEdtNameArr[i]);
    end
    else
    begin
      ParaComboArr[i]      := TComboBox.Create(Self);
      ParaComboArr[i].Parent := pnlSettings;
      ParaComboArr[i].Top  := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) +
        section_num * SECTION_DIV_HEIGHT;
      ParaComboArr[i].Left := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaComboArr[i].Width := COMBO_WIDTH;
      ParaComboArr[i].Height := ITEM_HEIGHT;
      ParaComboArr[i].OnChange := @SettingChange;
      ParaComboArr[i].Style := csDropDownList;
      ParaComboArr[i].Tag  := i;
      ParaComboArr[i].ShowHint := True;
      ParaComboArr[i].Enabled := Param_Record.settings[i].Enabled;
      ParaComboArr[i].Clear;
      choices_num := Length(Param_Record.settings[i].choices);
      for j := 0 to choices_num - 1 do
      begin
        ParaComboArr[i].Items.Add(Param_Record.settings[i].choices[j].Text);
      end;
      CenterControl(ParaComboArr[i], ParaEdtNameArr[i]);
    end;
  end;

  i := TOP_MARGIN + BOTTOM_MARGIN + settings_num * (Y_MARGIN + ITEM_HEIGHT) +
    section_num * SECTION_DIV_HEIGHT;
  ClientHeight := i + pnlButton.Height;
  pnlSettings.ClientHeight := i;
  i := LEFT_MARGIN + RIGHT_MARGIN + EDT_WIDTH + X_MARGIN + COMBO_WIDTH;
  ClientWidth := i;
  pnlSettings.ClientWidth := i;
  pnlButton.ClientWidth := i;
  // center buttons
  btnOK.Left := (pnlButton.Width div 2 - btnOK.Width) div 2;
  btnCancel.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnOK.Width) div 2;
  UpdateTitle();

  ValueToSetting();
  UpdateShowing;
end;

procedure TFormParaEditor.FreeRecord();
begin
  SetLength(Param_Record.warnings, 0);
  SetLength(Param_Record.settings, 0);
end;

function TFormParaEditor.ParseLine(line: string): boolean;
var
  i, j, num, dis: integer;
begin
  result := True;
  if Pos('warning: ', line) = 1 then
  begin
    i := Length(Param_Record.warnings) + 1;
    SetLength(Param_Record.warnings, i);
    GetNumericParameter(line, 'mask', Param_Record.warnings[i - 1].mask);
    GetNumericParameter(line, 'value', Param_Record.warnings[i - 1].Value);
    GetLiteralParameter(line, 'msg', Param_Record.warnings[i - 1].msg);
  end
  else if Pos('setting: ', line) = 1 then
  begin
    i := Length(Param_Record.settings) + 1;
    SetLength(Param_Record.settings, i);
    SetLength(Param_Record.settings[i - 1].choices, 0);
    GetLiteralParameter(line, 'name', Param_Record.settings[i - 1].Name);
    GetNumericParameter(line, 'mask', Param_Record.settings[i - 1].mask);
    Param_Record.settings[i - 1].bytelen := 0;
    GetNumericParameter(line, 'bytelen', Param_Record.settings[i - 1].bytelen);
    Param_Record.settings[i - 1].radix := 0;
    GetNumericParameter(line, 'radix', Param_Record.settings[i - 1].radix);
    if (Param_Record.settings[i - 1].radix < 2) or
      (Param_Record.settings[i - 1].radix > 16) then
    begin
      Param_Record.settings[i - 1].radix := 10;
    end;
    GetNumericParameter(line, 'shift', Param_Record.settings[i - 1].shift);
    GetLiteralParameter(line, 'info', Param_Record.settings[i - 1].info);
    Param_Record.settings[i - 1].use_checkbox := False;
    if GetNumericParameter(line, 'checked', Param_Record.settings[i - 1].Checked) then
    begin
      Param_Record.settings[i - 1].use_checkbox := True;
    end;
    if GetNumericParameter(line, 'unchecked', Param_Record.settings[i -
      1].unchecked) then
    begin
      Param_Record.settings[i - 1].use_checkbox := True;
    end;
    dis := 0;
    GetNumericParameter(line, 'disabled', dis);
    if dis > 0 then
    begin
      Param_Record.settings[i - 1].Enabled := False;
    end
    else
    begin
      Param_Record.settings[i - 1].Enabled := True;
    end;
    num := 0;
    GetNumericParameter(line, 'num_of_choices', num);
    if not Param_Record.settings[i - 1].use_checkbox and (num = 0) then
    begin
      Param_Record.settings[i - 1].use_edit := True;
    end
    else
    begin
      Param_Record.settings[i - 1].use_edit := False;
    end;
  end
  else if Pos('choice: ', line) = 1 then
  begin
    i := Length(Param_Record.settings);
    j := Length(Param_Record.settings[i - 1].choices) + 1;
    SetLength(Param_Record.settings[i - 1].choices, j);
    GetNumericParameter(line, 'value', Param_Record.settings[i -
      1].choices[j - 1].Value);
    GetLiteralParameter(line, 'text', Param_Record.settings[i - 1].choices[j - 1].Text);
  end;
end;

initialization
  {$I parameditor.lrs}

end.


unit parameditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type
  TParam_Warning = record
    mask: integer;
    value: integer;
    msg: string;
  end;
  TParam_Choice = record
    value: integer;
    text: string;
  end;
  TParam_Setting = record
    name: string;
    info: string;
    mask: integer;
    use_checkbox: boolean;
    checked: integer;
    unchecked: integer;
    enabled: boolean;
    choices: array of TParam_Choice;
  end;
  TParam_Record = record
    init_value: integer;
    warnings: array of TParam_Warning;
    settings: array of TParam_Setting;
  end;

  { TFormParaEditor }

  TFormParaEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    pnlSettings: TPanel;
    pnlButton: TPanel;
    procedure SettingChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    Param_Record: TParam_Record;
    ParaEdtArr: array of TEdit;
    ParaComboArr: array of TComboBox;
    ParaCheckArr: array of TCheckBox;
    Param_name: string;
    Init_Value, Param_Value, Value_ByteLen: integer;
    EnableWarning: boolean;
    SettingParameter: boolean;
  public
    { public declarations }
    function WipeTailEnter(var line: string): string;
    function GetResult(): integer;
    procedure SettingToValue();
    procedure ValueToSetting();
    procedure UpdateTitle();
    function GetIntegerParameter(line, para_name: string; var value: integer): boolean;
    function GetStringParameter(line, para_name: string; var value: string): boolean;
    procedure SetParameter(init, bytelen, value: integer; title: string; warnEnabled: boolean);
    procedure ParseLine(line: string);
    procedure FreeRecord();
  end; 

var
  FormParaEditor: TFormParaEditor;

const
  EQUAL_STR: string = ' = ';
  LEFT_MARGIN: integer = 10;
  RIGHT_MARGIN: integer = 10;
  TOP_MARGIN: integer = 10;
  BOTTOM_MARGIN: integer = 10;
  X_MARGIN: integer = 4;
  Y_MARGIN: integer = 4;
  ITEM_HEIGHT: integer = 20;
  EDT_WIDTH: integer = 100;
  COMBO_WIDTH: integer = 400;

implementation

function TFormParaEditor.WipeTailEnter(var line: string): string;
begin
  if Pos(#13 + '', line) > 0 then
  begin
    SetLength(line, Length(line) - 1);
  end;
  if Pos(#10 + '', line) > 0 then
  begin
    SetLength(line, Length(line) - 1);
  end;
  result := line;
end;

function TFormParaEditor.GetStringParameter(line, para_name: string; var value: string): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  WipeTailEnter(line);

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

procedure TFormParaEditor.SettingChange(Sender: TObject);
var
  i: integer;
  setting_mask: integer;
begin
  if SettingParameter then
  begin
    exit;
  end;

  SettingToValue();

  if EnableWarning then
  begin
    if Sender is TComboBox then
    begin
      setting_mask := (Sender as TComboBox).Tag;
    end
    else if Sender is TCheckBox then
    begin
      setting_mask := (Sender as TCheckBox).Tag;
    end
    else
    begin
      // this component is not supported as a setting component
      exit;
    end;

    for i := 0 to Length(Param_Record.warnings) - 1 do
    begin
      if (Param_Record.warnings[i].mask and setting_mask) <> 0 then
      begin
        if (Param_Value and Param_Record.warnings[i].mask) = Param_Record.warnings[i].value then
        begin
          MessageDlg(Param_Record.warnings[i].msg, mtWarning, [mbOK], 0);
        end;
      end;
    end;
  end;

  UpdateTitle();
end;

procedure TFormParaEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: integer;
begin
  FreeRecord();

  for i := 0 to Length(ParaEdtArr) - 1 do
  begin
    if Assigned(ParaEdtArr[i]) then
    begin
      ParaEdtArr[i].Destroy;
    end;
  end;
  for i := 0 to Length(ParaComboArr) - 1 do
  begin
    if Assigned(ParaComboArr[i]) then
    begin
      ParaComboArr[i].Destroy;
    end;
  end;
  for i := 0 to Length(ParaCheckArr) - 1 do
  begin
    if Assigned(ParaCheckArr[i]) then
    begin
      ParaCheckArr[i].Destroy;
    end;
  end;

  SetLength(ParaEdtArr, 0);
  SetLength(ParaComboArr, 0);
  SetLength(ParaCheckArr, 0);
end;

procedure TFormParaEditor.UpdateTitle();
begin
  Caption := Param_Name + ': 0x' + IntToHex(Param_Value, Value_ByteLen * 2);
end;

function TFormParaEditor.GetResult(): integer;
begin
  result := Param_Value;
end;

procedure TFormParaEditor.SetParameter(init, bytelen, value: integer; title: string; warnEnabled: boolean);
begin
  Init_Value := init;
  Value_ByteLen := bytelen;
  Param_Value := value;
  Param_Name := title;
  EnableWarning := warnEnabled;
end;

procedure TFormParaEditor.SettingToValue();
var
  i: integer;
begin
  Param_Value := Init_Value;
  for i := 0 to Length(Param_Record.settings) - 1 do
  begin
    Param_Value := Param_Value and (not Param_Record.settings[i].mask);
    if Param_Record.settings[i].use_checkbox then
    begin
      if ParaCheckArr[i].Checked then
      begin
        Param_Value := Param_Value or Param_Record.settings[i].checked;
      end
      else
      begin
        Param_Value := Param_Value or Param_Record.settings[i].unchecked;
      end;
    end
    else
    begin
      Param_Value := Param_Value or Param_Record.settings[i].choices[ParaComboArr[i].ItemIndex].value;
    end;
  end;
end;

procedure TFormParaEditor.ValueToSetting();
var
  i, j: integer;
  value: integer;
  found: boolean;
begin
  SettingParameter := TRUE;
  for i := 0 to Length(Param_Record.settings) - 1 do
  begin
    value := Param_Value and Param_Record.settings[i].mask;
    if Param_Record.settings[i].use_checkbox then
    begin
      if value = Param_Record.settings[i].checked then
      begin
        ParaCheckArr[i].Checked := TRUE;
      end
      else if value = Param_Record.settings[i].unchecked then
      begin
        ParaCheckArr[i].Checked := FALSE;
      end
      else
      begin
        // unrecognized value
        ParaEdtArr[i].Color := clRed;
      end;
    end
    else
    begin
      found := FALSE;
      for j := 0 to Length(Param_Record.settings[i].choices) - 1 do
      begin
        if value = Param_Record.settings[i].choices[j].value then
        begin
          ParaEdtArr[i].Color := clWindow;
          ParaComboArr[i].ItemIndex := j;
          found := TRUE;
          break;
        end;
      end;
      if not found then
      begin
        // there is an error
        ParaEdtArr[i].Color := clRed;
      end;
    end;
  end;
  SettingParameter := FALSE;
end;

procedure TFormParaEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  settings_num, choices_num: integer;
begin
  // create components according to Param_Record
  settings_num := Length(Param_Record.settings);
  SetLength(ParaEdtArr, settings_num);
  SetLength(ParaComboArr, settings_num);
  SetLength(ParaCheckArr, settings_num);

  i := TOP_MARGIN + BOTTOM_MARGIN + settings_num * (Y_MARGIN + ITEM_HEIGHT);
  ClientHeight := i + pnlButton.Height;
  pnlSettings.ClientHeight := i;
  i := LEFT_MARGIN + RIGHT_MARGIN + EDT_WIDTH + X_MARGIN + COMBO_WIDTH;
  ClientWidth := i;
  pnlSettings.ClientWidth := i;
  pnlButton.ClientWidth := i;
  UpdateTitle();

  // center buttons
  btnOK.Left := (pnlButton.Width div 2 - btnOK.Width) div 2;
  btnCancel.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnOK.Width) div 2;

  SettingParameter := FALSE;
  for i := 0 to settings_num - 1 do
  begin
    ParaEdtArr[i] := TEdit.Create(Self);
    ParaEdtArr[i].Parent := pnlSettings;
    ParaEdtArr[i].Top := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
    ParaEdtArr[i].Left := LEFT_MARGIN;
    ParaEdtArr[i].Width := EDT_WIDTH;
    ParaEdtArr[i].Height := ITEM_HEIGHT;
    ParaEdtArr[i].Text := Param_Record.settings[i].name;
    ParaEdtArr[i].Color := clWindow;
    ParaEdtArr[i].ReadOnly := TRUE;
    if Param_Record.settings[i].use_checkbox then
    begin
      ParaCheckArr[i] := TCheckBox.Create(Self);
      ParaCheckArr[i].Parent := pnlSettings;
      ParaCheckArr[i].Top := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
      ParaCheckArr[i].Left := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaCheckArr[i].Height := ITEM_HEIGHT;
      ParaCheckArr[i].OnChange := @SettingChange;
      ParaCheckArr[i].Tag := Param_Record.settings[i].mask;
      ParaCheckArr[i].Enabled := Param_Record.settings[i].enabled;
      ParaCheckArr[i].Caption := '';
      ParaCheckArr[i].Checked := FALSE;
    end
    else
    begin
      ParaComboArr[i] := TComboBox.Create(Self);
      ParaComboArr[i].Parent := pnlSettings;
      ParaComboArr[i].Top := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
      ParaComboArr[i].Left := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaComboArr[i].Width := COMBO_WIDTH;
      ParaComboArr[i].Height := ITEM_HEIGHT;
      ParaComboArr[i].OnChange := @SettingChange;
      ParaComboArr[i].Style := csDropDownList;
      ParaComboArr[i].Tag := Param_Record.settings[i].mask;
      ParaComboArr[i].Enabled := Param_Record.settings[i].enabled;
      ParaComboArr[i].Clear;
      choices_num := Length(Param_Record.settings[i].choices);
      for j := 0 to choices_num - 1 do
      begin
        ParaComboArr[i].Items.Add(Param_Record.settings[i].choices[j].text);
      end;
    end;
  end;
  ValueToSetting();
  UpdateShowing;
end;

function TFormParaEditor.GetIntegerParameter(line, para_name: string; var value: integer): boolean;
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

procedure TFormParaEditor.FreeRecord();
begin
  SetLength(Param_Record.warnings, 0);
  SetLength(Param_Record.settings, 0);
end;

procedure TFormParaEditor.ParseLine(line: string);
var
  i, j, num, dis: integer;
begin
  if Pos('warning: ', line) = 1 then
  begin
    i := Length(Param_Record.warnings) + 1;
    SetLength(Param_Record.warnings, i);
    GetIntegerParameter(line, 'mask', Param_Record.warnings[i - 1].mask);
    GetIntegerParameter(line, 'value', Param_Record.warnings[i - 1].value);
    GetStringParameter(line, 'msg', Param_Record.warnings[i - 1].msg);
  end
  else if Pos('setting: ', line) = 1 then
  begin
    i := Length(Param_Record.settings) + 1;
    SetLength(Param_Record.settings, i);
    SetLength(Param_Record.settings[i - 1].choices, 0);
    GetStringParameter(line, 'name', Param_Record.settings[i - 1].name);
    GetIntegerParameter(line, 'mask', Param_Record.settings[i - 1].mask);
    GetStringParameter(line, 'info', Param_Record.settings[i - 1].info);
    if GetIntegerParameter(line, 'checked', Param_Record.settings[i - 1].checked) then
    begin
      Param_Record.settings[i - 1].use_checkbox := TRUE;
    end;
    if GetIntegerParameter(line, 'unchecked', Param_Record.settings[i - 1].unchecked) then
    begin
      Param_Record.settings[i - 1].use_checkbox := TRUE;
    end;
    dis := 0;
    GetIntegerParameter(line, 'disabled', dis);
    if dis > 0 then
    begin
      Param_Record.settings[i - 1].enabled := FALSE;
    end
    else
    begin
      Param_Record.settings[i - 1].enabled := TRUE;
    end;
    num := 0;
    GetIntegerParameter(line, 'num_of_choices', num);
  end
  else if Pos('choice: ', line) = 1 then
  begin
    i := Length(Param_Record.settings);
    j := Length(Param_Record.settings[i - 1].choices) + 1;
    SetLength(Param_Record.settings[i - 1].choices, j);
    GetIntegerParameter(line, 'value', Param_Record.settings[i - 1].choices[j - 1].value);
    GetStringParameter(line, 'text', Param_Record.settings[i - 1].choices[j - 1].text);
  end;
end;

initialization
  {$I parameditor.lrs}

end.


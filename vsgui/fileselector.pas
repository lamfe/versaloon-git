unit fileselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, EditBtn;

type
  TTargetFile = record
    target: string;
    filename: string;
  end;

  { TFormFileSelector }

  TFormFileSelector = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    pnlButton: TPanel;
    procedure FileNameEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FileNameLabelArr: array of TLabel;
    FileNameEdtArr: array of TFileNameEdit;
  public
    { public declarations }
    procedure Reset();
    function GetFileNameByTargetName(target: string): string;
    procedure AddFileSetting(target, filename: string);
  end; 

var
  FormFileSelector: TFormFileSelector;

const
  LEFT_MARGIN: integer = 10;
  RIGHT_MARGIN: integer = 10;
  TOP_MARGIN: integer = 10;
  BOTTOM_MARGIN: integer = 10;
  X_MARGIN: integer = 4;
  Y_MARGIN: integer = 4;
  ITEM_HEIGHT: integer = 20;
  FILELABEL_WIDTH: integer = 100;
  FILEEDIT_WIDTH: integer = 400;

implementation

{ TFormFileSelector }

procedure TFormFileSelector.Reset();
var
  i: integer;
begin
  for i := 0 to Length(FileNameLabelArr) - 1 do
  begin
    if Assigned(FileNameLabelArr[i]) then
    begin
      FileNameLabelArr[i].Destroy;
    end;
  end;
  SetLength(FileNameLabelArr, 0);

  for i := 0 to Length(FileNameEdtArr) - 1 do
  begin
    if Assigned(FileNameEdtArr[i]) then
    begin
      FileNameEdtArr[i].Destroy;
    end;
  end;
  SetLength(FileNameEdtArr, 0);
end;

procedure TFormFileSelector.AddFileSetting(target, filename: string);
var
  i: integer;
  found: boolean;
  str: string;
begin
  found := FALSE;
  for i := 0 to Length(FileNameLabelArr) - 1 do
  begin
    if FileNameLabelArr[i].Caption = target then
    begin
      found := TRUE;
    end;
  end;

  if found then
  begin
    FileNameEdtArr[i].FileName := filename;
  end
  else
  begin
    SetLength(FileNameEdtArr, Length(FileNameEdtArr) + 1);
    SetLength(FileNameLabelArr, Length(FileNameLabelArr) + 1);
    i := Length(FileNameLabelArr) - 1;

    FileNameLabelArr[i] := TLabel.Create(Self);
    FileNameLabelArr[i].Parent := pnlMain;
    FileNameLabelArr[i].Caption := target;
    FileNameLabelArr[i].Top := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
    FileNameLabelArr[i].Left := LEFT_MARGIN;
    FileNameLabelArr[i].Width := FILELABEL_WIDTH;
    FileNameLabelArr[i].Height := ITEM_HEIGHT;
    FileNameLabelArr[i].Hint := target;
    FileNameLabelArr[i].ShowHint := TRUE;

    FileNameEdtArr[i] := TFileNameEdit.Create(Self);
    FileNameEdtArr[i].Parent := pnlMain;
    FileNameEdtArr[i].FileName := filename;
    FileNameEdtArr[i].Top := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
    FileNameEdtArr[i].Left := LEFT_MARGIN + X_MARGIN + FILELABEL_WIDTH;
    FileNameEdtArr[i].Width := FILEEDIT_WIDTH - FileNameEdtArr[i].ButtonWidth;
    FileNameEdtArr[i].Height := ITEM_HEIGHT;
    FileNameEdtArr[i].Filter := 'HEX File|*.hex|BIN File|*.bin';
    FileNameEdtArr[i].Hint := filename;
    FileNameEdtArr[i].ShowHint := TRUE;
    FileNameEdtArr[i].OnChange := @FileNameEditChange;
    FileNameEdtArr[i].OnEditingDone := @FileNameEditChange;

    str := ExtractFileExt(filename);
    if (LowerCase(str) = '.hex') or (str = '') then
    begin
      FileNameEdtArr[i].FilterIndex := 1;
    end
    else if LowerCase(str) = '.bin' then
    begin
      FileNameEdtArr[i].FilterIndex := 2;
    end;
  end;
end;

function TFormFileSelector.GetFileNameByTargetName(target: string): string;
var
  i: integer;
begin
  result := '';

  for i := 0 to Length(FileNameLabelArr) - 1 do
  begin
    if FileNameLabelArr[i].Caption = target then
    begin
      result := FileNameEdtArr[i].FileName;
    end;
  end;
end;

procedure TFormFileSelector.FormDestroy(Sender: TObject);
begin
  Reset();
end;

procedure TFormFileSelector.FileNameEditChange(Sender: TObject);
var
  str: string;
begin
  (Sender as TFileNameEdit).Hint := (Sender as TFileNameEdit).FileName;

  str := ExtractFileExt((Sender as TFileNameEdit).filename);
  if (LowerCase(str) = '.hex') or (str = '') then
  begin
    (Sender as TFileNameEdit).FilterIndex := 1;
  end
  else if LowerCase(str) = '.bin' then
  begin
    (Sender as TFileNameEdit).FilterIndex := 2;
  end;
end;

procedure TFormFileSelector.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    close;
  end;
end;

procedure TFormFileSelector.FormShow(Sender: TObject);
var
  int_tmp: integer;
begin
  int_tmp:= LEFT_MARGIN + RIGHT_MARGIN + FILELABEL_WIDTH + X_MARGIN + FILEEDIT_WIDTH;
  ClientWidth := int_tmp;
  ClientHeight := TOP_MARGIN + BOTTOM_MARGIN + Length(FileNameLabelArr) * (Y_MARGIN + ITEM_HEIGHT) + pnlButton.Height;
  pnlMain.Width := int_tmp;
  pnlButton.Width := int_tmp;
  // center buttons
  btnOK.Left := (pnlButton.Width div 2 - btnOK.Width) div 2;
  btnCancel.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnOK.Width) div 2;

  UpdateShowing;
end;

initialization
  {$I fileselector.lrs}

end.


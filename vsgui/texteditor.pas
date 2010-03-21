unit texteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFormTextEditor }

  TFormTextEditor = class(TForm)
    memoText: TMemo;
    btnSave: TButton;
    btnClose: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormTextEditor: TFormTextEditor;
  TextFileName: String;

implementation

{ TFormTextEditor }

procedure TFormTextEditor.FormShow(Sender: TObject);
var
  tmpStrList: TStringList;
  i: integer;
begin
  if not FileExistsUtf8(TextFileName) then
  begin
    Beep;
    MessageDlg('Error', TextFileName + ' not exists.', mtError, [mbOK], 0);
    Close;
    Exit;
  end;

  Caption := 'TextEditor: ' + TextFileName;
  memoText.Hint := TextFileName;

  tmpStrList := TStringList.Create;
  tmpStrList.LoadFromFile(TextFileName);

  memoText.Lines.Clear;
  for i := 0 to tmpStrList.Count - 1 do
  begin
    memoText.Lines.Add(tmpStrList.Strings[i]);
  end;

  tmpStrList.Destroy;
end;

procedure TFormTextEditor.btnSaveClick(Sender: TObject);
begin
  memoText.Lines.SaveToFile(TextFileName);
end;

initialization
  {$I texteditor.lrs}

end.


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
begin
  if not FileExistsUtf8(TextFileName) then
  begin
    Beep;
    MessageDlg('Error', TextFileName + ' not exists.', mtError, [mbOK], 0);
    Close;
    Exit;
  end;

  memoText.Lines.Clear;
  memoText.Lines.LoadFromFile(TextFileName);
  memoText.Hint := TextFileName;
  Caption := 'TextEditor: ' + TextFileName;
end;

procedure TFormTextEditor.btnSaveClick(Sender: TObject);
begin
  memoText.Lines.SaveToFile(TextFileName);
end;

initialization
  {$I texteditor.lrs}

end.


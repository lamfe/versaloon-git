unit hexeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    pnlHexEditorCommand: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormHexEditor: TFormHexEditor;
  CurrentAddr: integer;
const
  COLNUM = 16;
  ROWNUM = 32;

implementation

procedure TFormHexEditor.FormCreate(Sender: TObject);
begin

end;

procedure TFormHexEditor.FormShow(Sender: TObject);
begin
end;

initialization
  {$I hexeditor.lrs}

end.


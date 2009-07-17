unit checkeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormCheckEditor }

  TFormCheckEditor = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormCheckEditor: TFormCheckEditor;

implementation

initialization
  {$I checkeditor.lrs}

end.


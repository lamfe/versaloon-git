unit inputdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type
  TInputType = (itNumeric, itLiteral);
  TStrCase = (scBoth, scUpper, scLower);
  TNumRadix = (nrBinary = 2, nrDecimal = 10, nrHexadecimal = 16);

  { TFormInputDialog }

  TFormInputDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtInput: TEdit;
    lblPrefix: TLabel;
    procedure edtInputKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function GetString: string;
    function GetNumber: integer;
    InputType: TInputType;

    CommonMaxLength: integer;
    CommonPrefix: string;
    CommonCase: TStrCase;
    NumMin: Int64;
    NumMax: Int64;
    NumRadix: TNumRadix;
  end;

  function StrToIntRadix(sData: string; radix: integer): integer;
  function IntToStrRadix(aData, radix: Integer): String;

var
  FormInputDialog: TFormInputDialog;

const
  HEX_PARSE_STR: string = '0123456789ABCDEF';
  BYTELEN_ACCORDING_TO_RADIX: array[2..16] of integer = (8, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2);

implementation

function IntToStrRadix(aData, radix: Integer): String;
var
  t: Integer;
begin
  Result := '';
  if (radix = 0) or (radix > 16) then
  begin
    exit;
  end;

  repeat
    t := aData mod radix;
    if t < 10 then
      Result := InttoStr(t)+Result
    else
      Result := InttoHex(t, 1)+Result;
    aData := aData div radix;
  until (aData = 0);
end;

function StrToIntRadix(sData: string; radix: integer): integer;
var
  i, r: integer;
begin
  result := 0;
  sData := UpperCase(sData);
  if (radix < 2) or (radix > 16)
     or (Length(sData) > (BYTELEN_ACCORDING_TO_RADIX[radix] * 4)) then
  begin
    exit;
  end;

  for i := 1 to Length(sData) do
  begin
    r := Pos(sData[i], HEX_PARSE_STR);
    if (r < 1) or (r > radix) then
    begin
      exit;
    end;
  end;

  r := 1;
  for i := Length(sData) downto 1 do
  begin
    Inc(result, r * (Pos(sData[i], HEX_PARSE_STR) - 1));
    r := r * radix;
  end;
end;

procedure TFormInputDialog.edtInputKeyPress(Sender: TObject; var Key: char);
var
  value, radix: integer;
  key_upper: char;
begin
  if Key = char(8) then
  begin
    // backspace is OK
    exit;
  end
  else if CommonCase <> scBoth then
  begin
    if CommonCase = scUpper then
    begin
      Key := AnsiStrUpper(@Key)^;
    end
    else
    begin
      Key := AnsiStrLower(@Key)^;
    end;
  end;

  case InputType of
  itNumeric:
    begin
      // process radix
      radix := Integer(NumRadix);
      key_upper := AnsiStrUpper(@Key)^;
      value := Pos(key_upper, HEX_PARSE_STR);
      if (value > radix) or (value = 0) then
      begin
        Key := char(0);
        exit;
      end;
      if not ((NumMax = 0) and (NumMin = 0)) then
      begin
        // process Min Max value
      end;
    end;
  itLiteral:
    begin

    end;
  end;
end;

procedure TFormInputDialog.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    close;
  end;
end;

procedure TFormInputDialog.FormShow(Sender: TObject);
begin
  lblPrefix.Caption := CommonPrefix;
  edtInput.MaxLength := CommonMaxLength;
  edtInput.Text := '';
end;

function TFormInputDialog.GetString: string;
begin
  result := edtInput.Text;
end;

function TFormInputDialog.GetNumber: integer;
var
  str_result: string;
begin
  str_result := edtInput.Text;

  result := StrToIntRadix(str_result, Integer(NumRadix));
end;

initialization
  {$I inputdialog.lrs}

end.


unit cli_caller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms;

type
  TCLI_Callback = function(line: string): boolean of Object;
  TCLI_Caller = class(TObject)
    procedure Take();
    procedure UnTake();
    procedure SetApplication(app: string);
    function GetApplication(): string;
    procedure SetDelimiter(deli: char);
    procedure AddParameter(para: string);
    procedure AddParametersString(para: string);
    procedure RemoveAllParameters();
    procedure Run(callback: TCLI_Callback; bView: boolean; bTimeout: boolean);
    procedure Stop();
    function IsRunning(): boolean;
  private
    { private declarations }
    application: string;
    parameter: string;
    delimiter: char;
    P: TProcess;
    running: boolean;
    taken: boolean;
  public
    { public declarations }
  end;

const
  BUF_INC = 1024;

implementation

procedure TCLI_Caller.SetApplication(app: string);
begin
  application := app;
end;

function TCLI_Caller.GetApplication(): string;
begin
  result := application;
end;

procedure TCLI_Caller.SetDelimiter(deli: char);
begin
  delimiter := deli;
end;

procedure TCLI_Caller.AddParameter(para: string);
begin
  parameter := parameter + ' ' + delimiter + para;
end;

procedure TCLI_Caller.AddParametersString(para: string);
begin
  parameter := parameter + ' ' + para;
end;

procedure TCLI_Caller.RemoveAllParameters();
begin
  parameter := '';
end;

procedure TCLI_Caller.Take;
begin
  taken := TRUE;
end;

procedure TCLI_Caller.UnTake;
begin
  taken := FALSE;
end;

function TCLI_Caller.IsRunning(): boolean;
begin
  if (P <> nil) and ((running and P.Running) or taken) then
  begin
    result := TRUE;
  end
  else
  begin
    result := FALSE;
  end;
end;

procedure TCLI_Caller.Stop();
begin
  if (P <> nil) and P.Running then
  begin
    P.Terminate(0);
  end;
end;

procedure TCLI_Caller.Run(callback: TCLI_Callback; bView: boolean; bTimeout: boolean);
var
  n, BytesRead: LongInt;
  dly: integer;
  buff: string;
  i: integer;
begin
  buff := '';
  dly := 0;

  BytesRead := 0;
  P := TProcess.Create(nil);
{$ifdef MSWINDOWS}
  P.CommandLine := '"' + application + '"' + Utf8ToAnsi(parameter);
{$else}
  P.CommandLine := application + parameter;
{$endif}
  if callback <> nil then
  begin
{$ifdef MSWINDOWS}
    callback(AnsiToUtf8(P.CommandLine));
{$else}
    callback(P.CommandLine);
{$endif}
  end;

  if not bView then
  begin
    P.ShowWindow := swoHIDE;
  end;
  P.Options := P.Options + [poUsePipes, poStderrToOutPut] - [poWaitOnExit];

try
  running := TRUE;
  P.Execute;

  while P.Running do
  begin
    // make sure we have room
    SetLength(buff, BytesRead + BUF_INC);

    // try reading it
    if P.Output.NumBytesAvailable > 0 then
    begin
      dly := 0;
      n := P.Output.Read(buff[BytesRead + 1], BUF_INC);
      if n > 0 then
      begin
        Inc(BytesRead, n);

        if callback <> nil then
        begin
          SetLength(buff, BytesRead);

          i := 0;
          while (BytesRead > 0) and (i <= BytesRead) do
          begin
            if buff[i] = #10 then
            begin
{$ifdef MSWINDOWS}
              callback(AnsiToUtf8(Copy(buff, 1, i - 1)));
{$else}
              callback(Copy(buff, 1, i - 1));
{$endif}
              Delete(buff, 1, i);
              Dec(BytesRead, i);
              i := 0;
            end
            else
            begin
              Inc(i);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // no data, wait 10 ms
      dly := dly + 1;
//      if bTimeout and (dly > 10) then
//      begin
        // Timeout
//        P.Terminate(0);
//      end;
      Sleep(10);
    end;
    Forms.Application.ProcessMessages;
  end;

  // read last part, process finished
  repeat
    // make sure we have room
    SetLength(buff, BytesRead + BUF_INC);

    // try reading it
    n := P.Output.Read(buff[BytesRead + 1], BUF_INC);
    if n > 0 then
    begin
      Inc(BytesRead, n);
      
      if callback <> nil then
      begin
        SetLength(buff, BytesRead);
        
        i := 0;
        while (BytesRead > 0) and (i <= BytesRead) do
        begin
          if buff[i] = #10 then
          begin
{$ifdef MSWINDOWS}
            callback(AnsiToUtf8(Copy(buff, 1, i - 1)));
{$else}
            callback(Copy(buff, 1, i - 1));
{$endif}
            Delete(buff, 1, i);
            Dec(BytesRead, i);
            i := 0;
          end
          else
          begin
            Inc(i);
          end;
        end;
      end;
    end;
  until n <= 0;
finally
  FreeAndNil(P);
end;
  running := FALSE;
  taken := FALSE;
end;

end.


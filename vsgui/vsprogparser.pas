unit vsprogparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, parameditor;

type

  TLogOutputFunc    = procedure(line: string) of object;
  TProgressInfoType = (liStartSection, liStep, liEndSection);
  TLogProgressFunc  = procedure(ProgressInfo: TProgressInfoType; info: string) of object;
  TParserFunc = function(line: string): boolean of object;

  TVSProg_Info = record
    TargetVoltage: integer;
  end;

  { TVSProg_Parser }

  TVSProg_Parser = class(TObject)
    procedure Prepare();
    procedure SetLogOutputFunc(func: TLogOutputFunc);
    procedure SetLogProgressFunc(func: TLogProgressFunc);
    procedure SetCallbackFunc(func: TParserFunc);

    function CommonParser(line: string): boolean;
    function ErrorParser(line: string): boolean;
    function VersionParser(line: string): boolean;
    function TargetVoltageParser(line: string): boolean;
    function OperationParser(line: string): boolean;
    // used for parsing fuse/lock/calibration settings
    function SettingTargetInfoParser(line: string): boolean;
    // used for parsing flash/eeprom information
    function MemoryTargetInfoParser(line: string): boolean;
    function FuseDataParser(line: string): boolean;
    function LockDataParser(line: string): boolean;
    function CaliDataParser(line: string): boolean;
    function SupportParser(line: string): boolean;
    function AutoDetectParser(line: string): boolean;
  private
    FLogOutputEnable: boolean;
    FLogOutputFunc: TLogOutputFunc;
    FLogProgressEnable: boolean;
    FLogProgressFunc: TLogProgressFunc;
    FCallbackEnable: boolean;
    FCallbackFunc:  TParserFunc;
    FParserFunc:    TParserFunc;
    FFatalError:    boolean;
    FErrorStr:      string;
    FResultStrings: TStringList;
    function ParseTargetData(line, target: string; var result_str: string): boolean;
  public
    constructor Create;
    destructor Destroy;
    property ParserFunc: TParserFunc Read FParserFunc Write FParserFunc;
    property LogOutputEnable: boolean Read FLogOutputEnable Write FLogOutputEnable;
    property LogOutputFunc: TLogOutputFunc Read FLogOutputFunc Write SetLogOutputFunc;
    property LogProgressEnable: boolean Read FLogProgressEnable Write FLogProgressEnable;
    property LogProgressFunc: TLogProgressFunc
      Read FLogProgressFunc Write SetLogProgressFunc;
    property CallbackEnable: boolean Read FCallbackEnable Write FCallbackEnable;
    property CallbackFunc: TParserFunc Read FCallbackFunc Write SetCallbackFunc;
    property HasError: boolean Read FFatalError;
    property ErrorStr: string Read FErrorStr;
    property ResultStrings: TStringList Read FResultStrings;
  end;

implementation

{ TVSProg_Parser }

constructor TVSProg_Parser.Create;
begin
  inherited Create;
  FResultStrings     := TStringList.Create;
  FLogProgressEnable := False;
  FLogOutputEnable   := False;
  FCallbackEnable    := False;
end;

destructor TVSProg_Parser.Destroy;
begin
  inherited Destroy;
  FResultStrings.Destroy;
end;

procedure TVSProg_Parser.SetLogOutputFunc(func: TLogOutputFunc);
begin
  FLogOutputFunc   := func;
  FLogOutputEnable := func <> nil;
end;

procedure TVSProg_Parser.SetLogProgressFunc(func: TLogProgressFunc);
begin
  FLogProgressFunc   := func;
  FLogProgressEnable := func <> nil;
end;

procedure TVSProg_Parser.SetCallbackFunc(func: TParserFunc);
begin
  FCallbackFunc   := func;
  FCallbackEnable := func <> nil;
end;

function TVSProg_Parser.ParseTargetData(line, target: string;
  var result_str: string): boolean;
var
  pos_start: integer;
begin
  Result     := False;
  result_str := '';
  pos_start  := Pos(target + ' read is ', line);
  if pos_start > 0 then
  begin
    result_str := Copy(line, pos_start + Length(target + ' read is '),
      Length(line) - pos_start);
    Result     := True;
  end;
end;

procedure TVSProg_Parser.Prepare;
begin
  FResultStrings.Clear;
  FFatalError := False;
  FErrorStr := '';
end;

function TVSProg_Parser.CommonParser(line: string): boolean;
begin
  if FLogOutputEnable and Assigned(FLogOutputFunc) then
  begin
    FLogOutputFunc(line);
  end;

  if not ErrorParser(line) then
  begin
    Result := False;
    exit;
  end;
  Result := True;

  if Assigned(FParserFunc) then
  begin
    Result := FParserFunc(line);
  end;

  if (Result <> False) and FCallbackEnable and Assigned(FCallbackFunc) then
  begin
    Result := FCallbackFunc(line);
  end;
end;

function TVSProg_Parser.ErrorParser(line: string): boolean;
begin
  Result := True;
  if ((Pos('Error:', line) = 1) or (Pos('/****Bug****/:', line) = 1) or
    (Pos('fail', line) <> 0)) and not FFatalError then
  begin
    FFatalError := True;
    FErrorStr   := line;
    Result      := False;
  end;
end;

function TVSProg_Parser.VersionParser(line: string): boolean;
begin
  Result := True;
  if Pos('vsprog', LowerCase(line)) = 1 then
  begin
    FResultStrings.Add(line);
  end;
end;

function TVSProg_Parser.TargetVoltageParser(line: string): boolean;
var
  pos_start: integer;
begin
  Result    := True;
  pos_start := Pos('Target runs at ', line);
  if pos_start > 0 then
  begin
    Inc(pos_start, Length('Target runs at '));
    FResultStrings.Add(IntToStr(
      Trunc(StrToFloat(Copy(line, pos_start, Length(line) - pos_start - 1)) * 1000)));
  end;
end;

function TVSProg_Parser.OperationParser(line: string): boolean;
var
  i: integer;
const
  operating: boolean = False;
begin
  Result := True;

  if ((Pos('writing', line) = 1) or (Pos('reading', line) = 1) or
    (Pos('verifying', line) = 1) or (Pos('erasing', line) = 1) or
    (Pos('checking', line) = 1) or (Pos('executing', line) = 1)) and
    (Length(line) > 9) then
  begin
    operating := True;
    if FLogProgressEnable and Assigned(FLogProgressFunc) then
    begin
      FLogProgressFunc(liStartSection, Copy(line, 1, Length(line) - 8));
    end;
  end;

  i := Pos('| ', line);
  if operating and (i > 0) then
  begin
    if FLogProgressEnable and Assigned(FLogProgressFunc) then
    begin
      FLogProgressFunc(liEndSection, Copy(line, i + 1, Length(line) - i));
    end;
    operating := False;
  end;

  if Pos('=', line) = 1 then
  begin
    i := 1;
    while line[i] = PChar('=') do
    begin
      if FLogProgressEnable and Assigned(FLogProgressFunc) then
      begin
        FLogProgressFunc(liStep, '=');
      end;
      Inc(i);
    end;
  end;
end;

function TVSProg_Parser.SettingTargetInfoParser(line: string): boolean;
begin
  Result := True;
  if (Pos('setting: ', line) = 1) or (Pos('warning: ', line) = 1) or
    (Pos('choice: ', line) = 1) then
  begin
    FResultStrings.Add(line);
  end;
end;

function TVSProg_Parser.MemoryTargetInfoParser(line: string): boolean;
begin
  line   := line;
  Result := True;
end;

function TVSProg_Parser.FuseDataParser(line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  if ParseTargetData(line, 'fuse', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.LockDataParser(line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  if ParseTargetData(line, 'lock', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.CaliDataParser(line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  if ParseTargetData(line, 'calibration', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.SupportParser(line: string): boolean;
begin
  line   := line;
  Result := True;
end;

function TVSProg_Parser.AutoDetectParser(line: string): boolean;
var
  pos_start: integer;
begin
  Result    := True;
  pos_start := Pos(' found', line);
  if pos_start >= 8 then
  begin
    // chip found
    FResultStrings.Add(Copy(line, 9, pos_start - 9));
  end;
end;

end.


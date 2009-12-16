unit hexeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Grids, ActnList, parameditor, inputdialog, Contnrs, Math;

type

  { TMemList TMemInfo }

  TMemInfo = class(TObject)
  private
    FStartAddr: cardinal;
    FByteSize:  cardinal;
    function GetTail: cardinal;
  public
    constructor Create(aStartAddr, aByteSize: cardinal);
    property StartAddr: cardinal Read FStartAddr Write FStartAddr;
    property ByteSize: cardinal Read FByteSize Write FByteSize;
    property Head: cardinal Read FStartAddr;
    property Tail: cardinal Read GetTail;
  end;

  TMemList = class(TObjectList)
  private
    function GetMemInfoItem(Index: integer): TMemInfo;
    procedure SetMemInfoItem(Index: integer; aMemInfoObject: TMemInfo);
  public
    constructor Create;
    function Add(aStartAddr, aByteSize: cardinal): integer;
    function Add(aMemInfo: TMemInfo): integer;
    function GetIndexByAddr(aAddr: cardinal): integer;
    property MemInfoItems[Index: integer]: TMemInfo
      Read GetMemInfoItem Write SetMemInfoItem; default;
  end;

  { TFileParser }

  TReadFileFunc = function(hFile: TFileStream; var buffer: array of byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
  TValidateFileFunc = function(hFile: TFileStream; var buffer: array of byte;
    ChangeList: TMemList; default_byte: byte; bytesize, start_addr: cardinal;
    seg_offset, addr_offset: int64): boolean;

  TFileParser = record
    ext:      string;
    ReadFile: TReadFileFunc;
    ValidateFile: TValidateFileFunc;
  end;

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    actGoto:   TAction;
    alHotKey:  TActionList;
    btnSave:   TButton;
    btnExit:   TButton;
    lblMeasureSize: TLabel;
    pnlData:   TPanel;
    pnlButton: TPanel;
    sgData:    TStringGrid;
    timerInitSize: TTimer;
    procedure actGotoExecute(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sgDataKeyPress(Sender: TObject; var Key: char);
    procedure sgDataPrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
    procedure sgDataSelectCell(Sender: TObject; aCol, aRow: integer;
      var CanSelect: boolean);
    procedure sgDataTopLeftChanged(Sender: TObject);
    procedure timerInitSizeTimer(Sender: TObject);
    procedure DataBuffChangeRequire(addr: integer; aValue: byte);
  private
    { private declarations }
    DataBuff: array of byte;
    DataBuffChangeList: TMemList;
    RowDirty: array of boolean;
    StrGridInited: boolean;
    CurCellRow: integer;
    CurCellCol: integer;
    CurCellPos: integer;
    CurCellAddress: integer;
    CurCellAddressValid: boolean;
    CurFileParserIndex: integer;
    hFile: TFileStream;
    ChangedFont: TFont;
    procedure SetGridCanvasFont(aFont: TFont);
  public
    { public declarations }
    FileName:    string;
    Target:      string;
    SegOffset:   int64;
    AddressOffset: int64;
    StartAddress: cardinal;
    DataByteSize: cardinal;
    DefaultData: byte;
  end;

  THexFileLineInfo = record
    Addr:     cardinal;
    ByteSize: cardinal;
    DataType: byte;
    DataOffset: byte;
    FileLineLength: cardinal;
    EmptyLeadingByteLength: cardinal;
  end;

function ReadBinFile(hFile: TFileStream; var buffer: array of byte;
  bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
function ValidateBinFile(hFile: TFileStream; var buffer: array of byte;
  ChangeList: TMemList; default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function ReadHexFile(hFile: TFileStream; var buffer: array of byte;
  bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
function ValidateHexFile(hFile: TFileStream; var buffer: array of byte;
  ChangeList: TMemList; default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;

var
  FormHexEditor: TFormHexEditor;

const
  FileParser: array[0..1] of TFileParser =
    (
    (ext: '.bin'; ReadFile: @ReadBinFile; ValidateFile: @ValidateBinFile; ),
    (ext: '.hex'; ReadFile: @ReadHexFile; ValidateFile: @ValidateHexFile; )
    );
  BYTES_IN_ROW: integer  = 16;
  DIVIDER_WIDTH: integer = 20;



implementation

function ReadBinFile(hFile: TFileStream; var buffer: array of byte;
  bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
begin
  seg_offset := seg_offset;
  start_addr := start_addr;

  hFile.Position := addr_offset;
  hFile.Read(buffer[0], bytesize);
  Result := True;
end;

function ValidateBinFile(hFile: TFileStream; var buffer: array of byte;
  ChangeList: TMemList; default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  i: integer;
begin
  seg_offset := seg_offset;
  default_byte := default_byte;
  bytesize := bytesize;
  start_addr := start_addr;
  Result := False;

  for i := 0 to ChangeList.Count - 1 do
  begin
    if (addr_offset + ChangeList.MemInfoItems[i].StartAddr + 1) > hFile.Size then
    begin
      continue;
    end
    else if (addr_offset + ChangeList.MemInfoItems[i].StartAddr +
      ChangeList.MemInfoItems[i].ByteSize) > hFile.Size then
    begin
      ChangeList.MemInfoItems[i].ByteSize :=
        hFile.Size - addr_offset - ChangeList.MemInfoItems[i].StartAddr;
    end;

    hFile.Position := addr_offset + ChangeList.MemInfoItems[i].StartAddr;
    hFile.Write(buffer[ChangeList.MemInfoItems[i].StartAddr],
      ChangeList.MemInfoItems[i].ByteSize);
  end;
  Result := True;
end;

function IgnoreEmptyLine(hFile: TFileStream; var lastchar: char;
  var haslastchar: boolean): cardinal;
var
  ch: char;
begin
  Result      := 0;
  haslastchar := False;
  repeat
    if hFile.Read(ch, 1) <> 1 then
    begin
      exit;
    end;
    Inc(Result);
  until (ch <> char(10)) and (ch <> char(13));
  lastchar    := ch;
  haslastchar := True;
end;

function ReadHexFileLine(hFile: TFileStream; var buff: array of byte;
  var LineInfo: THexFileLineInfo): boolean;
var
  ch:   char;
  checksum: byte;
  line: string;
  i, j: integer;
  headread: boolean;
  FileLineLength: cardinal;
begin
  Result := False;

  FileLineLength := IgnoreEmptyLine(hFile, ch, headread);
  if (FileLineLength = 0) or (not headread) then
  begin
    Result := True;
    LineInfo.EmptyLeadingByteLength := FileLineLength;
    LineInfo.FileLineLength := 0;
    exit;
  end;
  LineInfo.EmptyLeadingByteLength := FileLineLength - 1;

  // first char MUST be :
  if (ch <> ':') then
  begin
    exit;
  end;

  // read line
  line := '';
  repeat
    if (hFile.Read(ch, 1) <> 1) then
    begin
      break;
    end
    else if (ch = char(10)) or (ch = char(13)) then
    begin
      Inc(FileLineLength);
      break;
    end
    else
    begin
      Inc(FileLineLength);
      line := line + ch;
    end;
  until False;
  if (Length(line) < 10) or ((Length(line) mod 2) = 1) then
  begin
    exit;
  end;
  i := 0;
  checksum := 0;
  while i < Length(line) do
  begin
    buff[i div 2] := byte(StrToIntRadix(Copy(line, i + 1, 2), 16));
    Inc(checksum, buff[i div 2]);
    Inc(i, 2);
  end;
  i := i div 2;
  // validity check
  if ((buff[0] + 5) <> i) or (checksum <> 0) then
  begin
    exit;
  end;

  LineInfo.ByteSize := buff[0];
  LineInfo.Addr := (buff[1] shl 8) + buff[2];
  LineInfo.DataOffset := 4;
  LineInfo.DataType := buff[3];
  LineInfo.FileLineLength := FileLineLength;
  Result := True;
end;

function ReadHexFile(hFile: TFileStream; var buffer: array of byte;
  bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
var
  buff:     array[0 .. 260] of byte;
  seg_addr, ext_addr, data_addr: cardinal;
  LineInfo: THexFileLineInfo;
begin
  seg_addr := 0;
  ext_addr := 0;
  hFile.Position := 0;
  Result   := False;

  while True do
  begin
    if not ReadHexFileLine(hFile, buff[0], LineInfo) then
    begin
      exit;
    end;
    if LineInfo.FileLineLength = 0 then
    begin
      Result := True;
      exit;
    end;

    // process data
    case LineInfo.DataType of
      0://htData
      begin
        data_addr := ext_addr + LineInfo.Addr;
        if not ((seg_addr <> seg_offset) or (data_addr < start_addr) or
          (data_addr < (addr_offset + start_addr))) then
        begin
          Dec(data_addr, start_addr);
          if (LineInfo.ByteSize + data_addr) > bytesize then
          begin
            if data_addr > bytesize then
            begin
              LineInfo.ByteSize := 0;
            end
            else
            begin
              LineInfo.ByteSize := bytesize - data_addr;
            end;
          end;
        end;
        Move(buff[4], buffer[data_addr - addr_offset], LineInfo.ByteSize);
      end;
      1://htEOF
      begin
        Result := True;
        exit;
      end;
      2://htSegAddr
      begin
        if LineInfo.ByteSize <> 2 then
        begin
          exit;
        end;
        seg_addr := (buff[4] shl 8) + buff[5];
      end;
      4://htExtAddr
      begin
        if LineInfo.ByteSize <> 2 then
        begin
          exit;
        end;
        ext_addr := (buff[4] shl 24) + (buff[5] shl 16);
      end;
    end;
  end;
end;

function ReadHexFileLineByAddr(hFile: TFileStream; var buff: array of byte;
  addr, seg: cardinal; var LineInfo: THexFileLineInfo): int64;
var
  seg_addr, ext_addr, data_addr: cardinal;
  FileLinePos: int64;
begin
  seg_addr    := 0;
  ext_addr    := 0;
  hFile.Position := 0;
  FilelinePos := 0;

  while True do
  begin
    if (not ReadHexFileLine(hFile, buff[0], LineInfo)) or
      (LineInfo.FileLineLength = 0) then
    begin
      Result := -1;
    end;

    case LineInfo.DataType of
      0://htData
      begin
        data_addr := ext_addr + LineInfo.Addr;
        if not ((seg_addr <> seg) or (data_addr > addr) or
          (addr >= (data_addr + LineInfo.ByteSize))) then
        begin
          LineInfo.Addr := addr;
          LineInfo.DataOffset := LineInfo.DataOffset + (addr - data_addr);
          LineInfo.ByteSize := LineInfo.ByteSize - (addr - data_addr);
          Result := FilelinePos + LineInfo.EmptyLeadingByteLength;
          exit;
        end;
      end;
      1://htEOF
      begin
        Result := -1;
        exit;
      end;
      2://htSegAddr
      begin
        if LineInfo.ByteSize <> 2 then
        begin
          exit;
        end;
        seg_addr := (buff[4] shl 8) + buff[5];
      end;
      4://htExtAddr
      begin
        if LineInfo.ByteSize <> 2 then
        begin
          exit;
        end;
        ext_addr := (buff[4] shl 24) + (buff[5] shl 16);
      end;
    end;
    Inc(FileLinePos, LineInfo.FileLineLength);
  end;
end;

function ValidateHexFile(hFile: TFileStream; var buffer: array of byte;
  ChangeList: TMemList; default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  i, j:     integer;
  checksum: byte;
  headread: boolean;
  cur_write_len: cardinal;
  buff:     array[0 .. 260] of byte;
  seg_addr, ext_addr, data_addr: cardinal;
  FileLinePos: int64;
  LineInfo: THexFileLineInfo;
  str_tmp:  string;
begin
  for i := 0 to ChangeList.Count - 1 do
  begin
    while ChangeList.MemInfoItems[i].ByteSize > 0 do
    begin
      FileLinePos := ReadHexFileLineByAddr(hFile, buff[0], start_addr +
        addr_offset + ChangeList.MemInfoItems[i].StartAddr, seg_offset, LineInfo);

      if FileLinePos >= 0 then
      begin
        // found
        cur_write_len := Min(LineInfo.ByteSize, ChangeList.MemInfoItems[i].ByteSize);
        Move(buffer[LineInfo.Addr - start_addr], buff[LineInfo.DataOffset],
          cur_write_len);

        hFile.Position := FileLinePos + 1 + 2 * LineInfo.DataOffset;
        for j := LineInfo.DataOffset to LineInfo.DataOffset + cur_write_len - 1 do
        begin
          str_tmp := IntToHex(buff[j], 2);
          hFile.Write(str_tmp[1], 2);
        end;
        // write checksum
        checksum := 0;
        for j := 0 to LineInfo.DataOffset + LineInfo.ByteSize - 1 do
        begin
          Inc(checksum, buff[j]);
        end;
        checksum := $100 - checksum;
        str_tmp  := IntToHex(checksum, 2);
        hFile.Position := FileLinePos + 1 + 2 * (LineInfo.DataOffset +
          LineInfo.ByteSize);
        hFile.Write(str_tmp[1], 2);

        ChangeList.MemInfoItems[i].ByteSize :=
          ChangeList.MemInfoItems[i].ByteSize - cur_write_len;
        ChangeList.MemInfoItems[i].StartAddr :=
          ChangeList.MemInfoItems[i].StartAddr + cur_write_len;
      end
      else
      begin
        // no expanding hex file
        ChangeList.MemInfoItems[i].ByteSize := 0;
      end;
    end;
  end;
  Result := True;
end;

{ TMemInfo }

constructor TMemInfo.Create(aStartAddr, aByteSize: cardinal);
begin
  inherited Create;

  StartAddr := aStartAddr;
  ByteSize  := aByteSize;
end;

function TMemInfo.GetTail: cardinal;
begin
  if (StartAddr = 0) and (ByteSize = 0) then
  begin
    Result := 0;
  end
  else
  begin
    Result := StartAddr + ByteSize - 1;
  end;
end;

{ TMemList }

procedure TMemList.SetMemInfoItem(Index: integer; aMemInfoObject: TMemInfo);
begin
  Put(Index, Pointer(aMemInfoObject));
end;

function TMemList.GetMemInfoItem(Index: integer): TMemInfo;
begin
  Result := TMemInfo(inherited Get(Index));
end;

function TMemList.GetIndexByAddr(aAddr: cardinal): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (MemInfoItems[i].StartAddr <= aAddr) and
      ((MemInfoItems[i].StartAddr + MemInfoItems[i].ByteSize) > aAddr) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TMemList.Add(aStartAddr, aByteSize: cardinal): integer;
var
  i:     integer;
  aTail: cardinal;
  found: boolean;
begin
  if aByteSize = 0 then
  begin
    Result := -1;
    exit;
  end;

  aTail := aStartAddr + aByteSize - 1;
  // find a location to insert or merge
  found := False;
  for i := 0 to Count - 1 do
  begin
    if aStartAddr <= (MemInfoItems[i].Tail + 1) then
    begin
      found := True;
      break;
    end;
  end;
  if not found then
  begin
    // add last
    Result := inherited Add(TMemInfo.Create(aStartAddr, aByteSize));
    exit;
  end;
  Result := i;
  // merge or insert
  if ((aTail + 1) < MemInfoItems[Result].Head) then
  begin
    // insert before
    Insert(i, TmemInfo.Create(aStartAddr, aByteSize));
  end
  else
  begin
    // merge
    MemInfoItems[Result].StartAddr := Min(MemInfoItems[Result].StartAddr, aStartAddr);
    MemInfoItems[Result].ByteSize :=
      1 + Max(MemInfoItems[Result].Tail, aTail) - MemInfoItems[Result].StartAddr;
    // try merge the descendant
    i := i + 1;
    while i < Count do
    begin
      if (MemInfoItems[Result].Tail + 1) < MemInfoItems[i].Head then
      begin
        break;
      end;

      // merge current
      MemInfoItems[Result].ByteSize :=
        1 + Max(MemInfoItems[Result].Tail, MemInfoItems[i].Tail) -
        MemInfoItems[Result].StartAddr;
      Delete(i);

      Inc(i);
    end;
  end;
end;

function TMemList.Add(aMemInfo: TMemInfo): integer;
begin
  Result := Add(aMemInfo.StartAddr, aMemInfo.ByteSize);
end;

constructor TMemList.Create;
begin
  inherited Create(True);
end;

{ TFormHexEditor }

procedure TFormHexEditor.DataBuffChangeRequire(addr: integer; aValue: byte);
begin
  if DataBuff[addr] <> aValue then
  begin
    DataBuff[addr] := aValue;
    DataBuffChangeList.Add(addr, 1);
  end;
end;

procedure TFormHexEditor.FormDestroy(Sender: TObject);
begin
  SetLength(DataBuff, 0);
  SetLength(RowDirty, 0);
end;

procedure TFormHexEditor.FormCreate(Sender: TObject);
begin
  ChangedFont := TFont.Create;
  ChangedFont := Font;
  ChangedFont.Color := clRed;
end;

procedure TFormHexEditor.btnSaveClick(Sender: TObject);
var
  i: integer;
  start, Lines: cardinal;
begin
  if (CurFileParserIndex >= Length(FileParser)) then
  begin
    exit;
  end;

  if (FileParser[CurFileParserIndex].ValidateFile <> nil) and
    (not FileParser[CurFileParserIndex].ValidateFile(hFile, DataBuff[0],
    DataBuffChangeList, DefaultData, DataByteSize, StartAddress,
    SegOffset, AddressOffset)) then
  begin
    MessageDlg('Error', 'fail to write ' + FileName + '.', mtError, [mbOK], 0);
    exit;
  end;

  // Dirty corresponding lines
  for i := 0 to DataBuffChangeList.Count - 1 do
  begin
    start := DataBuffChangeList.MemInfoItems[i].Head div BYTES_IN_ROW;
    Lines := DataBuffChangeList.MemInfoItems[i].tail div BYTES_IN_ROW;
    Dec(Lines, start);
    FillChar(RowDirty[start], Lines, True);
  end;
  DataBuffChangeList.Clear;
  // update dirty color
  sgData.Invalidate;
end;

procedure TFormHexEditor.btnExitClick(Sender: TObject);
begin
  if DataBuffChangeList.Count > 0 then
  begin
    if mrYes = MessageDlg('Query', 'Data changed, Save?', mtConfirmation,
      [mbYes, mbNo], 0) then
    begin
      btnSave.Click;
    end;
  end;
end;

procedure TFormHexEditor.actGotoExecute(Sender: TObject);
var
  goto_addr: cardinal;
begin
  FormInputDialog.CommonMaxLength := 8;
  FormInputDialog.CommonCase := scUpper;
  FormInputDialog.InputType := itNumeric;
  FormInputDialog.NumMax    := 0;
  FormInputDialog.NumMin    := 0;
  FormInputDialog.NumRadix  := nrHexadecimal;
  FormInputDialog.CommonPrefix := '0x';
  FormInputDialog.Caption   := 'Goto: input hex address';
  if FormInputDialog.ShowModal = mrOk then
  begin
    goto_addr := FormInputDialog.GetNumber;
    if (goto_addr >= StartAddress) and (goto_addr < (StartAddress + DataByteSize)) then
    begin
      Dec(goto_addr, StartAddress);
      sgData.TopRow := 1 + goto_addr div BYTES_IN_ROW;
      sgData.Row    := 1 + goto_addr div BYTES_IN_ROW;
      sgData.Col    := 1 + (goto_addr mod BYTES_IN_ROW);
    end;
  end;
end;

procedure TFormHexEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(hFile) then
  begin
    hFile.Free;
  end;
  if Assigned(DataBuffChangeList) then
  begin
    DataBuffChangeList.Destroy;
  end;
  CloseAction := caHide;
end;

procedure TFormHexEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormHexEditor.FormShow(Sender: TObject);
var
  i:   integer;
  ext: string;
  success: boolean;
begin
  DataBuffChangeList := TMemList.Create;
  StrGridInited := False;
  CurCellRow := 0;
  CurCellCol := 0;
  CurCellPos := 0;
  CurCellAddress := 0;
  CurCellAddressValid := False;
  sgData.Row := 0;
  sgData.Col := 0;
  Caption := Target + ' ' + FileName;
  sgData.RowCount := 0;
  sgData.TopRow := 0;
  ActiveControl := sgData;

  // prepare memory
  DataByteSize := (DataByteSize + (BYTES_IN_ROW - 1)) div BYTES_IN_ROW * BYTES_IN_ROW;
  if DataByteSize = 0 then
  begin
    // set default size
    DataByteSize := 256 * 1024;
  end;
  SetLength(DataBuff, DataByteSize);
  FillChar(DataBuff[0], DataByteSize, DefaultData);

  // open file and read
  if FileExists(FileName) then
  begin
    ext := LowerCase(ExtractFileExt(FileName));
    CurFileParserIndex := 0;
    for i := low(FileParser) to high(FileParser) do
    begin
      if ext = FileParser[i].ext then
      begin
        CurFileParserIndex := i;
      end;
    end;
    success := True;
    try
      hFile := TFileStream.Create(FileName, fmOpenReadWrite);
      if FileParser[CurFileParserIndex].ReadFile <> nil then
      begin
        if not FileParser[CurFileParserIndex].ReadFile(hFile,
          DataBuff[0], DataByteSize, StartAddress, SegOffset,
          AddressOffset) then
        begin
          success := False;
          MessageDlg('Error', 'fail to read ' + FileName + '.', mtError, [mbOK], 0);
        end;
      end;
    except
      begin
        success := False;
        MessageDlg('Error', 'File Open Failed:' + FileName, mtError, [mbOK], 0);
      end;
    end;
    if not success then
    begin
      Close;
      exit;
    end;
  end
  else
  begin
    MessageDlg('Error', FileName + ' not exists.', mtError, [mbOK], 0);
    Close;
    exit;
  end;

  // adjust GUI and display data
  sgData.RowCount := 1 + DataByteSize div BYTES_IN_ROW;
  SetLength(RowDirty, DataByteSize div BYTES_IN_ROW);
  FillChar(RowDirty[0], Length(RowDirty), True);
  sgData.ColCount  := 2 + 2 * BYTES_IN_ROW;
  sgData.FixedRows := 1;
  sgData.FixedCols := 1;
  for i := 1 to BYTES_IN_ROW do
  begin
    sgData.Cells[i, 0] := UpperCase(IntToHex(i - 1, 1));
  end;
  sgData.Row := 1;
  sgData.Col := 1;

  timerInitSize.Enabled := True;
end;

procedure TFormHexEditor.sgDataKeyPress(Sender: TObject; var Key: char);
var
  i:      integer;
  Value:  byte;
  IncPos: integer;
begin
  IncPos := 0;

  if CurCellAddressValid then
  begin
    // data input
    if CurCellCol > 1 + BYTES_IN_ROW then
    begin
      // input char data
      Value := byte(Key);
      Key   := char(0);
      // change data
      DataBuffChangeRequire(CurCellAddress, Value);
      IncPos := 2 + BYTES_IN_ROW;
    end
    else
    begin
      // input hex value
      Key := AnsiStrUpper(@Key)^;
      i   := Pos(Key, HEX_PARSE_STR);
      Key := char(0);
      if (i < 1) or (i > 16) then
      begin
        exit;
      end;
      Value := i - 1;
      // valid input, change data
      case CurCellPos of
        0:
        begin
          Value := (DataBuff[CurCellAddress] and $0F) or (Value shl 4);
          DataBuffChangeRequire(CurCellAddress, Value);
          Inc(CurCellPos);
        end;
        1:
        begin
          Value := (DataBuff[CurCellAddress] and $F0) or Value;
          DataBuffChangeRequire(CurCellAddress, Value);
          IncPos := 1;
        end;
      end;
    end;

    RowDirty[CurCellAddress div BYTES_IN_ROW] := True;
    // increase address if required
    if (IncPos > 0) and (not (CurCellAddress = DataByteSize - 1)) then
    begin
      if (CurCellAddress mod BYTES_IN_ROW) = (BYTES_IN_ROW - 1) then
      begin
        SgData.Col := IncPos;
        sgData.Row := sgData.Row + 1;
      end
      else
      begin
        sgData.Col := sgData.Col + 1;
      end;
    end;

    // update dirty data
    sgDataTopLeftChanged(sgData);
  end;
end;

procedure TFormHexEditor.SetGridCanvasFont(aFont: TFont);
const
  lastFont: TFont = nil;
begin
  if (aFont <> nil) and (aFont <> lastFont) then
  begin
    sgData.Canvas.Font := aFont;
  end;
end;

procedure TFormHexEditor.sgDataPrepareCanvas(Sender: TObject;
  aCol, aRow: integer; aState: TGridDrawState);
var
  addr: cardinal;
begin
  if gdFixed in aState then
  begin
    SetGridCanvasFont((Sender as TStringGrid).TitleFont);
  end
  else if (aRow > 0) and (aCol > 0) then
  begin
    if aCol > (1 + BYTES_IN_ROW) then
    begin
      Dec(aCol, 1 + BYTES_IN_ROW);
    end;

    addr := (aRow - 1) * BYTES_IN_ROW + aCol - 1;
    if DataBuffChangeList.GetIndexByAddr(addr) >= 0 then
    begin
      SetGridCanvasFont(ChangedFont);
    end
    else
    begin
      SetGridCanvasFont((Sender as TStringGrid).Font);
    end;
  end;
end;

procedure TFormHexEditor.sgDataSelectCell(Sender: TObject; aCol, aRow: integer;
  var CanSelect: boolean);
begin
  if aCol = (1 + BYTES_IN_ROW) then
  begin
    CanSelect := False;
  end
  else if (CurCellRow <> aRow) or (CurCellCol <> aCol) then
  begin
    CanSelect  := True;
    CurCellPos := 0;
    CurCellRow := aRow;
    CurCellCol := aCol;

    if (sgData.ColCount > 0) and (sgData.RowCount > 0) then
    begin
      if aCol > 1 + BYTES_IN_ROW then
      begin
        Dec(aCol, BYTES_IN_ROW + 1);
      end;

      CurCellAddressValid := True;
      CurCellAddress      := (aRow - 1) * BYTES_IN_ROW + aCol - 1;
      sgData.Cells[0, 0]  := IntToHex(StartAddress + CurCellAddress, 8);
    end;
  end;
end;

procedure TFormHexEditor.sgDataTopLeftChanged(Sender: TObject);
var
  i, j: integer;
begin
  // update data
  for i := sgData.TopRow to sgData.TopRow + sgData.VisibleRowCount do
  begin
    if (i < sgData.RowCount) and RowDirty[i - 1] then
    begin
      sgData.Cells[0, i] := UpperCase(IntToHex(StartAddress + (i - 1) * $10, 8));
      for j := 1 to BYTES_IN_ROW do
      begin
        sgData.Cells[j, i] := UpperCase(IntToHex(DataBuff[(i - 1) * 16 + (j - 1)], 2));
      end;
      for j := 2 + BYTES_IN_ROW to 1 + 2 * BYTES_IN_ROW do
      begin
        sgData.Cells[j, i] := char(DataBuff[(i - 1) * 16 + (j - 2 - BYTES_IN_ROW)]);
      end;
      RowDirty[i - 1] := False;
    end;
  end;
end;

procedure TFormHexEditor.timerInitSizeTimer(Sender: TObject);
var
  i: integer;
  tmpWidth, allWidth: integer;
begin
  (Sender as TTimer).Enabled := False;
  allWidth := 0;

  lblMeasureSize.Caption := '000000000';
  lblMeasureSize.AdjustSize;
  tmpWidth := lblMeasureSize.Width;
  sgData.ColWidths[0] := tmpWidth;
  Inc(allWidth, tmpWidth);

  lblMeasureSize.Caption := '000';
  lblMeasureSize.AdjustSize;
  tmpWidth := lblMeasureSize.Width;
  for i := 1 to BYTES_IN_ROW do
  begin
    sgData.ColWidths[i] := tmpWidth;
  end;
  Inc(allWidth, BYTES_IN_ROW * tmpWidth);

  sgData.ColWidths[BYTES_IN_ROW + 1] := DIVIDER_WIDTH;
  Inc(allWidth, DIVIDER_WIDTH);

  lblMeasureSize.Caption := '00';
  lblMeasureSize.AdjustSize;
  tmpWidth := lblMeasureSize.Width;
  for i := 2 + BYTES_IN_ROW to 1 + 2 * BYTES_IN_ROW do
  begin
    sgData.ColWidths[i] := tmpWidth;
  end;
  Inc(allWidth, BYTES_IN_ROW * tmpWidth);

  sgData.Align := alCustom;
  sgData.Width := allWidth;
  Width := pnlData.BevelWidth * 2 + sgData.Width + 20;
  sgData.Align := alClient;

  // Center Buttons
  btnSave.Left := (pnlButton.Width div 2 - btnSave.Width) div 2;
  btnExit.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnSave.Width) div 2;

  StrGridInited := True;
  sgDataTopLeftChanged(sgData);
  UpdateShowing();
end;

initialization
  {$I hexeditor.lrs}

end.


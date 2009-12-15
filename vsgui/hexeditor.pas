unit hexeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Grids, ActnList, parameditor, inputdialog, Contnrs, math;

type

  { TMemList TMemInfo }

  TMemInfo = class(TObject)
  private
    FStartAddr: Cardinal;
    FByteSize: Cardinal;
    function GetTail: Cardinal;
  public
    constructor Create(aStartAddr, aByteSize: Cardinal);
    property StartAddr: Cardinal read FStartAddr write FStartAddr;
    property ByteSize: Cardinal read FByteSize write FByteSize;
    property Head: Cardinal read FStartAddr;
    property Tail: Cardinal read GetTail;
  end;

  TMemList = class(TObjectList)
  private
    function GetMemInfoItem(Index: integer): TMemInfo;
    procedure SetMemInfoItem(Index: integer; aMemInfoObject: TMemInfo);
  public
    constructor Create;
    function Add(aStartAddr, aByteSize: Cardinal): integer;
    function Add(aMemInfo: TMemInfo): Integer;
    function GetIndexByAddr(aAddr: Cardinal): integer;
    property MemInfoItems[Index: Integer]: TMemInfo read GetMemInfoItem write SetMemInfoItem; default;
  end;

  { TFileParser }

  TReadFileFunc = function(hFile: TFileStream; var buffer: array of byte;
                bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
  TValidateFileFunc = function(hFile: TFileStream; buffer: array of byte; default_byte: byte;
                 bytesize, start_addr, seg_offset, addr_offset: integer): boolean;

  TFileParser = record
    ext: string;
    ReadFile: TReadFileFunc;
    ValidateFile: TValidateFileFunc;
  end;

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    actGoto: TAction;
    alHotKey: TActionList;
    btnSave: TButton;
    btnExit: TButton;
    lblMeasureSize: TLabel;
    pnlData: TPanel;
    pnlButton: TPanel;
    sgData: TStringGrid;
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
    procedure sgDataPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure sgDataSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
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
    FileName: string;
    SegOffset: integer;
    AddressOffset: integer;

    StartAddress: Cardinal;
    DataByteSize: Cardinal;
    DefaultData: byte;
  end;

  function ReadBinFile(hFile: TFileStream; var buffer: array of byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
  function ValidateBinFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
  function ReadHexFile(hFile: TFileStream; var buffer: array of byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
  function ValidateHexFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): boolean;

var
  FormHexEditor: TFormHexEditor;

const
  FileParser: array[0..1] of TFileParser =
  (
  (ext: '.bin'; ReadFile: @ReadBinFile; ValidateFile: @ValidateBinFile;),
  (ext: '.hex'; ReadFile: @ReadHexFile; ValidateFile: @ValidateHexFile;)
  );
  BYTES_IN_ROW: integer = 16;
  DIVIDER_WIDTH: integer = 20;



implementation

function ReadBinFile(hFile: TFileStream; var buffer: array of byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
begin
  seg_offset := seg_offset;
  hFile.Position := addr_offset;
  hFile.Read(buffer[0], bytesize);
  result := TRUE;
end;

function ValidateBinFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
begin
end;

function ReadHexFile(hFile: TFileStream; var buffer: array of byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
var
  ch: char;
  line: string;
  buff: array[0 .. 260] of byte;
  i, j: integer;
  checksum: byte;
  line_data_len, line_addr, line_type: integer;
  seg_addr, ext_addr, data_addr: Int64;
begin
  seg_addr := 0;
  ext_addr := 0;

  hFile.Position := 0;
  result := FALSE;

  while TRUE do
  begin
    // ignore empty lines
    repeat
      if hFile.Read(ch, 1) <> 1 then
      begin
        result := TRUE;
        exit;
      end;
    until (ch <> char(10)) and (ch <> char(13));
    // first char MUST be :
    if (ch <> ':') then
    begin
      exit;
    end;
    // read line
    line := '';
    repeat
      if (hFile.Read(ch, 1) <> 1) or (ch = char(10)) or (ch = char(13)) then
      begin
        break;
      end
      else
      begin
        line := line + ch;
      end;
    until FALSE;
    if (Length(line) < 10) or ((Length(line) mod 2) = 1) then
    begin
      exit;
    end;
    i := 0;
    while i < Length(line) do
    begin
      buff[i div 2] := byte(StrToIntRadix(Copy(line, i + 1, 2), 16));
      i := i + 2;
    end;
    i := i div 2;
    // validity check
    line_data_len := buff[0];
    if (line_data_len + 5) <> i then
    begin
      exit;
    end;
    checksum := 0;
    for j := 0 to i - 1 do
    begin
      checksum := checksum + buff[j];
    end;
    if checksum <> 0 then
    begin
      exit;
    end;
    // process data
    case buff[3] of
      0://htData
        begin
          data_addr := ext_addr + (buff[1] shl 8) + buff[2] - start_addr;
          if data_addr < addr_offset then
          begin
            continue;
          end;
          if (line_data_len + data_addr) > bytesize then
          begin
            if data_addr > bytesize then
            begin
              line_data_len := 0;
            end
            else
            begin
              line_data_len := bytesize - data_addr;
            end;
          end;
          Move(buff[4], buffer[data_addr - addr_offset], line_data_len);
        end;
      1://htEOF
        begin
          result := TRUE;
          exit;
        end;
      2://htSegAddr
        begin
          if line_data_len <> 2 then
          begin
            exit;
          end;
          seg_addr := (buff[4] shl 8) + buff[5];
        end;
      4://htExtAddr
        begin
          if line_data_len <> 2 then
          begin
            exit;
          end;
          ext_addr := (buff[4] shl 24) + (buff[5] shl 16);
        end
    else
      // ignore data
      continue;
    end;
  end;
end;

function ValidateHexFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): boolean;
begin

end;

{ TMemInfo }

constructor TMemInfo.Create(aStartAddr, aByteSize: Cardinal);
begin
  inherited Create;

  StartAddr := aStartAddr;
  ByteSize := aByteSize;
end;

function TMemInfo.GetTail: Cardinal;
begin
  if (StartAddr = 0) and (ByteSize = 0) then
  begin
    result := 0;
  end
  else
  begin
    result := StartAddr + ByteSize - 1;
  end;
end;

{ TMemList }

procedure TMemList.SetMemInfoItem(Index: integer; aMemInfoObject: TMemInfo);
begin
  Put(Index, Pointer(aMemInfoObject));
end;

function TMemList.GetMemInfoItem(Index: integer): TMemInfo;
begin
  Result := TMemInfo(Inherited Get(Index));
end;

function TMemList.GetIndexByAddr(aAddr: Cardinal): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if (MemInfoItems[i].StartAddr <= aAddr) and
        ((MemInfoItems[i].StartAddr + MemInfoItems[i].ByteSize) > aAddr) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TMemList.Add(aStartAddr, aByteSize: Cardinal): integer;
var
  i: integer;
  aTail: integer;
  found: boolean;
begin
  if aByteSize = 0 then
  begin
    result := -1;
    exit;
  end;

  aTail := aStartAddr + aByteSize - 1;
  // find a location to insert or merge
  found := FALSE;
  for i := 0 to Count - 1 do
  begin
    if aStartAddr <= (MemInfoItems[i].Tail + 1) then
    begin
      found := TRUE;
      break;
    end;
  end;
  if not found then
  begin
    // add last
    result := inherited Add(TMemInfo.Create(aStartAddr, aByteSize));
    exit;
  end;
  result := i;
  // merge or insert
  if ((aTail + 1) < MemInfoItems[result].Head) then
  begin
    // insert before
    Insert(i, TmemInfo.Create(aStartAddr, aByteSize));
  end
  else
  begin
    // merge
    MemInfoItems[result].StartAddr := Min(MemInfoItems[result].StartAddr, aStartAddr);
    MemInfoItems[result].ByteSize := 1 + Max(MemInfoItems[result].Tail, aTail) - MemInfoItems[result].StartAddr;
    // try merge the descendant
    i := i + 1;
    while i < Count do
    begin
      if (MemInfoItems[result].Tail + 1) < MemInfoItems[i].Head then
      begin
        break;
      end;

      // merge current
      MemInfoItems[result].ByteSize := 1 + Max(MemInfoItems[result].Tail, MemInfoItems[i].Tail) - MemInfoItems[result].StartAddr;
      Delete(i);

      i := i + 1;
    end;
  end;
end;

function TMemList.Add(aMemInfo: TMemInfo): Integer;
begin
  result := Add(aMemInfo.StartAddr, aMemInfo.ByteSize);
end;

constructor TMemList.Create;
begin
  inherited Create(TRUE);
end;

{ TFormHexEditor }

procedure TFormHexEditor.DataBuffChangeRequire(addr: integer; aValue: byte);
var
  i: integer;
  index: integer;
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
  start, lines: integer;
begin
  if (CurFileParserIndex >= Length(FileParser)) then
  begin
    exit;
  end;

  if (FileParser[CurFileParserIndex].ValidateFile <> nil) then
  begin
    // TODO: Add Validate function here
//    FileParser[CurFileParserIndex].ValidateFile(hFile,
//        DataBuff[0], DefaultData, CurFileInfo.byte_size,
//        CurFileInfo.start_addr, SegOffset, AddressOffset);
  end;

  // Dirty corresponding lines
  for i := 0 to DataBuffChangeList.Count - 1 do
  begin
    start := DataBuffChangeList.MemInfoItems[i].Head div BYTES_IN_ROW;
    lines := DataBuffChangeList.MemInfoItems[i].tail div BYTES_IN_ROW;
    Dec(lines, start);
    FillChar(RowDirty[start], lines, TRUE);
  end;
  DataBuffChangeList.Clear;
  // update dirty color
  sgData.Invalidate;
end;

procedure TFormHexEditor.btnExitClick(Sender: TObject);
begin
  if DataBuffChangeList.Count > 0 then
  begin
    if mrYes = MessageDlg('Query','Data changed, Save?', mtConfirmation, [mbYes, mbNo], 0) then
    begin
      btnSave.Click;
    end;
  end;
end;

procedure TFormHexEditor.actGotoExecute(Sender: TObject);
var
  goto_addr: Cardinal;
begin
  FormInputDialog.CommonMaxLength := 8;
  FormInputDialog.CommonCase := scUpper;
  FormInputDialog.InputType := itNumeric;
  FormInputDialog.NumMax := 0;
  FormInputDialog.NumMin := 0;
  FormInputDialog.NumRadix := nrHexadecimal;
  FormInputDialog.CommonPrefix := '0x';
  FormInputDialog.Caption := 'Goto: input hex address';
  if FormInputDialog.ShowModal = mrOK then
  begin
    goto_addr := FormInputDialog.GetNumber;
    if (goto_addr >= StartAddress) and (goto_addr < (StartAddress + DataByteSize)) then
    begin
      Dec(goto_addr, StartAddress);
      sgData.TopRow := 1 + goto_addr div BYTES_IN_ROW;
      sgData.Row := 1 + goto_addr div BYTES_IN_ROW;
      sgData.Col := 1 + (goto_addr mod BYTES_IN_ROW);
    end;
  end;
end;

procedure TFormHexEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(DataBuffChangeList) then
  begin
    DataBuffChangeList.Destroy;
  end;
end;

procedure TFormHexEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    close;
  end;
end;

procedure TFormHexEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  ext: string;
  success: boolean;
begin
  DataBuffChangeList := TMemList.Create;
  StrGridInited := FALSE;
  CurCellRow := 0;
  CurCellCol := 0;
  CurCellPos := 0;
  CurCellAddress := 0;
  CurCellAddressValid := FALSE;
  sgData.Row := 0;
  sgData.Col := 0;
  Caption := Caption + ' ' + FileName;
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
    ext := ExtractFileExt(FileName);
    CurFileParserIndex := 0;
    for i := low(FileParser) to high(FileParser) do
    begin
      if LowerCase(ext) = FileParser[i].ext then
      begin
        CurFileParserIndex := i;
      end;
    end;
    success := TRUE;
    try
      hFile := TFileStream.Create(FileName, fmOpenRead);
      if FileParser[CurFileParserIndex].ReadFile <> nil then
      begin
        if not FileParser[CurFileParserIndex].ReadFile(hFile, DataBuff[0],
                    DataByteSize, StartAddress, SegOffset, AddressOffset) then
        begin
          success := FALSE;
          MessageDlg('Error','fail to read ' + FileName + '.', mtError, [mbOK], 0);
        end;
      end;
    except
      begin
        success := FALSE;
        MessageDlg('Error', 'File Open Failed:' + FileName, mtError, [mbOK], 0);
      end;
    end;
    hFile.Free;
    if not success then
    begin
      close;
      exit;
    end;
  end
  else
  begin
    MessageDlg('Error',FileName + ' not exists.', mtError, [mbOK], 0);
    close;
    exit;
  end;

  // adjust GUI and display data
  sgData.RowCount := 1 + DataByteSize div BYTES_IN_ROW;
  SetLength(RowDirty, DataByteSize div BYTES_IN_ROW);
  FillChar(RowDirty[0], Length(RowDirty), TRUE);
  sgData.ColCount := 2 + 2 * BYTES_IN_ROW;
  sgData.FixedRows := 1;
  sgData.FixedCols := 1;
  for i := 1 to BYTES_IN_ROW do
  begin
    sgData.Cells[i,0] := UpperCase(IntToHex(i - 1,1));
  end;
  sgData.Row := 1;
  sgData.Col := 1;

  timerInitSize.Enabled := TRUE;
end;

procedure TFormHexEditor.sgDataKeyPress(Sender: TObject; var Key: char);
var
  i: integer;
  value: byte;
  IncPos: integer;
begin
  IncPos := 0;

  if CurCellAddressValid then
  begin
    // data input
    if CurCellCol > 1 + BYTES_IN_ROW then
    begin
      // input char data
      value := byte(Key);
      Key := char (0);
      // change data
      DataBuffChangeRequire(CurCellAddress, value);
      CurCellPos := CurCellPos + 1;
      IncPos := 2 + BYTES_IN_ROW;
    end
    else
    begin
      // input hex value
      Key := AnsiStrUpper(@Key)^;
      i := Pos(Key, HEX_PARSE_STR);
      Key := char(0);
      if (i < 1) or (i > 16) then
      begin
        exit;
      end;
      value := i - 1;
      // valid input, change data
      case CurCellPos of
        0:
          begin
            value := (DataBuff[CurCellAddress] and $0F) or (value shl 4);
            DataBuffChangeRequire(CurCellAddress, value);
            CurCellPos := CurCellPos + 1;
          end;
        1:
          begin
            value := (DataBuff[CurCellAddress] and $F0) or value;
            DataBuffChangeRequire(CurCellAddress, value);
            CurCellPos := 0;
            IncPos := 1;
          end;
      end;
    end;

    RowDirty[CurCellAddress div BYTES_IN_ROW] := TRUE;
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

procedure TFormHexEditor.sgDataPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  addr: Cardinal;
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

procedure TFormHexEditor.sgDataSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (CurCellRow <> aRow) or (CurCellCol <> aCol) then
  begin
    CurCellPos := 0;
    CurCellRow := aRow;
    CurCellCol := aCol;

    if (sgData.ColCount > 0) and (sgData.RowCount > 0) then
    begin
      if aCol = 1 + BYTES_IN_ROW then
      begin
        CurCellAddressValid := FALSE;
        sgData.Cells[0,0] := '';
        exit;
      end
      else if aCol > 1 + BYTES_IN_ROW then
      begin
        Dec(aCol, BYTES_IN_ROW + 1);
      end;

      CurCellAddressValid := TRUE;
      CurCellAddress := (aRow - 1) * BYTES_IN_ROW + aCol - 1;
      sgData.Cells[0,0] := IntToHex(StartAddress + CurCellAddress, 8);
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
        sgData.Cells[j, i] := Char(DataBuff[(i - 1) * 16 + (j - 2 - BYTES_IN_ROW)]);
      end;
      RowDirty[i - 1] := FALSE;
    end;
  end;
end;

procedure TFormHexEditor.timerInitSizeTimer(Sender: TObject);
var
  i: integer;
  tmpWidth, allWidth: integer;
begin
  (Sender as TTimer).Enabled := FALSE;
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

  StrGridInited := TRUE;
  sgDataTopLeftChanged(sgData);
  UpdateShowing();
end;

initialization
  {$I hexeditor.lrs}

end.


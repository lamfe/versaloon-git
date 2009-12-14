unit hexeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Grids, parameditor, inputdialog;

type

  { TFileParser }
  TFileInfo = record
    start_addr: integer;
    byte_size: integer;
  end;

  TReadFileFunc = function(hFile: TFileStream; var buffer: array of byte;
                bytesize, start_addr, seg_offset, addr_offset: integer): TFileInfo;
  TWriteFileFunc = function(hFile: TFileStream; buffer: array of byte; default_byte: byte;
                 bytesize, start_addr, seg_offset, addr_offset: integer): integer;
  TEndFileFunc = procedure(hFile: TFileStream);

  TFileParser = record
    ext: string;
    ReadFile: TReadFileFunc;
    WriteFile: TWriteFileFunc;
    EndFile: TEndFileFunc;
  end;

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    lblMeasureSize: TLabel;
    pnlData: TPanel;
    pnlButton: TPanel;
    sgData: TStringGrid;
    timerInitSize: TTimer;
    procedure btnSaveClick(Sender: TObject);
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
  private
    { private declarations }
    DataGridWidth: integer;
    AddressGridWidth: integer;
    DataBuff: array of byte;
    RowDirty: array of boolean;
    StrGridInited: boolean;
    CurCellRow: integer;
    CurCellCol: integer;
    CurCellPos: integer;
    CurCellAddress: integer;
    CurFileParserIndex: integer;
    hFile: TFileStream;
    CurFileInfo: TFileInfo;
  public
    { public declarations }
    FileName: string;
    SegOffset: integer;
    AddressOffset: integer;

    StartAddress: integer;
    DataByteSize: integer;
    DefaultData: byte;
  end;

  function ReadBinFile(hFile: TFileStream; var buffer: array of byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): TFileInfo;
  function WriteBinFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): integer;
  function ReadHexFile(hFile: TFileStream; var buffer: array of byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): TFileInfo;
  function WriteHexFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
           bytesize, start_addr, seg_offset, addr_offset: integer): integer;
  procedure EndHexFile(hFile: TFileStream);

var
  FormHexEditor: TFormHexEditor;

const
  FileParser: array[0..1] of TFileParser =
  (
  (ext: '.bin'; ReadFile: @ReadBinFile; WriteFile: @WriteBinFile; EndFile: nil),
  (ext: '.hex'; ReadFile: @ReadHexFile; WriteFile: @WriteHexFile; EndFile: @EndHexFile)
  );
  BYTES_IN_ROW: integer = 16;



implementation

function ReadBinFile(hFile: TFileStream; var buffer: array of byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): TFileInfo;
begin
  seg_offset := seg_offset;
  hFile.Position := addr_offset;
  result.byte_size := hFile.Read(buffer[0], bytesize);
  result.start_addr := start_addr;
end;

function WriteBinFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): integer;
begin
end;

function ReadHexFile(hFile: TFileStream; var buffer: array of byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): TFileInfo;
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
  result.byte_size := 0;
  result.start_addr := 0;

  while TRUE do
  begin
    // ignore empty lines
    repeat
      if hFile.Read(ch, 1) <> 1 then
      begin
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
      if hFile.Read(ch, 1) <> 1 then
      begin
        exit;
      end;
      if (ch = char(10)) or (ch = char(13)) then
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
    if (line_data_len = 0) or ((line_data_len + 5) <> i) then
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
          if seg_addr <> seg_offset then
          begin
            continue;
          end;

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
        exit;
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
      exit;
    end;
  end;
end;

function WriteHexFile(hFile: TFileStream; buffer: array of byte; default_byte: byte;
         bytesize, start_addr, seg_offset, addr_offset: integer): integer;
begin

end;

procedure EndHexFile(hFile: TFileStream);
begin

end;

{ TFormHexEditor }

procedure TFormHexEditor.FormDestroy(Sender: TObject);
begin
  SetLength(DataBuff, 0);
  SetLength(RowDirty, 0);
end;

procedure TFormHexEditor.FormCreate(Sender: TObject);
begin
  pnlButton.Height := 0;
end;

procedure TFormHexEditor.btnSaveClick(Sender: TObject);
begin
  if FileParser[CurFileParserIndex].WriteFile <> nil then
  begin
    FileParser[CurFileParserIndex].WriteFile(hFile,
        DataBuff[0], DefaultData, CurFileInfo.byte_size,
        CurFileInfo.start_addr, SegOffset, AddressOffset);
  end;
  if FileParser[CurFileParserIndex].EndFile <> nil then
  begin
    FileParser[CurFileParserIndex].EndFile(hFile);
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
begin
  StrGridInited := FALSE;
  CurCellRow := 0;
  CurCellCol := 0;
  CurCellPos := 0;
  Caption := Caption + ' ' + FileName;
  sgData.RowCount := 0;
  sgData.TopRow := 0;

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
    try
      hFile := TFileStream.Create(FileName, fmOpenRead);
    except
      begin
        MessageDlg('Error', 'File Open Failed:' + FileName, mtError, [mbOK], 0);
        exit;
      end;
    end;
    ext := ExtractFileExt(FileName);
    CurFileParserIndex := 0;
    for i := 0 to Length(FileParser) - 1 do
    begin
      if LowerCase(ext) = FileParser[i].ext then
      begin
        CurFileParserIndex := i;
        if FileParser[i].ReadFile <> nil then
        begin
          CurFileInfo := FileParser[i].ReadFile(hFile, DataBuff[0],
                      DataByteSize, StartAddress, SegOffset, AddressOffset);
        end;
      end;
    end;
    hFile.Free;
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
  sgData.ColCount := 1 + BYTES_IN_ROW;
  sgData.FixedRows := 1;
  sgData.FixedCols := 1;
  for i := 1 to sgData.ColCount-1 do
  begin
    sgData.Cells[i,0] := UpperCase(IntToHex(i - 1,1));
    sgData.ColWidths[i] := DataGridWidth;
  end;
  for i := 1 to sgData.RowCount - 1 do
  begin
    sgData.Cells[0, i] := UpperCase(IntToHex(StartAddress + (i - 1) * $10, 8));
  end;

  timerInitSize.Enabled := TRUE;
end;

procedure TFormHexEditor.sgDataKeyPress(Sender: TObject; var Key: char);
var
  goto_addr: integer;
  i: integer;
  value: byte;
begin
  if (Key = 'g') or (Key = 'G') then
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
      goto_addr := goto_addr shr 4 shl 4;
      for i := 1 to sgData.RowCount -1 do
      begin
        if StrToIntRadix(sgData.Cells[0, i], 16) = goto_addr then
        begin
          sgData.TopRow := i;
          Key := char(0);
          exit;
        end;
      end;
    end;
    Key := char(0);
  end
  else
  begin
    // data input
    Key := AnsiStrUpper(@Key)^;
    i := Pos(Key, HEX_PARSE_STR);
    Key := char(0);
    if (i < 1) or (i > 16) then
    begin
      exit;
    end;
    i := i - 1;
    // valid input, change data
    case CurCellPos of
      0:
        begin
          DataBuff[CurCellAddress] := (DataBuff[CurCellAddress] and $0F) or (i shl 4);
          CurCellPos := CurCellPos + 1;
        end;
      1:
        // move to next address
        begin
          DataBuff[CurCellAddress] := (DataBuff[CurCellAddress] and $F0) or i;
          CurCellPos := 0;
          if not (CurCellAddress = DataByteSize - 1) then
          begin
            if (CurCellAddress mod BYTES_IN_ROW) = (BYTES_IN_ROW - 1) then
            begin
              SgData.Col := 1;
              sgData.Row := sgData.Row + 1;
            end
            else
            begin
              sgData.Col := sgData.Col + 1;
            end;
          end;
        end;
    end;
    // update dirty
    RowDirty[(CurCellAddress + (BYTES_IN_ROW - 1)) div BYTES_IN_ROW] := TRUE;
    sgDataTopLeftChanged(sgData);
  end;
end;

procedure TFormHexEditor.sgDataPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if gdFixed in aState then
  begin
//    (Sender as TCustomGrid).SetCanvasFont(TitleFont);
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
      CurCellAddress := (CurCellRow - 1) * BYTES_IN_ROW + CurCellCol - 1;
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
    if (i < sgData.RowCount) and RowDirty[i] then
    begin
      for j := 1 to sgData.ColCount - 1 do
      begin
        sgData.Cells[j, i] := UpperCase(IntToHex(DataBuff[(i - 1) * 16 + (j - 1)], 2));
      end;
      RowDirty[i] := FALSE;
    end;
  end;
end;

procedure TFormHexEditor.timerInitSizeTimer(Sender: TObject);
var
  i: integer;
begin
  (Sender as TTimer).Enabled := FALSE;

  lblMeasureSize.Caption := '000';
  lblMeasureSize.AdjustSize;
  DataGridWidth := lblMeasureSize.Width;
  lblMeasureSize.Caption := '000000000';
  lblMeasureSize.AdjustSize;
  AddressGridWidth := lblMeasureSize.Width;

  for i := 1 to sgData.ColCount-1 do
  begin
    sgData.ColWidths[i] := DataGridWidth;
  end;
  sgData.ColWidths[0] := AddressGridWidth;

  sgData.Align := alCustom;
  sgData.Width := AddressGridWidth + DataGridWidth * 16;
  Width := pnlData.BevelWidth * 2 + sgData.Width + 20;
  sgData.Align := alClient;

  // Center Buttons
  btnSave.Left := (pnlButton.Width div 2 - btnSave.Width) div 2;
  btnCancel.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnSave.Width) div 2;

  StrGridInited := TRUE;
  sgDataTopLeftChanged(sgData);
  UpdateShowing();
end;

initialization
  {$I hexeditor.lrs}

end.


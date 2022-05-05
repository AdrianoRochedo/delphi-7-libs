unit SheetWrapper;

interface
uses SysUtils,
     Graphics,
     Dialogs,
     SysUtilsEx,
     Types,
     {$ifdef WinStat}
     wsVec,
     {$endif}
     wsConstTypes,
     DiretivasDeCompilacao,
     BaseSpreadSheetBook,
     cxSSTypes,
     cxSSheet;

type
  TVarType = (vtString, vtInteger, vtFloat);

  TSheet = class(TBaseSheet)
  private
    FCenterText: boolean;
    FSS: TcxSSBookSheet;
    FSSB: TcxSpreadSheetBook;
    FActiveRow: integer;
    FActiveCol: integer;

    procedure getValue(aRow, aCol: integer;
                       cellType: TVarType;
                       out sVal: string;
                       out iVal: integer;
                       out rVal: real;
                       getDisplayText: boolean);

    function getTopLeftText: String; override;
    procedure setTopLeftText(const Value: String); override;
    function getSH: Boolean; override;
    procedure setSH(const Value: Boolean); override;
    function GetColWidth(i: integer): Integer; override;
    procedure SetColWidth(i: integer; const Value: Integer); override;
    function getColCount: Integer; override;
    function getRowCount: Integer; override;
    function GetSelectionRect: TRect; override;
    procedure SetSelectionRect(const Value: TRect); override;
    function getCaption: string; override;
    procedure setCaption(const Value: string); override;
    procedure setActiveRow(const Value : integer); override;
    procedure setActiveCol(const Value : integer); override;
    function getActiveRow(): Integer; override;
    function getActiveCol(): Integer; override;
    function GetRowHeight(i: integer): Integer; override;
    procedure SetRowHeight(i: integer; const Value: Integer); override;
  public
    constructor Create (SpreadSheetBook: TcxSpreadSheetBook);

    // Métodos de escrita
    procedure Write (const aText: string; IncRow, IncCol: boolean); overload; override;
    procedure Write (const aValue: Real; IncRow, IncCol: boolean; DecimalPlaces: integer = 2); overload; override;
    procedure Write (aRow, aCol: integer; const aText: string); overload; override;
    procedure Write (aRow, aCol: Integer; const aValue: Real; DecimalPlaces: byte = 2); overload; override;
    procedure Write (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aText: string); overload; override;
    procedure Write (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; override;
    procedure Write (aRow, aCol: integer; IsBold: Boolean; const aText: string); overload; override;
    procedure Write (aRow, aCol: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; override;
    procedure WriteCenter (const aText: string; IncRow, IncCol: boolean); overload; override;
    procedure WriteCenter (const aValue: Real; IncRow, IncCol: boolean; DecimalPlaces: integer = 2); overload; override;
    procedure WriteCenter (aRow, aCol: Integer; const aText: String); overload; override;
    procedure WriteCenter (aRow, aCol: Integer; const aValue: Real; DecimalPlaces: byte = 2); overload; override;
    procedure WriteCenter (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aText: string); overload; override;
    procedure WriteCenter (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; override;
    procedure WriteCenter (aRow, aCol: integer; IsBold: Boolean; const aText: string); overload; override;
    procedure WriteCenter (aRow, aCol: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; override;
    procedure WriteVecInCol (aVec: IArray; aCol, startRow: Integer; DecimalPlaces: byte = 2); override;
    procedure WriteVecInRow (aVec: IArray; aRow, startCol: Integer; DecimalPlaces: byte = 2); override;

    // Valor das células
    function GetText        (aRow, aCol: Integer): String; override;
    function GetDisplayText (aRow, aCol: Integer): String; override;
    function GetFloat       (aRow, aCol: Integer): Real; override;
    function GetInt         (aRow, aCol: Integer): Integer; override;

    {$ifdef WinStat}
    // Converte uma linha/coluna para vetor
    function ColToVec (aCol, startRow, endRow: Integer): TwsVec; override;
    function RowToVec (aRow, startCol, endCol: Integer): TwsVec; override;
    {$endif}

    // Formatacao..............................................................

    // se "aFontName" = '' valerá a fonte corrente
    // se "aSize" = 0 valerá o tamanho corrente
    procedure SetCellFont (aRow, aCol : Integer;
                           aFontName  : String;
                           aSize      : Smallint = 0;
                           aColor     : TColor   = clBlack;
                           isBold     : Boolean  = False;
                           isItalic   : Boolean  = False); override;

    procedure BoldCell(aRow, aCol : Integer); override;
    procedure BoldRow(aRow: Integer); override;
    procedure BoldCol(aCol: Integer); override;

    procedure SetCellColor(aRow, aCol : Integer;
                           aColor: TColor;
                           IsBold: boolean = false); override;

    procedure FormatCells(const ACells: TRect); override;
    procedure Merge(Row1, Col1, Row2, Col2: integer); override;

    procedure WordWrap(aRow, aCol : Integer); override;

    // clipboard
    procedure Cut(); override;
    procedure Copy(); override;
    procedure CopyAll(); override;
    procedure Paste(); override;

    // métodos gerais
    procedure Clear(); override;

    // Cabeçalhos de linhas e colunas da Planilha
    property TopLeftText : String  read getTopLeftText write setTopLeftText;
    property ShowHeaders : Boolean read getSH          write setSH;

    // Tamanho de uma coluna e de uma linha
    property ColWidth[i: integer] : Integer read GetColWidth write SetColWidth;
    property RowHeight[i: integer] : Integer read GetRowHeight write SetRowHeight;

    // Número de Colunas e Linhas com valores válidos
    property ColCount : Integer read getColCount;
    property RowCount : Integer read getRowCount;

    // Nome da Folha
    property Caption : string read getCaption write setCaption;

    // Folha ativa
    property Sheet: TcxSSBookSheet read FSS write FSS;
  end;

implementation
uses cxExcelConst;

{ TSheet }

constructor TSheet.Create(SpreadSheetBook: TcxSpreadSheetBook);
begin
  inherited Create();
  FSS := SpreadSheetBook.ActiveSheet;
  FSSB := SpreadSheetBook;
end;

procedure TSheet.Write(aRow, aCol: integer; const aText: string);
var c: TcxSSCellObject;
begin
  c := FSS.GetCellObject(aCol-1, aRow-1);
  if FCenterText then
     begin
     c.Style.HorzTextAlign := cxSSTypes.haCENTER;
     c.Style.VertTextAlign := cxSSTypes.vaCENTER;
     end;
  c.SetCellText(aText, False);
  c.Free();
end;

procedure TSheet.Write(aRow, aCol: Integer; const aValue: Real; DecimalPlaces: byte);
begin
  Write(aRow, aCol, toString(aValue, DecimalPlaces));
end;

procedure TSheet.WriteCenter(aRow, aCol: Integer; const aText: String);
begin
  FCenterText := true;
  Write(aRow, aCol, aText);
  FCenterText := false;
end;

procedure TSheet.WriteCenter(aRow, aCol: Integer; const aValue: Real; DecimalPlaces: byte = 2);
begin
  FCenterText := true;
  Write(aRow, aCol, toString(aValue, DecimalPlaces));
  FCenterText := false;
end;

procedure TSheet.WriteVecInCol(aVec: IArray; aCol, startRow: Integer; DecimalPlaces: byte);
var i: Integer;
    x: Double;
begin
  dec(startRow);
  for i := aVec.Low() to aVec.High() do
    if not aVec.IsMissValue(i, x) then
       WriteCenter(startRow + i, aCol, x, DecimalPlaces)
    else
       WriteCenter(startRow + i, aCol, wscMissValueChar);
end;

procedure TSheet.WriteVecInRow(aVec: IArray; aRow, startCol: Integer; DecimalPlaces: byte);
var i: Integer;
    x: Double;
begin
  dec(startCol);
  for i := aVec.Low() to aVec.High() do
    if not aVec.IsMissValue(i, x) then
       WriteCenter(aRow, startCol + i, x, DecimalPlaces)
    else
       WriteCenter(aRow, startCol + i, wscMissValueChar);
end;

procedure TSheet.getValue(aRow, aCol: integer;
                          cellType: TVarType;
                          out sVal: string;
                          out iVal: integer;
                          out rVal: real;
                          getDisplayText: boolean);
begin
  with FSS.GetCellObject(aCol-1, aRow-1) do
    begin
    case cellType of
      vtString  : if getDisplayText then sVal := DisplayText else sVal := Text;
      vtInteger : iVal := toInt(Text, 0);
      vtFloat   : rVal := toFloat(Text, 0);
      end;
    Free();
    end;
end;

function TSheet.GetDisplayText(aRow, aCol: Integer): String;
var i: integer; r: Real;
begin
  getValue(aRow, aCol, vtString, {out} Result, {out} i, {out} r, true);
end;

function TSheet.GetFloat(aRow, aCol: Integer): Real;
var s: string; i: integer;
begin
  getValue(aRow, aCol, vtFloat, {out} s, {out} i, {out} Result, false);
end;

function TSheet.GetInt(aRow, aCol: Integer): Integer;
var s: string; r: real;
begin
  getValue(aRow, aCol, vtInteger, {out} s, {out} Result, {out} r, false);
end;

function TSheet.GetText(aRow, aCol: Integer): String;
var i: integer; r: Real;
begin
  getValue(aRow, aCol, vtString, {out} Result, {out} i, {out} r, false);
end;

{$ifdef WinStat}
function TSheet.ColToVec(aCol, startRow, endRow: Integer): TwsVec;
var k, i: Integer;
begin
  k := 0;
  Result := TwsDFVec.Create(endRow - startRow + 1);
  for i := startRow to endRow do
    begin
    inc(k);
    Result[k] := getFloat(i, aCol);
    end
end;

function TSheet.RowToVec(aRow, startCol, endCol: Integer): TwsVec;
var k, i: Integer;
begin
  k := 0;
  Result := TwsDFVec.Create(endCol - startCol + 1);
  for i := startCol to endCol do
    begin
    inc(k);
    Result[k] := getFloat(aRow, i);
    end
end;
{$endif}

procedure TSheet.SetCellFont(aRow, aCol : Integer;
                             aFontName : String; aSize : Smallint;
                             aColor : TColor; isBold : Boolean; isItalic : Boolean);
var fs: TFontStyles;
     s: String;
     i: Smallint;
     c: word;
begin
  fs := [];
  with FSS.GetCellObject(aCol-1, aRow-1) do
    begin
    if isBold then include(fs, fsBold);
    if isItalic then include(fs, fsItalic);
    if aFontName = '' then s := Style.Font.Name else s := aFontName;
    if aSize = 0 then i := Style.Font.Size else i := aSize;
    c := xlsSetColor(aColor, FSSB.Palette, 0);
    Style.Font.AssignInfo(s, i, fs, Style.Font.Charset, c);
    Free();
    end;
end;

procedure TSheet.Copy;
begin
  {$IF dir_NivelDeRestricao < 1}
  FSS.Copy(FSS.SelectionRect, false);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSheet.Cut;
begin
  {$IF dir_NivelDeRestricao < 1}
  FSS.Copy(FSS.SelectionRect, true);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSheet.Paste;
begin
  {$IF dir_NivelDeRestricao < 1}
  FSS.Paste(FSS.ActiveCell);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSheet.Clear;
begin
  FSS.ClearAll();
end;

function TSheet.getTopLeftText: String;
begin
  Result := getDisplayText(FSS.Corners.Left+1, FSS.Corners.Top+1);
end;

procedure TSheet.setTopLeftText(const Value: String);
begin
  Write(FSS.Corners.Left+1, FSS.Corners.Top+1, Value);
end;

function TSheet.getSH: Boolean;
begin
  result := FSS.ShowHeaders;
end;

procedure TSheet.setSH(const Value: Boolean);
begin
  FSS.ShowHeaders := Value;
end;

function TSheet.GetColWidth(i: integer): Integer;
begin
  result := FSS.Cols.Size[i-1];
end;

procedure TSheet.SetColWidth(i: integer; const Value: Integer);
begin
  FSS.Cols.Size[i-1] := Value;
end;

function TSheet.getColCount: Integer;
begin
  result := FSS.ColumnCount;
end;

function TSheet.getRowCount: Integer;
begin
  result := FSS.RowCount;
end;

procedure TSheet.FormatCells(const aCells: TRect);
begin
  FSS.FormatCells(aCells);
end;

function TSheet.GetSelectionRect: TRect;
begin
  result := FSS.SelectionRect;
end;

procedure TSheet.SetSelectionRect(const Value: TRect);
begin
  FSS.SelectionRect := Value;
end;

procedure TSheet.BoldCell(aRow, aCol: Integer);
var fs: TFontStyles;
begin
  with FSS.GetCellObject(aCol-1, aRow-1) do
    begin
    fs := Style.Font.Style;
    include(fs, fsBold);
    Style.Font.Style := fs;
    Free();
    end;
end;

procedure TSheet.SetCellColor(aRow, aCol: Integer; aColor: TColor; IsBold: boolean = false);
var fs: TFontStyles;
begin
  with FSS.GetCellObject(aCol-1, aRow-1) do
    begin
    if IsBold then
       begin
       fs := Style.Font.Style;
       include(fs, fsBold);
       Style.Font.Style := fs;
       end;
    Style.Font.FontColor := xlsSetColor(aColor, FSSB.Palette, 0);
    Free();
    end;
end;

procedure TSheet.CopyAll();
begin
  FSS.Copy(Rect(0, 0, FSS.ContentColCount, FSS.ContentRowCount), false);
end;

procedure TSheet.BoldCol(aCol: Integer);
var i: Integer;
begin
  for i := 0 to FSS.ContentRowCount do
    self.BoldCell(i+1, aCol);
end;

procedure TSheet.BoldRow(aRow: Integer);
var i: Integer;
begin
  for i := 0 to FSS.ContentColCount do
    self.BoldCell(aRow, i+1);
end;

function TSheet.getCaption(): string;
begin
  result := Sheet.Caption;
end;

procedure TSheet.setCaption(const Value: string);
begin
  Sheet.Caption := Value;
end;

procedure TSheet.Merge(Row1, Col1, Row2, Col2: integer);
var r: TRect;
begin
  r := Rect(Col1-1, Row1-1, Col2-1, Row2-1);
  FSS.SetMergedState(r, true);
end;

procedure TSheet.WordWrap(aRow, aCol: Integer);
begin
  with FSS.GetCellObject(aCol-1, aRow-1) do
    begin
    Style.WordBreak := true;
    Free();
    end;
end;

function TSheet.getActiveCol(): Integer;
begin
  FActiveCol := FSS.ActiveCell.X + 1;
  result := FActiveCol;
end;

function TSheet.getActiveRow(): Integer;
begin
  FActiveRow := FSS.ActiveCell.Y + 1;
  result := FActiveRow;
end;

procedure TSheet.setActiveCol(const Value: integer);
begin
  FActiveCol := Value;
  FSS.ActiveCell := Point(FActiveRow-1, Value);
end;

procedure TSheet.setActiveRow(const Value: integer);
begin
  FActiveRow := Value;
  FSS.ActiveCell := Point(Value, FActiveCol-1);
end;

procedure TSheet.Write(const aText: string; IncRow, IncCol: boolean);
begin
  Write(FActiveRow, FActiveCol, aText);
  if incRow then inc(FActiveRow);
  if incCol then inc(FActiveCol);
end;

procedure TSheet.Write(const aValue: Real; IncRow, IncCol: boolean; DecimalPlaces: integer = 2);
begin
  Write(FActiveRow, FActiveCol, aValue, DecimalPlaces);
  if incRow then inc(FActiveRow);
  if incCol then inc(FActiveCol);
end;

function TSheet.GetRowHeight(i: integer): Integer;
begin
  result := FSS.Rows.Size[i-1];
end;

procedure TSheet.SetRowHeight(i: integer; const Value: Integer);
begin
  FSS.Rows.Size[i-1] := Value;
end;

procedure TSheet.Write(aRow, aCol, aSize, aColor: integer; isBold: Boolean; const aText: string);
var fs: TFontStyles;
     i: Smallint;
     c: word;
begin
  fs := [];
  with FSS.GetCellObject(aCol-1, aRow-1) do
    begin
    if FCenterText then
       begin
       Style.HorzTextAlign := cxSSTypes.haCENTER;
       Style.VertTextAlign := cxSSTypes.vaCENTER;
       end;

    SetCellText(aText, False);

    if isBold then include(fs, fsBold);
    if aSize = 0 then i := Style.Font.Size else i := aSize;
    c := xlsSetColor(aColor, FSSB.Palette, 0);
    Style.Font.AssignInfo(Style.Font.Name, i, fs, Style.Font.Charset, c);

    Free();
    end;
end;

procedure TSheet.Write(aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte);
begin
  Write(aRow, aCol, aSize, aColor, IsBold, toString(aValue, DecimalPlaces));
end;

procedure TSheet.WriteCenter(aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aText: string);
begin
  FCenterText := true;
  Write(aRow, aCol, aSize, aColor, IsBold, aText);
  FCenterText := false;
end;

procedure TSheet.WriteCenter(aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte);
begin
  FCenterText := true;
  Write(aRow, aCol, aSize, aColor, IsBold, toString(aValue, DecimalPlaces));
  FCenterText := false;
end;

procedure TSheet.Write(aRow, aCol: integer; IsBold: Boolean; const aText: string);
var fs: TFontStyles;
     o: TcxSSCellObject;
begin
  fs := [];
  o := FSS.GetCellObject(aCol-1, aRow-1);
  try
    if FCenterText then
       begin
       o.Style.HorzTextAlign := cxSSTypes.haCENTER;
       o.Style.VertTextAlign := cxSSTypes.vaCENTER;
       end;
    o.Text := aText;
    if isBold then include(fs, fsBold);
    o.Style.Font.AssignInfo(o.Style.Font.Name, o.Style.Font.Size, fs, o.Style.Font.Charset, o.Style.Font.FontColor);
  finally
    o.Free();
  end;  
end;

procedure TSheet.Write(aRow, aCol: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte);
begin
  Write(aRow, aCol, IsBold, toString(aValue, DecimalPlaces));
end;

procedure TSheet.WriteCenter(aRow, aCol: integer; IsBold: Boolean; const aText: string);
begin
  FCenterText := true;
  Write(aRow, aCol, IsBold, aText);
  FCenterText := false;
end;

procedure TSheet.WriteCenter(aRow, aCol: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte);
begin
  FCenterText := true;
  Write(aRow, aCol, IsBold, toString(aValue, DecimalPlaces));
  FCenterText := false;
end;

procedure TSheet.WriteCenter(const aText: string; IncRow, IncCol: boolean);
begin
  FCenterText := true;
  Write(aText, IncRow, IncCol);
  FCenterText := false;
end;

procedure TSheet.WriteCenter(const aValue: Real; IncRow, IncCol: boolean; DecimalPlaces: integer);
begin
  FCenterText := true;
  Write(aValue, IncRow, IncCol);
  FCenterText := false;
end;

end.

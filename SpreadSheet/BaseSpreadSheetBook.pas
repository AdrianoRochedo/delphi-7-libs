unit BaseSpreadSheetBook;

interface
uses Forms,
     SysUtilsEx,
     Types,
     {$ifdef WinStat}
     wsVec,
     {$endif}
     Graphics;

type
  TBaseSheet = class
  protected
    function GetSelectionRect: TRect; virtual; abstract;
    procedure SetSelectionRect(const Value: TRect); virtual; abstract;
    function getTopLeftText: String; virtual; abstract;
    procedure setTopLeftText(const Value: String); virtual; abstract;
    function getSH: Boolean; virtual; abstract;
    procedure setSH(const Value: Boolean); virtual; abstract;
    function GetColWidth(i: integer): Integer; virtual; abstract;
    procedure SetColWidth(i: integer; const Value: Integer); virtual; abstract;
    function getColCount: Integer; virtual; abstract;
    function getRowCount: Integer; virtual; abstract;
    function getCaption: string; virtual; abstract;
    procedure setCaption(const Value: string); virtual; abstract;
    procedure setActiveRow(const Value : integer); virtual; abstract;
    procedure setActiveCol(const Value : integer); virtual; abstract;
    function getActiveRow(): Integer; virtual; abstract;
    function getActiveCol(): Integer; virtual; abstract;
    function getRowHeight(i: integer): Integer; virtual; abstract;
    procedure setRowHeight(i: integer; const Value: Integer); virtual; abstract;
  public
    // Métodos de escrita ...

    procedure Write (const aText: string; IncRow, IncCol: boolean); overload; virtual; abstract;
    procedure Write (const aValue: Real; IncRow, IncCol: boolean; DecimalPlaces: integer = 2); overload; virtual; abstract;
    procedure Write (aRow, aCol: integer; const aText: string); overload; virtual; abstract;
    procedure Write (aRow, aCol: Integer; const aValue: Real; DecimalPlaces: byte = 2); overload; virtual; abstract;
    procedure Write (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aText: string); overload; virtual; abstract;
    procedure Write (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; virtual; abstract;
    procedure Write (aRow, aCol: integer; IsBold: Boolean; const aText: string); overload; virtual; abstract;
    procedure Write (aRow, aCol: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; virtual; abstract;

    procedure WriteCenter (const aText: string; IncRow, IncCol: boolean); overload; virtual; abstract;
    procedure WriteCenter (const aValue: Real; IncRow, IncCol: boolean; DecimalPlaces: integer = 2); overload; virtual; abstract;
    procedure WriteCenter (aRow, aCol: Integer; const aText: String); overload; virtual; abstract;
    procedure WriteCenter (aRow, aCol: Integer; const aValue: Real; DecimalPlaces: byte = 2); overload; virtual; abstract;
    procedure WriteCenter (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aText: string); overload; virtual; abstract;
    procedure WriteCenter (aRow, aCol, aSize, aColor: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; virtual; abstract;
    procedure WriteCenter (aRow, aCol: integer; IsBold: Boolean; const aText: string); overload; virtual; abstract;
    procedure WriteCenter (aRow, aCol: integer; IsBold: Boolean; const aValue: Real; DecimalPlaces: byte = 2); overload; virtual; abstract;

    procedure WriteVecInCol (aVec: IArray; aCol, startRow: Integer; DecimalPlaces: byte = 2); virtual; abstract;
    procedure WriteVecInRow (aVec: IArray; aRow, startCol: Integer; DecimalPlaces: byte = 2); virtual; abstract;

    // Valor das células ...
    
    function GetText        (aRow, aCol: Integer): String; virtual; abstract;
    function GetDisplayText (aRow, aCol: Integer): String; virtual; abstract;
    function GetFloat       (aRow, aCol: Integer): Real; virtual; abstract;
    function GetInt         (aRow, aCol: Integer): Integer; virtual; abstract;

    {$ifdef WinStat}
    // Converte uma linha/coluna para vetor
    function ColToVec (aCol, startRow, endRow: Integer): TwsVec; virtual; abstract;
    function RowToVec (aRow, startCol, endCol: Integer): TwsVec; virtual; abstract;
    {$endif}

    // Formatacao ..............................................................

    // se "aFontName" = '' valerá a fonte corrente
    // se "aSize" = 0 valerá o tamanho corrente
    procedure SetCellFont (aRow, aCol : Integer;
                           aFontName  : String;
                           aSize      : Smallint = 0;
                           aColor     : TColor   = clBlack;
                           isBold     : Boolean  = False;
                           isItalic   : Boolean  = False); virtual; abstract;

    procedure SetCellColor(aRow, aCol : Integer;
                           aColor     : TColor;
                           IsBold     : boolean = false); virtual; abstract;

    procedure BoldCell(aRow, aCol : Integer); virtual; abstract;
    procedure BoldRow(aRow: Integer); virtual; abstract;
    procedure BoldCol(aCol: Integer); virtual; abstract;

    // Formata um grupo de células através de um diálogo
    procedure FormatCells(const ACells: TRect); virtual; abstract;

    procedure Merge(Row1, Col1, Row2, Col2: integer); virtual; abstract;
    procedure WordWrap(aRow, aCol: Integer); virtual; abstract;

    // clipboard
    procedure Cut(); virtual; abstract;
    procedure Copy(); virtual; abstract;
    procedure CopyAll(); virtual; abstract;
    procedure Paste(); virtual; abstract;

    // métodos gerais
    procedure Clear(); virtual; abstract;

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

    // Retorna ou estabelece as células selecionadas
    property SelectionRect: TRect read GetSelectionRect write SetSelectionRect;

    // Células ativas
    property ActiveRow : integer read getActiveRow write setActiveRow;
    property ActiveCol : integer read getActiveCol write setActiveCol;
  end;

  TBaseSpreadSheetBook = class
  protected
    function getActiveSheet: TBaseSheet; virtual; abstract;
    function getActiveSheetIndex: integer; virtual; abstract;
    function getSheetCount: integer; virtual; abstract;
    procedure setActiveSheetIndex(const Value: integer); virtual; abstract;
    function getCaption: String; virtual; abstract;
    procedure SetCaption(const Value: String); virtual; abstract;
  public
    // Geral
    procedure Show(FormStyle: TFormStyle = fsNormal); virtual; abstract;
    procedure ShowModal(); virtual; abstract;
    procedure BeginUpdate(); virtual; abstract;
    procedure EndUpdate(); virtual; abstract;

    // leitura e salvamento
    procedure LoadFromFile(const FileName: String); virtual; abstract;
    procedure SaveToFile  (const FileName: String); virtual; abstract;

    // Folhas
    procedure NewSheet(const aPageName: string = ''); virtual; abstract;

    // Titulo da janela
    property Caption: String read getCaption write SetCaption;

    // Folhas
    property ActiveSheet      : TBaseSheet read getActiveSheet;
    property ActiveSheetIndex : integer read getActiveSheetIndex write setActiveSheetIndex;
    property SheetCount       : integer read getSheetCount;
  end;

implementation

end.

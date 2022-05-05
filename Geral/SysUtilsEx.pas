// Versão 1.36

{
  23/11/2000 - Rochedo
  Inserida as rotinas de Codificacao/decodificacao de streams

  23/12/2000 - Rochedo
  Inserida a rotina ValidaData()

  30/01/2001 - Rochedo
  Inserida a Rotina AjustDate()

  31/01/2001 - Rochedo
  Inserida as rotinas SetToStr() e LengthOfSet()

  06/03/2001 - Rochedo
  Bug fix em StringToStrings, agora está correta e cerca de 1000 X mais rápida

  19/04/2001 - Rochedo
  Bug fix em StringToStrings. Quando a string passada não continha separadores o primeiro
  elemento era a propria string e este elemento era removido.

  05/05/2001 - Rochedo
  Incluída a rotina LastChar()

  18/05/2001 - Rochedo
  Incluída a rotina PosR()

  12/11/2001 - Rochedo
  Incluída a rotina DeleteLastChar

  16/02/2002 - Rochedo
  Incluída a rotina Locate

  16/02/2002 - Rochedo
  Incluída a rotina HTTP_EncodeChars

  07/03/2002 - Rochedo
  Melhorias na rotina StringToStrings

  25/03/2001 - Rochedo
  Adicionada as rotinas:
    MultiLineToLine
    LineToMultiLine

  12/03/2003 - Rochedo
  Atualizadas e renomeadas as rotinas:
    - getInfo() --> SubStrings()
    - getSubString() --> SubString()

  23/06/2003 - Rochedo
  Adicionada a rotina:
    LoadFile(Name): TStrings;

  07/07/2003 - Rochedo
  Adicionada as rotina de conversao:
    - toString(int): string;
    - toString(float): string;

  01/08/2003 - Rochedo
  Adicionada as rotina de conversao:
    - StringToIntArray
    - StringToFloatArray

  24/12/2003 - Rochedo
    StringToStrings renomeada para Split

  04/03/2003 - Rochedo
    Criada as rotinas toPoint(): TPoint

  30/09/2004 - Rochedo
    Criada a classe TStringMat

  06/10/2004 - Rochedo
    - Inserido as rotinas dependentes somente da unidade Windows
      - ProcessMessages()
      - ShowMessage()
}

{$H+}
unit SysUtilsEx;

interface
uses Windows,
     Messages,
     SysUtils,
     DateUtils,
     Classes;

{Declarações de Tipos e Constantes -----------------------------------------------------}
const
  Separators : set of char = [#00,' ','-',#13, #10,'.',',','/','\', '#', '"', '''',
    ':','+','%','*','(',')',';','=','{','}','[',']', '{', '}', '<', '>'];

type
  TBit32       = 1..32;
  Str16        = String[16];
  TCharSet     = set of Char;
  TByteSet     = set of byte;
  TCharStr     = array[0..255] of Char;
  TIntArray    = array of Integer;
  TFloatArray  = array of Real;

Const
  K_RETURN                =  #13;
  K_ESC                   =  #27;
  K_SPACE                 =  #32;
  K_TAB                   =  #9;
  K_INSERT                =  #82;
  K_BACKSPACE             =  #8;
  K_DELETE                =  #83;
  K_LEFT                  =  #75;
  K_RIGHT                 =  #77;
  K_HOME                  =  #71;
  K_END                   =  #79;
  K_UP                    =  #72;
  K_DOWN                  =  #80;
  K_PGUP                  =  #73;
  K_PGDN                  =  #81;
  K_APOST                 =  #39;
  CRLF                    =  #13#10;

  Digitos    : TCharSet = ['0'..'9'];
  Maiusculas : TCharSet = ['A'..'Z'];
  Minusculas : TCharSet = ['a'..'z'];
  Controles  : TCharSet = [#8, #9, #13, #27];
  Espaco     : TCharSet = [K_SPACE ];
  Virgula    : TCharSet = [','];
  Ponto      : TCharSet = ['.'];
  Menos      : TCharSet = ['-'];

var
  DelChar  : TCharSet = [#9, #10, #13, ' ', ',', '=', '\', '"', '(', ')'];

  {----------------------------------------------------------------------------}

  {Rotinas Gerais -------------------------------------------------------------}

  // Se Trim: Remove os espacoes antes e depois do resultado da copia
  function Copy(const s: string; StartPos, Count: integer; Trim: boolean): string;

  Procedure Swap(var x1, x2: Integer); overLoad;
  Procedure Swap(var x1, x2: Double); overLoad;
  Procedure Swap(var x1, x2: TObject); overLoad;
  function  LengthOfSet(const s: TByteSet; Max: byte): byte;
  procedure SetApplicationDir(const Dir: String);
  function  GetApplicationDir: String;

  // Windows util
  procedure ProcessMessages();
  procedure ShowMessage(const msg: string);

  // Lê um arquivo e devolve uma StringList
  function LoadTextFile(const Name: string): TStrings;

  // Salva o ponto decimal corrente
  // Estabelece o novo ponto decimal como: "."
  procedure SaveDecimalSeparator();

  // Restaura o ponto decimal salvo
  procedure RestoreDecimalSeparator();

  {$ifndef VER130}
  procedure FreeAndNil(var Obj);
  {$endif}

  {Rotinas para Conversão de valores ---------------------------------------------------}

  Function SetToStr(var s: TByteSet): String;
  Function BooleanToStr(B: Boolean): String;
  Function StrToBoolean(s: String): Boolean;
  function StrToFloatDef(const s: String; const Default: Extended = 0): Extended;
  function AjustDate(const sDate, Format: String): String;

  function toString(const x: integer): string; overload;
  function toString(const x: boolean): string; overload;
  function toString(const x: real; DecimalPlaces: byte = 2): string; overload;

  function toInt(const s: string): integer; overload;
  function toInt(const s: string; Default: integer): integer; overload;
  function toInt(const s: string; Trim: boolean; Default: integer): integer; overload;
  function toInt(const s: string; StartPos, Count: integer; Trim: boolean; Default: integer): integer; overload;

  function toBoolean(const s: string): boolean; overload;
  function toBoolean(const i: integer): boolean; overload;

  function toFloat(const s: string): real; overload;
  function toFloat(const s: string; Default: real): real; overload;
  function toFloat(const s: string; StartPos, Count: integer; Trim: boolean; Default: real): real; overload;

  function toPoint(const s: string): TPoint; overload; // ex: 50,100
  function toPoint(const x, y: integer): TPoint; overload;
  function toPoint(const x, y: real): TPoint; overload;

  {Rotina de Erros ---------------------------------------------------------------------}
  procedure ShowException(Error: Word);
  procedure VirtualError(Sender: TObject; const MethodName: string);

  {Rotinas de Validação ----------------------------------------------------------------}
  Function  ValidateChar(C: Char; CC: Array of TCharSet; Beep: Boolean): Char;
  Function  ValidaData(const strData: String; var Data: TDateTime): Boolean;
  Function  IsDigit(c: Char): Boolean;

  {Rotinas para Manípulação de Strings -------------------------------------------------}
  function  LTrimZeros(Const S: String): String;
  function  Pos(const aSubstr, S: String; aOfs: Integer): Integer;
  function  PosR(const aSubstr, S: String): Integer;
  function  LeftDelimiter(Const S, Delim: String): String;
  function  Alltrim(Const S: String): String;
  function  RTrim(Const S :String; Const DelChar: TCharSet): String;
  function  DeleteSubStr( SourceStr,BeginChars,EndChars : String): String;
  function  ReplaceSubStr( SourceStr,OldChars,newChars : String): String;
  function  StrDeleteSubStr( SourceStr,BeginChars,EndChars : PChar): PChar;
  function  StrReplaceSubStr( SourceStr,OldChars,newChars : PChar): PChar;
  function  SubString(Const SourceStr, BeginChar, EndChar: String): String; overload;
  function  SubString(Const SourceStr: String; BeginIndex, EndIndex: Integer): String; overload;
  function  StrToken(const P: String; var J: Cardinal; Separators: TCharSet): String;
  Function  RemoveDuplicates(SL : TStrings) : TStrings;
  Function  StrCenter (Const S: String; W: Integer): String;
  Function  ChangeChar(Const S: String; co, cd: Char): String;
  Function  IsValidIdent(const Ident: string): Boolean;
  Function  LastChar(const s: String): Char;
  Function  Reverse(const s: String): string;
  procedure DeleteLastChar(var s: String);

  // Retorna a string que fica a esquerda de "RightStr"
  // Se "TrimResult" remove os espaços iniciais e finais do resultado.
  function LeftStringOf(const s, RightStr: String; TrimResult: Boolean = false): String;

  // Retorna a string que fica a direita de "LeftStr"
  // Se "TrimResult" remove os espaços iniciais e finais do resultado.
  function RigthStringOf(const s, LeftStr: String; TrimResult: Boolean = false): String;

  function Encrypt(const S: String; Key: Word = 1111): String; overload;
  function Encrypt(const S, Key: string) : string; overload;
  function Decrypt(const S: String; Key: Word = 1111): String;

  // Estas rotinas codificam e decodificam um stream em memória
  procedure EncryptStream(Stream: TMemoryStream; Key: Word = 1111);
  procedure DecryptStream(Stream: TMemoryStream; Key: Word = 1111);

  function  AddCharL(C: Char; const S: string; N: Integer): string;
  function  AddCharR(C: Char; const S: string; N: Integer): string;
  function  LeftStr(const S: string; N: Integer; RightChar: char = ' '): string;
  function  RightStr(const S: string; N: Integer; LaftChar: char = ' '): string;
  function  WordCount(const S: string; WordDelims: TCharSet): Integer;
  function  strPadZeroL(const S: String; Len: Integer): String;
  procedure RemoveChar(var s: string; car : char);

  {Ex: 'abc   def  gh' -->  'abc def gh'}
  function DelSpacesToOne(const S: string): string;

  {Ex: '  ab c d ef ' --> 'abcdef'}
  function DelSpaces(const S: string): string;

  // Pega as duas partes de uma string dado um separador
  // Retorna a posição do Separador
  // Se "TrimResult" retira os espaços em branco do inicio e do final de cada parte
  Function SubStrings(const Sep: String;
                        out S1, S2: String;
                      const S: String;
                      const TrimResult: boolean = false): Integer; overload;

  function SubStrings(const s, sStart, sEnd: string; EnableDuplicates: boolean): TStrings; overload;

  {A partir de um nome qualquer gera um identificador válido.
  Troca qualquer caractere inválido pelo caractere "_" (sublinha) alexdg (04/09/1998)}
  Function GetValidId(Const s: String): String;

  // Cria uma única string a partir de uma lista
  function StringsToString(SL: TStrings; Separator: Char = ' '): String;

  // Localiza uma SubString em um texto a partir de uma linha.
  function Locate(const SubString: String;
                  Text: TStrings;
                  var OffSet: Integer;
                  CaseSensitive: Boolean = False;
                  FromEnd: Boolean = False): Boolean;

  // Quebra uma String em n pedaços de acordo com os separadores
  // Se espaco esta definido nos separadores, os espacos inuteis sao eliminados
  // deixando apenas um espaco entre as palavras.
  procedure Split(s: String;
                  var outStrings: TStrings;
                  Seps: TCharSet);

  // Quebra um pChar em n pedaços de acordo com os separadores
  procedure PCharToStrList(const s: String;
                           outStrings: TStrings;
                           Seps: TCharSet);

  {Rotinas de codificação de caracteres}
  function HTTP_EncodeChars(const s: String): String;

  function MultiLineToLine(const s: String): String;
  function LineToMultiLine(const s: String): String;

  {Rotinas de manipulação de tempo -----------------------------------------------------}
  Function TimeToStr(Time: TDateTime): String;

  {Funções Matemáticas -----------------------------------------------------------------}
  Function iPow(x, y: Word): Longint;
  Function Mean(x, y: Double): Double;

  {Rotinas Binárias -------------------------------------------------------------------}
  Function BitIsOn(n: Longint; Bit: TBit32): Boolean;
  Function BitOn(n: Longint; Bit: TBit32): Longint;
  Function BitOff(n: Longint; Bit: TBit32): Longint;
  Function BinStrToWord  (Const Value: Str16): Word;

  function KeyPressed(VK : integer) : boolean;
  function SubStr(const S : string; const index : integer; const Separator : string) : string;
  function CharInSet(const Ch : Char; const SetOfChar : TCharSet) : boolean;
  function GetWordOnPos(const S : string; const P : integer) : string;
  function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;
  function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;

  // Conta o numero de ocorrencias de < Ch > em < S >
  // Esta funcao eh CaseSensitive
  function CharCount(const Ch: Char; const S: string): integer;

  function HasChar(const Ch : Char; const S : string) : boolean;
  function HasAnyChar(const Chars : string; const S : string) : boolean;
  function Spaces(const N : integer) : string;
  procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
  function Cmp(const S1, S2 : string) : boolean;

  // Manipulação de Datas
  procedure DateDiff(Date1, Date2 : TDateTime; var Days, Months, Years : Word);

  // Arrays
  procedure Fill(var x: TFloatArray; const Value: real); overload;
  procedure Fill(var x: TIntArray; const Value: integer); overload;
  procedure Divide(var x: TFloatArray; const Value: real);
  function StringToIntArray(const s: string; Seps: TCharSet): TIntArray;
  function StringToFloatArray(const s: string; Seps: TCharSet): TFloatArray;

type
  IInterface = interface
    function ToString(): wideString;
    procedure Release();
  end;

  IDisposable = interface
    procedure BeforeDestruction();
  end;

  IArray = interface
    ['{3C6F6EC5-012B-4CC2-A051-8E4468B6BB78}']
    function Low(): integer;
    function High(): integer;
    function IsMissValue(i: integer; out x: double): boolean;
    function getAsString(i: integer): string;
    function getAsInteger(i: integer): integer;
    function getAsFloat(i: integer): double;
    procedure setAsString(i: integer; value: string);
    procedure setAsInteger(i: integer; value: integer);
    procedure setAsFloat(i: integer; value: double);
  end;

  // Os indices deverao ser baseados em 1
  IMatrix = interface
    ['{8B39B227-D3C1-4A2F-AFC3-2325D657E8DB}']
    function RowCount(): integer;
    function ColCount(): integer;
    function IsMissValue(i, j: integer; out x: double): boolean;
    function getAsString(i, j: integer): string;
    function getAsInteger(i, j: integer): integer;
    function getAsFloat(i, j: integer): double;
    procedure setAsString(i, j: integer; value: string);
    procedure setAsInteger(i, j: integer; value: integer);
    procedure setAsFloat(i, j: integer; value: double);
  end;

  IVisualizable = interface
    ['{5A9182DB-3BA9-4580-877C-562D9819F4CA}']
    procedure Show();
  end;

  IVisualizableInSheet = interface
    ['{19ED9082-73F0-456B-B1FE-28434EC8D6C8}']
    procedure ShowInSheet();
  end;

  IVisualizableInChart = interface
    ['{F3F22589-B44D-41AB-A928-05787306026C}']
    procedure ShowInChart();
  end;

  IEditable = interface
    ['{A40516F2-5FFC-4D6F-9F42-8E64281F3170}']
    procedure Edit();
  end;

  IProgressBar = interface
    ['{86AC9BAC-C5DF-4447-AA49-16D8F0DEDE01}']
    procedure setMin(value: integer);
    procedure setMax(value: integer);
    procedure setValue(value: integer);
    procedure setMessage(const s: string);
  end;

  TFontAttributesRec = record
    FontName : string;
    Color : integer;
    Size : integer;
  end;

  IWriter = interface
    ['{0C2BE26C-3205-4954-B837-B9CC10A163FC}']
    procedure Write(const s: string); 
  end;

  ITextWriter = interface(IWriter)
    ['{B2183BF0-8953-4D02-8CB3-B9E034A08319}']
    procedure Write(const s, FontName: string; const Color, Size: integer); overload;
    procedure Write(const s: string; const FontAttributes: TFontAttributesRec); overload;
  end;

  ISQL = interface
    ['{90C78E33-DC24-4C6D-802F-120EC8F94710}']
    procedure setTitle(const Title: string);
    function getTitle(): string;
    procedure setDescription(const Desc: string);
    function getDescription(): string;
    procedure setFilename(const Filename: string);
    function getFilename(): string;
    procedure setLocked(const Value: boolean);
    function getLocked(): boolean;
    procedure setSQL(const SQL: string);
    function getSQL(): string;
    function Execute(const Command: string; out rowsAffected: integer): string;
  end;

  // NRC = no reference-counting
  // Os objetos descendentes desta classe não são liberados automaticamente pois
  // não há contagem de referências.
  T_NRC_InterfacedObject = class(TObject, System.IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

{ THandledObject }
  THandledObject = class(TObject)
  protected
    FHandle: THandle;
  public
    destructor Destroy; override;
    property Handle: THandle read FHandle;
  end;

{ TSharedMem }

{ Esta classe simplifica o processo de criação de uma região de memória compartilhada.
  No Win32, isto é conseguido utilizando as funções CreateFileMapping e MapViewOfFile. }

  TSharedMem = class(THandledObject)
  private
    FName: string;
    FSize: Integer;
    FCreated: Boolean;
    FFileView: Pointer;
    function getString: String;
    procedure setString(const Value: String);
  public
    constructor Create(const Name: string; Size: Integer);
    destructor Destroy; override;

    property Name    : string read FName;
    property Size    : Integer read FSize;
    property Buffer  : Pointer read FFileView;
    property Created : Boolean read FCreated;

    property AsString : String read getString write setString;
  end;

  TCryptStrings = class(TStringList)
  private
    FKey: Word;
  public
    constructor Create(Key: Word);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Key: Word read FKey write FKey;
  end;

  TStringList = class(Classes.TStringList)
  public
    constructor CreateFrom(const Items: array of string);
  end;

  // Representa uma matriz de strings
  // Os indices iniciam em 1
  TStringMat = class(T_NRC_InterfacedObject, IMatrix)
  private
    FArray: array of array of string;
    FCheckBounds: boolean;

    // Excessoes
    procedure InvalidRowIndex(row: integer);
    procedure InvalidColIndex(row, col: integer);

    procedure InternalCheckBounds(i, j: integer);

    function RowCount(): integer;
    function ColCount(): integer;
    function IsMissValue(i, j: integer; out x: double): boolean;
    function getAsString(i, j: integer): string;
    function getAsInteger(i, j: integer): integer;
    function getAsFloat(i, j: integer): double;
    procedure setAsString(i, j: integer; value: string);
    procedure setAsInteger(i, j: integer; value: integer);
    procedure setAsFloat(i, j: integer; value: double);
    function getSizeOfRow(row: integer): integer;
    function getItem(row, col: integer): string;
    procedure setColCount(const Value: integer);
    procedure setSizeOfRow(row: integer; const Value: integer);
    procedure setItem(row, col: integer; const Value: string);
    procedure setRowCount(const Value: integer);
  public
    // Cria a matriz baseado em um arquivo onde o separador de elementos eh o ";"
    constructor Create(const Filename: string); overload;

    // Cria uma matriz R x C
    constructor Create(RowCount, ColCount: integer); overload;

    // Verifica de todas as linhas possuem o mesmo tamanho
    function SizeOfRowsAreEquals(): boolean;

    // Celula da matriz
    // O acesso a uma celula invalida gerara uma excessao
    property Item[row, col: integer] : string read getItem write setItem; default;

    // Em um acesso, retorna o tamanho da 1. linha
    // Em uma atribuicao, torna todas as linhas com o mesmo tamanho
    // Para saber se todas as linhas possuem o mesmo tamanho utiliza a funcao "SizeOfRowsAreEquals"
    property Cols : integer read ColCount write setColCount;

    // Informa ou estabelece o numero de linhas da matriz
    // As linhas adicionais serao criadas com o numero de elementos da 1.linha
    property Rows : integer read RowCount write setRowCount;

    // Informa ou estabelece o numero de elementos de uma linha da matriz
    property SizeOfRow[row: integer] : integer read getSizeOfRow write setSizeOfRow;

    // Realiza ou nao a validacao dos limites nos acessos aos elementos da matriz
    // Por falta eh "true"
    property CheckBounds : boolean read FCheckBounds write FCheckBounds;
  end;

implementation

Function ValidateChar(C: Char; CC: Array of TCharSet; Beep: Boolean): Char;
Var i: Byte;
    CS: TCharSet;
Begin
  CS := [];
  For i := 0 to High(CC) do CS := CS + CC[i];
  If (C in CS) Then
    Result := C
  Else
    Begin
    Result := #0;
    If Beep Then MessageBeep(0);
    End;
End;

Function  ValidaData(const strData: String; var Data: TDateTime): Boolean;
begin
  try
    Data := StrToDate(strData);
    Result := True;
  except
    Result := False;
  end;
end;

Function IsDigit(c: Char): Boolean;
Begin
  Case c of
    '0'..'9' : Result := True;
  Else
    Result := False;
  End;
End;

{--------------------------  Rotinas de Conversão -------------------------}

Function BooleanToStr(B: Boolean): String;
Begin
  If B Then Result := 'Sim' Else Result := 'Não';
End;

Function StrToBoolean(s: String): Boolean;
Begin
  s := Uppercase(s);
  Result := (s = '1')          or
            (s = 'SIM')        or
            (s = 'TRUE')       or
            (s = 'VERDADEIRO') or
            (s = 'S')          or
            (s = 'T');
End;

function StrToFloatDef(const s: String; const Default: Extended = 0): Extended;
begin
  try
    Result := SysUtils.StrToFloat(s);
  except
    Result := Default;
  end;
end;

{----------------------------  Rotinas de Erro -----------------------------}

Procedure ShowException(Error: Word);
Begin
  Raise Exception.CreateRes(Error);
End;

{Exponenciação Positiva e Inteira}
Function IPow( x,y: Word): Longint;
Begin
  try
    Result := Trunc(exp( y * ln(x)));
  Except
    Exception.Create('Exponenciação não definida');
  End;
End;

{Verifica se um determinado Bit está ligado}
Function BitIsOn(n: Longint; Bit: TBit32): Boolean;
Begin
  Result := Boolean(n and IPow(2, Bit-1));
End;

{Liga um determinado Bit}
Function BitOn(n: Longint; Bit: TBit32): Longint;
Begin
  Result := n or IPow(2, Bit-1);
End;

{Desliga um determinado Bit}
Function BitOff(n: Longint; Bit: TBit32): Longint;
Begin
  If BitIsOn(n, Bit) Then
     Result := n xor IPow(2, Bit-1)
  Else
     Result := n;
End;

Function BinStrToWord  (Const Value: Str16): Word;
Var i : Integer;
Begin
  Result := 0;
  For i:=1 to Length(Value) do
    Begin
    Result := Result shl 1;
    If Value[i] <> '0' then Inc(Result);
    End;
End;

Function LTrimZeros(Const S: String): String;
Var i : Integer;
Begin
  i := 1;
  While S[i] = '0' do Inc(i);
  If i<= Length(S) Then
     Result := System.Copy(S, i, Length(S))
  Else
     Result := '0';
End;

Function LeftDelimiter(Const S, Delim: String): String;
Begin
  Result := System.Copy(S, 1, System.Pos(Delim, S+Delim)-1);
End;

Function Alltrim(Const S: String): String;
Var I, F: Integer;
Begin
  If S <> '' Then
     If (S[1] in [#32, #9]) or (S[Length(S)] in [#32, #9]) Then
        Begin
        I := 1; F := Length(S);
        While (I < F ) and (S[I] in [#32, #9]) do Inc(I);
        While (F >= I) and (S[F] in [#32, #9]) do Dec(F);
        Result := System.Copy(S, I, F-I+1);
        End
     Else
        Result := S
  Else
     Result := '';
End;

Function RTrim(Const S :String; Const DelChar: TCharSet): String;
Var i : Byte;
    Aux : Byte;
Begin
  Result := S;
  Aux := Length(s);
  For i := Length(s) DownTo 1 do
    If S[i] in DelChar Then Dec(Aux) Else Break;

{$IFDEF WIN32}
  SetLength(Result, Aux);
{$ELSE}
  Result[0] := Chr(Aux);
{$ENDIF}
End;

{
Utilize RightStr
Function StrRight(const S: String; Len: Integer): String;
begin
  if Len >= Length(S) then
    Result := S
  else
    Result := System.Copy(S, Succ(Length(S))-Len, Len);
end;
}
function StrReplaceSubStr( SourceStr,OldChars,newChars : PChar): PChar;
var
   TargetStr         : PChar;
   SourcePos         : PChar;
   TargetCurrentChar : PChar;
   newCharPos        : PChar;
   Replaced          : Boolean;

begin
     if StrLen(SourceStr) > 1 then
     begin
          TargetStr         := StrAlloc(32768);
          TargetStr^        := #0;
          SourcePos         := SourceStr;
          TargetCurrentChar := TargetStr;
          while SourcePos^ <> #0 do
          begin
               Replaced := false;
               if SourcePos^ = OldChars^ then
               begin
                    if StrPos(SourcePos,OldChars) = SourcePos then
                    begin
                         Inc(SourcePos,StrLen(OldChars));
                         newCharPos := newChars;
                         while newCharPos^ <> #0 do
                         begin
                              TargetCurrentChar^ := newCharPos^;
                              inc(newCharPos);
                              inc(TargetCurrentChar);
                         end;
                         Replaced := true;
                    end;
               end;
               TargetCurrentChar^ := SourcePos^;
               if not Replaced then
               begin
                    inc(SourcePos);
                    inc(TargetCurrentChar);
               end;
          end;
          TargetCurrentChar^ := #0;
          StrLCopy(SourceStr,TargetStr,StrLen(SourceStr));
          StrDispose(TargetStr);
     end;
     Result := SourceStr;
end;

function StrDeleteSubStr( SourceStr,BeginChars,EndChars : PChar): PChar;
var
   TargetStr         : PChar;
   SourcePos         : PChar;
   TargetCurrentChar : PChar;

begin
     if StrLen(SourceStr) > 1 then
     begin
          TargetStr         := StrAlloc(32768);
          TargetStr^        := #0;
          SourcePos         := SourceStr;
          TargetCurrentChar := TargetStr;
          while SourcePos^ <> #0 do
          begin
               if SourcePos^ = BeginChars^ then
               begin
                    if StrPos(SourcePos,BeginChars) = SourcePos then
                    begin
                         while ((StrPos(SourcePos,EndChars) <> SourcePos) and
                                (SourcePos^ <> #0)) do
                         begin
                              Inc(SourcePos);
                         end;
                         if StrPos(SourcePos,EndChars) = SourcePos then
                         begin
                              Inc(SourcePos);
                         end;
                    end;
               end;
               TargetCurrentChar^ := SourcePos^;
               inc(SourcePos);
               inc(TargetCurrentChar);
          end;
          TargetCurrentChar^ := #0;
          StrCopy(SourceStr,TargetStr);
          StrDispose(TargetStr);
     end;
     Result := SourceStr;
end;

function ReplaceSubStr( SourceStr,OldChars,newChars : String): String;
var
   aPChar1  : PChar;
   aPChar2  : PChar;
   aPChar3  : PChar;
   newPChar : PChar;
begin
  aPChar1 := StrAlloc(Length(SourceStr)+1);
  aPChar2 := StrAlloc(Length(OldChars)+1);
  aPChar3 := StrAlloc(Length(newChars)+1);
  StrPCopy(aPChar1,SourceStr);
  StrPCopy(aPChar2,OldChars);
  StrPCopy(aPChar3,newChars);
  newPChar := StrReplaceSubStr(aPChar1,aPChar2,aPChar3);
  Result   := StrPas(newPChar);
  StrDispose(aPChar1);
  StrDispose(aPChar2);
  StrDispose(aPChar3);
end;

function DeleteSubStr( SourceStr,BeginChars,EndChars : String): String;
var
   aPChar1  : PChar;
   aPChar2  : PChar;
   aPChar3  : PChar;
   newPChar : PChar;
begin
  aPChar1 := StrAlloc(Length(SourceStr)+1);
  aPChar2 := StrAlloc(Length(BeginChars)+1);
  aPChar3 := StrAlloc(Length(EndChars)+1);
  StrPCopy(aPChar1,SourceStr);
  StrPCopy(aPChar2,BeginChars);
  StrPCopy(aPChar3,EndChars);
  newPChar := StrDeleteSubStr(aPChar1,aPChar2,aPChar3);
  Result   := StrPas(newPChar);
  StrDispose(aPChar1);
  StrDispose(aPChar2);
  StrDispose(aPChar3);
end;

function SubString(Const SourceStr, BeginChar, EndChar: String): String;
Var PosIni, PosFim : Integer;
Begin
  PosIni := System.Pos(BeginChar, SourceStr);
  PosFim := PosR(EndChar, SourceStr);
  if (PosIni > 0) and (PosFim > 0) then
     Result := System.Copy(SourceStr, PosIni + 1, PosFim - PosIni - 1)
  else
     Result := '';
End;

function SubString(Const SourceStr: String; BeginIndex, EndIndex: Integer): String; overload;
begin
  if (BeginIndex > 0) and (EndIndex > 0) then
     Result := System.Copy(SourceStr, BeginIndex + 1, EndIndex - BeginIndex - 1)
  else
     Result := '';
end;

function StrToken(const P: String; var J: Cardinal; Separators: TCharSet): String;
var I, Len: Cardinal;
begin
  Len := Length(P);

  { Pulando os delimitadores do inicio }
  while (J <= Len) and (P[J] in Separators) do Inc(J); I := J;

  { Quantos serao copiados ?}
  while (J <= Len) and not (P[J] in Separators) do Inc(J);

  { Se copiou algum, retorna }
  if I < J then
    Result := System.Copy(P, I, J - I)
  else
    Result := ''
end;

function StringsToString(SL: TStrings; Separator: Char = ' '): String;
var i: Integer;
begin
  if SL.Count = 0 then
     Result := ''
  else
     Result := SL[0];

  for i := 1 to SL.Count-1 do
     Result := Result + Separator + SL[i]
end;

procedure Split(s: String;
                var outStrings: TStrings;
                Seps: TCharSet);

var i, bc, Len, Start: Integer;
Begin
  if outStrings = nil then
     outStrings := TStringList.Create();

  If (s <> '') Then
     begin
     if (#32 in Seps) then // Normaliza a string
        s := DelSpacesToOne(AllTrim(s));

     // Limpa a lista
     outStrings.Clear();

     i := 1;
     Len := Length(s);
     while i <= Len do
       begin
       Start := i;
       for bc := i to Len do
         begin
         inc(i);
         if (s[bc] in Seps) then Break;
         end;
       outStrings.Add( System.Copy(s, Start, bc-Start) );
       end;

     if (s[Len] in Seps) then outStrings.Add('');
     end;
End;

procedure PCharToStrList(const s: String;
                         outStrings: TStrings;
                         Seps: TCharSet);
var i: Cardinal;
    st: String;
begin
  i := 1;
  outStrings.Clear;
  st := strToken(s, i, Seps);
  while st <> '' do
    begin
    outStrings.Add(st);
    st := strToken(s, i, Seps);
    end;
end;

{$R-}
Function RemoveDuplicates(SL : TStrings): TStrings;
Type TMarcado = Array[0..0] of Boolean;
     PMarcado = ^TMarcado;

Var i,j     : Integer;
    H       : THandle;
    Marcado : PMarcado;
Begin
  Try
    {Alocação de memória para os arrays}
    H := GlobalAlloc(GMem_Fixed, SL.Count);
    Marcado := GlobalLock(H);

    {Inicialização do array}
    For i := 0 To SL.Count-1 do
      Marcado^[i] := False;

    {Marcação das Strings repetidas}
    For i := 0 to SL.Count-2 do
      Begin
      If Marcado^[i] Then Continue;
      For j := i+1 To SL.Count-1 do
        If CompareText(SL.Strings[i], SL.Strings[j]) = 0 Then
           Marcado^[j] := True;
      End;

    {Retirada das Strings repetidas}
    Result := TStringList.Create;
    For i := 0 To Sl.Count-1 do
      If Not Marcado^[i] Then
         Result.Add(SL.Strings[i]);

  Finally
    GlobalUnLock(H);
    GlobalFree(H);
  End;
End;
{$R+}

Function StrCenter(Const S: String; W: Integer): String;
Var n : Integer;
Begin
  n := (W Div 2) - (Length(S) Div 2);
  Result := StringOfChar(' ', n) + S;
End;

Function ChangeChar(Const S: String; co, cd: Char): String;
var i: Integer;
Begin
  Result := S;
  For i := 1 to Length(Result) do
    If Result[i] = co Then Result[i] := cd;
End;

// Pega as duas partes de uma string dado um separador
// Retorna a posição do Separador
Function SubStrings(const Sep: String;
                      out S1, S2: String;
                    const S: String;
                    const TrimResult: Boolean = false): Integer;
Begin
  Result := System.Pos(Sep, S);
  S1 := System.Copy(S, 1, Result-1);
  S2 := System.Copy(S, Result+1, Length(S));
  if TrimResult then
     begin
     S1 := AllTrim(S1);
     S2 := AllTrim(S2);
     end;
End;

(*****************************************************************************
alexdg (04/09/1998)
Função:
A partir de um nome qualquer gera um identificador válido.
Troca qualquer caractere inválido pelo caractere "_" (sublinha);
Parametros:
s - nome a ser analisado.
Retorno:
Retorna o id válido.
*)
Function GetValidId(Const s: string): String;

  Function isCharAlfa(c: byte): Boolean;
  Begin
    Result := ((c >= 65) and (c <= 90 )) or
              ((c >= 97) and (c <= 122)) or
              (chr(c) = '_');
  End;

var i   : integer;
    Cod : Integer;
Begin
  if s = '' Then exit;
  Result := s;
  if not isCharAlfa(ord(Result[1])) Then Insert('_', Result, 1);
  for i := 2 to Length(s) do
    begin
    cod := ord(Result[i]);
    if not isCharAlfa(cod) and not ((Cod >= 48) and (Cod <= 57 )) Then Result[i] := '_';
    end;
End;

{******************************************************************************}
(* alexdg 16/07/1998
Retira da string s todos os caracteres iguais a car.
*)
Procedure RemoveChar(var s: string; car : char);
var s_aux : string;
    i     : integer;
Begin
  For i := 1 To Length(s) Do
    If s[i] <> car Then
       s_aux := s_aux + s[i];
  s := s_aux;
End; {RemoveChar}

procedure Swap(var x1, x2: Integer);
var I: Integer;
begin
  I := x1; x1 := x2; x2 := I;
end;

Procedure Swap(var x1, x2: Double);
var I: Double;
begin
  I := x1; x1 := x2; x2 := I;
end;

Procedure Swap(var x1, x2: TObject);
var I: TObject;
begin
  I := x1; x1 := x2; x2 := I;
end;

{$ifndef VER130}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;
{$endif}

Function Mean(x, y: Double): Double;
Begin
  Result := (x + y) / 2;
End;

{Só que esta aqui aceita o ponto...}
function IsValidIdent(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_', '.'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  I := Length(Ident);
  if (I = 0) or not (Ident[1] in Alpha) or
     (Ident[1] = '.') or (Ident[I] = '.') then Exit;

  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;

Function TimeToStr(Time: TDateTime): String;
var  HH,  MM,  SS,  MS : Word;
    sHH, sMM, sSS      : String[5];
begin
  DecodeTime(Time, HH, MM, SS, MS);
  inc(HH, Trunc(int(Time) * 24 {horas}));
  sHH := intToStr(HH);
  sMM := intToStr(MM);
  sSS := intToStr(SS);
  If Length(sHH) = 1 Then sHH := '0' + sHH;
  If Length(sMM) = 1 Then sMM := '0' + sMM;
  If Length(sSS) = 1 Then sSS := '0' + sSS;
  Result := sHH + TimeSeparator + sMM + TimeSeparator + sSS;
end;

const
  C1: word = 52845;
  C2: word = 22719;

function Encrypt(const S: String; Key: Word = 1111): String;
var I: Integer;
begin
  SetLength(Result, Length(S));

  for I := 1 to Length(S) do
    begin
    Result[I] := Char(Ord(S[I]) xor (Key shr 8));
   {$OVERFLOWCHECKS ON}
    Key := Word((Ord(Result[I]) + Key) * C1 + C2);
   {$OVERFLOWCHECKS OFF}
    end;
end;

procedure EncryptStream(Stream: TMemoryStream; Key: Word = 1111);
var I: Integer;
    p: pChar;
begin
  Stream.Position := 0;
  p := Stream.Memory;
  for I := 0 to Stream.Size-1 do
    begin
    p[I] := Char(Ord(p[I]) xor (Key shr 8));
   {$OVERFLOWCHECKS ON}
    Key := Word((Ord(p[I]) + Key) * C1 + C2);
   {$OVERFLOWCHECKS OFF}
    end;
end;

function Decrypt(const S: String; Key: Word = 1111): String;
var I: Integer;
begin
  SetLength(Result,Length(S));

  for I := 1 to Length(S) do
    begin
    Result[I] := char(Ord(S[I]) xor (Key shr 8));
    {$OVERFLOWCHECKS ON}
    Key := Word((Ord(S[I]) + Key) * C1 + C2);
    {$OVERFLOWCHECKS OFF}
    end;
end;

procedure DecryptStream(Stream: TMemoryStream; Key: Word = 1111);
var I: Integer;
    p: pChar;
    c: Char;
begin
  Stream.Position := 0;
  p := Stream.Memory;
  for I := 0 to Stream.Size-1 do
    begin
    c := p[I];
    p[I] := char(Ord(c) xor (Key shr 8));
    {$OVERFLOWCHECKS ON}
    Key := Word((Ord(c) + Key) * C1 + C2);
    {$OVERFLOWCHECKS OFF}
    end;
end;

function Encrypt(const S, Key : string) : string;
var
  I, Z, X: integer;
  k: real;
  C : char;
  Code : byte;
  ss: string;
begin
  // New Key
  ss := Key;
  k := 0.7765;
  for i := 1 to Length(ss) do
    k := (k + ord(ss[i])) * 0.5242487;
  ss := Reverse(toString(k * 0.838736736, 15));
  ChangeChar(ss, ',', '.');
  for i := 1 to Length(ss)-1 do
    begin
    ss[i] := Chr(ord(ss[i+1]) xor ord(ss[i]));
    if ord(ss[i]) < 55 then ss[i] := chr(ord(ss[i]) + 10)
    end;

  // Encryptation
  Result:= '';
  Z:= length(ss);
  if (Z > 0) and (length(S) > 0) then
    for I:= 1 to length(S) do
      begin
      Code:= ord(ss[(I - 1) mod Z + 1]);
      if S[I] >= #128 then
        C:= Chr(ord(S[I]) xor (Code and $7F))
      else if S[I] >= #64 then
        C:= Chr(ord(S[I]) xor (Code and $3F))
      else if S[I] >= #32 then
        C:= Chr(ord(S[I]) xor (Code and $1F))
      else
        C:= S[I];
      Result:= Result + C;
      end;
end;

// deleta espacos e tabs
function DelSpacesToOne(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 2 do begin
    if (Result[I] in [#9, #32]) and (Result[I - 1] in [#9, #32]) then
      Delete(Result, I, 1);
  end;
end;

// deleta espacos e tabs
function DelSpaces(const S: string): string;
var
  i, Tam: Integer;
begin
  Tam := 0;
  for i := 1 to Length(s) do
    if not (s[i] in [#9, #32]) then inc(Tam);

  SetLength(Result, Tam);
  Tam := 0;
  for i := 1 to Length(s) do
    if not (s[i] in [#9, #32]) then
       begin
       inc(Tam);
       Result[Tam] := s[i];
       end;
end;

function AddCharL(C: Char; const S: string; N: Integer): string;
begin
  result := StringOfChar(C, N - Length(S)) + S;
end;

function AddCharR(C: Char; const S: string; N: Integer): string;
begin
  Result := S + StringOfChar(C, N - Length(S));
end;

function LeftStr(const S: string; N: Integer; RightChar: char = ' '): string;
begin
  Result := AddCharR(RightChar, S, N);
  if Length(Result) > N then
     Result := System.Copy(Result, 1, N);
end;

function RightStr(const S: string; N: Integer; LaftChar: char = ' '): string;
begin
  Result := AddCharL(LaftChar, S, N);
  if Length(Result) > N then
     Result := System.Copy(Result, 1, N);
end;

function WordCount(const S: string; WordDelims: TCharSet): Integer;
var
  SLen : Cardinal;
  I : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do begin
    while (I <= SLen) and (S[I] in WordDelims) do Inc(I);
    if I <= SLen then Inc(Result);
    while (I <= SLen) and not(S[I] in WordDelims) do Inc(I);
  end;
end;

function strPadZeroL(const S: String; Len: Integer): String;
begin
  Result := AddCharL('0', AllTrim(S), Len);
end;

{ T_NRC_InterfacedObject }

function T_NRC_InterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function T_NRC_InterfacedObject._Release: Integer;
begin
  Result := 1;
end;

function T_NRC_InterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{ THandledObject }

destructor THandledObject.Destroy;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
end;

{ TSharedMem }

constructor TSharedMem.Create(const Name: string; Size: Integer);
begin
  try
    FName := Name;
    FSize := Size;
    { CreateFileMapping, when called with $FFFFFFFF for the hanlde value,
      creates a region of shared memory }
    FHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0,
        Size, PChar(Name));
    if FHandle = 0 then abort;
    FCreated := GetLastError = 0;
    { We still need to map a pointer to the handle of the shared memory region }
    FFileView := MapViewOfFile(FHandle, FILE_MAP_WRITE, 0, 0, Size);
    if FFileView = nil then abort;
  except
    Raise Exception.Create(Format('Erro criando área de memória compartilhada %s (%d)', [Name, GetLastError]));
  end;
end;

destructor TSharedMem.Destroy;
begin
  if FFileView <> nil then
    UnmapViewOfFile(FFileView);
  inherited Destroy;
end;

function TSharedMem.getString: String;
begin
  System.SetString(Result, pChar(Buffer), strLen(Buffer));
end;

procedure TSharedMem.setString(const Value: String);
var p: pByteArray;
    i: Integer;
    Tam: Integer;
begin
  p := Buffer;
  Tam := Length(Value);
  if Tam > FSize then Tam := Size-1;
  for i := 1 to Tam do p[i-1] := byte(Value[i]);
  p[i] := byte(#0);
end;

{ TCryptStrings }

constructor TCryptStrings.Create(Key: Word);
begin
  inherited Create;
  FKey := Key;
end;

procedure TCryptStrings.LoadFromStream(Stream: TStream);
var Size: Integer;
    S: string;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    if FKey <> 0 then S := Decrypt(S, FKey);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TCryptStrings.SaveToStream(Stream: TStream);
var S: string;
begin
  S := GetTextStr;
  if FKey <> 0 then S := Encrypt(S, FKey);
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;


function Spaces(const N : integer) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to N do Result := Result + ' ';
end;

function HasChar(const Ch : Char; const S : string) : boolean;
begin
  Result := System.Pos(Ch, S) > 0;
end;

function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;
begin
  Result := '';
  if (P > Length(S)) or (P = 1) then exit;
  iBeg := P;
  if S[P] in Separators then
    if (P < 1) or (S[P-1] in Separators) then
      inc(iBeg)
    else if not (S[P-1] in Separators) then
      dec(iBeg);
  while iBeg >= 1 do
    if S[iBeg] in Separators then break else dec(iBeg);
  inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then break else inc(iEnd);
  if iEnd > iBeg then
    Result := System.Copy(S, iBeg, iEnd - iBeg) else
    Result := S[P];
end;

function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;
var
  LW : integer;
  P : PChar;
  Sm : integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do begin
    Sm := P-PChar(S);
    S := System.Copy(S, 1, Sm)+NewPattern+System.Copy(S, Sm+LW+1, Length(S));
    P := StrPos(PChar(S)+Sm+Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

function GetWordOnPos(const S : string; const P : integer) : string;
var
  i, Beg : integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  for i := P downto 1 do
    if S[i] in Separators then break;
  Beg := i + 1;
  for i := P to Length(S) do
    if S[i] in Separators then break;
  if i > Beg then
    Result := System.Copy(S, Beg, i-Beg) else
    Result := S[P];
end;

function CharInSet(const Ch : Char; const SetOfChar : TCharSet) : boolean;
begin
  Result := Ch in SetOfChar;
end;

function SubStr(const S : string; const index : integer; const Separator : string) : string;
var
  i : integer;
  pB, pE : PChar;
begin
  Result := '';
  if (index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator)) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function KeyPressed(VK : integer) : boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
var
  i, iB : integer;
begin
  X := -1; Y := -1; iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then begin
    i := 1;
    Y := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then begin inc(Y); iB := i+1 end;
      inc(i);
    end;
    X := Pos - iB;
  end;
end;

function Cmp(const S1, S2 : string) : boolean;
begin
  Result := ANSICompareText(S1, S2) = 0;
end;

function HasAnyChar(const Chars : string; const S : string) : boolean;
var
  i : integer;
begin
  for i := 1 to Length(Chars) do
    if HasChar(Chars[i], S) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function Pos(const aSubstr, S: String; aOfs: Integer): Integer;
begin
  Result := System.Pos(aSubStr, System.Copy(S, aOfs, (Length(S)-aOfs)+1));
  if (Result > 0) and (aOfs > 1) then Inc(Result, aOfs-1);
end;

function AjustDate(const sDate, Format: String): String;
var d: TDateTime;
begin
  d := StrToDate(sDate);
  Result := FormatDateTime(Format, d);
end;

function SetToStr(var s: TByteSet): String;
var i: byte;
begin
  Result := '';
  for i := 0 to 255 do
    if i in s then
       if Result = '' then
          Result := intToStr(i)
       else
          Result := Result + ', ' + intToStr(i)
end;

function LengthOfSet(const s: TByteSet; Max: byte): byte;
var i: byte;
begin
  Result := 0;
  for i := 0 to Max do
    if i in s then inc(Result);
end;

Function LastChar(const s: String): Char;
begin
  if s <> '' then
     Result := s[Length(s)]
  else
     Result := #0;
end;

function PosR(const aSubstr, S: String): Integer;
var i, k: Integer;
    PL: Char;
begin
  Result := -1;
  if (aSubstr <> '') and (S <> '') then
     begin
     i := Length(aSubstr);
     PL := aSubstr[1];
     for k := Length(S) downTo 1 do
       if S[k] = PL then
          if System.Copy(S, k, i) = aSubstr then
             begin
             Result := k;
             Break;
             end;
     end;
end;

procedure DateDiff(Date1, Date2 : TDateTime; var Days, Months, Years : Word);
var Day1, Day2, Month1, Month2, Year1, Year2 : Word;
begin
  {we want Date2 > Date1}
  if Date1 > Date2 then
    Swap(Double(Date1), Double(Date2));

  {convert dates to day,month,year}
  DecodeDate(Date1, Year1, Month1, Day1);
  DecodeDate(Date2, Year2, Month2, Day2);

  {days first}
  if Day2 < Day1 then
     begin
     Dec(Month2);
     if Month2 = 0 then
        begin
        Month2 := 12;
        Dec(Year2);
        end;
     Inc(Day2, DateUtils.DaysInAMonth(Year2, Month2));
     end;
  Days := Day2-Day1;

  {now months and years}
  if Month2 < Month1 then
     begin
     Inc(Month2, 12);
     Dec(Year2);
     end;
  Months := Month2-Month1;
  Years := Year2-Year1;
end;

procedure DeleteLastChar(var s: String);
begin
  Delete(s, Length(s), 1);
end;

function Locate(const SubString: String; Text: TStrings; var OffSet: Integer;
                CaseSensitive: Boolean = False; FromEnd: Boolean = False): Boolean;
var b: Boolean;
begin
  Result := False;
  if FromEnd then OffSet := Text.Count-1;

  if (OffSet < Text.Count) or FromEnd then
     repeat
       if CaseSensitive then
          b := (System.Pos(SubString, Text[OffSet]) > 0)
       else
          b := (System.Pos(upperCase(SubString), upperCase(Text[OffSet])) > 0);

       if b then
          begin
          Result := True;
          Exit;
          end;

       if FromEnd then
          begin
          dec(OffSet);
          b := (OffSet = -1);
          end
       else
          begin
          inc(OffSet);
          b := (OffSet = Text.Count);
          end;

     until b;
end;

// Ajusta os caracteres (<, >, ", &) para o formato HTTP 
function HTTP_EncodeChars(const s: String): String;
var i, k: Integer;
begin
  // Determino o tamanho da String destino
  k := 0;
  for i := 1 to Length(s) do
    case s[i] of
      '<': inc(k, 4);
      '>': inc(k, 4);
      '&': inc(k, 5);
      '"': inc(k, 6);
    else
      inc(k);
    end;

  // Seto o tamanho da String de destino
  SetLength(Result, k);

  // Troco os caracteres
  i := 1;
  k := 1;
  while k <= Length(s) do
    begin
    case s[k] of
      '<': begin
           Result[i+0] := '&';
           Result[i+1] := 'l';
           Result[i+2] := 't';
           Result[i+3] := ';';
           inc(i, 4);
           end;

      '>': begin
           Result[i+0] := '&';
           Result[i+1] := 'g';
           Result[i+2] := 't';
           Result[i+3] := ';';
           inc(i, 4);
           end;

      '&': begin
           Result[i+0] := '&';
           Result[i+1] := 'a';
           Result[i+2] := 'm';
           Result[i+3] := 'p';
           Result[i+4] := ';';
           inc(i, 5);
           end;

      '"': begin
           Result[i+0] := '&';
           Result[i+1] := 'q';
           Result[i+2] := 'u';
           Result[i+3] := 'o';
           Result[i+4] := 't';
           Result[i+5] := ';';
           inc(i, 6);
           end;

    else
        begin
        Result[i] := s[k];
        inc(i);
        end;
      end;

    inc(k); // Percorro carac. a carac. a String de entrada
    end;
end;

function MultiLineToLine(const s: String): String;
var i: Integer;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
    case s[i] of
      #13: Result[i] := #2;
      #10: Result[i] := #1;
      else
         Result[i] := s[i];
      end;
end;

function LineToMultiLine(const s: String): String;
var i: Integer;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
    case s[i] of
      #2: Result[i] := #13;
      #1: Result[i] := #10;
      else
         Result[i] := s[i];
      end;
end;

var
  FApplicationDir: String = '';

procedure SetApplicationDir(const Dir: String);
begin
  FApplicationDir := Dir;
end;

// Para uso desta funcao o usuario devera primeiro chamar "SetApplicationDir()"
function GetApplicationDir(): String;
begin
  Result := FApplicationDir;
  if Result = '' then
     raise Exception.Create('SysUtilsEx: SetApplicationDir() not called');
end;

// Retorna a string que fica a esquerda de "RightStr"
// Se "TrimResult" remove os espaços iniciais e finais do resultado.
function LeftStringOf(const s, RightStr: String; TrimResult: Boolean = false): String;
begin
  Result := System.Copy(s, 1, System.Pos(RightStr, s)-1);
  if TrimResult then
     Result := Alltrim(Result);
end;

// Retorna a string que fica a direita de "LeftStr"
// Se "TrimResult" remove os espaços iniciais e finais do resultado.
function RigthStringOf(const s, LeftStr: String; TrimResult: Boolean = false): String;
var p: integer;
begin
  p := System.Pos(LeftStr, s);
  if p = 0 then
     result := ''
  else
     begin
     Result := System.Copy(s, p+1, System.Length(s));
     if TrimResult then
        Result := Alltrim(Result);
     end;
end;

// Lê um arquivo e devolve uma StringList
function LoadTextFile(const Name: string): TStrings;
begin
  Result := TStringList.Create;
  Result.LoadFromFile(Name);
end;

function toString(const x: integer): string;
begin
  Result := IntToStr(x);
end;

function toString(const x: real; DecimalPlaces: byte = 2): string;
begin
  Result := FloatToStrF(x, ffFixed, 15, DecimalPlaces);
end;

function toString(const x: boolean): string;
begin
  Result := BooleanToStr(x);
end;

function SubStrings(const s, sStart, sEnd: string; EnableDuplicates: boolean): TStrings; overload;

  function Locate(OffSet: Integer; out i1, i2: integer): boolean;
  begin
    i1 := Pos(sStart, s, OffSet);
    i2 := Pos(sEnd  , s, OffSet + 1);
    result := (i1 > 0) and (i2 > 0);
  end;

var Offset, i1, i2: integer;
begin
  result := TStringList.Create();
  Offset := 1;

  if not EnableDuplicates then
     begin
     TStringList(result).Sorted := true;
     TStringList(result).Duplicates := dupIgnore;
     end;

  if Length(s) > 0 then
     while Locate(OffSet, i1, i2) do
       begin
       result.Add(subString(s, i1, i2));
       OffSet := i2 + 1;
       end;
end;

function StringToIntArray(const s: string; Seps: TCharSet): TIntArray;
var i: Integer;
    x: TStrings;
begin
  x := nil;
  Split(s, x, Seps);
  setLength(Result, x.Count);
  for i := 0 to x.Count-1 do
    Result[i] := strToIntDef(x[i], 0);
  x.Free();  
end;

function StringToFloatArray(const s: string; Seps: TCharSet): TFloatArray;
var i: Integer;
    x: TStrings;
begin
  x := nil;
  Split(s, x, Seps);
  setLength(Result, x.Count);
  for i := 0 to x.Count-1 do
    Result[i] := strToFloatDef(x[i], 0);
  x.Free();
end;

procedure Fill(var x: TFloatArray; const Value: real);
var i: Integer;
begin
  for i := 0 to High(x) do
    x[i] := Value;
end;

procedure Fill(var x: TIntArray; const Value: integer);
var i: Integer;
begin
  for i := 0 to High(x) do
    x[i] := Value;
end;

// Divide cada elemento do array por Value
procedure Divide(var x: TFloatArray; const Value: real);
var i: Integer;
begin
  for i := 0 to High(x) do
    x[i] := x[i] / Value;
end;

function toInt(const s: string): integer; overload;
begin
  result := strToInt(s);
end;

function toInt(const s: string; Default: integer): integer; overload;
begin
  result := strToIntDef(s, Default);
end;

function toInt(const s: string; Trim: boolean; Default: integer): integer; overload;
begin
  if Trim then
     result := strToIntDef( AllTrim(s), Default )
  else
     result := strToIntDef( s, Default );
end;

function toInt(const s: string; StartPos, Count: integer; Trim: boolean; Default: integer): integer; overload;
begin
  if Trim then
     result := strToIntDef( AllTrim(System.Copy(s, StartPos, Count)), Default )
  else
     result := strToIntDef( System.Copy(s, StartPos, Count), Default );
end;

function toBoolean(const s: string): boolean; overload;
begin
  result := strToBoolean(s);
end;

function toBoolean(const i: integer): boolean; overload;
begin
  result := Boolean(i);
end;

function toFloat(const s: string): real;
begin
  result := strToFloat(s);
end;

function toFloat(const s: string; Default: real): real;
begin
  result := strToFloatDef(s, Default);
end;

function toFloat(const s: string; StartPos, Count: integer; Trim: boolean; Default: real): real;
begin
  if Trim then
     result := strToFloatDef( AllTrim(System.Copy(s, StartPos, Count)), Default )
  else
     result := strToFloatDef( System.Copy(s, StartPos, Count), Default );
end;

function Copy(const s: string; StartPos, Count: integer; Trim: boolean): string;
begin
  if Trim then
     result := AllTrim( System.Copy(s, StartPos, Count) )
  else
     result :=  System.Copy(s, StartPos, Count);
end;

function toPoint(const s: string): TPoint; overload; // ex: 50,100
var s1, s2: string;
begin
  SubStrings(',', s1, s2, s);
  result.x := toInt(s1, 0);
  result.y := toInt(s2, 0);
end;

function toPoint(const x, y: integer): TPoint; overload;
begin
  result.x := x;
  result.y := y;
end;

function toPoint(const x, y: real): TPoint; overload;
begin
  result.x := trunc(x);
  result.y := trunc(y);
end;

var ds: char;
    dsCount: integer = 0;

// Salva o ponto decimal corrente
// Estabelece o novo ponto decimal como: "."
procedure SaveDecimalSeparator();
begin
  if dsCount = 0 then
     begin
     ds := SysUtils.DecimalSeparator;
     SysUtils.DecimalSeparator := '.';
     end;

  inc(dsCount);
end;

// Restaura o ponto decimal salvo
procedure RestoreDecimalSeparator();
begin
  dec(dsCount);
  if dsCount = 0 then
     SysUtils.DecimalSeparator := ds;
end;

procedure VirtualError(Sender: TObject; const MethodName: string);
begin
  raise Exception.Create('Método virtual não implementado: ' +
                         Sender.ClassName + '.' + MethodName);
end;

{ TStringList }

constructor TStringList.CreateFrom(const Items: array of string);
var i: Integer;
begin
  inherited Create();
  for i := 0 to High(Items) do
    Add(Items[i]);
end;

{ TStringMat }

function TStringMat.ColCount(): integer;
begin
  if System.Length(FArray) = 0 then
     result := 0
  else
     result := System.Length(FArray[0]);
end;

constructor TStringMat.Create(const Filename: string);
var arq, sl: TStrings;
    i, j: integer;
begin
  inherited Create();
  FCheckBounds := true;
  sl := nil;
  arq := LoadTextFile(Filename);
  try
    System.SetLength(FArray, arq.Count);
    for i := 0 to arq.Count-1 do
      begin
      Split(arq[i], sl, [';']);
      System.SetLength(FArray[i], sl.Count);
      for j := 0 to sl.Count-1 do
        FArray[i][j] := sl[j];
      end;
  finally
    arq.Free();
    sl.Free();
  end;
end;

constructor TStringMat.Create(RowCount, ColCount: integer);
var i: integer;
begin
  inherited Create();
  FCheckBounds := true;
  System.SetLength(FArray, RowCount);
  for i := 0 to RowCount-1 do
    System.SetLength(FArray[i], ColCount);
end;

function TStringMat.getAsFloat(i, j: integer): double;
begin
  result := toFloat(getAsString(i, j));
end;

function TStringMat.getAsInteger(i, j: integer): integer;
begin
  result := toInt(getAsString(i, j));
end;

function TStringMat.getAsString(i, j: integer): string;
begin
  if FCheckBounds then InternalCheckBounds(i, j);
  result := FArray[i-1][j-1];
end;

function TStringMat.getSizeOfRow(row: integer): integer;
begin
  if (row > 0) and (row <= System.Length(FArray)) then
     result := System.Length(FArray[row-1])
  else
     InvalidRowIndex(row);
end;

function TStringMat.getItem(row, col: integer): string;
begin
  result := getAsString(row, col);
end;

function TStringMat.IsMissValue(i, j: integer; out x: double): boolean;
begin
  result := false;
  x := self.getAsFloat(i, j);
end;

function TStringMat.RowCount(): integer;
begin
  result := System.Length(FArray);
end;

procedure TStringMat.setAsFloat(i, j: integer; value: double);
begin
  setAsString(i, j, ToString(value));
end;

procedure TStringMat.setAsInteger(i, j, value: integer);
begin
  setAsString(i, j, ToString(value));
end;

procedure TStringMat.setAsString(i, j: integer; value: string);
var r, c: integer;
begin
  if FCheckBounds then InternalCheckBounds(i, j);
  FArray[i-1][j-1] := value;
end;

procedure TStringMat.setColCount(const Value: integer);
var i: integer;
begin
  for i := 0 to System.High(FArray) do
    System.SetLength(FArray[i], Value);
end;

procedure TStringMat.setSizeOfRow(row: integer; const Value: integer);
begin
  if (row > 0) and (row <= System.Length(FArray)) then
     System.SetLength(FArray[row-1], Value)
  else
     InvalidRowIndex(row);
end;

procedure TStringMat.setItem(row, col: integer; const Value: string);
begin
  setAsString(row, col, Value);
end;

procedure TStringMat.setRowCount(const Value: integer);
var i, k, j: integer;
begin
  i := self.RowCount();
  System.SetLength(FArray, Value);
  if Value > i then
     begin
     j := self.ColCount();
     for k := i to Value-1 do
       System.SetLength(FArray[k], j);
     end;
end;

function TStringMat.SizeOfRowsAreEquals(): boolean;
var i, k: Integer;
begin
  k := self.ColCount();
  result := true;
  for i := 0 to System.High(FArray) do
    if System.Length(FArray[i]) <> k then
       begin
       result := false;
       break;
       end;
end;

procedure TStringMat.InvalidColIndex(row, col: integer);
begin
  raise Exception.CreateFmt(
    '(TStringMat) Invalid col index (%d) in row (%d)'#13 +
    'Size of row: %d', [col, row, self.getSizeOfRow(row)]);
end;

procedure TStringMat.InvalidRowIndex(row: integer);
begin
  raise Exception.CreateFmt(
    '(TStringMat) Invalid row index: %d'#13 +
    'Rows of matrix: %d', [row, self.RowCount()]);
end;

procedure TStringMat.InternalCheckBounds(i, j: integer);
var r, c: integer;
begin
  r := self.RowCount();
  if (i > 0) and (i <= r) then
     begin
     r := i-1;
     c := System.Length(FArray[r]);
     if (j > 0) and (j <= c) then
        // correct
     else
        InvalidColIndex(i, j)
     end
  else
     InvalidRowIndex(i);
end;

procedure ProcessMessages();
var MSG: TMSG;
begin
  if Windows.PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
     if Msg.Message <> WM_QUIT then
        begin
        Windows.TranslateMessage(Msg);
        Windows.DispatchMessage(Msg);
        end;
end;

procedure ShowMessage(const msg: string);
begin
  Windows.MessageBox(0, pChar(msg), 'Mensagem', MB_ICONINFORMATION or MB_OK);
end;

// Conta o numero de ocorrencias de < Ch > em < S >
// Esta funcao eh CaseSensitive
function CharCount(const Ch: Char; const S: string): integer;
var i: Integer;
begin
  result := 0;
  for i := 1 to System.Length(S) do
    if S[i] = Ch then inc(result);
end;

Function Reverse(const s: String): string;
var i, k: Integer;
begin
  k := Length(s);
  System.SetLength(Result, Length(s));
  for i := 1 to Length(s) do
    begin
    result[k] := s[i];
    dec(k);
    end;
end;

end.

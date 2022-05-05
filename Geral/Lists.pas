unit Lists;

// Versão 1.0

interface
uses Classes,
     Types,
     {$ifdef MSXML}
     MSXML4,
     {$endif}
     Windows;

Const
  cIntegerListError    = 'Índice Inválido (TIntegerList[%d])';
  cBooleanListError    = 'Índice Inválido (TBooleanList[%d])';

type
  TDoubleList = Class
  private
    FList: TList;
    function getAsString(index: Integer): String;
    function getAsDouble(index: Integer): System.Double;
    procedure setAsString(index: Integer; const Value: String);
    procedure setAsDouble(index: Integer; const Value: Double);
    function getCount(): Integer;
    procedure setCount(const Value: Integer);
  public
    constructor Create(Count: integer = 0);
    Destructor Destroy; override;

    procedure Add(const Value: Double);
    procedure Delete(index: Integer);
    procedure Assign(List: TDoubleList);
    procedure Clear();

    function ToArray: TDoubleDynArray;

    property Count: Integer read getCount write setCount;

    property AsDouble [index: Integer]: Double read getAsDouble write setAsDouble; default;
    property AsString [index: Integer]: String read getAsString write setAsString;
  end;

  TBooleanList = Class
  private
    FList: TBits;
    FCount: Integer;
    function getAsString(i: Integer): String;
    function getAsBoolean(i: Integer): Boolean;
    procedure setAsString(i: Integer; const Value: String);
    procedure setAsBoolean(i: Integer; const Value: Boolean);
    procedure setCount(const Value: Integer);
  public
    constructor Create(); overload;
    constructor Create(Size: integer); overload;

    Destructor Destroy(); override;

    procedure Add(Value: Boolean);
    procedure Delete(i: Integer);
    procedure Clear();
    procedure Assign(List: TBooleanList);

    // Salvamento e recuperacao de dados
    procedure toXML(Buffer: TStrings; Ident: Integer; const NodeName: string = '');
    {$ifdef MSXML}
    procedure fromXML(no: IXMLDomNode);
    {$endif}

    property Count: Integer read FCount write setCount;

    property AsBoolean[i: Integer] : Boolean read getAsBoolean write setAsBoolean; default;
    property AsString [i: Integer] : String  read getAsString  write setAsString;
  end;

  TIntegerList = Class
  private
    FList: TList;
    function getAsInteger(i: Integer): Integer;
    procedure setAsInteger(i: Integer; const Value: Integer);
    function getCount: Integer;
    function getAsString(i: Integer): String;
    procedure setAsString(i: Integer; const Value: String);
    procedure setCount(const Value: Integer);
  public
    constructor Create(); overload;
    constructor Create(Size: integer); overload;
    Destructor Destroy; override;

    procedure Add(const Value: Integer);
    procedure Assign(List: TIntegerList);
    procedure Clear;
    procedure Delete(index: Integer);
    procedure DeleteVal(Value: Integer);
    function  Exists(Value: Integer; out Index: Integer): Boolean;

    // Salvamento e recuperacao de dados
    procedure toXML(Buffer: TStrings; Ident: Integer; const NodeName: string = '');
    {$ifdef MSXML}
    procedure fromXML(no: IXMLDomNode);
    {$endif}

    property Count: Integer read getCount write setCount;

    property AsInteger[i: Integer]: Integer read getAsInteger write setAsInteger; default;
    property AsString [i: Integer]: String  read getAsString  write setAsString;
  end;

  TRegionList = Class
  private
    FList: TList;
    function getRegion(i: Integer): HRGN;
    function getCount: Integer;
    function getName(i: Integer): String;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Exist(x, y: Integer): Integer;
    function  Add(const Vertices: Array of TPoint; const Name: String = ''): Integer;
    procedure ShowVertices(i: Integer; SL: TStrings; const Format: String = '%d,%d');
    procedure SaveToFile(const Name: String);
    procedure LoadFromFile(const Name: String);
    procedure Clear();

    property  Count: Integer read getCount;

    property  Region [i: Integer]: HRGN   read getRegion; default;
    property  Name   [i: Integer]: String read getName;
  end;

implementation
uses Sysutils,
     IniFiles,
     SysUtilsEx;

{ TDoubleList }

type pDouble = ^Double;

procedure TDoubleList.Add(const Value: System.Double);
var p: pDouble;
begin
  New(p);
  p^ := Value;
  FList.Add(p);
end;

procedure TDoubleList.Assign(List: TDoubleList);
var i: Integer;
begin
  self.Clear();
  if List <> nil then
     for i := 0 to List.Count-1 do
       Self.Add(List[i]);
end;

procedure TDoubleList.Clear();
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    Dispose(pDouble(FList[i]));
  FList.Clear;
end;

constructor TDoubleList.Create(Count: integer = 0);
var i: Integer;
begin
  inherited Create;
  FList := TList.Create;
  for i := 1 to Count do self.Add(0);
end;

procedure TDoubleList.Delete(index: Integer);
begin
  FList.Delete(Index);
end;

destructor TDoubleList.Destroy;
begin
  self.Clear();
  FList.Free();
  inherited;
end;

function TDoubleList.getAsString(index: Integer): String;
begin
  Result := FloatToStr(getAsDouble(index));
end;

function TDoubleList.getCount: Integer;
begin
  Result := FList.Count;
end;

function TDoubleList.getAsDouble(index: Integer): Double;
begin
  Result := pDouble(FList[index])^;
end;

procedure TDoubleList.setAsString(index: Integer; const Value: String);
begin
  setAsDouble(Index, strToFloat(Value));
end;

procedure TDoubleList.setAsDouble(index: Integer; const Value: Double);
begin
  pDouble(FList[index])^ := Value;
end;

function TDoubleList.ToArray: TDoubleDynArray;
var i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count-1 do
    Result[i] := getAsDouble(i);
end;

procedure TDoubleList.setCount(const Value: Integer);
begin
  FList.Count := Value;
end;

{ TBooleanList }

procedure TBooleanList.Add(Value: Boolean);
begin
  inc(FCount);
  if FCount > FList.Size then
     FList.Size := FList.Size + 64;

  FList[FCount-1] := Value;
end;

constructor TBooleanList.Create();
begin
  inherited;
  FList := TBits.Create;
  FList.Size := 64;
  FCount := 0;
end;

constructor TBooleanList.Create(Size: integer);
begin
  Create();
  FList.Size := Size;
  FCount := Size;
end;

procedure TBooleanList.Delete(i: Integer);
var ii: Integer;
begin
  if (i < FCount) and (i >= 0) then
     begin
     for ii := i+1 to FCount-1 do FList[ii-1] := FList[ii];
     dec(FCount);
     end;
end;

destructor TBooleanList.Destroy();
begin
  FList.Free;
  inherited;
end;

procedure TBooleanList.setCount(const Value: Integer);
begin
  FCount := Value;
  FList.Size := Value;
end;

procedure TBooleanList.Assign(List: TBooleanList);
var i: Integer;
begin
  if List <> nil then
     begin
     self.setCount(List.Count);
     for i := 0 to List.Count-1 do
       self.setAsBoolean(i, List[i]);
     end;
end;

{$ifdef MSXML}
procedure TBooleanList.fromXML(no: IXMLDomNode);
var i: Integer;
begin
  FCount := no.childNodes.length;
  FList.Size := FCount;
  for i := 0 to FCount-1 do
    FList[i] := (no.childNodes.item[i].text <> '0');
end;
{$endif}

procedure TBooleanList.toXML(Buffer: TStrings; Ident: Integer; const NodeName: string);
var s, sIdent: String;
    i: integer;
begin
  sIdent := StringOfChar(' ', Ident);
  if NodeName = '' then s := '<BL>' else s := '<' + NodeName + '>';
  s := sIdent + s;
  for i := 0 to FCount-1 do
    s := s + '<e>' + toString(byte(FList[i])) + '</e>';
  if NodeName = '' then s := s +  '</BL>' else s := s + '</' + NodeName + '>';
  Buffer.Add(s);
end;

function TBooleanList.getAsString(i: Integer): String;
begin
  if (i < FCount) and (i >= 0) then
     if FList[i] then
        Result := 'VERDADEIRO'
     else
        Result := 'FALSO'
  else
     Result := '';
end;

function TBooleanList.getAsBoolean(i: Integer): Boolean;
begin
  if (i < FCount) and (i >= 0) then
     Result := FList[i]
  else
     Raise Exception.CreateFmt(cBooleanListError, [i]);
end;

procedure TBooleanList.setAsString(i: Integer; const Value: String);
begin
  if (i < FCount) and (i >= 0) then
     FList[i] := (CompareText('verdadeiro', Value) = 0) or (Value = '1');
end;

procedure TBooleanList.setAsBoolean(i: Integer; const Value: Boolean);
begin
  if (i < FCount) and (i >= 0) then
     FList[i] := Value;
end;

procedure TBooleanList.Clear();
begin
  FList.Size := 0;
end;

{ TIntegerList }

procedure TIntegerList.Add(const Value: Integer);
begin
  FList.Add(pointer(Value));
end;

procedure TIntegerList.Assign(List: TIntegerList);
var i: Integer;
begin
  self.Clear();
  if List <> nil then
     for i := 0 to List.Count-1 do
       FList.Add(Pointer(List[i]));
end;

procedure TIntegerList.Clear;
begin
  FList.Clear;
end;

constructor TIntegerList.Create();
begin
  inherited;
  FList := TList.Create;
end;

constructor TIntegerList.Create(Size: integer);
begin
  Create();
  FList.Count := Size;
end;

procedure TIntegerList.Delete(index: Integer);
begin
  FList.Delete(Index);
end;

procedure TIntegerList.DeleteVal(Value: Integer);
var i: Integer;
begin
  i := FList.IndexOf(pointer(Value));
  if i > -1 then FList.Delete(i);
end;

destructor TIntegerList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TIntegerList.Exists(Value: Integer; out Index: Integer): Boolean;
begin
  Index := FList.IndexOf(pointer(Value));
  Result := (Index > -1);
end;

function TIntegerList.getAsString(i: Integer): String;
begin
  if (i < FList.Count) and (i >= 0) then
     Result := IntToStr(integer(FList[i]))
  else
     Raise Exception.CreateFmt(cIntegerListError, [i]);
end;

function TIntegerList.getCount(): Integer;
begin
  Result := FList.Count;
end;

function TIntegerList.getAsInteger(i: Integer): Integer;
begin
  if (i < FList.Count) and (i >= 0) then
     Result := integer(FList[i])
  else
     Raise Exception.CreateFmt(cIntegerListError, [i]);
end;

procedure TIntegerList.setAsString(i: Integer; const Value: String);
begin
  if (i < FList.Count) and (i >= 0) then
     FList[i] := pointer(StrToInt(Value))
  else
     Raise Exception.CreateFmt(cIntegerListError, [i]);
end;

procedure TIntegerList.setAsInteger(i: Integer; const Value: Integer);
begin
  if (i < FList.Count) and (i >= 0) then
     FList[i] := pointer(Value)
  else
     Raise Exception.CreateFmt(cIntegerListError, [i]);
end;

{$ifdef MSXML}
procedure TIntegerList.fromXML(no: IXMLDomNode);
var i: Integer;
begin
  FList.Count := no.childNodes.length;
  for i := 0 to FList.Count-1 do
    setAsInteger(i, toInt(no.childNodes.item[i].text));
end;
{$endif}

procedure TIntegerList.toXML(Buffer: TStrings; Ident: Integer; const NodeName: string = '');
var s, sIdent: String;
    i: integer;
begin
  sIdent := StringOfChar(' ', Ident);
  if NodeName = '' then s := '<IL>' else s := '<' + NodeName + '>';
  s := sIdent + s;
  for i := 0 to self.getCount()-1 do
    s := s + '<e>' + self.getAsString(i) + '</e>';
  if NodeName = '' then s := s +  '</IL>' else s := s + '</' + NodeName + '>';
  Buffer.Add(s);
end;

procedure TIntegerList.setCount(const Value: Integer);
begin
  FList.Count := Value;
end;

{ TRegionList }

type
  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;

  pRecRegiao = ^TRecRegiao;
  TRecRegiao = record
                 Xs, Ys : TIntegerList;
                 HR     : HRGN;
                 Nome   : String;
               end;

function TRegionList.Add(const Vertices: array of TPoint; const Name: String = ''): Integer;
var HR: HRGN;
    p : pRecRegiao;
    i : Integer;
begin
  HR := CreatePolygonRgn(
    PPoints(@Vertices)^,   // ponteiro para o primeiro elemento do array de pontos
    High(Vertices) + 1,	   // número de pontos
    ALTERNATE	             // modo de preenchimento
    );

  new(p);
  p.Xs   := TIntegerList.Create;
  p.Ys   := TIntegerList.Create;
  p.HR   := HR;
  p.Nome := Name;
  for i := 0 to High(Vertices) do
    begin
    p.Xs.Add(Vertices[i].x);
    p.Ys.Add(Vertices[i].y);
    end;

  Result := FList.Add(p);
end;

constructor TRegionList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TRegionList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TRegionList.Exist(x, y: Integer): Integer;
begin
  for Result := 0 to FList.Count - 1 do
    if PtInRegion(GetRegion(Result), x, y) then Exit;

  Result := -1;
end;

function TRegionList.getName(i: Integer): String;
begin
  Result := pRecRegiao(FList[i])^.Nome;
end;

function TRegionList.getCount: Integer;
begin
  Result := FList.Count;
end;

function TRegionList.getRegion(i: Integer): HRGN;
begin
  Result := pRecRegiao(FList[i])^.HR;
end;

procedure TRegionList.LoadFromFile(const Name: String);
var Ini            : TMemIniFile;
    p              : Array of TPoint;
    i, j, k        : Integer;
    s, s2, s3, s4  : String;
    s5             : String;

  Function GetInfo(Sep: Char; Var S1, S2: String; Const S: String): Integer;
  Begin
    Result := System.Pos(Sep, S);
    S1 := System.Copy(S, 1, Result-1);
    S2 := System.Copy(S, Result+1, Length(S));
  End;

begin
  Ini := TMemIniFile.Create(Name);
  try
    k := Ini.ReadInteger('Geral', 'NumPols', 0);
    if k > 0 then
       begin
       Clear;
       for i := 1 to k do
         begin
         s := 'Poligono ' + intToStr(i);
         k := Ini.ReadInteger(s, 'NumPontos', 0);
         s5 := Ini.ReadString(s, 'Nome', '');
         SetLength(p, k);
         for j := 1 to k do
           begin
           s2 := Ini.ReadString(s, 'P'+intToStr(j), '0,0');
           getInfo(',', s3, s4, s2);
           p[j-1].x := strToInt(s3);
           p[j-1].y := strToInt(s4);
           end;
         Add(p, s5);
         end;
       end;

  finally
    Ini.Free;
  end;
end;

procedure TRegionList.Clear;
var p : pRecRegiao;
    i : integer;
begin
  for i := 0 to FList.Count - 1 do
    begin
    p := pRecRegiao(FList[i]);
    p.Xs.Free;
    p.Ys.Free;
    Windows.DeleteObject(p.HR);
    Dispose(p);
    end;
  FList.Clear;
end;

procedure TRegionList.ShowVertices(i: Integer; SL: TStrings; const Format: String = '%d,%d');
var ii: Integer;
    p : pRecRegiao;
begin
  SL.Clear;
  p := pRecRegiao(FList[i]);

  for ii := 0 to p.Xs.Count - 1 do
    SL.Add(SysUtils.Format(Format, [p.Xs[ii], p.Ys[ii]]));
end;

procedure TRegionList.SaveToFile(const Name: String);
var Ini : TMemIniFile;
    p   : pRecRegiao;
    i, j: Integer;
    s   : String;
begin
  Ini := TMemIniFile.Create(Name);

  Ini.WriteInteger('Geral', 'NumPols', FList.Count);
  for i := 0 to FList.Count - 1 do
    begin
    p := pRecRegiao(FList[i]);
    s := 'Poligono ' + intToStr(i + 1);
    Ini.WriteInteger(s, 'NumPontos', p.Xs.Count);
    Ini.WriteString (s, 'Nome', p.Nome);
    for j := 0 to p.Xs.Count-1 do
      Ini.WriteString(s, 'P'+intToStr(j+1), Format('%d,%d', [p.Xs[j], p.Ys[j]]));
    end;

  Ini.UpdateFile;
  Ini.Free;
end;

end.

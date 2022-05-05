unit Solver.Classes;

interface
uses Classes,
     Contnrs,
     SysUtils,
     SysUtilsEx;

type
  // Representa uma variavel otimizada
  TVariable = class
  private
    FName: string;
    FValue: real;
  public
    constructor Create(const Name: string; const Value: real);

    property Name : string read FName;
    property Value : real read FValue;

    function ToString(): string; virtual;
  end;

  // Representa a lista das variaveis otimizadas
  TVariableList = class
  private
    FSorted: boolean;
    FList: TObjectList;
    destructor Destroy(); override;
    function getCount: integer;
    function getVar(i: integer): TVariable;
    function Find(const S: string; var Index: Integer): Boolean;
  public
    constructor Create();

    procedure Clear();
    procedure Assign(List: TVariableList);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
    procedure Add(const Name: string; const Value: real);
    procedure Sort();

    function varByName(const varName: string): TVariable;
    function getValue(const varName: string): real; overload;
    function getValue(const varIndex: integer): real; overload;

    property Count : integer read getCount;
    property Item[i: integer] : TVariable read getVar; default;
  end;

implementation

{ TVariable }

constructor TVariable.Create(const Name: string; const Value: real);
begin
  inherited Create();
  FName := Name;
  FValue := Value;
end;

function TVariable.ToString(): string;
begin
  result := FName + ' = ' + SysUtilsEx.toString(FValue, 3);
end;

{ TVariableList }

procedure TVariableList.Add(const Name: string; const Value: real);
begin
  FSorted := false;
  FList.Add(
    TVariable.Create(Name, Value));
end;

procedure TVariableList.Assign(List: TVariableList);
var i: Integer;
begin
  Clear();
  for i := 0 to List.Count-1 do
    Add(List[i].Name, List[i].Value);
end;

procedure TVariableList.Clear();
begin
  FList.Clear();
end;

constructor TVariableList.Create();
begin
  inherited Create();
  FList := TObjectList.Create(true);
end;

destructor TVariableList.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TVariableList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := getCount() - 1;

  while L <= H do
    begin
    I := (L + H) shr 1;
    C := AnsiCompareText(self[i].Name, S);
    if C < 0 then
       L := I + 1
    else
       begin
       H := I - 1;
       if C = 0 then
          begin
          Result := True;
          L := I;
          end;
       end;
    end;

  Index := L;
end;

function TVariableList.getCount(): integer;
begin
  result := FList.Count;
end;

function TVariableList.getValue(const varName: string): real;
var i: Integer;
begin
  if Find(varName, {out} i) then
     result := getValue(i)
  else
     raise Exception.Create('Variável desconhecida: ' + varName);
end;

function TVariableList.getValue(const varIndex: integer): real;
begin
  result := getVar(varIndex).Value;
end;

function TVariableList.getVar(i: integer): TVariable;
begin
  result := TVariable(FList[i]);
end;

function CompareNames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText( TVariable(Item1).Name, TVariable(Item2).Name);
end;

procedure TVariableList.LoadFromFile(const Filename: string);
var SL: TStrings;
     i: integer;
    s1, s2: string;
begin
  Clear();
  SysUtilsEx.SaveDecimalSeparator();
  SL := TStringList.Create();
  SL.LoadFromFile(Filename);
  try
    for i := 0 to SL.Count-1 do
      begin
      SubStrings(';', s1, s2, SL[i]);
      Add(s1, toFloat(s2));
      end;
  finally
    SysUtilsEx.RestoreDecimalSeparator();
    SL.Free();
  end;
end;

procedure TVariableList.SaveToFile(const Filename: string);
var SL: TStrings;
     i: integer;
     v: TVariable;
begin
  SL := TStringList.Create();

  for i := 0 to self.Count-1 do
    begin
    v := self.Item[i];
    SL.Add(v.Name + ';' + toString(v.Value, 10));
    end;

  SL.SaveToFile(Filename);
  SL.Free();
end;

procedure TVariableList.Sort();
begin
  FList.Sort(@CompareNames);
  FSorted := true;
end;

function TVariableList.VarByName(const varName: string): TVariable;
var i: Integer;
begin
  result := nil;
  if FSorted then
     begin
     if Find(varName, i) then result := self[i]
     end
  else
     for i := 0 to getCount()-1 do
       if CompareText(varName, self[i].Name) = 0 then
          begin
          result := self[i];
          break;
          end;
end;

end.

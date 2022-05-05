unit Arrays;

interface
uses Classes, IniFiles;

type
  pReal = ^Real;

  p2D_REAL_REC = ^T2D_REAL_REC;
  T2D_REAL_REC = record
                   x, y: Real;
                 end;

  p3D_REAL_REC = ^T3D_REAL_REC;
  T3D_REAL_REC = record
                   x, y, z: Real;
                 end;

  TPointerArray = array of Pointer;

  TBaseArray = class
  private
    FArray: TPointerArray;
    function getLen: Integer;
    procedure setLen(const Value: Integer);
    function SizeOfElement: Integer; virtual; abstract;
    procedure SetAsString(i: Integer; const Value: String); virtual; abstract;
    function GetAsString(i: Integer): String; virtual; abstract;
  public
    constructor Create(Len: Integer);
    destructor Destroy; override;

    procedure SaveToFile(aFile: TCustomIniFile; const Section, Prefix: String);
    procedure LoadFromFile(aFile: TCustomIniFile; const Section, Prefix: String);

    procedure Assign(aArray: TBaseArray); virtual; abstract;

    function  ItemToString(i: Integer; SizeOfItem: byte): String; virtual; abstract;
    procedure ToStrings(SL: TStrings; ItemsLine, SizeOfItem: byte);

    property Len   : Integer read getLen write setLen;
    property Items : TPointerArray read FArray;

    property AsString[i: Integer] : String read GetAsString write SetAsString;
  end;

  TReal_Array = class(TBaseArray)
  private
    function SizeOfElement: Integer; override;
    function getItem(i: Integer): Real;
    procedure SetAsString(i: Integer; const Value: String); override;
    function GetAsString(i: Integer): String; override;
    procedure SetItem(i: Integer; const Value: Real);
  public
    procedure Assign(aArray: TBaseArray); override;
    function  ItemToString(i: Integer; SizeOfItem: byte): String; override;
    property Item[i: Integer]: Real read getItem write SetItem; Default;
  end;

  T2D_Real_Array = class(TBaseArray)
  private
    function SizeOfElement: Integer; override;
    function getItem(i: Integer): p2D_REAL_REC;
    procedure SetAsString(i: Integer; const Value: String); override;
    function GetAsString(i: Integer): String; override;
  public
    procedure Assign(aArray: TBaseArray); override;
    function  ItemToString(i: Integer; SizeOfItem: byte): String; override;
    property Item[i: Integer]: p2D_REAL_REC read getItem; Default;
  end;

  T3D_Real_Array = class(TBaseArray)
  private
    function SizeOfElement: Integer; override;
    function getItem(i: Integer): p3D_REAL_REC;
    procedure SetAsString(i: Integer; const Value: String); override;
    function GetAsString(i: Integer): String; override;
  public
    procedure Assign(aArray: TBaseArray); override;
    function  ItemToString(i: Integer; SizeOfItem: byte): String; override;
    property Item[i: Integer]: p3D_REAL_REC read getItem; Default;
  end;

implementation
uses SysUtils,
     SysUtilsEx;

{ TBaseArray }

constructor TBaseArray.Create(Len: Integer);
begin
  inherited Create;
  setLen(Len);
end;

destructor TBaseArray.Destroy;
var i, MS: Integer;
begin
  MS := SizeOfElement;
  for i := 0 to High(FArray) do
    FreeMem(FArray[i], MS);
  inherited;
end;

function TBaseArray.getLen: Integer;
begin
  Result := Length(FArray);
end;

procedure TBaseArray.LoadFromFile(aFile: TCustomIniFile; const Section, Prefix: String);
var i: Integer;
begin
  Len := aFile.ReadInteger(Section, Prefix + 'Count', 0);
  for i := 0 to Len do
    SetAsString(i, aFile.ReadString(Section, Prefix + intToStr(i), ''));
end;

procedure TBaseArray.SaveToFile(aFile: TCustomIniFile; const Section, Prefix: String);
var i: Integer;
begin
  aFile.WriteInteger(Section, Prefix + 'Count', Len);
  for i := 0 to High(FArray) do
    aFile.WriteString(Section, Prefix + intToStr(i), GetAsString(i));
end;

procedure TBaseArray.setLen(const Value: Integer);
var Tam: Integer;
    i  : Integer;
    MS : Integer;
begin
  Tam := Length(FArray);
  if Value <> Tam then
     begin
     MS := SizeOfElement;
     if Value > Tam then
        begin
        SetLength(FArray, Value);
        for i := Tam+1 to Value do
          GetMem(FArray[i-1], MS);
        end
     else
        begin
        for i := Tam-1 downto Value do
          FreeMem(FArray[i], MS);
        SetLength(FArray, Value);
        end
     end;
end;

procedure TBaseArray.ToStrings(SL: TStrings; ItemsLine, SizeOfItem: byte);
var i, k: Integer;
    s   : String;
begin
  k := 0;
  s := '';
  for i := 0 to High(FArray) do
    begin
    inc(k);
    s := s + ItemToString(i, SizeOfItem);
    if k = ItemsLine then
       begin
       SL.ADD(s);
       k := 0;
       s := '';
       end
    end;

  if s <> '' then SL.ADD(s);
end;

{ T2D_Real_Array }

function T2D_Real_Array.getItem(i: Integer): p2D_REAL_REC;
begin
  Result := p2D_REAL_REC(FArray[i]);
end;

function T2D_Real_Array.GetAsString(i: Integer): String;
var p: p2D_REAL_REC;
begin
  p := getItem(i);
  Result := Format('x: %f  y: %f', [p.x, p.y]);
end;

procedure T2D_Real_Array.SetAsString(i: Integer; const Value: String);
var p: p2D_REAL_REC;
    posY: byte;
begin
  if Value = '' then Exit;
  p := getItem(i);
  posY := System.Pos('y', Value);
  p.x := toFloat(Value, 4, posY-6, false, 0);
  p.y := toFloat(Value, posY+3, Length(Value), false, 0);
end;

function T2D_Real_Array.SizeOfElement: Integer;
begin
  Result := SizeOf(T2D_REAL_REC);
end;

procedure T2D_Real_Array.Assign(aArray: TBaseArray);
var i: Integer;
begin
  if aArray is T2D_Real_Array then
     begin
     setLen(aArray.Len);
     for i := 0 to aArray.Len-1 do
       p2D_REAL_REC(FArray[i])^ := T2D_Real_Array(aArray).Item[i]^;
     end
  else
     Raise Exception.Create('Assign Error: T2D_Real_Array');
end;

function T2D_Real_Array.ItemToString(i: Integer; SizeOfItem: byte): String;
var p: p2D_REAL_REC;
begin
  p := getItem(i);
  Result := RightStr(FormatFloat('0.00', p.x), SizeOfItem) +
            RightStr(FormatFloat('0.00', p.y), SizeOfItem);
end;

{ T3D_Real_Array }

function T3D_Real_Array.GetAsString(i: Integer): String;
var p: p3D_REAL_REC;
begin
  p := getItem(i);
  Result := Format('x: %f  y: %f  z: %f', [p.x, p.y, p.z]);
end;

procedure T3D_Real_Array.SetAsString(i: Integer; const Value: String);
var p: p3D_REAL_REC;
    posY, posZ: byte;
begin
  if Value = '' then Exit;
  p := getItem(i);
  posY := System.Pos('y', Value);
  posZ := System.Pos('z', Value);
  p.x := toFloat(Value, 4, posY-6, false, 0);
  p.y := toFloat(Value, posY+3, posZ-5-posY, false, 0);
  p.z := toFloat(Value, posZ+3, 20, false, 0);
end;

function T3D_Real_Array.getItem(i: Integer): p3D_REAL_REC;
begin
  Result := p3D_REAL_REC(FArray[i]);
end;

function T3D_Real_Array.SizeOfElement: Integer;
begin
  Result := SizeOf(T3D_REAL_REC);
end;

procedure T3D_Real_Array.Assign(aArray: TBaseArray);
var i: Integer;
begin
  if aArray is T3D_Real_Array then
     begin
     setLen(aArray.Len);
     for i := 0 to aArray.Len-1 do
       P3D_REAL_REC(FArray[i])^ := T3D_Real_Array(aArray).Item[i]^;
     end
  else
     Raise Exception.Create('Assign Error: T3D_Real_Array');
end;

function T3D_Real_Array.ItemToString(i: Integer; SizeOfItem: byte): String;
var p: p3D_REAL_REC;
begin
  p := getItem(i);
  Result := RightStr(FormatFloat('0.00', p.x), SizeOfItem) +
            RightStr(FormatFloat('0.00', p.y), SizeOfItem) +
            RightStr(FormatFloat('0.00', p.z), SizeOfItem);
end;

{ TReal_Array }

procedure TReal_Array.Assign(aArray: TBaseArray);
var i: Integer;
begin
  if aArray is TReal_Array then
     begin
     setLen(aArray.Len);
     for i := 0 to aArray.Len-1 do
       pReal(FArray[i])^ := TReal_Array(aArray).Item[i];
     end
  else
     Raise Exception.Create('Assign Error: TReal_Array');
end;

function TReal_Array.GetAsString(i: Integer): String;
begin
  Result := FloatToStr(getItem(i));
end;

function TReal_Array.getItem(i: Integer): Real;
begin
  Result := pReal(FArray[i])^;
end;

function TReal_Array.ItemToString(i: Integer; SizeOfItem: byte): String;
begin
  Result := RightStr(FormatFloat('0.00', getItem(i)), SizeOfItem);
end;

procedure TReal_Array.SetAsString(i: Integer; const Value: String);
begin
  if Value <> '' then
     SetItem(i, StrToFloat(Value));
end;

procedure TReal_Array.SetItem(i: Integer; const Value: Real);
begin
  pReal(FArray[i])^ := Value;
end;

function TReal_Array.SizeOfElement: Integer;
begin
  Result := SizeOf(Real);
end;

end.

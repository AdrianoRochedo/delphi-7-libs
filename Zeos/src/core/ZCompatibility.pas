{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Compatibility Classes and Functions          }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZCompatibility;

interface

{$I ZCore.inc}

uses Classes, SysUtils;

type
  TIntegerDynArray      = array of Integer;
  TCardinalDynArray     = array of Cardinal;
  TWordDynArray         = array of Word;
  TSmallIntDynArray     = array of SmallInt;
  TByteDynArray         = array of Byte;
  TShortIntDynArray     = array of ShortInt;
  TInt64DynArray        = array of Int64;
  TLongWordDynArray     = array of LongWord;
  TSingleDynArray       = array of Single;
  TDoubleDynArray       = array of Double;
  TBooleanDynArray      = array of Boolean;
  TStringDynArray       = array of string;
  TWideStringDynArray   = array of WideString;

  PPointer              = ^Pointer;
  PByte                 = ^Byte;
  PBoolean              = ^Boolean;
  PShortInt             = ^ShortInt;
  PSmallInt             = ^SmallInt;
  PInteger              = ^Integer;
  PLongInt              = ^LongInt;
  PSingle               = ^Single;
  PDouble               = ^Double;
  PWord                 = ^Word;
  PWordBool             = ^WordBool;
  PCardinal             = ^Cardinal;
  PInt64                = ^Int64;

type
  TZStringList = class;

  TZStringListSortCompare = function(List: TZStringList;
    Index1, Index2: Integer): Integer;

  {** Implements a compatible TStringList class. }
  TZStringList = class(TStrings)
  private
{$IFDEF VER130BELOW}
    FUpdateCount: Integer;
{$ENDIF}

    FDelimiter: Char;
    FQuoteChar: Char;
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;

    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TZStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const Value: string);
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const Value: Char);

{$IFDEF VER130BELOW}
    property UpdateCount: Integer read FUpdateCount;
{$ENDIF}
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: string): Integer;
{$IFNDEF VER130BELOW}
      override;
{$ENDIF}
    procedure InsertItem(Index: Integer; const S: string;
      AObject: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
{$IFNDEF VER130BELOW}
      override;
{$ENDIF}
    procedure Sort; virtual;
    procedure CustomSort(Compare: TZStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{$IFDEF VER125BELOW}
type
  TLoginEvent = procedure(Sender: TObject; Username, Password: string) of object;
{$ENDIF}

{$IFDEF VER130BELOW}
var
  LoginDialogProc: function (const ADatabaseName: string; var AUserName, APassword: string): Boolean;
{$ENDIF}

{**
  Clears a list of objects and frees object instances.
  @param List a list of objects.
}
procedure ClearObjectList(List: TList);

{$IFDEF VER130KBELOW}

{**
  Converts string into float value.
  @param Str a string value.
  @param Def a default value.
  @return a converted value or default one if something wrong.
}
function StrToFloatDef(Str: string; Def: Extended): Extended;

{$ENDIF}

{$IFDEF VER130BELOW}

{**
  Converts variant into string value.
  @param Value a variant value.
  @param Def a default value.
  @return a converted value or default one if something wrong.
}
function VarToStrDef(Value: Variant; Def: string): string;

{**
  Checks variant is numeric.
  @param V a variant value.
  @return <code>True</code> is variant value has a numeric type.
}
function VarIsNumeric(const V: Variant): Boolean;

{**
  Checks variant is string.
  @param V a variant value.
  @return <code>True</code> is variant value has a string type.
}
function VarIsStr(const V: Variant): Boolean;

{**
  AnsiDequotedStr is a simplified version of AnsiExtractQuotedStr.
}
function AnsiDequotedStr(const S: string; AQuote: Char): string;

{$ENDIF}

implementation

uses
{$IFDEF VER130BELOW}
  Consts;
{$ELSE}
  RTLConsts;
{$ENDIF}

{ TZStringList }

constructor TZStringList.Create;
begin
  FDelimiter := ',';
  FQuoteChar := '"';
end;

destructor TZStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TZStringList.Add(const S: string): Integer;
begin
  Result := AddObject(S, nil);
end;

function TZStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TZStringList.Changed;
begin
  if (UpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TZStringList.Changing;
begin
  if (UpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TZStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TZStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
  Changed;
end;

procedure TZStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TZStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TZStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TZStringList.GetDelimitedText: string;
var
  S: string;
  P: PChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not (P^ in [#0..' ', QuoteChar, Delimiter]) do
        Inc(P);
      if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

procedure TZStringList.SetDelimitedText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    while P^ in [#1..' '] do
      Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while P^ in [#1..' '] do
        Inc(P);
      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (P^ in [#1..' ']);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TZStringList.GetDelimiter: Char;
begin
  Result := FDelimiter;
end;

procedure TZStringList.SetDelimiter(const Value: Char);
begin
  if FDelimiter <> Value then
  begin
    FDelimiter := Value;
  end
end;

function TZStringList.GetQuoteChar: Char;
begin
  Result := FQuoteChar;
end;

procedure TZStringList.SetQuoteChar(const Value: Char);
begin
  if FQuoteChar <> Value then
  begin
    FQuoteChar := Value;
  end
end;

function TZStringList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TZStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TZStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TZStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TZStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TZStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TZStringList.Insert(Index: Integer; const S: string);
begin
  InsertObject(Index, S, nil);
end;

procedure TZStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TZStringList.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TZStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TZStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TZStringList.QuickSort(L, R: Integer;
  SCompare: TZStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TZStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

procedure TZStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TZStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TZStringList;
  Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
    List.FList^[Index2].FString);
end;

procedure TZStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TZStringList.CustomSort(Compare: TZStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TZStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := AnsiCompareText(S1, S2);
end;

procedure TZStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

{ *************************** Functions ****************************}

{**
  Clears a list of objects and frees object instances.
  @param List a list of objects.
}
procedure ClearObjectList(List: TList);
var
  I: Integer;
  Current: TObject;
begin
  for I := 0 to List.Count - 1 do
  begin
    Current := TObject(List[I]);
    if Current <> nil then
      Current.Free;
  end;
  List.Clear;
end;

{$IFDEF VER130KBELOW}

{**
  Converts string into float value.
  @param Str a string value.
  @param Def a default value.
  @return a converted value or default one if something wrong.
}
function StrToFloatDef(Str: string; Def: Extended): Extended;
begin
  try
    if Str <> '' then
      Result := StrToFloat(Str)
    else Result := Def;
  except
    Result := Def;
  end;
end;

{$ENDIF}

{$IFDEF VER130BELOW}

{**
  Converts variant into string value.
  @param Value a variant value.
  @param Def a default value.
  @return a converted value or default one if something wrong.
}
function VarToStrDef(Value: Variant; Def: string): string;
begin
  try
    Result := VarToStr(Value);
  except
    Result := Def;
  end;
end;

{**
  Checks variant is numeric.
  @param V a variant value.
  @return <code>True</code> is variant value has a numeric type.
}
function VarIsNumeric(const V: Variant): Boolean;
begin
  Result := ((TVarData(V).VType and varTypeMask) in [varSmallInt, varInteger,
    varBoolean, varByte]) or ((TVarData(V).VType and varTypeMask)
    in [varSingle, varDouble, varCurrency]);
end;

{**
  Checks variant is string.
  @param V a variant value.
  @return <code>True</code> is variant value has a string type.
}
function VarIsStr(const V: Variant): Boolean;
begin
  Result := ((TVarData(V).VType and varTypeMask) = varOleStr) or
            ((TVarData(V).VType and varTypeMask) = varString);
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

{$ENDIF}

end.


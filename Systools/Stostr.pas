{$I STDEFINE.INC}

{$B-} {Incomplete boolean evaluation}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF OS32}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   STOSTR.PAS 1.05                     *}
{*                     String Class                      *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit STOStr;
{- String class }

interface

uses SysUtils, Classes, STBase, STStrZ;

const
{.Z+}
  DefAllocSize = 8;
  DefDelimiters = ' ';
  DefQuote = '''';
  DefRepeatValue = 1;
  DefResetRepeat = True;
  DefTabSize = 8;
  DefWrap = 80;
{.Z-}

type

  TStString = class(TPersistent)
{.Z+}
  protected {private}
    FAlloc : Cardinal;
    FBM : BTable;
    FBMString : PAnsiChar;
    FCursor : PAnsiChar;
    FDelimiters : PAnsiChar;
    FEnableCursor : Boolean;
    FItems : TStringList;
    FOneBased : Boolean;
    FRepeatValue : Cardinal;
    FResetRepeat : Boolean;
    FQuote : AnsiChar;
    FString : PAnsiChar;
    FTabSize : Byte;
    FTemp : PAnsiChar;
    FTempAlloc : Cardinal;
    FWrap : Cardinal;
  protected
    procedure AddIntToList(Num : LongInt);
    procedure AllocTemp(Size : Cardinal);
    procedure BMMakeTable(S : PAnsiChar);
    procedure CheckAlloc(Size : Cardinal);
    function  DesiredCursor : PAnsiChar;
    procedure FixCursor(Pos, Size : Cardinal; Delete : Boolean);
    function  GetAsciiCount : Cardinal;
    function  GetAsShortStr : ShortString;
    function  GetCursorPos : Cardinal;
    function  GetDelimiters : string;
    function  GetLength : Cardinal;
    function  GetRelativePos(Pos : Cardinal) : Cardinal;
    function  GetSoundex : string;
    function  GetWordCount : Cardinal;
    procedure SetAllocLength(Value : Cardinal);
    procedure SetAsShortStr(Value : ShortString);
    procedure SetCursorPos(Value : Cardinal);
    procedure SetDelimiters(Value : string);
    procedure SetItems(Value : TStringList);
    function  SuggestSize(Size : Cardinal) : Cardinal;
    procedure TempToString;
    procedure UpdateCursor(Pos : Cardinal);
  {$IFDEF WIN32}
    function  GetAsLongStr : AnsiString;
    procedure SetAsLongStr(Value : AnsiString);
    function  GetAsVariant : Variant;
    procedure SetAsVariant(Value : Variant);
  {$ENDIF}
{.Z-}
  public
    constructor Create;
    constructor CreateAlloc(Size : Cardinal); virtual;
    constructor CreateS(const S : string); virtual;
    constructor CreateZ(const S : PAnsiChar); virtual;
  {$IFDEF WIN32}
    constructor CreateV(const S : Variant); virtual;
  {$ENDIF}
    destructor Destroy; override;
    procedure AppendPChar(S : PAnsiChar);
    procedure AppendString(S : string);
    function  AsciiPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
    function  BMSearch(const S : string; var Pos : Cardinal) : Boolean;
    function  BMSearchUC(const S : string; var Pos : Cardinal) : Boolean;
    procedure Center(Size : Cardinal);
    procedure CenterCh(const C : AnsiChar; Size : Cardinal);
    function  CharCount(const C : AnsiChar) : Cardinal;
    function  CharExists(const C : AnsiChar) : boolean;
    procedure CharStr(const C : AnsiChar; Size : Cardinal);
    procedure ClearItems;
    procedure CursorNextWord;
    procedure CursorNextWordPrim;
    procedure CursorPrevWord;
    procedure CursorPrevWordPrim;
    procedure CursorToEnd;
    procedure DeleteAsciiAtCursor;
    procedure DeleteAtCursor(Length : Cardinal);
    procedure DeleteWordAtCursor;
    procedure Detab;
    procedure Entab;
    function  ExtractAscii(N : Cardinal) : string;
    function  ExtractWord(N : Cardinal) : string;
    procedure Filter(const Filters : PAnsiChar);
    function  GetAsciiAtCursor : string;
    function  GetAsciiAtCursorZ(Dest : PAnsiChar) : PAnsiChar;
    function  GetAsPChar(Dest : PAnsiChar) : PAnsiChar;
    function  GetWordAtCursor : string;
    function  GetWordAtCursorZ(Dest : PAnsiChar) : PAnsiChar;
    procedure InsertCRLFAtCursor;
    procedure InsertCRLF(Pos : Cardinal);
    procedure InsertPCharAtCursor(S : PAnsiChar);
    procedure InsertStringAtCursor(S : string);
    procedure ItemsToString;
    procedure LeftPad(Size : Cardinal);
    procedure LeftPadCh(const C : AnsiChar; Size : Cardinal);
    function  MakeLetterSet : LongInt;
    procedure MoveCursor(Delta : Integer);                             {!!.01}
    procedure Pack;
    procedure Pad(Size : Cardinal);
    procedure PadCh(const C : AnsiChar; Size : Cardinal);
    procedure ResetCursor;
    procedure Scramble(const Key : string);
    procedure SetAsPChar(S : PAnsiChar);
    function  SizeAsciiAtCursor(InclTrailers : Boolean) : Cardinal;
    function  SizeWordAtCursor(InclTrailers : Boolean) : Cardinal;
    procedure StrChDelete(Pos : Cardinal);
    procedure StrChInsert(const C : AnsiChar; Pos : Cardinal);
    function  StrChPos(const C : AnsiChar; var Pos : Cardinal) : Boolean;
    procedure StringToItems;
    procedure StripCRLFs;
    procedure StrStDelete(const Pos, Length : Cardinal);
    procedure StrStInsert(const S : string; Pos : Cardinal);
    function  StrStPos(const S : string; var Pos : Cardinal) : Boolean;
    procedure Substitute(FromStr, ToStr : PAnsiChar);
    procedure Trim;
    procedure TrimLead;
    procedure TrimSpaces;
    procedure TrimTrail;
    function  WordPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
    procedure WrapToItems;

    property AllocLength : Cardinal read FAlloc write SetAllocLength;
    property AsciiCount : Cardinal read GetAsciiCount;
  {$IFDEF WIN32}
    property AsLongStr : AnsiString read GetAsLongStr write SetAsLongStr;
    property AsVariant : Variant read GetAsVariant write SetAsVariant;
  {$ENDIF}
    property AsShortStr : ShortString read GetAsShortStr write SetAsShortStr;
    property CursorPos : Cardinal read GetCursorPos write SetCursorPos;
    property Delimiters : string read GetDelimiters write SetDelimiters;
    property EnableCursor : Boolean read FEnableCursor write FEnableCursor;
    property Length : Cardinal read GetLength;
    property Items : TStringList read FItems write SetItems;
    property OneBased : Boolean read FOneBased write FOneBased;
    property RepeatValue : Cardinal read FRepeatValue write FRepeatValue;
    property ResetRepeat : Boolean read FResetRepeat write FResetRepeat;
    property Soundex : string read GetSoundex;
    property Quote : AnsiChar read FQuote write FQuote;
    property TabSize : Byte read FTabSize write FTabSize;
    property WordCount : Cardinal read GetWordCount;
    property WrapColumn : Cardinal read FWrap write FWrap;
  end;

implementation

{$IFDEF SYSDEMO} uses SysUtil; {$ENDIF}

constructor TStString.Create;
{- Create nil string object. }
begin
  inherited Create;
  SetDelimiters(DefDelimiters);
  FItems := TStringList.Create;
  FTabSize := DefTabSize;
  FQuote := DefQuote;
  FRepeatValue := DefRepeatValue;
  FResetRepeat := DefResetRepeat;
  FWrap := DefWrap;
{$IFDEF SYSDEMO}
  _CC_; _VC_;
{$ENDIF}
end;

constructor TStString.CreateAlloc(Size : Cardinal);
{- Create string object allocated to given size. }
var
  AllocSize : Cardinal;
begin
  Create;
  AllocSize := SuggestSize(Size);
  FString := StrAlloc(AllocSize);
  FString[0] := #0;
  FAlloc := AllocSize;
  ResetCursor;
end;

{$IFDEF WIN32}
constructor TStString.CreateV(const S : Variant);
{- Create string object and copy variant into it. }
var
  Len : Cardinal;
  Temp : AnsiString;
begin
  Create;
  Temp := S;
  Len := System.Length(Temp);
  FString := StrAlloc(SuggestSize(Len));
  if Assigned(FString) then begin
    FAlloc := SuggestSize(Len);
    StrCopy(FString, PAnsiChar(Temp));
  end;
  ResetCursor;
end;
{$ENDIF}

constructor TStString.CreateS(const S : string);
{- Create string object and copy string into it. }
begin
  Create;
  FString := StrAlloc(SuggestSize(System.Length(S)));
  if Assigned(FString) then begin
    FAlloc := SuggestSize(System.Length(S));
    StrPCopy(FString, S);
  end;
  ResetCursor;
end;

constructor TStString.CreateZ(const S : PAnsiChar);
{- Create string object and copy PChar into it. }
begin
  Create;
  FString := StrAlloc(SuggestSize(StrLen(S)));
  if Assigned(FString) then begin
    StrCopy(FString, S);
    FAlloc := SuggestSize(StrLen(S));
  end;
  ResetCursor;
end;

destructor TStString.Destroy;
{- Dispose string object. }
begin
  FItems.Free;
  StrDispose(FBMString);
  StrDispose(FDelimiters);
  StrDispose(FString);
  inherited Destroy;
end;

procedure TStString.AppendPChar(S : PAnsiChar);
{- Appends PChar to end of string. }
var
  Temp : PAnsiChar;
begin
  CheckAlloc(StrLen(S) + GetLength);
  Temp := StrEnd(FString);
  StrCopy(Temp, S);
end;

procedure TStString.AppendString(S : string);
{- Appends string to end of string. }
var
  Temp : PAnsiChar;
begin
  CheckAlloc(System.Length(S) + GetLength);
  Temp := StrEnd(FString);
  StrPCopy(Temp, S);
end;

function TStString.AsciiPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
{- Returns the Pos of the Nth word using ASCII rules. }
var
  I, Num : Cardinal;
begin
  Result := False;
  Num := N;
  ClearItems;
  for I := 1 to FRepeatValue do begin
    if AsciiPositionZ(Num, DesiredCursor, FDelimiters, FQuote, Pos) then begin
      if Result = False then Inc(Num);
      Pos := GetRelativePos(Pos);
      Result := True;
      UpdateCursor(Pos);
      if FOneBased then Inc(Pos);
      AddIntToList(Pos);
    end;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

function TStString.BMSearch(const S : string; var Pos : Cardinal) : Boolean;
{- Performs case sensitive BM search on string. }
var
  I : Cardinal;
  Temp : PAnsiChar;
begin
  Result := False;
  ClearItems;
  Temp := StrAlloc(Succ(System.Length(S)));
  try
    StrPCopy(Temp, S);
    BMMakeTable(Temp);
    for I := 1 to FRepeatValue do begin
      if BMSearchZ(DesiredCursor^, StrLen(DesiredCursor), FBM, Temp, Pos) then begin
        Result := True;
        Pos := GetRelativePos(Pos);
        UpdateCursor(Pos);
        if FOneBased then Inc(Pos);
        AddIntToList(Pos);
        Inc(FCursor);
      end else if Result then begin
        Dec(FCursor);
        Exit;
      end;
    end;
  finally
    StrDispose(Temp);
  end;
end;

function TStString.BMSearchUC(const S : string; var Pos : Cardinal) : Boolean;
{- Performs case insensitive BM search on string. }
var
  I : Cardinal;
  Temp : PAnsiChar;
begin
  Result := False;
  ClearItems;
  Temp := StrAlloc(Succ(System.Length(S)));
  try
    StrPCopy(Temp, S);
    StrUpper(Temp);
    BMMakeTable(Temp);
    for I := 1 to FRepeatValue do begin
      if BMSearchUCZ(DesiredCursor^, StrLen(DesiredCursor), FBM, Temp, Pos) then begin
        Result := True;
        Pos := GetRelativePos(Pos);
        UpdateCursor(Pos);
        if FOneBased then Inc(Pos);
        AddIntToList(Pos);
        Inc(FCursor);
      end else if Result then begin
        Dec(FCursor);
        Exit;
      end;
    end;
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.Center(Size : Cardinal);
{- Centers string to Size. }
begin
  CheckAlloc(Size);
  CenterPrimZ(FString, Size);
  ResetCursor;
end;

procedure TStString.CenterCh(const C : AnsiChar; Size : Cardinal);
{- Centers string with 'Ch' to Size. }
begin
  CheckAlloc(Size);
  CenterChPrimZ(FString, C, Size);
  ResetCursor;
end;

function TStString.CharExists(const C : AnsiChar) : boolean;
{- Determines whether C exists in string. }
begin
  Result := CharExistsZ(DesiredCursor, C);
end;

function TStString.CharCount(const C : AnsiChar) : Cardinal;
{- Counts C in string. }
begin
  Result := CharCountZ(DesiredCursor, C);
end;

procedure TStString.CharStr(const C : AnsiChar; Size : Cardinal);
{- Fills string to Size with C. }
begin
  CheckAlloc(Size);
  FString := CharStrZ(FString, C, Size);
  ResetCursor;
end;

procedure TStString.AddIntToList(Num : LongInt);
{- Adds integer value to Items -- as both numeric value and numeric string. }
begin
  FItems.AddObject(IntToStr(Num), TObject(Num));
end;

procedure TStString.AllocTemp(Size : Cardinal);
{- Allocates FTemp to Size. }
begin
  FTemp := StrAlloc(Size);
  FTempAlloc := Size;
end;

procedure TStString.BMMakeTable(S : PAnsiChar);
{- Checks whether table needs to be made -- and makes it. }
begin
  if Assigned(FBMString) then
    if StrComp(S, FBMString) = 0 then Exit;
  StrDispose(FBMString);
  FBMString := StrNew(S);
  BMMakeTableZ(FBMString, FBM);
end;

procedure TStString.CheckAlloc(Size : Cardinal);
{- Sets allocated length for string if needed size is > current size. }
begin
  if FAlloc = 0 then begin
    FString := StrAlloc(SuggestSize(Size));
    FAlloc := SuggestSize(Size);
    FString[0] := #0;
    ResetCursor;
  end else if Succ(Size) > FAlloc then
    SetAllocLength(Succ(Size));
end;

procedure TStString.ClearItems;
{- Clears Items list. }
begin
  FItems.Clear;
end;

procedure TStString.CursorNextWord;
{- Moves cursor to the beginning of the next word, or terminating null. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    CursorNextWordPrim;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.CursorNextWordPrim;
{- Moves cursor to the beginning of the next word, or terminating null. }
var
  Ch : AnsiChar;
begin
  Ch := FCursor^;
  while (Ch <> #0) and (not CharExistsZ(FDelimiters, Ch)) do begin
    Inc(FCursor);
    Ch := FCursor^;
  end;
  while (Ch <> #0) and (CharExistsZ(FDelimiters, Ch)) do begin
    Inc(FCursor);
    Ch := FCursor^;
  end;
end;

procedure TStString.CursorPrevWord;
{- Move Cursor to beginning of prev word, or first word in string. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    CursorPrevWordPrim;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.CursorPrevWordPrim;
{- Move Cursor to beginning of prev word, or first word in string. }
var
  Ch : AnsiChar;
begin
  Ch := FCursor^;
  while (FCursor <> FString) and (CharExistsZ(FDelimiters, Ch)) do begin
    Dec(FCursor);
    Ch := FCursor^;
  end;
  while (FCursor <> FString) and (not CharExistsZ(FDelimiters, Ch)) do begin
    Dec(FCursor);
    Ch := FCursor^;
  end;
end;

procedure TStString.CursorToEnd;
{- Set cursor to null terminator at the end of string. }
begin
  FCursor := StrEnd(FString);
end;

procedure TStString.DeleteAsciiAtCursor;
{- Deletes word (and any trailing delimiters) at cursor following ASCII rules. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    StrStDeletePrimZ(FCursor, 0, SizeAsciiAtCursor(True));
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.DeleteAtCursor(Length : Cardinal);
{- Deletes Length number of characters at cursor. }
begin
  StrStDeletePrimZ(FCursor, 0, Length);
end;

procedure TStString.DeleteWordAtCursor;
{- Deletes word (and any trailing delimiters) at cursor. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    StrStDeletePrimZ(FCursor, 0, SizeWordAtCursor(True));
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.Detab;
{- Detabs string. }
begin
  AllocTemp(SuggestSize(FAlloc + (CharCountZ(FString, #9)*FTabSize)));
  if Assigned(FTemp) then begin
    DetabZ(FTemp, FString, FTabSize);
    TempToString;
  end;
  ResetCursor;
end;

procedure TStString.Entab;
{- Entabs string. }
begin
  AllocTemp(FAlloc);
  if Assigned(FTemp) then begin
    EntabZ(FTemp, FString, FTabsize);
    TempToString;
  end;
  ResetCursor;
end;

function TStString.ExtractAscii(N : Cardinal) : string;
{- Extracts the N'th word in string. }
var
  OldCursor : PAnsiChar;
  Position : Cardinal;
begin
  Result := '';
  OldCursor := FCursor;
  if AsciiPositionZ(N, FString, FDelimiters, FQuote, Position) then
    FCursor := FString + Position
  else Exit;
  Result := GetAsciiAtCursor;
  FCursor := OldCursor;
end;

function TStString.ExtractWord(N : Cardinal) : string;
{- Extracts the N'th word in string. }
var
  OldCursor : PAnsiChar;
  Position : Cardinal;
begin
  Result := '';
  OldCursor := FCursor;
  if WordPositionZ(N, FString, FDelimiters, Position) then
    FCursor := FString + Position
  else Exit;
  Result := GetWordAtCursor;
  FCursor := OldCursor;
end;

procedure TStString.Filter(const Filters : PAnsiChar);
{- Filters characters from string. }
begin
  AllocTemp(FAlloc);
  FilterZ(FTemp, DesiredCursor, Filters);
  TempToString;
  ResetCursor;
end;

procedure TStString.FixCursor(Pos, Size : Cardinal; Delete : Boolean);
{- Fixes cursor position following an insertion or deletion. }
begin
  if (FCursor - FString) < Pos then Exit;
  if Delete then begin
    FCursor := FCursor - Size;
    if (FCursor - FString) < Pos then
      FCursor := FString + Pos;
  end else begin
    if (FCursor - FString) = Pos then Exit;
    FCursor := FCursor + Size;
  end;
end;

function TStString.MakeLetterSet : LongInt;
{- Performs MakeLetterSetZ on the word at the Cursor. }
var
  Temp : PAnsiChar;
begin
  Temp := StrAlloc(Succ(SizeWordAtCursor(False)));
  try
    GetWordAtCursorZ(Temp);
    Result := MakeLetterSetZ(Temp);
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.MoveCursor(Delta : Integer);                       {!!.01}
{- Moves Cursor by Delta characters. }
begin
  Inc(FCursor, Delta);
  if FCursor < FString then
    FCursor := FString;
  if FCursor > (FString + Succ(GetLength)) then
    CursorToEnd;
end;

function TStString.GetAsciiAtCursor : string;
{- Gets the word that the Cursor is pointing to -- returns string. }
var
  I, Size : Longint;
begin
  Size := SizeAsciiAtCursor(False);
{$IFDEF Win32}
  SetLength(Result, Size);
{$ELSE}
  if Size > 255 then
    Size := 255;
  Result[0] := Char(Size);
{$ENDIF}
  for I := 0 to Pred(Size) do
    Result[Succ(I)] := FCursor[I];
end;

function TStString.GetAsciiAtCursorZ(Dest : PAnsiChar) : PAnsiChar;
{- Gets the word that the Cursor is pointing to -- returns PChar. }
var
  I, Size : Longint;
begin
  Size := SizeAsciiAtCursor(False);
  Result := Dest;
  for I := 0 to Pred(Size) do
    Dest[I] := FCursor[I];
  Dest[Size] := #0;
end;

function TStString.GetAsPChar(Dest : PAnsiChar) : PAnsiChar;
{- Exports string as a null-terminated string. }
begin
  Result := Dest;
  StrCopy(Result, FString);
end;

function TStString.GetWordAtCursor : string;
{- Gets the word that the Cursor is pointing to -- returns string. }
var
  I, Size : Longint;
begin
  Size := SizeWordAtCursor(False);
{$IFDEF Win32}
  SetLength(Result, Size);
{$ELSE}
  if Size > 255 then
    Size := 255;
  Result[0] := Char(Size);
{$ENDIF}
  for I := 0 to Pred(Size) do
    Result[Succ(I)] := FCursor[I];
end;

function TStString.GetWordAtCursorZ(Dest : PAnsiChar) : PAnsiChar;
{- Gets the word that the Cursor is pointing to -- returns PChar. }
var
  I, Size : Longint;
begin
  Size := SizeWordAtCursor(False);
  Result := Dest;
  for I := 0 to Pred(Size) do
    Dest[I] := FCursor[I];
  Dest[Size] := #0;
end;

procedure TStString.Pack;
{- Sets string allocation to minimum size. }
var
  StrLen : Cardinal;
begin
  StrLen := GetLength;
  if SuggestSize(StrLen) < FAlloc then
    SetAllocLength(StrLen);
end;

procedure TStString.Pad(Size : Cardinal);
{- Pads string. }
begin
  CheckAlloc(Size);
  PadPrimZ(FString, Size);
  ResetCursor;
end;

procedure TStString.InsertCRLFAtCursor;
{- Inserts CR/LF pair at cursor position. }
var
  Pos : Cardinal;
begin
  CheckAlloc(GetLength + 2);
  Pos := FCursor - FString;
  StrStInsertPrimZ(FString, #13#10, Pos);
end;

procedure TStString.InsertCRLF(Pos : Cardinal);
{- Inserts CR/LF pair at given position. }
var
  AdjPos : Cardinal;
begin
  CheckAlloc(GetLength + 2);
  AdjPos := Pos;
  if FOneBased then Dec(AdjPos);
  StrStInsertPrimZ(FString, #13#10, AdjPos);
end;

procedure TStString.InsertPCharAtCursor(S : PAnsiChar);
{- Inserts null-terminated string at cursor position. }
var
  Len, Pos : Cardinal;
begin
  Len := StrLen(S);
  Pos := FCursor - FString;
  CheckAlloc(GetLength + Len);
  StrStInsertPrimZ(FString, S, Pos);
end;

procedure TStString.InsertStringAtCursor(S : string);
{- Inserts string at cursor position. }
var
  Pos, Len : Cardinal;
  Temp : PAnsiChar;
begin
  Pos := FCursor - FString;
  Len := System.Length(S);
  Temp := StrAlloc(Succ(Len));
  try
    StrPCopy(Temp, S);
    CheckAlloc(GetLength + Len);
    StrStInsertPrimZ(FString, Temp, Pos);
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.ItemsToString;
{- Copies items strings to string. }
begin
  SetAsPChar(FItems.GetText);
end;

procedure TStString.LeftPad(Size : Cardinal);
{- Left pad string. }
begin
  CheckAlloc(Size);
  LeftPadPrimZ(FString, Size);
  ResetCursor;
end;

procedure TStString.LeftPadCh(const C : AnsiChar; Size : Cardinal);
{- Left pad string with C. }
begin
  CheckAlloc(Size);
  LeftPadChPrimZ(FString, C, Size);
  ResetCursor;
end;

procedure TStString.PadCh(const C : AnsiChar; Size : Cardinal);
{- Pad string with C. }
begin
  CheckAlloc(Size);
  PadChPrimZ(FString, C, Size);
  ResetCursor;
end;

procedure TStString.ResetCursor;
{- Resets Cursor to beginning of string. }
begin
  FCursor := FString;
end;

procedure TStString.Scramble(const Key : string);
{- Encrypts / Decrypts string. }
var
  Temp : PAnsiChar;
begin
  Temp := StrAlloc(Succ(System.Length(Key)));
  try
    StrPCopy(Temp, Key);
    ScramblePrimZ(FString, Temp);
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.SetAsPChar(S : PAnsiChar);
{- Sets string to PChar. }
begin
  CheckAlloc(StrLen(S));
  StrCopy(FString, S);
  ResetCursor;
end;

function TStString.SizeAsciiAtCursor(InclTrailers : Boolean) : Cardinal;
{- Get the size of the word that the Cursor on (follows ASCII rules). }
var
  TempPtr : PAnsiChar;
  Ch : AnsiChar;
  InQuote : Boolean;
begin
  InQuote := False;
  TempPtr := FCursor;
  Ch := TempPtr^;
  while (Ch <> #0) and ((InQuote) or (not CharExistsZ(FDelimiters, Ch))) do begin
    if Ch = FQuote then
      InQuote := not InQuote;
    Inc(TempPtr);
    Ch := TempPtr^;
  end;
  if InclTrailers then begin
    while (Ch <> #0) and CharExistsZ(FDelimiters, Ch) do begin
      Inc(TempPtr);
      Ch := TempPtr^;
    end;
  end;
  Result := TempPtr - FCursor;
end;

function TStString.SizeWordAtCursor(InclTrailers : Boolean) : Cardinal;
{- Get the size of the word that the Cursor is pointing to. }
var
  TempPtr : PAnsiChar;
  Ch : AnsiChar;
begin
  TempPtr := FCursor;
  Ch := TempPtr^;
  while (Ch <> #0) and (not CharExistsZ(FDelimiters, Ch)) do begin
    Inc(TempPtr);
    Ch := TempPtr^;
  end;
  if InclTrailers then begin
    while (Ch <> #0) and CharExistsZ(FDelimiters, Ch) do begin
      Inc(TempPtr);
      Ch := TempPtr^;
    end;
  end;
  Result := TempPtr - FCursor;
end;

procedure TStString.StrChDelete(Pos : Cardinal);
{- Delete character at Pos. }
var
  AdjPos : Cardinal;
begin
  AdjPos := Pos;
  if FOneBased then Dec(AdjPos);
  StrChDeletePrimZ(FString, AdjPos);
  FixCursor(AdjPos, 1, True);
end;

procedure TStString.StrChInsert(const C : AnsiChar; Pos : Cardinal);
{- Insert character at Pos. }
var
  AdjPos : Cardinal;
begin
  AdjPos := Pos;
  CheckAlloc(Succ(GetLength));
  if FOneBased then Dec(AdjPos);
  StrChInsertPrimZ(FString, C, AdjPos);
  FixCursor(AdjPos, 1, False);
end;

function TStString.StrChPos(const C : AnsiChar; var Pos : Cardinal) : Boolean;
{- Search string for character. }
var
  I : Cardinal;
begin
  Result := False;
  ClearItems;
  for I := 1 to FRepeatValue do begin
    if StrChPosZ(DesiredCursor, C, Pos) then begin
      Result := True;
      Pos := GetRelativePos(Pos);
      UpdateCursor(Pos);
      if FOneBased then Inc(Pos);
      AddIntToList(Pos);
      Inc(FCursor);
    end else if Result then begin
      Dec(FCursor);
      Exit;
    end;
  end;
end;

procedure TStString.StripCRLFs;
{- Strip all CR/LF pairs from string, replacing them with a space. }
begin
  Filter(#13);
  Substitute(#10, ' ');
  ResetCursor;
end;

procedure TStString.StrStDelete(const Pos, Length : Cardinal);
{- Delete substring from string at Pos. }
var
  AdjPos : Cardinal;
begin
  AdjPos := Pos;
  if FOneBased then Dec(AdjPos);
  StrStDeletePrimZ(FString, AdjPos, Length);
  FixCursor(AdjPos, Length, True);
end;

procedure TStString.StrStInsert(const S : string; Pos : Cardinal);
{- Insert string at Pos. }
var
  AdjPos, Len : Cardinal;
  Temp : PAnsiChar;
begin
  AdjPos := Pos;
  Len := System.Length(S);
  Temp := StrAlloc(Succ(Len));
  try
    StrPCopy(Temp, S);
    if FOneBased then Dec(AdjPos);
    CheckAlloc(GetLength + Len);
    StrStInsertPrimZ(FString, Temp, AdjPos);
  finally
    StrDispose(Temp);
  end;
  FixCursor(AdjPos, Len, False);
end;

function TStString.StrStPos(const S : string; var Pos : Cardinal) : Boolean;
{- Search for substring in string. }
var
  I : Cardinal;
  Temp : PAnsiChar;
begin
  Result := False;
  ClearItems;
  Temp := StrAlloc(Succ(System.Length(S)));
  try
    StrPCopy(Temp, S);
    for I := 1 to FRepeatValue do begin
      if StrStPosZ(DesiredCursor, Temp, Pos) then begin
        Result := True;
        Pos := GetRelativePos(Pos);
        UpdateCursor(Pos);
        if FOneBased then inc(Pos);
        AddIntToList(Pos);
        Inc(FCursor);
      end else if Result then begin
        Dec(FCursor);
        Exit;
      end;
    end;
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.Substitute(FromStr, ToStr : PAnsiChar);
{- Substitute characters in string. }
var
  CursorDelta : Cardinal;
begin
  AllocTemp(FAlloc);
  CursorDelta := FCursor - FString;
  if Assigned(FTemp) then begin
    SubstituteZ(FTemp, FString, FromStr, ToStr);
    TempToString;
  end;
  FCursor := FString + CursorDelta;
end;

procedure TStString.Trim;
{- Trim string. }
begin
  TrimPrimZ(FString);
  ResetCursor;
end;

procedure TStString.TrimLead;
{- Trim leading whitespace from string. }
begin
  TrimLeadPrimZ(FString);
  ResetCursor;
end;

procedure TStString.TrimSpaces;
{- Trim spaces from string. }
begin
  TrimSpacesPrimZ(FString);
  ResetCursor;
end;

procedure TStString.TrimTrail;
{- Trim trailing whitespace from string. }
begin
  TrimTrailPrimZ(FString);
  ResetCursor;
end;

function TStString.WordPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
{- Return the position of the N'th word. }
var
  I, Temp, Num : Cardinal;
begin
  Result := False;
  Num := N;
  ClearItems;
  for I := 1 to FRepeatValue do begin
    if WordPositionZ(Num, DesiredCursor, FDelimiters, Temp) then begin
      if Result = False then Inc(Num);
      Pos := GetRelativePos(Temp);
      Result := True;
      UpdateCursor(Pos);
      if FOneBased then Inc(Pos);
      AddIntToList(Pos);
    end;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.WrapToItems;
{- Copy string to items with word wrap. }
var
  I, J : Cardinal;
  Anchor, Cur, EndTemp : PAnsiChar;
  InWord, EndFound : Boolean;
begin
  if GetLength > FWrap then begin
    EndFound := False;
    AllocTemp(SuggestSize(GetLength + (GetLength div FWrap * 2)));
    FTemp^ := #0;
    Anchor := FString;
    Cur := FString;
    repeat
      I := 0;
      J := 0;
      InWord := False;
      while (Cur^ <> #0) and (I < Succ(FWrap)) do begin
        if CharExistsZ(' '#13#10, Cur^) then begin
          if InWord then begin
            InWord := False;
            J := I;
          end;
          if Cur^ <> ' ' then Break;
        end else begin
          InWord := True;
        end;
        Inc(I);
        Inc(Cur);
      end;
      if Cur^ = #0 then begin
        EndFound := True;
        J := I;
      end;
      EndTemp := StrEnd(FTemp);
      if InWord and (J = 0) then
        J := FWrap;
      StrLCopy(EndTemp, Anchor, J);
      if not EndFound then begin
        StrCat(FTemp, #13#10);
        Anchor := Anchor + J;
        while Anchor^ = ' ' do
          Inc(Anchor);
        if Anchor^ = #13 then Inc(Anchor);
        if Anchor^ = #10 then Inc(Anchor);
        Cur := Anchor;
      end;
    until EndFound;
    FItems.SetText(FTemp);
    StrDispose(FTemp);
  end else begin
    StringToItems;
  end;
end;

function TStString.DesiredCursor : PAnsiChar;
{- Returns FString or FCursor. }
begin
  if FEnableCursor then
    Result := FCursor
  else
    Result := FString;
end;

function TStString.GetAsciiCount : Cardinal;
{- Count words following ASCII rules. }
begin
  Result := AsciiCountZ(FString, FDelimiters, FQuote);
end;

function TStString.GetAsShortStr : ShortString;
{- Provide short string output. }
begin
  Result := StrPas(FString);
end;

function TStString.GetCursorPos : Cardinal;
{- Return the position of the Cursor relative to the beginning of the string. }
begin
  Result := FCursor - FString;
  if FOneBased then Inc(Result);
end;

function TStString.GetDelimiters : string;
{- Return string with current delimiters. }
begin
  Result := StrPas(FDelimiters);
end;

function TStString.GetLength : Cardinal;
{- Return the length of the string. }
begin
  if Assigned(FString) then
    Result := StrLen(FString)
  else
    Result := 0;
end;

function TStString.GetRelativePos(Pos : Cardinal) : Cardinal;
{- Return position relative to FString. }
begin
  if FEnableCursor then
    Result := Pos + FCursor - FString
  else
    Result := Pos;
end;

function TStString.GetSoundex : string;
{- Return Soundex for word at Cursor. }
var
  I : Integer;
  Temp, Dest : PAnsiChar;
begin
  ClearItems;
  Dest := StrAlloc(5);
  try
    for I := 1 to FRepeatValue do begin
      if FCursor^ = #0 then Exit;
      Temp := StrAlloc(Succ(SizeWordAtCursor(False)));
      try
        GetWordAtCursorZ(Temp);
        Result := StrPas(SoundexZ(Dest, Temp));
        FItems.Add(Result);
      finally
        StrDispose(Temp);
      end;
      if FRepeatValue > 1 then CursorNextWordPrim;
    end;
  finally
    StrDispose(Dest);
  end;
end;

function TStString.GetWordCount : Cardinal;
{- Count words in string. }
begin
  Result := WordCountZ(FString, FDelimiters);
end;

procedure TStString.SetAllocLength(Value : Cardinal);
{- Sets allocated length for string - including the terminating null. }
begin
  if Value <> FAlloc then begin
    AllocTemp(SuggestSize(Value));
    if Assigned(FTemp) then begin
      if Assigned(FString) then begin
        StrLCopy(FTemp, FString, Value);
      end;
      TempToString;
    end;
  end;
end;

procedure TStString.SetAsShortStr(Value : ShortString);
{- Copy short string into string object. }
begin
  CheckAlloc(Byte(Value[0]));
  StrPCopy(FString, Value);
  ResetCursor;
end;

procedure TStString.SetCursorPos(Value : Cardinal);
{- Sets the position of the cursor. }
begin
  FCursor := FString + Value;
  if FOneBased then Dec(FCursor);
end;

procedure TStString.SetDelimiters(Value : string);
{- Set the delimiters. }
begin
  StrDispose(FDelimiters);
  FDelimiters := StrAlloc(Succ(System.Length(Value)));
  if Assigned(FDelimiters) then
    StrPCopy(FDelimiters, Value);
end;

procedure TStString.SetItems(Value: TStringList);
{- Sets Items. }
begin
  FItems.Assign(Value);
end;

procedure TStString.StringToItems;
{- Copies string into items -- respects CR/LF. }
begin
  FItems.SetText(FString);
end;

function TStString.SuggestSize(Size : Cardinal) : Cardinal;
{- Internal method -- returns recommended size for allocation. }
var
  AdjSize, Delta : Cardinal;
begin
  AdjSize := Succ(Size);
  Delta := AdjSize mod DefAllocSize;
  Result := AdjSize - Delta + DefAllocSize;
end;

procedure TStString.TempToString;
{- Internal method -- copys temp to string. }
begin
  FAlloc := FTempAlloc;
  FCursor := (FCursor - FString) + FTemp;
  StrDispose(FString);
  FString := FTemp;
  FTemp := nil;
end;

procedure TStString.UpdateCursor(Pos : Cardinal);
{- Internal method -- updates cursor position if necessary. }
begin
  if EnableCursor then
    FCursor := FString + Pos;
end;

{$IFDEF WIN32}
function TStString.GetAsLongStr : AnsiString;
{- Provide output as long string. }
begin
  Result := FString;
end;

procedure TStString.SetAsLongStr(Value : AnsiString);
{- Copy long string into string object. }
begin
  CheckAlloc(System.Length(Value));
  StrCopy(FString, PAnsiChar(Value));
  ResetCursor;
end;

function TStString.GetAsVariant : Variant;
{- Provide output as variant. }
begin
  Result := StrPas(FString);
end;

procedure TStString.SetAsVariant(Value : Variant);
{- Copy variant into string object. }
var
  Temp : AnsiString;
begin
  Temp := Value;
  CheckAlloc(System.Length(Temp));
  StrCopy(FString, PAnsiChar(Temp));
  ResetCursor;
end;

{$ENDIF}

{$IFDEF SYSDEMO}
initialization
  _CC_; _VC_;
{$ENDIF}

end.

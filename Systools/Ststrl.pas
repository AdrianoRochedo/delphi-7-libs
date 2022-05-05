{$I STDEFINE.INC}

{$A+} {Aligned records}
{$B-} {Incomplete boolean evaluation}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF WIN32}
  !! Error: This unit can only be compiled with Delphi 2.0
{$ENDIF}

{*********************************************************}
{*                   STSTRL.PAS 1.05                     *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit STStrL;
  {-Long string routines}

interface

uses
  Windows,
  SysUtils,
  STConst,
  STBase;

{.Z+}
type
  LStrRec = record
    AllocSize : Longint;
    RefCount  : Longint;
    Length    : Longint;
  end;

const
  StrOffset = SizeOf(LStrRec);
{.Z-}

  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}

function HexLL(L : LongInt) : AnsiString;
  {-Return the hex string for a long integer.}

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}

function BinaryLL(L : LongInt) : AnsiString;
  {-Return the binary string for a long integer.}

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}

function OctalLL(L : LongInt) : AnsiString;
  {-Return an octal string for a long integer.}

function Str2Int16L(const S : AnsiString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

function Str2WordL(const S : AnsiString; var I : Word) : Boolean;
  {-Convert a string to a word.}

function Str2LongL(const S : AnsiString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

{!!.03 Real to Double for BCB}
function Str2RealL(const S : AnsiString; var R : Double) : Boolean;
  {-Convert a string to a real.}

function Str2ExtL(const S : AnsiString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}

function Long2StrL(L : LongInt) : AnsiString;
  {-Convert an integer type to a string.}

{!!.03 Real to Double for BCB}
function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert a real to a string.}

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert an extended to a string.}

function ValPrepL(const S : AnsiString) : AnsiString;                  {!!.01}
  {-Prepares a string for calling Val.}

  {-------- General purpose string manipulation --------}

function CharStrL(C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Return a string filled with the specified character.}

function PadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with a specified character.}

function PadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with spaces.}

function LeftPadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with a specified character.}

function LeftPadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with spaces.}

function TrimLeadL(const S : AnsiString) : AnsiString;
  {-Return a string with leading white space removed.}

function TrimTrailL(const S : AnsiString) : AnsiString;
  {-Return a string with trailing white space removed.}

function TrimL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing spaces removed.}

function CenterChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with a specified character.}

function CenterL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with spaces.}

function EntabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Convert blanks in a string to tabs.}

function DetabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Expand tabs in a string to blanks.}

function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteL(const S, FromStr, ToStr : AnsiString) : AnsiString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterL(const S, Filters : AnsiString) : AnsiString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}

  {--------------- Word / Char manipulation -------------------------}

function CharExistsL(const S : AnsiString; C : AnsiChar) : Boolean;
  {-Determine whether a given character exists in a string. }

function CharCountL(const S : AnsiString; C : AnsiChar) : Cardinal;
  {-Count the number of a given character in a string. }

function WordCountL(const S, WordDelims : AnsiString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}

function WordPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordL(N : Cardinal; const S, WordDelims : AnsiString) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountL(const S, WordDelims : AnsiString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}

function AsciiPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}

function ExtractAsciiL(N : Cardinal; const S, WordDelims : AnsiString;
                       Quote : AnsiChar) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapL(const InSt : AnsiString; var OutSt, Overlap : AnsiString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

  {--------------- String comparison and searching -----------------}
function CompStringL(const S1, S2 : AnsiString) : Integer;
  {-Compare two strings.}

function CompUCStringL(const S1, S2 : AnsiString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}

function SoundexL(const S : AnsiString) : AnsiString;
  {-Return 4 character soundex of an input string.}

function MakeLetterSetL(const S : AnsiString) : LongInt;
  {-Return a bit-mapped long storing the individual letters contained in S.}

procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable);
  {-Build a Boyer-Moore link table}

function BMSearchL(var Buffer; BufLength : Cardinal; var BT : BTable;
                  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string.}

function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
                    const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Return a file name with a default extension attached.}

function ForceExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Force the specified extension onto the file name.}

function JustFilenameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename and extension of a pathname.}

function JustNameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionL(const Name : AnsiString) : AnsiString;
  {-Return just the extension of a pathname.}

function JustPathnameL(const PathName : AnsiString) : AnsiString;
  {-Return just the drive and directory portion of a pathname.}

function AddBackSlashL(const DirName : AnsiString) : AnsiString;
  {-Add a default backslash to a directory name.}

function CleanPathNameL(const PathName : AnsiString) : AnsiString;
  {-Return a pathname cleaned up as DOS does it.}

function HasExtensionL(const Name : AnsiString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

  {------------------ Formatting routines --------------------}

function CommaizeL(L : LongInt) : AnsiString;
  {-Convert a long integer to a string with commas.}

function CommaizeChL(L : LongInt; Ch : AnsiChar) : AnsiString;
  {-Convert a long integer to a string with Ch in comma positions.}

function FloatFormL(const Mask : AnsiString ; R : TstFloat ; const LtCurr,
                    RtCurr : AnsiString ; Sep, DecPt : AnsiChar) : AnsiString;
  {-Return a formatted string with digits from R merged into mask.}

function LongIntFormL(const Mask : AnsiString ; L : LongInt ; const LtCurr,
                      RtCurr : AnsiString ; Sep : AnsiChar) : AnsiString;
  {-Return a formatted string with digits from L merged into mask.}

function StrChPosL(const P : AnsiString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}

function StrStPosL(const P, S : AnsiString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Copy characters at a specified position in a string.}

function StrChInsertL(const S : AnsiString; C : AnsiChar; Pos : Cardinal) : AnsiString;
  {-Insert a character into a string at a specified position.}

function StrStInsertL(const S1, S2 : AnsiString; Pos : Cardinal) : AnsiString;
  {-Insert a string into another string at a specified position.}

function StrChDeleteL(const S : AnsiString; Pos : Cardinal) : AnsiString;
  {-Delete the character at a specified position in a string.}

function StrStDeleteL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Delete characters at a specified position in a string.}

implementation

{$IFDEF SYSDEMO} uses SysUtil; {$ENDIF}

  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}
begin
  SetLength(Result, 2);
  Result[1] := Digits[B shr 4];
  Result[2] := Digits[B and $F];
end;

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}
begin
  SetLength(Result, 4);
  Result[1] := Digits[hi(W) shr 4];
  Result[2] := Digits[hi(W) and $F];
  Result[3] := Digits[lo(W) shr 4];
  Result[4] := Digits[lo(W) and $F];
end;

function HexLL(L : LongInt) : AnsiString;
  {-Return the hex string for a long integer.}
begin
  SetLength(Result, 8);
  Result := HexWL(HiWord(L)) + HexWL(LoWord(L));
end;

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}
begin
  SetLength(Result, 9);
  Result := ':' + HexLL(LongInt(P));
end;

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 8);
  for I := 7 downto 0 do begin
    Result[N] := Digits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 16);
  for I := 15 downto 0 do begin
    Result[N] := Digits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLL(L : LongInt) : AnsiString;
  {-Return the binary string for a long integer.}
var
  I : LongInt;
  N : Byte;
begin
  N := 1;
  SetLength(Result, 32);
  for I := 31 downto 0 do begin
    Result[N] := Digits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}
var
  I : Word;
begin
  SetLength(Result, 3);
  for I := 0 to 2 do begin
    Result[3-I] := Digits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}
var
  I : Word;
begin
  SetLength(Result, 6);
  for I := 0 to 5 do begin
    Result[6-I] := Digits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLL(L : LongInt) : AnsiString;
  {-Return an octal string for a long integer.}
var
  I : Word;
begin
  SetLength(Result, 12);
  for I := 0 to 11 do begin
    Result[12-I] := Digits[L and 7];
    L := L shr 3;
  end;
end;

function Str2Int16L(const S : AnsiString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailL(S);                                                 {!!.03}
(*                                                                     {!!.03}
  St := S;                                                             {!!.03}
  while St[Length(St)] = ' ' do                                        {!!.03}
    SetLength(St, Pred(Length(St)));                                   {!!.03}
*)                                                                     {!!.03}
  if (St = '') then Exit;                                              {!!.03}

  if (Length(St) > 1) and (Upcase(St[Length(St)]) = 'H') then begin
    Move(St[1], St[2], Pred(Length(St)));
    St[1] := '$';
  end else if (Length(St) > 2) and (St[1] = '0') and (Upcase(St[2]) = 'X') then begin
    Move(St[3], St[2], Pred(Length(St)));                              {!!.03}
    SetLength(St, Pred(Length(St)));
{    Move(St[3], St[2], Pred(Length(St)));}
    St[1] := '$';
  end;
  Val(St, I, Code);
  if Code <> 0 then begin
    I := Code;
  end else
    Result := True;
end;

function Str2WordL(const S : AnsiString; var I : Word) : Boolean;
  {-Convert a string to a word.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailL(S);                                                 {!!.03}
(*                                                                     {!!.03}
  St := S;                                                             {!!.03}
  while St[Length(St)] = ' ' do                                        {!!.03}
    SetLength(St, Pred(Length(St)));                                   {!!.03}
*)                                                                     {!!.03}
  if (St = '') then Exit;                                              {!!.03}

  if (Length(St) > 1) and (Upcase(St[Length(St)]) = 'H') then begin
    Move(St[1], St[2], Pred(Length(St)));
    St[1] := '$';
  end
  else if (Length(St) > 2) and (St[1] = '0') and (Upcase(St[2]) = 'X') then begin
    Move(St[3], St[2], Pred(Length(St)));                              {!!.03}
    SetLength(St, Pred(Length(St)));
{    Move(St[3], St[2], Pred(Length(St)));}
    St[1] := '$';
  end;
  Val(St, I, Code);
  if Code <> 0 then begin
    I := Code;
  end else
    Result := True;
end;

function Str2LongL(const S : AnsiString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailL(S);                                                 {!!.03}
(*                                                                     {!!.03}
  St := S;                                                             {!!.03}
  while St[Length(St)] = ' ' do                                        {!!.03}
    SetLength(St, Pred(Length(St)));                                   {!!.03}
*)                                                                     {!!.03}
  if (St = '') then Exit;                                              {!!.03}

  if (Length(St) > 1) and (Upcase(St[Length(St)]) = 'H') then begin
    Move(St[1], St[2], Pred(Length(St)));
    St[1] := '$';
  end else if (Length(St) > 2) and (St[1] = '0') and (Upcase(St[2]) = 'X') then begin
    Move(St[3], St[2], Pred(Length(St)));                              {!!.03}
    SetLength(St, Pred(Length(St)));
{    Move(St[3], St[2], Pred(Length(St)));}
    St[1] := '$';
  end;
  Val(St, I, Code);
  if Code <> 0 then begin
    I := Code;
  end else
    Result := True;
end;

{!!.03 Real to Double for BCB}
function Str2RealL(const S : AnsiString; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailL(S);                                                 {!!.03}
(*                                                                     {!!.03}
  St := S;                                                             {!!.03}
  while St[Length(St)] = ' ' do                                        {!!.03}
    SetLength(St, Pred(Length(St)));                                   {!!.03}
*)                                                                     {!!.03}
  if St = '' then Exit;                                                {!!.03}
  Val(ValPrepL(St), R, Code);                                          {!!.01}
  if Code <> 0 then begin
    R := Code;
  end else
    Result := True;
end;

function Str2ExtL(const S : AnsiString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  P := TrimTrailL(S);
  if P = '' then Exit;                                                 {!!.03}
  Val(ValPrepL(P), R, Code);                                           {!!.01}
  if Code <> 0 then begin
    R := Code - 1;
  end else
    Result := True;
end;

function Long2StrL(L : LongInt) : AnsiString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

{!!.03 Real to Double for BCB}
function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;

function ValPrepL(const S : AnsiString) : AnsiString;                  {!!.01}
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
begin
  Result := TrimSpacesL(S);
  if Result <> '' then begin
    if StrChPosL(Result, DecimalSeparator, P) then begin
      Result[P] := '.';
      if P = Length(Result) then
        SetLength(Result, Pred(P));
    end;
  end else begin
    Result := '0';
  end;
end;

  {-------- General purpose string manipulation --------}

function CharStrL(C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Return a string filled with the specified character.}
begin
  SetLength(Result, Len);                                              {!!.02}
  if Len <> 0 then begin
    {SetLength(Result, Len);}                                          {!!.02}
    FillChar(Result[1], Len, C);
  end;
end;

function PadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else begin
    SetLength(Result, Len);
    Move(S[1], Result[1], Length(S));
    FillChar(Result[Succ(Length(S))], Len-Length(S), C);
  end;
end;

function PadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChL(S, ' ', Len);
end;

function LeftPadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    Move(S[1], Result[Succ(Word(Len))-Length(S)], Length(S));
    FillChar(Result[1], Len-Length(S), C);
  end;
end;

function LeftPadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChL(S, ' ', Len);
end;

function TrimLeadL(const S : AnsiString) : AnsiString;
  {-Return a string with leading white space removed}
var
  I : Cardinal;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  SetLength(Result, Length(S)-Pred(I));
  Move(S[I], Result[1], Length(S)-Pred(I));
end;

function TrimTrailL(const S : AnsiString) : AnsiString;
  {-Return a string with trailing white space removed.}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do    {!!.03}
    SetLength(Result, Pred(Length(Result)));
end;

function TrimL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing white space removed.}
var
  I : Cardinal;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do    {!!.03}
    SetLength(Result, Pred(Length(Result)));

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function TrimSpacesL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing spaces removed.}
var
  I : Cardinal;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    SetLength(Result, Pred(Length(Result)));
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function CenterChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    FillChar(Result[1], Len, C);
    Move(S[1], Result[Succ((Len-Length(S)) shr 1)], Length(S));
  end;
end;

function CenterL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChL(S, ' ', Len);
end;

function EntabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Convert blanks in a string to tabs.}
var
  InLen, OutLen : Cardinal;
begin
  if S = '' then Exit;
  InLen := Length(S);
  OutLen := 0;
  SetLength(Result, InLen);
asm
  push   ebx                   { Save registers }
  push   edi
  push   esi

  mov    edi, [Result]
  mov    edi, [edi]
  xor    ecx, ecx
  add    cl, TabSize
  jz     @@Done

  mov    esi, S
  xor    ebx, ebx              { Zero EBX and EDX }
  xor    edx, edx
  inc    edx                   { Set output length to 1 }

@@Next:
  or     ebx, ebx
  je     @@NoTab               { Jump to NoTab if spacecount is zero }
  mov    eax, edx              { IPos to EAX }
  push   edx
  xor    edx, edx
  div    ecx
  cmp    edx, 1                { Is mod = 1? }
  pop    edx
  jne    @@NoTab               { If not, no tab }

  sub    edi, ebx
  sub    OutLen, ebx
  inc    OutLen
  xor    ebx, ebx              { Reset spacecount }
  mov    byte ptr [edi], 9h    { Store a tab }
  inc    edi

@@NoTab:
  mov    al, [esi]             { Get next input character }
  inc    esi
  cmp    edx, InLen            { End of string? }
  jg     @@Done                { Yes, done }
  inc    ebx                   { Increment SpaceCount }
  cmp    al, 20h               { Is character a space? }
  jz     @@Store               { Yes, store it for now }
  xor    ebx, ebx              { Reset SpaceCount }
  cmp    al, 27h               { Is it a quote? }
  jz     @@Quotes              { Yep, enter quote loop }
  cmp    al, 22h               { Is it a doublequote? }
  jnz    @@Store               { Nope, store it }

@@Quotes:
  mov    ah, al                { Save quote start }

@@NextQ:
  mov    [edi], al             { Store quoted character }
  inc    edi
  inc    OutLen
  mov    al, [esi]             { Get next character }
  inc    esi
  inc    edx                   { Increment Ipos }

  cmp    edx, ecx              { At end of line? }
  jae    @@Store               { If so, exit quote loop }

  cmp    al, ah                { Matching end quote? }
  jnz    @@NextQ               { Nope, stay in quote loop }

  cmp    al, 27h               { Single quote? }
  jz     @@Store               { Exit quote loop }

  cmp    byte ptr [esi-2],'\'  { Previous character an escape? }
  jz     @@NextQ               { Stay in if so }

@@Store:
  mov    [edi], al             { Store last character }
  inc    edi
  inc    OutLen
  inc    edx                   { Increment input position }
  jmp    @@Next                { Repeat while characters left }

@@Done:
  mov    byte ptr [edi], 0h
  pop    esi
  pop    edi
  pop    ebx
end;
  SetLength(Result, OutLen);
end;

function DetabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Expand tabs in a string to blanks.}
var
  NumTabs : Integer;
begin
  if S = '' then Exit;
  if TabSize = 0 then Exit;
  Result := S;
  NumTabs := CharCountL(S, #9);
  if NumTabs = 0 then Exit;
  SetLength(Result, Length(Result)+NumTabs*(Pred(TabSize)));
asm
  push   ebx                { Save registers since we'll be changing them. }
  push   edi
  push   esi

  mov    edi, Result        { EDI => output string. }
  mov    esi, S             { ESI => input string. }
  xor    ebx, ebx
  mov    bl, TabSize
  mov    edi, [edi]
  xor    ecx, ecx           { Default input length = 0. }
  xor    edx, edx           { Zero EDX for output length }
  xor    eax, eax           { Zero EAX }
  mov    ecx, [esi-StrOffset].LStrRec.Length  { Get input length. }
  or     ebx, ebx           { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx           { Return zero length string if TabSize = 0. }

@@DefLength:
  mov    [edi-StrOffset].LStrRec.Length, ecx  { Store default output length. }
  or     ecx, ecx
  jz     @@Done             { Done if empty input string. }

@@Next:
  mov    al, [esi]          { Next input character. }
  inc    esi
  cmp    al, 09h            { Is it a tab? }
  jz     @@Tab              { Yes, compute next tab stop. }
  mov    [edi], al          { No, store to output. }
  inc    edi
  inc    edx                { Increment output length. }
  dec    ecx                { Decrement input length. }
  jnz    @@Next
  jmp    @@StoreLen         { Loop termination. }

@@Tab:
  push   ecx                { Save input length. }
  push   edx                { Save output length. }
  mov    eax, edx           { Get current output length in EDX:EAX. }
  xor    edx, edx
  div    ebx                { Output length MOD TabSize in DX. }
  mov    ecx, ebx           { Calc number of spaces to insert... }
  sub    ecx, edx           {  = TabSize - Mod value. }
  pop    edx
  add    edx, ecx           { Add count of spaces into current output length. }

  mov    eax,$2020          { Blank in AH, Blank in AL. }
  shr    ecx, 1             { Store blanks. }
  rep    stosw
  adc    ecx, ecx
  rep    stosb
  pop    ecx                { Restore input length. }
  dec    ecx
  jnz    @@Next                                                        {!!.01}
  {jmp    @@Next}           { Back for next input. }                   {!!.01}

@@StoreLen:
  xor    ebx, ebx
  mov    [edi], bl          { Store terminating null }
  mov    eax, edx
  sub    edi, eax
  mov    [edi-StrOffset].LStrRec.Length, edx  { Store final length. }

@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;
end;

function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  I, J, LKey, LStr : Cardinal;
begin
  Result := S;
  if Key = '' then Exit;
  if S = '' then Exit;
  LKey := Length(Key);
  LStr := Length(S);
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := Char(Byte(S[I]) xor Byte(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

function SubstituteL(const S, FromStr, ToStr : AnsiString) : AnsiString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  I : Cardinal;
  P : Cardinal;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      {P := System.Pos(S[I], FromStr);}                                {!!.01}
      {if P <> 0 then}                                                 {!!.01}
      if StrChPosL(FromStr, S[I], P) then                              {!!.01}
        Result[I] := ToStr[P];
    end;
end;

function FilterL(const S, Filters : AnsiString) : AnsiString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if not CharExistsL(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  SetLength(Result, Len);
end;

  {--------------- Word / Char manipulation -------------------------}

function CharExistsL(const S : AnsiString; C : AnsiChar) : Boolean; register;
  {-Count the number of a given character in a string. }
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;

function CharCountL(const S : AnsiString; C : AnsiChar) : Cardinal; register;
  {-Count the number of a given character in a string. }
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;

function WordCountL(const S, WordDelims : AnsiString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I    : Cardinal;
  SLen : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsL(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  Count : Cardinal;
  I     : Cardinal;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> N) do begin
    {skip over delimiters}
    while (I <= Length(S)) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= Length(S)) and not CharExistsL(WordDelims, S[I]) do
        Inc(I)
    else begin                                                         {!!.02}
      Pos := I;
      Result := True;
    end;                                                               {!!.02}
  end;
end;

function ExtractWordL(N : Cardinal; const S, WordDelims : AnsiString) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  I, J   : Cardinal;
begin
  Result := '';                                                        {!!.03}
  if WordPositionL(N, S, WordDelims, I) then begin
    {find the end of the current word}
    J := I;
    while (I <= Length(S)) and not CharExistsL(WordDelims, S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], I-J);
  end;
end;

function AsciiCountL(const S, WordDelims : AnsiString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Cardinal;
  InQuote : Boolean;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= Length(S) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote)
      and CharExistsL(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Result);
    {find the end of the current word}
    while (I <= Length(S)) and (InQuote or not CharExistsL(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                        Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  Count, I : Cardinal;
  InQuote  : Boolean;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= Length(S)) and (Count <> N) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote) and CharExistsL(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= Length(S)) and (InQuote or not CharExistsL(WordDelims, S[I])) do begin
        if S[I] = Quote then
          InQuote := not InQuote;
        Inc(I);
      end
    else begin                                                         {!!.02}
      Pos := I;
      Result := True;
    end;                                                               {!!.02}
  end;
end;

function ExtractAsciiL(N : Cardinal; const S, WordDelims : AnsiString;
                       Quote : AnsiChar) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  I, J    : Cardinal;
  InQuote : Boolean;
begin
  InQuote := False;
  if AsciiPositionL(N, S, WordDelims, Quote, I) then begin
    J := I;
    {find the end of the current word}
    while (I <= Length(S)) and ((InQuote)
      or not CharExistsL(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not(InQuote);
      Inc(I);
    end;
    SetLength(Result, I-J);
    Move(S[J], Result[1], I-J);
  end;
end;

procedure WordWrapL(const InSt : AnsiString; var OutSt, Overlap : AnsiString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  InStLen  : Cardinal;
  EOS, BOS : Cardinal;
begin
  InStLen := Length(InSt);
  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (InSt[EOS] = ' ') and (EOS > 0) do
      Dec(EOS);

    if EOS > Margin then begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (InSt[EOS] = ' ') and (EOS > 0) do
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  SetLength(OutSt, EOS);
  Move(InSt[1], OutSt[1], Length(OutSt));

  {find the start of the next word in the line}
  BOS := Succ(EOS);
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    SetLength(OverLap, 0)
  else begin
    {copy from the start of the next word to the end of the line}
    SetLength(OverLap, Succ(InStLen-BOS));
    Move(InSt[BOS], Overlap[1], Length(OverLap));
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (Length(OutSt) < Margin) then begin
    SetLength(OutSt, Margin);
    FillChar(OutSt[Succ(Length(OutSt))], Margin-Length(OutSt), ' ');
  end;
end;

  {--------------- String comparison and searching -----------------}
function CompStringL(const S1, S2 : AnsiString) : Integer; register;
  {-Compare two strings.}
asm
  push   edi
  mov    edi, edx           { EDI points to S2 }
  push   esi
  mov    esi, eax           { ESI points to S1 }

  xor    edx, edx
  xor    ecx, ecx

  or     edi, edi
  jz     @@1
  mov    edx, [edi-StrOffset].LStrRec.Length

@@1:
  or     esi, esi
  jz     @@2
  mov    ecx, [esi-StrOffset].LStrRec.Length

@@2:
  or     eax, -1            { EAX holds temporary result }

  cmp    ecx, edx           { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    eax                { S1 longer than S2 }
  mov    ecx, edx           { Length(S2) in CL }

@@EqLen:
  inc    eax                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if either is empty }

  repe   cmpsb              { Compare until no match or ECX = 0 }
  je     @@Done             { If Equal, result ready based on length }

  mov    eax, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     eax, -1            { Else S1 Less, Return -1 }

@@Done:
  pop    esi
  pop    edi
end;

function CompUCStringL(const S1, S2 : AnsiString) : Integer; register;
  {-Compare two strings. This compare is not case sensitive.}
asm
  push   ebx                { Save registers }
  push   edi
  push   esi

  mov    edi, edx           { EDI points to S2 }
  mov    esi, eax           { ESI points to S1 }

  xor    eax, eax
  xor    ecx, ecx
  xor    edx, edx           { DL chars from S2 }
  or     ebx, -1

  or     edi, edi
  jz     @@1
  mov    eax, [edi-StrOffset].LStrRec.Length

@@1:
  or     esi, esi
  jz     @@2
  mov    ecx, [esi-StrOffset].LStrRec.Length

@@2:
  cmp    ecx, eax           { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    ebx                { S1 longer than S2 }
  mov    ecx, eax           { Shorter length in ECX }

@@EqLen:
  inc    ebx                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if lesser string is empty }

@@Start:
  xor    eax, eax           { EAX holds chars from S1 }
  mov    al, [esi]          { S1[?] into AL }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  mov    dl, [edi]          { S2[?] into DL }
  inc    edi                { Point EDI to next char in S2 }
  mov    dh, al
  mov    al, dl
  mov    dl, dh

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  cmp    dl, al             { Compare until no match }
  jne    @@Output
  dec    ecx
  jnz    @@Start

  je     @@Done             { If Equal, result ready based on length }

@@Output:
  mov    ebx, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     ebx, -1            { Else S1 Less, Return -1 }

@@Done:
  mov    eax, ebx           { Result into EAX }
  pop    esi                { Restore Registers }
  pop    edi
  pop    ebx
end;

function SoundexL(const S : AnsiString) : AnsiString;
  {-Return 4 character soundex of an input string}
const
  SoundexTable : array[0..255] of Char =
    (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0,
    { A   B    C    D    E   F    G    H   I   J    K    L    M  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { N    O   P    Q    R    S    T    U   V    W   X    Y   X  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0,
    { a   b    c    d    e   f    g    h   i   j    k    l    m  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { n    o   p    q    r    s    t    u   v    w   x    y   x  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0);
begin
  if S = '' then Exit;
  SetLength(Result, 4);
asm
  push  edi
  mov   edi, [Result]            { EDI => output string. }
  mov   edi, [edi]
  push  ebx
  push  esi

  mov   esi, S                   { ESI => input string. }
  mov   dword ptr [edi], '0000'  { Initialize output string to '0000'. }
  xor   eax, eax
  mov   [edi+4], al              { Set null at end of string. }

  mov   ecx, [esi-StrOffset].LStrRec.Length
  or    ecx, ecx                 { Exit if null string. }
  jz    @@Done

  mov   al, [esi]                { Get first character of input string. }
  inc   esi

  push  ecx                      { Save ECX across call to CharUpper. }
  push  eax                      { Push Char onto stack for CharUpper. }
  call  CharUpper                { Uppercase AL. }
  pop   ecx                      { Restore saved register. }

  mov   [edi], al                { Store first output character. }
  inc   edi

  dec   ecx                      { One input character used. }
  jz    @@Done                   { Was input string one char long?. }

  mov   bh, 03h                  { Output max 3 chars beyond first. }
  mov   edx, offset SoundexTable { EDX => Soundex table. }
  xor   eax, eax                 { Prepare for address calc. }
  xor   bl, bl                   { BL will be used to store 'previous char'. }

@@Next:
  mov   al, [esi]                { Get next char in AL. }
  inc   esi
  mov   al, [edx+eax]            { Get soundex code into AL. }
  or    al, al                   { Is AL zero? }
  jz    @@NoStore                { If yes, skip this char. }
  cmp   bl, al                   { Is it the same as the previous stored char? }
  je    @@NoStore                { If yes, skip this char. }
  mov   [edi], al                { Store char to Dest. }
  inc   edi
  dec   bh                       { Decrement output counter. }
  jz    @@Done                   { If zero, we're done. }
  mov   bl, al                   { New previous character. }

@@NoStore:
  dec   ecx                      { Decrement input counter. }
  jnz   @@Next

@@Done:
  pop   esi
  pop   ebx
  pop   edi
end;
end;

function MakeLetterSetL(const S : AnsiString) : LongInt; register;
  {-Return a bit-mapped long storing the individual letters contained in S.}
asm
  push   ebx                { Save registers }
  push   esi

  mov    esi, eax           { ESI => string }
  xor    ecx, ecx           { Zero ECX }
  xor    edx, edx           { Zero EDX }
  {or     edx, edx}                                                    {!!.01}
  or     eax, eax                                                      {!!.01}
  jz     @@Exit
  xor    eax, eax           { Zero EAX }
  add    ecx, [esi-StrOffset].LStrRec.Length
  jz     @@Exit             { Done if ECX is 0 }

@@Next:
  mov    al, [esi]          { EAX has next char in S }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  sub    eax, 'A'           { Convert to bit number }
  cmp    eax, 'Z'-'A'       { Was char in range 'A'..'Z'? }
  ja     @@Skip             { Skip it if not }

  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx
  ror    edx, cl
  or     edx, 01h               { Set appropriate bit }
  rol    edx, cl
  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx

@@Skip:
  dec    ecx
  jnz    @@Next             { Get next character }

@@Exit:
  mov    eax, edx           { Move EDX to result }
  pop    esi                { Restore registers }
  pop    ebx
end;

procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable); register;
  {-Build a Boyer-Moore link table}
asm
  push  edi                { Save registers because they will be changed }
  push  esi
  mov   esi, eax           { Move EAX to ESI }
  push  ebx

  or    eax, eax
  jz    @@MTDone

  xor   eax, eax           { Zero EAX }
  mov   ecx, [esi-StrOffset].LStrRec.Length
  cmp   ecx, 0FFh          { If ECX > 255, force to 255 }
  jbe   @@1
  mov   ecx, 0FFh

@@1:
  mov   ch, cl             { Duplicate CL in CH }
  mov   eax, ecx           { Fill each byte in EAX with length }
  shl   eax, 16
  mov   ax, cx
  mov   edi, edx           { Point to the table }
  mov   ecx, 64            { Fill table bytes with length }
  rep   stosd
  cmp   al, 1              { If length <= 1, we're done }
  jbe   @@MTDone
  mov   edi, edx           { Reset EDI to beginning of table }
  xor   ebx, ebx           { Zero EBX }
  mov   cl, al             { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   al, [esi]          { Load table with positions of letters }
  mov   bl, al             { that exist in the search string }
  inc   esi
  mov   [edi+ebx], cl
  dec   cl
  jnz   @@MTNext

@@MTDone:
  pop   ebx                { Restore registers }
  pop   esi
  pop   edi
end;

function BMSearchL(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean; register;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  or    esi, esi
  jz    @@BMSNotFound
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  xor   eax, eax            { Zero EAX }

  mov   edx, [esi-StrOffset].LStrRec.Length
  cmp   edx, 0FFh          { If EDX > 255, force to 255 }
  jbe   @@1
  mov   edx, 0FFh

@@1:
  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  mov   esi, Pos            { Set position in Pos }
  dec   edi                 { Found, calculate position }
  sub   edi, ebx
  mov   eax, 1              { Set result to True }
  mov   [esi], edi
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  repe  cmpsb               { Compare MatchString to buffer }
  je    @@BMSFound          { If equal, string is found }

  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean; register;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  or    esi, esi
  jz    @@BMSNotFound
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  xor   eax, eax            { Zero EAX }

  mov   edx, [esi-StrOffset].LStrRec.Length
  cmp   edx, 0FFh           { If EDX > 255, force to 255 }
  jbe   @@1
  mov   edx, 0FFh

@@1:
  cmp   dl, 1               { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  push  eax                 { Push Char onto stack for CharUpper }
  call  CharUpper
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  jecxz @@BMSFound          { If ECX is zero, string is found }

@@StringComp:
  xor   eax, eax
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  call  CharUpper
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  mov   al, [esi]           { Get MatchString char }
  dec   esi
  cmp   ah, al              { Compare }
  loope @@StringComp        { OK?  Get next character }
  je    @@BMSFound          { Matched! }

  xor   ah, ah              { Zero AH }
  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := System.Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';                                                        {!!.02}
  if PathName = '' then Exit;                                          {!!.02}
  I := Succ(Word(Length(PathName)));
  repeat
    Dec(I);
  until (PathName[I] in DosDelimSet) or (I = 0);
  Result := System.Copy(PathName, Succ(I), MaxFileLen);
end;

function JustNameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
  S      : AnsiString;
begin
  S := JustFileNameL(PathName);
  if HasExtensionL(S, DotPos) then
    S := System.Copy(S, 1, DotPos-1);
  Result := S;
end;

function JustExtensionL(const Name : AnsiString) : AnsiString;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := System.Copy(Name, Succ(DotPos), MaxFileLen)
  else
    Result := '';
end;

function JustPathnameL(const PathName : AnsiString) : AnsiString;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  if PathName = '' then Exit;                                          {!!.01}

  I := Succ(Word(Length(PathName)));
  repeat
    Dec(I);
  until (PathName[I] in DosDelimSet) or (I = 0);

  if I = 0 then
    {Had no drive or directory name}
    SetLength(Result, 0)
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := System.Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := System.Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := System.Copy(PathName, 1, I);
end;

function AddBackSlashL(const DirName : AnsiString) : AnsiString;
  {-Add a default backslash to a directory name}
begin
  if Length(DirName) = 0 then Exit;
  if DirName[Length(DirName)] in DosDelimSet then
    Result := DirName
  else
    Result := DirName + '\';
end;

function CleanFileNameL(const FileName : AnsiString) : AnsiString;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos : Cardinal;
  NameLen : Word;
begin
  if HasExtensionL(FileName, DotPos) then begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := Pred(DotPos);
    if NameLen > 8 then
      NameLen := 8;
    Result := System.Copy(FileName, 1, NameLen)+System.Copy(FileName, DotPos, 4);
  end else
    {Take the first 8 chars of name}
    Result := System.Copy(FileName, 1, 8);
end;

function CleanPathNameL(const PathName : AnsiString) : AnsiString;
  {-Return a pathname cleaned up as DOS does it.}
var
  I : Cardinal;
  S : AnsiString;
begin
  SetLength(Result, 0);
  S := PathName;

  I := Succ(Word(Length(S)));
  repeat
    dec(I);
    if I > 2 then
      if (S[I] = '\') and (S[I-1] = '\') then
        if (S[I-2] <> ':') then
          System.Delete(S, I, 1);
  until I <= 0;

  I := Succ(Word(Length(S)));
  repeat
    {Get the next directory or drive portion of pathname}
    repeat
      Dec(I);
    until (S[I] in DosDelimSet) or (I = 0);

    {Clean it up and prepend it to output string}
    Result := CleanFileNameL(System.Copy(S, Succ(I), MaxFileLen)) + Result;
    if I > 0 then begin
      Result := S[I] + Result;
      System.Delete(S, I, 255);
    end;
  until I <= 0;

end;

function HasExtensionL(const Name : AnsiString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
var
  I : Cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    {and (Pos('\', System.Copy(Name, Succ(DotPos), MaxFileLen)) = 0);} {!!.01}
    and not CharExistsL(System.Copy(Name, Succ(DotPos), MaxFileLen), '\'); {!!.03}
end;

  {------------------ Formatting routines --------------------}


function CommaizeChL(L : LongInt; Ch : AnsiChar) : AnsiString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp : string;
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrL(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeL(L : LongInt) : AnsiString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChL(L, ',');
end;

function FormPrimL(const Mask : AnsiString; R : TstFloat; const LtCurr, RtCurr : AnsiString;
                  Sep, DecPt : AnsiChar; AssumeDP : Boolean) : AnsiString;
  {-Returns a formatted string with digits from R merged into the Mask}
type
  FillType = (Blank, Asterisk, Zero);
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars : string[8] = '#@*$-+,.';
  PlusArray : array[Boolean] of Char = ('+', '-');
  MinusArray : array[Boolean] of Char = (' ', '-');
  FillArray : array[FillType] of Char = (' ', '*', '0');
var
  S : string;              {temporary string}
  Filler : FillType;       {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative : Boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF : Cardinal;         {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Places,                  {# of digits after the '.'}
  Blanks,                  {# of blanks returned by Str}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr : Byte;         {pointer into temporary string of digits}
  I : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if AssumeDP and (Result <> '') then begin
    SetLength(Result, Succ(Length(Result)));
    Result[Length(Result)] := '.';
  end;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Result) = 0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 1;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  for I := Length(Result) downto 1 do begin
    if Result[I] = 'C' then begin
      Inc(RtChars);
      System.Delete(Result, I, 1);
    end else if Result[I] = 'c' then begin
      Inc(LtChars);
      System.Delete(Result, I, 1);
    end;
  end;

  {find the starting point for the field}
  while (StartF <= Length(Result))
    {and (System.Pos(Result[StartF], FormChars) = 0) do}               {!!.01}
    and not CharExistsL(FormChars, Result[StartF]) do                  {!!.01}
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do begin
    case Result[EndF] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := EndF;
    else
      goto EndFound;
    end;
    Inc(EndF);
  end;

  {if we get here at all, the last char was part of the field}
  Inc(EndF);

EndFound:
  {if we jumped to here instead, it wasn't}
  Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$' :
        begin
          Inc(Digits);
          if (I > DotPos) and (DotPos <> 0) then
            Inc(Places);
        end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Str(R:Digits:Places, S);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    I := Length(S);
    SetLength(S, I+ (Places-MaxPlaces));
    FillChar(S[Succ(I)], Places-MaxPlaces, '0');
    while (Length(S) > Digits) and (S[1] = ' ') do
      System.Delete(S, 1, 1);
  end;

  {count number of initial blanks}
  Blanks := 1;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;
  Dec(Blanks);

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (Length(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$' : Result[I] := '*';
        '+' : Result[I] := PlusArray[Negative];
        '-' : Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then begin
    FillChar(S[1], Blanks, FillArray[Filler]);

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Blanks] := LtCurr[1];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Blanks] := '-';
  end;

  {put in the digits / signs}
  DigitPtr := Length(S);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if DigitPtr <> 0 then begin
          Result[I] := S[DigitPtr];
          Dec(DigitPtr);
          if (S[DigitPtr] = '.') and (DigitPtr <> 0) then
            Dec(DigitPtr);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '+' : Result[I] := PlusArray[Negative];
      '-' : Result[I] := MinusArray[Negative];
    end;
  end;

Done:
  if AssumeDP then
    SetLength(Result, Pred(Length(Result)));
  if RtChars > 0 then begin
    S := RtCurr;
    if Length(S) > RtChars then
      SetLength(S, RtChars)
    else
      S := LeftPadL(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Length(S) > LtChars then
      SetLength(S, LtChars)
    else
      S := PadL(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormL(const Mask : AnsiString ; R : TstFloat ; const LtCurr,
                    RtCurr : AnsiString ; Sep, DecPt : AnsiChar) : AnsiString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimL(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormL(const Mask : AnsiString ; L : LongInt ; const LtCurr,
                      RtCurr : AnsiString ; Sep : AnsiChar) : AnsiString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimL(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosL(const P : AnsiString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
(*var
  Temp : string;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Pos := System.Pos(Temp, P);
  Result := Pos <> 0;*)
asm                                                            {!!.01 Revised}
  push  ebx             { Save registers }
  push  edi

  or    eax, eax        { Protect against null string }
  jz    @@NotFound

  xor   edi, edi        { Zero counter }
  mov   ebx, [eax-StrOffset].LStrRec.Length  { Get input length }

@@Loop:
  inc   edi             { Increment counter }
  cmp   [eax], dl       { Did we find it? }
  jz    @@Found
  inc   eax             { Increment pointer }

  cmp   edi, ebx        { End of string? }
  jnz   @@Loop          { If not, loop }

@@NotFound:
  xor   eax, eax        { Not found, zero EAX for False }
  mov   [ecx], eax
  jmp   @@Done

@@Found:
  mov   [ecx], edi      { Set Pos }
  mov   eax, 1          { Set EAX to True }

@@Done:
  pop   edi             { Restore registers }
  pop   ebx
end;

function StrStPosL(const P, S : AnsiString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertL(const S : AnsiString; C : AnsiChar; Pos : Cardinal) : AnsiString;
  {-Insert a character into a string at a specified position.}
var
  Temp : string;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertL(const S1, S2 : AnsiString; Pos : Cardinal) : AnsiString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteL(const S : AnsiString; Pos : Cardinal) : AnsiString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;

{$IFDEF SYSDEMO}
initialization
  _CC_; _VC_;
{$ENDIF}

end.

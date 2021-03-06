{$I STDEFINE.INC}

{$A+} {Aligned records}
{$B-} {Incomplete boolean evaluation}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF OS32}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   STSTRS.PAS 1.05                     *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit STStrS;
  {-Short string routines}

interface

uses
 {$IFDEF OS32}
  {$IFDEF WIN32}
  Windows,
  {$ELSE}
  Use32,
  {$ENDIF}
 {$ELSE}
  WinTypes,
  WinProcs,
 {$ENDIF}
  SysUtils,
  STConst, STBase;

  {-------- Numeric conversion -----------}

function HexBS(B : Byte) : ShortString;
  {-Return the hex string for a byte.}

function HexWS(W : Word) : ShortString;
  {-Return the hex string for a word.}

function HexLS(L : LongInt) : ShortString;
  {-Return the hex string for a long integer.}

function HexPtrS(P : Pointer) : ShortString;
  {-Return the hex string for a pointer.}

function BinaryBS(B : Byte) : ShortString;
  {-Return a binary string for a byte.}

function BinaryWS(W : Word) : ShortString;
  {-Return the binary string for a word.}

function BinaryLS(L : LongInt) : ShortString;
  {-Return the binary string for a long integer.}

function OctalBS(B : Byte) : ShortString;
  {-Return an octal string for a byte.}

function OctalWS(W : Word) : ShortString;
  {-Return an octal string for a word.}

function OctalLS(L : LongInt) : ShortString;
  {-Return an octal string for a long integer.}

function Str2Int16S(const S : ShortString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

function Str2WordS(const S : ShortString; var I : Word) : Boolean;
  {-Convert a string to a word.}

function Str2LongS(const S : ShortString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

{!!.03 Real to Double for BCB}
function Str2RealS(const S : ShortString; var R : Double) : Boolean;
  {-Convert a string to a real.}

function Str2ExtS(const S : ShortString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}

function Long2StrS(L : LongInt) : ShortString;
  {-Convert an integer type to a string.}

{!!.03 Real to Double for BCB}
function Real2StrS(R : Double; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert a real to a string.}

function Ext2StrS(R : Extended; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert an extended to a string.}

function ValPrepS(const S : ShortString) : ShortString;                {!!.01}
  {-Prepares a string for calling Val.}


  {-------- General purpose string manipulation --------}

function CharStrS(C : AnsiChar; Len : Cardinal) : ShortString;
  {-Return a string filled with the specified character.}

function PadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the right with a specified character.}

function PadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the right with spaces.}

function LeftPadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left with a specified character.}

function LeftPadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left with spaces.}

function TrimLeadS(const S : ShortString) : ShortString;
  {-Return a string with leading white space removed.}

function TrimTrailS(const S : ShortString) : ShortString;
  {-Return a string with trailing white space removed.}

function TrimS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing spaces removed.}

function CenterChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with a specified character.}

function CenterS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with spaces.}

function EntabS(const S : ShortString; TabSize : Byte) : ShortString;
  {-Convert blanks in a string to tabs.}

function DetabS(const S : ShortString; TabSize : Byte) : ShortString;
  {-Expand tabs in a string to blanks.}

function ScrambleS(const S, Key : ShortString) : ShortString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteS(const S, FromStr, ToStr : ShortString) : ShortString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterS(const S, Filters : ShortString) : ShortString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}

  {--------------- Word / Char manipulation -------------------------}

function CharExistsS(const S : ShortString; C : AnsiChar) : Boolean;
  {-Determines whether a given character exists in a string. }

function CharCountS(const S : ShortString; C : AnsiChar) : Byte;
  {-Count the number of a given character in a string. }

function WordCountS(const S, WordDelims : ShortString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}

function WordPositionS(N : Cardinal; const S, WordDelims : ShortString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordS(N : Cardinal; const S, WordDelims : ShortString) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountS(const S, WordDelims : ShortString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}

function AsciiPositionS(N : Cardinal; const S, WordDelims : ShortString;
                        Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}

function ExtractAsciiS(N : Cardinal; const S, WordDelims : ShortString;
                       Quote : AnsiChar) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapS(const InSt : ShortString; var OutSt, Overlap : ShortString;
                    Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

  {--------------- String comparison and searching -----------------}
function CompStringS(const S1, S2 : ShortString) : Integer;
  {-Compare two strings.}

function CompUCStringS(const S1, S2 : ShortString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}

function SoundexS(const S : ShortString) : ShortString;
  {-Return 4 character soundex of an input string.}

function MakeLetterSetS(const S : ShortString) : LongInt;
  {-Return a bit-mapped long storing the individual letters contained in S.}

procedure BMMakeTableS(const MatchString : ShortString; var BT : BTable);
  {-Build a Boyer-Moore link table}

function BMSearchS(var Buffer; BufLength : Cardinal; var BT : BTable;
                  const MatchString : ShortString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string.}

function BMSearchUCS(var Buffer; BufLength : Cardinal; var BT : BTable;
                    const MatchString : ShortString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Return a file name with a default extension attached.}

function ForceExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Force the specified extension onto the file name.}

function JustFilenameS(const PathName : ShortString) : ShortString;
  {-Return just the filename and extension of a pathname.}

function JustNameS(const PathName : ShortString) : ShortString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionS(const Name : ShortString) : ShortString;
  {-Return just the extension of a pathname.}

function JustPathnameS(const PathName : ShortString) : ShortString;
  {-Return just the drive and directory portion of a pathname.}

function AddBackSlashS(const DirName : ShortString) : ShortString;
  {-Add a default backslash to a directory name.}

function CleanPathNameS(const PathName : ShortString) : ShortString;
  {-Return a pathname cleaned up as DOS does it.}

function HasExtensionS(const Name : ShortString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

  {------------------ Formatting routines --------------------}

function CommaizeS(L : LongInt) : ShortString;
  {-Convert a long integer to a string with commas.}

function CommaizeChS(L : LongInt; Ch : AnsiChar) : ShortString;
  {-Convert a long integer to a string with Ch in comma positions.}

function FloatFormS(const Mask : ShortString ; R : TstFloat ; const LtCurr,
                    RtCurr : ShortString ; Sep, DecPt : AnsiChar) : ShortString;
  {-Return a formatted string with digits from R merged into mask.}

function LongIntFormS(const Mask : ShortString ; L : LongInt ; const LtCurr,
                     RtCurr : ShortString ; Sep : AnsiChar) : ShortString;
  {-Return a formatted string with digits from L merged into mask.}

function StrChPosS(const P : ShortString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}

function StrStPosS(const P, S : ShortString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Copy characters at a specified position in a string.}

function StrChInsertS(const S : ShortString; C : AnsiChar; Pos : Cardinal) : ShortString;
  {-Insert a character into a string at a specified position.}

function StrStInsertS(const S1, S2 : ShortString; Pos : Cardinal) : ShortString;
  {-Insert a string into another string at a specified position.}

function StrChDeleteS(const S : ShortString; Pos : Cardinal) : ShortString;
  {-Delete the character at a specified position in a string.}

function StrStDeleteS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Delete characters at a specified position in a string.}

implementation

{$IFDEF SYSDEMO} uses SysUtil; {$ENDIF}

{$IFDEF VirtualPascal}
{&frame-} {&uses none}
function HiWord(A: Longint): SmallWord; inline;
begin
  Result := A shr 16;
end;

function LoWord(A: Longint): SmallWord; inline;
begin
  Result := A and $FFFF;
end;
{$ENDIF}

  {-------- Numeric conversion -----------}

function HexBS(B : Byte) : ShortString;
  {-Return the hex string for a byte.}
begin
  Result[0] := #2;
  Result[1] := Digits[B shr 4];
  Result[2] := Digits[B and $F];
end;

function HexWS(W : Word) : ShortString;
  {-Return the hex string for a word.}
begin
  Result[0] := #4;
  Result[1] := Digits[hi(W) shr 4];
  Result[2] := Digits[hi(W) and $F];
  Result[3] := Digits[lo(W) shr 4];
  Result[4] := Digits[lo(W) and $F];
end;

function HexLS(L : LongInt) : ShortString;
  {-Return the hex string for a long integer.}
begin
  Result := HexWS(HiWord(L)) + HexWS(LoWord(L));
end;

function HexPtrS(P : Pointer) : ShortString;
  {-Return the hex string for a pointer.}
type
  OS = record O, S : word; end;
begin
{$IFDEF OS32}
  Result := ':' + HexLS(LongInt(P));
{$ELSE}
  Result := HexWS(OS(P).S)+':' + HexWS(OS(P).O);
{$ENDIF}
end;

function BinaryBS(B : Byte) : ShortString;
  {-Return a binary string for a byte.}
var
  I, N : Cardinal;
begin
  N := 1;
  Result[0] := #8;
  for I := 7 downto 0 do begin
    Result[N] := Digits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWS(W : Word) : ShortString;
  {-Return the binary string for a word.}
var
  I, N : Cardinal;
begin
  N := 1;
  Result[0] := #16;
  for I := 15 downto 0 do begin
    Result[N] := Digits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLS(L : LongInt) : ShortString;
  {-Return the binary string for a long integer.}
var
  I : LongInt;
  N : Byte;
begin
  N := 1;
  Result[0] := #32;
  for I := 31 downto 0 do begin
    Result[N] := Digits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBS(B : Byte) : ShortString;
  {-Return an octal string for a byte.}
var
  I : Cardinal;
begin
  Result[0] := #3;
  for I := 0 to 2 do begin
    Result[3-I] := Digits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWS(W : Word) : ShortString;
  {-Return an octal string for a word.}
var
  I : Cardinal;
begin
  Result[0] := #6;
  for I := 0 to 5 do begin
    Result[6-I] := Digits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLS(L : LongInt) : ShortString;
  {-Return an octal string for a long integer.}
var
  I : Cardinal;
begin
  Result[0] := #12;
  for I := 0 to 11 do begin
    Result[12-I] := Digits[L and 7];
    L := L shr 3;
  end;
end;

function Str2Int16S(const S : ShortString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}
var
  Code : Integer;
  St   : ShortString;
  SLen : Byte absolute St;
begin
  St := S;
  while St[SLen] = ' ' do
    Dec(SLen);
  if (SLen > 1) and (Upcase(St[SLen]) = 'H') then begin
    Move(St[1], St[2], SLen-1);
    St[1] := '$';
  end else if (SLen > 2) and (St[1] = '0') and (Upcase(St[2]) = 'X') then begin
    Dec(SLen);
    Move(St[3], St[2], SLen-1);
    St[1] := '$';
  end;
  Val(St, I, Code);
  if Code <> 0 then begin
    I := Code;
    Result := False;
  end else
    Result := True;
end;

function Str2WordS(const S : ShortString; var I : Word) : Boolean;
  {-Convert a string to a word.}
var
  Code : Integer;
  St   : ShortString;
  SLen : Byte absolute St;
begin
  St := S;
  while St[SLen] = ' ' do
    Dec(SLen);
  if (SLen > 1) and (Upcase(St[SLen]) = 'H') then begin
    Move(St[1], St[2], SLen-1);
    St[1] := '$';
  end
  else if (SLen > 2) and (St[1] = '0') and (Upcase(St[2]) = 'X') then begin
    Dec(SLen);
    Move(St[3], St[2], SLen-1);
    St[1] := '$';
  end;
  Val(St, I, Code);
  if Code <> 0 then begin
    I := Code;
    Result := False;
  end else
    Result := True;
end;

function Str2LongS(const S : ShortString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}
var
  Code : Integer;
  St   : ShortString;
  SLen : Byte absolute St;
begin
  St := S;
  while St[SLen] = ' ' do
    Dec(SLen);
  if (SLen > 1) and (Upcase(St[SLen]) = 'H') then begin
    Move(St[1], St[2], SLen-1);
    St[1] := '$';
  end else if (SLen > 2) and (St[1] = '0') and (Upcase(St[2]) = 'X') then begin
    Dec(SLen);
    Move(St[3], St[2], SLen-1);
    St[1] := '$';
  end;
  Val(St, I, Code);
  if Code <> 0 then begin
    I := Code;
    Result := False;
  end else
    Result := True;
end;

{!!.03 Real to Double for BCB}
function Str2RealS(const S : ShortString; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : ShortString;
  SLen : Byte absolute St;
begin
  St := S;
  {trim trailing blanks}
  while St[SLen] = ' ' do
    Dec(SLen);
  Val(ValPrepS(St), R, Code);
  if Code <> 0 then begin
    R := Code;
    Result := False;
  end else
    Result := True;
end;

function Str2ExtS(const S : ShortString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P : ShortString;
  PLen : Byte absolute P;
begin
  P := S;
  {trim trailing blanks}
  while P[PLen] = ' ' do
    Dec(PLen);
  Val(ValPrepS(P), R, Code);
  if Code <> 0 then begin
    R := Code;
    Result := False;
  end else
    Result := True;
end;

function Long2StrS(L : LongInt) : ShortString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

{!!.03 Real to Double for BCB}
function Real2StrS(R : Double; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrS(R : Extended; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;

function ValPrepS(const S : ShortString) : ShortString;                {!!.01}
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
begin
  Result := TrimSpacesS(S);
  if Result <> '' then begin
    if StrChPosS(Result, DecimalSeparator, P) then begin
      Result[P] := '.';
      if P = Byte(Result[0]) then
        Result[0] := AnsiChar(Pred(P));
    end;
  end else begin
    Result := '0';
  end;
end;

  {-------- General purpose string manipulation --------}

function CharStrS(C : AnsiChar; Len : Cardinal) : ShortString;
  {-Return a string filled with the specified character.}
begin
  if Len = 0 then
    Result[0] := #0
  else begin
    Result[0] := Chr(Len);
    FillChar(Result[1], Len, C);
  end;
end;

function PadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the right with a specified character.}
var
  SLen : Byte absolute S;
begin
  if Length(S) >= Len then
    Result := S
  else begin
    if Len > 255 then Len := 255;
    Result[0] := Chr(Len);
    Move(S[1], Result[1], SLen);
    if SLen < 255 then
      FillChar(Result[Succ(SLen)], Len-SLen, C);
  end;
end;

function PadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChS(S, ' ', Len);
end;

function LeftPadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < 255 then begin
    if Len > 255 then Len := 255;
    Result[0] := Chr(Len);
    Move(S[1], Result[Succ(Word(Len))-Length(S)], Length(S));
    FillChar(Result[1], Len-Length(S), C);
  end;
end;

function LeftPadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChS(S, ' ', Len);
end;

function TrimLeadS(const S : ShortString) : ShortString;
  {-Return a string with leading white space removed}
var
  I : Cardinal;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  Move(S[I], Result[1], Length(S)-I+1);
  Result[0] := Char(Length(S)-I+1);
end;

function TrimTrailS(const S : ShortString) : ShortString;
  {-Return a string with trailing white space removed.}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do    {!!.03}
    Dec(Result[0]);
end;

function TrimS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing white space removed.}
var
  I    : Cardinal;
  SLen : Byte absolute Result;
begin
  Result := S;
  while (SLen > 0) and (Result[SLen] <= ' ') do
    Dec(SLen);

  I := 1;
  while (I <= SLen) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function TrimSpacesS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing spaces removed.}
var
  I    : Word;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    Dec(Result[0]);
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function CenterChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < 255 then begin
    if Len > 255 then Len := 255;
    Result[0] := Chr(Len);
    FillChar(Result[1], Len, C);
    Move(S[1], Result[Succ((Len-Length(S)) shr 1)], Length(S));
  end;
end;

function CenterS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChS(S, ' ', Len);
end;

{$IFDEF OS32}
{&Frame-} {&Uses ebx,esi,edi}
function EntabS(const S : ShortString; TabSize : Byte) : ShortString;
  {-Convert blanks in a string to tabs.}
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    esi,s
  xor    edx,edx
  mov    dl,TabSize
  mov    edi,@Result
{$ELSE}
  push   ebx                 { Save registers }
  push   edi
  push   esi

  mov    esi, eax            { ESI => input string }
  mov    edi, ecx            { EDI => output string }
{$ENDIF}
  xor    ebx, ebx            { Initial SpaceCount = 0 }
  xor    ecx, ecx            { Default input length = 0 }
  and    edx, 0FFh           { Default output length = 0 in DH, TabSize in DL }

  mov    cl, [esi]           { Get input length }
  inc    esi
  or     edx, edx            { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx            { Return zero length string if TabSize = 0 }

@@DefLength:
  mov    [edi], cl           { Store default output length }
  inc    edi
  or     ecx, ecx
  jz     @@Done              { Done if empty input string }
  inc    ch                  { Current input position=1 }

@@Next:
  or     ebx, ebx            { Compare SpaceCount to 0 }
  jz     @@NoTab             { If SpaceCount=0 then no tab insert here }
  xor    eax, eax
  mov    al, ch              { Ipos to AL }
  div    dl                  { Ipos DIV TabSize }
  cmp    ah, 1               { Ipos MOD TabSize = 1 ? }
  jnz    @@NoTab             { If not, no tab insert here }
  sub    edi, ebx            { Remove unused characters from output string }
  sub    dh, bl              { Reduce Olen by SpaceCount }
  inc    dh                  { Add one to output length }
  xor    ebx, ebx            { Reset SpaceCount }
  mov    byte ptr [edi], 09h { Store a tab }
  inc    edi

@@NoTab:
  mov    al, [esi]           { Get next input character }
  inc    esi
  cmp    cl, ch              { End of string? }
  jz     @@Store             { Yes, store character anyway }
  inc    bl                  { Increment SpaceCount }
  cmp    al, 32              { Is character a space? }
  jz     @@Store             { Yes, store it for now }
  xor    ebx, ebx            { Reset SpaceCount }
  cmp    al, 39              { Is it a quote? }
  jz     @@Quotes            { Yep, enter quote loop }
  cmp    al, 34              { Is it a doublequote? }
  jnz    @@Store             { Nope, store it }

@@Quotes:
  mov    ah, al              { Save quote start }

@@NextQ:
  mov    [edi], al           { Store quoted character }
  inc    edi
  inc    dh                  { Increment output length }
  mov    al, [esi]           { Get next character }
  inc    esi
  inc    ch                  { Increment Ipos }
  cmp    ch, cl              { At end of line? }
  jae    @@Store             { If so, exit quote loop }
  cmp    al, ah              { Matching end quote? }
  jnz    @@NextQ             { Nope, stay in quote loop }
  cmp    al, 39              { Single quote? }
  jz     @@Store             { Exit quote loop }
  cmp    byte ptr [esi-2],'\'{ Previous character an escape? }
  jz     @@NextQ             { Stay in if so }

@@Store:
  mov    [edi], al           { Store last character }
  inc    edi
  inc    dh                  { Increment output length }
  inc    ch                  { Increment input position }
  jz     @@StoreLen          { Exit if past 255 }
  cmp    ch, cl              { Compare Ipos to Ilen }
  jbe    @@Next              { Repeat while characters left }

@@StoreLen:
  xor    eax, eax
  mov    al, dh
  sub    edi, eax
  dec    edi
  mov    [edi], dh           { Store final length }

@@Done:
{$IFNDEF VirtualPascal}
  pop    esi
  pop    edi
  pop    ebx
{$ENDIF}
end;
{$ELSE}
function EntabS(const S : ShortString; TabSize : Byte) : ShortString; assembler;
  {-Convert blanks in a string to tabs.}
asm
  push   ds
  cld

  xor    bx, bx              { Initial SpaceCount = 0 }
  xor    cx, cx              { Default input length = 0 }
  xor    dx, dx              { Default output length = 0 in DL }
  mov    dh, TabSize         { DH will hold TabSize }

  lds    si, S               { DS:SI => input string }
  les    di, Result          { ES:DI => output string }
  lodsb                      { Get input length }
  or     dh, dh              { TabSize = 0? }
  jnz    @@DefLength
  xor    al, al              { Return zero length string if TabSize = 0 }

@@DefLength:
  mov    cl, al              { Store length in counter }
  stosb                      { Store default output length }
  jcxz   @@Done              { Done if empty input string }

  inc    ch                  { Current input position=1 }

@@Next:
  or     bl, bl              { Compare SpaceCount to 0 }
  je     @@NoTab             { If SpaceCount=0 then no tab insert here }
  mov    al, ch              { Ipos to AL }
  xor    ah, ah              { AX has Ipos }
  div    dh                  { Ipos DIV TabSize }
  cmp    ah, 1               { Ipos MOD TabSize = 1 ? }
  jne    @@NoTab             { If not, no tab insert here }
  sub    dl, bl              { Reduce Olen by SpaceCount }
  sub    di, bx              { Remove unused characters from output string }
  mov    al, 09
  stosb                      { Store a tab }
  inc    dl                  { Add one to output length }
  xor    bl, bl              { Reset SpaceCount }

@@NoTab:
  lodsb                      { Get next input character }
  cmp    cl, ch              { End of string? }
  je     @@Store             { Yes, store character anyway }
  inc    bl                  { Increment SpaceCount }
  cmp    al, 32              { Is character a space? }
  jz     @@Store             { Yes, store it for now }
  xor    bl, bl              { Reset SpaceCount }
  cmp    al, 39              { Is it a quote? }
  jz     @@Quotes            { Yep, enter quote loop }
  cmp    al, 34              { Is it a doublequote? }
  jnz    @@Store             { Nope, store it }

@@Quotes:
  mov    ah, al              { Save quote start }

@@NextQ:
  stosb                      { Store quoted character }
  inc    dl                  { Increment output length }
  lodsb                      { Get next character }
  inc    ch                  { Increment Ipos }
  cmp    ch, cl              { At end of line? }
  jae    @@Store             { If so, exit quote loop }
  cmp    al, ah              { Matching end quote? }
  jnz    @@NextQ             { Nope, stay in quote loop }
  cmp    al, 39              { Single quote? }
  jz     @@Store             { Exit quote loop }
  cmp    byte ptr [si-2],'\' { Previous character an escape? }
  jz     @@NextQ             { Stay in if so }

@@Store:
  stosb                      { Store last character }
  inc    dl                  { Increment output length }
  inc    ch                  { Increment input position }
  jz     @@StoreLen          { Exit if past 255 }
  cmp    ch, cl              { Compare Ipos to Ilen }
  jbe    @@Next              { Repeat while characters left }

@@StoreLen:
  les    di, Result          { ES:DI => output string }
  mov    es:[di], dl         { Store final length }

@@Done:
  pop    ds
end;
{$ENDIF}

{$IFDEF OS32}
{&Uses ebx,esi,edi} {&Frame-}
function DetabS(const S : ShortString; TabSize : Byte) : ShortString;
  {-Expand tabs in a string to blanks.}
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    esi,s
  xor    edx,edx
  mov    dl,TabSize
  mov    edi,@Result
{$ELSE}
  push   ebx
  push   edi
  push   esi

  mov    edi, ecx           { EDI => output string }
  mov    esi, eax           { ESI => input string }
{$ENDIF}
  xor    ecx, ecx           { Default input length = 0 }
  and    edx, 0FFh          { Default output length = 0 in DH, DL is Tabsize }
  xor    eax, eax
  mov    cl, [esi]          { Get input length }
  inc    esi
  or     edx, edx           { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx           { Return zero length string if TabSize = 0 }

@@DefLength:
  mov    [edi], cl          { Store default output length }
  inc    edi
  or     ecx, ecx
  jz     @@Done             { Done if empty input string }
  mov    ah, 09h            { Store tab in AH }
  mov    bl, 255            { Maximum length of output }

@@Next:
  mov    al, [esi]          { Next input character }
  inc    esi
  cmp    al, ah             { Is it a tab? }
  jz     @@Tab              { Yes, compute next tab stop }
  mov    [edi], al          { No, store to output }
  inc    edi
  inc    dh                 { Increment output length }
  cmp    dh, bl             { 255 characters max }
  jz     @@StoreLen
  dec    cl
  jnz    @@Next             { Next character while Olen <= 255 }
  jmp    @@StoreLen         { Loop termination }

@@Tab:
  mov    bh, cl             { Save input counter }
  mov    al, dh             { Current output length in AL }
  and    eax, 0FFh          { Clear top byte }
  div    dl                 { OLen DIV TabSize in AL }
  inc    al                 { Round up to next tab position }
  mul    dl                 { Next tab position in AX }
  or     ah, ah             { AX > 255? }
  jnz    @@StoreLen         { Can't store it }
  sub    al, dh             { Count of blanks to insert }
  add    dh, al             { New output length in DH }
  mov    cl, al             { Loop counter for blanks }
  mov    ax, 0920h          { Tab in AH, Blank in AL }
  rep    stosb              { Store blanks }
  mov    cl, bh             { Restore input position }
  dec    cl
  jnz    @@Next             { Back for next input }

@@StoreLen:
  xor    eax, eax
  mov    al, dh
  sub    edi, eax
  dec    edi
  mov    [edi], dh           { Store final length }

@@Done:
{$IFNDEF VirtualPascal}
  pop    esi
  pop    edi
  pop    ebx
{$ENDIF}
end;
{$ELSE}
function DetabS(const S : ShortString; TabSize : Byte) : ShortString; assembler;
  {-Expand tabs in a string to blanks.}
asm
  push   ds
  cld
  xor    cx, cx                   {Default input length = 0}
  xor    dx, dx                   {Default output length = 0 in DL}
  mov    dh, TabSize              {DH will hold TabSize}
  lds    si, s                    {DS:SI => input string}
  les    di, Result               {ES:DI => output string}
  lodsb                           {Get input length}
  or     dh, dh                   {TabSize = 0?}
  jnz    @@DefLength
  xor    al, al                   {Return zero length string if TabSize = 0}
@@DefLength:
  mov    cl, al                   {Store length in counter}
  stosb                           {Store default output length}
  jcxz   @@Done                   {Done if empty input string}

  mov    ah, 09                   {Store tab in AH}
  mov    bl, 255                  {Maximum length of output}
@@Next:
  lodsb                           {Next input character}
  cmp    al, ah                   {Is it a tab?}
  je     @@Tab                    {Yes, compute next tab stop}
  stosb                           {No, store to output}
  inc    dl                       {Increment output length}
  cmp    dl, bl                   {255 characters max}
  loopne @@Next                   {Next character while Olen <= 255}
  jmp    @@StoreLen               {Loop termination}
@@Tab:
  mov    bh, cl                   {Save input counter}
  mov    al, dl                   {Current output length in AL}
  xor    ah, ah                   {Clear top byte}
  div    dh                       {OLen DIV TabSize in AL}
  inc    al                       {Round up to next tab position}
  mul    dh                       {Next tab position in AX}
  or     ah, ah                   {AX > 255?}
  jne    @@StoreLen               {Can't store it}
  sub    al, dl                   {Count of blanks to insert}
  add    dl, al                   {New output length in DL}
  mov    cl, al                   {Loop counter for blanks}
  mov    ax, 0920h                {Tab in AH, Blank in AL}
  rep    stosb                    {Store blanks}
  mov    cl, bh                   {Restore input position}
  loop   @@Next                   {Back for next input}
@@StoreLen:
  les    di, Result               {ES:DI => output string}
  mov    es:[di], dl              {Store final length}
@@Done:
  pop    ds
end;
{$ENDIF}

function ScrambleS(const S, Key : ShortString) : ShortString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  {I,} J, LKey, LStr : Byte;                                           {!!.04}
  I : Cardinal;                                                        {!!.04}
begin
  Result := S;
  LKey := Length(Key);
  LStr := Length(S);
  if LKey = 0 then Exit;
  if LStr = 0 then Exit;
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := Char(Byte(S[I]) xor Byte(Key[J]));
    inc(I);
    dec(J);
  end;
end;

function SubstituteS(const S, FromStr, ToStr : ShortString) : ShortString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  P : Cardinal;
  I : Byte;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      {P := Pos(S[I], FromStr);}                                       {!!.01}
      {if P <> 0 then}                                                 {!!.01}
      if StrChPosS(FromStr, S[I], P) then                              {!!.01}
        Result[I] := ToStr[P];
    end;
end;

function FilterS(const S, Filters : ShortString) : ShortString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  for I := 1 to Length(S) do
    if not CharExistsS(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  Result[0] := Char(Len);
end;

  {--------------- Word / Char manipulation -------------------------}

{$IFDEF OS32}
{&frame-} {&uses none}
function CharExistsS(const S : ShortString; C : AnsiChar) : Boolean;
  {-Determine whether a given character exists in a string. }
{$IFDEF WIN32} register; {$ENDIF}
asm
  {$IFDEF VirtualPascal}
  mov   eax,s
  mov   dl,c
  {$ENDIF}
  xor   ecx, ecx
  mov   ch, [eax]
  inc   eax
  or    ch, ch
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   cl
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   cl
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   cl
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   cl
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ch, 4

@@5:
  cmp   ch, 4
  jge   @@Loop

  cmp   ch, 3
  je    @@1

  cmp   ch, 2
  je    @@2

  cmp   ch, 1
  je    @@3

@@Done:
  xor   eax, eax
  mov   al, cl
end;

{$ELSE}

function CharExistsS(const S : ShortString; C : AnsiChar) : Boolean; assembler;
  {-Count the number of a given character in a string. }
asm
  push   ds
  xor    bl, bl
  xor    cx, cx
  cld
  les    di, S
  mov    cl, es:[di]
  inc    di
  mov    al, C
  jmp    @@2

@@1:
  inc    bl
  jmp    @@Done

@@2:
  repne  scasb
  jz     @@1

@@Done:
  xor    ax, ax
  mov    al, bl
  pop    ds
end;
{$ENDIF}

{$IFDEF OS32}
{&frame-} {&uses none}
function CharCountS(const S : ShortString; C : AnsiChar) : Byte;
  {-Count the number of a given character in a string. }
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    eax,s
  mov    dl,c
{$ENDIF}
  xor   ecx, ecx
  mov   ch, [eax]
  inc   eax
  or    ch, ch
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   cl

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   cl

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   cl

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   cl

@@4:
  add   eax, 4
  sub   ch, 4

@@5:
  cmp   ch, 4
  jge   @@Loop

  cmp   ch, 3
  je    @@1

  cmp   ch, 2
  je    @@2

  cmp   ch, 1
  je    @@3

@@Done:
  mov   al, cl
end;

{$ELSE}

function CharCountS(const S : ShortString; C : AnsiChar) : Byte; assembler;
  {-Count the number of a given character in a string. }
asm
  push   ds
  xor    bl, bl
  xor    cx, cx
  cld
  les    di, S
  mov    cl, es:[di]
  inc    di
  mov    al, C
  jmp    @@2

@@1:
  inc    bl

@@2:
  repne  scasb
  jz     @@1

@@Done:
  mov    al, bl
  pop    ds
end;
{$ENDIF}

function WordCountS(const S, WordDelims : ShortString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I     : Integer;
  SLen  : Byte;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsS(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionS(N : Cardinal; const S, WordDelims : ShortString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  I     : Cardinal;
  Count : Byte;
  SLen  : Byte absolute S;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= SLen) and (Count <> N) do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsS(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
        Inc(I)
    else begin                                                         {!!.02}
      Pos := I;
      Result := True;
    end;                                                               {!!.02}
  end;
end;

function ExtractWordS(N : Cardinal; const S, WordDelims : ShortString) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  I    : Cardinal;
  Len  : Byte;
  SLen : Byte absolute S;
begin
  Len := 0;
  if WordPositionS(N, S, WordDelims, I) then
    {find the end of the current word}
    while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do begin
      {add the I'th character to result}
      Inc(Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  Result[0] := Char(Len);
end;

function AsciiCountS(const S, WordDelims : ShortString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Cardinal;
  InQuote : Boolean;
  SLen    : Byte absolute S;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and (S[i] <> Quote) and CharExistsS(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);
    {find the end of the current word}
    while (I <= SLen) and (InQuote or not CharExistsS(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionS(N : Cardinal; const S, WordDelims : ShortString;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  I       : Cardinal;
  Count   : Byte;
  InQuote : Boolean;
  SLen    : Byte absolute S;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= SLen) and (Count <> N) do begin
    {skip over delimiters}
    while (I <= SLen) and (S[I] <> Quote) and CharExistsS(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= SLen) and (InQuote or not CharExistsS(WordDelims, S[I])) do begin
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

function ExtractAsciiS(N : Cardinal; const S, WordDelims : ShortString;
                       Quote : AnsiChar) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  I       : Cardinal;
  Len     : Byte;
  SLen    : Byte absolute S;
  InQuote : Boolean;
begin
  Len := 0;
  InQuote := False;
  if AsciiPositionS(N, S, WordDelims, Quote, I) then
    {find the end of the current word}
    while (I <= SLen) and ((InQuote) or not CharExistsS(WordDelims, S[I])) do begin
      {add the I'th character to result}
      Inc(Len);
      if S[I] = Quote then
        InQuote := not(InQuote);
      Result [Len] := S[I];
      Inc(I);
    end;
  Result [0] := Char(Len);
end;

procedure WordWrapS(const InSt : ShortString; var OutSt, Overlap : ShortString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  EOS, BOS : Cardinal;
  InStLen  : Byte;
  OutStLen : Byte absolute OutSt;
  OvrLen   : Byte absolute Overlap;
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
  OutStLen := EOS;
  Move(InSt[1], OutSt[1], OutStLen);

  {find the start of the next word in the line}
  BOS := EOS+1;
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    OvrLen := 0
  else begin
    {copy from the start of the next word to the end of the line}
    OvrLen := Succ(InStLen-BOS);
    Move(InSt[BOS], Overlap[1], OvrLen);
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (OutStLen < Margin) then begin
    FillChar(OutSt[OutStLen+1], Margin-OutStLen, ' ');
    OutStLen := Margin;
  end;
end;

  {--------------- String comparison and searching -----------------}
{$IFDEF OS32}
{&frame-} {&uses esi,edi}
function CompStringS(const S1, S2 : ShortString) : integer;
  {-Compare two strings.}
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    esi,s1
  mov    edi,s2
{$ELSE}
  push   edi
  mov    edi, edx           { EDI points to S2 }
  push   esi
  mov    esi, eax           { ESI points to S1 }
{$ENDIF}

  xor    ecx, ecx

  mov    dl, [edi]          { DL = Length(S2) }
  inc    edi                { EDI points to S2[1] }
  mov    cl, [esi]
  inc    esi                { CL = Length(S1) - ESI points to S1[1] }

  or     eax, -1            { EAX holds temporary result }

  cmp    cl, dl             { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    eax                { S1 longer than S2 }
  mov    cl, dl             { Length(S2) in CL }

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
{$IFNDEF VirtualPascal}
  pop    esi
  pop    edi
{$ENDIF}
end;
{$ELSE}
function CompStringS(const S1, S2 : ShortString) : Integer; assembler;
  {-Compare two strings.}
asm
  mov     dx, ds           { Save DS }
  cld                      { Go forward }

  les     di, S2           { ES:DI points to S2 }
  lds     si, S1           { DS:SI points to S1 }

  mov     ah, es:[di]      { AH = Length(S2) }
  inc     di               { DI points to S2[1] }
  lodsb                    { AL = Length(S1) - SI points to S1[1] }

  or      bx, -1           { BX holds temporary result }
  xor     cx, cx           { CX holds count of chars to compare }

  mov     cl, al           { Length(S1) in CL }
  cmp     al, ah           { Compare lengths }
  je      @@EqLen          { Lengths equal? }
  jb      @@Comp           { Jump if S1 shorter than S1 }

  inc     bx               { S1 longer than S2 }
  mov     cl, ah           { Length(S2) in CL }

@@EqLen:
  inc     bx               { Equal or greater }

@@Comp:
  jcxz    @@Done           { Done if either is empty }

  repe    cmpsb            { Compare until no match or CX = 0 }
  je      @@Done           { If Equal, result ready based on length }

  mov     bx, 1
  ja      @@Done           { S1 Greater? Return 1 }
  or      bx, -1           { Else S1 Less, Return -1 }

@@Done:
  mov     ax, bx           { Result into AX }
  mov     ds, dx           { Restore DS }
end;
{$ENDIF}

{$IFDEF OS32}
{&frame-} {&uses ebx,esi,edi}
function CompUCStringS(const S1, S2 : ShortString) : integer;
  {-Compare two strings. This compare is not case sensitive.}
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    esi,s1
  mov    edi,s2
{$ELSE}
  push   ebx
  push   edi                { Save registers }
  push   esi

  mov    edi, edx           { EDI points to S2 }
  mov    esi, eax           { ESI points to S1 }
{$ENDIF}

  xor    eax, eax           { EAX holds chars from S1 }
  xor    ecx, ecx           { ECX holds count of chars to compare }
  xor    edx, edx           { DH holds temp result, DL chars from S2 }
  or     ebx, -1

  mov    al, [edi]          { AH = Length(S2) }
  inc    edi                { EDI points to S2[1] }
  mov    cl, [esi]          { CL = Length(S1) - SI points to S1[1] }
  inc    esi

  cmp    cl, al             { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    ebx                { S1 longer than S2 }
  mov    cl, al             { Shorter length in CL }

@@EqLen:
  inc    ebx                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if lesser string is empty }

@@Start:
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
  jnz    @@Output
  dec    ecx
  jnz    @@Start

  je     @@Done             { If Equal, result ready based on length }

@@Output:
  mov    ebx, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     ebx, -1            { Else S1 Less, Return -1 }

@@Done:
  mov    eax, ebx           { Result into AX }
{$IFNDEF VirtualPascal}
  pop    esi                { Restore Registers }
  pop    edi
  pop    ebx
{$ENDIF}
end;
{$ELSE}
function CompUCStringS(const S1, S2 : ShortString) : Integer; assembler;
  {-Compare two strings. This compare is not case sensitive.}
asm
  push    ds              { Save DS }
  cld                     { Go forward }

  les     di, S2          { ES:DI points to S2 }
  lds     si, S1          { DS:SI points to S1 }

  mov     ah, es:[di]     { AH = Length(S2) }
  inc     di              { DI points to S2[1] }
  lodsb                   { AL = Length(S1) - SI points to S1[1] }

  or      bx, -1          { BX holds temporary result }
  xor     cx, cx          { CX holds count of chars to compare }

  mov     cl, al          { Length(S1) in CL }
  cmp     al, ah          { Compare lengths }
  je      @@EqLen         { Lengths equal? }
  jb      @@Comp          { Jump if S1 shorter than S1 }

  inc     bx              { S1 longer than S2 }
  mov     cl, ah          { Shorter length in CL }

@@EqLen:
  inc     bx              { Equal or greater }

@@Comp:
  jcxz    @@Done          { Done if lesser string is empty }

@@Start:
  lodsb                   { S1[?] into AL }
  call    UpcasePrim      { convert to upper case }
  mov     ah, es:[di]     { S2[?] into AH }
  inc     di              { Point ES:DI to next char in S2 }
  xchg    al, ah
  call    UpcasePrim      { convert to upper case }
  cmp     ah, al          { Compare until no match }
  loope   @@Start

  je      @@Done          { If Equal, result ready based on length }

  mov     bl, 1
  ja      @@Done          { S1 Greater? Return 2 }
  or      bx, -1          { Else S1 Less, Return 0 }

@@Done:
  mov     ax, bx          { Result into AX }
  pop     ds              { Restore DS }
end;
{$ENDIF}

{$IFDEF OS32}
{&frame-} {&uses none}
function SoundexS(const S : ShortString) : ShortString; assembler;
  {-Return 4 character soundex of an input string}
{$IFDEF WIN32} register; {$ENDIF}
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
asm
{$IFDEF VirtualPascal}
  mov    eax,s
  mov    edx,@Result
{$ENDIF}
  push  edi
  mov   edi, edx                 { EDI => output string }
  push  ebx
  push  esi

  mov   esi, eax                 { ESI => input string }
  mov   byte ptr [edi], 4        { Prepare output string to be #4'0000' }
  mov   dword ptr [edi+1], '0000'
  inc   edi

  mov   cl, byte ptr [esi]
  inc   esi
  or    cl, cl                   { Exit if null string }
  jz    @@Done

  xor   eax, eax
  mov   al, [esi]                { Get first character of input string }
  inc   esi

  push  ecx                      { Save ECX across call to CharUpper }
  push  eax                      { Push Char onto stack for CharUpper }
  call  CharUpper                { Uppercase AL }
  pop   ecx                      { Restore saved register }

  mov   [edi], al                { Store first output character }
  inc   edi

  dec   cl                       { One input character used }
  jz    @@Done                   { Was input string one char long? }

  mov   ch, 03h                  { Output max 3 chars beyond first }
  mov   edx, offset SoundexTable { EDX => Soundex table }
  xor   eax, eax                 { Prepare for address calc }
  xor   bl, bl                   { BL will be used to store 'previous char' }

@@Next:
  mov   al, [esi]                { Get next char in AL }
  inc   esi
  mov   al, [edx+eax]            { Get soundex code into AL }
  or    al, al                   { Is AL zero? }
  jz    @@NoStore                { If yes, skip this char }
  cmp   bl, al                   { Is it the same as the previous stored char? }
  je    @@NoStore                { If yes, skip this char }
  mov   [edi], al                { Store char to Dest }
  inc   edi
  dec   ch                       { Decrement output counter }
  jz    @@Done                   { If zero, we're done }
  mov   bl, al                   { New previous character }

@@NoStore:
  dec   cl                       { Decrement input counter }
  jnz   @@Next

@@Done:
  pop   esi
  pop   ebx
  pop   edi
end;
{$ELSE}
function SoundexS(const S : ShortString) : ShortString; assembler;
  {-Return 4 character soundex of input string}
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
asm
  push   ds
  cld
  les    di, Result               { ES:DI => function result }
  mov    al, 4
  stosb                           { Result will be 4 characters long }
  mov    bx, di                   { Store output position in BX }
  mov    al, '0'                  { Store four '0's in output }
  mov    cx, 4
  rep    stosb                    { Initialize to zeros }
  mov    di, bx                   { Reset output position }

  lds    si, S                    { DS:SI => Input string }
  lodsb                           { Length byte into AL }
  mov    cl, al                   { Length into CX }
  jcxz   @@Done                   { We're done if null string }
  lodsb                           { Get first character of input }
  call   UpCasePrim               { Uppercase it }
  stosb                           { Store first output character }
  dec    cx                       { One input character used }
  jcxz   @@Done                   { Done if one character string }

  mov    ah, al                   { Save previous character }
  mov    dx, 0401h                { DL has output length, DH max output length }
  xor    bh, bh                   { Prepare BX for indexing }

@@Next:
  lodsb                           { Next character into AL }
  mov    bl, al                   { Set up base register }
  push   ds
  push   si
  mov    si, seg @Data
  mov    ds, si
  lea    si, SoundexTable
  mov    al, [SI+BX]              { Get soundex code into AL }
  pop    si
  pop    ds
  or     al, al                   { Null soundex code? }
  jz     @@NoStore                { Don't store it }
  cmp    ah, al                   { Code same as previous output? }
  jz     @@NoStore                { Don't store it }
  stosb                           { Store to output }
  inc    dl                       { Output length increased by one }
  cmp    dl, dh                   { Check output length }
  jae    @@Done                   { Stop at four chars of output }
  mov    ah, al                   { Store previous output character }

@@NoStore:
  loop   @@Next

@@Done:
  pop    ds
end;
{$ENDIF}

{$IFDEF OS32}
{&frame-} {&uses ebx,esi}
function MakeLetterSetS(const S : ShortString) : LongInt;
  {-Return a bit-mapped long storing the individual letters contained in S.}
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    esi,s
{$ELSE}
  push   ebx                { Save registers }
  push   esi

  mov    esi, eax           { ESI => string }
{$ENDIF}
  xor    ecx, ecx           { Zero ECX }
  xor    edx, edx           { Zero EDX }
  xor    eax, eax           { Zero EAX }
  add    cl, [esi]          { CX = Length(S) }
  jz     @@Exit             { Done if ECX is 0 }
  inc    esi

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
{$IFNDEF VirtualPascal}
  pop    esi                { Restore registers }
  pop    ebx
{$ENDIF}
end;
{$ELSE}
function MakeLetterSetS(const S : ShortString) : LongInt; assembler;
  {-Return a bit-mapped long storing the individual letters contained in S.}
asm
  push    bp            { Save BP }
  push    ds            { Save DS }
  xor     di, di        { DI = 0 }
  mov     ax, di        { AX = 0 }
  cld                   { Go forward }
  lds     si, S         { DS:SI => string }
  lodsb                 { AX = Length(S) }
  mov     cx, ax        { CX = Length(S) }
  mov     bx, di        { DI:BX = 0 }
  jcxz    @@Exit        { Done if CX is 0 }

@@Next:
  xor     ah, ah        { AH = 0 }
  lodsb                 { AL has next char in S }
  call    UpcasePrim    { Convert to upper case }
  sub     ax, 'A'       { Convert to bit number }
  cmp     ax, 'Z'-'A'   { Was char in range 'A'..'Z'? }
  ja      @@Skip        { Skip it if not }

  xchg    cx, ax        { CX = bit #, AX = loop count }
  xor     dx, dx        { DX:AX = 1 }
  mov     bp, 1
  jcxz    @@NoShift     { don't shift if CX is 0 }

@@Shift:                { DX:BP = 1 shl BitNumber }
  shl     bp, 1         { shift low word }
  rcl     dx, 1         { shift high word }
  loop    @@Shift       { repeat }

@@NoShift:
  or      di, dx        { DI:BX = DI:BX or DX:BP }
  or      bx, bp
  mov     cx, ax        { Restore CX from AX }

@@Skip:
  loop    @@Next        { Get next character }

@@Exit:
  mov     dx, di        { DX:AX = DI:BX }
  mov     ax, bx
  pop     ds            { Restore DS }
  pop     bp            { Restore BP }
end;
{$ENDIF}

{$IFDEF OS32}
{&frame-} {&uses ebx,esi,edi}
procedure BMMakeTableS(const MatchString : ShortString; var BT : BTable);
  {-Build a Boyer-Moore link table}
{$IFDEF WIN32} register; {$ENDIF}
asm
{$IFDEF VirtualPascal}
  mov    esi,MatchString
  mov    edx,BT
{$ELSE}
  push  edi                { Save registers because they will be changed }
  push  esi
  mov   esi, eax           { Move EAX to ESI }
  push  ebx
{$ENDIF}

  xor   eax, eax           { Zero EAX }
  xor   ecx, ecx           { Zero ECX }
  mov   cl, [esi]          { ECX has length of MatchString }
  inc   esi

  mov   ch, cl             { Duplicate CL in CH }
  mov   eax, ecx           { Fill each byte in EAX with length }
  shl   eax, 16
  or    eax, ecx
  mov   edi, edx           { Point to the table }
  mov   ecx, 64            { Fill table bytes with length }
  rep   stosd
  cmp   al, 1              { If length <= 1, we're done }
  jbe   @@MTDone
  xor   ebx, ebx           { Zero EBX }
  mov   cl, al             { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   al, [esi]          { Load table with positions of letters }
  mov   bl, al             { that exist in the search string }
  inc   esi
  mov   [edx+ebx], cl
  dec   cl
  jnz   @@MTNext

@@MTDone:
{$IFNDEF VirtualPascal}
  pop   ebx                { Restore registers }
  pop   esi
  pop   edi
{$ENDIF}
end;
{$ELSE}
procedure BMMakeTableS(const MatchString : ShortString; var BT : BTable); assembler;
  {-Build a Boyer-Moore link table}
asm
  mov    dx, ds           { Save DS in DX }
  cld                     { Go forward }

  lds    si, MatchString  { DS:SI => MatchString }
  les    di, BT           { ES:DI => BY }
  mov    bx, di           { Save DI in BX }
  lodsb                   { AL = length(MatchString) }
  mov    ah, al           { Copy it to AH }
  mov    cx, 128          { Number of words in BT }
  rep    stosw            { Fill BT with length(MatchString) }
  cmp    al, 1            { Is length(MatchString) <= 1? }
  jbe    @@Done           { Yes, we're done }

  mov    di, bx           { Restore base of table from BX }
  mov    bh, ch           { BH = 0 }
  mov    cl, al           { CX = length(MatchString) }
  dec    cx               { CX = length(MatchString)-1 }

@@Next:
  lodsb                   { AL = MatchString[i] }
  mov    bl, al           { BL = MatchString[i] }
  mov    es:[bx+di], cl   { BTable[char] = length(MatchString)-i }
  loop   @@Next           { Repeat for all characters in MatchString }

@@Done:
  mov    ds, dx           { Restore DS from DX }
end;
{$ENDIF}

{$IFDEF OS32}
{&frame+} {&uses ebx,esi,edi}
function BMSearchS(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
{$IFDEF WIN32} register; {$ENDIF}
var
  BufPtr : Pointer;
asm
{$IFDEF VirtualPascal}
  mov    edi, Buffer
  mov    Bufptr, edi
  mov    ebx,BT
  mov    ecx,BufLength
{$ELSE}
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and EDI }
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
{$ENDIF}
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  xor   eax, eax            { Zero EAX }

  mov   dl, [esi]           { Length of MatchString in EDX }
  inc   esi
  and   edx, 0FFh

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
  inc   eax                 { Pos is one based }
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
{$IFNDEF VirtualPascal}
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
{$ENDIF}
end;
{$ELSE}
function BMSearchS(var Buffer; BufLength : Cardinal; var BT : BTable;
                  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
asm
  push   ds                 { Will wipe out DS }
  push   bp                 { Will use BP for temp storage later }

  mov    cx, BufLength      { CX = Buffer size }
  les    di, Buffer         { ES:DI => Buffer }
  lds    bx, BT             { DS:BX => BTable }
  mov    ax, ds             { Keep BTable segment in AX a moment }
  lds    si, MatchString    { DS:SI => MatchString }
  mov    bp, ax             { Keep BTable segment in BP }

  xor    ax, ax             { AX = 0 }
  mov    dx, ax             { DX = 0 }
  mov    dl, [si]           { DL = length(MatchString) }
  cmp    dl, 1              { Check for trivial cases }
  ja     @@Init             { Do Boyer-Moore if longer than one char }
  jb     @@NotFound         { Fail for empty string }

  mov    al, [si+1]         { AL = one and only char to find }
  mov    bx, di             { Save offset of Buffer }
  cld                       { Forward }
  repne  scasb              { Scan Buffer for AL }
  jne    @@NotFound         { Char wasn't found }
  mov    ax, di             { AX holds offset where char was found }
  dec    ax                 { Back up one }
  sub    ax, bx             { Subtract base offset of Buffer }
  jmp    @@Done             { We're done }

@@Init:
  dec    dl                 { DX = length(MatchString)-1 }
  add    si, dx             { DS:SI => MatchString[length(MatchString)-1] }
  add    cx, di             { CX = offset of last char in buffer }
  add    di, dx             { ES:DI => first position to search }
  mov    dh, [si+1]         { DH = MatchString[length(MatchString)] }
  std                       { Go backwards }
  jmp    @@Comp             { Skip link table first time }

@@Next:
  push   ds                 { Save DS a moment }
  mov    ds, bp             { Get segment of link table }
  xlat                      { Get size of link at DS:[BX+AL] }
  pop    ds                 { Restore DS }
  add    di, ax             { Compute next place to search }

@@Comp:
  jc     @@NotFound         { Done if overflowed 64K }
  cmp    di, cx             { At end of buffer? }
  jae    @@NotFound         { Done if so }
  mov    al, es:[di]        { AL = next char to try }
  cmp    dh, al             { Does it match the end of MatchString? }
  jne    @@Next             { If not same, go back and try again }

  push   cx                 { Save end of buffer position }
  dec    di                 { Start comparing one character before }
  mov    cl, dl             { Compare length(MatchString)-1 characters }
  mov    ch, ah             { CH = 0 }
  repe   cmpsb              { Compare backwards while matched }
  je     @@Found            { Matched! }

  mov    al, dl             { Restore SI,DI,AL }
  sub    al, cl
  add    si, ax
  add    di, ax
  inc    di
  mov    al, dh             { Put matched char back in AL }
  pop    cx                 { Restore end of buffer }
  jmp    @@Next             { Try again }

@@Found:                    { DI points to start of match }
  inc    sp                 { End of buffer off stack }
  inc    sp
  pop    bp                 { Get frame pointer back }
  sub    di, word ptr Buffer{ Subtract buffer start address }
  mov    ax, di
  inc    ax
  inc    ax                 { One based string }
  les    di,Pos
  mov    es:[di], ax        { Set Pos }
  mov    ax, 1              { Result = True }
  jmp    @@Done2            { We're done }

@@NotFound:
  xor    ax, ax             { Result = False }

@@Done:                     { Result returned in AX }
  pop    bp

@@Done2:
  cld
  pop    ds
end;
{$ENDIF}

{$IFDEF OS32}
{&frame+} {&uses ebx,esi,edi}
function BMSearchUCS(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
{$IFDEF WIN32} register; {$ENDIF}
var
  BufPtr : Pointer;
asm
{$IFDEF VirtualPascal}
  mov   edi, Buffer
  mov   BufPtr, edi
  mov   ebx, BT
  mov   ecx, BufLength
{$ELSE}
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
{$ENDIF}
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  xor   eax, eax            { Zero EAX }

  mov   dl, byte ptr [esi]  { Length of MatchString in EDX }
  and   edx, 0FFh           { Clean up EDX }
  inc   esi                 { Set ESI to first character }

  cmp   dl, 1               { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

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

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
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
  cld
  call  CharUpper
  std
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
  inc   eax                 { Pos is one based }
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
{$IFNDEF VirtualPascal}
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
{$ENDIF}
end;
{$ELSE}
function BMSearchUCS(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
asm
  push   ds                 { Will wipe out DS }
  push   bp                 { Will use BP for temp storage later }

  mov    cx, BufLength      { CX = Buffer size }
  les    di, Buffer         { ES:DI => Buffer }
  lds    bx, BT             { DS:BX => BTable }
  mov    ax, ds             { Keep BTable segment in AX a moment }
  lds    si, MatchString    { DS:SI => MatchString }
  mov    bp, ax             { Keep BTable segment in BP }

  xor    ax, ax             { AX = 0 }
  mov    dx, ax             { DX = 0 }
  mov    dl, [si]           { DL = length(MatchString) }
  or     dl, dl             { Check for trivial case }
  jz     @@NotFound         { Fail for empty string }

@@Init:
  dec    dl                 { DX = length(MatchString)-1 }
  add    si, dx             { DS:SI => MatchString[length(MatchString)-1] }
  add    cx, di             { CX = offset of last char in buffer }
  add    di, dx             { ES:DI => first position to search }
  mov    dh, [si+1]         { DH = MatchString[length(MatchString)] }
  std                       { Go backwards }
  jmp    @@Comp             { Skip link table first time }

@@Next:
  push   ds                 { Save DS a moment }
  mov    ds, bp             { Get segment of link table }
  xlat                      { Get size of link at DS:[BX+AL] }
  pop    ds                 { Restore DS }
  add    di, ax             { Compute next place to search }

@@Comp:
  jc     @@NotFound         { Done if overflowed 64K }
  cmp    di, cx             { At end of buffer? }
  jae    @@NotFound         { Done if so }
  mov    al, es:[di]        { AL = next char to try }
  call   UpCasePrim         { Raise it to uppercase }
  cmp    dh, al             { Does it match the end of MatchString? }
  jne    @@Next             { If not same, go back and try again }

  push   cx                 { Save end of buffer position }
  dec    di                 { Start comparing one character before }
  mov    cl, dl             { Compare length(MatchString)-1 characters }
  mov    ch, ah             { CH = 0 }
  jcxz   @@Found            { Completely matched if CX = 0 }

@@Comp2:
  lodsb                     { Next match character in AL }
  mov    ah, es:[di]        { Next buffer character in AH }
  dec    di                 { Decrement buffer index }
  xchg   al, ah             { Uppercase it }
  call   UpCasePrim
  cmp    ah, al             { A match? }
  loope  @@Comp2            { Loop while AH=AL and CX<>0 }
  je     @@Found            { Matched! }

  xor    ah, ah             { Restore SI,DI,AX }
  mov    al, dl
  sub    al, cl
  add    si, ax
  add    di, ax
  inc    di
  mov    al, dh             { Put matched char back in AL }
  pop    cx                 { Restore end of buffer }
  jmp    @@Next             { Try again }

@@Found:                    { DI points to start of match }
  inc    sp                 { End of buffer off stack }
  inc    sp
  pop    bp                 { Get frame pointer back }
  sub    di, word ptr Buffer{ Subtract buffer start address }
  mov    ax, di
  inc    ax
  inc    ax                 { Pos will be one based }
  les    di,Pos
  mov    es:[di], ax        { Set Pos }
  mov    ax, 1              { Result = True }
  jmp    @@Done2            { We're done }

@@NotFound:
  xor    ax, ax             { Result = False }

@@Done:                     { Result returned in AX }
  pop    bp

@@Done2:
  cld
  pop    ds
end;
{$ENDIF}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameS(const PathName : ShortString) : ShortString;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';                                                        {!!.02}
  if PathName = '' then Exit;                                          {!!.02}
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (PathName[I] in DosDelimSet) or (I = 0);
  Result := Copy(PathName, Succ(I), MaxFileLen);
end;

function JustNameS(const PathName : ShortString) : ShortString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
begin
  Result := JustFileNameS(PathName);
  if HasExtensionS(Result, DotPos) then
    Result := Copy(Result, 1, DotPos-1);
end;

function JustExtensionS(const Name : ShortString) : ShortString;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Copy(Name, Succ(DotPos), MaxFileLen)
  else
    Result := '';
end;

function JustPathnameS(const PathName : ShortString) : ShortString;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (PathName[I] in DosDelimSet) or (I = 0);

  if I = 0 then
    {Had no drive or directory name}
    Result [0] := #0
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := Copy(PathName, 1, I);
end;

function AddBackSlashS(const DirName : ShortString) : ShortString;
  {-Add a default backslash to a directory name}
begin
  if DirName[Length(DirName)] in DosDelimSet then
    Result := DirName
  else
    Result := DirName + '\';
end;

function CleanFileNameS(const FileName : ShortString) : ShortString;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos  : Cardinal;
  NameLen : Cardinal;
begin
  if HasExtensionS(FileName, DotPos) then begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := Pred(DotPos);
    if NameLen > 8 then
      NameLen := 8;
    Result := Copy(FileName, 1, NameLen)+Copy(FileName, DotPos, 4);
  end else
    {Take the first 8 chars of name}
    Result := Copy(FileName, 1, 8);
end;

function CleanPathNameS(const PathName : ShortString) : ShortString;
  {-Return a pathname cleaned up as DOS does it.}
var
  I : Cardinal;
  S : ShortString;
begin
  Result[0] := #0;
  S := PathName;

  I := Succ(Cardinal(Length(S)));
  repeat
    dec(I);
    if I > 2 then
      if (S[I] = '\') and (S[I-1] = '\') then
        if (S[I-2] <> ':') then
          Delete(S, I, 1);
  until I <= 0;

  I := Succ(Cardinal(Length(S)));
  repeat
    {Get the next directory or drive portion of pathname}
    repeat
      Dec(I);
    until (S[I] in DosDelimSet) or (I = 0);

    {Clean it up and prepend it to output string}
    Result := CleanFileNameS(Copy(S, Succ(I), MaxFileLen)) + Result;
    if I > 0 then begin
      Result := S[I] + Result;
      Delete(S, I, 255);
    end;
  until I <= 0;

end;

function HasExtensionS(const Name : ShortString; var DotPos : Cardinal) : Boolean;
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
    {and (Pos('\', Copy(Name, Succ(DotPos), MaxFileLen)) = 0);}        {!!.01}
    and not CharExistsS(Copy(Name, Succ(DotPos), MaxFileLen), '\');    {!!.03}
end;

  {------------------ Formatting routines --------------------}


function CommaizeChS(L : LongInt; Ch : AnsiChar) : ShortString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrS(L);
  Len := Length(Result);
  NumCommas := (Len - 1) div 3;
  for I := 1 to NumCommas do
    System.Insert(Ch, Result, Len-(I * 3)+1);
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeS(L : LongInt) : ShortString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChS(L, ',');
end;

function FormPrimS(const Mask : ShortString; R : TstFloat; const LtCurr,
                   RtCurr : ShortString; Sep, DecPt : AnsiChar;
                   AssumeDP : Boolean) : ShortString;
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
  S : ShortString;         {temporary string}
  Filler : FillType;       {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative : Boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF : Word;             {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Places,                  {# of digits after the '.'}
  Blanks,                  {# of blanks returned by Str}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr : Byte;         {pointer into temporary string of digits}
  I : Word;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if AssumeDP and (Result <> '') and (Length(Result) < 255) then begin
    Inc(Result[0]);
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
  while (StartF <= Length(Result)) and
    {(Pos(Result[StartF], FormChars) = 0) do}                          {!!.01}
    not CharExistsS(FormChars, Result[StartF]) do                      {!!.01}
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do begin
    case Result[I] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := I;
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
    FillChar(S[Length(S)+1], Places-MaxPlaces, '0');
    inc(S[0], Places-MaxPlaces);
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
    Dec(Result[0]);
  if RtChars > 0 then begin
    S := RtCurr;
    if Byte(S[0]) > RtChars then
      S[0] := Char(RtChars)
    else
      S := LeftPadS(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Byte(S[0]) > LtChars then
      S[0] := Char(LtChars)
    else
      S := PadS(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormS(const Mask : ShortString ; R : TstFloat ; const LtCurr,
                    RtCurr : ShortString ; Sep, DecPt : AnsiChar) : ShortString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimS(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormS(const Mask : ShortString ; L : LongInt ; const LtCurr,
                      RtCurr : ShortString ; Sep : AnsiChar) : ShortString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimS(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

{&Frame+} {&Uses ebx,edi}
function StrChPosS(const P : ShortString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
{var
  Temp : string[2];
begin
  Temp[0] := #1;
  Temp[1] := C;
  Pos := System.Pos(Temp, P);
  Result := Pos <> 0;
end;}
{$IFDEF OS32}
asm                                                            {!!.01 Revised}
 {$IFDEF Win32}
  push  ebx             { Save registers }
  push  edi
 {$ELSE}  {Virtual Pascal}
  mov   eax, P
  xor   edx, edx
  mov   dl, C
  mov   ecx, Pos
 {$ENDIF}

  xor   edi, edi        { Zero counter }
  xor   ebx, ebx
  add   bl, [eax]       { Get input length }
  jz    @@NotFound
  inc   eax

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
 {$IFDEF Win32}
  pop   edi             { Restore registers }
  pop   ebx
 {$ENDIF}
end;
{$ELSE}
assembler;
asm
  push   ds
  les    di, P
  xor    cx, cx
  add    ch, es:[di]
  jz     @@NotFound
  inc    di
  mov    al, C

@@Loop:
  inc   cl              { Increment counter }
  cmp   es:[di], al     { Did we find it? }
  jz    @@Found
  inc   di              { Increment pointer }

  cmp   cl, ch          { End of string? }
  jnz   @@Loop          { If not, loop }

@@NotFound:
  xor   ax, ax          { Not found, zero AX for False }
  les   di, Pos
  mov   es:[di], ax
  jmp   @@Done

@@Found:
  xor   ax, ax
  les   di, Pos
  mov   es:[di], ax     { Zero Pos }
  mov   es:[di], cl     { Set Pos }
  inc   ax              { Set AX to True }

@@Done:
  pop   ds              { Restore DS }
end;
{$ENDIF}

function StrStPosS(const P, S : ShortString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertS(const S : ShortString; C : AnsiChar; Pos : Cardinal) : ShortString;
  {-Insert a character into a string at a specified position.}
var
  Temp : string[2];
begin
  Temp[0] := #1;
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertS(const S1, S2 : ShortString; Pos : Cardinal) : ShortString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteS(const S : ShortString; Pos : Cardinal) : ShortString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
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

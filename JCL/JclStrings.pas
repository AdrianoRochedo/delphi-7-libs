{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclStrings.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: September 06, 2000                                            }
{                                                                              }
{******************************************************************************}

unit JclStrings;

{$I JCL.INC}

{
   (Azret)
   o CharIsAlphaNum was using AND, should be OR. (Fixed)
   o Modified StrLength to use AnsiLnOffset constant instead of
     hardcoded 4
   o AnsiOctalDigits was renamed to AnsiOctDigits
   o AnsiOctDigits and AnsiHexDigits changed to sets (Robert Marquardt)

   o StrTokens (Azret)
     StrWord: tokenizes the string. Returns True if end of string is reached.
              see StrTokens for an example.

   o StrReplace  (Robert Lee)
   o StrLen (Robert Lee)

   o TrimStrings (Anthony Steele)
   o RepeatStr   (Anthony Steele)
                 Az: renamed to StrRepeat,
                 Az: Re-implemented to use move proc. instead of Result + Result

   o fixes:   StrCompareRange, (Azret)
   o fixed:   StrLen  (Azret)
   o added:   AnsiLineBreak (Robert Marquardt)
   o changed: #13, #10 to AnsiCarriageReturn, AnsiLineFeed (Robert Marquardt)
   o improved: StrFind (Azret)
               StrFind now does not need the string to be null terminated you can use
               it on any ansistring buffer.

   o added: StrProper, StrProperBuff functions (Azret)
           lowercases the string and then upercases the first char

           TEST STRING->Test string
           
   o added: CharHex, StrToHex

            CharHex:  converts a given character hex char to a byte, Returns $FF on error

            example:    CharHex('0') = $00;
                        CharHex('1') = $01;
                        CharHex('9') = $09;
                        CharHex('A') = $0F;
                        CharHex('F') = $0F;
                        CharHex('Z') = $FF;  error

            StrToHex: converts a given hex string to byte array and retruns that array in
                      a string buffer. Size of the buffer is the length of
                      the returned string.

   o        example:    StrHex('ABCD') = $ABCD
                        StrHex('ABCD') = $ABCD
                        StrHex('DFGF') = ''   error

                        var
                          Data: string;
                          Stream: TStream;
                        begin
                          ......
                          Data := StrHex('00FFAADFCE24'); // 6 bytes
                          Stream.WriteBuffer(Pointer(Data)^, Length(Data));
                          ......
                        end;
                        
   o Reorganized unit and added (long overdue) string routines from Anthony
     Working on updating the documentation, undocumented routines have a TODO
     comment in the helpfile, or TODOC comment after declaration (MVB)
 }

interface

uses
  Classes, SysUtils;

//------------------------------------------------------------------------------
// Character constants and sets
//------------------------------------------------------------------------------

const

// Misc. often used character definitions 

  AnsiNull           = AnsiChar(#0);
  AnsiBell           = AnsiChar(#7);
  AnsiBackspace      = AnsiChar(#8);
  AnsiTab            = AnsiChar(#9);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiVerticalTab    = AnsiChar(#11);
  AnsiFormFeed       = AnsiChar(#12);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  AnsiEndOfFile      = AnsiChar(#26);
  AnsiEscape         = AnsiChar(#27);
  AnsiSpace          = AnsiChar(' ');
  AnsiComma          = AnsiChar(',');
  AnsiBackslash      = AnsiChar('\');
  AnsiForwardSlash   = AnsiChar('/');

  AnsiDoubleQuote = AnsiChar('"');
  AnsiSingleQuote = AnsiChar('''');

  {$IFDEF WIN32}
  AnsiLineBreak      = AnsiCrLf;
  {$ENDIF}

// Misc. character sets 

  AnsiSigns          = ['-', '+'];
  AnsiWhiteSpace     = [AnsiTab, AnsiLineFeed, AnsiVerticalTab, AnsiFormFeed,
                        AnsiCarriageReturn, AnsiSpace];
  AnsiDecDigits      = ['0'..'9'];
  AnsiOctDigits      = ['0'..'7'];
  AnsiHexDigits      = ['0'..'9', 'A'..'F', 'a'..'f'];

{$IFDEF WIN32}

const

// CharType return values

  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

{$ENDIF}

//------------------------------------------------------------------------------
// String Test Routines
//------------------------------------------------------------------------------

function StrIsAlpha(const S: AnsiString): Boolean;
function StrIsAlphaNum(const S: AnsiString): Boolean;
function StrIsAlphaNumUnderscore(const S: AnsiString): Boolean;
function StrIsNumber(const S: AnsiString): Boolean;
function StrIsSubset(const S: AnsiString; const ValidChars: TSysCharSet): Boolean;
function StrSame(const S1, S2: AnsiString): Boolean;

//------------------------------------------------------------------------------
// String Transformation Routines
//------------------------------------------------------------------------------

function StrCenter(const S: AnsiString; L: Integer; C: AnsiChar  = ' '): AnsiString;
function StrDoubleQuote(const S: AnsiString): AnsiString;
function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrEscapedToString(const S: AnsiString): AnsiString;
procedure StrLower(var S: AnsiString);
procedure StrLowerBuff(S: PAnsiChar);
procedure StrMove(var Dest: AnsiString; const Source: AnsiString; const ToIndex,
  FromIndex, Count: Integer);
function StrPadLeft(const S: AnsiString; Len: Integer;
  C: AnsiChar {$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiSpace {$ENDIF}): AnsiString;
function StrPadRight(const S: AnsiString; Len: Integer;
  C: AnsiChar {$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiSpace {$ENDIF}): AnsiString;
function StrProper(const S: AnsiString): AnsiString;
procedure StrProperBuff(S: PAnsiChar);
function StrQuote(const S: AnsiString; C: AnsiChar): AnsiString;
function StrRemoveChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString); // TODOC Robert Lee
function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
function StrReverse(const S: AnsiString): AnsiString;
procedure StrReverseInPlace(var S: AnsiString);
function StrSingleQuote(const S: AnsiString): AnsiString;
function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
function StrStringToEscaped(const S: AnsiString): AnsiString;
function StrStripNonNumberChars(const S: AnsiString): AnsiString;
function StrToHex(const S: AnsiString): AnsiString;
function StrTrimQuotes(const S: AnsiString): AnsiString;
procedure StrUpper(var S: AnsiString);
procedure StrUpperBuff(S: PAnsiChar);

//------------------------------------------------------------------------------
// String Management
//------------------------------------------------------------------------------

procedure StrAddRef(var S: AnsiString);
function StrAllocSize(const S: AnsiString): Longint;
procedure StrDecRef(var S: AnsiString);
function StrLen(S: PChar): Integer;
function StrLength(const S: AnsiString): Longint;
function StrRefCount(const S: AnsiString): Longint;
procedure StrResetLength(var S: AnsiString);

//------------------------------------------------------------------------------
// String Search and Replace Routines
//------------------------------------------------------------------------------

function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
function StrCompare(const S1, S2: AnsiString): Integer;
function StrCompareRange(const S1, S2: AnsiString; const Index, Count: Integer): Integer;
function StrFillChar(const C: AnsiChar; const Count: Integer): AnsiString;
function StrFind(const Substr, S: AnsiString; const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function StrHasPrefix(const S: string; const Prefixes: array of string): Boolean;
function StrIndex(const S: string; const List: array of string): Integer;
function StrILastPos(const SubStr, S: AnsiString): Integer;
function StrIPos(const SubStr, S: AnsiString): Integer;
function StrIsOneOf(const S: AnsiString; const List: array of AnsiString): Boolean;
function StrLastPos(const SubStr, S: AnsiString): Integer;
function StrMatch(const Substr, S: AnsiString; const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function StrPrefixIndex(const S: string; const Prefixes: array of string): Integer;
function StrSearch(const Substr, S: AnsiString; const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;

//------------------------------------------------------------------------------
// String Extraction
//------------------------------------------------------------------------------

function StrAfter(const SubStr, S: AnsiString): AnsiString;
function StrBefore(const SubStr, S: AnsiString): AnsiString;
function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
function StrChopRight(const S: AnsiString; N: Integer): AnsiString;
function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
function StrRestOf(const S: AnsiString; N: Integer): AnsiString;
function StrRight(const S: AnsiString; Count: Integer): AnsiString;

//------------------------------------------------------------------------------
// Character Test Routines
//------------------------------------------------------------------------------

function CharEqualNoCase(const C1, C2: AnsiChar): Boolean;
function CharIsAlpha(const C: AnsiChar): Boolean;
function CharIsAlphaNum(const C: AnsiChar): Boolean;
function CharIsBlank(const C: AnsiChar): Boolean;
function CharIsControl(const C: AnsiChar): Boolean;
function CharIsDelete(const C: AnsiChar): Boolean;
function CharIsDigit(const C: AnsiChar): Boolean;
function CharIsLower(const C: AnsiChar): Boolean;
function CharIsNumber(const C: AnsiChar): Boolean;
function CharIsPrint(const C: AnsiChar): Boolean;
function CharIsPunctuation(const C: AnsiChar): Boolean;
function CharIsReturn(const C: AnsiChar): Boolean;
function CharIsSpace(const C: AnsiChar): Boolean;
function CharIsUpper(const C: AnsiChar): Boolean;
function CharIsWhiteSpace(const C: AnsiChar): Boolean;
function CharType(const C: AnsiChar): Word;

//------------------------------------------------------------------------------
// Character Transformation Routines
//------------------------------------------------------------------------------

function CharHex(const C: AnsiChar): Byte;
function CharLower(const C: AnsiChar): AnsiChar;
function CharUpper(const C: AnsiChar): AnsiChar;
function CharToggleCase(const C: AnsiChar): AnsiChar;

//------------------------------------------------------------------------------
// Character Search and Replace
//------------------------------------------------------------------------------

function CharPos(const S: AnsiString; const C: AnsiChar;
  const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function CharIPos(const S: AnsiString; const C: AnsiChar;
  const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): Integer;

//------------------------------------------------------------------------------
// PCharVector
//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_DYNAMICARRAYS}

type
  PCharVector = ^PChar;

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
function PCharVectorCount(const Source: PCharVector): Integer;
procedure PCharVectorToStrings(const Dest: TStrings; const Source: PCharVector);
procedure FreePCharVector(var Dest: PCharVector);

{$ENDIF}

//------------------------------------------------------------------------------
// MultiSz Routines
//------------------------------------------------------------------------------

function StringsToMultiSz(var Dest: PChar; const Source: TStrings): PChar;
procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
procedure FreeMultiSz(var Dest: PChar);

//------------------------------------------------------------------------------
// TStrings Manipulation
//------------------------------------------------------------------------------

procedure StrToStrings(S: AnsiString; Sep: AnsiString; const List: TStrings);
function StringsToStr(const List: TStrings; Sep: AnsiString): AnsiString;
procedure TrimStrings(const List: TStrings);
procedure TrimStringsRight(const List: TStrings);

//------------------------------------------------------------------------------
// Miscellaneous
//------------------------------------------------------------------------------

function BooleanToStr(B: Boolean): string;
function FileToString(const FileName: AnsiString): AnsiString;
procedure StringToFile(const FileName, Contents: AnsiString);
function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
procedure StrTokens(const S: AnsiString; const List: TStrings);
procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; const List: TStrings);
function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean;

implementation

uses
  {$IFDEF WIN32}
  Windows;
  {$ENDIF}

//==============================================================================
// Internal
//==============================================================================

type
  TAnsiStrRec = packed record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;

const
  AnsiStrRecSize  = SizeOf(TAnsiStrRec);     // size of the AnsiString header rec
  AnsiCharCount   = Ord(High(AnsiChar)) + 1; // # of chars in one set
  AnsiLoOffset    = AnsiCharCount * 0;       // offset to lower case chars
  AnsiUpOffset    = AnsiCharCount * 1;       // offset to upper case chars
  AnsiReOffset    = AnsiCharCount * 2;       // offset to reverse case chars
  AnsiAlOffset    = 12;                      // offset to AllocSize in StrRec
  AnsiRfOffset    = 8;                       // offset to RefCount in StrRec
  AnsiLnOffset    = 4;                       // offset to Length in StrRec
  AnsiCaseMapSize = AnsiCharCount * 3;       // # of chars is a table

var
  AnsiCaseMap: array [0..AnsiCaseMapSize - 1] of AnsiChar; // case mappings
  AnsiCaseMapReady: Boolean = False;         // true if case map exists
  AnsiCharTypes: array [AnsiChar] of Word;

//------------------------------------------------------------------------------

{$IFDEF WIN32}

procedure LoadCharTypes;
var
  CurrChar: AnsiChar;
  CurrType: Word;
begin
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
    AnsiCharTypes[CurrChar] := CurrType;
  end;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF WIN32}

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: AnsiChar;
begin
  if not AnsiCaseMapReady then
  begin
    for CurrChar := Low(AnsiChar) to High(AnsiChar) do
    begin
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      Windows.CharLowerBuff(@LoCaseChar, 1);
      Windows.CharUpperBuff(@UpCaseChar, 1);
      if CharIsUpper(CurrChar) then
        ReCaseChar := LoCaseChar
      else
        if CharIsLower(CurrChar) then
          ReCaseChar := UpCaseChar
        else
          ReCaseChar := CurrChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiLoOffset] := LoCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiUpOffset] := UpCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiReOffset] := ReCaseChar;
    end;
    AnsiCaseMapReady := True;
  end;
end;

{$ENDIF}

//------------------------------------------------------------------------------

// Uppercases or Lowercases a give AnsiString depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCase{(var Str: AnsiString; const Offset: Integer)}; assembler;
asm
        // make sure that the string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // create unique string if this one is ref-counted

        PUSH    EDX
        CALL    UniqueString
        POP     EDX

        // make sure that the new string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // get the length, and prepare the counter

        MOV     ECX, [EAX - AnsiStrRecSize].TAnsiStrRec.Length
        DEC     ECX
        JS      @@StrIsNull

        // ebx will hold the case map, esi pointer to Str

        PUSH    EBX
        PUSH    ESI

        // load case map and prepare variables }

        LEA     EBX,[AnsiCaseMap + EDX]
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        // get current char from the AnsiString

        MOV     DL, [ESI]

        // get corresponding char from the case map

        MOV     AL, [EBX + EDX]

        // store it back in the string

        MOV     [ESI], AL

        // update the loop counter and check the end of stirng

        DEC     ECX
        JL      @@Done

        // do the same thing with next 3 chars

        MOV     DL, [ESI + 1]
        MOV     AL, [EBX + EDX]
        MOV     [ESI + 1], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 2]
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 3]
        MOV     AL, [EBX + EDX]
        MOV     [ESI + 3], AL

        // point AnsiString to next 4 chars

        ADD     ESI, 4

        // update the loop counter and check the end of stirng

        DEC     ECX
        JGE     @@NextChar

@@Done:
        POP     ESI
        POP     EBX

@@StrIsNull:
end;

//------------------------------------------------------------------------------

// Internal utility function
// Uppercases or Lowercases a give null terminated string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCaseBuff{(S: PAnsiChar; const Offset: Integer)}; assembler;
asm
        // make sure the string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // ebx will hold the case map, esi pointer to Str

        PUSH    EBX
        PUSH    ESI

        // load case map and prepare variables

        LEA     EBX, [AnsiCaseMap + EDX]
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        // get current char from the string

        MOV     DL, [ESI]

        // check for null char

        TEST    DL, DL
        JZ      @@Done

        // get corresponding char from the case map

        MOV     AL, [EBX + EDX]

        // store it back in the string

        MOV     [ESI], AL

        // do the same thing with next 3 chars

        MOV     DL, [ESI + 1]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 1], AL

        MOV     DL, [ESI + 2]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        MOV     DL, [ESI + 3]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 3], AL

        // point string to next 4 chars

        ADD     ESI, 4
        JMP     @@NextChar

@@Done:
        POP     ESI
        POP     EBX

@@StrIsNull:
end;

//==============================================================================
// String Test Routines
//==============================================================================

function StrIsAlpha(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    if not CharIsAlpha(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function StrIsAlphaNum(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function StrIsAlphaNumUnderscore(const S: AnsiString): Boolean;
var
  I: Integer;
  C: AnsiChar;
begin
  Result := True;
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if not (CharIsAlphaNum(C) or (C = '_')) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function StrIsNumber(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    if not CharIsNumber(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function StrIsSubset(const S: AnsiString; const ValidChars: TSysCharSet): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if not (S[I] in ValidChars) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

//------------------------------------------------------------------------------

function StrSame(const S1, S2: AnsiString): Boolean;
begin
  Result := StrCompare(S1, S2) = 0;
end;

//==============================================================================
// String Transformation Routines
//==============================================================================

function StrCenter(const S: AnsiString; L: Integer; C: AnsiChar  = ' '): AnsiString;
begin
  if Length(S) < L then
  begin
    Result := StringOfChar(C, (L - Length(S)) div 2) + S;
    Result := Result + StringOfChar(C, L - Length(Result));
  end
  else
    Result := S;
end;

//------------------------------------------------------------------------------

function StrDoubleQuote(const S: AnsiString): AnsiString;
begin
  Result := AnsiDoubleQuote + S + AnsiDoubleQuote;
end;

//------------------------------------------------------------------------------

function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Text
  else
    Result := Prefix + Text;
end;

//------------------------------------------------------------------------------

function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
var
  SuffixLen: Integer;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

//------------------------------------------------------------------------------

function StrEscapedToString(const S: AnsiString): AnsiString;
var
  I, Len, N, Val: Integer;

  procedure HandleHexEscapeSeq;
  const
    HexDigits = AnsiString('0123456789abcdefABCDEF');
  begin
    N := Pos(S[I + 1], HexDigits) - 1;
    if N < 0 then
      // '\x' without hex digit following is not escape sequence
      Result := Result + '\x'
    else
    begin
      Inc(I); // Jump over x
      if N >= 16 then
        N := N - 6;
      Val := N;
      // Same for second digit
      if I < Len then
      begin
        N := Pos(S[I + 1], HexDigits) - 1;
        if N >= 0 then
        begin
          Inc(I); // Jump over first digit
          if N >= 16 then
            N := N - 6;
          Val := Val * 16 + N;
        end;
      end;
      Result := Result + Chr(Val);
    end;
  end;

  procedure HandleOctEscapeSeq;
  const
    OctDigits = AnsiString('01234567');
  begin
    // first digit
    Val := Pos(S[I], OctDigits) - 1;
    if I < Len then
    begin
      N := Pos(S[I + 1], OctDigits) - 1;
      if N >= 0 then
      begin
        Inc(I);
        Val := Val * 8 + N;
      end;
      if I < Len then
      begin
        N := Pos(S[I + 1], OctDigits) - 1;
        if N >= 0 then
        begin
          Inc(I);
          Val := Val * 8 + N;
        end;
      end;
    end;
    Result := Result + Chr(Val);
  end;

begin
  Result := '';
  I := 1;
  Len := Length(S);
  while I <= Len do
  begin
    if not ((S[I] = '\') and (I < Len)) then
      Result := Result + S[I]
    else
    begin
      Inc(I); // Jump over escape character
      case S[I] of
        'a':
          Result := Result + AnsiBell;
        'b':
          Result := Result + AnsiBackspace;
        'f':
          Result := Result + AnsiFormFeed;
        'n':
          Result := Result + AnsiLineFeed;
        'r':
          Result := Result + AnsiCarriageReturn;
        't':
          Result := Result + AnsiTab;
        'v':
          Result := Result + AnsiVerticalTab;
        '\':
          Result := Result + '\';
        '"':
          Result := Result + '"';
        '''':
          Result := Result + ''''; // Optionally escaped
        '?':
          Result := Result + '?';  // Optionally escaped
        'x':
          if I < Len then
            // Start of hex escape sequence
            HandleHexEscapeSeq
          else
            // '\x' at end of AnsiString is not escape sequence
            Result := Result + '\x';
        '0'..'9':
          // start of octal escape sequence
          HandleOctEscapeSeq;
      else
        // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
end;

//------------------------------------------------------------------------------

procedure StrLower(var S: AnsiString); assembler;
asm
        // StrCase(Str, LoOffset)

        XOR     EDX, EDX         // MOV     EDX, LoOffset
        JMP     StrCase
end;

//------------------------------------------------------------------------------

procedure StrLowerBuff(S: PAnsiChar); assembler;
asm
        // StrCaseBuff(S, LoOffset)
        XOR     EDX, EDX                // MOV     EDX, LoOffset
        JMP     StrCaseBuff
end;

//------------------------------------------------------------------------------

procedure StrMove(var Dest: AnsiString; const Source: AnsiString;
  const ToIndex, FromIndex, Count: Integer); assembler;
asm
        // make sure that Source is not null

        TEST    EDX, EDX
        JZ      @@NoMove

        // save params before UniqueString call

        PUSH    EDX
        PUSH    ECX

        // create a new AnsiString if Dest if ref-counted

        CALL    UniqueString

        // restore params

        POP     ECX
        POP     EDX

        // make sure that the new AnsiString is not null

        TEST    EAX, EAX
        JZ      @@NoMove

        // ToIndex >= 1

        DEC     ECX
        JS      @@NoMove

        // FromIndex >= 1

        DEC     FromIndex
        JS      @@NoMove

        PUSH    EBX
        PUSH    EDI
        PUSH    ESI

        // AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // get lengths of strings

        MOV     EBX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     EDX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length

        // make sure that FromIndex index is <= Length(Source)

        SUB     EBX, FromIndex
        JLE     @@Skip

        // make sure that count is less then # of chars availible.
        // if Count > Length(Source) then Count := Length(Source)

        CMP     EBX, Count
        JLE     @@Skip
        MOV     EBX, Count

@@Skip:
        // make sure that we don't move data beyond Dest AnsiString
        // if ToIndex > Length(Dest) then Exit;

        SUB     EDX, ECX
        JLE     @@Done

        // make sure that Dest AnsiString is big enough for Count
        // after subtructing indexes

        CMP     EDX, EBX
        JL      @@Done

        // point Dest to requested index

        ADD     EDI, ECX

        // point Source to requested index

        ADD     ESI, FromIndex

        // move # of chars to move into ECX and leave EBX we'll need it

        MOV     ECX, EBX

        // # of DWORDs we have in Count, the long loop will be
        // DWORD by DWORD, Count div 4

        SHR     ECX, 2
        JZ      @@moveRest

@@longLoop:

        // get 4 bytes from source

        MOV     EAX, [ESI]

        // move 'em into dest

        MOV     [EDI], EAX

        // check the counter

        DEC     ECX
        JZ      @@moveRest4

        // do the same thing with next 4 bytes

        MOV     EAX, [ESI + 4]
        MOV     [EDI + 4], EAX

        // inc AnsiString pointers by 8

        ADD     ESI, 8
        ADD     EDI, 8

        // check the counter

        DEC     ECX
        JNE     @@longLoop
        JMP     @@moveRest

@@moveRest4:
        ADD     ESI, 4
        ADD     EDI, 4

@@moveRest:
        // get saved count back into ECX

        MOV     ECX, EBX

        // clear direction flag so that MOVSB goes forword

        CLD

        // get # of chars remaining, Count mod 4

        AND     ECX, $03

        // and move the rest of the Source

        REP     MOVSB

@@Done:
        POP     EDI
        POP     ESI
        POP     EBX

@@NoMove:
end;

//------------------------------------------------------------------------------

function StrPadLeft(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else
    Result := S;
end;

//------------------------------------------------------------------------------

function StrPadRight(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else
    Result := S;
end;

//------------------------------------------------------------------------------

function StrProper(const S: AnsiString): AnsiString;
var
  P: PChar;
begin
  Result := '';
  if Length(S) > 0 then
  begin
    Pointer(Result) := Pointer(S); // let StrLower call UniqueString
    StrLower(Result);
    P := Pointer(Result);
    P^ := CharUpper(P^);
  end;
end;

//------------------------------------------------------------------------------

procedure StrProperBuff(S: PAnsiChar);
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    StrLowerBuff(S);
    S^ := CharUpper(S^);
  end;
end;

//------------------------------------------------------------------------------

function StrQuote(const S: AnsiString; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  Result := S;
  if L > 0 then
  begin
    if Result[1] <> C then
    begin
      Result := C + Result;
      Inc(L);
    end;
    if Result[L] <> C then
      Result := Result + C;
  end;
end;

//------------------------------------------------------------------------------

function StrRemoveChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;
begin
  SetLength(Result, Length(S));
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  while (Source <> nil) and (Source^ <> #0) do
  begin
    if not (Source^ in Chars) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, (Longint(Dest) - Longint(PChar(Result))) div SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
var
  L: Integer;
  P: PChar;
begin
  L := Length(S);
  SetLength(Result, Count * L);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(S)^, P^, L);
    P := P + L;
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------

procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString);
var
  SearchPtr: PChar;
  istr, jstr, StrEnd, ReplaceEnd, pTemp : PChar;
  k, l, SearchLen, ReplaceLen: Integer;
  WorkStr: AnsiString;
  C: Char;
begin
  SearchPtr := PChar(Search);

  SearchLen := Length(Search);
  ReplaceLen := Length(Replace);

  StrEnd := @S[Length(S)+1];
  ReplaceEnd := @Replace[ReplaceLen+1];

  Assert(SearchLen > 0, 'Search - must not be empty.');

  // make sure newStr is long enough
  if ReplaceLen > SearchLen then
    SetLength(WorkStr, ((Length(S) div SearchLen)+1) * ReplaceLen)
  else
    SetLength(WorkStr, Length(S));

  iStr := @S[1];
  jStr := @WorkStr[1];

  // primary search char
  C := Search[1];

  // set to stop loop (could be a problem if ref count > 1 )
  StrEnd[0] := c;

  while True do
  begin
    // copy until possible match
    k := 0;
    while iStr[k] <> c do
    begin
      jStr[k] := iStr[k];
      Inc(k);
    end;

    Inc(jStr, k);
    Inc(iStr, k);

    if iStr = StrEnd then
       Break;

    l := StrEnd - iStr;

    k := 1; // First char is already matched.
    if l > SearchLen then
    begin
      // don't need to check l since the zero at end of Search will stop loop
      l := SearchLen;
      pTemp := SearchPtr;  // increases "priority" of SearchPtr

      while iStr[k] = ptemp[k] do
        Inc(k);
    end
    else
      // here we have to check
      while (k < l) and (iStr[k] = SearchPtr[k]) do
        Inc(k);

    if k = l then // match
    begin  // copy
      Inc(iStr, SearchLen);
      k := -ReplaceLen;
      Inc(jstr, ReplaceLen);

      while k < 0 do
      begin
        jstr[k] := ReplaceEnd[k];
        Inc(k);
      end;
    end
    else // no match
    begin
      jStr^ := iStr^;
      Inc(jStr);
      Inc(iStr);
    end;
  end;

  StrEnd[0] := #0;  // return to original state
  SetLength(WorkStr, jstr-PChar(WorkStr));
  S := WorkStr;
end;

//------------------------------------------------------------------------------

function StrReverse(const S: AnsiString): AnsiString;
var
  P1, P2: PChar;
  C: AnsiChar;
begin
  Result := S;
  UniqueString(Result);
  P1 := PChar(Result);
  P2 := P1 + SizeOf(AnsiChar) * (Length(Result) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;

//------------------------------------------------------------------------------

procedure StrReverseInPlace(var S: AnsiString);
var
  P1, P2: PChar;
  C: AnsiChar;
begin
  UniqueString(S);
  P1 := PChar(S);
  P2 := P1 + SizeOf(AnsiChar) * (Length(S) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;

//------------------------------------------------------------------------------

function StrSingleQuote(const S: AnsiString): AnsiString;
begin
  Result := AnsiSingleQuote + S + AnsiSingleQuote;
end;

//------------------------------------------------------------------------------

function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;
begin
  if Delimiters = [] then
    Include(Delimiters, AnsiSpace);
  if S <> '' then
  begin
    SetLength(Result, Length(S));
    Source := PChar(S);
    Dest := PChar(Result);
    Dest^ := UpCase(Source^);
    Inc(Source);
    Inc(Dest);
    while Source^ <> #0 do
    begin
      Dest^ := Source^;
      if Source^ in Delimiters then
      begin
        Inc(Source);
        Inc(Dest);
        Dest^ := UpCase(Source^);
      end;
      Inc(Dest);
      Inc(Source);
    end;
  end;
end;

//------------------------------------------------------------------------------

function StrStringToEscaped(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      AnsiBackspace:
        Result := Result + '\b';
      AnsiBell:
        Result := Result + '\a';
      AnsiCarriageReturn:
        Result := Result + '\r';
      AnsiFormFeed:
        Result := Result + '\f';
      AnsiLineFeed:
        Result := Result + '\n';
      AnsiTab:
        Result := Result + '\t';
      AnsiVerticalTab:
        Result := Result + '\v';
      '\':
        Result := Result + '\\';
      '"':
        Result := Result + '\"';
    else
      // Characters < ' ' are escaped with hex sequence
      if S[I] < #32 then
        Result := Result + Format('\x%.2x',[Integer(S[I])])
      else
        Result := Result + S[I];
    end;
  end;
end;

//------------------------------------------------------------------------------

function StrStripNonNumberChars(const S: AnsiString): AnsiString;
var
  I: Integer;
  C: AnsiChar;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if CharIsNumber(C) then
      Result := Result + C;
  end;
end;

//------------------------------------------------------------------------------

function StrToHex(const S: AnsiString): AnsiString;
var
  P: PChar;
  I, L, N, C: Integer;
  BL, BH: Byte;
begin
  Result := '';
  if S = '' then Exit;

  L := Length(S);
  if L = 1 then
  begin
    BL := CharHex(S[1]);
    if BL = $FF then
      Exit;
    SetLength(Result, 1);
    Byte(Result[1]) := BL;
    Exit;
  end;

  P := Pointer(S);
  I := L div 2;
  SetLength(Result, I);
  C := 1;
  N := 1;
  while C <= L do
  begin
    BH := CharHex(P^);
    Inc(C);
    BL := CharHex((P + 1)^);
    if C > L then
      BL := 0;
    Inc(C);
    if (BH = $FF) or (BL = $FF) then
    begin
      Result := '';
      Exit;
    end;
    BH := (BH shl 4) + BL;
    Byte(Result[N]) := BH;
    Inc(P, 2);
    Inc(N);
  end;
end;

//------------------------------------------------------------------------------

function StrTrimQuotes(const S: AnsiString): AnsiString;
var
  First, Last: AnsiChar;
  L: Integer;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and ((First = AnsiSingleQuote) or (First = AnsiDoubleQuote)) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------

procedure StrUpper(var S: AnsiString); assembler;
asm
        // StrCase(Str, UpOffset)
        MOV     EDX, AnsiUpOffset
        JMP     StrCase
end;

//------------------------------------------------------------------------------

procedure StrUpperBuff(S: PAnsiChar); assembler;
asm
        // StrCaseBuff(S, UpOffset)
        MOV     EDX, AnsiUpOffset
        JMP     StrCaseBuff
end;

//==============================================================================
// String Management
//==============================================================================

procedure StrAddRef(var S: AnsiString);
var
  Foo: AnsiString;
begin
  if StrRefCount(S) = -1 then
    UniqueString(S)
  else
  begin
    Foo := S;
    Pointer(Foo) := nil;
  end;
end;

//------------------------------------------------------------------------------

function StrAllocSize(const S: AnsiString): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(Pointer(S)) - AnsiRfOffset);
    if Integer(P^) <> -1 then
    begin
      P := Pointer(Integer(Pointer(S)) - AnsiAlOffset);
      Result := Integer(P^);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure StrDecRef(var S: AnsiString);
var
  Foo: string;
begin
  case StrRefCount(S) of
    -1, 0: { nothing } ;
     1:
       begin
         Finalize(S);
         Pointer(S) := nil;
       end;
  else
    Pointer(Foo) := Pointer(S);
  end;
end;

//------------------------------------------------------------------------------

function StrLen(S: PChar): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @@EXIT

        PUSH    EBX
        MOV     EDX, EAX                 // save pointer 
@L1:    MOV     EBX, [EAX]               // read 4 bytes
        ADD     EAX, 4                   // increment pointer
        LEA     ECX, [EBX-$01010101]     // subtract 1 from each byte
        NOT     EBX                      // invert all bytes
        AND     ECX, EBX                 // and these two
        AND     ECX, $80808080           // test all sign bits
        JZ      @L1                      // no zero bytes, continue loop
        TEST    ECX, $00008080           // test first two bytes
        JZ      @L2
        SHL     ECX, 16                  // not in the first 2 bytes
        SUB     EAX, 2
@L2:    SHL     ECX, 9                   // use carry flag to avoid a branch
        SBB     EAX, EDX                 // compute length
        POP     EBX

        JZ      @@EXIT                   // Az: SBB sets zero flag
        DEC     EAX                      // do not include null terminator
@@EXIT:
end;

//------------------------------------------------------------------------------

function StrLength(const S: AnsiString): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(Pointer(S)) - AnsiLnOffset);
    Result := Integer(P^) and (not $80000000 shr 1);
  end;
end;

//------------------------------------------------------------------------------

function StrRefCount(const S: AnsiString): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(Pointer(S)) - AnsiRfOffset);
    Result := Integer(P^);
  end;
end;

//------------------------------------------------------------------------------

procedure StrResetLength(var S: AnsiString);
begin
  SetLength(S, StrLen(PChar(S)));
end;

//==============================================================================
// String Search and Replaces Routines
//==============================================================================

function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

//------------------------------------------------------------------------------

function StrCompare(const S1, S2: AnsiString): Integer; assembler;
asm
        // check if pointers are equal

        CMP     EAX, EDX
        JE      @@Equal

        // if S1 is nil return - Length(S2)

        TEST    EAX, EAX
        JZ      @@Str1Null

        // if S2 is nill return  Length(S1)

        TEST    EDX, EDX
        JZ      @@Str2Null

        // EBX will hold case map, ESI S1, EDI S2

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // move AnsiString pointers

        MOV     ESI, EAX
        MOV     EDI, EDX

        // get the length of strings

        MOV     EAX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     EDX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length

        // exit if Length(S1) <> Length(S2)

        CMP     EAX, EDX
        JNE     @@MissMatch

        // check the length just in case

        DEC     EDX
        JS      @@InvalidStr

        DEC     EAX
        JS      @@InvalidStr

        // load case map

        LEA     EBX, AnsiCaseMap

        // make ECX our loop counter

        MOV     ECX, EAX

        // clear working regs

        XOR     EAX, EAX
        XOR     EDX, EDX

        // get last chars

        MOV     AL, [ESI+ECX]
        MOV     DL, [EDI+ECX]

        // lower case them

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]

        // compare them

        CMP     AL, DL
        JNE     @@MissMatch

        // if there was only 1 char then exit

        JECXZ   @@Match

@@NextChar:
        // case sensitive compare of strings

        REPE    CMPSB
        JE      @@Match

        // if there was a missmatch try case insensitive compare, get the chars

        MOV     AL, [ESI-1]
        MOV     DL, [EDI-1]

        // lowercase and compare them, if equal then continue

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]
        CMP     AL, DL
        JE      @@NextChar

        // if we make it here then strings don't match,  return the difference

@@MissMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Match:
        // match, return 0

        XOR     EAX, EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@InvalidStr:
        XOR     EAX, EAX
        DEC     EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Str1Null:
        // return = - Length(Str2);

        MOV     EDX, [EDX-AnsiStrRecSize].TAnsiStrRec.Length
        SUB     EAX, EDX
        RET

@@Str2Null:
        // return = Length(Str2);

        MOV     EAX, [EAX-AnsiStrRecSize].TAnsiStrRec.Length
        RET

@@Equal:
        XOR     EAX, EAX
end;

//------------------------------------------------------------------------------

function StrCompareRange(const S1, S2: AnsiString; const Index, Count: Integer): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @@StrNull

        TEST    EDX, EDX
        JZ      @@StrNull

        DEC     ECX
        JS      @@StrNull

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX, Count
        DEC     EBX
        JS      @@NoWork

        MOV     ESI, EAX
        MOV     EDI, EDX

        MOV     EDX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // # of chars in S1 - (Index - 1)
        SUB     EDX, ECX
        JLE     @@NoWork

        // # of chars in S1 - (Count - 1)
        SUB     EDX, EBX
        JLE     @@NoWork

        // move to index'th char
        ADD     ESI, ECX

        MOV     ECX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        DEC     ECX
        JS      @@NoWork

        // if Length(S2) > Count then ECX := Count else ECX := Length(S2)

        CMP     ECX, EBX
        JLE     @@Skip1
        MOV     ECX, EBX

@@Skip1:
        XOR     EAX, EAX
        XOR     EDX, EDX

@@Loop:
        MOV     AL, [ESI]
        INC     ESI
        MOV     DL, [EDI]
        INC     EDI

        CMP     AL, DL
        JNE     @@MisMatch

        DEC     ECX
        JG      @@Loop

@@Match:
        XOR     EAX, EAX
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@MisMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@NoWork:
        MOV     EAX, -2
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@StrNull:
        MOV     EAX, -1

@@Exit:
end;

//------------------------------------------------------------------------------

function StrFillChar(const C: AnsiChar; const Count: Integer): AnsiString;
begin
  Assert(Count >= 0);
  SetLength(Result, Count);
  FillChar(Result[1], Count, Ord(C));
end;

//------------------------------------------------------------------------------

function StrFind(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
const
   SearchChar: Byte = 0;
   NumberOfChars: Integer = 0;
asm
        // if SubStr = '' then  Return := 0;

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        // if Str = '' then  Return := 0;

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // Index := Index - 1; if Index < 0 then Return := 0;

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and - # of chars in Substr to compare

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // save the address of Str to compute the result

        PUSH    ESI

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     NumberOfChars, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     EDX, EDX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     DL, [EDI]
        INC     EDI

        // lower case it

        MOV     DL, [EBX + EDX]
        MOV     SearchChar, DL

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of AnsiString.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the AnsiString, and point Str to the next one

        MOV     AL, [ESI]
        INC     ESI


        // lower case current char

        MOV     AL, [EBX + EAX]

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, SearchChar
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare

        MOV     EDX, NumberOfChars

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [ESI + EDX]
        CMP     AL, [EDI + EDX]
        JE      @@CompareNext

        // otherwise try the reverse case. If they still don't match go back to the Find loop

        MOV     AL, [EBX + EAX + AnsiReOffset]
        CMP     AL, [EDI + EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//------------------------------------------------------------------------------

function StrHasPrefix(const S: string; const Prefixes: array of string): Boolean;
begin
  Result := StrPrefixIndex(S, Prefixes) > -1;
end;

//------------------------------------------------------------------------------

function StrIndex(const S: string; const List: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(List) to High(List) do
  begin
    if AnsiSameText(S, List[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function StrILastPos(const SubStr, S: AnsiString): Integer;
begin
  Result := JclStrings.StrLastPos(AnsiUpperCase(SubStr), AnsiUpperCase(S));
end;

//------------------------------------------------------------------------------

function StrIPos(const SubStr, S: AnsiString): integer;
begin
  Result := Pos(AnsiUpperCase(SubStr), AnsiUpperCase(S));
end;

//------------------------------------------------------------------------------

function StrIsOneOf(const S: AnsiString; const List: array of AnsiString): Boolean;
begin
  Result := StrIndex(S, List) > -1;
end;

//------------------------------------------------------------------------------

function StrLastPos(const SubStr, S: AnsiString): Integer;
var
  Last, Current: PAnsiChar;
begin
  Result := 0;
  Last := nil;
  Current := PAnsiChar(S);
  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := AnsiStrPos(PAnsiChar(Current), PAnsiChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current, Length(SubStr));
    end;
  end;
  if Last <> nil then
    Result := Abs((Longint(PAnsiChar(S)) - Longint(Last)) div SizeOf(AnsiChar)) + 1;
end;

//------------------------------------------------------------------------------

function StrMatch(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and EBP # of chars in Substr to compare 

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX & ECX

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     ECX, ECX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     CL, [EDI]
        INC     EDI

        // lower case it

        MOV     CL, [EBX + ECX]

@@FindNext:

        // get the current char from Str into al

        MOV     AL, [ESI]
        INC     ESI

        // check the end of AnsiString

        TEST    AL, AL
        JZ      @@NotFound


        CMP     CL, '*'    // Wild Card?
        JE      @@Compare

        CMP     CL, '?'    // Wild Card?
        JE      @@Compare

        // lower case current char

        MOV     AL, [EBX + EAX]

        // check if the current char matches the primary search char,
        // if not continue searching

        CMP     AL, CL
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare }

        MOV     EDX, EBP

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [EDI + EDX]               // char from  Substr

        CMP     AL, '*'                     // wild card?
        JE      @@CompareNext

        CMP     AL, '?'                     // wild card?
        JE      @@CompareNext

        CMP     AL, [ESI + EDX]               // equal to PChar(Str)^ ?
        JE      @@CompareNext

        MOV     AL, [EBX + EAX + AnsiReOffset]  // reverse case?
        CMP     AL, [ESI + EDX]
        JNE     @@FindNext                  // if still no, go back to the main loop

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//------------------------------------------------------------------------------

function StrPrefixIndex(const S: string; const Prefixes: array of string): Integer;
var
  I: Integer;
  Test: string;
begin
  Result := -1;
  for I := Low(Prefixes) to High(Prefixes) do
  begin
    Test := StrLeft(S, Length(Prefixes[I]));
    if AnsiSameText(Test, Prefixes[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function StrSearch(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // ebp will hold # of chars in Substr to compare, esi pointer to Str,
        // edi pointer to Substr, ebx primary search char

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the (Index - 1) in edx

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // # of positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // point Str to Index'th char

        ADD     ESI, EDX

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // clear EAX & ECX (working regs)

        XOR     EAX, EAX
        XOR     EBX, EBX

        // bring the first char out of the Substr, and
        // point Substr to the next char

        MOV     BL, [EDI]
        INC     EDI

        // jump into the loop

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of AnsiString.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the AnsiString, and /point Str to the next one.
        MOV     AL, [ESI]
        INC     ESI

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, BL
        JNE     @@FindNext

        // otherwise compare SubStr

@@Compare:

        // move # of char to compare into edx, edx will be our compare loop counter.

        MOV     EDX, EBP

@@CompareNext:

        // check if we reached the end of Substr. If yes we found it.

        DEC     EDX
        JL      @@Found

        // get last chars from Str and SubStr and compare them,
        // if they don't match go back to out main loop.

        MOV     AL, [EDI+EDX]
        CMP     AL, [ESI+EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result and exit.

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:
        // not found it, clear result and exit.

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:
        // clear result and exit.

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//==============================================================================
// String Extraction
//==============================================================================

function StrAfter(const SubStr, S: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := StrFind(SubStr, S); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

//------------------------------------------------------------------------------

function StrBefore(const SubStr, S: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := StrFind(SubStr, S);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;

//------------------------------------------------------------------------------

function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
var
  PosStart, PosEnd: Integer;
  L: Integer;
begin
  PosStart := Pos(Start, S);
  PosEnd := Pos(Stop, S);
  if (PosStart > 0) and (PosEnd > PosStart) then
  begin
    L := PosEnd - PosStart;
    Result := Copy(S, PosStart + 1, L - 1);
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function StrChopRight(const S: AnsiString; N: Integer): AnsiString;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

//------------------------------------------------------------------------------

function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, 1, Count);
end;

//------------------------------------------------------------------------------

function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
begin
  Result := Copy(S, Start, Count);
end;

//------------------------------------------------------------------------------

function StrRestOf(const S: AnsiString; N: Integer ): AnsiString;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

//------------------------------------------------------------------------------

function StrRight(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

//==============================================================================
// Character
//==============================================================================

function CharEqualNoCase(const C1, C2: AnsiChar): Boolean;
begin
//if they are not equal chars, may be same letter different case
  Result := (C1 = C2) or
    (CharIsAlpha(C1) and CharIsAlpha(C2) and (CharLower(C1) = CharLower(C2)));
end;

//------------------------------------------------------------------------------

function CharIsAlpha(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsAlphaNum(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or
    ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

//------------------------------------------------------------------------------

function CharIsBlank(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_BLANK) <> 0);
end;

//------------------------------------------------------------------------------

function CharIsControl(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_CNTRL) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsDelete(const C: AnsiChar): Boolean;
begin
  Result := (C = #8);
end;

//------------------------------------------------------------------------------

function CharIsDigit(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsLower(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_LOWER) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsNumber(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or
    (C in AnsiSigns) or (C = DecimalSeparator);
end;

//------------------------------------------------------------------------------

function CharIsPrint(const C: AnsiChar): Boolean;
begin
  Result := not CharIsControl(C);
end;

//------------------------------------------------------------------------------

function CharIsPunctuation(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_PUNCT) <> 0);
end;

//------------------------------------------------------------------------------

function CharIsReturn(const C: AnsiChar): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;

//------------------------------------------------------------------------------

function CharIsSpace(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_SPACE) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsUpper(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_UPPER) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsWhiteSpace(const C: AnsiChar): Boolean;
begin
  Result := C in AnsiWhiteSpace;
end;

//------------------------------------------------------------------------------

function CharType(const C: AnsiChar): Word;
begin
  Result := AnsiCharTypes[C];
end;

{$IFDEF SUPPORTS_DYNAMICARRAYS}

//==============================================================================
// PCharVector
//==============================================================================

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
var
  I: Integer;
  S: AnsiString;
  List: array of PChar;
begin
  Assert(Source <> nil);
  Dest := AllocMem((Source.Count + SizeOf(AnsiChar)) * SizeOf(PChar));
  SetLength(List, Source.Count + SizeOf(AnsiChar));
  for I := 0 to Source.Count - 1 do
  begin
    S := Source[I];
    List[I] := StrAlloc(Length(S) + SizeOf(AnsiChar));
    StrPCopy(List[I], S);
  end;
  List[Source.Count] := nil;
  Move(List[0], Dest^, (Source.Count + 1) * SizeOf(PChar));
  Result := Dest;
end;

//------------------------------------------------------------------------------

function PCharVectorCount(const Source: PCharVector): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source^;
    while P <> nil do
    begin
      Inc(Result);
      P := PCharVector(Longint(Source) + (SizeOf(PChar) * Result))^;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure PCharVectorToStrings(const Dest: TStrings; const Source: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  Assert(Dest <> nil);
  if Source <> nil then
  begin
    Count := PCharVectorCount(Source);
    SetLength(List, Count);
    Move(Source^, List[0], Count * SizeOf(PChar));
    Dest.Clear;
    for I := 0 to Count - 1 do
      Dest.Add(List[I]);
  end;
end;

//------------------------------------------------------------------------------

procedure FreePCharVector(var Dest: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  if Dest <> nil then
  begin
    Count := PCharVectorCount(Dest);
    SetLength(List, Count);
    Move(Dest^, List[0], Count * SizeOf(PChar));
    for I := 0 to Count - 1 do
      StrDispose(List[I]);
    FreeMem(Dest, (Count + 1) * SizeOf(PChar));
    Dest := nil;
  end;
end;

{$ENDIF}

//==============================================================================
// Character Transformation Routines
//==============================================================================

function CharHex(const C: AnsiChar): Byte;
begin
  Result := $FF;
  if C in AnsiDecDigits then
    Result := Ord(CharUpper(C)) - 48
  else
  begin
    if C in AnsiHexDigits then
      Result := Ord(CharUpper(C)) - 55;
  end;
end;

//------------------------------------------------------------------------------

function CharLower(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiLoOffset];
end;

//------------------------------------------------------------------------------

function CharToggleCase(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiReOffset];
end;

//------------------------------------------------------------------------------

function CharUpper(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiUpOffset];
end;

//==============================================================================
// Character Search and Replace
//==============================================================================

function CharPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  if (Index > 0) and (Index < Length(S)) then
  begin
    P := PAnsiChar(S);
    Inc(P, Index - 1);
    while P^ <> #0 do
    begin
      Inc(Result);
      if P^ = C then
        Break;
      Inc(P);
    end;
    if P^ = #0 then
      Result := 0;
  end;
end;

//------------------------------------------------------------------------------

function CharIPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer;
var
  P: PAnsiChar;
  CU: AnsiChar;
begin
  Result := 0;
  if (Index > 0) and (Index < Length(S)) then
  begin
    CU := CharUpper(C);
    P := PAnsiChar(S);
    Inc(P, Index - 1);
    while P^ <> #0 do
    begin
      Inc(Result);
      if AnsiCaseMap[Ord(P^) + AnsiUpOffset] = CU then
        Break;
      Inc(P);
    end;
    if P^ = #0 then
      Result := 0;
  end;
end;

//------------------------------------------------------------------------------

function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): Integer; assembler;
var
  P: PAnsiChar;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    P := PAnsiChar(S);
    while P^ <> #0 do
    begin
      if P^ = Search then
      begin
        P^ := Replace;
        Inc(Result);
      end;
      Inc(P);
    end;
  end;
end;

//==============================================================================
// MultiSz
//==============================================================================

function StringsToMultiSz(var Dest: PChar; const Source: TStrings): PChar;
var
  I, TotalLength: Integer;
  P: PChar;
begin
  Assert(Source <> nil);
  TotalLength := 0;
  for I := 0 to Source.Count - 1 do
    Inc(TotalLength, Length(Source[I]) + SizeOf(AnsiChar));
  Dest := AllocMem(TotalLength + SizeOf(AnsiChar));
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopy(P, PChar(Source[I]));
    Inc(P);
  end;
  Result := Dest;
end;

//------------------------------------------------------------------------------

procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
var
  P: PChar;
begin
  Assert(Dest <> nil);
  Dest.Clear;  
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do
    begin
      Dest.Add(P);
      P := StrEnd(P);
      Inc(P);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure FreeMultiSz(var Dest: PChar);
begin
  FreeMem(Dest);
  Dest := nil;
end;

//==============================================================================
// TStrings Manipulation
//==============================================================================

procedure StrToStrings(S: AnsiString; Sep: AnsiString; const List: TStrings);
var
  I, L: Integer;
  Left: AnsiString;
begin
  Assert(List <> nil);
  List.Clear;
  L := Length(Sep);
  I := Pos(Sep, S);
  while (I > 0) do
  begin
    Left := StrLeft(S, I - 1);
    List.Add(Left);
    Delete(S, 1, I + L - 1);
    I := Pos(Sep, S);
  end;
  if S <> '' then
    List.Add(S);
end;

//------------------------------------------------------------------------------

function StringsToStr(const List: TStrings; Sep: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    Result := Result + List[I];
    Result := Result + Sep;
  end;
  // remove last separator, doing this afterwards saves an if in the loop
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

//------------------------------------------------------------------------------

procedure TrimStrings(const List: TStrings);
var
  I: Integer;
begin
  Assert(List <> nil);
  for I := List.Count - 1 downto 0 do
  begin
    List[I] := Trim(List[I]);
    if List[I] = '' then
      List.Delete(I);
  end;
end;

//------------------------------------------------------------------------------

procedure TrimStringsRight(const List: TStrings);
var
  I: Integer;
begin
  I := List.Count - 1;
  while I >= 0 do
  begin
    // remove spaces at the end
    List[I] := TrimRight(List[I]);
    // empty line
    if List[I] = '' then
    begin
      // delete it & try the previous line
      List.Delete(I);
      Dec(I);
    end
    else
    begin
      // last line not empty - we are done
      I := -1;
    end;
  end;
end;

//==============================================================================
// Miscelanuous
//==============================================================================

function BooleanToStr(B: Boolean): string;
const
  Bools: array [Boolean] of PChar = ('False', 'True');
begin
  Result := Bools[B];
end;


//------------------------------------------------------------------------------

function FileToString(const FileName: AnsiString): AnsiString;
var
  F: File;
  Size: Integer;
  Buffer: Pointer;
begin
  Assert(FileExists(FileName));
  AssignFile(F, FileName);
  Reset(F, 1);
  try
    Size := FileSize(F);
    SetLength(Result, Size);
    Buffer := PChar(Result);
    BlockRead(F, Buffer^, Size);
  finally
    CloseFile(F);
  end;
end;

//------------------------------------------------------------------------------

procedure StringToFile(const FileName, Contents: AnsiString);
var
  F: File;
  Size: Integer;
  Buffer: Pointer;
begin
  AssignFile(F, FileName);
  Rewrite(F, 1);
  try
    Size := Length(Contents);
    Buffer := PChar(Contents);
    BlockWrite(F, Buffer^, Size);
  finally
    CloseFile(F);
  end;
end;

//------------------------------------------------------------------------------

function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
var
  I: Integer;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

//------------------------------------------------------------------------------

procedure StrTokens(const S: AnsiString; const List: TStrings);
var
  Start: PChar;
  Token: string;
  Done: Boolean;
begin
  Assert(List <> nil);
  List.Clear;
  Start := Pointer(S);
  repeat
    Done := StrWord(Start, Token);
    if Token <> '' then
      List.Add(Token);
  until Done;
end;

//------------------------------------------------------------------------------

procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; const List: TStrings);
var
  Token: AnsiString;
begin
  Assert(List <> nil);
  List.Clear;
  Token := StrToken(S, Separator);
  while Token <> '' do
  begin
    List.Add(Token);
    Token := StrToken(S, Separator);
  end;
end;

//------------------------------------------------------------------------------

function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean;
var
  Start: PAnsiChar;
begin
  Word := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := False;
  while True do
  begin
    case S^ of
      #0:
        begin
          if Start <> nil then
            SetString(Word, Start, S - Start);
          Result := True;
          Exit;
        end;
      AnsiSpace, AnsiLineFeed, AnsiCarriageReturn:
        begin
          if Start <> nil then
          begin
            SetString(Word, Start, S - Start);
            Exit;
          end
          else
            while (S^ in [AnsiSpace, AnsiLineFeed, AnsiCarriageReturn]) do
              Inc(S);
        end;
    else
      if Start = nil then
        Start := S;
      Inc(S);
    end;
  end;
end;

//==============================================================================
// Initialization
//==============================================================================

initialization

  LoadCharTypes;  // this table first
  LoadCaseMap;    // or this function does not work

end.

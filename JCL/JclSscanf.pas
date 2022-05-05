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
{ The Original Code is JclSscanf.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: July 12, 2000                                                 }
{                                                                              }
{******************************************************************************}

unit JclSscanf;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils,
  JclBase;

{*****************************************************************************
(C) Clinton R. Johnson January 27, 1999.
You may freely re-use and re-distribute this source code.

Warning: My document writing skills are a little rusty.  Any C or C++
reference provides excellent documentation for the scanf function.  There
are only a few differences between the C and C++ implementation, and this
implementation for Delphi.

Sscanf is a data parsing procedure. It reads data from DATA, and places the
values into arguments passed in ARGS. Format is a series of format specifiers
that indicate how to parse data from DATA.

Format:
  Format Specifiers
  White Space characters
  Non White Space characters.

Format Specifiers:

Format specifiers start with a %, contain an optional storage specifier, an
optional width specifier, and a field type specifier.  Strings and characters
also have an additional SET specifer. Text in the format string, which is not a
format specifier, is LITERAL text. It must match exactly, AND IS CASE SENSITIVE.

The only exception for format specifiers is %%, which matches a literal % instead of
indicating a format specifier.

  Storage Specifier:
    *  - If * is included in the format specifier, the
         field is parsed, checked for type but is not
         stored.

  Width Specifier:
    123 - If a numeric value is included between the
          % and the type specifier, the value indicates
          the maximum length of the data read from the
          string.  For character types, it is expected
          that the pointer passed points at an array
          large enough to hold the data.

  Set Specifier:
    [ab-c]  - A set of characters which are acceptable in
              the string or char. This overrides the normal white
              space detection used to indicate the end of a string or
              array of chars.
              Set members are listed without break.
              Ranges can be indicated with a hyphen. Ranges MUST be
              presented in ascending order (a-z is valid, z-a is NOT).

              *** UNLIKE C, \ is used to indicate a literal character.
              \[ - [
              \\ - \
              \] - ]
              \- - -

              Dashes which are the first or last character of
              the set are interpreted literally as a member of the set.

              If the first character of the set is a ^, then the set is
              inverted and members are EXCLUDED from the set.

              ie:
                  []     DEFAULT SET- Accepts anything except whitespace.
                  [abc]  accepts a, b or c.
                  [^abc] accepts anything EXCEPT a, b or c.
                  [^]    accepts anything.  ** NOTE, this differs from C,C++.
                  [a-cA-C]  Accepts a,b,c,A,B, or C

              WARNING: SETS ARE CASE SENSITIVE.

              NOTE:  C and C++ use the %[] format without type specifier
                     implies a CHAR type. FOR THIS IMPLEMENTATION,
                     NO TYPE SPECIFIER IMPLIES A STRING TYPE. The only
                     other valid specifer is c for char.
              ie:
                    %[abc]    -> String set
                    %[abc]c   -> char set, width of 1.
                    %4[abc]c  -> char set, max width of 4.
                    %[abc]s   -> String set, followed by a literal s.

 TYPE SEPECIFIERS

Type | Data Type     | REQUIRED DATA TYPE
-----+---------------+-------------------
  c  | Char          | char
  d  | INTEGER       | Integer (32 bit, signed)
  e  | Extended      | Extended
  f  | Extended      | Extended
  g  | Extended      | Extended
  h  | ShortInt      | ShortInt (16 bit, signed)
  i  | Integer       | Int64 (64 bit, signed)
  m  | Money         | Currency (may not contain currency symbol, must use standard +- sign indications)
  n  | Number Read   | Integer -> Returns the # of chars read from DATA so far.
  p  | Pointer       | Pointer  (32 bit pointer always)
  s  | String        | Ansistring or shortstring
  u  | Unsigned      | Cardinal  (32bit)
  x  | Hex           | Integer or Cardinal (32bit value)

Integers can be Hex values. Hex values can start with $ or 0x. Hex values read with
the %x format specifier do not need a leading $ or 0x, but it is accepted.

ARGUMENTS
  All variables, with the exception of shortstrings are passed
  by reference (pointers). This means that the risk of
  data corruption is high. You must be absolutely sure you are
  passing a pointer to the correct data type, otherwise data
  corruption is likely (ie: passing a pointer to a Integer, when
  the return result is an array of 20 characters, or an extended).

  In order to pass a variable by reference, precede the variable
  name with an @ sign.

  ie:

  var S1, S2: String; i1: Integer;

       Sscanf('Hello, World! 22', '%s%s%d', [@s1, @s2, @i1])

  NOTE:  It is highly recommended that you always use AnsiStrings
         instead of ShortStrings.

  NOTE:  NEVER pass shortstrings by reference, pass the variable
         without a preceeding @.

  var S1: ShortString; S2: String;

       Sscanf('Hello, World!', '%s%s', [s1, @s2])

Returns: The # of results successfully assigned.
         Unlike C and C++, this implementation never returns EOF (-1).

*****************************************************************************}

function Sscanf(const Data: string; const Format: string;
  const Args: array of const): Integer;

type
  EJclSscanfError = class (EJclError);

implementation

uses
  // (rom) to use the better AnsiWhiteSpace which contains
  // also VerticalTab and FormFeed
  JclResources, JclStrings;

type
  TFieldType = (ftChar, ftSmallInt, ftInteger, ftInt64, ftFloating,
                ftCurrency, ftString, ftHex, ftPointer, ftCount,
                ftUnsigned, ftLiteral, ftWhiteSpace);

//------------------------------------------------------------------------------

procedure CreateSet(Source: string; var ResultSet: TSysCharSet);
var
  At: Integer;
  Token: Char;
  EndToken: Char;
  LoopToken: Char;
  Negate: Boolean;

  function GetToken(IsLiteral: Boolean): Char;
  begin
    if At <= Length(Source) then
    begin
      Result := Source[At];
      Inc(At);
      if not IsLiteral then
      begin
        if Result = '\' then  // Literal character.
          Result := GetToken(True);
// Provides support for embedded control characters with the standard Pascal ^a format.
// Removed because ^ has a special meaning for scanf sets. Kept for future reference.
(*
        else
        if Result ='^' then
        begin
          Result := UpCase(GetToken(True));
          if not Result in [#64..#95] then
            raise EJclSscanfError.Create(RsSscanfBadSet + Source);
          Result := Char(Byte(Result)-64);
        end;
*)
      end;
    end
    else
      raise EJclSscanfError.Create(RsSscanfBadSet + Source);
  end;

begin
  ResultSet := [];
  At := 1;
  Negate := (Copy(Source, 1, 1) = '^');
  if Negate then
  begin
    Delete(Source, 1, 1);
    ResultSet := [#0..#255];
  end;
  while At <= Length(Source) do
  begin
    Token := GetToken(False);
    EndToken := Token;

    if (Copy(Source, At, 1) = '-') and (At <> Length(Source)) then  // This is a range.
    begin
      Inc(At, 1);  // Go past dash.

      if At > Length(Source) then
        raise EJclSscanfError.Create(RsSscanfBadSet + Source);
      EndToken := GetToken(False);
    end;

    if EndToken < Token then  // Z-A is probably the result of a missing letter.
      raise EJclSscanfError.Create(RsSscanfBadSet + Source);

    if Negate then
      for LoopToken := Token to EndToken do
        Exclude(ResultSet, LoopToken)
    else
      for LoopToken := Token to EndToken do
        Include(ResultSet, LoopToken);
  end;
end;

//------------------------------------------------------------------------------

function GetToken(const Str: string; var At: Integer): Char;
begin
  if At <= Length(Str) then
  begin
    Result := Str[At];
    Inc(At);
  end
  else
  begin
    At := Length(Str)+1;
    raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
  end;
end;

//------------------------------------------------------------------------------

function PeekToken(const Str: string; const At: Integer): Char;
begin
  if At <= Length(Str) then
    Result := Str[At]
  else
    Result := #0;
end;

//------------------------------------------------------------------------------

function GetScanfToken(const Format: string; var At: Integer): string;
var
  Token: Char;
  TokenDone: Boolean;
  BuildingSet: Boolean;
begin
  Token := GetToken(Format, At);
  if Token = '%' then
  begin
    Result := Token;
    BuildingSet := False;
    TokenDone := False;
    repeat
      Token := GetToken(Format, At);
      Result := Result+Token;
      if (Token = '\') and BuildingSet then
      begin
        Token := GetToken(Format, At);
        Result := Result+Token;
      end
      else
      if Token = '[' then
        BuildingSet := True
      else
      if Token = ']' then
      begin
        BuildingSet := False;
        Token := PeekToken(Format, At);
        if Token in ['C', 'c'] then
        begin
          Token := GetToken(Format, At);
          Result := Result+Token;
        end;
        TokenDone := True;
      end
      else
      if (Token in ['*', '0'..'9']) or BuildingSet then
      begin
        // Data is accepted and added to the tag.
      end
      else
        TokenDone := True;
    until TokenDone;
  end
  else
  begin
    Result := Token;
    while Token in AnsiWhiteSpace do
    begin
      Token := PeekToken(Format, At);
      if Token in AnsiWhiteSpace then
        Token := GetToken(Format, At);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure InterpretScanToken(ScanToken: string;
  var FieldType: TFieldType; var FieldWidth: Integer;
  var CharSet: TSysCharSet; var Stored: Boolean);
const
  NumberSet = ['0'..'9', '-'];
  HexSet = ['0'..'9', '$', 'x', 'X', '-'];
var
  TokenChar: Char;
  At: Integer;
  EndAt: Integer;
  Frag: string;
  Token: Char;
begin
  CharSet := [];
  if Copy(ScanToken, 1, 1) = '%' then
  begin
    Delete(ScanToken, 1, 1);
    TokenChar := ScanToken[Length(ScanToken)];
    if TokenChar <> ']' then
      Delete(ScanToken, Length(ScanToken), 1)
    else
      TokenChar := 's';
    At := Pos('[', ScanToken);
    if At <> 0 then
    begin
      EndAt := At;
      Token := GetToken(ScanToken, EndAt);
      while Token <> ']' do
      begin
        Token := GetToken(ScanToken, EndAt);
        if Token = '\' then
        begin
          Token := GetToken(ScanToken, EndAt);
          if Token = ']' then
            Token := #0; // Skip Literal ]'s.
        end;
      end;
      Dec(EndAt);
      Frag := Copy(ScanToken, At+1, EndAt-At-1);
      Delete(ScanToken, At, EndAt-At+1);
      CreateSet(Frag, CharSet);
    end
    else
      CharSet := [];
    At := Pos('*', ScanToken);
    Stored := (At = 0);
    if not Stored then
      Delete(ScanToken, At, 1);
    if ScanToken <> '' then
    begin
      try
        FieldWidth := StrToInt(ScanToken);
      except
        raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
      end;
    end
    else
      FieldWidth := -1;
    case TokenChar of
      'c':
        begin
          FieldType := ftChar;
          if FieldWidth = -1 then
            FieldWidth := 1;
        end;
      'd':
        begin
          FieldType := ftInteger;
          CharSet := ['0'..'9', '-'];
        end;
      'e', 'f', 'g':
        begin
          FieldType := ftFloating;
          CharSet := ['0'..'9', '.', '+', '-', 'E', 'e'];
        end;
      {$IFDEF SUPPORTS_INT64}
      'i':
        begin
          FieldType := ftInt64;
          CharSet := NumberSet;
        end;
      {$ENDIF}
      'h':
        begin
          FieldType := ftSmallInt;
          CharSet := NumberSet;
        end;
      'm':
        begin
          FieldType := ftCurrency;
          CharSet := NumberSet;
        end;
      's':
        begin
           FieldType := ftString;
        end;
      'x':
        begin
          FieldType := ftHex;
          CharSet := HexSet;
        end;
      'p':
        begin
          FieldType := ftPointer; // All pointers are 32 bit.
          CharSet := HexSet;
        end;
      'u':
        begin
          FieldType := ftUnsigned;
          CharSet := ['0'..'9'];
        end;
      '%':
        begin
          FieldType := ftLiteral;
          if (FieldWidth <> -1) or (not Stored) or (CharSet <> []) then
            raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
          Stored := False;
        end;
    else
      raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
    end;
  end
  else
  begin
    if ScanToken[1] in AnsiWhiteSpace then
    begin
      FieldType := ftWhiteSpace;
      FieldWidth := -1;
      Stored := False;
      CharSet := [];
    end
    else
    begin
      FieldType := ftLiteral;
      FieldWidth := Length(ScanToken);
      Stored := False;
      CharSet := [];
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetDataToken(const Data: string; var DataAt: Integer;
  var OutOfData: Boolean; const FieldType: TFieldType;
  const FieldWidth: Integer; CharSet: TSysCharSet): string;
var
  Token: Char;
begin
  OutOfData := False;
  Result := '';
  if FieldType = ftChar then
  begin
    try
      GetToken(Data, DataAt);
    except
      OutOfData := True;
      Exit; // Hit End of string.
    end;
  end
  else
  begin
    Token := #32;
    while Token in AnsiWhiteSpace do
    begin
      try
        Token := GetToken(Data, DataAt);
      except
        OutOfData := True;
        Exit; // Hit end of string.
      end;
    end;
    if FieldType = ftWhiteSpace then
    begin
      Dec(DataAt);  // Unget the last token.
      Exit;
    end;
  end;

  if CharSet = [] then
  begin
    CharSet := [#0..#255];
    if FieldType <> ftChar then
      CharSet := CharSet-AnsiWhiteSpace;
  end;

  Dec(DataAt);  // Unget the last token.
  while (FieldWidth = -1) or (Length(Result) < FieldWidth) do // Check Length
  begin
    Token := PeekToken(Data, DataAt);
    if Token in CharSet then
    begin
      try
        Token := GetToken(Data, DataAt);
        Result := Result+Token;
      except
        Break;
      end;
    end
    else
      Break;
  end;
end;

//------------------------------------------------------------------------------

function CheckFieldData(Data: string; const FieldType: TFieldType): Boolean;
begin
  try
    case FieldType of
//      ftChar:
//        begin
//        end;
      ftSmallInt:
        StrToInt(Data);
      ftInteger:
        StrToInt(Data);
      {$IFDEF SUPPORTS_INT64}
      ftInt64:
        StrToInt64(Data);
      {$ENDIF}
      ftFloating:
        StrToFloat(Data);
      ftCurrency:
        StrToFloat(Data);
//      ftString:
//        begin
//        end;
      ftHex:
        begin
          if AnsiUpperCase(Copy(Data, 1, 2)) = '0X' then
            Delete(Data, 1, 2);
          if Copy(Data, 1, 1) <> '$' then
            Data := '$'+Data;
          StrToInt(Data);
        end;
      ftPointer:
        StrToInt(Data);
//      ftCount:
//        begin
//        end;
      ftUnsigned:
        StrToInt(Data);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function StoreData(Data: string; const FieldType: TFieldType;
  const FieldWidth: Integer; P: Pointer; VType: Integer): Boolean;
begin
  try
    case FieldType of
      ftChar:
        Move(Data[1], P^, FieldWidth);
      ftSmallInt:
        SmallInt(P^) := StrToInt(Data);
      ftInteger:
        Integer(P^) := StrToInt(Data);
      {$IFDEF SUPPORTS_INT64}
      ftInt64:
        Int64(P^) := StrToInt64(Data);
      {$ENDIF}
      ftFloating:
        Extended(P^) := StrToFloat(Data);
      ftCurrency:
        Currency(P^) := StrToFloat(Data);
      ftString:
        begin
          if VType = vtString then
            ShortString(P^) := Data
          else
            string(P^) := Data;
        end;
      ftHex:
        begin
          if AnsiUpperCase(Copy(Data, 1, 2)) ='0X' then
            Delete(Data, 1, 2);
          if Copy(Data, 1, 1) <> '$' then
            Data := '$'+Data;
          Integer(P^) := StrToInt(Data);
        end;
      ftPointer:
        Integer(P^) := StrToInt(Data);
      ftCount:
        begin
        end;
      ftUnsigned:
        Cardinal(P^) := StrToInt(Data);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function Sscanf(const Data: string; const Format: string;
  const Args: array of const): Integer;
var
  At: Integer;
  DataAt: Integer;
  Results: Integer;
  ScanToken: string;
  StringToken: string;
  FieldType: TFieldType;
  FieldWidth: Integer;
  CharSet: TSysCharSet;
  Stored: Boolean;
  OutOfData: Boolean;
  P: Pointer;
begin
  At := 1;
  DataAt := 1;
  Results := 0;

  while At <= Length(Format) do
  begin
    try
      ScanToken := GetScanfToken(Format, At);
      InterpretScanToken(ScanToken, FieldType, FieldWidth, CharSet, Stored);
    except
      Break;
    end;
    if FieldType <> ftCount then
      StringToken := GetDataToken(Data, DataAt, OutOfData, FieldType, FieldWidth, CharSet);

    if FieldType = ftLiteral then
      if StringToken <> ScanToken then
        Break;

    if not OutOfData then
    begin
      if not CheckFieldData(StringToken, FieldType) then
        Break;
      if Stored then
      begin
        if (Results+Low(Args)) > High(Args) then
          raise EJclSscanfError.CreateResRec(@RsSscanfInsufficient);
        P := Args[Results+Low(Args)].VPointer;
        StoreData(StringToken, FieldType, FieldWidth, P, Args[Results+Low(Args)].VType);
        Inc(Results);
      end;
    end;
  end;
  Result := Results;
end;

end.

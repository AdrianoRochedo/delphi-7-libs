{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           System Utility Classes and Functions          }
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

unit ZSysUtils;

interface

{$I ZCore.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZCompatibility, Classes, SysUtils;

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(Delimiters: string; Str: string): Integer;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(Delimiters: string; Str: string): Integer;

{**
  Checks is the string starts with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(Str: string; SubStr: string): Boolean;

{**
  Checks is the string ends with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(Str: string; SubStr: string): Boolean;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SqlStrToFloatDef(Str: string; Def: Extended): Extended;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a string retrived from the buffer.
}
function BufferToStr(Buffer: PChar; Length: LongInt): string;

{**
  Converts a string into boolean value.
  @param Str a string value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: string): Boolean;

{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(Str: string): Boolean;

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(Str, Delimiters: string): TStrings;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; Str, Delimiters: string);

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; Str, Delimiters: string);

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; Delimiter: string): string;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSqlStr(Value: Extended): string;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; Str, Delimiter: string);

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(Str, Delimiter: string): TStrings;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; Str, Delimiter: string);

{**
  Converts bytes into a string representation.
  @param Value an array of bytes to be converted.
  @return a converted string.
}
function Bytes2Str(Value: TByteDynArray): string;

{**
  Converts string into an array of bytes.
  @param Value a string to be converted.
  @return a converted array of bytes.
}
function Str2Bytes(Value: string): TByteDynArray;

{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function Bytes2Var(Value: TByteDynArray): Variant;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function Var2Bytes(Value: Variant): TByteDynArray;

implementation

uses ZMatchPattern;

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(Delimiters: string; Str: string): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  for I := 1 to Length(Delimiters) do
  begin
    Index := Pos(Delimiters[I], Str);
    if (Index > 0) and ((Index < Result) or (Result = 0)) then
      Result := Index;
  end;
end;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(Delimiters: string; Str: string): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  for I := Length(Str) downto 1 do
  begin
    Index := Pos(Str[I], Delimiters);
    if (Index > 0) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Checks is the string starts with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(Str: string; SubStr: string): Boolean;
begin
  Result := Copy(Str, 1, Length(SubStr)) = SubStr;
end;

{**
  Checks is the string ends with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(Str: string; SubStr: string): Boolean;
begin
  if Length(SubStr) <= Length(Str) then
  begin
    Result := Copy(Str, Length(Str) - Length(SubStr) + 1,
    Length(SubStr)) = SubStr;
  end else
    Result := False;
end;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SqlStrToFloatDef(Str: string; Def: Extended): Extended;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  Result := StrToFloatDef(Str, Def);
  DecimalSeparator := OldDecimalSeparator;
end;

{ Convert string buffer into pascal string }
function BufferToStr(Buffer: PChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
  begin
    Result := Buffer;
    SetLength(Result, Length);
//    SetString(Temp, Buffer, Length);
  end;
end;

{**
  Converts a string into boolean value.
  @param Str a string value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: string): Boolean;
begin
  Str := UpperCase(Str);
  Result := (Str = 'Y') or (Str = 'YES') or (Str = 'T') or (Str = 'TRUE')
    or (StrToIntDef(Str, 0) <> 0);
end;

{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(Str: string): Boolean;
var
  I, N, M, Pos: Integer;
begin
  if IsMatch('[0-9]*.[0-9]*.[0-9]*.[0-9]*', Str) then
  begin
    N := 0;
    M := 0;
    Pos := 1;
    for I := 1 to Length(Str) do
    begin
      if I - Pos > 3 then
        Break;
      if Str[I] = '.' then begin
       if StrToInt(Copy(Str, Pos, I - Pos)) > 255 then
         Break;
       Inc(N);
       Pos := I + 1;
      end;
      if Str[I] in ['0'..'9'] then Inc(M);
    end;
    Result := (M + N = Length(Str)) and (N = 3);
  end else
    Result := False;
end;

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(Str, Delimiters: string): TStrings;
var
  DelimPos: Integer;
begin
  Result := TStringList.Create;
  repeat
    DelimPos := FirstDelimiter(Delimiters, Str);
    if DelimPos > 0 then
    begin
      if DelimPos > 1 then
        Result.Add(Copy(Str, 1, DelimPos - 1));
      Str := Copy(Str, DelimPos + 1, Length(Str) - DelimPos);
    end else
      Break;
  until DelimPos <= 0;
  if Str <> '' then
    Result.Add(Str);
end;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; Str, Delimiters: string);
var
  Temp: TStrings;
begin
  Temp := SplitString(Str, Delimiters);
  try
    List.Assign(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; Str, Delimiters: string);
var
  Temp: TStrings;
begin
  Temp := SplitString(Str, Delimiters);
  try
    List.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; Delimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + List[I];
  end;
end;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSqlStr(Value: Extended): string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  Result := FloatToStr(Value);
  DecimalSeparator := OldDecimalSeparator;
end;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; Str, Delimiter: string);
var
  Temp: Tstrings;
begin
  Temp := SplitStringEx(Str, Delimiter);
  try
    List.Clear;
    List.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(Str, Delimiter: string): TStrings;
var
 Pos: integer;
 TmpStr: string;
begin
  Result := TStringList.Create;
  TmpStr := Str;
  repeat
    Pos := AnsiPos(Delimiter, TmpStr);
    Result.Add(Copy(TmpStr, 1, Pos-1));
    Delete(TmpStr, 1, Pos + Length(Delimiter)-1);
  until Pos = 0;
  if TmpStr <> '' then
    Result.Add(TmpStr);
end;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; Str, Delimiter: string);
var
  Temp: Tstrings;
begin
  Temp := SplitStringEx(Str, Delimiter);
  try
    List.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Converts bytes into a string representation.
  @param Value an array of bytes to be converted.
  @return a converted string.
}
function Bytes2Str(Value: TByteDynArray): string;
var
  I: Integer;
begin
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do
    Result[I] := Char(Value[I - 1]);
end;

{**
  Converts string into an array of bytes.
  @param Value a string to be converted.
  @return a converted array of bytes.
}
function Str2Bytes(Value: string): TByteDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do
    Result[I - 1] := Ord(Value[I]);
end;

{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function Bytes2Var(Value: TByteDynArray): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  for I := 0 to Length(Value) - 1 do
    Result[I] := Value[I - 1];
end;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function Var2Bytes(Value: Variant): TByteDynArray;
var
  I: Integer;
begin
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte)) then
    raise Exception.Create('Invalid Var Byte Array');

  SetLength(Result, VarArrayHighBound(Value, 1) + 1);
  for I := 0 to VarArrayHighBound(Value, 1) do
    Result[I] := Value[I];
end;

end.

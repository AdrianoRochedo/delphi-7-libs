{$I STDEFINE.INC}

{$A-} {Packed records}
{$B-} {Incomplete boolean evaluation}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF OS32}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                    STSTRB.PAS 1.05                    *}
{*            Bonus Character and String Routines        *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

(*
  This unit is provided as an extra value to the SysTools package.  It
  contains some character and string routines that we thought might be
  useful to some of our customers...most are pretty simple, and some are
  merely wrappers around Windows API calls.  TurboPower's support of this
  unit is limited to bug fixes only.  Source code comments are currently
  the only documentation.  This unit has not been tested under Virtual
  Pascal.  TurboPower Software offers no warranty, express or implied,
  for these BONUS routines. You may use the routines within any
  application that you write, without restriction.
*)

unit StStrB;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinProcs,
  WinTypes,
{$ENDIF}
  SysUtils,
  StConst,
  StBase,
  StStrS,
  StStrZ;

const
  FileNameSet : string = '\/:*? <>|';
  PathNameSet : string = '/*? <>|';
  NumberSet   : string = '1234567890';
  HexCharSet  : string = 'abcdefABCDEF0123456789';
  WildCharSet : string = '*?';
  DriveSet    : string = 'abcdefghijklmnopqrstuvwxyz';

{ Character Routines }

function IsAlpha(C : AnsiChar) : Boolean;
  { Returns True if C is an alpha character (by Windows definition)}

function IsAlphaNum(C : AnsiChar) : Boolean;
  { Returns True if C is an alphanumeric character (by Windows definition)}

function IsCtrl(C : AnsiChar) : Boolean;
  { Returns True if C is a control character }

function IsDrive(C : AnsiChar) : Boolean;
  { Returns True if C is a valid drive letter (does not check if drive exists)}

function IsFileName(C : AnsiChar) : Boolean;
  { Returns True if C is a valid character for a file name }

function IsFilePath(C : AnsiChar) : Boolean;
  { Returns True if C is a valid character for a path name }

function IsHex(C : AnsiChar) : Boolean;
  { Returns True if C is a valid character for a hex number }

function IsLower(C : AnsiChar) : Boolean;
  { Returns True if C is lower case or a number }

function IsNum(C : AnsiChar) : Boolean;
  { Returns True if C is a number }

function IsSpace(C : AnsiChar) : Boolean;
  { Returns True if C is a space }

function IsUpper(C : AnsiChar) : Boolean;
  { Returns True if C is upper case or a number }

function IsWildCard(C : AnsiChar) : Boolean;
  { Returns True if C is a wild card character }


{ Short String Routines }

function BoolToStrS(B : Boolean) : ShortString;
  { Returns a 'TRUE' or 'FALSE' depending on the value of B.  These strings
    are retrieved from a resource (STCONST). }

function GetSystemDirS : ShortString;
  { Returns the Windows System directory }

function GetTempDirS : ShortString;
  { Returns the Windows Temp directory }

function GetWindowsDirS : ShortString;
  { Returns the Windows directory }

function StrAnsiToOEMS(S : ShortString) : ShortString;
  { Translates a string from the Windows character set into the specified
    OEM character set. }

function StrOEMToAnsiS(S : ShortString) : ShortString;
  { Translates a string from the specified OEM character set to the
  Windows character. }

function StrToBoolS(S : ShortString) : Boolean;
  { Compares S to True string (in STCONST resource) -- returns True if
    match, otherwise returns False.  The compare is not case-sensitive. }


{ Null Terminated String Routines }

function BoolToStrZ(Dest : PAnsiChar; B : Boolean) : PAnsiChar;
  { Returns a 'TRUE' or 'FALSE' depending on the value of B.  These strings
    are retrieved from a resource (STCONST). }

function GetSystemDirZ(Dest : PAnsiChar) : PAnsiChar;
  { Returns the Windows System directory }

function GetTempDirZ(Dest : PAnsiChar) : PAnsiChar;
  { Returns the Windows Temp directory }

function GetWindowsDirZ(Dest : PAnsiChar) : PAnsiChar;
  { Returns the Windows directory }

function StrAnsiToOEMZ(Dest, S : PAnsiChar) : PAnsiChar;
  { Translates a string from the Windows character set into the specified
    OEM character set. }

function StrOEMToAnsiZ(Dest, S : PAnsiChar) : PAnsiChar;
  { Translates a string from the specified OEM character set to the
  Windows character. }

function StrToBoolZ(S : PAnsiChar) : Boolean;
  { Compares S to True string (in STCONST resource) -- returns True if
    match, otherwise returns False.  The compare is not case-sensitive. }


{ Long String Routines }

{$IFDEF Win32}
function BoolToStrL(B : Boolean) : AnsiString;
  { Returns a 'TRUE' or 'FALSE' depending on the value of B.  These strings
    are retrieved from a resource (STCONST). }

function GetSystemDirL : AnsiString;
  { Returns the Windows System directory }

function GetTempDirL : AnsiString;
  { Returns the Windows Temp directory }

function GetWindowsDirL : AnsiString;
  { Returns the Windows directory }

function StrAnsiToOEML(S : AnsiString) : AnsiString;
  { Translates a string from the Windows character set into the specified
    OEM character set. }

function StrOEMToAnsiL(S : AnsiString) : AnsiString;
  { Translates a string from the specified OEM character set to the
  Windows character. }

function StrToBoolL(S : AnsiString) : Boolean;
  { Compares S to True string (in STCONST resource) -- returns True if
    match, otherwise returns False.  The compare is not case-sensitive. }

{$ENDIF}

implementation

{ Character Routines }

function IsAlpha(C : AnsiChar) : Boolean;
begin
  Result := IsCharAlpha(C);
end;

function IsAlphaNum(C : AnsiChar) : Boolean;
begin
  Result := IsCharAlphaNumeric(C);
end;

function IsCtrl(C : AnsiChar) : Boolean;
begin
  Result := (Ord(C) > 0) and (Ord(C) < 32);
end;

function IsDrive(C : AnsiChar) : Boolean;
begin
  Result := CharExistsS(DriveSet, Locase(C));
end;

function IsFileName(C : AnsiChar) : Boolean;
begin
  Result := (C > #32) and (C < #128) and not (CharExistsS(FileNameSet, C));
end;

function IsFilePath(C : AnsiChar) : Boolean;
begin
  Result := (C > #32) and (C < #128) and not (CharExistsS(PathNameSet,C));
end;

function IsHex(C : AnsiChar) : Boolean;
begin
  Result := CharExistsS(HexCharSet, C);
end;

function IsLower(C : AnsiChar) : Boolean;
begin
  if not IsNum(C) then
    Result := IsCharLower(C)
  else
    Result := True;
end;

function IsNum(C : AnsiChar) : Boolean;
begin
  Result := CharExistsS(NumberSet, C);
end;

function IsSpace(C : AnsiChar) : Boolean;
begin
  Result := Ord(C) = 32;
end;

function IsUpper(C : AnsiChar) : Boolean;
begin
  if not IsNum(C) then
    Result := IsCharUpper(C)
  else
    Result := True;
end;

function IsWildCard(C : AnsiChar) : Boolean;
begin
  Result := CharExistsS(WildCharSet, C);
end;


{ Short String Routines }

function BoolToStrS(B : Boolean) : ShortString;
begin
  if B then
    Result := SysToolsLoadStr(stscTrueString)                  {!!.04}
  else
    Result := SysToolsLoadStr(stscFalseString);                {!!.04}
end;

function GetSystemDirS : ShortString;
var
  B : array[0..255] of AnsiChar;
begin
  GetSystemDirectory(B, SizeOf(B));
  Result := StrPas(B);
end;

function GetTempDirS : ShortString;
var
  B : array[0..255] of AnsiChar;
begin
{$IFDEF WIN32}
  GetTempPath(255, B);
  Result := StrPas(B);
  SetLength(Result, Pred(Length(Result)));
{$ELSE}
  GetTempFileName(GetTempDrive('a'), 'aaa', 0, B);
  Result := JustPathNameS(StrPas(B));
{$ENDIF}
end;

function GetWindowsDirS : ShortString;
var
  B : array[0..255] of AnsiChar;
begin
  GetWindowsDirectory(B, SizeOf(B));
  Result := StrPas(B);
end;

function StrAnsiToOEMS(S : ShortString) : ShortString;
var
  B : array[0..255] of AnsiChar;
begin
  StrPCopy(B, S);
  AnsiToOem(B, B);
  Result := StrPas(B);
end;

function StrOEMToAnsiS(S : ShortString) : ShortString;
var
  B : array[0..255] of AnsiChar;
begin
  StrPCopy(B, S);
  OemToAnsi(B, B);
  Result := StrPas(B);
end;

function StrToBoolS(S : ShortString) : Boolean;
begin
  Result := CompUCStringS(S, SysToolsLoadStr(stscTrueString)) = 0; {!!.04}
end;


{ Null Terminated String Routines }

function BoolToStrZ(Dest : PAnsiChar; B : Boolean) : PAnsiChar;
begin
  Result := Dest;
  if B then
    StrPCopy(Dest, SysToolsLoadStr(stscTrueString))            {!!.04}
  else
    StrPCopy(Dest, SysToolsLoadStr(stscFalseString));          {!!.04}
end;

function GetSystemDirZ(Dest : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
  GetSystemDirectory(Dest, MaxFileLen);
end;

function GetTempDirZ(Dest : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
{$IFDEF WIN32}
  GetTempPath(MaxFileLen, Dest);
  StrChDeleteZ(Dest, Dest, Pred(StrLen(Dest)));
{$ELSE}
  GetTempFileName(GetTempDrive('a'), 'aaa', 0, Dest);
  JustPathNameZ(Dest, Dest);
{$ENDIF}
end;

function GetWindowsDirZ(Dest : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
  GetWindowsDirectory(Dest, MaxFileLen);
end;

function StrAnsiToOEMZ(Dest, S : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
{$IFDEF WIN32}
  CharToOEM(S, Dest);
{$ELSE}
  AnsiToOem(S, Dest);
{$ENDIF}
end;

function StrOEMToAnsiZ(Dest, S : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
{$IFDEF WIN32}
  OEMtoChar(S, Dest);
{$ELSE}
  OEMToAnsi(S, Dest);
{$ENDIF}
end;

function StrToBoolZ(S : PAnsiChar) : Boolean;
var
  B : array[0..255] of AnsiChar;
begin
  StrPCopy(B, SysToolsLoadStr(stscTrueString));                {!!.04}
  Result := StrIComp(S, B) = 0;
end;


{ Long String Routines }

{$IFDEF Win32}
function BoolToStrL(B : Boolean) : AnsiString;
begin
  if B then
    Result := SysToolsLoadStr(stscTrueString)                  {!!.04}
  else
    Result := SysToolsLoadStr(stscFalseString);                {!!.04}
end;

function GetSystemDirL : AnsiString;
begin
  SetLength(Result, MaxFileLen);
  GetSystemDirectory(PChar(Result), MaxFileLen);
end;

function GetTempDirL : AnsiString;
begin
  SetLength(Result, MaxFileLen);
  GetTempPath(MaxFileLen, PChar(Result));
  SetLength(Result, Pred(StrLen(PChar(Result))));
end;

function GetWindowsDirL : AnsiString;
begin
  SetLength(Result, MaxFileLen);
  GetWindowsDirectory(PChar(Result), MaxFileLen);
end;

function StrAnsiToOEML(S : AnsiString) : AnsiString;
begin
  Result := S;
  CharToOem(PChar(Result), PChar(Result));
end;

function StrOEMToAnsiL(S : AnsiString) : AnsiString;
begin
  Result := S;
  OemToChar(PChar(Result), PChar(Result));
end;

function StrToBoolL(S : AnsiString) : Boolean;
begin
  Result := AnsiCompareText(S, SysToolsLoadStr(stscTrueString)) = 0;{!!.04}
end;
{$ENDIF}

end.

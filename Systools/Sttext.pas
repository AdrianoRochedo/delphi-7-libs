{$I STDEFINE.INC}

{$A-} {Packed records}
{$B-} {Incomplete boolean evaluation}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF OS32}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   STTEXT.PAS 1.05                     *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit STText;
  {-Routines for manipulating Delphi Text files}

interface

uses
{$IFDEF OS32}
{$IFDEF VirtualPascal}
  Os2Base,
{$ELSE}
  Windows,
{$ENDIF}
{$ELSE}
  WinTypes,
  WinProcs,
{$ENDIF}
  SysUtils,
  STConst,
  STBase,
  STUtils;

function TextSeek(var F : TextFile; Target : LongInt) : Boolean;
 {-Seek to the specified position in a text file opened for input}

function TextFileSize(var F : TextFile) : LongInt;
  {-Return the size of a text file}

function TextPos(var F : TextFile) : LongInt;
 {-Return the current position of the logical file pointer (that is,
   the position of the physical file pointer, adjusted to account for
   buffering)}

function TextFlush(var F : TextFile) : Boolean;
  {-Flush the buffer(s) for a text file}

implementation

{$IFDEF SYSDEMO} uses SysUtil; {$ENDIF}

{$IFNDEF OS32}
const
  FILE_BEGIN = 0;
  FILE_CURRENT = 1;
  FILE_END = 2;
{$ENDIF}

function TextSeek(var F : TextFile; Target : LongInt) : Boolean;
  {-Do a Seek for a text file opened for input. Returns False in case of I/O
    error.}
var
  Pos : LongInt;
begin
  with TTextRec(F) do begin
    {assume failure}
    Result := False;
    {check for file opened for input}
    if Mode <> fmInput then Exit;
    Pos := FileSeek(Handle, 0, FILE_CURRENT);
    if Pos = -1 then Exit;
    Dec(Pos, BufEnd);
    {see if the Target is within the buffer}
    Pos := Target-Pos;
    if (Pos >= 0) and (Pos < BufEnd) then
      {it is--just move the buffer pointer}
      BufPos := Pos
    else begin
      if FileSeek(Handle, Target, FILE_BEGIN) = -1 then Exit;
      {tell Delphi its buffer is empty}
      BufEnd := 0;
      BufPos := 0;
    end;
  end;
  {if we get to here we succeeded}
  Result := True;
end;

function TextFileSize(var F : TextFile) : LongInt;
  {-Return the size of text file F. Returns -1 in case of I/O error.}
var
  Old : LongInt;
  Res : LongInt;
begin
  Result := -1;
  with TTextRec(F) do begin
    {check for open file}
    if Mode = fmClosed then Exit;
    {get/save current pos of the file pointer}
    Old := FileSeek(Handle, 0, FILE_CURRENT);
    if Old = -1 then Exit;
    {have OS move to end-of-file}
    Res := FileSeek(Handle, 0, FILE_END);
    if Res = -1 then Exit;
    {reset the old position of the file pointer}
    if FileSeek(Handle, Old, FILE_BEGIN) = - 1 then Exit;
  end;
  Result := Res;
end;

function TextPos(var F : TextFile) : LongInt;
  {-Return the current position of the logical file pointer (that is,
    the position of the physical file pointer, adjusted to account for
    buffering). Returns -1 in case of I/O error.}
var
  Position : LongInt;
begin
  Result := -1;
  with TTextRec(F) do begin
    {check for open file}
    if Mode = fmClosed then Exit;
    Position := FileSeek(Handle, 0, FILE_CURRENT);
    if Position = -1 then Exit;
  end;
  with TTextRec(F) do
    if Mode = fmOutput then     {writing}
      Inc(Position, BufPos)
    else if BufEnd <> 0 then    {reading}
      Dec(Position, BufEnd-BufPos);
  {return the calculated position}
  Result := Position;
end;

function TextFlush(var F : TextFile) : Boolean;
  {-Flush the buffer(s) for a text file. Returns False in case of I/O error.}
var
  Position : LongInt;
  Code : Integer;
begin
  Result := False;
  with TTextRec(F) do begin
    {check for open file}
    if Mode = fmClosed then Exit;
    {see if file is opened for reading or writing}
    if Mode = fmInput then begin
      {get current position of the logical file pointer}
      Position := TextPos(F);
      {exit in case of I/O error}
      if Position = -1 then Exit;
      if FileSeek(Handle, Position, FILE_BEGIN) = - 1 then Exit;
    end
    else begin
      {write the current contents of the buffer, if any}
      if BufPos <> 0 then begin
        Code := FileWrite(Handle, BufPtr^, BufPos);
        if Code = -1 {<> 0} then Exit;                                 {!!.02}
      end;
      {flush OS's buffers}
      if not FlushOsBuffers(Handle) then Exit;
    end;
    {tell Delphi its buffer is empty}
    BufEnd := 0;
    BufPos := 0;
  end;
  {if we get to here we succeeded}
  Result := True;
end;

{$IFDEF SYSDEMO}
initialization
  _CC_; _VC_;
{$ENDIF}

end.

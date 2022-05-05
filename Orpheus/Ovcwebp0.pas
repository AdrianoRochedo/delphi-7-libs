{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                  OVCWEBP0.PAS 2.15                    *}
{*     Copyright (c) 1995-97 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}


unit OvcWebP0;
  {-Component editor to provide web access}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  DsgnIntf, Classes, Controls, Dialogs, ShellApi, TypInfo,
  OvcData;

const
  WebText  = 'TurboPower''s WEB Page';
  MailText = 'Send mail to TurboPower';

type
  TOvcWebEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer);
      override;
    function GetVerb(Index : Integer) : AnsiString;
      override;
    function GetVerbCount : Integer;
      override;
  end;

procedure ShellWebCall;
procedure ShellMailCall;

implementation

procedure ShellWebCall;
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
end;

procedure ShellMailCall;
begin
  if ShellExecute(0, 'open', 'mailto:support@turbopower.com', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start mail client. Make sure you have it properly set-up on your system.');
end;

{*** TOvcWebEditor ***}

procedure TOvcWebEditor.ExecuteVerb(Index : Integer);
begin
  if Index = 0 then
    ShellWebCall
  else if Index = 1 then
    ShellMailCall;
end;

function TOvcWebEditor.GetVerb(Index : Integer) : AnsiString;
begin
  case Index of
    0 : Result := WebText;
    1 : Result := MailText;
  else
    Result := '?';
  end;
end;

function TOvcWebEditor.GetVerbCount : Integer;
begin
  Result := 2;
end;


end.

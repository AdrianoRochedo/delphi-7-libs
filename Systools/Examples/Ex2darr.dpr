{*********************************************************}
{*                  EX2DARR.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Ex2darr;

uses
  Forms,
  Uex2darr in 'UEX2DARR.PAS' {STDlg};

{$R *.RES}

begin
  Application.CreateForm(TSTDlg, STDlg);
  Application.Run;
end.

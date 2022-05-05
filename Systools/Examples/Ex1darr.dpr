{*********************************************************}
{*                  EX1DARR.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Ex1darr;

uses
  Forms,
  Uex1darr in 'UEX1DARR.PAS' {STDlg};

{$R *.RES}

begin
  Application.CreateForm(TSTDlg, STDlg);
  Application.Run;
end.

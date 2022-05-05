{*********************************************************}
{*                  EXCOLL.DPR 1.05                      *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Excoll;

uses
  Forms,
  Uexcoll in 'UEXCOLL.PAS' {STDlg};

{$R *.RES}

begin
  Application.CreateForm(TStDlg, STDlg);
  Application.Run;
end.

{*********************************************************}
{*                   EXBITS.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Exbits;

uses
  Forms,
  Uexbits in 'UEXBITS.PAS' {STDlg};

{$R *.RES}

begin
  Application.CreateForm(TSTDlg, STDlg);
  Application.Run;
end.

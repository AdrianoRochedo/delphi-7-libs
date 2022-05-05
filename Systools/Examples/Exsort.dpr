{*********************************************************}
{*                   EXSORT.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Exsort;

uses
  Forms,
  Uexsort in 'UEXSORT.PAS' {TSTDlg};

{$R *.RES}

begin
  Application.CreateForm(TSTDlg, STDlg);
  Application.Run;
end.

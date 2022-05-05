{*********************************************************}
{*                   EXDQUE.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Exdque;

uses
  Forms,
  Uexdque in 'UEXDQUE.PAS' {STDlg};

{$R *.RES}

begin
  Application.CreateForm(TStDlg, StDlg);
  Application.Run;
end.

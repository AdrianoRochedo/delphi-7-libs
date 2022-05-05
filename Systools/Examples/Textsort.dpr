{*********************************************************}
{*                  TEXTSORT.DPR 1.05                    *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Textsort;

uses
  Forms,
  Utxtsort in 'UTXTSORT.PAS' {STDlg},
  Utxtsrt2 in 'UTXTSRT2.PAS' {AboutBox};

{$R *.RES}

begin
  Application.CreateForm(TSTDlg, STDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.

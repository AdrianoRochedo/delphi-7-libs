{*********************************************************}
{*                   RIEDIT.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program RIEDIT;

uses
  Forms,
  RIEditU1 in 'RIEDITU1.PAS' {Form1},
  RIEditU2 in 'RIEDITU2.PAS' {DataDlg},
  Rieditu3 in 'RIEDITU3.PAS' {AboutBox};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataDlg, DataDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.

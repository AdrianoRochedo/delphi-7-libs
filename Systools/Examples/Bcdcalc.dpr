{*********************************************************}
{*                  BCDCALC.DPR 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Bcdcalc;

uses
  Forms,
  Ubcdcal1 in 'UBCDCAL1.PAS' {BCDCalcDlg},
  Ubcdcal2 in 'UBCDCAL2.PAS' {BCDCalcAbout};

{$R *.RES}

begin
  Application.CreateForm(TBCDCalcDlg, BCDCalcDlg);
  Application.CreateForm(TBCDCalcAbout, BCDCalcAbout);
  Application.Run;
end.

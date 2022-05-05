{*********************************************************}
{*                  ASTCAL.DPR 1.05                      *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

program Astcal;

uses
  Forms,
  Uastcal in 'UASTCAL.PAS' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

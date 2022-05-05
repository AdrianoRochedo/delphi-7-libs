{*****************************************}
{   TeeChart-Pro 4.0                      }
{   Copyright (c) 1995-98 David Berneda   }
{       Component Registration Unit       }
{*****************************************}
{$I teedefs.inc}
unit TeeChart;

interface

Procedure Register;

implementation

Uses ChartReg,   { <-- Basic Series and Functions }
     ChartPro,   { <-- Extended Series and Functions }
     TeeLoper    { <-- Developer Sample Series }
   {$IFDEF D1}   { <-- TUpDown component only for Delphi 1 }
     ,TeeUpDow
   {$ENDIF}
   {$IFDEF D2}
     ,ChartExp   { <-- Chart Wizard in Delphi 2.0 }
   {$ENDIF}
     ;

Procedure Register;
Begin
  ChartReg.Register;
{$IFDEF D1}
  TeeUpDow.Register;
{$ENDIF}
  ChartPro.Register;
{$IFNDEF D3}
  TeeLoper.Register;
{$ENDIF}
{$IFDEF D2}
  ChartExp.Register;    { <-- Chart Wizard in Delphi 2.0 or greater }
{$ENDIF}
End;

end.

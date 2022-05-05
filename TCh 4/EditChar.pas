{****************************************}
{     TeeChart Pro Charting Library      }
{  For Delphi 1,2,3,4 & C++ Builder 1&3  }
{ Copyright (c) 1995-98 by David Berneda }
{         All Rights Reserved            }
{****************************************}
{$I teedefs.inc}
unit EditChar;

interface

Uses Classes,Graphics,Forms,TeEngine,Chart,
     {$IFDEF D1}
     iEdit16
     {$ELSE}
     iEditCha
     {$ENDIF}
     ;

Function EditFont(AOwner:TComponent; AFont:TFont):TFont;

Procedure EditChartLegend(Form:TForm; AChart:TCustomChart);
Procedure EditSeries(Form:TForm; ASeries:TChartSeries);
Procedure EditChart(Form:TForm; AChart:TCustomChart);
Procedure EditChartAxis(Form:TForm; Axis:TCustomChartAxis);
Procedure EditChartWall(Form:TForm; AWall:TChartWall);
Procedure EditChartTitle(Form:TForm; ATitle:TChartTitle);
Procedure EditChartPage(Form:TForm; AChart:TCustomChart; PageIndex:Longint);
Procedure EditChartPart(Form:TForm; AChart:TCustomChart; Const Part:TChartClickedPart);

{$IFDEF D3}
Procedure EditDSSChart(AOwner:TComponent; ADSSChart:TCustomChart);
{$ENDIF}

implementation

Uses TeeProcs,TeeConst,CustEdit,AreaEdit,BarEdit,PieEdit,FLineEdi,ShapeEdi,
     GanttEdi,ArrowEdi,BubbleCh,BrushDlg,StdCtrls;

Function EditFont(AOwner:TComponent; AFont:TFont):TFont;
Begin
  result:=InternalEditFont(AOwner,AFont);
end;

Function GetChartEditClass(AChart:TCustomAxisPanel):TChartEditForm;
begin
  result:=TChartEditForm.Create(Application);
  result.TheChart:=TCustomChart(AChart);
  {$IFDEF TEEHELPEDITOR}
  result.CheckHelpFile;
  {$ENDIF}
end;

Procedure EditSeries(Form:TForm; ASeries:TChartSeries);
begin
  With GetChartEditClass(ASeries.ParentChart) do
  try
    TheEditSeries:=ASeries;
    ShowModal;
  finally
    Free;
  end;
end;

Procedure EditChartPage(Form:TForm; AChart:TCustomChart; PageIndex:Longint);
begin
  With GetChartEditClass(AChart) do
  try
    TheActivePageIndex:=PageIndex;
    ShowModal;
  finally
    Free;
  end;
end;

Procedure EditChartTitle(Form:TForm; ATitle:TChartTitle);
Begin
  With GetChartEditClass(TCustomChart(ATitle.ParentChart)) do
  try
    TheTitle:=ATitle;
    TheActivePageIndex:=teeEditTitlePage;
    ShowModal;
  finally
    Free;
  end;
end;

Procedure EditChartWall(Form:TForm; AWall:TChartWall);
Begin
  With GetChartEditClass(AWall.ParentChart) do
  try
    TheWall:=AWall;
    TheActivePageIndex:=teeEditWallsPage;
    ShowModal;
  finally
    Free;
  end;
end;

Procedure EditChartAxis(Form:TForm; Axis:TCustomChartAxis);
Begin
  With GetChartEditClass(Axis.ParentChart) do
  try
    TheAxis:=Axis;
    TheActivePageIndex:=teeEditAxisPage;
    ShowModal;
  finally
    Free;
  end;
end;

{$IFDEF D3}
Procedure EditDSSChart(AOwner:TComponent; ADSSChart:TCustomChart);
begin
  With GetChartEditClass(ADSSChart) do
  try
    IsDssGraph:=True;
    ShowModal;
  finally
    Free;
  end;
end;
{$ENDIF}

Procedure EditChart(Form:TForm; AChart:TCustomChart);
Begin
  EditChartPage(Form,AChart,teeEditMainPage);
end;

Procedure EditChartLegend(Form:TForm; AChart:TCustomChart);
begin
  EditChartPage(Form,AChart,teeEditLegendPage);
end;

Procedure EditChartPart(Form:TForm; AChart:TCustomChart; Const Part:TChartClickedPart);
begin
  case Part.Part of
    cpLegend   : EditChartLegend(Form,AChart);
    cpAxis     : EditChartAxis(Form,Part.AAxis);
    cpSeries   : EditSeries(Form,Part.ASeries);
    cpTitle    : EditChartTitle(Form,AChart.Title);
    cpFoot     : EditChartTitle(Form,AChart.Foot);
  else
    EditChart(Form,AChart);
  end;
end;

end.

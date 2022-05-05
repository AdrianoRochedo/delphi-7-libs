unit NoRedraw;

interface

{ This unit shows 2 things:

  1. How to create a Chart component with auto-created Series at design-time.
  2. How to avoid the Chart Editor Dialog to redraw the Chart until finished.

  INSTALLATION:

  Simply install this unit in Delphi Component Library.
}
uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart;

type
  TMyChart = class(TChart)          { <-- My Chart class }
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
     Editing:Boolean;
     Constructor Create( AOwner : TComponent ); override ;
     procedure Invalidate; override;
  published
    { change the default Color and View3D properties }
    property Color default clWhite ;
    property View3D default False ;
  end;

{ Necessary to register the TMyChart }
procedure Register;

implementation

Uses Series,EditChar,ChartReg,DsgnIntf;

Constructor TMyChart.Create( AOwner : TComponent );
begin
   inherited Create( AOwner );
   { change the Color and View3D }
   Color:=clWhite ;
   View3D:=False;

   { This variable is used only when showing the Chart Editor }
   Editing:=False;

   { Auto-Create example Series }
   if not (csLoading in Owner.ComponentState) then
   begin
    AddSeries( TBarSeries.Create( Owner ) );
    Series[0].Name:='Sales' ;
    Series[0].SeriesColor:=clBlue ;

    AddSeries( TLineSeries.Create( Owner ) );
    Series[1].Name:='Expenses' ;
    Series[1].SeriesColor:=clGreen ;
  end;
end;

{ If we are Editing DO NOT redraw the Chart }
procedure TMyChart.Invalidate;
begin
  if not Editing then inherited Invalidate;
end;

{ A special Chart Editor }
type TMyChartEditor = class ( TChartCompEditor )
     public
       procedure Edit; override;
     end;

procedure TMyChartEditor.Edit;
Var AChart:TMyChart;
begin
  AChart:=TMyChart(Component);
  AChart.Editing:=True;
  try
    inherited Edit;
  finally
    AChart.Editing:=False;
    AChart.Repaint;        { <-- redraw the Chart when finished }
  end;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TMyChart]);
  RegisterComponentEditor( TMyChart, TMyChartEditor );
end;

end.

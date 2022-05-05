{****************************************}
{    TFastLineSeries Editor Dialog       }
{ Copyright (c) 1996-98 by David Berneda }
{****************************************}
{$I teedefs.inc}
unit FLineEdi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Chart, Buttons,Teengine,Series;

type
  TFastLineSeriesEditor = class(TForm)
    BLinePen: TButton;
    SHColor: TShape;
    procedure BLinePenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SHColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    TheSeries:TFastLineSeries;
  end;

implementation

{$R *.DFM}
Uses PenDlg, TeeProcs;

procedure TFastLineSeriesEditor.BLinePenClick(Sender: TObject);
begin
  With TheSeries do
  begin
    LinePen.Color:=SeriesColor;
    EditChartPen(Self,LinePen);
    SeriesColor:=LinePen.Color;
    SHColor.Brush.Color:=SeriesColor;
  end;
end;

procedure TFastLineSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  TheSeries:=TFastLineSeries(Tag);
  SHColor.Brush.Color:=TheSeries.SeriesColor;
  SHColor.Cursor:=crTeeHand;
end;

procedure TFastLineSeriesEditor.SHColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With TheSeries do
  begin
    SeriesColor:=EditColor(Self,SeriesColor);
    SHColor.Brush.Color:=SeriesColor;
  end;
end;

initialization
  RegisterClass(TFastLineSeriesEditor);
end.

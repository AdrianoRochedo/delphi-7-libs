{****************************************}
{ TeeChart Pro 4.0 TVolumeSeries Editor  }
{ Copyright (c) 1998 by David Berneda    }
{****************************************}
{$I teedefs.inc}
unit TeeVolEd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Chart, Buttons, Teengine, CandleCh;

type
  TVolumeSeriesEditor = class(TForm)
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
    TheSeries:TVolumeSeries;
  end;

implementation

{$R *.DFM}
Uses PenDlg, TeeProcs;

procedure TVolumeSeriesEditor.BLinePenClick(Sender: TObject);
begin
  With TheSeries do
  begin
    LinePen.Color:=SeriesColor;
    EditChartPen(Self,LinePen);
    SeriesColor:=LinePen.Color;
    SHColor.Brush.Color:=SeriesColor;
  end;
end;

procedure TVolumeSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  TheSeries:=TVolumeSeries(Tag);
  SHColor.Brush.Color:=TheSeries.SeriesColor;
  SHColor.Cursor:=crTeeHand;
end;

procedure TVolumeSeriesEditor.SHColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With TheSeries do
  begin
    SeriesColor:=EditColor(Self,SeriesColor);
    SHColor.Brush.Color:=SeriesColor;
  end;
end;

initialization
  RegisterClass(TVolumeSeriesEditor);
end.

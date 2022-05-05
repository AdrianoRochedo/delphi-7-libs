{****************************************}
{    TGanttSeries Editor Dialog          }
{ Copyright (c) 1996-98 by David Berneda }
{****************************************}
{$I teedefs.inc}
unit GanttEdi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, GanttCh
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}


type
  TGanttSeriesEditor = class(TForm)
    Label2: TLabel;
    SEPointVertSize: TEdit;
    BConnLines: TBitBtn;
    UDPointVertSize: TUpDown;
    GPLine: TGroupBox;
    SHColor: TShape;
    BitBtn1: TBitBtn;
    CBColorEach: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SEPointVertSizeChange(Sender: TObject);
    procedure BConnLinesClick(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SHColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    CreatingForm:Boolean;
    procedure RefreshShape;
  public
    { Public declarations }
    TheSeries:TGanttSeries;
  end;

implementation

{$R *.DFM}
uses PenDlg, Teengine, Series, TeePoEdi, TeeProcs;

procedure TGanttSeriesEditor.RefreshShape;
begin
  SHColor.Brush.Color:= TheSeries.SeriesColor;
  SHColor.Visible    := not TheSeries.ColorEachPoint;
end;

procedure TGanttSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  TheSeries:=TGanttSeries(Tag);
  With TheSeries do
  begin
    UDPointVertSize.Position := Pointer.VertSize;
    CBColorEach.Checked      := ColorEachPoint;
  end;
  SHColor.Cursor:=crTeeHand;
  RefreshShape;
  TeeInsertPointerForm(Parent,TheSeries.Pointer);
  CreatingForm:=False;
end;

procedure TGanttSeriesEditor.Button1Click(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.LinePen);
end;

procedure TGanttSeriesEditor.FormCreate(Sender: TObject);
begin
  TheSeries:=nil;
  CreatingForm:=True;
end;

procedure TGanttSeriesEditor.SEPointVertSizeChange(Sender: TObject);
begin
  if not CreatingForm then TheSeries.Pointer.VertSize:=UDPointVertSize.Position;
end;

procedure TGanttSeriesEditor.BConnLinesClick(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.ConnectingPen);
end;

procedure TGanttSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  TheSeries.ColorEachPoint:=CBColorEach.Checked;
  RefreshShape;
end;

procedure TGanttSeriesEditor.BitBtn1Click(Sender: TObject);
begin
  With TheSeries do
  begin
    SeriesColor:=EditColor(Self,SeriesColor);
    ColorEachPoint:=False;
    RefreshShape;
  end;
end;

procedure TGanttSeriesEditor.SHColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BitBtn1Click(Self);
end;

initialization
  RegisterClass(TGanttSeriesEditor);
end.

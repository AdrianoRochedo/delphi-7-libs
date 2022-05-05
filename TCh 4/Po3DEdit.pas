{**********************************************}
{   TPoint3DSeries Component Editor Dialog     }
{   Copyright (c) 1998 by David Berneda        }
{**********************************************}
{$I teedefs.inc}
unit Po3DEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart,Series, TeePoin3
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TPoint3DSeriesEditor = class(TForm)
    GPLine: TGroupBox;
    BitBtn1: TBitBtn;
    CBColorEach: TCheckBox;
    Button1: TButton;
    SHColor: TShape;
    Label4: TLabel;
    SEPointDepth: TEdit;
    UDPointDepth: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
    procedure SHColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SEPointDepthChange(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
  public
    { Public declarations }
    TheSeries:TPoint3DSeries;
  end;

implementation

{$R *.DFM}
uses PenDlg, Teengine, TeePoEdi, TeeProcs;

procedure TPoint3DSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  TheSeries:=TPoint3DSeries(Tag);
  With TheSeries do
  Begin
    SHColor.Brush.Color:=SeriesColor;
    UDPointDepth.Position :=Round(DepthSize);
    CBColorEach.Checked:=ColorEachPoint;
  end;
  SHColor.Cursor:=crTeeHand;
  TeeInsertPointerForm(Parent,TheSeries.Pointer);
  CreatingForm:=False;
end;

procedure TPoint3DSeriesEditor.BitBtn1Click(Sender: TObject);
begin
  With TheSeries do SeriesColor:=EditColor(Self,SeriesColor);
  SHColor.Brush.Color:=TheSeries.SeriesColor;
end;

procedure TPoint3DSeriesEditor.Button1Click(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.LinePen);
end;

procedure TPoint3DSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

procedure TPoint3DSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  TheSeries.ColorEachPoint:=CBColorEach.Checked;
end;

procedure TPoint3DSeriesEditor.SHColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BitBtn1Click(Self);
end;

procedure TPoint3DSeriesEditor.SEPointDepthChange(Sender: TObject);
begin
  if not CreatingForm then TheSeries.DepthSize:=UDPointDepth.Position;
end;

initialization
  RegisterClass(TPoint3DSeriesEditor);
end.

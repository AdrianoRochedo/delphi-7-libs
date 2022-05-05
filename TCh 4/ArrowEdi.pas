{**********************************************}
{   TArrowSeries Editor Dialog                 }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit ArrowEdi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ArrowCha, ExtCtrls
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}


type
  TArrowSeriesEditor = class(TForm)
    BPen: TBitBtn;
    BBrush: TBitBtn;
    Label1: TLabel;
    SEArrowWidth: TEdit;
    Label2: TLabel;
    SEArrowHeight: TEdit;
    GroupBox1: TGroupBox;
    CBColorEach: TCheckBox;
    BArrowColor: TBitBtn;
    SHArrowColor: TShape;
    UDArrowWidth: TUpDown;
    UDArrowHeight: TUpDown;
    procedure CBColorEachClick(Sender: TObject);
    procedure BArrowColorClick(Sender: TObject);
    procedure BPenClick(Sender: TObject);
    procedure BBrushClick(Sender: TObject);
    procedure SEArrowWidthChange(Sender: TObject);
    procedure SEArrowHeightChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SHArrowColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
  public
    { Public declarations }
    Arrow : TArrowSeries;
  end;

implementation

{$R *.DFM}
Uses PenDlg, BrushDlg, TeEngine, TeeProcs;

procedure TArrowSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  Arrow.ColorEachPoint:=CBColorEach.Checked;
  BArrowColor.Enabled:=not Arrow.ColorEachPoint;
  if Arrow.ColorEachPoint then Arrow.Pointer.Brush.Color:=clTeeColor;
  SHArrowColor.Visible:=BArrowColor.Enabled;
end;

procedure TArrowSeriesEditor.BArrowColorClick(Sender: TObject);
begin
  With Arrow do
  begin
    SeriesColor:=EditColor(Self,SeriesColor);
    SHArrowColor.Brush.Color:=SeriesColor;
  end;
end;

procedure TArrowSeriesEditor.BPenClick(Sender: TObject);
begin
  EditChartPen(Self,Arrow.Pointer.Pen);
end;

procedure TArrowSeriesEditor.BBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,Arrow.Pointer.Brush);
end;

procedure TArrowSeriesEditor.SEArrowWidthChange(Sender: TObject);
begin
  if not CreatingForm then Arrow.ArrowWidth:=UDArrowWidth.Position;
end;

procedure TArrowSeriesEditor.SEArrowHeightChange(Sender: TObject);
begin
  if not CreatingForm then Arrow.ArrowHeight:=UDArrowHeight.Position;
end;

procedure TArrowSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Arrow:=TArrowSeries(Tag);
  With Arrow do
  begin
    CBColorEach.Checked    :=ColorEachPoint;
    UDArrowWidth.Position  :=ArrowWidth;
    UDArrowHeight.Position :=ArrowHeight;
    SHArrowColor.Brush.Color:=SeriesColor;
  end;
  SHArrowColor.Cursor:=crTeeHand;
  CreatingForm:=False;
end;

procedure TArrowSeriesEditor.SHArrowColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BArrowColorClick(Self);
end;

procedure TArrowSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

initialization
  RegisterClass(TArrowSeriesEditor);
end.

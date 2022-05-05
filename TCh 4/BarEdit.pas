{**********************************************}
{   TBarSeries Component Editor Dialog         }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
Unit BarEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, Series
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TBarSeriesEditor = class(TForm)
    SEBarwidth: TEdit;
    Label1: TLabel;
    CBBarStyle: TComboBox;
    LStyle: TLabel;
    BBarPen: TButton;
    BBarBrush: TButton;
    CBYOrigin: TCheckBox;
    EYOrigin: TEdit;
    RGMultiBar: TRadioGroup;
    Label3: TLabel;
    SEBarOffset: TEdit;
    GroupBox1: TGroupBox;
    CBDarkBar: TCheckBox;
    CBBarSideMargins: TCheckBox;
    GroupBox2: TGroupBox;
    SHBarColor: TShape;
    CBColorEach: TCheckBox;
    BBarColor: TBitBtn;
    CBMarksAutoPosition: TCheckBox;
    UDBarWidth: TUpDown;
    UDBarOffset: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure SEBarwidthChange(Sender: TObject);
    procedure CBYOriginClick(Sender: TObject);
    procedure EYOriginChange(Sender: TObject);
    procedure CBBarStyleChange(Sender: TObject);
    procedure BBarPenClick(Sender: TObject);
    procedure BBarBrushClick(Sender: TObject);
    procedure RGMultiBarClick(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
    procedure BBarColorClick(Sender: TObject);
    procedure CBDarkBarClick(Sender: TObject);
    procedure CBBarSideMarginsClick(Sender: TObject);
    procedure SEBarOffsetChange(Sender: TObject);
    procedure SHBarColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBMarksAutoPositionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    Procedure RefreshShape;
  public
    { Public declarations }
    Bar:TCustomBarSeries;
  end;

implementation

{$R *.DFM}
Uses PenDlg, BrushDlg, TeeProcs;

procedure TBarSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Bar:=TCustomBarSeries(Tag);
  With Bar do
  begin
    CBBarStyle.ItemIndex :=Ord(BarStyle);
    UDBarWidth.Position  :=BarWidthPercent;
    UDBarOffset.Position :=OffsetPercent;
    CBYOrigin.Checked    :=UseYOrigin;
    EYOrigin.Text        :=FloatToStr(YOrigin);
    EYOrigin.Enabled     :=UseYOrigin;
    RGMultiBar.ItemIndex :=Ord(MultiBar);
    CBColorEach.Checked  :=ColorEachPoint;
    CBMarksAutoPosition.Checked:=AutoMarkPosition;
    CBDarkBar.Checked    :=Dark3D;
    CBBarSideMargins.Checked:=SideMargins;
  end;
  SHBarColor.Cursor:=crTeeHand;
  RefreshShape;
  CreatingForm:=False;
end;

Procedure TBarSeriesEditor.RefreshShape;
Begin
  SHBarColor.Visible:=not Bar.ColorEachPoint;
  BBarColor.Enabled:=SHBarColor.Visible;
  if SHBarColor.Visible then SHBarColor.Brush.Color:=Bar.SeriesColor;
end;

procedure TBarSeriesEditor.SEBarWidthChange(Sender: TObject);
begin
  if not CreatingForm then Bar.BarWidthPercent:=UDBarWidth.Position;
end;

procedure TBarSeriesEditor.CBYOriginClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    Bar.UseYOrigin:=CBYOrigin.Checked;
    EYOrigin.Enabled:=Bar.UseYOrigin;
    if EYOrigin.Enabled then EYOrigin.SetFocus;
  end;
end;

procedure TBarSeriesEditor.EYOriginChange(Sender: TObject);
begin
  if not CreatingForm then
  if EYOrigin.Text<>'' then
     Bar.YOrigin:=StrToFloat(EYOrigin.Text);
end;

procedure TBarSeriesEditor.CBBarStyleChange(Sender: TObject);
begin
  if not CreatingForm then
     Bar.BarStyle:=TBarStyle(CBBarStyle.ItemIndex);
end;

procedure TBarSeriesEditor.BBarPenClick(Sender: TObject);
begin
  EditChartPen(Self,Bar.BarPen);
end;

procedure TBarSeriesEditor.BBarBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,Bar.BarBrush);
end;

procedure TBarSeriesEditor.RGMultiBarClick(Sender: TObject);
begin
  if not CreatingForm then
     Bar.MultiBar:=TMultiBar(RGMultiBar.ItemIndex);
end;

procedure TBarSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    Bar.ColorEachPoint:=CBColorEach.Checked;
    RefreshShape;
  end;
end;

procedure TBarSeriesEditor.BBarColorClick(Sender: TObject);
begin
  With Bar do SeriesColor:=EditColor(Self,SeriesColor);
  CBColorEach.Checked:=False;
  RefreshShape;
end;

procedure TBarSeriesEditor.CBDarkBarClick(Sender: TObject);
begin
  if not CreatingForm then Bar.Dark3D:=CBDarkBar.Checked;
end;

procedure TBarSeriesEditor.CBBarSideMarginsClick(Sender: TObject);
begin
  if not CreatingForm then
     Bar.SideMargins:=CBBarSideMargins.Checked;
end;

procedure TBarSeriesEditor.SEBarOffsetChange(Sender: TObject);
begin
  if not CreatingForm then Bar.OffsetPercent:=UDBarOffset.Position;
end;

procedure TBarSeriesEditor.SHBarColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BBarColorClick(Self);
end;

procedure TBarSeriesEditor.CBMarksAutoPositionClick(Sender: TObject);
begin
  Bar.AutoMarkPosition:=CBMarksAutoPosition.Checked;
end;

procedure TBarSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

initialization
  RegisterClass(TBarSeriesEditor);
end.

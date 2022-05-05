{**********************************************}
{   TAreaSeries Component Editor Dialog        }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit AreaEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Chart, Series, ExtCtrls;

type
  TAreaSeriesEditor = class(TForm)
    RGMultiArea: TRadioGroup;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    CBStairs: TCheckBox;
    CBAreaBrush: TComboBox;
    BAreaLinesPen: TButton;
    BAreaLinePen: TButton;
    GroupBox1: TGroupBox;
    CBColorEach: TCheckBox;
    BAreaColor: TBitBtn;
    SHAreaColor: TShape;
    CBInvStairs: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure RGMultiAreaClick(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
    procedure BAreaColorClick(Sender: TObject);
    procedure CBStairsClick(Sender: TObject);
    procedure CBAreaBrushChange(Sender: TObject);
    procedure BAreaLinesPenClick(Sender: TObject);
    procedure SHAreaColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BAreaLinePenClick(Sender: TObject);
    procedure CBInvStairsClick(Sender: TObject);
  private
    { Private declarations }
    Procedure RefreshShape;
    Function GetAreaColor:TColor;
  public
    { Public declarations }
    Area:TAreaSeries;
  end;

implementation

{$R *.DFM}
Uses TeEngine, PenDlg, BrushDlg, TeePoEdi, TeeProcs;

procedure TAreaSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Area:=TAreaSeries(Tag);
  With Area do
  begin
    RGMultiArea.ItemIndex :=Ord(MultiArea);
    CBColorEach.Checked   :=ColorEachPoint;
    CBAreaBrush.ItemIndex :=Ord(AreaBrush);
    CBStairs.Checked      :=Stairs;
    CBInvStairs.Checked   :=InvertedStairs;
    CBInvStairs.Enabled   :=Stairs;
  end;
  SHAreaColor.Cursor:=crTeeHand;
  RefreshShape;
  TeeInsertPointerForm(Parent,Area.Pointer);
end;

Procedure TAreaSeriesEditor.RefreshShape;
Begin
  SHAreaColor.Visible:=not Area.ColorEachPoint;
  BAreaColor.Enabled:=SHAreaColor.Visible;
  if SHAreaColor.Visible then SHAreaColor.Brush.Color:=GetAreaColor;
end;

procedure TAreaSeriesEditor.RGMultiAreaClick(Sender: TObject);
begin
  Area.MultiArea:=TMultiArea(RGMultiArea.ItemIndex);
end;

procedure TAreaSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  Area.ColorEachPoint:=CBColorEach.Checked;
  RefreshShape;
end;

Function TAreaSeriesEditor.GetAreaColor:TColor;
begin
  With Area do
  if AreaColor=clTeeColor then result:=SeriesColor
                          else result:=AreaColor;
end;

procedure TAreaSeriesEditor.BAreaColorClick(Sender: TObject);
begin
  Area.SeriesColor:=EditColor(Self,GetAreaColor);
  RefreshShape;
end;

procedure TAreaSeriesEditor.CBStairsClick(Sender: TObject);
begin
  Area.Stairs:=CBStairs.Checked;
  CBInvStairs.Enabled:=Area.Stairs;
end;

procedure TAreaSeriesEditor.CBAreaBrushChange(Sender: TObject);
begin
  Area.AreaBrush:=TBrushStyle(CBAreaBrush.ItemIndex);
end;

procedure TAreaSeriesEditor.BAreaLinesPenClick(Sender: TObject);
begin
  EditChartPen(Self,Area.AreaLinesPen);
end;

procedure TAreaSeriesEditor.SHAreaColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BAreaColorClick(Self);
end;

procedure TAreaSeriesEditor.BAreaLinePenClick(Sender: TObject);
begin
  EditChartPen(Self,Area.LinePen);
end;

procedure TAreaSeriesEditor.CBInvStairsClick(Sender: TObject);
begin
  Area.InvertedStairs:=CBInvStairs.Checked;
end;

initialization
  RegisterClass(TAreaSeriesEditor);
end.

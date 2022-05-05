{**********************************************}
{   TCustomSeries Component Editor Dialog      }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit CustEdit;

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
  TCustomSeriesEditor = class(TForm)
    GPLine: TGroupBox;
    BLineBorder: TButton;
    BLineColor: TBitBtn;
    LPattern: TLabel;
    CBLineBrush: TComboBox;
    GBStair: TGroupBox;
    CBStairs: TCheckBox;
    CBInvStairs: TCheckBox;
    CBColorEach: TCheckBox;
    CBDark3D: TCheckBox;
    ShColor: TShape;
    procedure FormShow(Sender: TObject);
    procedure BLineColorClick(Sender: TObject);
    procedure BLineBorderClick(Sender: TObject);
    procedure CBStairsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBLineBrushChange(Sender: TObject);
    procedure CBInvStairsClick(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
    procedure CBDark3DClick(Sender: TObject);
    procedure ShColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    CreatingForm:Boolean;
  public
    { Public declarations }
    TheSeries:TCustomSeries;
  end;

implementation

{$R *.DFM}
uses PenDlg, Teengine, TeePoEdi, TeeProcs;

procedure TCustomSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  TheSeries:=TCustomSeries(Tag);
  With TheSeries do
  Begin
    CBColorEach.Checked:=ColorEachPoint;
    SHColor.Brush.Color:=SeriesColor;

    if TheSeries is TLineSeries then
    begin
      CBLineBrush.ItemIndex:=Ord(LineBrush);
      CBStairs.Checked:=Stairs;
      CBInvStairs.Checked:=InvertedStairs;
      CBInvStairs.Enabled:=CBStairs.Checked;
      CBDark3D.Checked:=Dark3D;
    end
    else
    begin
      GPLine.Caption:='';
      ShowControls(False,[ BLineBorder,LPattern,CBLineBrush,
                           GBStair,CBDark3D]);
    end;
  end;
  SHColor.Cursor:=crTeeHand;
  if Parent<>nil then
     TeeInsertPointerForm(Parent,TheSeries.Pointer);
  CreatingForm:=False;
end;

procedure TCustomSeriesEditor.BLineColorClick(Sender: TObject);
begin
  With TheSeries do SeriesColor:=EditColor(Self,SeriesColor);
end;

procedure TCustomSeriesEditor.BLineBorderClick(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.LinePen);
end;

procedure TCustomSeriesEditor.CBStairsClick(Sender: TObject);
begin
  TheSeries.Stairs:=CBStairs.Checked;
  CBInvStairs.Enabled:=CBStairs.Checked;
end;

procedure TCustomSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  TheSeries:=nil;
end;

procedure TCustomSeriesEditor.CBLineBrushChange(Sender: TObject);
begin
  TheSeries.LineBrush:=TBrushStyle(CBLineBrush.ItemIndex);
end;

procedure TCustomSeriesEditor.CBInvStairsClick(Sender: TObject);
begin
  TheSeries.InvertedStairs:=CBInvStairs.Checked;
end;

procedure TCustomSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  TheSeries.ColorEachPoint:=CBColorEach.Checked;
end;

procedure TCustomSeriesEditor.CBDark3DClick(Sender: TObject);
begin
  TheSeries.Dark3D:=CBDark3D.Checked;
end;

procedure TCustomSeriesEditor.ShColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With TheSeries do
  begin
    SeriesColor:=EditColor(Self,SeriesColor);
    SHColor.Brush.Color:=SeriesColor;
  end;
end;

initialization
  RegisterClass(TCustomSeriesEditor);
end.

{**********************************************}
{   TGrid3DSeries Editor Dialog                }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeeGriEd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, TeeSurfa
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TGrid3DSeriesEditor = class(TForm)
    GroupBox1: TGroupBox;
    RGColor: TRadioButton;
    RGRange: TRadioButton;
    RGPalette: TRadioButton;
    Shape1: TShape;
    Label2: TLabel;
    Shape2: TShape;
    Label3: TLabel;
    Shape3: TShape;
    Label4: TLabel;
    SEPalette: TEdit;
    UDPalette: TUpDown;
    GroupBox2: TGroupBox;
    SEXGrid: TEdit;
    UDXGrid: TUpDown;
    SEZGrid: TEdit;
    UDZGrid: TUpDown;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EDepth: TEdit;
    UDDepth: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure SEZGridChange(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SEPaletteChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGColorClick(Sender: TObject);
    procedure RGRangeClick(Sender: TObject);
    procedure RGPaletteClick(Sender: TObject);
    procedure EDepthChange(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    Grid3D : TCustom3DGridSeries;
    Procedure RefreshShapes;
  public
    { Public declarations }
  end;

Procedure TeeInsertGrid3DForm(AParent:TControl; AGrid3D:TCustom3DGridSeries);

implementation

{$R *.DFM}
Uses PenDlg,BrushDlg,TeeProcs,TeeProco
     {$IFDEF D1}
     ,IEdit16
     {$ELSE}
     ,IEdiSeri
     {$ENDIF};

Procedure TeeInsertGrid3DForm(AParent:TControl; AGrid3D:TCustom3DGridSeries);
begin
  {$IFDEF D1}
  (GetOwnerForm(AParent) as TChartEditForm).InsertSeriesForm( TGrid3DSeriesEditor,
                                                        1,TeeMsg_Grid3D,
                                                        AGrid3D);
  {$ELSE}
  (AParent.Owner as TFormTeeSeries).InsertSeriesForm( TGrid3DSeriesEditor,
                                                     1,TeeMsg_Grid3D,
                                                     AGrid3D);
  {$ENDIF}
end;

procedure TGrid3DSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Grid3D:=TCustom3DGridSeries(Tag);
  With Grid3D do
  begin
    UDXGrid.Position:=NumXValues;
    UDZGrid.Position:=NumZValues;
    EnableControls(CanCreateValues,[UDXGrid,UDZGrid,SEXGrid,SEZGrid]);

    UDDepth.Position:=TimesZOrder;
    UDPalette.Position:=PaletteSteps;

    RGRange.Checked:=UseColorRange;
    RGPalette.Checked:=UsePalette;
    RGColor.Checked:=(not UseColorRange) and (not UsePalette);
    RefreshShapes;
  end;
  CreatingForm:=False;
end;

Procedure TGrid3DSeriesEditor.RefreshShapes;
Begin
  With Grid3D do
  Begin
    Shape1.Brush.Color:=SeriesColor;
    Shape2.Brush.Color:=StartColor;
    Shape3.Brush.Color:=EndColor;
  end;
end;

procedure TGrid3DSeriesEditor.SEZGridChange(Sender: TObject);
begin
  if not CreatingForm then
     Grid3D.CreateValues(UDXGrid.Position,UDZGrid.Position);
end;

procedure TGrid3DSeriesEditor.Shape1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With Grid3D do SeriesColor:=EditColor(Self,SeriesColor);
  RefreshShapes;
end;

procedure TGrid3DSeriesEditor.Shape2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With Grid3D do StartColor:=EditColor(Self,StartColor);
  RefreshShapes;
end;

procedure TGrid3DSeriesEditor.Shape3MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With Grid3D do EndColor:=EditColor(Self,EndColor);
  RefreshShapes;
end;

procedure TGrid3DSeriesEditor.SEPaletteChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEPalette.Text<>'' then
  begin
    Grid3D.PaletteSteps:=UDPalette.Position;
    Grid3D.Repaint;
  end;
end;

procedure TGrid3DSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  Shape1.Cursor:=crTeeHand;
  Shape2.Cursor:=crTeeHand;
  Shape3.Cursor:=crTeeHand;
end;

procedure TGrid3DSeriesEditor.RGColorClick(Sender: TObject);
begin
  With Grid3D do
  begin
    ColorEachPoint:=True;
    ColorEachPoint:=False;
    UseColorRange:=False;
    UsePalette:=False;
  end;
end;

procedure TGrid3DSeriesEditor.RGRangeClick(Sender: TObject);
begin
  Grid3D.UseColorRange:=True;
  Grid3D.UsePalette:=False;
end;

procedure TGrid3DSeriesEditor.RGPaletteClick(Sender: TObject);
begin
  Grid3D.UseColorRange:=False;
  Grid3D.UsePalette:=True;
end;

procedure TGrid3DSeriesEditor.EDepthChange(Sender: TObject);
begin
  if not CreatingForm then Grid3D.TimesZOrder:=UDDepth.Position;
end;

initialization
  RegisterClass(TGrid3DSeriesEditor);
end.

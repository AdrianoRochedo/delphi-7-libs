{****************************************}
{    TChartShape Editor Dialog           }
{ Copyright (c) 1996-98 by David Berneda }
{****************************************}
{$I teedefs.inc}
unit ShapeEdi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Chart, Buttons, Teengine, TeeShape;

type
  TChartShapeEditor = class(TForm)
    BShapePen: TButton;
    BShapeBrush: TButton;
    SHColor: TShape;
    SEX0: TEdit;
    Label1: TLabel;
    LX1: TLabel;
    SEX1: TEdit;
    Label2: TLabel;
    SEY0: TEdit;
    LY1: TLabel;
    SEY1: TEdit;
    Button1: TButton;
    RGAlign: TRadioGroup;
    Label5: TLabel;
    MemoText: TMemo;
    CBTrans: TCheckBox;
    BShapeColor: TButton;
    CBStyle: TComboBox;
    Label3: TLabel;
    procedure BShapePenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SHColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SEX0Change(Sender: TObject);
    procedure SEY0Change(Sender: TObject);
    procedure SEX1Change(Sender: TObject);
    procedure SEY1Change(Sender: TObject);
    procedure BShapeBrushClick(Sender: TObject);
    procedure MemoTextChange(Sender: TObject);
    procedure RGAlignClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBTransClick(Sender: TObject);
    procedure BShapeColorClick(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
  public
    { Public declarations }
    TheSeries:TChartShape;
  end;

implementation

{$R *.DFM}
Uses PenDlg, BrushDlg, TeeProcs;

procedure TChartShapeEditor.BShapePenClick(Sender: TObject);
begin
  EditPen(Self,TheSeries.Pen);
end;

procedure TChartShapeEditor.FormShow(Sender: TObject);
begin
  CreatingForm:=True;
  Screen.Cursor:=crDefault;
  if Tag=0 then exit;
  TheSeries:=TChartShape(Tag);
  SHColor.Brush.Color:=TheSeries.Brush.Color;
  SHColor.Cursor:=crTeeHand;
  CBStyle.ItemIndex:=Ord(TheSeries.Style);
  CBTrans.Checked:=TheSeries.Transparent;
  if TheSeries.Count=2 then
  With TheSeries do
  begin
    SEX0.Text:=FloatToStr(X0);
    SEY0.Text:=FloatToStr(Y0);
    SEX1.Text:=FloatToStr(X1);
    SEY1.Text:=FloatToStr(Y1);
  end;
  MemoText.Font.Assign(TheSeries.Font);
  MemoText.Lines:=TheSeries.Text;
  Case TheSeries.Alignment of
    taLeftJustify:  RGAlign.ItemIndex:=0;
    taCenter:       RGAlign.ItemIndex:=1;
    taRightJustify: RGAlign.ItemIndex:=2;
  end;
  CreatingForm:=False;
end;

procedure TChartShapeEditor.SHColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BShapeColorClick(Self);
end;

procedure TChartShapeEditor.SEX0Change(Sender: TObject);
begin
  if not CreatingForm then
  if SEX0.Text<>'' then TheSeries.X0:=StrToFloat(SEX0.Text);
end;

procedure TChartShapeEditor.SEY0Change(Sender: TObject);
begin
  if not CreatingForm then
  if SEY0.Text<>'' then
     TheSeries.Y0:=StrToFloat(SEY0.Text)
end;

procedure TChartShapeEditor.SEX1Change(Sender: TObject);
begin
  if not CreatingForm then
  if SEX1.Text<>'' then TheSeries.X1:=StrToFloat(SEX1.Text);
end;

procedure TChartShapeEditor.SEY1Change(Sender: TObject);
begin
  if not CreatingForm then
  if SEY1.Text<>'' then
     TheSeries.Y1:=StrToFloat(SEY1.Text)
end;

procedure TChartShapeEditor.BShapeBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,TheSeries.Brush);
  SHColor.Brush.Color:=TheSeries.Brush.Color;
end;

procedure TChartShapeEditor.MemoTextChange(Sender: TObject);
begin
  if not CreatingForm then TheSeries.Text:=MemoText.Lines;
end;

procedure TChartShapeEditor.RGAlignClick(Sender: TObject);
begin
  Case RGAlign.ItemIndex of
    0: TheSeries.Alignment:=taLeftJustify;
    1: TheSeries.Alignment:=taCenter;
    2: TheSeries.Alignment:=taRightJustify;
  end;
end;

procedure TChartShapeEditor.Button1Click(Sender: TObject);
begin
  InternalEditFont(Self,TheSeries.Font);
end;

procedure TChartShapeEditor.CBTransClick(Sender: TObject);
begin
  TheSeries.Transparent:=CBTrans.Checked;
end;

procedure TChartShapeEditor.BShapeColorClick(Sender: TObject);
begin
  With TheSeries do
  begin
    Brush.Color:=EditColor(Self,Brush.Color);
    SHColor.Brush.Color:=Brush.Color;
  end;
end;

procedure TChartShapeEditor.CBStyleChange(Sender: TObject);
begin
  TheSeries.Style:=TChartShapeStyle(CBStyle.ItemIndex);
end;

initialization
  RegisterClass(TChartShapeEditor);
end.

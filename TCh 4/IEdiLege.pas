{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiLege;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Chart, ComCtrls;

type
  TFormTeeLegend = class(TForm)
    CBShow: TCheckBox;
    BColor: TButton;
    BFont: TButton;
    BFrame: TButton;
    CBResizeChart: TCheckBox;
    CBInverted: TCheckBox;
    CBLegendStyle: TComboBox;
    CBLegStyle: TComboBox;
    L12: TLabel;
    L7: TLabel;
    L10: TLabel;
    L11: TLabel;
    SETopPos: TEdit;
    SEColWi: TEdit;
    BDivLines: TButton;
    GB1: TGroupBox;
    L25: TLabel;
    BShadow: TButton;
    SEShadowSize: TEdit;
    GB7: TGroupBox;
    L1: TLabel;
    RBLeft: TRadioButton;
    RBRight: TRadioButton;
    RBBottom: TRadioButton;
    RBTop: TRadioButton;
    SEMargin: TEdit;
    UDMargin: TUpDown;
    UDTopPos: TUpDown;
    UDColWi: TUpDown;
    UDShadowSize: TUpDown;
    procedure SEMarginChange(Sender: TObject);
    procedure CBLegendStyleChange(Sender: TObject);
    procedure SEColWiChange(Sender: TObject);
    procedure SETopPosChange(Sender: TObject);
    procedure BFontClick(Sender: TObject);
    procedure BColorClick(Sender: TObject);
    procedure CBLegStyleChange(Sender: TObject);
    procedure CBShowClick(Sender: TObject);
    procedure BFrameClick(Sender: TObject);
    procedure CBResizeChartClick(Sender: TObject);
    procedure CBInvertedClick(Sender: TObject);
    procedure BShadowClick(Sender: TObject);
    procedure SEShadowSizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBTopClick(Sender: TObject);
    procedure RBLeftClick(Sender: TObject);
    procedure RBRightClick(Sender: TObject);
    procedure RBBottomClick(Sender: TObject);
    procedure BDivLinesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    Procedure EnableLegendMarginControls;
  public
    { Public declarations }
    TheLegend:TChartLegend;
    Constructor CreateLegend(Owner:TComponent; ALegend:TChartLegend);
  end;

implementation

{$R *.DFM}
Uses Teengine, PenDlg, BrushDlg;

Constructor TFormTeeLegend.CreateLegend(Owner:TComponent; ALegend:TChartLegend);
begin
  inherited Create(Owner);
  TheLegend:=ALegend;
end;

procedure TFormTeeLegend.SEMarginChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheLegend do
  if Vertical then HorizMargin:=UDMargin.Position
              else VertMargin:=UDMargin.Position;
end;

procedure TFormTeeLegend.CBLegendStyleChange(Sender: TObject);
begin
  TheLegend.LegendStyle:=TLegendStyle(CBLegendStyle.ItemIndex);
end;

procedure TFormTeeLegend.SEColWiChange(Sender: TObject);
begin
  if not CreatingForm then TheLegend.ColorWidth:=UDColWi.Position;
end;

procedure TFormTeeLegend.SETopPosChange(Sender: TObject);
begin
  if not CreatingForm then TheLegend.TopPos:=UDTopPos.Position;
end;

procedure TFormTeeLegend.BFontClick(Sender: TObject);
begin
  With TheLegend do Font:=InternalEditFont(Self,Font);
end;

procedure TFormTeeLegend.BColorClick(Sender: TObject);
begin
  with TheLegend do Color:=EditColor(Self,Color);
end;

procedure TFormTeeLegend.CBLegStyleChange(Sender: TObject);
begin
  TheLegend.TextStyle:=TLegendTextStyle(CBLegStyle.ItemIndex);
end;

procedure TFormTeeLegend.CBShowClick(Sender: TObject);
begin
  TheLegend.Visible:=CBShow.Checked;
end;

procedure TFormTeeLegend.BFrameClick(Sender: TObject);
begin
  EditChartPen(Self,TheLegend.Frame);
end;

procedure TFormTeeLegend.CBResizeChartClick(Sender: TObject);
begin
  TheLegend.ResizeChart:=CBResizeChart.Checked;
end;

procedure TFormTeeLegend.CBInvertedClick(Sender: TObject);
begin
  TheLegend.Inverted:=CBInverted.Checked;
end;

procedure TFormTeeLegend.BShadowClick(Sender: TObject);
begin
  with TheLegend do ShadowColor:=EditColor(Self,ShadowColor);
end;

procedure TFormTeeLegend.SEShadowSizeChange(Sender: TObject);
begin
  if not CreatingForm then TheLegend.ShadowSize:=UDShadowSize.Position;
end;

procedure TFormTeeLegend.FormShow(Sender: TObject);
begin
  With TheLegend do
  begin
    UDTopPos.Position      :=TopPos;
    UDColWi.Position       :=ColorWidth;
    UDShadowSize.Position  :=ShadowSize;
    CBResizeChart.Checked  :=ResizeChart;
    CBLegendStyle.ItemIndex:=Ord(LegendStyle);
    CBLegStyle.ItemIndex   :=Ord(TextStyle);
    CBShow.Checked      :=Visible;
    Case Alignment of
      laTop   : RBTop.Checked:=True;
      laBottom: RBBottom.Checked:=True;
      laLeft  : RBLeft.Checked:=True;
      laRight : RBRight.Checked:=True;
    end;
    EnableLegendMarginControls;
  end;
  CreatingForm:=False;
end;

procedure TFormTeeLegend.RBTopClick(Sender: TObject);
begin
  TheLegend.Alignment:=laTop;
  EnableLegendMarginControls;
end;

procedure TFormTeeLegend.RBLeftClick(Sender: TObject);
begin
  TheLegend.Alignment:=laLeft;
  EnableLegendMarginControls;
end;

procedure TFormTeeLegend.RBRightClick(Sender: TObject);
begin
  TheLegend.Alignment:=laRight;
  EnableLegendMarginControls;
end;

procedure TFormTeeLegend.RBBottomClick(Sender: TObject);
begin
  TheLegend.Alignment:=laBottom;
  EnableLegendMarginControls;
end;

procedure TFormTeeLegend.BDivLinesClick(Sender: TObject);
begin
  EditChartPen(Self,TheLegend.DividingLines);
end;

Procedure TFormTeeLegend.EnableLegendMarginControls;
begin
  With TheLegend do
  if Vertical then UDMargin.Position:=HorizMargin
              else UDMargin.Position:=VertMargin;
end;

procedure TFormTeeLegend.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

end.

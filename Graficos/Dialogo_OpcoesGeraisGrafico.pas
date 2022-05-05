unit Dialogo_OpcoesGeraisGrafico;

 { AUTOR: .............................................. Adriano Rochedo Conceição
   DATA DA CRIAÇÃO: .................................... 16/08/1999

   HISTÓRICO:
     16/08/1999 - Rochedo
       - Primeira versão
 }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin, ExtCtrls, Chart, Mask, ComCtrls, Teengine;

type
  TgrDialogo_OpcoesGeraisGrafico = class(TForm)
    btnOk: TBitBtn;
    ColorDLG: TColorDialog;
    Book: TPageControl;
    TabSheet1: TTabSheet;
    L1: TLabel;
    Label1: TLabel;
    mmCab: TMemo;
    mmRodape: TMemo;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    cb3D: TCheckBox;
    Percent: TSpinEdit;
    TabSheet3: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    cbLegenda: TCheckBox;
    cb_EstiloTextoLegenda: TComboBox;
    cb_EstiloLegenda: TComboBox;
    TabSheet4: TTabSheet;
    Label5: TLabel;
    cbEixos: TCheckBox;
    cbParedes: TCheckBox;
    e_Zoom: TEdit;
    btnCor: TBitBtn;
    Label6: TLabel;
    PainelCor: TPanel;
    GB7: TGroupBox;
    Label7: TLabel;
    RBLeft: TRadioButton;
    RBRight: TRadioButton;
    RBBottom: TRadioButton;
    RBTop: TRadioButton;
    SEMargin: TEdit;
    UDMargin: TUpDown;
    L4: TLabel;
    L35: TLabel;
    L36: TLabel;
    LZoom: TLabel;
    LRotation: TLabel;
    LElevation: TLabel;
    Label8: TLabel;
    LHOffset: TLabel;
    Label9: TLabel;
    LVOffset: TLabel;
    Label10: TLabel;
    LPerspec: TLabel;
    SBZoom: TScrollBar;
    SBRotation: TScrollBar;
    SBElevation: TScrollBar;
    SBHOffset: TScrollBar;
    SBVOffset: TScrollBar;
    SBPerspec: TScrollBar;
    CBZoomText: TCheckBox;
    CBOrthogonal: TCheckBox;
    TabEixos: TTabSheet;
    CBShow: TCheckBox;
    RGWhich: TRadioGroup;
    CBVisible: TCheckBox;
    PageAxis: TPageControl;
    TabScales: TTabSheet;
    L5: TLabel;
    L8: TLabel;
    LAxisMax: TLabel;
    LAxisMin: TLabel;
    L6: TLabel;
    LAxisIncre: TLabel;
    CBAutomatic: TCheckBox;
    CBLogarithmic: TCheckBox;
    CBInverted: TCheckBox;
    CBAutoMax: TCheckBox;
    CBAutoMin: TCheckBox;
    BAxisMax: TButton;
    BAxisMin: TButton;
    BIncre: TButton;
    TabTitle: TTabSheet;
    L14: TLabel;
    L3: TLabel;
    L9: TLabel;
    ETitle: TEdit;
    SETitleAngle: TEdit;
    SETitleSize: TEdit;
    UDTitleSize: TUpDown;
    UDTitleAngle: TUpDown;
    TabLabels: TTabSheet;
    L20: TLabel;
    LabelAxisFormat: TLabel;
    L26: TLabel;
    L23: TLabel;
    CBLabels: TCheckBox;
    CBOnAxis: TCheckBox;
    CBRoundFirst: TCheckBox;
    EValuesFormat: TEdit;
    SESepar: TEdit;
    SELabelsSize: TEdit;
    SELabelsAngle: TEdit;
    RGLabelStyle: TRadioGroup;
    CBMultiline: TCheckBox;
    UDSepar: TUpDown;
    UDLabelsSize: TUpDown;
    UDLabelsAngle: TUpDown;
    TabPositions: TTabSheet;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    EPos: TEdit;
    EStart: TEdit;
    EEnd: TEdit;
    UDPos: TUpDown;
    UDStart: TUpDown;
    UDEnd: TUpDown;
    procedure cbEixosClick(Sender: TObject);
    procedure cb3DClick(Sender: TObject);
    procedure PercentChange(Sender: TObject);
    procedure mmRodapeChange(Sender: TObject);
    procedure mmCabChange(Sender: TObject);
    procedure cbParedesClick(Sender: TObject);
    procedure btnCorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLegendaClick(Sender: TObject);
    procedure cb_EstiloTextoLegendaChange(Sender: TObject);
    procedure cb_EstiloLegendaChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure e_ZoomKeyPress(Sender: TObject; var Key: Char);
    procedure e_ZoomExit(Sender: TObject);
    procedure SEMarginChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBLeftClick(Sender: TObject);
    procedure RBTopClick(Sender: TObject);
    procedure RBRightClick(Sender: TObject);
    procedure RBBottomClick(Sender: TObject);
    procedure CBOrthogonalClick(Sender: TObject);
    procedure CBZoomTextClick(Sender: TObject);
    procedure SBZoomChange(Sender: TObject);
    procedure SBRotationChange(Sender: TObject);
    procedure SBElevationChange(Sender: TObject);
    procedure SBHOffsetChange(Sender: TObject);
    procedure SBVOffsetChange(Sender: TObject);
    procedure SBPerspecChange(Sender: TObject);
    procedure CBShowClick(Sender: TObject);
    procedure RGWhichClick(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure PageAxisChange(Sender: TObject);
    procedure PageAxisChanging(Sender: TObject; var AllowChange: Boolean);
    procedure CBAutomaticClick(Sender: TObject);
    procedure CBAutoMaxClick(Sender: TObject);
    procedure CBLogarithmicClick(Sender: TObject);
    procedure CBInvertedClick(Sender: TObject);
    procedure ETitleChange(Sender: TObject);
    procedure SETitleAngleChange(Sender: TObject);
    procedure SETitleSizeChange(Sender: TObject);
    procedure CBLabelsClick(Sender: TObject);
    procedure CBMultilineClick(Sender: TObject);
    procedure CBOnAxisClick(Sender: TObject);
    procedure CBRoundFirstClick(Sender: TObject);
    procedure SESeparChange(Sender: TObject);
    procedure SELabelsSizeChange(Sender: TObject);
    procedure SELabelsAngleChange(Sender: TObject);
    procedure EValuesFormatChange(Sender: TObject);
    procedure RGLabelStyleClick(Sender: TObject);
    procedure EPosChange(Sender: TObject);
    procedure EStartChange(Sender: TObject);
    procedure EEndChange(Sender: TObject);
    procedure BAxisMaxClick(Sender: TObject);
    procedure BAxisMinClick(Sender: TObject);
    procedure BIncreClick(Sender: TObject);
  private
    FGrafico: TChart;
    FCreatingForm: boolean;
    FEixo : TCustomChartAxis;
    Procedure SetAxisScales();
    Procedure EnableLegendMarginControls();
  public
    property Grafico: TChart read FGrafico write FGrafico;
  end;

implementation
uses TeeConst, TeeProcs, AxMaxMin, AxisIncr;

{$R *.DFM}

procedure TgrDialogo_OpcoesGeraisGrafico.cbEixosClick(Sender: TObject);
begin
  FGrafico.AxisVisible := cbEixos.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.cb3DClick(Sender: TObject);
var tmp:Boolean;
begin
  With FGrafico do
  Begin
    View3D := cb3D.Checked;
    Percent.Enabled := View3D;
    CBOrthogonal.Enabled := View3D;
    tmp := View3D and not View3DOptions.Orthogonal;
    SBRotation.Enabled := tmp;
    SBElevation.Enabled := tmp;
    SBPerspec.Enabled := tmp;
    SBHOffset.Enabled := View3D;
    SBVOffset.Enabled := View3D;
    SBZoom.Enabled := View3D;
    CBZoomText.Enabled := View3D;
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.PercentChange(Sender: TObject);
begin
  if not FCreatingForm then
     FGrafico.Chart3DPercent := Percent.Value;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.mmRodapeChange(Sender: TObject);
begin
  FGrafico.Foot.Text.Assign(mmRodape.Lines);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.mmCabChange(Sender: TObject);
begin
  FGrafico.Title.Text.Assign(mmCab.Lines);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.cbParedesClick(Sender: TObject);
begin
  FGrafico.View3dWalls := cbParedes.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.btnCorClick(Sender: TObject);
begin
  if ColorDLG.Execute then
     begin
     PainelCor.Color := ColorDLG.Color;
     FGrafico.Color := ColorDLG.Color;
     end;end;

procedure TgrDialogo_OpcoesGeraisGrafico.FormShow(Sender: TObject);
var tmp: Integer;
begin
  mmCab.Lines.Text := (FGrafico.Title.Text.Text);
  mmRodape.Lines.Text := (FGrafico.Foot.Text.Text);

  PainelCor.Color   := FGrafico.Color;
  cbParedes.Checked := FGrafico.View3dWalls;
  cbEixos.Checked   := FGrafico.AxisVisible;

  // 3D
  with FGrafico do
    begin
    cb3D.Checked    := View3d;
    Percent.Enabled := View3D;
    Percent.Value   := Chart3dPercent;

    if Canvas.SupportsFullRotation then
       tmp := 1
    else
       tmp := 270;

    SBRotation.Min  := tmp;
    SBElevation.Min := tmp;

    CBOrthogonal.Enabled := View3D;

    With View3DOptions do
      begin
      SBZoom.Position      := Zoom;
      CBOrthogonal.Checked := Orthogonal;
      SBRotation.Position  := Rotation;
      SBElevation.Position := Elevation;
      SBHOffset.Position   := HorizOffset;
      SBVOffset.Position   := VertOffset;
      SBRotation.Enabled   := CBOrthogonal.Enabled and (not Orthogonal);
      CBZoomText.Checked   := ZoomText;
      SBPerspec.Position   := Perspective;
      end;
    end;

  with FGrafico.Legend do
    begin
    cb_EstiloTextoLegenda.ItemIndex := Ord(TextStyle);
    cb_EstiloLegenda.ItemIndex := Ord(LegendStyle);
    cbLegenda.Checked := Visible;
    Case Alignment of
      laTop   : RBTop.Checked := True;
      laBottom: RBBottom.Checked := True;
      laLeft  : RBLeft.Checked := True;
      laRight : RBRight.Checked := True;
      end;
    EnableLegendMarginControls();
    end;

  FEixo := FGrafico.LeftAxis;
  With FEixo.ParentChart do
  begin
    if FEixo=LeftAxis   then RGWhich.ItemIndex:=0 else
    if FEixo=RightAxis  then RGWhich.ItemIndex:=1 else
    if FEixo=TopAxis    then RGWhich.ItemIndex:=2 else
    if FEixo=BottomAxis then RGWhich.ItemIndex:=3 else
    if FEixo=DepthAxis  then RGWhich.ItemIndex:=4;
    CBShow.Checked:=AxisVisible;
  end;
  CBVisible.Checked:=FEixo.Visible;
  PageAxis.ActivePage:=TabScales;
  PageAxisChange(Self);

  FCreatingForm := False;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.cbLegendaClick(Sender: TObject);
begin
  FGrafico.Legend.Visible:= cbLegenda.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.cb_EstiloTextoLegendaChange(
  Sender: TObject);
begin
{TLegendTextStyle=(ltsPlain,ltsLeftValue,ltsRightValue,ltsLeftPercent,ltsRightPercent,ltsXValue);}
  FGrafico.Legend.TextStyle:= TLegendTextStyle(cb_EstiloTextoLegenda.ItemIndex);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.cb_EstiloLegendaChange(Sender: TObject);
begin
  FGrafico.Legend.LegendStyle := TLegendStyle(cb_EstiloLegenda.ItemIndex);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.btnOkClick(Sender: TObject);
begin
  FGrafico.Legend.Visible := not (cbLegenda.Checked);
  FGrafico.Legend.Visible := cbLegenda.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.e_ZoomKeyPress(Sender: TObject; var Key: Char);
begin
  Case Key of
    #48..#57, #8 : {nada} ;
  Else
    Key := #0;
  End;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.e_ZoomExit(Sender: TObject);
begin
  FGrafico.BottomAxis.Automatic:= False;
  FGrafico.Repaint();
end;

procedure TgrDialogo_OpcoesGeraisGrafico.EnableLegendMarginControls();
begin
  with FGrafico.Legend do
    if Vertical then
       UDMargin.Position := HorizMargin
    else
       UDMargin.Position := VertMargin;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SEMarginChange(Sender: TObject);
begin
  if not FCreatingForm then
     with FGrafico.Legend do
       if Vertical then
          HorizMargin := UDMargin.Position
       else
          VertMargin := UDMargin.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.FormCreate(Sender: TObject);
begin
  FCreatingForm := true;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.RBLeftClick(Sender: TObject);
begin
  FGrafico.Legend.Alignment := laLeft;
  EnableLegendMarginControls();
end;

procedure TgrDialogo_OpcoesGeraisGrafico.RBTopClick(Sender: TObject);
begin
  FGrafico.Legend.Alignment := laTop;
  EnableLegendMarginControls();
end;

procedure TgrDialogo_OpcoesGeraisGrafico.RBRightClick(Sender: TObject);
begin
  FGrafico.Legend.Alignment := laRight;
  EnableLegendMarginControls();
end;

procedure TgrDialogo_OpcoesGeraisGrafico.RBBottomClick(Sender: TObject);
begin
  FGrafico.Legend.Alignment := laBottom;
  EnableLegendMarginControls();
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBOrthogonalClick(Sender: TObject);
begin
  with FGrafico.View3DOptions do
    begin
    Orthogonal := CBOrthogonal.Checked;
    SBRotation.Enabled := (not Orthogonal);
    SBElevation.Enabled := not Orthogonal;
    SBPerspec.Enabled := not Orthogonal;
    end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBZoomTextClick(Sender: TObject);
begin
  FGrafico.View3DOptions.ZoomText := CBZoomText.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SBZoomChange(Sender: TObject);
begin
  FGrafico.View3DOptions.Zoom := SBZoom.Position;
  LZoom.Caption := IntToStr(SBZoom.Position) + '%';
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SBRotationChange(Sender: TObject);
begin
  FGrafico.View3DOptions.Rotation := SBRotation.Position;
  LRotation.Caption := IntToStr(SBRotation.Position);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SBElevationChange(Sender: TObject);
begin
  FGrafico.View3DOptions.Elevation := SBElevation.Position;
  LElevation.Caption := IntToStr(SBElevation.Position);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SBHOffsetChange(Sender: TObject);
begin
  FGrafico.View3DOptions.HorizOffset := SBHOffset.Position;
  LHOffset.Caption := IntToStr(SBHOffset.Position);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SBVOffsetChange(Sender: TObject);
begin
  FGrafico.View3DOptions.VertOffset := SBVOffset.Position;
  LVOffset.Caption := IntToStr(SBVOffset.Position);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SBPerspecChange(Sender: TObject);
begin
  FGrafico.View3DOptions.Perspective := SBPerspec.Position;
  LPerspec.Caption := IntToStr(SBPerspec.Position);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBShowClick(Sender: TObject);
begin
  FEixo.ParentChart.AxisVisible := TCheckBox(Sender).Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.RGWhichClick(Sender: TObject);
begin
  With FEixo.ParentChart do
  Case TRadioGroup(Sender).ItemIndex of
    0: FEixo:=LeftAxis;
    1: FEixo:=RightAxis;
    2: FEixo:=TopAxis;
    3: FEixo:=BottomAxis;
  else FEixo:=DepthAxis;
  end;
  CBVisible.Checked:=FEixo.Visible;
  FCreatingForm:=True;
  PageAxisChange(Self);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBVisibleClick(Sender: TObject);
begin
  FEixo.Visible := CBVisible.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.PageAxisChange(Sender: TObject);

  Procedure SetAxisLabels;
  var tmp : String;
  begin
    With FEixo do
    Begin
      CBLabels.Checked:=Labels;
      RGLabelStyle.ItemIndex:=Ord(LabelStyle);
      CBOnAxis.Checked:=LabelsOnAxis;
      CBRoundFirst.Checked:=RoundFirstLabel;
      UDLabelsAngle.Position:=LabelsAngle;
      UDSepar.Position:=LabelsSeparation;
      UDLabelsSize.Position:=LabelsSize;
      if IsDateTime then
      begin
        tmp:=DateTimeFormat;
        if tmp='' then tmp:=DateTimeDefaultFormat(Maximum-Minimum);
        EValuesFormat.Text:=tmp;
      end
      else
      begin
        tmp:=AxisValuesFormat;
        if tmp='' then tmp:=TeeMsg_DefValueFormat;
        EValuesFormat.Text:=DelphiToLocalFormat(tmp);
      end;
    end;
  end;

  Procedure SetAxisTitle;
  begin
    With FEixo do
    Begin
      ETitle.Text:=Title.Caption;
      UDTitleAngle.Position:=Title.Angle;
      UDTitleSize.Position:=TitleSize;
    end;
  end;

  Procedure SetAxisPositions;
  begin
    With FEixo do
    Begin
      UDPos.Position:=Round(PositionPercent);
      UDStart.Position:=Round(StartPosition);
      UDEnd.Position:=Round(EndPosition);
      EPos.Enabled:=not IsDepthAxis;
      EStart.Enabled:=not IsDepthAxis;
      EEnd.Enabled:=not IsDepthAxis;
    end;
  end;

begin
  With PageAxis do
  if ActivePage=TabLabels then SetAxisLabels else
  if ActivePage=TabScales then SetAxisScales else
  if ActivePage=TabTitle then SetAxisTitle else
  if ActivePage=TabPositions then SetAxisPositions;
  FCreatingForm:=False;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.PageAxisChanging(Sender: TObject; var AllowChange: Boolean);
begin
  FCreatingForm := True;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBAutomaticClick(Sender: TObject);
begin
  With FEixo do
  begin
    Automatic:=CBAutomatic.Checked;
    if Automatic then AdjustMaxMin
    else
    begin
      AutomaticMaximum:=False;
      AutomaticMinimum:=False;
    end;
  end;
  SetAxisScales;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBAutoMaxClick(Sender: TObject);
begin
  With FEixo do
  if Sender=CBAutoMax then AutomaticMaximum:=CBAutoMax.Checked
                      else AutomaticMinimum:=CBAutoMin.Checked;
  FEixo.AdjustMaxMin;
  SetAxisScales;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBLogarithmicClick(Sender: TObject);
begin
  try
    FEixo.Logarithmic:=CBLogarithmic.Checked;
  except
    on AxisException do
    Begin
      FEixo.Logarithmic:=False;
      CBLogarithmic.Checked:=False;
      Raise;
    end;
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBInvertedClick(Sender: TObject);
begin
  FEixo.Inverted := CBInverted.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.ETitleChange(Sender: TObject);
begin
  FEixo.Title.Caption := ETitle.Text;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SETitleAngleChange(Sender: TObject);
begin
  if not FCreatingForm then FEixo.Title.Angle:=UDTitleAngle.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SETitleSizeChange(Sender: TObject);
begin
  if not FCreatingForm then FEixo.TitleSize:=UDTitleSize.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBLabelsClick(Sender: TObject);
begin
  FEixo.Labels := CBLabels.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBMultilineClick(Sender: TObject);
begin
  FEixo.LabelsMultiLine:=CBMultiline.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBOnAxisClick(Sender: TObject);
begin
  FEixo.LabelsOnAxis:=CBOnAxis.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.CBRoundFirstClick(Sender: TObject);
begin
  FEixo.RoundFirstLabel:=CBRoundFirst.Checked;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SESeparChange(Sender: TObject);
begin
  if not FCreatingForm then FEixo.LabelsSeparation:=UDSepar.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SELabelsSizeChange(Sender: TObject);
begin
  if not FCreatingForm then FEixo.LabelsSize:=UDLabelsSize.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SELabelsAngleChange(Sender: TObject);
begin
  if not FCreatingForm then FEixo.LabelsAngle:=UDLabelsAngle.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.EValuesFormatChange(Sender: TObject);
begin
  With FEixo do
  if IsDateTime then DateTimeFormat:=EValuesFormat.Text
                else AxisValuesFormat:=LocalToDelphiFormat(EValuesFormat.Text);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.RGLabelStyleClick(Sender: TObject);
begin
  FEixo.LabelStyle:=TAxisLabelStyle(RGLabelStyle.ItemIndex);
end;

procedure TgrDialogo_OpcoesGeraisGrafico.EPosChange(Sender: TObject);
begin
  if not FCreatingForm then FEixo.PositionPercent:=UDPos.Position;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.EStartChange(Sender: TObject);
begin
  if not FCreatingForm then
  begin
    if UDStart.Position<FEixo.EndPosition then
       FEixo.StartPosition:=UDStart.Position
    else
       UDStart.Position:=Round(FEixo.StartPosition);
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.EEndChange(Sender: TObject);
begin
  if not FCreatingForm then
  begin
    if UDEnd.Position>FEixo.StartPosition then
       FEixo.EndPosition:=UDEnd.Position
    else
       UDEnd.Position:=Round(FEixo.EndPosition);
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.BAxisMaxClick(Sender: TObject);
begin
  With TAxisMaxMin.Create(Self) do
  try
    Caption:=TeeMsg_Maximum+' '+(FEixo.ParentChart as TCustomChart).AxisTitleOrName(FEixo);
    IsDateTime:=FEixo.IsDateTime;
    MaxMin:=FEixo.Maximum;
    if ShowModal=mrOk then
    Begin
      FEixo.Maximum:=MaxMin;
      CBAutoMax.Checked:=False;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.BAxisMinClick(Sender: TObject);
begin
  With TAxisMaxMin.Create(Self) do
  try
    Caption:=TeeMsg_Minimum+' '+(FEixo.ParentChart as TCustomChart).AxisTitleOrName(FEixo);
    IsDateTime:=FEixo.IsDateTime;
    MaxMin:=FEixo.Minimum;
    if ShowModal=mrOk then
    Begin
      FEixo.Minimum:=MaxMin;
      CBAutoMin.Checked:=False;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.BIncreClick(Sender: TObject);
begin
  With TAxisIncrement.Create(Self) do
  try
    //Caption:=Format(TeeMsg_DesiredIncrement,
    //               [(FEixo.ParentChart as TCustomChart).AxisTitleOrName(FEixo)]);
    IsDateTime:=FEixo.IsDateTime;
    IsExact:=FEixo.ExactDateTime;
    Increment:=FEixo.Increment;
    IStep:=FindDateTimeStep(Increment);
    if ShowModal=mrOk then
    Begin
      FEixo.Increment:=Increment;
      FEixo.ExactDateTime:=IsExact;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TgrDialogo_OpcoesGeraisGrafico.SetAxisScales();
begin
  With FEixo do
  Begin
    LAxisIncre.Caption:=GetIncrementText(Self,Increment,IsDateTime,True,AxisValuesFormat);
    if IsDateTime then
    begin
      if Minimum>=1 then LAxisMin.Caption:=DateTimeToStr(Minimum)
                    else LAxisMin.Caption:=TimeToStr(Minimum);
      if Maximum>=1 then LAxisMax.Caption:=DateTimeToStr(Maximum)
                    else LAxisMax.Caption:=TimeToStr(Maximum);
    end
    else
    begin
      LAxisMin.Caption:=FormatFloat(AxisValuesFormat,Minimum);
      LAxisMax.Caption:=FormatFloat(AxisValuesFormat,Maximum);
    end;
    CBAutomatic.Checked:=Automatic;
    CBAutoMax.Checked:=AutomaticMaximum;
    CBAutoMin.Checked:=AutomaticMinimum;
    CBLogarithmic.Checked:=Logarithmic;
    CBLogarithmic.Enabled:=not IsDepthAxis;
    CBInverted.Checked:=Inverted;
    { enable controls... }
    BAxisMax.Enabled:=(not Automatic) and (not AutomaticMaximum);
    BAxisMin.Enabled:=(not Automatic) and (not AutomaticMinimum);
    EnableControls(not Automatic,[CBAutoMax,CBAutoMin]);
  end;
end;

end.

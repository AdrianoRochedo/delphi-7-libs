{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiAxis;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Teengine;

type
  TFormTeeAxis = class(TForm)
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
    BTitleFont: TButton;
    SETitleAngle: TEdit;
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
    BLabelsFont: TButton;
    SELabelsSize: TEdit;
    SELabelsAngle: TEdit;
    RGLabelStyle: TRadioGroup;
    CBMultiline: TCheckBox;
    TabTicks: TTabSheet;
    L28: TLabel;
    L29: TLabel;
    L30: TLabel;
    L31: TLabel;
    BAxisPen: TButton;
    BTickPen: TButton;
    BTickInner: TButton;
    SEAxisTickLength: TEdit;
    SEInnerTicksLength: TEdit;
    BTickMinor: TButton;
    BAxisGrid: TButton;
    SEAxisMinorTickLen: TEdit;
    SEMinorCount: TEdit;
    CBTickOnLabels: TCheckBox;
    CBGridCentered: TCheckBox;
    SETitleSize: TEdit;
    UDInnerTicksLength: TUpDown;
    UDAxisTickLength: TUpDown;
    UDAxisMinorTickLen: TUpDown;
    UDMinorCount: TUpDown;
    UDTitleSize: TUpDown;
    UDTitleAngle: TUpDown;
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
    procedure BAxisGridClick(Sender: TObject);
    procedure BTickInnerClick(Sender: TObject);
    procedure BTickMinorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RGWhichClick(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure SEAxisTickLengthChange(Sender: TObject);
    procedure CBAutomaticClick(Sender: TObject);
    procedure ETitleChange(Sender: TObject);
    procedure BTitleFontClick(Sender: TObject);
    procedure BAxisMaxClick(Sender: TObject);
    procedure BAxisMinClick(Sender: TObject);
    procedure BIncreClick(Sender: TObject);
    procedure CBLogarithmicClick(Sender: TObject);
    procedure SEInnerTicksLengthChange(Sender: TObject);
    procedure SEAxisMinorTickLenChange(Sender: TObject);
    procedure SEMinorCountChange(Sender: TObject);
    procedure CBAutoMaxClick(Sender: TObject);
    procedure CBInvertedClick(Sender: TObject);
    procedure SETitleAngleChange(Sender: TObject);
    procedure SETitleSizeChange(Sender: TObject);
    procedure CBLabelsClick(Sender: TObject);
    procedure SELabelsAngleChange(Sender: TObject);
    procedure RGLabelStyleClick(Sender: TObject);
    procedure SELabelsSizeChange(Sender: TObject);
    procedure CBOnAxisClick(Sender: TObject);
    procedure SESeparChange(Sender: TObject);
    procedure CBRoundFirstClick(Sender: TObject);
    procedure EValuesFormatChange(Sender: TObject);
    procedure BLabelsFontClick(Sender: TObject);
    procedure BAxisPenClick(Sender: TObject);
    procedure BTickPenClick(Sender: TObject);
    procedure CBTickOnLabelsClick(Sender: TObject);
    procedure CBGridCenteredClick(Sender: TObject);
    procedure CBMultilineClick(Sender: TObject);
    procedure PageAxisChange(Sender: TObject);
    procedure CBShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageAxisChanging(Sender: TObject; var AllowChange: Boolean);
    procedure EPosChange(Sender: TObject);
    procedure EStartChange(Sender: TObject);
    procedure EEndChange(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    Procedure SetAxisScales;
  public
    { Public declarations }
    TheAxis : TCustomChartAxis;
    Constructor CreateAxis(Owner:TComponent; AAxis:TCustomChartAxis);
  end;

implementation

{$R *.DFM}
Uses TeeConst,TeeProcs,AxisIncr,PenDlg,BrushDlg,AxMaxMin,Chart;

Constructor TFormTeeAxis.CreateAxis(Owner:TComponent; AAxis:TCustomChartAxis);
begin
  inherited Create(Owner);
  TheAxis:=AAxis;
end;

procedure TFormTeeAxis.BAxisGridClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.Grid);
end;

procedure TFormTeeAxis.BTickInnerClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.TicksInner);
end;

procedure TFormTeeAxis.BTickMinorClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.MinorTicks);
end;

procedure TFormTeeAxis.FormShow(Sender: TObject);
begin
  With TheAxis.ParentChart do
  begin
    if TheAxis=LeftAxis   then RGWhich.ItemIndex:=0 else
    if TheAxis=RightAxis  then RGWhich.ItemIndex:=1 else
    if TheAxis=TopAxis    then RGWhich.ItemIndex:=2 else
    if TheAxis=BottomAxis then RGWhich.ItemIndex:=3 else
    if TheAxis=DepthAxis  then RGWhich.ItemIndex:=4;
    CBShow.Checked:=AxisVisible;
  end;
  CBVisible.Checked:=TheAxis.Visible;
  PageAxis.ActivePage:=TabScales;
  PageAxisChange(Self);
end;

procedure TFormTeeAxis.RGWhichClick(Sender: TObject);
begin
  With TheAxis.ParentChart do
  Case TRadioGroup(Sender).ItemIndex of
    0: TheAxis:=LeftAxis;
    1: TheAxis:=RightAxis;
    2: TheAxis:=TopAxis;
    3: TheAxis:=BottomAxis;
  else TheAxis:=DepthAxis;
  end;
  CBVisible.Checked:=TheAxis.Visible;
  CreatingForm:=True;
  PageAxisChange(Self);
end;

procedure TFormTeeAxis.CBVisibleClick(Sender: TObject);
begin
  TheAxis.Visible:=CBVisible.Checked;
end;

procedure TFormTeeAxis.SEAxisTickLengthChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.TickLength:=UDAxisTickLength.Position;
end;

procedure TFormTeeAxis.CBAutomaticClick(Sender: TObject);
begin
  With TheAxis do
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

procedure TFormTeeAxis.ETitleChange(Sender: TObject);
begin
  TheAxis.Title.Caption:=ETitle.Text;
end;

procedure TFormTeeAxis.BTitleFontClick(Sender: TObject);
begin
  With TheAxis.Title do Font:=InternalEditFont(Self,Font);
end;

procedure TFormTeeAxis.BAxisMaxClick(Sender: TObject);
begin
  With TAxisMaxMin.Create(Self) do
  try
    Caption:=TeeMsg_Maximum+' '+(TheAxis.ParentChart as TCustomChart).AxisTitleOrName(TheAxis);
    IsDateTime:=TheAxis.IsDateTime;
    MaxMin:=TheAxis.Maximum;
    if ShowModal=mrOk then
    Begin
      TheAxis.Maximum:=MaxMin;
      CBAutoMax.Checked:=False;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TFormTeeAxis.BAxisMinClick(Sender: TObject);
begin
  With TAxisMaxMin.Create(Self) do
  try
    Caption:=TeeMsg_Minimum+' '+(TheAxis.ParentChart as TCustomChart).AxisTitleOrName(TheAxis);
    IsDateTime:=TheAxis.IsDateTime;
    MaxMin:=TheAxis.Minimum;
    if ShowModal=mrOk then
    Begin
      TheAxis.Minimum:=MaxMin;
      CBAutoMin.Checked:=False;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TFormTeeAxis.BIncreClick(Sender: TObject);
begin
  With TAxisIncrement.Create(Self) do
  try
    Caption:=Format(TeeMsg_DesiredIncrement,
                   [(TheAxis.ParentChart as TCustomChart).AxisTitleOrName(TheAxis)]);
    IsDateTime:=TheAxis.IsDateTime;
    IsExact:=TheAxis.ExactDateTime;
    Increment:=TheAxis.Increment;
    IStep:=FindDateTimeStep(Increment);
    if ShowModal=mrOk then
    Begin
      TheAxis.Increment:=Increment;
      TheAxis.ExactDateTime:=IsExact;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TFormTeeAxis.CBLogarithmicClick(Sender: TObject);
begin
  try
    TheAxis.Logarithmic:=CBLogarithmic.Checked;
  except
    on AxisException do
    Begin
      TheAxis.Logarithmic:=False;
      CBLogarithmic.Checked:=False;
      Raise;
    end;
  end;
end;

procedure TFormTeeAxis.SEInnerTicksLengthChange(Sender: TObject);
begin
  if not CreatingForm then
     TheAxis.TickInnerLength:=UDInnerTicksLength.Position;
end;

procedure TFormTeeAxis.SEAxisMinorTickLenChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.MinorTickLength:=UDAxisMinorTickLen.Position;
end;

procedure TFormTeeAxis.SEMinorCountChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.MinorTickCount:=UDMinorCount.Position;
end;

procedure TFormTeeAxis.CBAutoMaxClick(Sender: TObject);
begin
  With TheAxis do
  if Sender=CBAutoMax then AutomaticMaximum:=CBAutoMax.Checked
                      else AutomaticMinimum:=CBAutoMin.Checked;
  TheAxis.AdjustMaxMin;
  SetAxisScales;
end;

procedure TFormTeeAxis.CBInvertedClick(Sender: TObject);
begin
  TheAxis.Inverted:=CBInverted.Checked;
end;

procedure TFormTeeAxis.SETitleAngleChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.Title.Angle:=UDTitleAngle.Position;
end;

procedure TFormTeeAxis.SETitleSizeChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.TitleSize:=UDTitleSize.Position;
end;

procedure TFormTeeAxis.CBLabelsClick(Sender: TObject);
begin
  TheAxis.Labels:=CBLabels.Checked;
end;

procedure TFormTeeAxis.SELabelsAngleChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.LabelsAngle:=UDLabelsAngle.Position;
end;

procedure TFormTeeAxis.RGLabelStyleClick(Sender: TObject);
begin
  TheAxis.LabelStyle:=TAxisLabelStyle(RGLabelStyle.ItemIndex);
end;

procedure TFormTeeAxis.SELabelsSizeChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.LabelsSize:=UDLabelsSize.Position;
end;

procedure TFormTeeAxis.CBOnAxisClick(Sender: TObject);
begin
  TheAxis.LabelsOnAxis:=CBOnAxis.Checked;
end;

procedure TFormTeeAxis.SESeparChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.LabelsSeparation:=UDSepar.Position;
end;

procedure TFormTeeAxis.CBRoundFirstClick(Sender: TObject);
begin
  TheAxis.RoundFirstLabel:=CBRoundFirst.Checked;
end;

procedure TFormTeeAxis.EValuesFormatChange(Sender: TObject);
begin
  With TheAxis do
  if IsDateTime then DateTimeFormat:=EValuesFormat.Text
                else AxisValuesFormat:=LocalToDelphiFormat(EValuesFormat.Text);
end;

procedure TFormTeeAxis.BLabelsFontClick(Sender: TObject);
begin
  With TheAxis do LabelsFont:=InternalEditFont(Self,LabelsFont);
end;

procedure TFormTeeAxis.BAxisPenClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.Axis);
end;

procedure TFormTeeAxis.BTickPenClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.Ticks);
end;

procedure TFormTeeAxis.CBTickOnLabelsClick(Sender: TObject);
begin
  TheAxis.TickOnLabelsOnly:=CBTickOnLabels.Checked;
end;

procedure TFormTeeAxis.CBGridCenteredClick(Sender: TObject);
begin
  TheAxis.GridCentered:=CBGridCentered.Checked;
end;

procedure TFormTeeAxis.CBMultilineClick(Sender: TObject);
begin
  TheAxis.LabelsMultiLine:=CBMultiline.Checked;
end;

Procedure TFormTeeAxis.SetAxisScales;
Begin
  With TheAxis do
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

procedure TFormTeeAxis.PageAxisChange(Sender: TObject);

  Procedure SetAxisLabels;
  var tmp : String;
  begin
         { Axis Labels  }
    With TheAxis do
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
        LabelAxisFormat.Caption:=TeeMsg_DateTimeFormat;
        tmp:=DateTimeFormat;
        if tmp='' then tmp:=DateTimeDefaultFormat(Maximum-Minimum);
        EValuesFormat.Text:=tmp;
      end
      else
      begin
        LabelAxisFormat.Caption:=TeeMsg_ValuesFormat;
        tmp:=AxisValuesFormat;
        if tmp='' then tmp:=TeeMsg_DefValueFormat;
        EValuesFormat.Text:=DelphiToLocalFormat(tmp);
      end;
    end;
  end;

  Procedure SetAxisTicks;
  begin
    With TheAxis do
    Begin
      UDAxisTickLength.Position:=TickLength;
      UDInnerTicksLength.Position:=TickInnerLength;
      UDAxisMinorTickLen.Position:=MinorTickLength;
      UDMinorCount.Position:=MinorTickCount;
      CBTickOnLabels.Checked:=TickOnLabelsOnly;
      CBGridCentered.Checked:=GridCentered;
    end;
  end;

  Procedure SetAxisTitle;
  begin
    With TheAxis do
    Begin
      ETitle.Text:=Title.Caption;
      UDTitleAngle.Position:=Title.Angle;
      UDTitleSize.Position:=TitleSize;
    end;
  end;

  Procedure SetAxisPositions;
  begin
    With TheAxis do
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
  if ActivePage=TabTicks then SetAxisTicks else
  if ActivePage=TabPositions then SetAxisPositions;
  CreatingForm:=False;
end;

procedure TFormTeeAxis.CBShowClick(Sender: TObject);
begin
  TheAxis.ParentChart.AxisVisible:= TCheckBox(Sender).Checked;
end;

procedure TFormTeeAxis.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  {$IFDEF D3}
  PageAxis.HotTrack:=True;
  {$ENDIF}
end;

procedure TFormTeeAxis.PageAxisChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  CreatingForm:=True;
end;

procedure TFormTeeAxis.EPosChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.PositionPercent:=UDPos.Position;
end;

procedure TFormTeeAxis.EStartChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    if UDStart.Position<TheAxis.EndPosition then
       TheAxis.StartPosition:=UDStart.Position
    else
       UDStart.Position:=Round(TheAxis.StartPosition);
  end;
end;

procedure TFormTeeAxis.EEndChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    if UDEnd.Position>TheAxis.StartPosition then
       TheAxis.EndPosition:=UDEnd.Position
    else
       UDEnd.Position:=Round(TheAxis.EndPosition);
  end;
end;

initialization
  RegisterClass(TFormTeeAxis);
end.

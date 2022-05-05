{****************************************}
{    TPolarSeries Editor Dialog          }
{ Copyright (c) 1996-98 by David Berneda }
{****************************************}
{$I teedefs.inc}
unit PolarEdi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, Series, TeePolar,
  Teengine
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE} 
  , ComCtrls
  {$ENDIF}
  ;

type
  TPolarSeriesEditor = class(TForm)
    CBCircled: TCheckBox;
    SERotation: TEdit;
    LRotation: TLabel;
    BPiePen: TButton;
    SEXRadius: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    SEYRadius: TEdit;
    Button2: TButton;
    CheckBox1: TCheckBox;
    LAngleInc: TLabel;
    SEAngleInc: TEdit;
    BBackPieColor: TBitBtn;
    Label8: TLabel;
    SERadiusInc: TEdit;
    UDYRadius: TUpDown;
    UDXRadius: TUpDown;
    UDRotation: TUpDown;
    UDRadiusInc: TUpDown;
    UDAngleInc: TUpDown;
    BBrush: TButton;
    GroupBox1: TGroupBox;
    Button1: TButton;
    CBAngleLabels: TCheckBox;
    CBLabelsRot: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CBCircledClick(Sender: TObject);
    procedure SERotationChange(Sender: TObject);
    procedure BPiePenClick(Sender: TObject);
    procedure SEXRadiusChange(Sender: TObject);
    procedure SEYRadiusChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SEAngleIncChange(Sender: TObject);
    procedure BBackPieColorClick(Sender: TObject);
    procedure SERadiusIncChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BBrushClick(Sender: TObject);
    procedure CBAngleLabelsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBLabelsRotClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
  public
    { Public declarations }
    Polar:TCustomPolarSeries;
  end;

implementation

{$R *.DFM}
Uses BrushDlg,PenDlg,TeePoEdi;

procedure TPolarSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Polar:=TCustomPolarSeries(Tag);
  With Polar do
  begin
    CBCircled.Checked       :=Circled;
    CheckBox1.Checked       :=CloseCircle;
    UDRotation.Position     :=RotationAngle;
    UDXRadius.Position      :=CustomXRadius;
    UDYRadius.Position      :=CustomYRadius;
    UDAngleInc.Position     :=Round(AngleIncrement);
    UDRadiusInc.Position    :=Round(RadiusIncrement);
    CBAngleLabels.Checked   :=CircleLabels;
    CBLabelsRot.Checked     :=CircleLabelsRotated;
    ShowControls(Polar is TPolarSeries,[ LRotation,UDRotation,SERotation,
                                         LAngleInc,UDAngleInc,SEAngleInc]);
  end;
  TeeInsertPointerForm(Parent,Polar.Pointer);
  CreatingForm:=False;
end;

procedure TPolarSeriesEditor.CBCircledClick(Sender: TObject);
begin
  Polar.Circled:=CBCircled.Checked;
end;

procedure TPolarSeriesEditor.SERotationChange(Sender: TObject);
begin
  if not CreatingForm then Polar.RotationAngle:=UDRotation.Position;
end;

procedure TPolarSeriesEditor.BPiePenClick(Sender: TObject);
begin
  EditChartPen(Self,Polar.CirclePen);
end;

procedure TPolarSeriesEditor.SEXRadiusChange(Sender: TObject);
begin
  if not CreatingForm then Polar.CustomXRadius:=UDXRadius.Position;
end;

procedure TPolarSeriesEditor.SEYRadiusChange(Sender: TObject);
begin
  if not CreatingForm then Polar.CustomYRadius:=UDYRadius.Position;
end;

procedure TPolarSeriesEditor.Button2Click(Sender: TObject);
begin
  EditChartPen(Self,Polar.Pen);
  Polar.SeriesColor:=Polar.Pen.Color;
end;

procedure TPolarSeriesEditor.CheckBox1Click(Sender: TObject);
begin
  Polar.CloseCircle:=CheckBox1.Checked;
end;

procedure TPolarSeriesEditor.SEAngleIncChange(Sender: TObject);
begin
  if not CreatingForm then Polar.AngleIncrement:=UDAngleInc.Position;
end;

procedure TPolarSeriesEditor.BBackPieColorClick(Sender: TObject);
begin
  Polar.CircleBackColor:=EditColor(Self,Polar.CircleBackColor);
end;

procedure TPolarSeriesEditor.SERadiusIncChange(Sender: TObject);
begin
  if not CreatingForm then Polar.RadiusIncrement:=UDRadiusInc.Position;
end;

procedure TPolarSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

procedure TPolarSeriesEditor.BBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,Polar.Brush);
end;

procedure TPolarSeriesEditor.CBAngleLabelsClick(Sender: TObject);
begin
  Polar.CircleLabels:=CBAngleLabels.Checked;
end;

procedure TPolarSeriesEditor.Button1Click(Sender: TObject);
begin
  With Polar do CircleLabelsFont:=InternalEditFont(Self,CircleLabelsFont);
end;

procedure TPolarSeriesEditor.CBLabelsRotClick(Sender: TObject);
begin
  Polar.CircleLabelsRotated:=CBLabelsRot.Checked;
end;

initialization
  RegisterClass(TPolarSeriesEditor);
end.

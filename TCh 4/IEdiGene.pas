{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiGene;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Chart, ComCtrls;

type
  TFormTeeGeneral = class(TForm)
    GBZoom: TGroupBox;
    LSteps: TLabel;
    CBAllowZoom: TCheckBox;
    CBAnimatedZoom: TCheckBox;
    SEAniZoomSteps: TEdit;
    RGPanning: TRadioGroup;
    CBClipPoints: TCheckBox;
    BPrint: TButton;
    BExport: TButton;
    GBMargins: TGroupBox;
    SETopMa: TEdit;
    SELeftMa: TEdit;
    SEBotMa: TEdit;
    SERightMa: TEdit;
    UDTopMa: TUpDown;
    UDRightMa: TUpDown;
    UDLeftMa: TUpDown;
    UDBotMa: TUpDown;
    UDAniZoomSteps: TUpDown;
    procedure BPrintClick(Sender: TObject);
    procedure CBAllowZoomClick(Sender: TObject);
    procedure CBClipPointsClick(Sender: TObject);
    procedure CBAnimatedZoomClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BExportClick(Sender: TObject);
    procedure RGPanningClick(Sender: TObject);
    procedure SEAniZoomStepsChange(Sender: TObject);
    procedure SERightMaChange(Sender: TObject);
    procedure SETopMaChange(Sender: TObject);
    procedure SEBotMaChange(Sender: TObject);
    procedure SELeftMaChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    Function ChangeMargin(UpDown:TUpDown; APos,OtherSide:Integer):Integer;
  public
    { Public declarations }
    TheChart:TCustomChart;
    Constructor CreateChart(Owner:TComponent; AChart:TCustomChart);
  end;

{ Show the Export dialog }
Procedure ChartExport(AOwner:TForm; AChart:TCustomChart);

implementation

{$R *.DFM}
Uses TeeProcs, TeePrevi, TeExport, TeeStore;

Procedure TeeExportSaveChart(ExportPanel:TCustomTeePanel; Const AFileName:String);
begin
  if ExportPanel is TCustomChart then
     SaveChartToFile(TCustomChart(ExportPanel),AFileName)
  else
     SaveTeeToFile(ExportPanel,AFileName);
end;

Procedure ChartExport(AOwner:TForm; AChart:TCustomChart);
begin
  With TTeeExportForm.Create(AOwner) do
  try
    ExportPanel:=AChart;
    TeeExportSave:=TeeExportSaveChart;
    try
      ShowModal;
    finally
      TeeExportSave:=nil;
    end;
  finally
    Free;
  end;
end;

{ Chart General }
Constructor TFormTeeGeneral.CreateChart(Owner:TComponent; AChart:TCustomChart);
begin
  inherited Create(Owner);
  TheChart:=AChart;
end;

procedure TFormTeeGeneral.BPrintClick(Sender: TObject);
begin
  ChartPreview(Self,TheChart);
end;

procedure TFormTeeGeneral.CBAllowZoomClick(Sender: TObject);
begin
  TheChart.AllowZoom:=CBAllowZoom.Checked;
end;

procedure TFormTeeGeneral.CBClipPointsClick(Sender: TObject);
begin
  TheChart.ClipPoints:=CBClipPoints.Checked;
end;

procedure TFormTeeGeneral.CBAnimatedZoomClick(Sender: TObject);
begin
  TheChart.AnimatedZoom:=CBAnimatedZoom.Checked;
end;

procedure TFormTeeGeneral.FormShow(Sender: TObject);
begin
  With TheChart do
  begin
    RGPanning.ItemIndex    :=Ord(AllowPanning);
    CBClipPoints.Checked   :=ClipPoints;

    UDTopMa.Position       :=MarginTop;
    UDLeftMa.Position      :=MarginLeft;
    UDBotMa.Position       :=MarginBottom;
    UDRightMa.Position     :=MarginRight;

    CBAllowZoom.Checked    :=AllowZoom;
    CBAnimatedZoom.Checked :=AnimatedZoom;
    UDAniZoomSteps.Position:=AnimatedZoomSteps;
  end;
  CreatingForm:=False;
end;

procedure TFormTeeGeneral.BExportClick(Sender: TObject);
begin
  ChartExport(Self,TheChart);
end;

procedure TFormTeeGeneral.RGPanningClick(Sender: TObject);
begin
  TheChart.AllowPanning:=TPanningMode(RGPanning.ItemIndex);
end;

procedure TFormTeeGeneral.SEAniZoomStepsChange(Sender: TObject);
begin
  if not CreatingForm then TheChart.AnimatedZoomSteps:=UDAniZoomSteps.Position;
end;

Function TFormTeeGeneral.ChangeMargin(UpDown:TUpDown; APos,OtherSide:Integer):Integer;
begin
  result:=APos;
  if not CreatingForm then
  With UpDown do
  if Position+OtherSide<100 then result:=Position
                            else Position:=APos;
end;

procedure TFormTeeGeneral.SERightMaChange(Sender: TObject);
begin
  With TheChart do MarginRight:=ChangeMargin(UDRightMa,MarginRight,MarginLeft);
end;

procedure TFormTeeGeneral.SETopMaChange(Sender: TObject);
begin
  With TheChart do MarginTop:=ChangeMargin(UDTopMa,MarginTop,MarginBottom);
end;

procedure TFormTeeGeneral.SEBotMaChange(Sender: TObject);
begin
  With TheChart do MarginBottom:=ChangeMargin(UDBotMa,MarginBottom,MarginTop);
end;

procedure TFormTeeGeneral.SELeftMaChange(Sender: TObject);
begin
  With TheChart do MarginLeft:=ChangeMargin(UDLeftMa,MarginLeft,MarginRight);
end;

procedure TFormTeeGeneral.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

end.

unit TeeAx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,Chart,StdCtrls;

type
  TTeeChart = class(TCustomPanel)
  private
    { Private declarations }
    FChart:TChart;
    FChartFile:String;
    tmpPanel:TPanel;
    tmpEditButton,tmpSaveButton,
    tmpLoadButton:TButton;
  protected
    { Protected declarations }
    Procedure SetChartFile(Const Value:String); virtual;
    Function GetToolVisible:Boolean;
    Procedure SetToolVisible(Value:Boolean);
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
    Procedure ClickEdit(Sender:TObject);
    Procedure ClickSave(Sender:TObject);
    Procedure ClickLoad(Sender:TObject);
  published
    { Published declarations }
    property Chart:TChart read FChart write FChart;
    property ChartFile:String read FChartFile write SetChartFile;
    property ToolbarVisible:Boolean read GetToolVisible write SetToolVisible;
  end;

procedure Register;

implementation

uses teeStore,Series,ArrowCha,BubbleCh,CandleCh,EditChar,StatChar,
     TeeProCo,ChartPro;

Constructor TTeeChart.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  tmpPanel:=TPanel.Create(Self);
  With tmpPanel do
  begin
    Align:=alTop;
    Caption:='';
    Parent:=Self;
  end;
  tmpEditButton:=TButton.Create(Self);
  With tmpEditButton do
  begin
    Caption:='&Edit...';
    OnClick:=ClickEdit;
    Top:=6;
    Left:=8;
    Parent:=tmpPanel;
    Enabled:=False;
  end;
  tmpSaveButton:=TButton.Create(Self);
  With tmpSaveButton do
  begin
    Caption:='&Save...';
    OnClick:=ClickSave;
    Top:=6;
    Left:=88;
    Parent:=tmpPanel;
    Enabled:=False;
  end;
  tmpLoadButton:=TButton.Create(Self);
  With tmpLoadButton do
  begin
    Caption:='&Load...';
    OnClick:=ClickLoad;
    Top:=6;
    Left:=168;
    Parent:=tmpPanel;
  end;
  FChart:=TChart.Create(Self);
  FChart.Parent:=Self;
  FChart.Align:=alClient;
//  FChart.AddSeries(TBarSeries.Create(Self));
//  FChart[0].FillSampleValues(8);
end;

Function TTeeChart.GetToolVisible:Boolean;
begin
  result:=tmpPanel.Visible;
end;

Procedure TTeeChart.SetToolVisible(Value:Boolean);
begin
  tmpPanel.Visible:=Value;
end;

Procedure TTeeChart.ClickSave(Sender:TObject);
begin
  if Assigned(FChart) then SaveChartDialog(FChart);
  tmpSaveButton.Enabled:=True;
end;

Procedure TTeeChart.ClickLoad(Sender:TObject);
begin
  With TOpenDialog.Create(nil) do
  try
    Filter:=TeeMsg_TeeFiles+
          ' (*.' +
          TeeMsg_TeeExtension +
          ')|*.' +
          TeeMsg_TeeExtension;
    Options:=[ofHideReadOnly];
    if Execute then ChartFile:=FileName;
  finally
    Free;
  end;
end;

Procedure TTeeChart.ClickEdit(Sender:TObject);
begin
  EditChart(nil,FChart);
  tmpSaveButton.Enabled:=True;
end;

Procedure TTeeChart.SetChartFile(Const Value:String);
var tmp:TCustomChart;
begin
  tmpEditButton.Enabled:=False;
  tmpSaveButton.Enabled:=False;
  if Value<>FChartFile then
  begin
    FChartFile:=Value;
    if Assigned(FChart) then FChart.Free;
    if FChartFile<>'' then
    begin
      FChart:=TChart.Create(Self);
      tmp:=FChart;
      LoadChartFromFile(tmp,FChartFile);
      FChart.Parent:=Self;
      FChart.Align:=alClient;
      tmpEditButton.Enabled:=True;
      tmpSaveButton.Enabled:=True;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('TeeChart', [TTeeChart]);
end;

end.

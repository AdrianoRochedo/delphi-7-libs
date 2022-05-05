unit Optimizer_Form_ObjectiveFunctionViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, Series, Buttons;

type
  TfoObjectiveFunctionViewer = class(TForm)
    Grafico: TChart;
    paPai: TPanel;
    paVA: TPanel;
    Panel3: TPanel;
    paDP: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    paM: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FOwner : TObject;
  public
    constructor Create(aOwner: Pointer);
    procedure Clear;
    procedure setValue(const x, y: Real);
    procedure setMean(const Value: real);
    procedure setDP(const Value: real);
  end;

implementation
uses SysUtilsEx,
     Optimizer_Base;

{$R *.DFM}

{ TfoObjectiveFunctionViewer }

constructor TfoObjectiveFunctionViewer.Create(aOwner: Pointer);
begin
  inherited Create(nil);
  FOwner := aOwner;
  Grafico.AddSeries(TFastLineSeries.Create(self));
end;

procedure TfoObjectiveFunctionViewer.setValue(const x, y: Real);
begin
  With Grafico.Series[0] do
    begin
    if Count > 400 then Delete(0);
    AddXY(x, y, '', clTeeColor);
    paVA.Caption := '  ' + toString(y);
    paVA.Refresh();
    end;
  Grafico.Repaint();
end;

procedure TfoObjectiveFunctionViewer.FormClose(Sender: TObject; var Action: TCloseAction);
var i: Integer;
begin
  Action := caFree;
  for i := 0 to TOptimizer(FOwner).ObjectivesCount-1 do
    if TOptimizer(FOwner).OF_Viwers[i] = self then
       TOptimizer(FOwner).OF_Viwers[i] := nil;
end;

procedure TfoObjectiveFunctionViewer.FormShow(Sender: TObject);
begin
  Grafico.Series[0].Clear;
  Repaint();
end;

procedure TfoObjectiveFunctionViewer.Clear();
begin
  Grafico.Series[0].Clear;
end;

procedure TfoObjectiveFunctionViewer.setDP(const Value: real);
begin
  paDP.Caption := '  ' + toString(Value);
  paDP.Refresh();
end;

procedure TfoObjectiveFunctionViewer.setMean(const Value: real);
begin
  paM.Caption := '  ' + toString(Value);
  paM.Refresh();
end;

end.

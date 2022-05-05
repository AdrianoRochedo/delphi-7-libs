unit Optimizer_Frame_ParManager;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Gauges, ExtCtrls;

type
  TfrParManager = class(TFrame)
    paName: TPanel;
    paValue: TPanel;
    paMarca: TPanel;
    Panel4: TPanel;
    Gauge: TGauge;
    paTP: TPanel;
    procedure paNameDblClick(Sender: TObject);
  private
    FLL: Real;
    FHL: Real;
    procedure SetMax(const Value: Real);
    procedure SetMin(const Value: Real);
    procedure SetName(const Value: String);
    procedure SetValue(const Value: Real);
    procedure CalculateGaugeHint;
    procedure SetOF_Status(const Value: Char);
    procedure SetStepLen(const Value: Real);
    procedure SetColor(const Value: TColor);
  public
    property ParName   : String           write SetName;
    property Value     : Real             write SetValue;
    property Min       : Real    read FLL write SetMin;
    property Max       : Real    read FHL write SetMax;
    property OF_Status : Char             write SetOF_Status;
    property StepLen   : Real             write SetStepLen;
    property Color     : TColor           write SetColor;
  end;

implementation
uses Optimizer_Base;

{$R *.dfm}

{ TfrParManager }

procedure TfrParManager.SetMax(const Value: Real);
begin
  FHL := Value;
  Gauge.MaxValue := Trunc(Value*1000);
  CalculateGaugeHint;
end;

procedure TfrParManager.SetMin(const Value: Real);
begin
  FLL := Value;
  Gauge.MinValue := Trunc(Value*1000);
  CalculateGaugeHint;
end;                

procedure TfrParManager.SetName(const Value: String);
begin
  paName.Caption := '   ' + Value;
end;

procedure TfrParManager.SetValue(const Value: Real);
begin
  Gauge.Progress := Trunc(Value * 1000);
  paValue.Caption := FormatFloat('0.###', Value);
  paValue.Refresh();
end;

procedure TfrParManager.paNameDblClick(Sender: TObject);
var p: TParameter;
begin
  p := TParameter(Tag);
  if p.ParForm = nil then
     p.Show(100, 100)
  else
     p.ParForm.Show;
end;

procedure TfrParManager.CalculateGaugeHint;
begin
  Gauge.Hint := 'Limite Inferior: ' + FormatFloat('0.###', FLL) + #13 +
                'Limite Superior: ' + FormatFloat('0.###', FHL);
end;

procedure TfrParManager.SetOF_Status(const Value: Char);
begin
  paMarca.Caption := Value;
  paMarca.Refresh();
end;

procedure TfrParManager.SetStepLen(const Value: Real);
begin
  paTP.Caption := FormatFloat('0.##', Value);
  paTP.Refresh();
end;

procedure TfrParManager.SetColor(const Value: TColor);
begin
  Gauge.ForeColor := Value;
end;

end.

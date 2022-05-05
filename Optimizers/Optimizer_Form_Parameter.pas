unit Optimizer_Form_Parameter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gauges, ExtCtrls;

type
  TfoParameter = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label6: TLabel;
    L_Value: TLabel;
    L_Min: TLabel;
    L_Max: TLabel;
    L_Step: TLabel;
    L_Tolerance: TLabel;
    PL1: TPanel;
    Gauge: TGauge;
    Label16: TLabel;
    Label17: TLabel;
    Leds: TImage;
    Label9: TLabel;
    Label12: TLabel;
    L_OF_Status: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FOwner : TObject;

    procedure SetMax(const Value: Real);
    procedure SetMin(const Value: Real);
    procedure SetName(const Value: String);
    procedure SetStep(const Value: Real);
    procedure SetTolerance(const Value: Real);
    procedure SetValue(const Value: Real);
    procedure SetOF_Status(const Value: Char);
    { Private declarations }
  public
    constructor Create(aOwner: Pointer);

    property ParName      : String  write SetName;
    property Value        : Real    write SetValue;
    property Min          : Real    write SetMin;
    property Max          : Real    write SetMax;
    property Step         : Real    write SetStep;
    property Tolerance    : Real    write SetTolerance;
    property OF_Status    : Char    write SetOF_Status;
  end;

implementation
uses Optimizer_Base;

{$R *.DFM}

{ TrbFormParameter }

constructor TfoParameter.Create(aOwner: Pointer);
begin
  inherited Create(nil);
  FOwner := aOwner;
end;

procedure TfoParameter.SetMax(const Value: Real);
begin
  L_Max.Caption := FloatToStrF(Value, ffFixed, 10, 4);
  Gauge.MaxValue := Trunc(Value*1000);
end;

procedure TfoParameter.SetMin(const Value: Real);
begin
  L_Min.Caption := FloatToStrF(Value, ffFixed, 10, 4);
  Gauge.MinValue := Trunc(Value*1000);
end;

procedure TfoParameter.SetName(const Value: String);
begin
  Caption := Value;
end;

procedure TfoParameter.SetStep(const Value: Real);
begin
  L_Step.Caption := FloatToStrF(Value, ffFixed, 10, 4);
end;

procedure TfoParameter.SetTolerance(const Value: Real);
begin
  L_Tolerance.Caption := FloatToStrF(Value, ffFixed, 10, 4);
end;

procedure TfoParameter.SetValue(const Value: Real);
begin
  L_Value.Caption := FloatToStrF(Value, ffFixed, 10, 4);
  Gauge.Progress := Trunc(Value*1000);
end;

procedure TfoParameter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  TParameter(FOwner).ParForm := nil;
end;

procedure TfoParameter.SetOF_Status(const Value: Char);
begin
  L_OF_Status.Caption := Value;
end;

procedure TfoParameter.FormCreate(Sender: TObject);
begin
  L_OF_Status.Caption := '';
end;

end.

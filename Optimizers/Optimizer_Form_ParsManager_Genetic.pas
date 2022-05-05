unit Optimizer_Form_ParsManager_Genetic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Optimizer_Form_ParsManager, ExtCtrls;

type
  TfoParsManager_Genetic = class(TfoParsManager)
    Panel2: TPanel;
    paECA: TPanel;
    Panel12: TPanel;
    paE: TPanel;
    Panel14: TPanel;
    Panel3: TPanel;
    paCA: TPanel;
  private
    procedure SetComplexCount(const Value: Integer);
    procedure SetComplexEvolutionCount(const Value: Integer);
    procedure SetEvolutionCount(const Value: Integer);
  public
    property EvolutionCount        : Integer write SetEvolutionCount;
    property ComplexCount          : Integer write SetComplexCount;
    property ComplexEvolutionCount : Integer write SetComplexEvolutionCount;
  end;

implementation

{$R *.dfm}

{ TfoParsManager_Genetic }

procedure TfoParsManager_Genetic.SetComplexCount(const Value: Integer);
begin
  paCA.Caption := ' ' + IntToStr(Value);
  paCA.Refresh();
end;

procedure TfoParsManager_Genetic.SetComplexEvolutionCount(const Value: Integer);
begin
  paECA.Caption := ' ' + IntToStr(Value);
  paECA.Refresh();
end;

procedure TfoParsManager_Genetic.SetEvolutionCount(const Value: Integer);
begin
  paE.Caption := ' ' + IntToStr(Value);
  paE.Refresh();
end;

end.

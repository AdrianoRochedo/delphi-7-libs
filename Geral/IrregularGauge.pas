unit IrregularGauge;

interface
uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Types, Graphics;

type
  TigLine = record
              P1, P2: Types.TPoint;
            end;

  TigLines = array of TigLine;

  TigVerticalLines = array of TigLines;

  // Nesta implementação o gauge varia de baixo para cima (0 a 100%)
  TIrregularGauge = class(TBitmap)
  private
    FImageHeight: integer;
    FImageBase: integer;
    FPercent: real;
    FImageTop: integer;
    FyCurr: Integer;
    procedure setPercent(const Value: real);
    procedure DoUpdate(y: Integer);
  protected
    FVerticalLines: TigVerticalLines;

    // Responsável por iniciar os dados necessários (FVerticalLines, ImageTop, ImageHeight)
    procedure Init; virtual; abstract;
  public
    constructor Create; override;

    // coordenada y equivalente a 100%
    property ImageTop : integer read FImageTop write FImageTop;

    // Tamanho da variação do gauge (--> base - topo + 1)
    property ImageHeight : integer read FImageHeight write FImageHeight;

    // Valor do gauge
    property Percent : real read FPercent write setPercent;
  end;

implementation

{ TIrregularGauge }

constructor TIrregularGauge.Create;
begin
  inherited;
  Canvas.Pen.Mode := pmNotXor;
  Init;
  FImageBase := FImageTop + FImageHeight;
  FyCurr := FImageHeight-1;
end;

procedure TIrregularGauge.DoUpdate(y: Integer);
var yAtual, i, k: Integer;
    c: TCanvas;
begin
  c := self.Canvas;
  yAtual := FImageHeight - y;

  if yAtual <> FyCurr then
     if yAtual > FyCurr then
        for k := FyCurr to yAtual-1 do
          for i := 0 to High(FVerticalLines[k]) do
            begin
            c.MoveTo(FVerticalLines[k][i].P1.x, FVerticalLines[k][i].P1.y);
            c.LineTo(FVerticalLines[k][i].P2.x, FVerticalLines[k][i].P2.y);
            end
     else
        for k := FyCurr-1 downto yAtual do
          for i := 0 to High(FVerticalLines[k]) do
            begin
            c.MoveTo(FVerticalLines[k][i].P1.x, FVerticalLines[k][i].P1.y);
            c.LineTo(FVerticalLines[k][i].P2.x, FVerticalLines[k][i].P2.y);
            end;

  FyCurr := yAtual;
end;

procedure TIrregularGauge.setPercent(const Value: real);
begin
  if FPercent <> Value then
     begin
     if Value > 100 then Percent := 100 else
     if Value < 0   then Percent := 0
     else
        FPercent := Value;
        
     DoUpdate(Trunc(FImageHeight * FPercent / 100));
     end;
end;

end.

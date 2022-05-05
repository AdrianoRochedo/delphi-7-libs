unit Principal;

interface

uses
  Controls, Forms, StdCtrls, Classes, SysUtils, SysUtilsEx, ExtCtrls, TeeProcs,
  TeEngine, Chart, Series, drEdit, Graphics, Types, Extpanel,
  Optimizer_Interfaces, 
  GeneticMO_Optimizer;

type
  TfoPrincipal = class(TForm, IOptimizable)
    Saida: TMemo;
    Label1: TLabel;
    edPop: TdrEdit;
    Label2: TLabel;
    edNME: TdrEdit;
    Label4: TLabel;
    mmPars: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    btnTestar: TButton;
    btnPars: TButton;
    btnParar: TButton;
    P: TExtPanel;
    Label3: TLabel;
    edEscala: TdrEdit;
    Label7: TLabel;
    edSim: TdrEdit;
    procedure btnTestarClick(Sender: TObject);
    procedure btnPararClick(Sender: TObject);
    procedure btnParsClick(Sender: TObject);
    procedure PPaint(Sender: TObject);
  private
    FOpt: TGeneticMO_Optimizer;
    FPontos: array of TPoint;
    FDx, FDy: integer;
    FEscala: integer;

    procedure PrepararPainel();
    procedure MostrarPop();
    function getX(rx: real): integer;
    function getY(ry: real): integer;

    // IOptimizable interfaces
    procedure o_setOptimizer(obj: TObject);
    procedure o_ProcessMessage(MessageID: integer);
    procedure o_beginOptimization();
    procedure o_endOptimization();
    procedure o_CalculateObjetiveFunctions();
  public
    { Public declarations }
  end;

var
  foPrincipal: TfoPrincipal;

implementation

{$R *.dfm}

procedure TfoPrincipal.o_beginOptimization();
begin
  // nada
end;

procedure TfoPrincipal.o_endOptimization();
begin
  // nada
end;

procedure TfoPrincipal.o_CalculateObjetiveFunctions();
var x, y, F1, F2: real;
begin
  x := FOpt.Parameters[0].Value;
  y := FOpt.Parameters[1].Value;

  F1 := SQR(x-1) + SQR(y-2); // 0 --> (1, 2)
  F2 := SQR(x-2) + SQR(y-1); // 0 --> (2, 1)

  FOpt.setOFValue(0, F1);
  FOpt.setOFValue(1, F2);
end;

procedure TfoPrincipal.o_setOptimizer(obj: TObject);
begin
  // nada
end;

procedure TfoPrincipal.o_ProcessMessage(MessageID: integer);
var rx, ry: real;
    x, y: integer;
    i: integer;
begin
  case MessageID of
    MO_Evolution:
      begin
      Saida.Lines.Add('');
      Saida.Lines.Add('Evolução: ' + toString(FOpt.EvolutionCount));
      Saida.Lines.Add('');
      MostrarPop();
      end;

    MO_OFCalc:
      begin
      // Obtem a posição do indivíduo na população
      i := FOpt.Parameters.IndividualPos;

      // Obtém os genes do Indivíduo
      rx := FOpt.Parameters[0].Value;
      ry := FOpt.Parameters[1].Value;

      // Apaga o ponto anterior
      p.Canvas.Pixels[FPontos[i].x, FPontos[i].y] := clYellow;

      // Faz a transformação de escala
      x := getX(rx);
      y := getY(ry);

      // Desenha o novo ponto
      p.Canvas.Pixels[x, y] := clYellow;

      // Salva o ponto para poder ser apagado posteriormente
      FPontos[i] := Point(x, y);

      // Mostra o número de simulações
      edSim.AsInteger := FOpt.SimulationCount;
      end;
    end;
end;

procedure TfoPrincipal.btnTestarClick(Sender: TObject);
var s1, s2: string;
    i: integer;
begin
  FEscala := edEscala.AsInteger;
  Saida.Clear();
  PrepararPainel();
  setLength(FPontos, edPop.AsInteger);
  for i := 0 to High(FPontos) do
    FPontos[i] := Point(1000, 1000);

  // Criacao do mecanismo de otimizacao
  FOpt := TGeneticMO_Optimizer.Create();

  // Configuracoes Gerais
  FOpt.PopulationCount := edPop.AsInteger;
  FOpt.PopEvolutionCount := edNME.AsInteger;
  FOpt.ObjectivesCount := 2;

  // Parâmetros X
  SysUtilsEx.SubStrings('|', s1, s2, mmPars.Lines[0], true);
  FOpt.Parameters.Add('x', strToInt(s1), strToInt(s2));

  // Parâmetros Y
  SysUtilsEx.SubStrings('|', s1, s2, mmPars.Lines[1], true);
  FOpt.Parameters.Add('y', strToInt(s1), strToInt(s2));

  // mostra aa FOs
  FOpt.Show_OFViwer(0, 54, 450);
  FOpt.Show_OFViwer(1, 54, 600);

  // Otimizacao
  btnTestar.Enabled := false;
  try
    FOpt.Optimize(self);
  finally
    btnTestar.Enabled := true;
    FreeAndNil(FOpt);
  end;  
end;

procedure TfoPrincipal.btnPararClick(Sender: TObject);
begin
  if FOpt <> nil then
     FOpt.Stop();
end;

procedure TfoPrincipal.btnParsClick(Sender: TObject);
begin
  if FOpt <> nil then
     FOpt.Show_ParsManager(300, 600);
end;

procedure TfoPrincipal.PrepararPainel();
var i, dx: Integer;
begin
  FEscala := edEscala.AsInteger;
  FDx := p.Width div 2;
  FDy := p.Height div 2;

  // Limpa o Painel
  P.Canvas.Brush.Color := P.Color;
  P.Canvas.FillRect(P.ClientRect);

  // Desenha os dois pontos centrais
  P.Canvas.Brush.Color := clRed;
  P.Canvas.Ellipse(getX(1)-2, getY(2)-2, getX(1)+2, getY(2)+2);
  P.Canvas.Brush.Color := clLime;
  P.Canvas.Ellipse(getX(2)-2, getY(1)-2, getX(2)+2, getY(1)+2);

  // Desenha os circulos concêntricos
  dx := 20;
  P.Canvas.Brush.Style := bsClear;
  for i := 1 to 20 do
    begin
    P.Canvas.Pen.Style := psSolid;
    P.Canvas.Ellipse(getX(1)-dx, getY(2)-dx, getX(1)+dx, getY(2)+dx);
    P.Canvas.Pen.Style := psDot;
    P.Canvas.Ellipse(getX(2)-dx, getY(1)-dx, getX(2)+dx, getY(1)+dx);
    inc(dx, 20);
    end;

  // Muda o modo da caneta para desenho dos indivíduos
  P.Canvas.Pen.Mode := pmXOR;
end;

function TfoPrincipal.getX(rx: real): integer;
begin
  result := trunc(rx * FEscala) + FDx {- FEscala};
end;

function TfoPrincipal.getY(ry: real): integer;
begin
  result := trunc(ry * FEscala) + FDy {- FEscala};
end;

procedure TfoPrincipal.PPaint(Sender: TObject);
begin
  PrepararPainel();
end;

procedure TfoPrincipal.MostrarPop();
var i: Integer;
    rx, ry, F1, F2: string;
begin
  Saida.Lines.BeginUpdate();
  for i := 0 to FOpt.PopulationCount-1 do
    begin
    F1 := toString(FOpt.getAptness(i, 0), 3);
    F2 := toString(FOpt.getAptness(i, 1), 3);

    rx := toString(FOpt.getGen(i, 0), 3);
    ry := toString(FOpt.getGen(i, 1), 3);

    Saida.Lines.Add(
      'x: '  + SysUtilsEx.LeftStr(rx, 8) +
      'y: '  + SysUtilsEx.LeftStr(ry, 8) +
      'F1: ' + SysUtilsEx.LeftStr(F1, 8) +
      'F2: ' + SysUtilsEx.LeftStr(F2, 8)
      );
    end;
  Saida.Lines.EndUpdate();  
end;

end.

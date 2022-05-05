unit Principal;

interface

uses
  Controls, Forms, StdCtrls, Classes, SysUtils, SysUtilsEx, ExtCtrls, TeeProcs,
  TeEngine, Chart, Series, drEdit,
  Optimizer_Interfaces,
  Genetic_Optimizer;

type
  TfoPrincipal = class(TForm, IOptimizable)
    Saida: TMemo;
    c1: TChart;
    Label1: TLabel;
    edNC: TdrEdit;
    Label2: TLabel;
    edNEC: TdrEdit;
    Label4: TLabel;
    mmPars: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    btnTestar: TButton;
    btnPars: TButton;
    btnParar: TButton;
    procedure btnTestarClick(Sender: TObject);
    procedure btnPararClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnParsClick(Sender: TObject);
  private
    FOpt: TGeneticOptimizer;
    FNumEvolucoes: integer;
    FPontos: integer;
    p1: TFastLineSeries;
    p2: TFastLineSeries;
    p3: TFastLineSeries;

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
var x: real;
begin
  x := FOpt.Parameters[0].Value +
       FOpt.Parameters[1].Value +
       FOpt.Parameters[2].Value ;

  FOpt.setOFValue(0, x);
end;

procedure TfoPrincipal.o_setOptimizer(obj: TObject);
begin
  // nada
end;

procedure TfoPrincipal.o_ProcessMessage(MessageID: integer);
begin
  if FOpt.EvolutionCount <> FNumEvolucoes then
     begin
     FNumEvolucoes := FOpt.EvolutionCount;
     Saida.Lines.Add('');
     Saida.Lines.Add('Evolução: ' + toString(FNumEvolucoes));
     Saida.Lines.Add('');
     end;

  Saida.Lines.Add(Format('p1: %f  p2: %f  p3: %f',
                         [FOpt.Parameters[0].Value,
                          FOpt.Parameters[1].Value,
                          FOpt.Parameters[2].Value]));

  inc(FPontos);
  if p1.Count > 200 then p1.Delete(0);
  p1.AddXY(FPontos, FOpt.Parameters[0].Value);

  if p2.Count > 200 then p2.Delete(0);
  p2.AddXY(FPontos, FOpt.Parameters[1].Value);

  if p3.Count > 200 then p3.Delete(0);
  p3.AddXY(FPontos, FOpt.Parameters[2].Value);

  c1.Repaint();
end;

procedure TfoPrincipal.btnTestarClick(Sender: TObject);
var s1, s2: string;
    i: integer;
    Min, Max: real;
begin
  Saida.Clear();
  p1.Clear();
  p2.Clear();
  p3.Clear();

  FPontos := 0;
  FNumEvolucoes := 0;

  // Criacao do mecanismo de otimizacao
  FOpt := TGeneticOptimizer.Create();

  // Configuracoes Gerais
  FOpt.ComplexCount := edNC.AsInteger;
  FOpt.PopEvolutionCount := edNEC.AsInteger;

  // Parameters a serem otimizados
  for i := 0 to mmPars.Lines.Count-1 do
    begin
    SysUtilsEx.SubStrings('-', s1, s2, mmPars.Lines[i], true);
    FOpt.Parameters.Add('P' + toString(i+1), strToInt(s1), strToInt(s2));
    end;

  // Define uma faixa de variação fixa para o gráfico
  FOpt.Parameters.getMinMax(Min, Max);
  c1.LeftAxis.Automatic := false;
  c1.LeftAxis.Minimum := Min - 5;
  c1.LeftAxis.Maximum := Max + 5;

  // mostra a FO
  FOpt.Show_OFViwer(0, 54, 400);

  try
    // Otimizacao
    FOpt.Optimize(self);
  finally
    // Destruicao
    FreeAndNil(FOpt);
  end;  
end;

procedure TfoPrincipal.btnPararClick(Sender: TObject);
begin
  if FOpt <> nil then
     FOpt.Stop();
end;

procedure TfoPrincipal.FormCreate(Sender: TObject);
begin
  c1.Title.Text.Clear();

  p1 := TFastLineSeries.Create(c1);
  p1.ParentChart := c1;
  p1.Title := 'P1';

  p2 := TFastLineSeries.Create(c1);
  p2.ParentChart := c1;
  p2.Title := 'P2';

  p3 := TFastLineSeries.Create(c1);
  p3.ParentChart := c1;
  p3.Title := 'P3';
end;

procedure TfoPrincipal.btnParsClick(Sender: TObject);
begin
  if FOpt <> nil then
     FOpt.Show_ParsManager(100, 100);
end;

end.

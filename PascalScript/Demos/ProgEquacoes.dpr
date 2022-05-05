program ProgEquacoes;

uses
  Forms,
  SysUtils,
  Equacoes in 'Equacoes.pas' {Form1},
  Frame_Planilha in '..\..\Frames\Frame_Planilha.pas' {FramePlanilha: TFrame},
  drGraficosBase in '..\..\Graficos\drGraficosBase.pas' {grGraficoBase},
  drGraficos in '..\..\Graficos\drGraficos.pas' {grGrafico};

{$R *.res}

begin
  SysUtils.DecimalSeparator := '.';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

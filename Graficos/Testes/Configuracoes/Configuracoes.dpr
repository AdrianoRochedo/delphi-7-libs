program Configuracoes;

uses
  Forms,
  Chart.GlobalSetup in '..\..\Chart.GlobalSetup.pas',
  Chart.GlobalSetupForm in '..\..\Chart.GlobalSetupForm.pas' {foGlobalSetup};

{$R *.res}

var x: TGlobalSetup;
begin
  Application.Initialize();

  x := TGlobalSetup.Create('Configura��es Globais dos Gr�ficos');

  x.LoadFromFile('D:\ConfiguracoesGlobais.xml');
  x.Execute();
  x.SaveToFile('D:\ConfiguracoesGlobais.xml');

  x.Free();
end.

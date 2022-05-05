program AlgoGen_Prog;

uses
  Forms,
  Principal in 'Principal.pas' {foPrincipal},
  Optimizer_Interfaces in '..\..\Optimizer_Interfaces.pas',
  Optimizer_Base in '..\..\Optimizer_Base.pas',
  Optimizer_Frame_ParManager in '..\..\Optimizer_Frame_ParManager.pas' {frParManager: TFrame},
  Optimizer_Form_Parameter in '..\..\Optimizer_Form_Parameter.pas' {foParameter},
  Optimizer_Form_ParsManager in '..\..\Optimizer_Form_ParsManager.pas' {foParsManager},
  Optimizer_Form_ObjectiveFunctionViewer in '..\..\Optimizer_Form_ObjectiveFunctionViewer.pas' {foObjectiveFunctionViewer},
  Genetic_Optimizer in '..\..\Genetic_Optimizer.pas',
  Optimizer_Form_ParsManager_Rosen in '..\..\Optimizer_Form_ParsManager_Rosen.pas' {foParsManager_Rosen},
  Optimizer_Form_ParsManager_Genetic in '..\..\Optimizer_Form_ParsManager_Genetic.pas' {foParsManager_Genetic};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoPrincipal, foPrincipal);
  Application.Run;
end.

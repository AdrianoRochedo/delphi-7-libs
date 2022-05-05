program HidroSimulator;

{%ToDo 'HidroSimulator.todo'}

uses
  Forms,
  HidroSimulator.MainForm in 'HidroSimulator.MainForm.pas' {MainForm},
  Application_Class in '..\..\Geral\Application_Class.pas' {Application: TDataModule},
  HidroSimulator.Application in 'HidroSimulator.Application.pas' {HidroApplication: TDataModule},
  Rochedo.Simulators.Components in '..\Rochedo.Simulators.Components.pas',
  Rochedo.Simulators.Designer in '..\Rochedo.Simulators.Designer.pas' {Designer},
  Rochedo.Simulators.Shapes in '..\Rochedo.Simulators.Shapes.pas',
  HidroSimulator.Classes in 'HidroSimulator.Classes.pas';

{$R *.res}

begin
  TSystem.Run(
    THidroApplication.Create('Hidro Simulator', '1.00')
  );
end.

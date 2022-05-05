program SimulatorsDemo;

{%ToDo 'SimulatorsDemo.todo'}

uses
  Types,
  Forms,
  Frame_Memo in '..\..\Frames\Frame_Memo.pas' {fr_Memo: TFrame},
  StringsDialog in '..\..\Dialogs\StringsDialog.pas' {StringsDialog},
  Main in 'Main.pas' {foMain},
  Rochedo.Simulators.Designer in '..\Rochedo.Simulators.Designer.pas' {Designer},
  Rochedo.Simulators.Components in '..\Rochedo.Simulators.Components.pas',
  Rochedo.Simulators.Shapes in '..\Rochedo.Simulators.Shapes.pas',
  Rochedo.Simulators.Dialogs.Strings in '..\Rochedo.Simulators.Dialogs.Strings.pas' {SC_StringsDialog};

{$R *.res}

var MainForm : TfoMain;
    Designer : TDesigner;
    c        : TSimulatorComponent;
begin
  Application.CreateForm(TfoMain, MainForm);
  
  Designer := TDesigner.Create();

    Designer.setProject(MainForm);
    Designer.setToolBox(MainForm);
    Designer.setPainter(MainForm);
    Designer.setRules(MainForm);

      c := TStringListComponent.Create( Designer );
      c.Component.setPos( Point(50, 50) );

      c := TTriangleComponent.Create( Designer );
      c.Component.setPos( Point(150, 50) );

      c := TEllipseComponent.Create( Designer );
      c.Component.setPos( Point(150, 150) );

      c := TArrowComponent.Create( Designer );
      c.Component.setPos( Point(250, 250) );

      c := TFlagComponent.Create( Designer );
      c.Component.setPos( Point(300, 50) );

    Designer.Show();
      Application.Run();
    Designer.Free()
end.

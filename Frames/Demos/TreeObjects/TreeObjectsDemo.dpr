program TreeObjectsDemo;

uses
  Forms,
  Main in 'Main.pas' {foMain},
  Frame_TreeViewObjects in '..\..\Frame_TreeViewObjects.pas' {TreeViewObjects: TFrame},
  TreeViewUtils in '..\..\..\Lib\TreeViewUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.

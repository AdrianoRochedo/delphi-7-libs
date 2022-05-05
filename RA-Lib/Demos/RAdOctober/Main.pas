unit Main;

interface

uses SysUtils, Forms;


  procedure Run;

  
implementation

uses RAUtils, RAVersion, RAFDAppBuilder, fSplash;


var
  RAAppBuilder: TRAAppBuilder;

procedure Run;
begin
  RAFDDebug := HasSwitch('Debug');
  Application.Initialize;
  RAAppBuilder := TRAAppBuilder.Create(Application);
  RAAppBuilder.CreateAppBuilderWindow;
  Application.Icon := AppBuilder.Icon;
  Application.Title := 'RAd October ' + RALibVersionString;
  RAAppBuilder.IniFile := ChangeFileExt(Application.ExeName, '.ini');
  RAAppBuilder.Caption := Application.Title;
  ShowSplash;
  RAAppBuilder.Execute;
  HideSplash;
  Application.Run;
  RAAppBuilder.Free;
end;

end.

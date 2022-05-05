program Demo_Excel_DLL;

uses
  ShareMem,
  Forms,
  Main in 'Main.pas' {Form1},
  Excel_DLL in '..\..\Excel_DLL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

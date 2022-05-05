unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses drPascal;

{$R *.DFM}

type
  TPontoDeEntrada = procedure (Lib: TLib); stdcall;

procedure TForm1.Button1Click(Sender: TObject);
var h: THandle;
    p: TPontoDeEntrada;
    x: TPascalScript;
begin
  x := TPascalScript.Create(False);
  x.Text.LoadFromFile('F:\Projetos\Comum\PascalScript\Temp\Teste DLL\Teste.pScript');

  h := LoadLibrary('API.DLL');
  if h <> 0 then p := GetProcAddress(h, 'PontoDeEntrada');
  if @p <> nil then
     begin
     p(x.Lib);
     x.Execute;
     end;

  x.Free;
  FreeLibrary(h);
end;

end.

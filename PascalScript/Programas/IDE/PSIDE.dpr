program PSIDE;

{
  As bibliotecas do PascalScript precisam estar definidas no arquivo
  "PascalScriptLibs.ini" localizado na Pasta do executavel ou/e dentro
  da sub-pasta "DLLs".
}




uses
  ShareMem,
  Forms,
  psEditor;

{$R *.RES}

begin
  Application.Initialize();
  RunScript([], ParamSTR(1), nil, [], nil);
end.

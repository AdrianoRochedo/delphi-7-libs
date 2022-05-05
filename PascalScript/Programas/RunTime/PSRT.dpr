program PSRT;

{
  As bibliotecas do PascalScript precisam estar definidas no arquivo
  "PascalScriptLibs.ini" localizado na Pasta do executavel ou/e dentro
  da sub-pasta "DLLs".
}

uses
  ShareMem, // Borlandmm precisa ser distribuida <-----
  psCore,
  SysUtils,
  dialogs,
  Forms;

{$R *.RES}

var p: TPascalScript;
    i: Integer;
    s: string;
begin
  if paramCount > 0 then
     try
       s := ExtractFilePath(Application.ExeName);
       p := TPascalScript.Create();
       p.Lib.Load_APIs(s + 'PascalScriptLibs.ini', s + 'DLLs\');
       p.Code.LoadFromFile( paramStr(1) );

       if p.Compile() then
          try
            p.Execute()
          except
            on E: Exception do ShowMessage(E.Message);
          end
       else
          ShowMessage(p.Errors.Text);
     finally
       p.Free();
     end
  else
     ShowMessage('USO: PSRT "Nome Arquivo"');
end.

program Register;

{$APPTYPE CONSOLE}

uses
  Registry,
  Windows,
  SysUtils,
  classes;

var r: TRegistry;
    s: String;
    SL: TStrings;
begin
  r := TRegistry.Create;
  s := getCurrentDir;

  SL := TStringList.Create;
  SL.Add(s + '\DLLs');
  SL.SaveToFile(s + '\Programas\Config.Ini');
  SL.Free;

  r.OpenKey('.PScript', True);
    r.WriteString('', 'PascalScript');
  r.CloseKey;

  r.OpenKey('PascalScript', True);
    r.OpenKey('Shell', True);
      r.OpenKey('Execute', True);
        r.OpenKey('Command', True);
          r.WriteString('', s + '\Programas\PSRT.exe "%1"');
        r.CloseKey;

  r.OpenKey('PascalScript', True);
    r.OpenKey('Shell', True);
      r.OpenKey('Edit', True);
        r.OpenKey('Command', True);
          r.WriteString('', s + '\Programas\IDE.exe "%1"');
        r.CloseKey;

  r.Free;
  WriteLN('Registrado com Sucesso !');
end.

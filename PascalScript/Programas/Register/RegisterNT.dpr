program RegisterNT;

{$APPTYPE CONSOLE}

uses
  Registry,
  Windows,
  SysUtils,
  Classes;

var r : TRegistry;
    s : String;
    SL: TStrings;
begin
  r := TRegistry.Create;
  s := getCurrentDir;

  SL := TStringList.Create;
  SL.Add(s + '\DLLs');
  SL.SaveToFile(s + '\Programas\Config.Ini');
  SL.Free;

  r.RootKey := HKEY_CLASSES_ROOT;

  r.OpenKey('.PScript', True);
    r.WriteString('', 'PScript');
  r.CloseKey;

  r.OpenKey('PScript', True);
    r.WriteString('', 'Pascal Script');
    r.WriteString('AlwaysShowExt', '');
    r.WriteInteger('BrowserFlags', 8);
    r.WriteInteger('EditFlags', 0);
  r.CloseKey;

  r.OpenKey('PScript', True);
    r.OpenKey('DefaultIcon', True);
      r.WriteString('', 'shell32.dll,21');
  r.CloseKey;

  r.OpenKey('PScript', True);
    r.OpenKey('Shell', True);
      r.WriteString('', 'Execute');
  r.CloseKey;

  r.OpenKey('PScript', True);
    r.OpenKey('Shell', True);
      r.OpenKey('Edit', True);
        r.OpenKey('Command', True);
          r.WriteString('', s + '\Programas\IDE.exe "%1"');
  r.CloseKey;

  r.OpenKey('PScript', True);
    r.OpenKey('Shell', True);
      r.OpenKey('Execute', True);
        r.OpenKey('Command', True);
          r.WriteString('', s + '\Programas\PSRT.exe "%1"');
  r.CloseKey;

  // ----------------------------------------------------------------

  r.RootKey := HKEY_LOCAL_MACHINE;

  r.OpenKey('Software\Classes\.PScript', True);
    r.WriteString('', 'PScript');
  r.CloseKey;

  r.OpenKey('Software\Classes\PScript', True);
    r.WriteString('', 'Pascal Script');
    r.WriteString('AlwaysShowExt', '');
    r.WriteInteger('BrowserFlags', 8);
    r.WriteInteger('EditFlags', 0);
  r.CloseKey;

  r.OpenKey('Software\Classes\PScript', True);
    r.OpenKey('DefaultIcon', True);
      r.WriteString('', 'shell32.dll,21');
  r.CloseKey;

  r.OpenKey('Software\Classes\PScript', True);
    r.OpenKey('Shell', True);
      r.WriteString('', 'Execute');
  r.CloseKey;

  r.OpenKey('Software\Classes\PScript', True);
    r.OpenKey('Shell', True);
      r.OpenKey('Edit', True);
        r.OpenKey('Command', True);
          r.WriteString('', s + '\Programas\IDE.exe "%1"');
  r.CloseKey;

  r.OpenKey('Software\Classes\PScript', True);
    r.OpenKey('Shell', True);
      r.OpenKey('Execute', True);
        r.OpenKey('Command', True);
          r.WriteString('', s + '\Programas\PSRT.exe "%1"');
  r.CloseKey;

  r.Free;
  WriteLN('Registrado com Sucesso !');
end.

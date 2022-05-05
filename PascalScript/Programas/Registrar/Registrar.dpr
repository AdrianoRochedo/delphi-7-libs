program Registrar;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SysUtilsEx,
  Windows,
  Registry,
  RegistryUtils;

const
  PSF = 'PascalScriptFile';

var
  r : TRegistry;
  s : string;
begin
  s := getCurrentDir();
  if LastChar(s) <> '\' then s := s + '\';

  r := TRegistry.Create();
  try
    RegisterExtension(r, '.pscript', PSF, 'Pascal Script');
    RegisterExtensionIcon(r, PSF, 'Shell32.dll', 21);
    RegisterExtensionAction(r, PSF, 'Executar', true, s + 'PSRT.exe', '"%1"');
    RegisterExtensionAction(r, PSF, 'Editar', false, s + 'PSIDE.exe', '"%1"');
    NotifyRegChanges();
  finally
    r.Free();
  end;  
end.

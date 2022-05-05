unit RegistryUtils;

interface
uses Registry;

  procedure RegisterExtension(const Reg: TRegistry;             // TRegistry Instance
                              const Extension: String;          // '.abc'
                              const InternalFileType: String;   // 'Application.abc'
                              const ExternalFileType: String);  // 'abc file'

  procedure RegisterExtensionIcon(const Reg: TRegistry;             // TRegistry Instance
                                  const InternalFileType: String;   // 'Application.abc'
                                  const ModuleName: String;         // 'C:\Progs\Application.exe'
                                  const IconIndex: Integer);        // 72

  procedure RegisterExtensionAction(const Reg: TRegistry;             // TRegistry Instance
                                    const InternalFileType: String;   // 'Application.abc'
                                    const ActionName: String;         // 'Open'
                                    const Default: Boolean;           // Default Action
                                    const ApplicationName: String;    // 'C:\Progs\Application.exe'
                                    const Parameters: String = '%1'); // -name "%1" -v v

  procedure NotifyRegChanges();

  procedure UnregisterExtension(const Extension: String); // '.abc'

  // COM utils
  function COMClassIsRegistred(Reg: TRegistry; const ClassID: TGUID): Boolean;
  procedure RegisterCOM(const Filename: string);
  procedure UnRegisterCOM(const Filename: string);

implementation
uses SysUtils, Windows, ShlObj;

procedure RegisterExtension(const Reg: TRegistry;             // TRegistry Instance
                            const Extension: String;          // '.abc'
                            const InternalFileType: String;   // 'Application.abc'
                            const ExternalFileType: String);  // 'abc file'
begin
  Reg.RootKey := HKEY_CLASSES_ROOT;

  Reg.OpenKey(Extension, True);
  Reg.WriteString('', InternalFileType);
  Reg.CloseKey();

  Reg.OpenKey(InternalFileType, True);
  Reg.WriteString('', ExternalFileType);
  Reg.CloseKey();
end;

procedure RegisterExtensionIcon(const Reg: TRegistry;             // TRegistry Instance
                                const InternalFileType: String;   // 'Application.abc'
                                const ModuleName: String;         // 'C:\Progs\Application.exe'
                                const IconIndex: Integer);        // 72
begin
  Reg.OpenKey(InternalFileType + '\DefaultIcon', True);
  Reg.WriteString('', ModuleName + ',' + IntToStr(IconIndex));
  Reg.CloseKey;
end;

procedure RegisterExtensionAction(const Reg: TRegistry;             // TRegistry Instance
                                  const InternalFileType: String;   // 'Application.abc'
                                  const ActionName: String;         // 'Open'
                                  const Default: Boolean;           // Default Action
                                  const ApplicationName: String;    // 'C:\Progs\Application.exe'
                                  const Parameters: String = '%1'); // -name "%1" -v v
begin
  if Default then
     begin
     Reg.OpenKey(InternalFileType + '\Shell', True);
     Reg.WriteString('', ActionName);
     Reg.CloseKey;
     end;

  Reg.OpenKey(InternalFileType + '\Shell\' + ActionName, True);
  Reg.WriteString('', ActionName);
  Reg.CloseKey;

  Reg.OpenKey(InternalFileType + '\Shell\' + ActionName + '\Command', True);
  Reg.WriteString('', '"' + ApplicationName + '" ' + Parameters);
  Reg.CloseKey;
end;

procedure NotifyRegChanges();
begin
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure UnregisterExtension(const Extension: String); // '.abc'
var Reg: TRegistry;
    InternalFileType: String;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if Reg.OpenKey(Extension, False) then
       begin
       InternalFileType := Reg.ReadString('');
       Reg.DeleteKey(Extension);
       end;

    if Reg.OpenKey(InternalFileType, False) then
       Reg.DeleteKey(InternalFileType);
  finally
    Reg.Free;
  end;
end;

function ComClassIsRegistred(Reg: TRegistry; const ClassID: TGUID): Boolean;
begin
  Result := False;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  if Reg.OpenKey('CLSID\' + GUIDtoString(ClassID) + '\InprocServer32', False) then
     begin
     Result := FileExists(Reg.ReadString(''));
     Reg.CloseKey;
     end;
end;

procedure RegisterCOM(const Filename: string);
begin
  Windows.Winexec(pChar('regsvr32 /s ' + Filename), Windows.SW_HIDE);
end;

procedure UnRegisterCOM(const Filename: string);
begin
  Windows.Winexec(pChar('regsvr32 /u /s ' + Filename), Windows.SW_HIDE);
end;

end.

{ Tester for Delphi Open Tools API }

{ Tested only in Delphi 3 !}

unit APIExpert;

interface

  procedure Register;

implementation

uses Classes, ToolIntf, EditIntf, ExptIntf,
  iMTracer;


type

  TMyAddInNotifier = class(TIAddInNotifier)
  public
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

  TMyModuleNotifier = class(TIModuleNotifier)
  private
    FileName: string;
  public
    procedure Notify(NotifyCode: TNotifyCode); override;
    procedure ComponentRenamed(ComponentHandle: Pointer;
      const OldName, NewName: string); override;
  end;


var
  MyAddInNotifier: TMyAddInNotifier;
  ModuleInterfaceList: TList;

procedure Register;
begin
  MyAddInNotifier := TMyAddInNotifier.Create;
  if ToolServices.AddNotifier(MyAddInNotifier) then
  begin
    ModuleInterfaceList := TList.Create;
    ODS('AddNotifier installed');
  end
  else
  begin
    ODS('ToolServices.AddNotifier failed');
    MyAddInNotifier.Release;
    MyAddInNotifier := nil;
  end;
end;

procedure TMyAddInNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
const
  CodeName: array[TFileNotification] of string = (
    'FileOpening', 'FileOpened', 'FileClosing',
    'ProjectOpening', 'ProjectOpened', 'ProjectClosing', 'AddedToProject',
    'RemovedFromProject', 'DefaultDesktopLoad', 'DefaultDesktopSave',
    'ProjectDesktopLoad', 'ProjectDesktopSave', 'PackageInstalled',
    'PackageUninstalled');
var
  i: integer;
  ModuleInterface: TIModuleInterface;
  ModuleNotifier: TMyModuleNotifier;
begin
  ODS('AddInNotifier.FileNotification: ' + CodeName[NotifyCode]);
  ODS('  FileName = ' + FileName);
  case NotifyCode of
    fnFileOpened, fnProjectOpened:
      begin
        ModuleInterface := ToolServices.GetModuleInterface(FileName);
        if ModuleInterface <> nil then
        begin
          ModuleNotifier := TMyModuleNotifier.Create;
          if ModuleInterface.AddNotifier(ModuleNotifier) then
          begin
            ModuleNotifier.FileName := FileName;
            ModuleInterfaceList.Add(ModuleNotifier);
            ODS(' ModuleInterface installed')
          end
          else
          begin
            ModuleNotifier.Release;
            ODS(' ModuleInterface.AddNotifier failed')
          end;
          ModuleInterface.Release;
        end
        else
          ODS(' ToolServices.GetModuleInterface failed')
      end;
    fnFileClosing:
      begin
        for i := 0 to ModuleInterfaceList.Count - 1 do
          if TMyModuleNotifier(ModuleInterfaceList[i]).FileName = FileName then
          begin
           { ODS(' Releasing ModuleNotifier in FileClosing');
            TMyModuleNotifier(ModuleInterfaceList[i]).Release;
            ModuleInterfaceList.Delete(i); }
            Break;
          end;
      end;
  end;
end;


procedure TMyModuleNotifier.Notify(NotifyCode: TNotifyCode);
const
  CodeName: array[TNotifyCode] of string = (
    'ModuleDeleted', 'ModuleRenamed', 'EditorModified',
    'FormModified', 'EditorSelected', 'FormSelected', 'BeforeSave',
    'AfterSave', 'FormSaving', 'ProjResModified');
var
  i: integer;
begin
  ODS('ModuleNotifier.Notify: ' + CodeName[NotifyCode]);
  case NotifyCode of
    ncModuleDeleted:
      begin
        ODS(' Releasing ModuleNotifier');
        Release;
      end;
  end;
end;    { Notify }

procedure TMyModuleNotifier.ComponentRenamed(ComponentHandle: Pointer;
  const OldName, NewName: string);
begin
  ODS('ModuleNotifier.ComponentRenamed');
end;


initialization
finalization
  if Assigned(MyAddInNotifier) then
  begin
    ODS('AddInNotifier shutdown');
    ToolServices.RemoveNotifier(MyAddInNotifier);
    MyAddInNotifier.Release;
    ModuleInterfaceList.Free;
  end;
end.


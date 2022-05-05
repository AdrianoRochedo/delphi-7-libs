{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       description : R&A implementation of delphi ide objects

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

{ NOTE: Only absolutely neccessary methods was implemented }

unit RAFDIDE;

interface

uses Windows, SysUtils, Classes, Forms, Dialogs, Menus,
 {$IFDEF RA_D4H} ToolsAPI, ActiveX, {$ENDIF RA_D4H}
  ToolIntf, VirtIntf, EditIntf, FileIntf, ExptIntf, IStreams,
  {$IFDEF RA_D6H} DesignIntf, {$ELSE} DsgnIntf, {$ENDIF RA_D6H}
  RAFDCompat, RAVersion;

  procedure IDELoadExpert(const FileName: TFileName);
  procedure IDETerminateExperts;
  procedure IDEUnloadExperts;
  procedure IDECloseDesignWindows;
  function GetExpertList: TList;


type

  TIRAToolServices = class(TIToolServices)
  public
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); virtual; abstract;
    procedure SetBaseRegistryKey(Value: string); dynamic; abstract;
  end;

  TIRAModuleInterface = class(TIModuleInterface)
  public
    procedure Notify(NotifyCode: TNotifyCode); virtual; abstract;
  end;

  IRADelphiIDE = interface
    procedure DesignerClosed(const Designer: TDesigner);
  end;


  TExpertRec = class
  private
    Module: HMODULE;
    TerminateProc: TExpertTerminateProc;
  public
    Expert: TIExpert;
  end;

var
  RAToolServices: TIRAToolServices = nil;
  RADelphiIDE: IRADelphiIDE;

const
  DefaultRegistryKey = 'Software\R&A\RAdOctober\' + RALibVersionString;


implementation

uses
  LibIntf, DsgnWnds,
  iMTracer, RAUtils,
  RAFD, RAFDDesigner, RAFDAppBuilder, RAFDPalette,
  RAFDPropertyInspector, RAFDEditor, RAFDProjectManager;

{
procedure main;
begin
  LibIntf.
end;
}

type

 {$IFDEF RA_D5H}
  TIDelphiIDE = class(TInterfacedObject, IDelphiIDE)
   { compatibility section }
    procedure FormActivated; virtual; abstract;
    function GetMainWindowSize: TRect; virtual; abstract;
    procedure ModalEdit(EditKey: Char; ReturnWindow: Pointer); virtual; abstract;
    procedure SelectionChanged; virtual; abstract;
    function LockState: Boolean; virtual; abstract;
    function GetToolServices: TIToolServices; virtual; abstract;
    procedure GetDesignerOptions(var Options: TDesignerOptions); virtual; abstract;
    procedure SelectItemName(const PropertyName: string); virtual; abstract;
    function GetBaseRegKey: string; virtual; abstract;
    function GetPathAndBaseExeName: string; virtual; abstract;
    function GetAppHandle: HWND; virtual; abstract;
    function GetToolSelected: Boolean; virtual; abstract;
    function GetCurCompClass: IPaletteItem; virtual; abstract;
    function GetPaletteItem(ComponentClass: TComponentClass): IPaletteItem; virtual; abstract;
    function GetCurTime: Integer; virtual; abstract;
    procedure ResetCompClass; virtual; abstract;
    procedure ValidateActiveModule; virtual; abstract;
    procedure ExpertsLoaded; virtual; abstract;
    function CreateBackupFile: Boolean; virtual; abstract;
    function MakeBackupFileName(const FileName: string): string; virtual; abstract;
    function GetFileSystem(const Ident: string): IOTAFileSystem; virtual; abstract;
    procedure AddExpert(const Expert: TIExpert); virtual; abstract;
    procedure RemoveExpert(const Expert: TIExpert); virtual; abstract;
    procedure ActiveFormModified; virtual; abstract;
    procedure ExecDesignDialog(DesignDialog: TDesignDialog); virtual; abstract;
   { Delphi 5 section }
    procedure ComponentRenamed(const CurName: string; const NewName: string); virtual; abstract;
    function GetPaletteItemByName(const ClassName: String): IPaletteItem; virtual; abstract;
    function GetWorkspaceOrigin: TPoint; virtual; abstract;
    procedure OpenForm(const FormName: String; Show: Boolean); virtual; abstract;
    procedure RaiseException(const Message: String); virtual; abstract;
    procedure ShowClassHelp(const ClassName: String); virtual; abstract;
    function AddWizard(const Wizard: IOTAWizard): Integer; virtual; abstract;
    procedure RemoveWizard(Index: Integer); virtual; abstract;
    procedure RequestTemplate(const CompName: String; Stream: TStream; Dependencies: TStrings); virtual; abstract;
    procedure ValidateFormEdit; virtual; abstract;
    function WinHelp(const HelpFile: String; Command: Integer; Data: Integer): Boolean; virtual; abstract;
  end;

  TILibrary = class(TInterfacedObject, ILibrary, IInstance)
   { compatibility section }
    procedure RegisterDesignWindow(Window: TForm); virtual; abstract;
    procedure UnregisterDesignWindow(Window: TForm); virtual; abstract;
    procedure GetSelection(const ASelection: IDesignerSelections); virtual; abstract;
    procedure SetSelection(const Designer: IDesigner; DesignWindow: TCustomForm;
      ASelection: IDesignerSelections); virtual; abstract;
    function CreateForm(const AModule: IDesignerModule; const AFileName: string;
      Existing: Boolean; const AFormName: string; const AAncestor: string;
      const AFileSystem: string): TIForm; virtual; abstract;
    procedure ShowWindows; virtual; abstract;
    procedure HideWindows; virtual; abstract;
    procedure SetLockState(State: Boolean); virtual; abstract;
    procedure DesignerOptionsChanged; virtual; abstract;
    function EditAction(Action: TEditAction): Boolean; virtual; abstract;
    function FindFile(const FileName: string): TIFile; virtual; abstract;
    function GetActiveForm: TIForm; virtual; abstract;
    procedure GetClassUnits(const ClassName: string; Proc: TGetStrProc); virtual; abstract;
    function GetCompClass(Index: Integer): TICompClass; virtual; abstract;
    function GetCompClassCount: Integer; virtual; abstract;
    procedure GetCompHierarchy(Proc: TGetStrProc); virtual; abstract;
    function GetComponentBitmap: HBITMAP; virtual; abstract;
    function GetEditState: TEditState; virtual; abstract;
    function GetModuleCount: Integer; virtual; abstract;
    function GetModuleName(Index: Integer): string; virtual; abstract;
    procedure GetProperties(PropKind: TPropKind; Proc: TGetPropProc); virtual; abstract;
    function GetSelectionName: string; virtual; abstract;
    function GetSelectionType: string; virtual; abstract;
    procedure ModalEditDone(ReturnWindow: Pointer); virtual; abstract;
    procedure RaiseException(const Message: string); virtual; abstract;
    procedure SetProjectName(const Name: string); virtual; abstract;
    procedure RegisterDragTarget(const SourceStream: string; TargetClass: TDragTargetClass); virtual; abstract;
   { Delphi 5 section }
    procedure CopySelectionToStream(S: TMemoryStream; UnitDependencies: TStrings); virtual; abstract;
    function CreateFromStream(const AModule: IDesignerModule; const AFileName: String; const AFileSystem: String; const IStream: IStream): TIForm; virtual; abstract;
    function CreateNewForm(const AModule: IDesignerModule; const AFileName: String; const Creator: IUnknown): TIForm; virtual; abstract;
    function FindForm(const FormName: String): TIForm; virtual; abstract;
    function GetFirstSelectionType: String; virtual; abstract;
    procedure GetFormNames(Proc: TGetStrProc); virtual; abstract;
    function GetOleCtrlHelp(const Member: String; var HelpCtx: Integer; var HelpFile: String): Boolean; virtual; abstract;
    procedure IsDesignMsg(var Msg: tagMSG; var Handled: Boolean); virtual; abstract;
    function IsTopmostForm(Wnd: HWND): Boolean; virtual; abstract;
    function IsValidTemplateSelection: Boolean; virtual; abstract;
    procedure PasteSelectionFromStream(S: TMemoryStream; Parent: TComponent; const Rect: TRect); virtual; abstract;
    procedure UnregisterExpertGroup(Group: Integer); virtual; abstract;
    procedure CancelModes; virtual; abstract;
    function GetControlsLocked: Boolean; virtual; abstract;
    procedure FormActivated(const AForm: TIForm); virtual; abstract;
    procedure UpdateFormDependents; virtual; abstract;
    procedure UpdateSelections(AllowNoSelection: Boolean); virtual; abstract;
    procedure RemoveDependenciesOn(const Component: IComponent); virtual; abstract;
    procedure FormListAdd(const AForm: TIForm); virtual; abstract;
    procedure FormListRemove(const AForm: TIForm); virtual; abstract;
    procedure FormModified(const AForm: TIForm); virtual; abstract;
    procedure FormGoDormant(const AForm: TIForm); virtual; abstract;
    procedure EditorGetProperties(Proc: TGetPropProc; Editor: TPropertyEditor); virtual; abstract;
    function GetForms(Index: Integer): TIForm; virtual; abstract;
    function GetFormsCount: Integer; virtual; abstract;
    function GetFirstSelectionClass: TClass; virtual; abstract;
    function IsNestable(const ClassName: String): Boolean; virtual; abstract;
   { RA section }
    function GetInstance: TObject;
  end;

 {$ENDIF RA_D5H}


 { TRAToolServices }
  TRAToolServices = class(TIRAToolServices)
  private
    Notifiers: TList;
    FBaseRegistryKey: string;
  public
    { Action interfaces }
    function CloseProject: Boolean; override;
    function OpenProject(const ProjName: string): Boolean; override;
    function OpenProjectInfo(const ProjName: string): Boolean; override;
    function SaveProject: Boolean; override;
    function CloseFile(const FileName: string): Boolean; override;
    function SaveFile(const FileName: string): Boolean; override;
    function OpenFile(const FileName: string): Boolean; override;
    function ReloadFile(const FileName: string): Boolean; override;
    function ModalDialogBox(Instance: THandle; TemplateName: PChar;  WndParent: HWnd;
      DialogFunc: TFarProc; InitParam: LongInt): Integer; override;
    function CreateModule(const ModuleName: string;
      Source, Form: TIStream; CreateFlags: TCreateModuleFlags): Boolean;
      override;
    function CreateModuleEx(const ModuleName, FormName, AncestorClass,
      FileSystem: string; Source, Form: TIStream;
      CreateFlags: TCreateModuleFlags): TIModuleInterface; override;

    { Project/UI information }
    function GetParentHandle: HWND; override;
    function GetProjectName: string; override;
    function GetUnitCount: Integer; override;
    function GetUnitName(Index: Integer): string; override;
    function EnumProjectUnits(EnumProc: TProjectEnumProc; Param: Pointer): Boolean;
      override;
    function GetFormCount: Integer; override;
    function GetFormName(Index: Integer): string; override;
    function GetCurrentFile: string; override;
    function IsFileOpen(const FileName: string): Boolean; override;
    function GetNewModuleName(var UnitIdent, FileName: string): Boolean; override;

    { Component Library interface }
    function GetModuleCount: Integer; override;
    function GetModuleName(Index: Integer): string; override;
    function GetComponentCount(ModIndex: Integer): Integer; override;
    function GetComponentName(ModIndex, CompIndex: Integer): string; override;
    {function InstallModule(const ModuleName: string): Boolean; virtual; stdcall; abstract;
    function CompileLibrary: Boolean; virtual; stdcall; abstract;}

    { Virtual File system interfaces }
    function RegisterFileSystem(AVirtualFileSystem: TIVirtualFileSystem): Boolean; override;
    function UnRegisterFileSystem(const Ident: string): Boolean; override;
    function GetFileSystem(const Ident: string): TIVirtualFileSystem; override;

    { Editor Interfaces }
    function GetModuleInterface(const FileName: string): TIModuleInterface; override;
    function GetFormModuleInterface(const FormName: string): TIModuleInterface; override;

    { Menu Interfaces }
    function GetMainMenu: TIMainMenuIntf; override;

    { Notification registration }
    function AddNotifier(AddInNotifier: TIAddInNotifier): Boolean; override;
    function RemoveNotifier(AddInNotifier: TIAddInNotifier): Boolean; override;

    { Pascal string handling functions }
    function NewPascalString(Str: PChar): Pointer; override;
    procedure FreePascalString(var Str: Pointer); override;
    procedure ReferencePascalString(var Str: Pointer); override;
    procedure AssignPascalString(var Dest, Src: Pointer); override;

    { Error handling }
    procedure RaiseException(const Message: string); override;

    { Configuration Access }
    function GetBaseRegistryKey: string; override;

    { Extensions }
    function GetFormBounds(BoundsType: TBoundsType): TRect; override;
    function ProjectCreate(ProjectCreator: TIProjectCreator;
      CreateFlags: TCreateProjectFlags): TIModuleInterface; override;
    function ModuleCreate(ModuleCreator: TIModuleCreator;
      CreateFlags: TCreateModuleFlags): TIModuleInterface; override;

    { Extended Notification registration }
    function AddNotifierEx(AddInNotifier: TIAddInNotifier): Boolean; override;

    { Create a unique unit name and class name }
    function GetNewModuleAndClassName(const Prefix: string; var UnitIdent,
      ClassName, FileName: string): Boolean; override;

    {$IFDEF RA_D4H}
    function CreateCppModule(const ModuleName, FormName, AncestorClass,
      FileSystem: string; HdrSource, Source, Form: IStream;
      CreateFlags: TCreateModuleFlags): TIModuleInterface; override;
    function GetVcsCount: Integer; override;
    procedure GetVcsList(List: TStringList); override;
    function GetVcsName(Index: Integer): string; override;

    function EnumProjectModules(EnumProc: TProjectEnumModuleProc; Param: Pointer): Boolean; override;

    function ProjectCreateEx(ProjectCreator: TIProjectCreatorEx;
      CreateFlags: TCreateProjectFlags): TIModuleInterface; override;
    function ModuleCreateEx(ModuleCreator: TIModuleCreatorEx;
      CreateFlags: TCreateModuleFlags): TIModuleInterface; override;
    {$ENDIF RA_D4H}
  public
    constructor Create;
    destructor Destroy; override;
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure SetBaseRegistryKey(Value: string); override;
  end;


 { TRAModuleInterface }
  TRAModuleInterface = class(TIRAModuleInterface)
  private
    FileEditor: TFileEditor;
    Notifiers: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddRef: Longint; override;
    function Release: Longint; override;
    function GetEditorInterface: TIEditorInterface; override;
    function GetFormInterface: TIFormInterface; override;
    function GetAncestorModule: TIModuleInterface; override;
    function GetProjectResource: TIResourceFile; override;
    function IsProjectModule: Boolean; override;
    function Close: Boolean; override;
    function Save(ForceSave: Boolean): Boolean; override;
    function Rename(const NewName: string): Boolean; override;
    function GetFileSystem(var FileSystem: string): Boolean; override;
    function SetFileSystem(const FileSystem: string): Boolean; override;
    function ShowSource: Boolean; override;
    function ShowForm: Boolean; override;
    function AddNotifier(AModuleNotifier: TIModuleNotifier): Boolean; override;
    function RemoveNotifier(AModuleNotifier: TIModuleNotifier): Boolean; override;
   {$IFDEF RA_D5H}
    function GetAuxEditorInterface: TIEditorInterface; override;
   {$ENDIF RA_D5H}
    procedure Notify(NotifyCode: TNotifyCode); override;
  end;


 { TRAIDE }
  TRAIDE = class(TIDelphiIDE {$IFDEF RA_D5H}, IRADelphiIDE{$ENDIF RA_D5H} )
  public
    procedure FormActivated; override;
    function GetMainWindowSize: TRect; override;
    procedure ModalEdit(EditKey: Char; ReturnWindow: Pointer); override;
    procedure SelectionChanged; override;
    function LockState: Boolean; override;
    function GetToolServices: TIToolServices; override;
    procedure GetDesignerOptions(var Options: TDesignerOptions); override;
    procedure SelectItemName(const PropertyName: string); override;
    function GetBaseRegKey: string; override;
    function GetPathAndBaseExeName: string; override;
    function GetAppHandle: HWND; override;
    function GetToolSelected: Boolean; override;
   {$IFDEF RA_D3}
    function GetCurCompClass: TIPaletteItem; override;
    function GetPaletteItem(ComponentClass: TComponentClass): TIPaletteItem; override;
   {$ELSE}
    function GetCurCompClass: IPaletteItem; override;
    function GetPaletteItem(ComponentClass: TComponentClass): IPaletteItem; override;
   {$ENDIF}
    function GetCurTime: Integer; override;
    procedure ResetCompClass; override;
    procedure ValidateActiveModule; override;
    procedure ExpertsLoaded; override;
    function CreateBackupFile: Boolean; override;
    function MakeBackupFileName(const FileName: string): string; override;
   {$IFDEF RA_D3}
    function GetFileSystem(const Ident: string): TIVirtualFileSystem; override;
   {$ELSE}
    function GetFileSystem(const Ident: string): IOTAFileSystem; override;
   {$ENDIF}
   {$IFDEF RA_D5H}
    procedure AddExpert(const Expert: TIExpert); override;
    procedure RemoveExpert(const Expert: TIExpert); override;
   {$ELSE}
    procedure AddExpert(Expert: TIExpert); override;
    procedure RemoveExpert(Expert: TIExpert); override;
   {$ENDIF}
    procedure ActiveFormModified; override;
    procedure ExecDesignDialog(DesignDialog: TDesignDialog); override;
   {$IFDEF RA_D5H}
    procedure ComponentRenamed(const CurName: string; const NewName: string); override;
    function GetPaletteItemByName(const ClassName: String): IPaletteItem; override;
    function GetWorkspaceOrigin: TPoint; override;
    procedure OpenForm(const FormName: String; Show: Boolean); override;
    procedure RaiseException(const Message: String); override;
    procedure ShowClassHelp(const ClassName: String); override;
    function AddWizard(const Wizard: IOTAWizard): Integer; override;
    procedure RemoveWizard(Index: Integer); override;
    procedure RequestTemplate(const CompName: String; Stream: TStream; Dependencies: TStrings); override;
    procedure ValidateFormEdit; override;
    function WinHelp(const HelpFile: String; Command: Integer; Data: Integer): Boolean; override;
   {RA}
    procedure DesignerClosed(const Designer: TDesigner);
   {$ENDIF RA_D5H}
  end;


 { TRACompLib }
  TRACompLib = class(TILibrary)
  private
    WindowList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterDesignWindow(Window: TForm); override;
    procedure UnregisterDesignWindow(Window: TForm); override;
   {$IFDEF RA_D3}
    procedure GetSelection(ASelection: TComponentList); override;
    procedure SetSelection(Designer: TDesigner; DesignWindow: TForm;
      ASelection: TComponentList); override;
    function CreateForm(AModule: TIModule; const AFileName: string;
      Existing: Boolean; const AFormName: string; const AAncestor: string;
      const AFileSystem: string): TIForm; override;
   {$ENDIF}
   {$IFDEF RA_D4}
    procedure GetSelection(ASelection: IDesignerSelections); override;
    procedure SetSelection(const Designer: IDesigner; DesignWindow: TForm;
      ASelection: IDesignerSelections); override;
    function CreateForm(AModule: IDesignerModule; const AFileName: string;
      Existing: Boolean; const AFormName: string; const AAncestor: string;
      const AFileSystem: string): TIForm; override;
   {$ENDIF}
   {$IFDEF RA_D5H}
    procedure GetSelection(const ASelection: IDesignerSelections); override;
    procedure SetSelection(const Designer: IDesigner; DesignWindow: TCustomForm;
      ASelection: IDesignerSelections); override;
    function CreateForm(const AModule: IDesignerModule; const AFileName: string;
      Existing: Boolean; const AFormName: string; const AAncestor: string;
      const AFileSystem: string): TIForm; override;
   {$ENDIF}

    procedure ShowWindows; override;
    procedure HideWindows; override;
    procedure SetLockState(State: Boolean); override;
    procedure DesignerOptionsChanged; override;
    function EditAction(Action: TEditAction): Boolean; override;
    function FindFile(const FileName: string): TIFile; override;
    function GetActiveForm: TIForm; override;
    procedure GetClassUnits(const ClassName: string; Proc: TGetStrProc); override;
    function GetCompClass(Index: Integer): TICompClass; override;
    function GetCompClassCount: Integer; override;
    procedure GetCompHierarchy(Proc: TGetStrProc); override;
    function GetComponentBitmap: HBITMAP; override;
    function GetEditState: TEditState; override;
    function GetModuleCount: Integer; override;
    function GetModuleName(Index: Integer): string; override;
    procedure GetProperties(PropKind: TPropKind; Proc: TGetPropProc); override;
    function GetSelectionName: string; override;
    function GetSelectionType: string; override;
    procedure ModalEditDone(ReturnWindow: Pointer); override;
    procedure RaiseException(const Message: string); override;
    procedure SetProjectName(const Name: string); override;
    procedure RegisterDragTarget(const SourceStream: string; TargetClass: TDragTargetClass); override;
   {$IFDEF RA_D5H}
    procedure CopySelectionToStream(S: TMemoryStream; UnitDependencies: TStrings); override;
    function CreateFromStream(const AModule: IDesignerModule; const AFileName: String; const AFileSystem: String; const IStream: IStream): TIForm; override;
    function CreateNewForm(const AModule: IDesignerModule; const AFileName: String; const Creator: IUnknown): TIForm; override;
    function FindForm(const FormName: String): TIForm; override;
    function GetFirstSelectionType: String; override;
    procedure GetFormNames(Proc: TGetStrProc); override;
    function GetOleCtrlHelp(const Member: String; var HelpCtx: Integer; var HelpFile: String): Boolean; override;
    procedure IsDesignMsg(var Msg: tagMSG; var Handled: Boolean); override;
    function IsTopmostForm(Wnd: HWND): Boolean; override;
    function IsValidTemplateSelection: Boolean; override;
    procedure PasteSelectionFromStream(S: TMemoryStream; Parent: TComponent; const Rect: TRect); override;
    procedure UnregisterExpertGroup(Group: Integer); override;
    procedure CancelModes; override;
    function GetControlsLocked: Boolean; override;
    procedure FormActivated(const AForm: TIForm); override;
    procedure UpdateFormDependents; override;
    procedure UpdateSelections(AllowNoSelection: Boolean); override;
    procedure RemoveDependenciesOn(const Component: IComponent); override;
    procedure FormListAdd(const AForm: TIForm); override;
    procedure FormListRemove(const AForm: TIForm); override;
    procedure FormModified(const AForm: TIForm); override;
    procedure FormGoDormant(const AForm: TIForm); override;
    procedure EditorGetProperties(Proc: TGetPropProc; Editor: TPropertyEditor); override;
    function GetForms(Index: Integer): TIForm; override;
    function GetFormsCount: Integer; override;
    function GetFirstSelectionClass: TClass; override;
    function IsNestable(const ClassName: String): Boolean; override;
   {$ENDIF RA_D5H}
   { RA section }
    procedure CloseDesignWindows;
  end;


 { TRAMenuItemIntf }
  TRAMenuItemIntf = class(TIMenuItemIntf)
  private
    FMenuItem: TMenuItem;
  public
    constructor Create(AMenuItem: TMenuItem);
    function DestroyMenuItem: Boolean; override;
    function GetIndex: Integer; override;
    function GetItemCount: Integer; override;
    function GetItem(Index: Integer): TIMenuItemIntf; override;
    function GetName: string; override;
    function GetParent: TIMenuItemIntf; override;
    function GetCaption: string; override;
    function SetCaption(const Caption: string): Boolean; override;
    function GetShortCut: Integer; override;
    function SetShortCut(ShortCut: Integer): Boolean; override;
    function GetFlags: TIMenuFlags; override;
    function SetFlags(Mask, Flags: TIMenuFlags): Boolean; override;
    function GetGroupIndex: Integer; override;
    function SetGroupIndex(GroupIndex: Integer): Boolean; override;
    function GetHint: string; override;
    function SetHint(Hint: string): Boolean; override;
    function GetContext: Integer; override;
    function SetContext(Context: Integer): Boolean; override;
    function GetOnClick: TIMenuClickEvent; override;
    function SetOnClick(Click: TIMenuClickEvent): Boolean; override;
    function InsertItem(Index: Integer; Caption, Name, Hint: string;
      ShortCut, Context, GroupIndex: Integer; Flags: TIMenuFlags;
      EventHandler: TIMenuClickEvent): TIMenuItemIntf; override;
  end;

 { TRAMainMenuIntf }
  TRAMainMenuIntf = class(TIMainMenuIntf)
  public
    function GetMenuItems: TIMenuItemIntf; override;
    function FindMenuItem(const Name: string): TIMenuItemIntf; override;
  end;

  TRAFDMenuItem = class(TMenuItem)
  private
    FOnClick : TIMenuClickEvent;
  protected
    procedure Click; override;
    destructor Destroy; override;
  end;

  TRAEditorInterface = class(TIEditorInterface)
  private
    FileEditor: TFileEditor;
  public
    function CreateReader: TIEditReader; override;
    function CreateWriter: TIEditWriter; override;
    function FileName: string; override;
    function LinesInBuffer: Longint; override;
    function BufferModified: Boolean; override;
    function MarkModified: Boolean; override;
    function SetSyntaxHighlighter(SyntaxHighlighter: TSyntaxHighlighter): TSyntaxHighlighter;
      override;
    function GetViewCount: Integer; override;
    function GetView(Index: Integer): TIEditView; override;
    function CreateUndoableWriter: TIEditWriter; override;
    // These functions will affect all views on this buffer.
    function GetBlockAfter: TCharPos; override;
    function GetBlockStart: TCharPos; override;
    function GetBlockType: TBlockType; override;
    function GetBlockVisible: Boolean; override;
    procedure SetBlockAfter(Value: TCharPos); override;
    procedure SetBlockStart(Value: TCharPos); override;
    procedure SetBlockType(Value: TBlockType); override;
    procedure SetBlockVisible(Value: Boolean); override;
  end;

  TRAEditReader = class(TIEditReader)
  private
    FileEditor: TFileEditor;
  public
    function GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint; override;
  end;

  TRAEditWriter = class(TIEditWriter)
  private
    FileEditor: TFileEditor;
    Text: string;
    FPos: Integer;
  public
    constructor Create(AFileEditor: TFileEditor);
    destructor Destroy; override;
    function CopyTo(Pos: Longint): Boolean; override;
    function DeleteTo(Pos: Longint): Boolean;override;
    function Insert(Text: PChar): Boolean; override;
    function Position: Longint; override;
    function GetCurrentPos: TCharPos; override;
  end;

  TRAEditView = class(TIEditView)
  private
    FileEditor: TFileEditor;
  public
    function GetPos(Index: Integer): TEditPos; override;
    procedure SetPos(Index: Integer; Value: TEditPos); override;
    function GetViewSize: TSize; override;
    function PosToCharPos(Pos: Longint): TCharPos; override;
    function CharPosToPos(CharPos: TCharPos): Longint; override;
    procedure ConvertPos(EdPosToCharPos: Boolean; var EditPos: TEditPos;
      var CharPos: TCharPos); override;
    procedure GetAttributeAtPos(const EdPos: TEditPos; var Element, LineFlag: Integer);
      override;
  end;

  TRAForm = class(TIForm)
  private
    Designer: TRAFormDesigner;
  protected
   {$IFDEF RA_D3}
    function GetDesigner: TFormDesigner; override;
   {$ELSE}
    function GetDesigner: IFormDesigner; override;
   {$ENDIF}
  end;

{ internal }

  TMIRec = class
  private
    MenuItem: TMenuItem;
    MenuItemIntf: TIMenuItemIntf;
  end;

var
  MenuItemIntfList: TList;
  ExpertList: TList;

function GetMenuItemIntf(MenuItem: TMenuItem): TIMenuItemIntf; forward;

procedure ODS(S: string);
begin
  iMTracer.ODS(S);
end;


{$IFDEF RA_D5H}
function TILibrary.GetInstance: TObject;
begin
  Result := Self;
end;
{$ENDIF RA_D5H}


constructor TRAToolServices.Create;
begin
  inherited Create;
	Notifiers := TList.Create;
  FBaseRegistryKey := DefaultRegistryKey;
end;

destructor TRAToolServices.Destroy;
begin
  Notifiers.Free;
  inherited Destroy;
end;

function TRAToolServices.CloseProject: Boolean;
begin
  ODS('TIToolServices.CloseProject');
  Result := False;
end;

function TRAToolServices.OpenProject(const ProjName: string): Boolean;
begin
  ODS('TIToolServices.OpenProject');
  Result := False;
end;

function TRAToolServices.OpenProjectInfo(const ProjName: string): Boolean;
begin
  ODS('TIToolServices.OpenProjectInfo');
  Result := False;
end;

function TRAToolServices.SaveProject: Boolean;
begin
  ODS('TIToolServices.SaveProject');
  Result := False;
end;

function TRAToolServices.CloseFile(const FileName: string): Boolean;
begin
  ODS('TIToolServices.CloseFile');
  Result := False;
end;

function TRAToolServices.SaveFile(const FileName: string): Boolean;
begin
  ODS('TIToolServices.SaveFile');
  Result := False;
end;

function TRAToolServices.OpenFile(const FileName: string): Boolean;
begin
  ODS('TIToolServices.OpenFile');
  { !!! NOT COMPLETE !!! }
  AppBuilder.OpenFile(FileName);
  Result := Assigned(ActiveFileEditor) and
    Cmp(ActiveFileEditor.FileName, FileName);
end;

function TRAToolServices.ReloadFile(const FileName: string): Boolean;
begin
  ODS('TIToolServices.ReloadFile');
  Result := False;
end;

function TRAToolServices.ModalDialogBox(Instance: THandle; TemplateName: PChar;
  WndParent: HWnd; DialogFunc: TFarProc; InitParam: LongInt): Integer;
begin
  ODS('TIToolServices.ModalDialogBox');
  Result := 0;
end;

function TRAToolServices.CreateModule(const ModuleName: string;
	Source, Form: TIStream; CreateFlags: TCreateModuleFlags): Boolean;
begin
  ODS('TIToolServices.CreateModule');
  Result := ProjectManager.CreateModule(ModuleName, Source, Form, CreateFlags);
end;


function TRAToolServices.CreateModuleEx(const ModuleName, FormName, AncestorClass,
	FileSystem: string; Source, Form: TIStream;
	CreateFlags: TCreateModuleFlags): TIModuleInterface;
begin
  ODS('TIToolServices.CreateModuleEx');
  Result := nil;
end;


{ Project/UI information }
function TRAToolServices.GetParentHandle: HWND;
begin
  ODS('TIToolServices.GetParentHandle');
  Result := Application.Handle;
end;

function TRAToolServices.GetProjectName: string;
begin
  ODS('TIToolServices.GetProjectName');
  Result := ProjectManager.GetProjectName;
end;

function TRAToolServices.GetUnitCount: Integer;
begin
  ODS('TIToolServices.GetUnitCount');
  Result := ProjectManager.GetUnitCount;
end;

function TRAToolServices.GetUnitName(Index: Integer): string;
begin
  ODS('TIToolServices.GetUnitName');
  Result := ProjectManager.GetUnitName(Index);
end;

function TRAToolServices.EnumProjectUnits(EnumProc: TProjectEnumProc; Param: Pointer): Boolean;
begin
  ODS('TIToolServices.EnumProjectUnits');
  Result := ProjectManager.EnumProjectUnits(EnumProc, Param);
end;


function TRAToolServices.GetFormCount: Integer;
begin
  ODS('TIToolServices.GetFormCount');
  Result := ProjectManager.GetFormCount;
end;

function TRAToolServices.GetFormName(Index: Integer): string;
begin
  ODS('TIToolServices.GetFormName');
  Result := ProjectManager.GetFormName(Index);
end;

function TRAToolServices.GetCurrentFile: string;
begin
  ODS('TIToolServices.GetCurrentFile');
  { !!! NOT COMPLETE !!! }
  if ActiveFileEditor <> nil then
    if LastActiveWindow is TRAFDForm then
      Result := (ActiveFileEditor.Designer as TRAFormDesigner).FileName
    else
      Result := ActiveFileEditor.FileName
  else
    Result := '';
end;

function TRAToolServices.IsFileOpen(const FileName: string): Boolean;
var
  i: integer;
begin
  ODS('TIToolServices.IsFileOpen');
  { !!! NOT COMPLETE !!! }
  Result := False;
  if ActiveEditWindow <> nil then
    for i := 0 to ActiveEditWindow.EditorCount - 1 do
      if Cmp(ActiveEditWindow.Editors[i].FileName, FileName) then
      begin
        Result := True;
        Exit;
      end
end;

function TRAToolServices.GetNewModuleName(var UnitIdent, FileName: string): Boolean;
begin
  ODS('TIToolServices.GetNewModuleName');
  Result := ProjectManager.GetNewModuleName(UnitIdent, FileName);
end;


{ Component Library interface }
function TRAToolServices.GetModuleCount: Integer;
begin
  ODS('TIToolServices.GetModuleCount');
  Result := 0;
end;

function TRAToolServices.GetModuleName(Index: Integer): string;
begin
  ODS('TIToolServices.GetModuleName');
  Result := '';
end;

function TRAToolServices.GetComponentCount(ModIndex: Integer): Integer;
begin
  ODS('TIToolServices.GetComponentCount');
  Result := 0;
end;

function TRAToolServices.GetComponentName(ModIndex, CompIndex: Integer): string;
begin
  ODS('TIToolServices.GetComponentName');
  Result := '';
end;

{function InstallModule(const ModuleName: string): Boolean; virtual; stdcall; abstract;
function CompileLibrary: Boolean; virtual; stdcall; abstract;}

{ Virtual File system interfaces }
function TRAToolServices.RegisterFileSystem(AVirtualFileSystem: TIVirtualFileSystem): Boolean;
begin
  ODS('TIToolServices.RegisterFileSystem');
 { returning True allows run SQL Explorer from TDataSet popup menu } 
  Result := True;
end;

function TRAToolServices.UnRegisterFileSystem(const Ident: string): Boolean;
begin
  ODS('TIToolServices.TRAToolServices');
  Result := False;
end;

{$IFDEF RA_D3}
function TRAToolServices.GetFileSystem(const Ident: string): TIVirtualFileSystem;
{$ELSE}
function TRAToolServices.GetFileSystem(const Ident: string): TIVirtualFileSystem;
{$ENDIF}
begin
  ODS('TIToolServices.GetFileSystem');
  Result := nil;
end;


{ Editor Interfaces }
function TRAToolServices.GetModuleInterface(const FileName: string): TIModuleInterface;
var
  i: integer;
  RAModuleInterface: TRAModuleInterface;
  FileEditor: TFileEditor;
begin
  ODS('TIToolServices.GetModuleInterface');
  Result := nil;
 { Module interface for project file must be always available }
  if Cmp(FileName, ProjectManager.GetProjectName) then
  begin
		Result := ProjectManager.ModuleInterface;
    if Assigned(Result) then Result.AddRef;
    Exit;
  end;
  if ActiveEditWindow = nil then Exit;
  for i := 0 to ActiveEditWindow.EditorCount - 1 do
    if Cmp(ActiveEditWindow.Editors[i].FileName, FileName) then
    begin
      FileEditor := ActiveEditWindow.Editors[i];
      if not Assigned(FileEditor.ModuleInterface) then
      begin
        RAModuleInterface := TRAModuleInterface.Create;
        RAModuleInterface.FileEditor := FileEditor;
        RAModuleInterface.FileEditor.SetModuleInterface(RAModuleInterface);
      end;
      Result := FileEditor.ModuleInterface;
      Result.AddRef;
      Exit;
    end;
  if Result = nil then ODS('  result = nil ');
end;


function TRAToolServices.GetFormModuleInterface(const FormName: string): TIModuleInterface;
begin
  ODS('TIToolServices.GetFormModuleInterface');
  Result := nil;
end;


{ Menu Interfaces }
function TRAToolServices.GetMainMenu: TIMainMenuIntf;
begin
  ODS('TIToolServices.GetMainMenu');
  Result := TRAMainMenuIntf.Create;
end;

{ Notification registration }
function TRAToolServices.AddNotifier(AddInNotifier: TIAddInNotifier): Boolean;
begin
  ODS('TIToolServices.AddNotifier');
  Result := True;
  AddInNotifier.AddRef;
  Notifiers.Add(AddInNotifier);
end;


function TRAToolServices.RemoveNotifier(AddInNotifier: TIAddInNotifier): Boolean;
begin
  ODS('TIToolServices.RemoveNotifier');
  Result := True;
  Notifiers.Remove(AddInNotifier);
  AddInNotifier.Release;
end;


{ Pascal string handling functions }
function TRAToolServices.NewPascalString(Str: PChar): Pointer;
begin
  ODS('TIToolServices.NewPascalString');
  Result := nil;
end;

procedure TRAToolServices.FreePascalString(var Str: Pointer);
begin
  ODS('TIToolServices.FreePascalString');
end;

procedure TRAToolServices.ReferencePascalString(var Str: Pointer);
begin
  ODS('TIToolServices.ReferencePascalString');
end;

procedure TRAToolServices.AssignPascalString(var Dest, Src: Pointer);
begin
  ODS('TIToolServices.AssignPascalString');
end;


{ Error handling }
procedure TRAToolServices.RaiseException(const Message: string);
begin
  ODS('TIToolServices.RaiseException');
  raise Exception.Create(Message);
end;


{ Configuration Access }
function TRAToolServices.GetBaseRegistryKey: string;
begin
  ODS('TIToolServices.GetBaseRegistryKey');
  Result := FBaseRegistryKey;
end;


{ Extensions }
function TRAToolServices.GetFormBounds(BoundsType: TBoundsType): TRect;
begin
  ODS('TIToolServices.GetFormBounds');
  Result := Rect(0, 0, 0, 0);
end;

function TRAToolServices.ProjectCreate(ProjectCreator: TIProjectCreator;
	CreateFlags: TCreateProjectFlags): TIModuleInterface;
begin
  ODS('TIToolServices.ProjectCreate');
  Result := ProjectManager.ProjectCreate(ProjectCreator, CreateFlags);
end;

function TRAToolServices.ModuleCreate(ModuleCreator: TIModuleCreator;
	CreateFlags: TCreateModuleFlags): TIModuleInterface;
begin
  ODS('TIToolServices.ModuleCreate');
  Result := ProjectManager.ModuleCreate(ModuleCreator, CreateFlags);
end;


{ Extended Notification registration }
function TRAToolServices.AddNotifierEx(AddInNotifier: TIAddInNotifier): Boolean;
begin
  ODS('TIToolServices.AddNotifierEx');
  Result := True;
  AddInNotifier.AddRef;
  Notifiers.Add(AddInNotifier);
end;


{ Create a unique unit name and class name }
function TRAToolServices.GetNewModuleAndClassName(const Prefix: string;
  var UnitIdent, ClassName, FileName: string): Boolean;
begin
  ODS('TIToolServices.GetNewModuleAndClassName');
  Result := ProjectManager.GetNewModuleAndClassName(Prefix, UnitIdent,
    ClassName, FileName);
end;

{$IFDEF RA_D4H}
function TRAToolServices.CreateCppModule(const ModuleName, FormName, AncestorClass,
  FileSystem: string; HdrSource, Source, Form: IStream;
  CreateFlags: TCreateModuleFlags): TIModuleInterface;
begin
  ODS('TIToolServices.CreateCppModule');
  Result := nil;
end;

function TRAToolServices.GetVcsCount: Integer;
begin
  ODS('TIToolServices.GetVcsCoun');
  Result := 0;
end;

procedure TRAToolServices.GetVcsList(List: TStringList);
begin
  ODS('TIToolServices.GetVcsList');
end;

function TRAToolServices.GetVcsName(Index: Integer): string;
begin
  ODS('TIToolServices.GetVcsName');
end;

function TRAToolServices.EnumProjectModules(EnumProc: TProjectEnumModuleProc; Param: Pointer): Boolean;
begin
  ODS('TIToolServices.EnumProjectModules');
  Result := False;
end;

function TRAToolServices.ProjectCreateEx(ProjectCreator: TIProjectCreatorEx;
  CreateFlags: TCreateProjectFlags): TIModuleInterface;
begin
  ODS('TIToolServices.ProjectCreateEx');
  Result := nil;
end;

function TRAToolServices.ModuleCreateEx(ModuleCreator: TIModuleCreatorEx;
  CreateFlags: TCreateModuleFlags): TIModuleInterface;
begin
  ODS('TIToolServices.ModuleCreateEx');
  Result := nil;
end;
{$ENDIF RA_D4H}

procedure TRAToolServices.FileNotification(NotifyCode: TFileNotification;
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
begin
  ODS('TRAToolServices.FileNotification: ' + CodeName[NotifyCode]);
  if NotifyCode = fnProjectOpened then
    ProjectManager.SetModuleInterface(TRAModuleInterface.Create);
  for i := Notifiers.Count - 1 downto 0 do
  begin
    TIAddInNotifier(Notifiers[i]).FileNotification(NotifyCode, FileName, Cancel);
    if Cancel then Break;
  end;
  if NotifyCode = fnProjectClosing then
	begin
		if Assigned(ProjectManager.ModuleInterface) then
    begin
      (ProjectManager.ModuleInterface as TRAModuleInterface).
        Notify(ncModuleDeleted);
			ProjectManager.ModuleInterface.Release;
    end;
		ProjectManager.SetModuleInterface(nil);
	end;
end;

procedure TRAToolServices.SetBaseRegistryKey(Value: string);
begin
  FBaseRegistryKey := Value;
end;


{************************* TRAModuleInterface **************************}
constructor TRAModuleInterface.Create;
begin
  inherited Create;
  Notifiers := TList.Create;
end;

destructor TRAModuleInterface.Destroy;
begin
  ODS('TRAModuleInterface.Destroy');
  Notifiers.Free;
  if Assigned(FileEditor) then
    FileEditor.SetModuleInterface(nil);
  inherited Destroy;
end;    { Destroy }

function TRAModuleInterface.AddRef: Longint;
begin
//  ODS('TIModuleInterface.AddRef');
  Result := inherited AddRef;
end;

function TRAModuleInterface.Release: Longint;
begin
//  ODS('TIModuleInterface.Release');
  Result := inherited Release;
end;

function TRAModuleInterface.GetEditorInterface: TIEditorInterface;
begin
  ODS('TIModuleInterface.GetEditorInterface');
  if Assigned(FileEditor) then
  begin
    Result := TRAEditorInterface.Create;
    (Result as TRAEditorInterface).FileEditor := FileEditor;
  end
  else
    Result := nil;
end;

function TRAModuleInterface.GetFormInterface: TIFormInterface;
begin
  ODS('TIModuleInterface.GetFormInterface');
  Result := nil;
end;

function TRAModuleInterface.GetAncestorModule: TIModuleInterface;
begin
  ODS('TIModuleInterface.GetAncestorModule');
  Result := nil;
end;

function TRAModuleInterface.GetProjectResource: TIResourceFile;
begin
  ODS('TIModuleInterface.GetProjectResource');
  Result := nil;
end;

function TRAModuleInterface.IsProjectModule: Boolean;
begin
  ODS('TIModuleInterface.IsProjectModule');
  if Assigned(FileEditor) then
    Result := FileEditor.IsProjectFile
  else
    Result := True;
  { only ModuleInterface assotiated with project file
    can exists without FileEditor }
end;

function TRAModuleInterface.Close: Boolean;
begin
  ODS('TIModuleInterface.Close');
  Result := False;
end;

function TRAModuleInterface.Save(ForceSave: Boolean): Boolean;
begin
  ODS('TIModuleInterface.Save');
  Result := False;
end;

function TRAModuleInterface.Rename(const NewName: string): Boolean;
begin
  ODS('TIModuleInterface.Rename');
  Result := False;
end;

function TRAModuleInterface.GetFileSystem(var FileSystem: string): Boolean;
begin
  ODS('TIModuleInterface.GetFileSystem');
  Result := False;
end;

function TRAModuleInterface.SetFileSystem(const FileSystem: string): Boolean;
begin
  ODS('TIModuleInterface.SetFileSystem');
  Result := False;
end;

function TRAModuleInterface.ShowSource: Boolean;
begin
  ODS('TIModuleInterface.ShowSource');
  if Assigned(FileEditor) then
    FileEditor.ToggleToFront
  else
    ProjectManager.ShowSource;
  Result := True;
end;

function TRAModuleInterface.ShowForm: Boolean;
begin
  ODS('TIModuleInterface.ShowForm');
  Result := False;
end;

function TRAModuleInterface.AddNotifier(AModuleNotifier: TIModuleNotifier): Boolean;
begin
  ODS('TIModuleInterface.AddNotifier');
  Result := True;
  AModuleNotifier.AddRef;
  Notifiers.Add(AModuleNotifier);
end;

function TRAModuleInterface.RemoveNotifier(AModuleNotifier: TIModuleNotifier): Boolean;
begin
  ODS('TIModuleInterface.RemoveNotifier');
  Result := True;
  Notifiers.Remove(AModuleNotifier);
  AModuleNotifier.Release;
end;

{$IFDEF RA_D5H}
function TRAModuleInterface.GetAuxEditorInterface: TIEditorInterface;
begin
  ODS('TIModuleInterface.GetAuxEditorInterface');
  Result := nil;
end;
{$ENDIF RA_D5H}

procedure TRAModuleInterface.Notify(NotifyCode: TNotifyCode);
const
  CodeName: array[TNotifyCode] of string = (
    'ModuleDeleted', 'ModuleRenamed', 'EditorModified',
    'FormModified', 'EditorSelected', 'FormSelected', 'BeforeSave',
    'AfterSave', 'FormSaving', 'ProjResModified');
var
  i: integer;
begin
  ODS('TRAModuleInterface.Notify: ' + CodeName[NotifyCode]);
  for i := Notifiers.Count - 1 downto 0 do
    TIModuleNotifier(Notifiers[i]).Notify(NotifyCode);
//  ODS('                  .Notify ..ok')
end;    { Notify }



{************************* TRAIDE *************************}
procedure TRAIDE.FormActivated;
begin
  ODS('TIDelphiIDE.FormActivated');
end;    { FormActivated }

function TRAIDE.GetMainWindowSize: TRect;
begin
  ODS('TIDelphiIDE.GetMainWindowSize');
  Result := AppBuilder.BoundsRect;
end;    { GetMainWindowSize }

procedure TRAIDE.ModalEdit(EditKey: Char; ReturnWindow: Pointer);
begin
  ODS('TIDelphiIDE.ModalEdit');
end;    { ModalEdit }

procedure TRAIDE.SelectionChanged;
begin
  ODS('TIDelphiIDE.SelectionChanged');
end;    { SelectionChanged }

function TRAIDE.LockState: Boolean;
begin
  ODS('TIDelphiIDE.LockState');
  Result := False;
end;    { LockSate }

function TRAIDE.GetToolServices: TIToolServices;
begin
  ODS('TIDelphiIDE.GetToolServices');
  Result := ToolServices;
end;    { GetToolServices }

procedure TRAIDE.GetDesignerOptions(var Options: TDesignerOptions);
begin
  ODS('TIDelphiIDE.GetDesignerOptions');
end;    { GetDesignerOptions }

procedure TRAIDE.SelectItemName(const PropertyName: string);
begin
  ODS('TIDelphiIDE.SelectItemName');
end;    { SelectItemName }


function TRAIDE.GetBaseRegKey: string;
begin
  ODS('TIDelphiIDE.GetBaseRegKey');
  Result := ToolServices.GetBaseRegistryKey;
end;

function TRAIDE.GetPathAndBaseExeName: string;
begin
  ODS('TIDelphiIDE.GetPathAndBaseExeName');
  Result := ParamStr(0);
end;

function TRAIDE.GetAppHandle: HWND;
begin
  ODS('TIDelphiIDE.GetAppHandle');
  Result := ToolServices.GetParentHandle;
end;

function TRAIDE.GetToolSelected: Boolean;
begin
  ODS('TIDelphiIDE.GetToolSelected');
  Result := False;
end;

{$IFDEF RA_D3}
function TRAIDE.GetCurCompClass: TIPaletteItem;
{$ELSE}
function TRAIDE.GetCurCompClass: IPaletteItem;
{$ENDIF}
begin
  ODS('TIDelphiIDE.GetCurCompClass');
  Result := nil;
end;

{$IFDEF RA_D3}
function TRAIDE.GetPaletteItem(ComponentClass: TComponentClass): TIPaletteItem;
{$ELSE}
function TRAIDE.GetPaletteItem(ComponentClass: TComponentClass): IPaletteItem;
{$ENDIF}
begin
  ODS('TIDelphiIDE.GetPaletteItem');
  Result := nil;
end;

function TRAIDE.GetCurTime: Integer;
begin
  ODS('TIDelphiIDE.GetCurTime');
  Result := 0;
end;

procedure TRAIDE.ResetCompClass;
begin
  ODS('TIDelphiIDE.ResetCompClass');
end;

procedure TRAIDE.ValidateActiveModule;
begin
  ODS('TIDelphiIDE.ValidateActiveModule');
end;

procedure TRAIDE.ExpertsLoaded;
begin
  ODS('TIDelphiIDE.ExpertsLoaded');
end;

function TRAIDE.CreateBackupFile: Boolean;
begin
  ODS('TIDelphiIDE.CreateBackupFile');
  Result := False;
end;

function TRAIDE.MakeBackupFileName(const FileName: string): string;
begin
  ODS('TIDelphiIDE.MakeBackupFileName');
  Result := '';
end;

{$IFDEF RA_D3}
function TRAIDE.GetFileSystem(const Ident: string): TIVirtualFileSystem;
{$ELSE}
function TRAIDE.GetFileSystem(const Ident: string): IOTAFileSystem;
{$ENDIF}
begin
  ODS('TIDelphiIDE.GetFileSystem');
  Result := nil;
end;

{$IFDEF RA_D5H}
procedure TRAIDE.AddExpert(const Expert: TIExpert);
{$ELSE}
procedure TRAIDE.AddExpert(Expert: TIExpert);
{$ENDIF}
begin
  ODS('TIDelphiIDE.AddExpert');
end;

{$IFDEF RA_D5H}
procedure TRAIDE.RemoveExpert(const Expert: TIExpert);
{$ELSE}
procedure TRAIDE.RemoveExpert(Expert: TIExpert);
{$ENDIF}
begin
  ODS('TIDelphiIDE.RemoveExpert');
end;

procedure TRAIDE.ActiveFormModified;
var
  i: Integer;
begin
  ODS('TIDelphiIDE.ActiveFormModified');
  if Assigned(CompLib) then
   {$IFDEF RA_D5H}
    with (CompLib as IInstance).GetInstance as TRACompLib do
   {$ELSE}
    with CompLib as TRACompLib do
   {$ENDIF}
      for i := 0 to WindowList.Count - 1 do
        if TObject(WindowList[i]) is TDesignWindow then
          TDesignWindow(WindowList[i]).FormModified;
end;

{$IFDEF RA_D5H}
procedure TRAIDE.DesignerClosed(const Designer: TDesigner);
var
  i: Integer;
begin
  ODS('TRAIDE.DesignerClosed');
  if Assigned(CompLib) then
   {$IFDEF RA_D5H}
    with (CompLib as IInstance).GetInstance as TRACompLib do
   {$ELSE}
    with CompLib as TRACompLib do
   {$ENDIF}
      for i := 0 to WindowList.Count - 1 do
        if TObject(WindowList[i]) is TDesignWindow then
          TDesignWindow(WindowList[i]).FormClosed(Designer.Form);
end;
{$ENDIF}

procedure TRAIDE.ExecDesignDialog(DesignDialog: TDesignDialog);
begin
  ODS('TIDelphiIDE.ExecDesignDialog');
end;

{$IFDEF RA_D5H}
procedure TRAIDE.ComponentRenamed(const CurName: string; const NewName: string); 
begin
  ODS('TIDelphiIDE.ComponentRenamed');
end;

function TRAIDE.GetPaletteItemByName(const ClassName: String): IPaletteItem;
begin
  ODS('TIDelphiIDE.GetPaletteItemByName');
end;

function TRAIDE.GetWorkspaceOrigin: TPoint;
begin
  ODS('TIDelphiIDE.GetWorkspaceOrigin');
end;

procedure TRAIDE.OpenForm(const FormName: String; Show: Boolean);
begin
  ODS('TIDelphiIDE.OpenForm');
end;

procedure TRAIDE.RaiseException(const Message: String);
begin
  ODS('TIDelphiIDE.RaiseException');
end;

procedure TRAIDE.ShowClassHelp(const ClassName: String);
begin
  ODS('TIDelphiIDE.ShowClassHelp');
end;

function TRAIDE.AddWizard(const Wizard: IOTAWizard): Integer;
begin
  ODS('TIDelphiIDE.AddWizard');
  Result := -1;
end;

procedure TRAIDE.RequestTemplate(const CompName: String; Stream: TStream; Dependencies: TStrings); 
begin
  ODS('TIDelphiIDE.RequestTemplate');
end;

procedure TRAIDE.RemoveWizard(Index: Integer);
begin
  ODS('TIDelphiIDE.RemoveWizard');
end;

procedure TRAIDE.ValidateFormEdit;
begin
  ODS('TIDelphiIDE.ValidateFormEdit');
end;

function TRAIDE.WinHelp(const HelpFile: String; Command: Integer; Data: Integer): Boolean;
begin
  ODS('TIDelphiIDE.WinHelp');
  Result := False;
end;
{$ENDIF RA_D5H}


{************************* TRACompLib *************************}
constructor TRACompLib.Create;
begin
  inherited Create;
  WindowList := TList.Create;
end;    { Create }

destructor TRACompLib.Destroy;
begin
  WindowList.Free;
  inherited Destroy;
end;    { Destroy }

procedure TRACompLib.RegisterDesignWindow(Window: TForm);
begin
  ODS('TILibrary.RegisterDesignWindow');
  WindowList.Add(Window);
end;

procedure TRACompLib.UnregisterDesignWindow(Window: TForm);
begin
  ODS('TILibrary.UnregisterDesignWindow');
  WindowList.Remove(Window);
end;

{$IFDEF RA_D3}
procedure TRACompLib.GetSelection(ASelection: TComponentList);
{$ENDIF}
{$IFDEF RA_D4}
procedure TRACompLib.GetSelection(ASelection: IDesignerSelections);
{$ENDIF}
{$IFDEF RA_D5H}
procedure TRACompLib.GetSelection(const ASelection: IDesignerSelections);
{$ENDIF}
begin
  ODS('TILibrary.GetSelection');
end;

{$IFDEF RA_D3}
procedure TRACompLib.SetSelection(Designer: TDesigner; DesignWindow: TForm;
  ASelection: TComponentList);
{$ENDIF}
{$IFDEF RA_D4}
procedure TRACompLib.SetSelection(const Designer: IDesigner; DesignWindow: TForm;
  ASelection: IDesignerSelections);
var
  ComponentList: IComponentList;
{$ENDIF}
{$IFDEF RA_D5H}
procedure TRACompLib.SetSelection(const Designer: IDesigner; DesignWindow: TCustomForm;
  ASelection: IDesignerSelections);
var
  ComponentList: IComponentList;
{$ENDIF}
begin
  ODS('TILibrary.SetSelection');
{$IFDEF RA_D3}
  PropertyInspector.UpdatePropList(Designer as TFormDesigner, DesignWindow,
    ASelection);
{$ELSE}
  if ASelection.QueryInterface(IComponentList, ComponentList) <> S_OK then
    Error('Can''t get ComponentList from IDesignerSelections');
  PropertyInspector.UpdatePropList(Designer as IFormDesigner,
    DesignWindow as TForm, ComponentList.GetComponentList);
{$ENDIF}
//  (Designer as TFormDesigner).SetSelections(ASelection);
end;    { SetSelection }

{$IFDEF RA_D3}
function TRACompLib.CreateForm(AModule: TIModule; const AFileName: string;
  Existing: Boolean; const AFormName: string; const AAncestor: string;
  const AFileSystem: string): TIForm;
{$ENDIF}
{$IFDEF RA_D4}
function TRACompLib.CreateForm(AModule: IDesignerModule; const AFileName: string;
  Existing: Boolean; const AFormName: string; const AAncestor: string;
  const AFileSystem: string): TIForm;
{$ENDIF}
{$IFDEF RA_D5H}
function TRACompLib.CreateForm(const AModule: IDesignerModule; const AFileName: string;
  Existing: Boolean; const AFormName: string; const AAncestor: string;
  const AFileSystem: string): TIForm;
{$ENDIF}
begin
  ODS('TILibrary.CreateForm');
  Result := nil;
end;

procedure TRACompLib.ShowWindows;
var
  i: Integer;
begin
  ODS('TILibrary.ShowWindows');
  for i := 0 to WindowList.Count - 1 do
    TForm(WindowList[i]).Show;
end;

procedure TRACompLib.HideWindows;
var
  i: Integer;
begin
  ODS('TILibrary.HideWindows');
  for i := 0 to WindowList.Count - 1 do
    TForm(WindowList[i]).Hide;
end;

procedure TRACompLib.SetLockState(State: Boolean);
begin
  ODS('TILibrary.SetLockState');
end;

procedure TRACompLib.DesignerOptionsChanged;
begin
  ODS('TILibrary.DesignerOptionsChanged');
end;

function TRACompLib.EditAction(Action: TEditAction): Boolean;
begin
  ODS('TILibrary.EditAction');
  Result := False;
end;

function TRACompLib.FindFile(const FileName: string): TIFile;
begin
  ODS('TILibrary.FindFile');
  Result := nil;
end;

function TRACompLib.GetActiveForm: TIForm;
begin
  ODS('TILibrary.GetActiveForm');
  if Assigned(ActiveDesigner) then
  begin
   {$WARNINGS OFF}
    Result := TRAForm.Create;
   {$WARNINGS ON}
    (Result as TRAForm).Designer := ActiveDesigner;
  end
  else
    Result := nil;
end;

procedure TRACompLib.GetClassUnits(const ClassName: string; Proc: TGetStrProc);
begin
  ODS('TILibrary.GetClassUnits');
end;

function TRACompLib.GetCompClass(Index: Integer): TICompClass;
begin
  ODS('TILibrary.GetCompClass');
  Result := nil;
end;

function TRACompLib.GetCompClassCount: Integer;
begin
  ODS('TILibrary.GetCompClassCount');
  Result := 0;
end;

procedure TRACompLib.GetCompHierarchy(Proc: TGetStrProc);
begin
  ODS('TILibrary.GetCompHierarchy');
end;

function TRACompLib.GetComponentBitmap: HBITMAP;
begin
  ODS('TILibrary.GetComponentBitmap');
  Result := 0;
end;

function TRACompLib.GetEditState: TEditState;
begin
  ODS('TILibrary.GetEditState');
end;

function TRACompLib.GetModuleCount: Integer;
begin
  ODS('TILibrary.GetModuleCount');
  Result := 0;
end;

function TRACompLib.GetModuleName(Index: Integer): string;
begin
  ODS('TILibrary.GetModuleName');
  Result := '';
end;

procedure TRACompLib.GetProperties(PropKind: TPropKind; Proc: TGetPropProc);
begin
  ODS('TILibrary.GetProperties');
end;

function TRACompLib.GetSelectionName: string;
begin
  ODS('TILibrary.GetSelectionName');
  Result := '';
end;

function TRACompLib.GetSelectionType: string;
begin
  ODS('TILibrary.GetSelectionType');
  Result := '';
end;

procedure TRACompLib.ModalEditDone(ReturnWindow: Pointer);
begin
  ODS('TILibrary.ModalEditDone');
end;

procedure TRACompLib.RaiseException(const Message: string);
begin
  ODS('TILibrary.RaiseException');
end;

procedure TRACompLib.SetProjectName(const Name: string);
begin
  ODS('TILibrary.SetProjectName');
end;

procedure TRACompLib.RegisterDragTarget(const SourceStream: string; TargetClass: TDragTargetClass);
begin
  ODS('TILibrary.RegisterDragTarget');
end;

procedure TRACompLib.CloseDesignWindows;
var
  i: Integer;
begin
  ODS('TILibrary.CloseDesignWindows');
  for i := 0 to WindowList.Count - 1 do
    TForm(WindowList[i]).Free;
end;    {  }

{$IFDEF RA_D5H}
procedure TRACompLib.CopySelectionToStream(S: TMemoryStream; UnitDependencies: TStrings);
begin
  ODS('TILibrary.CopySelectionToStream');
end;

function TRACompLib.CreateFromStream(const AModule: IDesignerModule; const AFileName: String; const AFileSystem: String; const IStream: IStream): TIForm;
begin
  ODS('TILibrary.CreateFromStream');
  Result := nil;
end;

function TRACompLib.CreateNewForm(const AModule: IDesignerModule; const AFileName: String; const Creator: IUnknown): TIForm;
begin
  ODS('TILibrary.CreateNewForm');
  Result := nil;
end;

function TRACompLib.FindForm(const FormName: String): TIForm;
begin
  ODS('TILibrary.FindForm');
  Result := nil;
end;

function TRACompLib.GetFirstSelectionType: String;
begin
  ODS('TILibrary.GetFirstSelectionType');
end;

procedure TRACompLib.GetFormNames(Proc: TGetStrProc);
begin
  ODS('TILibrary.GetFormNames');
end;

function TRACompLib.GetOleCtrlHelp(const Member: String; var HelpCtx: Integer; var HelpFile: String): Boolean;
begin
  ODS('TILibrary.GetOleCtrlHelp');
  Result := False;
end;

procedure TRACompLib.IsDesignMsg(var Msg: tagMSG; var Handled: Boolean);
begin
  ODS('TILibrary.IsDesignMsg');
end;

function TRACompLib.IsTopmostForm(Wnd: HWND): Boolean;
begin
  ODS('TILibrary.IsTopmostForm');
  Result := Wnd = GetTopWindow(0);
end;

function TRACompLib.IsValidTemplateSelection: Boolean;
begin
  ODS('TILibrary.IsValidTemplateSelection');
  Result := False;
end;

procedure TRACompLib.PasteSelectionFromStream(S: TMemoryStream; Parent: TComponent; const Rect: TRect);
begin
  ODS('TILibrary.PasteSelectionFromStream');
end;

procedure TRACompLib.UnregisterExpertGroup(Group: Integer);
begin
  ODS('TILibrary.UnregisterExpertGroup');
end;

procedure TRACompLib.CancelModes;
begin
  ODS('TILibrary.CancelModes');
end;

function TRACompLib.GetControlsLocked: Boolean;
begin
  ODS('TILibrary.GetControlsLocked');
  Result := False;
end;

procedure TRACompLib.FormActivated(const AForm: TIForm);
begin
  ODS('TILibrary.FormActivated');
end;

procedure TRACompLib.UpdateFormDependents;
begin
  ODS('TILibrary.UpdateFormDependents');
end;

procedure TRACompLib.UpdateSelections(AllowNoSelection: Boolean);
begin
  ODS('TILibrary.UpdateSelections');
end;

procedure TRACompLib.RemoveDependenciesOn(const Component: IComponent);
begin
  ODS('TILibrary.RemoveDependenciesOn');
end;

procedure TRACompLib.FormListAdd(const AForm: TIForm);
begin
  ODS('TILibrary.FormListAdd');
end;

procedure TRACompLib.FormListRemove(const AForm: TIForm);
begin
  ODS('TILibrary.FormListRemove');
end;

procedure TRACompLib.FormModified(const AForm: TIForm);
begin
  ODS('TILibrary.FormModified');
end;

procedure TRACompLib.FormGoDormant(const AForm: TIForm);
begin
  ODS('TILibrary.FormGoDormant');
end;

procedure TRACompLib.EditorGetProperties(Proc: TGetPropProc; Editor: TPropertyEditor);
begin
  ODS('TILibrary.EditorGetProperties');
end;

function TRACompLib.GetForms(Index: Integer): TIForm;
begin
  ODS('TILibrary.GetForms');
  Result := nil; {?}
end;

function TRACompLib.GetFormsCount: Integer;
begin
  ODS('TILibrary.GetFormsCount');
  Result := 0; {?}
end;

function TRACompLib.GetFirstSelectionClass: TClass;
begin
  ODS('TILibrary.GetFirstSelectionClass');
  Result := nil; {?}
end;

function TRACompLib.IsNestable(const ClassName: String): Boolean;
begin
  ODS('TILibrary.IsNestable');
  Result := False;
end;
{$ENDIF RA_D5H}

{ TRAForm }

{$IFDEF RA_D3}
function TRAForm.GetDesigner: TFormDesigner;
{$ELSE}
function TRAForm.GetDesigner: IFormDesigner;
{$ENDIF}
begin
  ODS('TIForm.GetDesigner');
  Result := Designer;
end;

{*************************** TRAMenuItemIntf ***************************}
constructor TRAMenuItemIntf.Create(AMenuItem: TMenuItem);
begin
  inherited Create;
  FMenuItem := AMenuItem;
end;

function TRAMenuItemIntf.DestroyMenuItem: Boolean;
begin
  if FMenuItem is TRAFDMenuItem then
    FMenuItem.Free;
  Result := True;  
end;

function TRAMenuItemIntf.GetIndex: Integer;
begin
  Result := FMenuItem.MenuIndex;
end;

function TRAMenuItemIntf.GetItemCount: Integer;
begin
  Result := FMenuItem.Count;
end;

function TRAMenuItemIntf.GetItem(Index: Integer): TIMenuItemIntf;
begin
  Result := TRAMenuItemIntf.Create(FMenuItem.Items[Index]);
end;

function TRAMenuItemIntf.GetName: string;
begin
  Result := FMenuItem.Name;
end;

function TRAMenuItemIntf.GetParent: TIMenuItemIntf;
begin
  Result := TRAMenuItemIntf.Create(FMenuItem.Parent);
end;

function TRAMenuItemIntf.GetCaption: string;
begin
  Result := FMenuItem.Caption;
end;

function TRAMenuItemIntf.SetCaption(const Caption: string): Boolean;
begin
  FMenuItem.Caption := Caption;
  Result := True;
end;

function TRAMenuItemIntf.GetShortCut: Integer;
begin
  Result := FMenuItem.ShortCut;
end;

function TRAMenuItemIntf.SetShortCut(ShortCut: Integer): Boolean;
begin
  FMenuItem.ShortCut := ShortCut;
  Result := True;
end;

function TRAMenuItemIntf.GetFlags: TIMenuFlags;
begin
  Result := [];
  if FMenuItem.Enabled then Result := Result + [mfEnabled];
  if FMenuItem.Visible then Result := Result + [mfVisible];
  if FMenuItem.Checked then Result := Result + [mfChecked];
  if FMenuItem.Break = mbBreak then Result := Result + [mfBreak];
  if FMenuItem.Break = mbBarBreak then Result := Result + [mfBarBreak];
  if FMenuItem.RadioItem then Result := Result + [mfRadioItem];
 { mfInvalid ? }
end;

function TRAMenuItemIntf.SetFlags(Mask, Flags: TIMenuFlags): Boolean;
begin
  Result := False;
end;

function TRAMenuItemIntf.GetGroupIndex: Integer;
begin
  Result := FMenuItem.GroupIndex;
end;

function TRAMenuItemIntf.SetGroupIndex(GroupIndex: Integer): Boolean;
begin
  FMenuItem.GroupIndex := GroupIndex;
  Result := True;
end;

function TRAMenuItemIntf.GetHint: string;
begin
  Result := FMenuItem.Hint;
end;

function TRAMenuItemIntf.SetHint(Hint: string): Boolean;
begin
  FMenuItem.Hint := Hint;
  Result := True;
end;

function TRAMenuItemIntf.GetContext: Integer;
begin
  Result := FMenuItem.HelpContext;
end;

function TRAMenuItemIntf.SetContext(Context: Integer): Boolean;
begin
  FMenuItem.HelpContext := Context;
  Result := True;
end;

function TRAMenuItemIntf.GetOnClick: TIMenuClickEvent;
begin
  if FMenuItem is TRAFDMenuItem then
    Result := (FMenuItem as TRAFDMenuItem).FOnClick;
end;

function TRAMenuItemIntf.SetOnClick(Click: TIMenuClickEvent): Boolean;
begin
  Result := FMenuItem is TRAFDMenuItem;
  if Result then
    (FMenuItem as TRAFDMenuItem).FOnClick := Click;
end;

function TRAMenuItemIntf.InsertItem(Index: Integer; Caption, Name, Hint: string;
	ShortCut, Context, GroupIndex: Integer; Flags: TIMenuFlags;
	EventHandler: TIMenuClickEvent): TIMenuItemIntf;
var
  MenuItem: TRAFDMenuItem;
begin
  MenuItem := TRAFDMenuItem.Create(AppBuilder.MainMenu1);
  Result := GetMenuItemIntf(MenuItem);
  MenuItem.Caption := Caption;
  MenuItem.Name := Name;
  MenuItem.Hint := Hint;
  MenuItem.ShortCut := ShortCut;
  MenuItem.HelpContext := Context;
  MenuItem.GroupIndex := GroupIndex;
  Result.SetFlags(Flags, Flags);
  MenuItem.FOnClick := EventHandler;
  if (Index > -1) and (Index < FMenuItem.Count) then
    FMenuItem.Insert(Index, MenuItem) else
    FMenuItem.Add(MenuItem);
end;

{ TRAMainMenuIntf }
function TRAMainMenuIntf.GetMenuItems: TIMenuItemIntf;
begin
  Result := TRAMenuItemIntf.Create(AppBuilder.MainMenu1.Items);
end;

function TRAMainMenuIntf.FindMenuItem(const Name: string): TIMenuItemIntf;
var
  MenuItem: TMenuItem;
begin
  MenuItem := AppBuilder.FindComponent(Name) as TMenuItem;
  if MenuItem <> nil then
		Result := GetMenuItemIntf(MenuItem) else
    Result := nil;
end;

{ TRAFDMenuItem }
procedure TRAFDMenuItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(GetMenuItemIntf(Self));
end;

destructor TRAFDMenuItem.Destroy;
begin
  inherited Destroy;
end;    { Destroy }



function GetMenuItemIntf(MenuItem: TMenuItem): TIMenuItemIntf;
var
  i: integer;
  MIRec: TMIRec;
begin
  for i := 0 to MenuItemIntfList.Count - 1 do
  begin
    MIRec := TMIRec(MenuItemIntfList[i]);
    if MIRec.MenuItem = MenuItem then
    begin
      Result := MIRec.MenuItemIntf;
      Exit;
    end;
  end;
  MIRec := TMIRec.Create;
  MIRec.MenuItem := MenuItem;
  MIRec.MenuItemIntf := TRAMenuItemIntf.Create(MenuItem);
  Result := MIRec.MenuItemIntf;
end;


{*************************** TRAEditorInterface ***************************}

function TRAEditorInterface.CreateReader: TIEditReader;
begin
  ODS('TIEditorInterface.CreateReader');
  Result := TRAEditReader.Create;
  (Result as TRAEditReader).FileEditor := FileEditor;
end;

function TRAEditorInterface.CreateWriter: TIEditWriter;
begin
  ODS('TIEditorInterface.CreateWriter');
  Result := TRAEditWriter.Create(FileEditor);
end;

function TRAEditorInterface.FileName: string;
begin
  ODS('TIEditorInterface.FileName');
  Result := FileEditor.FileName;
end;

function TRAEditorInterface.LinesInBuffer: Longint;
begin
  ODS('TIEditorInterface.LinesInBuffer');
  Result := FileEditor.Lines.Count;
end;

function TRAEditorInterface.BufferModified: Boolean;
begin
  ODS('TIEditorInterface.BufferModified');
  Result := FileEditor.Modified;
end;

function TRAEditorInterface.MarkModified: Boolean;
begin
  ODS('TIEditorInterface.MarkModified');
  FileEditor.Modified := True;
  Result := True;
end;

function TRAEditorInterface.SetSyntaxHighlighter(SyntaxHighlighter: TSyntaxHighlighter): TSyntaxHighlighter;
begin
  ODS('TIEditorInterface.SetSyntaxHighlighter');
  Result := shNone;
end;

function TRAEditorInterface.GetViewCount: Integer;
begin
  ODS('TIEditorInterface.GetViewCount');
  Result := 1;
end;

function TRAEditorInterface.GetView(Index: Integer): TIEditView;
begin
  ODS('TIEditorInterface.GetView');
  Result := TRAEditView.Create;
  (Result as TRAEditView).FileEditor := FileEditor;
end;

function TRAEditorInterface.CreateUndoableWriter: TIEditWriter;
begin
  ODS('TIEditorInterface.CreateUndoableWriter');
 { !!! CREATES NOT UNDOABLE WRITER !!! }
  Result := CreateWriter;
end;

function TRAEditorInterface.GetBlockAfter: TCharPos;
begin
  ODS('TIEditorInterface.GetBlockAfter');
end;

function TRAEditorInterface.GetBlockStart: TCharPos;
begin
  ODS('TIEditorInterface.GetBlockStart');
end;

function TRAEditorInterface.GetBlockType: TBlockType;
begin
  ODS('TIEditorInterface.GetBlockType');
  Result := btNonInclusive;
end;

function TRAEditorInterface.GetBlockVisible: Boolean;
begin
  ODS('TIEditorInterface.GetBlockVisible');
  Result := False;
end;

procedure TRAEditorInterface.SetBlockAfter(Value: TCharPos);
begin
  ODS('TIEditorInterface.SetBlockAfter');
end;

procedure TRAEditorInterface.SetBlockStart(Value: TCharPos);
begin
  ODS('TIEditorInterface.SetBlockStart');
end;

procedure TRAEditorInterface.SetBlockType(Value: TBlockType);
begin
  ODS('TIEditorInterface.SetBlockType');
end;

procedure TRAEditorInterface.SetBlockVisible(Value: Boolean);
begin
  ODS('TIEditorInterface.SetBlockVisible');
end;

{ TRAEditReader }
function TRAEditReader.GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint;
begin
  ODS('TIEditReader.GetText: Position = ' + IntToStr(Position) + ', Count = ' + IntToStr(Count));
  Result := FileEditor.GetText(Position, Buffer, Count);
  ODS(' ..ok. Result =' + IntToStr(Result));
end;

(*
function TRAEditReader.GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint;
var
  S: string;
  P: PChar;
begin
  ODS('TIEditReader.GetText: Position = ' + IntToStr(Position) + ', Count = ' + IntToStr(Count));
  if Buffer = nil then ODS(' Buffer = nil !!');
//  Result := FileEditor.GetText(Position, Buffer, Count);
  S := FileEditor.Lines.Text;
  if Position <= Length(S) then
  begin
    S := Copy(S, Position + 1, Count);
    Result := Length(S);
    P := PChar(S);
    Move(P[0], Buffer[0], Result);
   // StrLCopy(Buffer, PChar(S), Result - 1);
   // Buffer[Result] := S[Result];
    ODS(' move .. ok');
  end
  else
    Result := 0;
  ODS(' ..ok. Result =' + IntToStr(Result));
end;
*)


{ TRAEditWriter }
constructor TRAEditWriter.Create(AFileEditor: TFileEditor);
begin
  inherited Create;
  FileEditor := AFileEditor;
  Text := FileEditor.Lines.Text;
  FPos := 1;
end;    { Create }

destructor TRAEditWriter.Destroy;
begin
  FileEditor.BeginUpdate;
  try
    FileEditor.Lines.Text := Text;
    FileEditor.SelStart := FPos;
  finally
    FileEditor.EndUpdate;
  end;    
  inherited Destroy;
end;    { Destroy }

function TRAEditWriter.CopyTo(Pos: Longint): Boolean;
begin
  ODS('TIEditWriter.CopyTo: Pos = ' + IntToStr(Pos));
  FPos := Pos;
  Result := True;
end;

function TRAEditWriter.DeleteTo(Pos: Longint): Boolean;
begin
  ODS('TIEditWriter.DeleteTo: Pos = ' + IntToStr(Pos));
  Delete(Text, FPos, Pos);
  Result := True;
end;

function TRAEditWriter.Insert(Text: PChar): Boolean;
begin
  ODS('TIEditWriter.Insert: Text = ' + SubStr(Text, 0, #13));
  System.Insert(Text, Self.Text, FPos);
  inc(FPos, Length(Text)); 
  Result := True;
end;

function TRAEditWriter.Position: Longint;
begin
  ODS('TIEditWriter.Position');
  Result := FPos;
end;

function TRAEditWriter.GetCurrentPos: TCharPos;
begin
  ODS('TIEditWriter.GetCurrentPos');
 { !!!! NOT COMPLETE !!!! } 
  Result.CharIndex := 0;
  Result.Line := 1;
end;


{ TRAEditView }
function TRAEditView.GetPos(Index: Integer): TEditPos;
begin
  ODS('TIEditView.GetPos');
  case Index of
    EditIntf.CursorPos:
      begin
        Result.Col := FileEditor.CaretX + 1;
        Result.Line := FileEditor.CaretY + 1;
      end;
    ViewTopPos:
      begin
        Result.Col := FileEditor.LeftCol + 1;
        Result.Line := FileEditor.TopRow + 1;
      end;
  end;
end;

procedure TRAEditView.SetPos(Index: Integer; Value: TEditPos);
begin
  ODS('TIEditView.SetPos ' + IntToStr(Index) + ' Line = ' + IntToStr(Value.Line) + ' Col = ' + IntToStr(Value.Col) + ', LineCount =' + IntToStr(FileEditor.Lines.Count));
  FileEditor.BeginUpdate;
  try
    case Index of
      EditIntf.CursorPos:
        begin
          FileEditor.SelLength := 0;
          FileEditor.SetCaret(Value.Col - 1, Value.Line - 1);
        end;
      ViewTopPos:
        FileEditor.SetLeftTop(Value.Col - 1, Value.Line - 1);
    end;
    FileEditor.ToggleToFront;
  finally
    FileEditor.EndUpdate;
  end;
end;

function TRAEditView.GetViewSize: TSize;
begin
  ODS('TIEditView.GetViewSize');
  Result.cx := FileEditor.VisibleColCount;
  Result.cy := FileEditor.VisibleRowCount;
end;

function TRAEditView.PosToCharPos(Pos: Longint): TCharPos;
var
  X, Y: Integer;
begin
  ODS('TIEditView.PosToCharPos');
  FileEditor.CaretFromPos(Pos, X, Y);
  Result.CharIndex := X;
  Result.Line := Y + 1;
end;

function TRAEditView.CharPosToPos(CharPos: TCharPos): Longint;
begin
  ODS('TIEditView.CharPosToPos');
  Result := FileEditor.PosFromCaret(CharPos.CharIndex, CharPos.Line - 1);
end;

procedure TRAEditView.ConvertPos(EdPosToCharPos: Boolean; var EditPos: TEditPos;
	var CharPos: TCharPos);
begin
  ODS('TIEditView.ConvertPos');
  if EdPosToCharPos then
  begin
    CharPos.Line := Min(EditPos.Line, FileEditor.Lines.Count);
    if CharPos.Line - 1 <= FileEditor.Lines.Count then
      CharPos.CharIndex := Min(EditPos.Col - 1, Length(
        FileEditor.Lines[CharPos.Line - 1]))
    else
      CharPos.CharIndex := 0;
  end
  else
  begin
    EditPos.Line := CharPos.Line;
    EditPos.Col := CharPos.CharIndex + 1;
  end;
end;

procedure TRAEditView.GetAttributeAtPos(const EdPos: TEditPos; var Element, LineFlag: Integer);
begin
  ODS('TIEditView.GetAttributeAtPos');
  Element := SyntaxOff;
  LineFlag := 0;
end;



{************************* Experts *************************}

function RAExpertRegisterProc(Expert: TIExpert): Boolean;

  procedure AddExpert;
  var
    ExpertRec: TExpertRec;
  begin
    ExpertRec := TExpertRec.Create;
    ExpertRec.Expert := Expert;
    Expert.AddRef;
    ExpertList.Add(ExpertRec);
  end;

begin
 // Expert.AddRef;
  case Expert.GetStyle of
    esStandard:
      begin
       {Add to Tools menu}
        AddExpert;
      end;
    esForm, esProject:
      AddExpert;
    esAddIn:
     // ExpertList.Add(Expert);
  end;
  Result := True;
end;

procedure IDELoadExpert(const FileName: TFileName);
var
  ExpertRec: TExpertRec;
  H: HModule;
  Proc: procedure;
  TerminateProc: TExpertTerminateProc;
begin
  if ToolServices = nil then Exit;
  ODS('Loading expert ' + FileName);
	H := LoadLibrary(PChar(FileName));
	if H = 0 then RaiseLastWin32Error;
	Proc := GetProcAddress(H, ExpertEntryPoint);
	if Assigned(Proc) then
	begin
		TerminateProc := nil;
		if TExpertInitProc(Proc)(ToolServices, RAExpertRegisterProc,
			 TerminateProc) then
    begin
      ExpertRec := TExpertRec.Create;
      ExpertRec.Module := H;
      ExpertRec.TerminateProc := TerminateProc;
      ExpertList.Add(ExpertRec)
    end;
	end
	else
		FreeLibrary(H);
end;

procedure IDETerminateExperts;
var
  ExpertRec: TExpertRec;
  i: Integer;
begin
  for i := 0 to ExpertList.Count - 1 do
  begin
    ExpertRec := TExpertRec(ExpertList[i]);
    if Assigned(ExpertRec.TerminateProc) then
      ExpertRec.TerminateProc;
    ExpertRec.TerminateProc := nil;
  end;
end;    { IDETerminateExperts }

procedure IDEUnloadExperts;
var
  ExpertRec: TExpertRec;
begin
  while ExpertList.Count > 0 do
  begin
    ExpertRec := TExpertRec(ExpertList[ExpertList.Count - 1]);
    if Assigned(ExpertRec.Expert) then
      ExpertRec.Expert.Release;
    if ExpertRec.Module > 0 then
      FreeLibrary(ExpertRec.Module);
    ExpertRec.Free;
    ExpertList.Delete(ExpertList.Count - 1);
  end;
end;    { DoneExperts }

function GetExpertList: TList;
begin
  Result := ExpertList;
end;

procedure IDECloseDesignWindows;
begin
  if Assigned(CompLib) then
   {$IFDEF RA_D5H}
    ((CompLib as IInstance).GetInstance as TRACompLib).CloseDesignWindows
   {$ELSE}
    (CompLib as TRACompLib).CloseDesignWindows;
   {$ENDIF}
end;    { CloseDesignWindows }


{
procedure AbstractErrorHandler;
begin
  TComponent(nil).ClassName;
 // raise EDivByZero.Create('Abstract Error - TD must handle it');
end;
}

procedure RAExceptionHandler;
begin
  SysUtils.ShowException(ExceptObject, ExceptAddr);
end;    { RAExceptionHandler }


var
  RAIDE: TRAIDE;

initialization
  ODS('RAFDIDE.initialization..');
  ExceptionHandler := RAExceptionHandler;
  LibraryExpertProc := RAExpertRegisterProc;
  RAToolServices := TRAToolServices.Create;
  ToolServices := RAToolServices;
  RAIDE := TRAIDE.Create;
  DelphiIDE := RAIDE;
 {$IFDEF RA_D5H}
  RADelphiIDE := RAIDE;
 {$ENDIF}
  RAFD.BaseRegKey := DelphiIDE.GetBaseRegKey;
  CompLib := TRACompLib.Create;
  MenuItemIntfList := TList.Create;
  ExpertList := TList.Create;
  //DesignPackageList := TDesignPackageList.Create;
  ODS('RAFDIDE.initialization.. done');

finalization
  ODS('RAFDIDE.finalization..');
  IDETerminateExperts;
  IDEUnloadExperts;
  //DesignPackageList.Free;
  ExpertList.Free;
 {$IFNDEF RA_D5H}
  DelphiIDE.Release;
 {$ENDIF}
  DelphiIDE := nil; {Delphi5: DelphiIDE._Release}
 {$IFNDEF RA_D5H}
  CompLib.Release;
 {$ENDIF}
  CompLib := nil; {Delphi5: CompLib._Release}
  ToolServices.Release;
  ToolServices := nil;
  ClearList(MenuItemIntfList);
  MenuItemIntfList.Free;
  ODS('RAFDIDE.finalization.. done');
end.


{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       description : appbuilder main window

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDAppBuilder;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, RARegAuto, Buttons,
  {$IFDEF RA_D6H} DesignIntf, {$ELSE} DsgnIntf, {$ENDIF RA_D6H}
  ToolIntf, ExptIntf, RAHLEdPropDlg, RAI2,
  RAFDDesigner, RARTTI, RAFDEditor, RAFDCompat, RAFDRun;

type

  TRAAppBuilder = class;
  TWhatRun = (ruProject, ruUnit, ruForm, ruReportPreview);

  TRAFDAppBuilder = class(TForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    FileExitItem: TMenuItem;
    N2: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileNewItem: TMenuItem;
    Bevel1: TBevel;
    OpenDialog1: TOpenDialog;
    FileCloseItem: TMenuItem;
    pnlToolbar: TPanel;
    FileSaveAllItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    FileCloseAllItem: TMenuItem;
    ViewsMenu: TMenuItem;
    ViewAlignItem: TMenuItem;
    ViewObjInspItem: TMenuItem;
    EditMenu: TMenuItem;
    EditDeleteItem: TMenuItem;
    N1: TMenuItem;
    ViewPaletteItem: TMenuItem;
    SpeedPanel: TPanel;
    Splitter1: TSplitter;
    FileNewFormItem: TMenuItem;
    raDesktop: TRegAuto;
    N3: TMenuItem;
    ViewToggleFormUnitItem: TMenuItem;
    ComponentMenu: TMenuItem;
    RunMenu: TMenuItem;
    RunFormItem: TMenuItem;
    RunReportPreviewItem: TMenuItem;
    ViewWindowListItem: TMenuItem;
    SearchMenu: TMenuItem;
    ToolsMenu: TMenuItem;
    ProjectMenu: TMenuItem;
    ProjectAddItem: TMenuItem;
    PrjViewSourceItem: TMenuItem;
    ToolsOptionsItem: TMenuItem;
    HelpMenu: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    ViewPrjMngrItem: TMenuItem;
    ToolsEditorOptionsItem: TMenuItem;
    raProject: TRegAuto;
    raPreferencies: TRegAuto;
    RunUnitItem: TMenuItem;
    RunProjectItem: TMenuItem;
    RAHLEdPropDlg1: TRAHLEdPropDlg;
    ViewFormItem: TMenuItem;
    ViewUnitItem: TMenuItem;
    InstallPackagesItem: TMenuItem;
    procedure FileOpenItemClick(Sender: TObject);
    procedure FileCloseItemClick(Sender: TObject);
    procedure FileSaveItemClick(Sender: TObject);
    procedure FileSaveAllItemClick(Sender: TObject);
    procedure FileSaveAsItemClick(Sender: TObject);
    procedure FileCloseAllItemClick(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure ViewAlignItemClick(Sender: TObject);
    procedure ViewObjInspItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ViewToggleFormUnitItemClick(Sender: TObject);
    procedure EditDeleteItemClick(Sender: TObject);
    procedure ViewPaletteItemClick(Sender: TObject);
    procedure ViewsMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure bRunFormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileNewFormItemClick(Sender: TObject);
    procedure bRunReportPreviewClick(Sender: TObject);
    procedure FileNewItemClick(Sender: TObject);
    procedure EditCopyItemClick(Sender: TObject);
    procedure EditPasteItemClick(Sender: TObject);
    procedure ViewPrjMngrItemClick(Sender: TObject);
    procedure ToolsEditorOptionsItemClick(Sender: TObject);
    procedure ToolsMenuClick(Sender: TObject);
    procedure PrjViewSourceItemClick(Sender: TObject);
    procedure RunUnitItemClick(Sender: TObject);
    procedure RunProjectItemClick(Sender: TObject);
    procedure ViewUnitItemClick(Sender: TObject);
    procedure ViewFormItemClick(Sender: TObject);
    procedure InstallPackagesItemClick(Sender: TObject);
  private
    { Private declarations }
    Palette: TControl;
    RAAppBuilder: TRAAppBuilder;
    Capt, Capt1: string;
    Selection: TComponentList;
    procedure FileCloseQuery(Sender: TObject);
    procedure WindowActivate(Sender: TObject);
    procedure DesignerActivate(Sender: TObject);
    procedure DesignerDestroy(Sender: TObject);
    procedure DesignerChange(Sender: TObject);
    procedure DesignerSelectionChange(Sender: TObject);
    procedure DesignerReaderError(Sender: TObject; Reader: TReader;
      const Message: string; var Handled: Boolean);
    procedure EditorLoadFile(Sender: TRAFDEditWindow; var FileName: TFileName;
      var FullFileName: TFileName; var Source: string; var Done: Boolean);
    procedure EditorSaveFile(Sender: TFileEditor; var FileName: TFileName;
      Text: string; var Done: Boolean);
    procedure EditorSaveFileAs(Sender: TFileEditor; var FileName: TFileName;
      Text: string; var Done: Boolean);
    procedure WMGetMinMaxInfo(var M : TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure EditorGetProperties(Sender: TFileEditor);
    procedure EditorChange(Sender: TObject);
    procedure EditorDestroy(Sender: TObject);
    procedure LoadLocale;
    procedure LoadPackages;
    procedure LoadExperts;
    procedure UpdateExpertMenu;
    procedure ExpertClick(Sender: TObject);
    procedure ProjectOpened(Sender: TObject);
    procedure ProjectClosed(Sender: TObject);
    procedure UpdateAllEditorsSettigns;
    procedure EnableItem(Item: TMenuItem; const Enabled: Boolean);
  private
    FRunner: TRAFDRunner;
    procedure Run(const What: TWhatRun);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure OpenFile(const FileName: TFileName);
    function FindFileEditor(const FileName: TFileName): TFileEditor;
    procedure NewEditWindow;
    procedure CheckActiveEditWindow;
    procedure UpdateState;
    procedure NewDesigner(const dfmFileName: TFileName; FileEditor: TFileEditor);
    procedure WriteProjectDesktop;
    procedure RestoreProjectDesktop;
    procedure RestoreDesktop(raProject: TRegAuto);
    procedure WriteDesktop(raProject: TRegAuto);
    procedure WritePackages;
    procedure ShowDesigners;
    procedure HideDesigners;
    function FormFileEditor: TFileEditor;
  end;

  TAppBuilder = class(TRAFDAppBuilder);

  TRAAppBuilder = class(TComponent)
  private
    FOnPackagesLoaded: TNotifyEvent;
    FRunning: Boolean;
    FIniFile: string;
    FCaption: string;
    procedure Done;
    procedure SetBaseRegistryKey(Value: string);
    function GetBaseRegistryKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateAppBuilderWindow;
    procedure Execute;
    property OnPackagesLoaded: TNotifyEvent read FOnPackagesLoaded write FOnPackagesLoaded;
    property BaseRegistryKey: string read GetBaseRegistryKey write SetBaseRegistryKey;
    property IniFile: string read FIniFile write FIniFile;
    property Caption: string read FCaption write FCaption;
  end;

var
  AppBuilder: TAppBuilder;

  LastActiveWindow: TForm;

//  Debug: Boolean = True;
  RAFDDebug: Boolean = False;

implementation

uses RAUtils, iMTracer, RAFD, RAFDIDE, RAFDDesktop,
  RAFDAlignPalette, RAFDPropertyInspector, RAFDPalette,
  RAFDProjectManager, RAFDNewItem, RAFDViewDialog, RAFDPakListDlg,
  RAFDReadErrorDlg;

{$R *.DFM}


function FormHasQuickRep(Form: TComponent): Boolean;
var
  i: Integer;
begin
  for i := 0 to Form.ComponentCount - 1 do
    if (Form.Components[i].ClassName = 'TQuickRep') then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

constructor TRAFDAppBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'AppBuilder';
  DefaultIcon := Icon;
  raPreferencies.IniFile := ChangeFileExt(Application.ExeName, '.ini');
  RAFDPalette.Create;
  Palette := ComponentPalette.GetControl;
  Palette.Parent := pnlToolbar;
  Palette.Visible := False;
  ViewPaletteItemClick(nil);
  RAFDProjectManager.Create;
  ProjectManager.OnProjectOpen := ProjectOpened;
  ProjectManager.OnProjectClose := ProjectClosed;
  FRunner := TRAFDRunner.Create(Self);
end;

destructor TRAFDAppBuilder.Destroy;
var
  i: integer;
  FD: TRAFormDesigner;
begin
  RAFDPalette.Free;
  RAFDProjectManager.Free;
  RAFDPropertyInspector.Free;
  RAFDAlignPalette.Free;
  Palette := nil;
  for i := 0 to DesignerList.Count - 1 do    { Iterate }
  begin
    FD := TRAFormDesigner(DesignerList[i]);
  	FD.OnActivate := nil;
  	FD.OnFormActivate := nil;
  	FD.OnDestroy := nil;
  	FD.OnChange := nil;
  	FD.OnSelectionChange := nil;
   // FD.Free;
  end;    { for }
  AppBuilder := nil;
  if Assigned(RAAppBuilder) then RAAppBuilder.Done;
  Selection.Free;
  inherited Destroy;
end;

procedure TRAFDAppBuilder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IDECloseDesignWindows;
  ProjectManager.CloseProject;
  WriteDesktop(raDesktop);
  WritePackages;
  if Assigned(ActiveEditWindow) then
    ActiveEditWindow.Close;
  IDETerminateExperts;
end;

procedure TRAFDAppBuilder.Execute;
begin
  Capt := Caption;
  Capt1 := Capt;
  raDesktop.IniFile := ChangeFileExt(raPreferencies.IniFile, '.dsk');

  LoadLocale;
  RAFDPropertyInspector.Create;
  RAFDAlignPalette.Create;
  FDDesktop.RestoreButtons(raPreferencies, 'SpeedBar', SpeedPanel);
  RestoreDesktop(raDesktop);
  LoadExperts;
  LoadPackages;
  UpdateExpertMenu;

  UpdateState;
  BringToFront;
end;

procedure TRAFDAppBuilder.RestoreProjectDesktop;
begin
  raProject.IniFile := ChangeFileExt(ProjectManager.GetProjectName, '.dsk');
  RestoreDesktop(raProject);
end;

procedure TRAFDAppBuilder.WriteProjectDesktop;
begin
  raProject.IniFile := ChangeFileExt(ProjectManager.GetProjectName, '.dsk');
  WriteDesktop(raProject);
end;

procedure TRAFDAppBuilder.WriteDesktop(raProject: TRegAuto);
var
  i: Integer;
begin
  { self }
  FDDesktop.WriteWindowPos(raProject, 'Main Window', Self);
  raProject.WriteBool('Main Window', 'PaletteVisible', Palette.Visible);
  raProject.WriteInteger('Main Window', 'SplitPos', Splitter1.Left);

  { others }
  ProjectManager.WriteDesktop(raProject);
  AlignPalette.WriteDesktop(raProject);
  PropertyInspector.WriteDesktop(raProject);
  if raProject <> raDesktop then
  begin
    raProject.WriteInteger('Modules', 'EditWindowCount', EditWindowList.Count);
    for i := 0 to EditWindowList.Count - 1 do
      TEditWindow(EditWindowList[i]).WriteDesktop(raProject);
  end;
end;

procedure TRAFDAppBuilder.RestoreDesktop(raProject: TRegAuto);
var
  i: Integer;
begin
  { self }
  FDDesktop.RestoreWindowPos(raProject, 'Main Window', Self);
  Palette.Visible := not raProject.ReadBool('Main Window', 'PaletteVisible',
    Palette.Visible);
  ViewPaletteItemClick(nil);
  Palette.Left := raProject.ReadInteger('Main Window', 'SplitPos', Splitter1.Left) + 4;
  Palette.Width := pnlToolbar.Width - Palette.Left;
  SpeedPanel.Width := Palette.Left - 4;

  { others }
  ProjectManager.RestoreDesktop(raProject);
  AlignPalette.RestoreDesktop(raProject);
  PropertyInspector.RestoreDesktop(raProject);
  if raProject <> raDesktop then
  begin
    for i := 0 to raProject.ReadInteger('Modules', 'EditWindowCount', 0) - 1 do
    begin
      NewEditWindow;
      ActiveEditWindow.RestoreDesktop(raProject);
      if ActiveEditWindow.EditorCount = 0 then
        ActiveEditWindow.Close;
    end;
  end;
  BringToFront;
  if ActiveEditWindow <> nil then
    ActiveEditWindow.BringToFront;
end;

function TRAFDAppBuilder.FormFileEditor: TFileEditor;
begin
  if LastActiveWindow = nil then
    Result := nil
  else if LastActiveWindow is TRAFDForm then
    Result := (LastActiveWindow as TRAFDForm).RAFormDesigner.FileEditor
  else
    Result := ActiveFileEditor;
end;

procedure TRAFDAppBuilder.FileOpenItemClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(OpenDialog1.FileName);
end;

procedure TRAFDAppBuilder.NewEditWindow;
begin
  ActiveEditWindow := TEditWindow.Create(Application);
  ActiveEditWindow.OnLoadFile := EditorLoadFile;
  ActiveEditWindow.OnSaveFile := EditorSaveFile;
  ActiveEditWindow.OnSaveFileAs := EditorSaveFileAs;
  ActiveEditWindow.OnChangeStatus := EditorChange;
  ActiveEditWindow.OnFileCloseQuery := FileCloseQuery;
  ActiveEditWindow.OnActivate := WindowActivate;
  ActiveEditWindow.OnDestroy := EditorDestroy;
  ActiveEditWindow.OnGetProperties := EditorGetProperties;
end;    { NewEditWindow }

procedure TRAFDAppBuilder.CheckActiveEditWindow;
begin
	if ActiveEditWindow = nil then
		NewEditWindow;
end;    { CheckActiveEditWindow }

procedure TRAFDAppBuilder.OpenFile(const FileName: TFileName);
var
  dfmFileName: TFileName;
  Cancel: Boolean;
  FileEditor: TFileEditor;
begin
  Application.BringToFront;
  FileEditor := FindFileEditor(FileName);
  if FileEditor <> nil then
  begin
    FileEditor.ToggleToFront;
    Exit;
  end;

  if Assigned(RAToolServices) then
  begin
    Cancel := False;
    RAToolServices.FileNotification(fnFileOpening, FileName, Cancel);
    if Cancel then Exit;
  end;

  if Cmp(ExtractFileExt(FileName), '.dpr') then
    ProjectManager.OpenProject(FileName)
  else
  begin
  	CheckActiveEditWindow;
  	if not ActiveEditWindow.OpenFile(FileName, False) then
  		Error('Can''t open file');
    Application.BringToFront;
    ActiveFileEditor.ToggleToFront;
  	if Cmp(ExtractFileExt(FileName), '.pas') then
  	begin
  		dfmFileName := ChangeFileExt(FileName, '.dfm');
  		if FileExists(dfmFileName) then
        NewDesigner(dfmFileName, ActiveFileEditor);
      LastActiveWindow := ActiveEditWindow;
  	end;
    if Assigned(RAToolServices) then
      RAToolServices.FileNotification(fnFileOpened, FileName, Cancel);
  end;
  UpdateState;
end;

procedure TRAFDAppBuilder.ProjectOpened(Sender: TObject);
begin
  EnableItem(ViewPrjMngrItem, True);
end;

procedure TRAFDAppBuilder.ProjectClosed(Sender: TObject);
begin
  EnableItem(ViewPrjMngrItem, False);
end;

procedure TRAFDAppBuilder.FileCloseQuery(Sender: TObject);
var
  Cancel: Boolean;
begin
  if (Sender as TFileEditor).IsProjectFile then
  begin
    ProjectManager.SourceClosing;
    Exit;
  end;
  if (Sender as TFileEditor).Modified or
     (((Sender as TFileEditor).Designer <> nil) and
      ((Sender as TFileEditor).Designer as TRAFormDesigner).Changed) then
    case MessageDlg(Format(ResStr(deResBase + deFileChanged, ''),
       [ExtractFileName((Sender as TFileEditor).FileName)]),
       mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if not (Sender as TFileEditor).Save then Abort;
      mrNo:
       { nothing } ;
      mrCancel:
        Abort;
    end;    { case }
  if Assigned(RAToolServices) then
  begin
    Cancel := False;
   { RAToolServices.FileNotification(fnFileClosing,
      (Sender as TFileEditor).FileName, Cancel); }
    if Cancel then Abort;
  end;
end;

procedure TRAFDAppBuilder.FileNewFormItemClick(Sender: TObject);
var
  FileName: TFileName;
begin
  CheckActiveEditWindow;
  FileName := ActiveEditWindow.FileNew;
	NewDesigner('', ActiveFileEditor);
end;

procedure TRAFDAppBuilder.NewDesigner(const dfmFileName: TFileName;
  FileEditor: TFileEditor);
var
  FD: TRAFormDesigner;
begin
  try
    FD := TRAFormDesigner.Create(FileEditor);
    FD.OnActivate := DesignerActivate;
    FD.OnFormActivate := WindowActivate;
    FD.OnDestroy := DesignerDestroy;
    FD.OnChange := DesignerChange;
    FD.OnSelectionChange := DesignerSelectionChange;
    FD.OnReaderError := DesignerReaderError;
    if dfmFileName <> '' then
      FD.LoadFromFile(dfmFileName)
    else
      FD.NewForm;
    UpdateState;
    FD.Form.Show;
  except
    on E: Exception do
    begin
      FileEditor.Designer := nil;
      E.Message := Format(ResStr(deResBase + deErrorCreatingForm,
        'Error creating form'#13'%s'), [E.Message]);
      raise;
    end;
  end;
end;    { NewDesigner }

procedure TRAFDAppBuilder.EditorLoadFile(Sender: TRAFDEditWindow;
  var FileName: TFileName; var FullFileName: TFileName;
  var Source: string; var Done: Boolean);
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  if FileName = '' then
  begin
   { gen new file name }
    FileName := 'Unit1.pas';
    FullFileName := FileName;
    Done := True;
    ResSaveToString(hInstance, 'pas', 'unit1', Source);
  end
  else if Cmp(ExtractFileExt(FileName), '.dfm') then
  begin
    FullFileName := AddPath(FileName, ExePath);
    FileStream := TFileStream.Create(FullFileName, fmOpenRead);
    StringStream := TStringStream.Create('');
    try
      ObjectResourceToText(FileStream, StringStream);
      Source := StringStream.DataString;
      Done := True;
    finally { wrap up }
      FileStream.Free;
      StringStream.Free;
    end;    { try/finally }
  end
  else
  begin
    FullFileName := AddPath(FileName, ExePath);
    if FileExists(FullFileName) then
    begin
      Source := LoadTextFile(FullFileName);
      Done := True;
    end;
  end;
  UpdateState;
end;

procedure TRAFDAppBuilder.EditorSaveFile(Sender: TFileEditor;
  var FileName: TFileName; Text: string; var Done: Boolean);
var
  FileStream: TFileStream;
  StringStream: TStringStream;
  FN: TFileName;
  BakFileName: TFileName;
begin
  BakFileName := ChangeFileExt(FileName, '.bak');
  DeleteFile(BakFileName);
  RenameFile(FileName, BakFileName);
  FN := GenTempFileName('');
  try
    if Cmp(ExtractFileExt(FileName), '.dfm') then
    begin
      FileStream := TFileStream.Create(FN, fmOpenWrite or fmCreate);
      StringStream := TStringStream.Create(Text);
      try
        ObjectTextToResource(StringStream, FileStream);
      finally { wrap up }
        FileStream.Free;
        StringStream.Free;
      end;    { try/finally }
    end
    else
      SaveTextFile(FN, Text);
		RenameFile(FN, FileName);
		DeleteFile(BakFileName);
    Done := True;
  except
    DeleteFile(FileName);
    DeleteFile(FN);
    RenameFile(BakFileName, FileName);
    raise;
  end;
  if Cmp(ExtractFileExt(FileName), '.dpr') then
    ProjectManager.UpdateFromSource;
  UpdateState;
end;

procedure TRAFDAppBuilder.EditorSaveFileAs(Sender: TFileEditor;
  var FileName: TFileName; Text: string; var Done: Boolean);
begin
  SaveDialog1.FileName := FileName;
  if SaveDialog1.Execute then
  begin
    FileName := SaveDialog1.FileName;
    if Cmp(ExtractFileExt(FileName), '.pas') then
      Sender.RenameUnit(ChangeFileExt(ExtractFileName(FileName), ''));
    SaveTextFile(FileName, Text);
    Done := True;
  end;
  OpenDialog1.FileName := SaveDialog1.FileName;
end;

procedure TRAFDAppBuilder.EditorChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TRAFDAppBuilder.EditorDestroy(Sender: TObject);
begin
 { with Sender as TFileEditor do
    if EditWindow.EditorCount > 1 then
      LastActiveWindow := EditWindow
    else }
      LastActiveWindow := nil;
  UpdateState;
end;

procedure TRAFDAppBuilder.FileCloseItemClick(Sender: TObject);
begin
  if FormFileEditor <> nil then
  begin
    FileCloseQuery(FormFileEditor);
    FormFileEditor.Check4Save;
    FormFileEditor.Close;
  end;
end;

procedure TRAFDAppBuilder.FileCloseAllItemClick(Sender: TObject);
begin
  ProjectManager.CloseProject;
  while EditWindowList.Count > 0 do
    TEditWindow(EditWindowList[0]).Close;
  UpdateState;
end;

procedure TRAFDAppBuilder.FileSaveItemClick(Sender: TObject);
begin
  if FormFileEditor <> nil then
  begin
    FormFileEditor.Save;
    UpdateState;
  end;
end;

procedure TRAFDAppBuilder.FileSaveAllItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to FileEditorList.Count - 1 do
    if FileEditorList[i].Modified2 then
      FileEditorList[i].Save;
  UpdateState;
end;

procedure TRAFDAppBuilder.FileSaveAsItemClick(Sender: TObject);
begin
  if FormFileEditor <> nil then
  begin
    FormFileEditor.SaveAs;
    UpdateState;
  end;
end;

procedure TRAFDAppBuilder.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TRAFDAppBuilder.EnableItem(Item: TMenuItem; const Enabled: Boolean);
var
  i: Integer;
begin
  Item.Enabled := Enabled;
  for i := 0 to SpeedPanel.ControlCount - 1 do
    if (SpeedPanel.Controls[i] is TSpeedButton) and
       (@(SpeedPanel.Controls[i] as TSpeedButton).OnClick = @Item.OnClick) then
      (SpeedPanel.Controls[i] as TSpeedButton).Enabled := Enabled;
end;

procedure TRAFDAppBuilder.UpdateState;
var
  i: integer;
  M: Boolean;
begin
  M := False;
  for i := 0 to EditWindowList.Count - 1 do    { Iterate }
    if TRAFDEditWindow(EditWindowList[i]).Changed then
		begin
			M := True;
			Break;
		end;

  EnableItem(FileSaveAllItem, M);
  EnableItem(FileSaveAsItem, FormFileEditor <> nil);
  EnableItem(FileCloseItem, FormFileEditor <> nil);
	EnableItem(FileSaveItem, (FormFileEditor <> nil) and
		(FormFileEditor.Modified or
    ((FormFileEditor.Designer <> nil) and
     (FormFileEditor.Designer as TRAFormDesigner).Changed)));
  EnableItem(RunUnitItem, FormFileEditor <> nil);
  EnableItem(RunProjectItem, ProjectManager.IsProjectOpen);

  EnableItem(FileCloseAllItem,(EditWindowList.Count > 0) or
    ProjectManager.IsProjectOpen);
  EnableItem(PrjViewSourceItem, ProjectManager.IsProjectOpen);
  EnableItem(ViewToggleFormUnitItem, (FormFileEditor <> nil) and
    (FormFileEditor.Designer <> nil));
  EnableItem(RunFormItem, ViewToggleFormUnitItem.Enabled);
  EnableItem(RunReportPreviewItem, (FormFileEditor <> nil) and
    (FormFileEditor.Designer <> nil) and
    (FormFileEditor.Designer.Form <> nil) and
    FormHasQuickRep(FormFileEditor.Designer.Form));
  EnableItem(ViewUnitItem, ProjectManager.IsProjectOpen);
  EnableItem(ViewFormItem, ProjectManager.IsProjectOpen);

  if ProjectManager.ProjectTitle > '' then
    Capt := Capt1 + ' - ' + ProjectManager.ProjectTitle
  else
    Capt := Capt1;
	Caption := Capt;
end;    { UpdateState }

procedure TRAFDAppBuilder.DesignerActivate(Sender: TObject);
begin
  DesignerSelectionChange(Sender);
end;

procedure TRAFDAppBuilder.DesignerChange(Sender: TObject);
begin
  UpdateState;
  Selection.Free;
  Selection := TComponentList.Create;
  (Sender as TRAFormDesigner).GetSelections(Selection);
  PropertyInspector.UpdatePropValues(Sender as TRAFormDesigner,
    (Sender as TRAFormDesigner).Form, Selection);
end;    { DesignerChange }

procedure TRAFDAppBuilder.DesignerSelectionChange(Sender: TObject);
begin
  UpdateState;
  Selection.Free;
  if Sender = nil then
    Selection := nil
  else
  begin
    Selection := TComponentList.Create;
    (Sender as TRAFormDesigner).GetSelections(Selection);
    PropertyInspector.UpdatePropList(Sender as TRAFormDesigner,
      (Sender as TRAFormDesigner).Form, Selection);
  end;
end;    { DesignerSelectionChange }

procedure TRAFDAppBuilder.DesignerDestroy(Sender: TObject);
begin
  LastActiveWindow := ActiveEditWindow;
  UpdateState;
  PropertyInspector.UpdatePropList(nil, nil, nil);
end;    { DesignerDestroy }

procedure TRAFDAppBuilder.DesignerReaderError(Sender: TObject; Reader: TReader;
  const Message: string; var Handled: Boolean);
begin
  case ReadErrorDlg(Message) of
    mrOk:
      Handled := True;
    mrCancel:
      Handled := False;
    mrAll:
      begin
        Handled := True;
        (Reader as TRAFDReader).IgnoreAll := True;
      end;
  end;    { case }
  if Handled then
    (Sender as TRAFormDesigner).Modified;
end;

procedure TRAFDAppBuilder.WindowActivate(Sender: TObject);
begin
  LastActiveWindow := Sender as TForm
end;    { WindowActivate }

procedure TRAFDAppBuilder.ViewObjInspItemClick(Sender: TObject);
begin
	PropertyInspector.Show;
  DesignerSelectionChange(ActiveDesigner);
end;

procedure TRAFDAppBuilder.ViewAlignItemClick(Sender: TObject);
begin
  AlignPalette.Show;
end;

procedure TRAFDAppBuilder.ViewToggleFormUnitItemClick(Sender: TObject);
begin
  if (FormFileEditor <> nil) and Assigned(FormFileEditor.Designer) then
  begin
    if LastActiveWindow is TRAFDEditWindow then
      (ActiveFileEditor.Designer as TRAFormDesigner).ToggleToFront
    else if LastActiveWindow is TRAFDForm then
      (LastActiveWindow as TRAFDForm).RAFormDesigner.FileEditor.ToggleToFront;
  end;
end;

procedure TRAFDAppBuilder.EditDeleteItemClick(Sender: TObject);
begin
  if Assigned(ActiveDesigner) and (Screen.ActiveForm = ActiveDesigner.Form) then
    ActiveDesigner.DeleteSelection;
end;

procedure TRAFDAppBuilder.EditCopyItemClick(Sender: TObject);
begin
  if (ActiveDesigner <> nil) and (Screen.ActiveForm = ActiveDesigner.Form) then
    ActiveDesigner.CopySelection;
end;

procedure TRAFDAppBuilder.EditPasteItemClick(Sender: TObject);
begin
  if Assigned(ActiveDesigner) and (Screen.ActiveForm = ActiveDesigner.Form) then
    ActiveDesigner.PasteSelection;
end;

procedure TRAFDAppBuilder.ViewPaletteItemClick(Sender: TObject);
var
  H: Integer;
begin
  if SpeedPanel.ControlCount > 0 then
    H := SpeedPanel.Controls[SpeedPanel.ControlCount - 1].BoundsRect.Bottom +
      Pixels(Self, 1)
  else
    H := Pixels(Self, 4);
  if Palette.Visible then
  begin
    Palette.Visible := False;
    pnlToolbar.Height := H;
  end
  else
  begin
    Palette.Left := Splitter1.Left + 4;
    Palette.Visible := True;
    pnlToolbar.Height := Max(H, Pixels(Self, 56));
    Palette.Top := 2;
  end;
  Splitter1.Visible := Palette.Visible;
  if not RAFDDebug then
    ClientHeight := pnlToolbar.BoundsRect.Bottom;
  Resize;
end;

procedure TRAFDAppBuilder.ViewPrjMngrItemClick(Sender: TObject);
begin
  if Assigned(ProjectManager) then
    ProjectManager.Show;
end;

procedure TRAFDAppBuilder.ViewsMenuClick(Sender: TObject);
begin
  ViewPaletteItem.Checked := Palette.Visible;
end;

procedure TRAFDAppBuilder.FormShow(Sender: TObject);
begin
  if not RAFDDebug then
    ClientHeight := pnlToolbar.BoundsRect.Bottom;
end;

procedure TRAFDAppBuilder.FormResize(Sender: TObject);
begin
  if Palette = nil then Exit;
  Palette.Left := Splitter1.Left + 4;
  Palette.Width := pnlToolbar.Width - Palette.Left;
end;

procedure TRAFDAppBuilder.Splitter1Moved(Sender: TObject);
begin
  Palette.Left := Splitter1.Left + 4;
  Palette.Width := pnlToolbar.Width - Palette.Left;
end;

procedure TRAFDAppBuilder.WMGetMinMaxInfo(var M : TWMGetMinMaxInfo);
begin
  inherited;
  if RAFDDebug then Exit;
  if Visible then
  begin
    M.MinMaxInfo^.ptMinTrackSize.Y := Height - ClientHeight + pnlToolbar.BoundsRect.Bottom;
    M.MinMaxInfo^.ptMaxTrackSize.Y := M.MinMaxInfo^.ptMinTrackSize.Y;
    M.MinMaxInfo^.ptMinTrackSize.X := 80;
  end;
end;

function TRAFDAppBuilder.FindFileEditor(const FileName: TFileName): TFileEditor;
var
  i: Integer;
begin
  for i := 0 to FileEditorList.Count - 1 do
  begin
    Result := FileEditorList[i];
    if Cmp(Result.FileName, FileName) or
       Cmp(ExtractFileName(Result.FileName), FileName) or
       Cmp(ChangeFileExt(ExtractFileName(Result.FileName), ''), FileName) then
      Exit;
  end;
  Result := nil;
end;

procedure TRAFDAppBuilder.ShowDesigners;
var
  i: Integer;
begin
  for i := 0 to DesignerList.Count - 1 do
    TRAFormDesigner(DesignerList[i]).ShowForm;
  DesignerSelectionChange(ActiveDesigner);
end;

procedure TRAFDAppBuilder.HideDesigners;
var
  i: Integer;
begin
  LastActiveWindow := ActiveEditWindow;
  PropertyInspector.UpdatePropList(nil, nil, nil);
  for i := DesignerList.Count - 1 downto 0 do
    TRAFormDesigner(DesignerList[i]).HideForm;
end;

procedure TRAFDAppBuilder.Run(const What: TWhatRun);
var
  Capt: string;
  FileEditor: TFileEditor;
begin
  FileEditor := FormFileEditor;

  if (FileEditor = nil) or ((What in [ruForm, ruReportPreview]) and
     (FileEditor.Designer = nil)) then Exit;
  Capt := Caption;
  Caption := Capt + ' - [Running]';
  try
    HideDesigners;
    try
      case What of
        ruProject:
          if ProjectManager.IsProjectOpen then
            FRunner.RunUnit(ProjectManager.GetProjectName);
        ruUnit:
          FRunner.RunUnit(FileEditor.FileName);
        ruForm:
          FRunner.RunFormModal(FileEditor.FileName);
        ruReportPreview:
          FRunner.RunReportPreview(FileEditor.FileName);
      end;
    finally { wrap up }
      Caption := Capt;
      ShowDesigners;
    end;    { try/finally }
  except
    on E: ERAI2Error do
    begin
      if E.ErrUnitName <> '' then
      begin
        OpenFile(E.ErrUnitName + '.pas');
        ActiveFileEditor.ShowErrorPoint(E.ErrPos);
      end
      else
        raise E;
    end;
  end;
end;

procedure TRAFDAppBuilder.RunUnitItemClick(Sender: TObject);
begin
  Run(ruUnit);
end;

procedure TRAFDAppBuilder.bRunFormClick(Sender: TObject);
begin
  Run(ruForm);
end;

procedure TRAFDAppBuilder.bRunReportPreviewClick(Sender: TObject);
begin
  Run(ruReportPreview);
end;

procedure TRAFDAppBuilder.RunProjectItemClick(Sender: TObject);
begin
  Run(ruProject);
end;

procedure TRAFDAppBuilder.LoadLocale;
begin
  LocaleMenuItem(FileMenu        , deFileMenu        , deNoHint);
  LocaleMenuItem(FileNewItem     , deFileNewItem     , deNoHint);
  LocaleMenuItem(FileNewFormItem , deFileNewFormItem , deNoHint);
  LocaleMenuItem(FileOpenItem    , deFileOpenItem    , deNoHint);
  LocaleMenuItem(FileSaveItem    , deFileSaveItem    , deNoHint);
  LocaleMenuItem(FileSaveAsItem  , deFileSaveAsItem  , deNoHint);
  LocaleMenuItem(FileSaveAllItem , deFileSaveAllItem , deNoHint);
  LocaleMenuItem(FileCloseItem   , deFileCloseItem   , deNoHint);
  LocaleMenuItem(FileCloseAllItem, deFileCloseAllItem, deNoHint);
  LocaleMenuItem(FileExitItem    , deFileExitItem    , deNoHint);
  LocaleMenuItem(EditMenu        , deEditMenu        , deNoHint);
  LocaleMenuItem(EditCopyItem    , deEditCopyItem    , deNoHint);
  LocaleMenuItem(EditPasteItem   , deEditPasteItem   , deNoHint);
  LocaleMenuItem(EditDeleteItem  , deEditDeleteItem  , deNoHint);
  LocaleMenuItem(ViewsMenu       , deViewsMenu       , deNoHint);
  LocaleMenuItem(ViewPrjMngrItem , deViewPrjMngrItem , deNoHint);
  LocaleMenuItem(ViewObjInspItem , deViewObjInspItem , deNoHint);
  LocaleMenuItem(ViewAlignItem   , deViewAlignItem   , deNoHint);
  LocaleMenuItem(ViewPaletteItem , deViewPaletteItem , deNoHint);
  LocaleMenuItem(ViewToggleFormUnitItem , deViewToggleFormUnitItem, deNoHint);
  LocaleMenuItem(ViewWindowListItem     , deViewWindowListItem    , deNoHint);
  LocaleMenuItem(ViewUnitItem           , deViewUnitItem          , deNoHint);
  LocaleMenuItem(ViewFormItem           , deViewFormItem          , deNoHint);
  LocaleMenuItem(RunMenu                , deRunMenu               , deNoHint);
  LocaleMenuItem(RunProjectItem         , deRunProjectItem        , deRunProjectItemHint);
  LocaleMenuItem(RunUnitItem            , deRunUnitItem           , deRunUnitItemHint);
  LocaleMenuItem(RunFormItem            , deRunFormItem           , deRunFormItemHint);
  LocaleMenuItem(RunReportPreviewItem   , deRunReportPreviewItem  , deRunReportPreviewItemHint);
  LocaleMenuItem(ComponentMenu          , deComponentMenu         , deNoHint);
  LocaleMenuItem(InstallPackagesItem    , deInstallPackagesItem   , deNoHint);
  LocaleMenuItem(SearchMenu             , deSearchMenu            , deNoHint);
  LocaleMenuItem(ProjectMenu            , deProjectMenu           , deNoHint);
  LocaleMenuItem(PrjViewSourceItem      , dePrjViewSourceItem     , deNoHint);
  LocaleMenuItem(ToolsMenu              , deToolsMenu             , deNoHint);
  LocaleMenuItem(ToolsOptionsItem       , deToolsOptionsItem      , deNoHint);
  LocaleMenuItem(ToolsEditorOptionsItem , deToolsEditorOptionsItem, deNoHint);
  LocaleMenuItem(HelpMenu               , deHelpMenu              , deNoHint);
end;    { LoadLocale }

procedure TRAFDAppBuilder.FileNewItemClick(Sender: TObject);
begin
  RAFDNewItem.Show;
end;


{************************** Dynamic package loading **************************}

procedure TRAFDAppBuilder.LoadPackages;
var
  i: integer;
  Item: string;
  FN: TFileName;
  Load: Boolean;
  Description: string;
begin
  ComponentPalette.BeginUpdate;
  try
    for i := 0 to raPreferencies.ReadInteger('Packages', 'Count', 0) - 1 do
    begin
      Item := raPreferencies.ReadString('Packages', IntToStr(i), '');
      FN := SubStr(Item, 0, ',');
      Load := Trim(SubStr(Item, 1, ',')) <> '0';
      Description := Trim(SubStr(Item, 2, ','));
      if Trim(FN) <> '' then
        try
          DesignPackageList.Add(FN, Load, False, Description);
          Application.ProcessMessages;
        except
          on E: Exception do
            Application.ShowException(E);
        end;
    end;
  finally { wrap up }
    ActiveModule := hInstance;
    ComponentPalette.EndUpdate;
  end;    { try/finally }
  if Assigned(RAAppBuilder) and Assigned(RAAppBuilder.FOnPackagesLoaded) then
    RAAppBuilder.FOnPackagesLoaded(RAAppBuilder);
  ComponentPalette.UpdatePalette;
end;    { LoadPackages }

{ Write Insalled Packages List into Ini-file }
procedure TRAFDAppBuilder.WritePackages;
var
  i: integer;
  Item: string;
  Count: Integer;
begin
  Count := 0;
  for i := 0 to DesignPackageList.Count - 1 do
    if not DesignPackageList[i].IDEPackage then
    begin
      Item := DesignPackageList[i].FileName + ',' +
        IntToStr(Integer(DesignPackageList[i].Loaded)) + ',' +
        DesignPackageList[i].Description;
      raPreferencies.WriteString('Packages', IntToStr(Count), Item);
      Inc(Count);
    end;
  raPreferencies.WriteInteger('Packages', 'Count', Count);
end;    { WritePackages }

{########################## Dynamic package loading ##########################}

procedure TRAFDAppBuilder.LoadExperts;
var
  i: integer;
  Item: string;
begin
  for i := 0 to raPreferencies.ReadInteger('Experts', 'Count', 0) - 1 do
  begin
    Item := raPreferencies.ReadString('Experts', IntToStr(i), '');
    if Trim(Item) <> '' then
      try
        IDELoadExpert(Item);
        Application.ProcessMessages;
      except
        on E: Exception do
        begin
          E.Message := Format(ResStr(deResBase + deLoadExpertFail, ''),
            [Item, E.Message]);
          Application.ShowException(E);
        end;
      end;
  end;
end;    { LoadExperts }

procedure TRAFDAppBuilder.UpdateExpertMenu;
var
  Expert: TIExpert;
  ExpertList: TList;
  i: integer;
  MenuItem: TMenuItem;
begin
  ExpertList := RAFDIDE.GetExpertList;
  for i := 0 to ExpertList.Count - 1 do
  begin
    Expert := TExpertRec(ExpertList[i]).Expert;
    if Assigned(Expert) and (Expert.GetStyle = esStandard) then
    begin
      MenuItem := NewItem(Expert.GetMenuText, 0, False, True, ExpertClick, 0, '');
      MenuItem.Tag := Integer(Expert);
      HelpMenu.Add(MenuItem);
    end;
  end;    { for }
end;    { UpdateExpertMenu }

procedure TRAFDAppBuilder.ExpertClick(Sender: TObject);
begin
  TIExpert((Sender as TMenuItem).Tag).Execute;
end;    { ExpertClick }

procedure TRAFDAppBuilder.ToolsEditorOptionsItemClick(Sender: TObject);
begin
  if ActiveFileEditor <> nil then
  begin
    RAHLEdPropDlg1.RAHLEditor := ActiveFileEditor;
    if RAHLEdPropDlg1.Execute then
      UpdateAllEditorsSettigns;
  end;
end;

procedure TRAFDAppBuilder.UpdateAllEditorsSettigns;
var
  i: Integer;
begin
  for i := 0 to FileEditorList.Count - 1 do
  begin
    RAHLEdPropDlg1.RAHLEditor := FileEditorList[i];
    RAHLEdPropDlg1.LoadCurrentHighlighterColors;
    FileEditorList[i].Invalidate;
  end;
  UpdateState;
end;

procedure TRAFDAppBuilder.EditorGetProperties(Sender: TFileEditor);
begin
  RAHLEdPropDlg1.RAHLEditor := Sender;
  RAHLEdPropDlg1.LoadCurrentHighlighterColors;
end;

procedure TRAFDAppBuilder.ToolsMenuClick(Sender: TObject);
begin
  EnableItem(ToolsEditorOptionsItem, ActiveFileEditor <> nil);
end;

procedure TRAFDAppBuilder.PrjViewSourceItemClick(Sender: TObject);
begin
  CheckActiveEditWindow;
  ActiveEditWindow.OpenFile(ProjectManager.GetProjectName, False);
  ActiveFileEditor.ToggleToFront;
end;

procedure TRAFDAppBuilder.ViewUnitItemClick(Sender: TObject);
begin
  RAFDViewDialog.ViewUnits;
end;

procedure TRAFDAppBuilder.ViewFormItemClick(Sender: TObject);
begin
  RAFDViewDialog.ViewForms;
end;

procedure TRAFDAppBuilder.InstallPackagesItemClick(Sender: TObject);
begin
  RAFDPakListDlg.Show;
  WritePackages;
end;

{************************** TRAAppBuilder ******************************}
constructor TRAAppBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;    { Create }

destructor TRAAppBuilder.Destroy;
begin
  AppBuilder.Free;
  inherited Destroy;
end;    { Destroy }

procedure TRAAppBuilder.CreateAppBuilderWindow;
begin
  Application.CreateForm(TAppBuilder, AppBuilder);
  AppBuilder.RAAppBuilder := Self;
end;    { CreateAppBuilderWindow }

procedure TRAAppBuilder.Execute;
begin
  FRunning := True;
  if AppBuilder = nil then
    CreateAppBuilderWindow;
  AppBuilder.raPreferencies.IniFile := FIniFile;
  AppBuilder.Caption := FCaption;
  AppBuilder.Execute;
 // FMainForm.Show;
end;

procedure TRAAppBuilder.Done;
begin
  FRunning := False;
end;

procedure TRAAppBuilder.SetBaseRegistryKey(Value: string);
begin
  if Assigned(RAToolServices) then
    RAToolServices.SetBaseRegistryKey(Value);
end;

function TRAAppBuilder.GetBaseRegistryKey: string;
begin
  if Assigned(RAToolServices) then
    Result := RAToolServices.GetBaseRegistryKey
  else
    Result := DefaultRegistryKey;
end;



end.

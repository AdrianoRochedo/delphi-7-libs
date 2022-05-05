{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2000 R&A

       form        : TRAFDEditWindow
       description : text file editor

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls,
  StdCtrls, Dialogs, RARegAuto, Menus, Forms,
  {$IFDEF RA_D4H} ImgList, {$ENDIF RA_D4H}
  RAEditor, RAHLEditor, RAHint, IParser, TypInfo, EditIntf,
  {$IFDEF RA_D6H} DesignIntf, {$ELSE} DsgnIntf, {$ENDIF RA_D6H}
  RAFDIDE, RAFDCompat;

const

  WM_RESETFOCUS = wm_User + $100 + 1;

type

  TRAFDEditWindow = class;

  TFileEditor = class(TRAHLEditor)
  private
    FExecBegX, FExecBegY, FExecEndX, FExecEndY : integer;
    FStackBegX, FStackBegY, FStackEndX, FStackEndY : integer;
    FErrY : integer;
    FclErrorFC, FclErrorBC: TColor;
   // XX, YY : integer;
    FDesigner: TFormDesigner;
    FFileName: TFileName;
    Tab: TTabSheet;
    FEditWindow: TRAFDEditWindow;
    UnitName : string;
    Parser: TIParser;
    IsNewFile: Boolean;
    dfm: Boolean;
    FModuleInterface: TIRAModuleInterface;
    FIsProjectFile: Boolean;
    ParserText: string;
    function FindMethod(const Name: string): Integer;
    function FindMethodDecl(const Name: string): Integer;
    procedure FindFormDecl(var Line: Integer);
    procedure FindMethodSection(var Line: Integer);
    procedure FindUnitEnd(var Line: Integer);
    procedure FindToken(const Token: string);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure ChangeAttr(Line, ColBeg, ColEnd : integer); override;
    procedure GutterPaint(Canvas : TCanvas); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ShowPoint(PosBeg, PosEnd : integer; var BegX, BegY, EndX, EndY : integer);
    procedure TextModified(Pos : integer; Action : TModifiedAction; Text : string); override;
    procedure Changed; override;
    procedure StatusChanged; override;
    procedure ExtensionChanged;
    function GetReservedWord(const Token: string; var Reserved: Boolean)
      : Boolean; override;
    function UserReservedWords: boolean; override;
    procedure UpdateSettings;
    procedure SetParser(const S: string);
    procedure InitParser;
    procedure ReplaceToken(const NewToken: string);
  public
    constructor CreateF(AOwner : TComponent; ATab: TTabSheet;
      AFileName : TFileName; Source : string);
    destructor Destroy; override;
    function Save: Boolean;
    function SaveAs: Boolean;
    procedure Close;
    procedure Check4Save;
    function Modified2: Boolean;
    procedure ShowExecutionPoint(PosBeg, PosEnd : integer);
    procedure ShowCallStackPoint(PosBeg, PosEnd : integer);
    procedure ShowErrorPoint(const ErrPos : integer);
    procedure ShowBreakPoint(PosBeg, PosEnd : integer);
    function BPIndex(PBeg, PEnd : integer) : integer;
    procedure pmEditorPopup;
    procedure TogleBreakPoint;
    procedure ToggleToFront;
    property Designer: TFormDesigner read FDesigner write FDesigner;
    property FileName: TFileName read FFileName;
    property EditWindow: TRAFDEditWindow read FEditWindow;
  public { source modifiers for designer requests }
    procedure CreateComponent(Component: TComponent; const NewName: string);
    procedure DeleteComponent(Component: TComponent);
    procedure RenameComponent(Component: TComponent; const NewName: string);
    procedure RenameForm(const CurName, NewName: string);
    procedure RenameUnit(const NewName: string);
    procedure ShowMethod(const Name: string);
    function MethodExists(const Name: string): Boolean;
    procedure RenameMethod(const CurName, NewName: string);
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStrProc);
    function CreateMethod(const Name: string; TypeData: PTypeData): TMethod;
  private { TStrings property editor support }
    IsStrings: Boolean;
    StringsComponent: TPersistent;
    Strings: TStrings;
    FormFileEditor: TFileEditor;
    procedure WMResetFocus(var Message: TMessage); message WM_RESETFOCUS;
    procedure SavePropValue;
    procedure DeleteStrings(AComponent: TComponent);
  public { TStrings property editor support }
    procedure EditStrings(AInstance: TPersistent; AStrings: TSTrings;
      APropInfo: PPropInfo; ACurrentValue: TStrings);
    function ActivateStrings(AInstance: TPersistent; AStrings: TSTrings;
      APropInfo: PPropInfo): Boolean;
  public
    procedure SetModuleInterface(AModuleInterface: TIRAModuleInterface);
    property ModuleInterface: TIRAModuleInterface read FModuleInterface;
    property IsProjectFile: Boolean read FIsProjectFile write FIsProjectFile;
  end;

  TLoadFile = procedure (Sender: TRAFDEditWindow; var FileName: TFileName;
    var FullFileName: TFileName; var Text: string; var Done: Boolean) of object;
  TSaveFile = procedure (Sender: TFileEditor; var FileName: TFileName;
    Text: string; var Done: Boolean) of object;
  TGetProperties = procedure (Sender: TFileEditor) of object;

  TRAFDEditWindow = class(TForm)
    StatusBar: TStatusBar;
    EditorLocalMenu: TPopupMenu;
    ecClosePage: TMenuItem;
    ecOpenFileAtCursor: TMenuItem;
    OpenFileDlg: TOpenDialog;
    Images: TImageList;
    RAHint1: TRAHint;
    miTogleBreakPoint: TMenuItem;
    N1: TMenuItem;
    ecReadOnlyItem: TMenuItem;
    miAddWatch: TMenuItem;
    miEvaluateModify: TMenuItem;
    imBookmark: TImageList;
    lblFont: TLabel;
    Label1: TLabel;
    N2: TMenuItem;
    ecPropertiesItem: TMenuItem;
    procedure ecClosePageClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure ecOpenFileAtCursorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditorLocalMenuPopup(Sender: TObject);
    procedure miTogleBreakPointClick(Sender: TObject);
    procedure ecReadOnlyItemClick(Sender: TObject);
    procedure miEvaluateModifyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ecPropertiesItemClick(Sender: TObject);
  private
    TabControl: TPageControl;
    FOnChangeStatus: TNotifyEvent;
    FOnLoadFile: TLoadFile;
    FOnSaveFile: TSaveFile;
    FOnSaveFileAs: TSaveFile;
    FOnFileCloseQuery: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnGetProperties: TGetProperties;
    Closing: Boolean;
    function GetEditor(index : integer): TFileEditor;
    procedure UpdateStatus;
    function FileOpen(AFileName : TFileName) : boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMGetMinMaxInfo(var M : TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    function MakeTab(const UnitFullName: TFileName; const Source: string): TFileEditor;
    procedure ActivateNextEditWindow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ActiveEditor : TFileEditor;
    function OpenFile(const FileName : string; ShowOpenDialog : boolean) : boolean;
    function FileNew: TFileName;
    procedure NewFile(const FileName : TFileName);
    procedure DeleteAllErrorPoint;
    procedure ShowBreakPoint(index : integer);
    function EditorCount : integer;
    function Changed: Boolean;
    procedure WriteDesktop(raProject: TRegAuto);
    procedure RestoreDesktop(raProject: TRegAuto);
    property Editors[index : integer] : TFileEditor read GetEditor;
    property OnChangeStatus: TNotifyEvent read FOnChangeStatus write FOnChangeStatus;
    property OnLoadFile: TLoadFile read FOnLoadFile write FOnLoadFile;
    property OnSaveFile: TSaveFile read FOnSaveFile write FOnSaveFile;
    property OnSaveFileAs: TSaveFile read FOnSaveFileAs write FOnSaveFileAs;
    property OnFileCloseQuery: TNotifyEvent read FOnFileCloseQuery write FOnFileCloseQuery;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnGetProperties: TGetProperties read FOnGetProperties write FOnGetProperties;
  end;

  TFileEditorList = class(TList)
  private
    function GetItem(Index: Integer): TFileEditor;
  public
    property Items[Index: Integer]: TFileEditor read GetItem; default;
  end;

  TEditWindow = class(TRAFDEditWindow);

var
  ActiveFileEditor: TFileEditor;
  ActiveEditWindow: TRAFDEditWindow;

  EditWindowList: TList;
  FileEditorList: TFileEditorList;

implementation

uses RAUtils, iMTracer, RACnst, RAFD, RAFDDesktop, RAFDDesigner,
  RAFDAppBuilder, RAFDProjectManager;

{$R *.DFM}

const

  clDontChange = High(TColor);

  { image indexes }
  imExecute = 0;
  imError   = 1;
  imStack   = 2;
  imBreak   = 3;

type
  TTabControl = class(TPageControl);  

{ TFileEditorList }
function TFileEditorList.GetItem(Index: Integer): TFileEditor;
begin
  Result := TFileEditor(inherited Items[Index]);
end;

{************************ TRAFDEditWindow ***********************}
constructor TRAFDEditWindow.Create(AOwner: TComponent);
begin
  EditWindowList.Add(Self);
  ODS('TRAFDEditWindow.Create');
  inherited Create(AOwner);
  if EditWindowList.Count = 0 then
    Name := 'EditWindow'
  else
    Name := 'EditWindow' + IntToStr(EditWindowList.Count - 1);
  ActiveEditWindow := Self;
  TabControl := TTabControl.Create(Self);
  TabControl.Parent := Self;
  TabControl.Name := 'TabControl';
//  TabControl.Caption := 'TabControl';
  TabControl.OnChange := TabControlChange;
  TabControl.TabStop := False;
  TabControl.PopupMenu := EditorLocalMenu;
  FDDesktop.RestoreWindowPos(AppBuilder.raDesktop, 'EditWindow', Self);
end;    { Create }

destructor TRAFDEditWindow.Destroy;
begin
  ODS('TRAFDEditWindow.Destroy');
  ActivateNextEditWindow;
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited Destroy;
end;

procedure TRAFDEditWindow.ActivateNextEditWindow;
begin
  EditWindowList.Remove(Self);
  if ActiveEditWindow = Self then
    if EditWindowList.Count > 0 then
      ActiveEditWindow := TEditWindow(EditWindowList[EditWindowList.Count - 1])
    else
      ActiveEditWindow := nil;
  Name := 'EditWindow' + IntToStr(GetTickCount);
end;

procedure TRAFDEditWindow.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i : integer;
begin
  FDDesktop.WriteWindowPos(AppBuilder.raDesktop, 'EditWindow', Self);
  Closing := True;
  for i := 0 to TabControl.PageCount-1 do
    Editors[i].Check4Save;
  Hide;
  ActivateNextEditWindow;
  while TabControl.PageCount > 0 do
    Editors[0].Close;
  if Assigned(FOnChangeStatus) then FOnChangeStatus(Self);
  Action := caFree;
end;

function TRAFDEditWindow.FileOpen(AFileName : TFileName) : boolean;
var
  i : integer;
  Source : string;
  UnitFullName: TFileName;
  D : boolean;
  IsNewFile: Boolean;
begin
  Result := false;
  IsNewFile := AFileName = '';
	{если файл уже открыт, то активизировать его страницу}
	for i := 0 to EditorCount-1 do
		if Editors[i].FileName = AFileName then
		begin
			TabControl.ActivePage := Editors[i].Parent as TTabSheet;
      TabControlChange(nil);
			Result := true;
			Exit;
		end;
  {иначе создать страницу}
  D := False;
  FOnLoadFile(Self, AFileName, UnitFullName, Source, D);
  if D then
  begin
    MakeTab(UnitFullName, Source);
  	if IsNewFile then
  	begin
  		ActiveFileEditor.IsNewFile := IsNewFile;
  		ActiveFileEditor.Modified := True;
  	end;
    Result := true;
  end;
end;

function TRAFDEditWindow.MakeTab(const UnitFullName: TFileName;
  const Source: string): TFileEditor;
var
  Tab : TTabSheet;
begin
	Tab := TTabSheet.Create(TabControl);
	Tab.PageControl := TabControl;
	Tab.Parent := TabControl;
	Result := TFileEditor.CreateF(Self, Tab, UnitFullName, Source);
  Result.ExtensionChanged;
	TabControl.ActivePage := Tab;
	TabControlChange(TabControl);
	Visible := true;
  ActiveFileEditor := Result;
end;    { MakeTab }

function TRAFDEditWindow.OpenFile(const FileName : string; ShowOpenDialog : boolean)
  : Boolean;
var
  FN : TFileName;
begin
  Result := FileOpen(FileName);
  if not Result and ShowOpenDialog then
  begin
    FN := ChangeFileExt(FileName, '.pas');
    OpenFileDlg.FileName := FN;
    if OpenFileDlg.Execute then
      FileOpen(OpenFileDlg.FileName);
  end;
end;

function TRAFDEditWindow.FileNew: TFileName;
begin
  FileOpen('');
  Result := ActiveEditor.FileName;
end;

procedure TRAFDEditWindow.NewFile(const FileName : TFileName);
begin
  FileOpen('');
  ActiveEditor.FFileName := FileName;
end;

function TRAFDEditWindow.EditorCount : integer;
begin
  Result := TabControl.PageCount;
end;

function TRAFDEditWindow.GetEditor(index : integer) : TFileEditor;
begin
  Result := TabControl.Pages[index].Controls[0] as TFileEditor;
end;

function TRAFDEditWindow.ActiveEditor : TFileEditor;
begin
  if TabControl.ActivePage <> nil then
    Result := TabControl.ActivePage.Controls[0] as TFileEditor else
    Result := nil;
end;

function TRAFDEditWindow.Changed: Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to EditorCount - 1 do
    if Editors[i].Modified2 then Exit;
  Result := False;
end;

procedure TRAFDEditWindow.WriteDesktop(raProject: TRegAuto);
var
  i, j: Integer;
  FirstViewNumber: Integer;
  EditWindowSection: string;
  ViewSection: string;
  Editor: TFileEditor;
begin
  EditWindowSection := 'EditWindow' + IntToStr(EditWindowList.IndexOf(Self));
  FDDesktop.WriteWindowPos(raProject, EditWindowSection, Self);
  raProject.WriteInteger(EditWindowSection, 'ViewCount', EditorCount);
  FirstViewNumber := 0;
  for i := 0 to EditWindowList.Count - 1 do
    if EditWindowList[i] = Self then
      Break
    else
      inc(FirstViewNumber, TEditWindow(EditWindowList[i]).EditorCount);
  for i := 0 to EditorCount - 1 do
  begin
    Editor := Editors[i];
    ViewSection := 'View' + IntToStr(FirstViewNumber + i);
    raProject.EraseSection(ViewSection);
    raProject.WriteString(EditWindowSection, 'View' + IntToStr(i), IntToStr(FirstViewNumber + i));
    raProject.WriteString(ViewSection, 'Module', Editors[i].FileName);
    raProject.WriteInteger(ViewSection, 'CursorX', Editors[i].CaretX - 1);
    raProject.WriteInteger(ViewSection, 'CursorY', Editors[i].CaretY - 1);
    raProject.WriteInteger(ViewSection, 'LeftCol', Editors[i].LeftCol - 1);
    raProject.WriteInteger(ViewSection, 'TopLine', Editors[i].TopRow - 1);
    for j := 0 to 9 do
      if Editor.Bookmarks[j].Valid then
      begin
        raProject.WriteInteger(ViewSection, 'Bookmarks.' + IntToStr(j) + '.X', Editor.Bookmarks[j].X);
        raProject.WriteInteger(ViewSection, 'Bookmarks.' + IntToStr(j) + '.Y', Editor.Bookmarks[j].Y);
      end;
  end;
  raProject.WriteInteger(EditWindowSection, 'CurrentView',
    TabControl.ActivePage.PageIndex);
end;

procedure TRAFDEditWindow.RestoreDesktop(raProject: TRegAuto);
var
  i, j: Integer;
  EditWindowSection: string;
  ViewSection: string;
  FileName: TFileName;
  CurrentView: Integer;
  Editor: TFileEditor;
  X, Y: Integer;
begin
  EditWindowSection := 'EditWindow' + IntToStr(EditWindowList.IndexOf(Self));
  FDDesktop.RestoreWindowPos(raProject, EditWindowSection, Self);
  for i := 0 to raProject.ReadInteger(EditWindowSection, 'ViewCount', 0) - 1 do
  begin
    ViewSection := 'View' + raProject.ReadString(EditWindowSection,
      'View' + IntToStr(i), '');
    FileName := raProject.ReadString(ViewSection, 'Module', '');
    if FileExists(FileName) then
    begin
      if Cmp(ExtractFileExt(FileName), '.dpr') then
        ProjectManager.ShowSource
      else
        AppBuilder.OpenFile(FileName);
      ActiveEditor.CaretX := raProject.ReadInteger(ViewSection, 'CursorX', 1) - 1;
      ActiveEditor.CaretY := raProject.ReadInteger(ViewSection, 'CursorY', 1) - 1;
      ActiveEditor.SetLeftTop(raProject.ReadInteger(ViewSection, 'LeftCol', 1) - 1,
        raProject.ReadInteger(ViewSection, 'TopLine', 1) - 1);
    end;
    Editor := Editors[i];
    for j := 0 to 9 do
    begin
      X := raProject.ReadInteger(ViewSection, 'Bookmarks.' + IntToStr(j) + '.X', -1);
      Y := raProject.ReadInteger(ViewSection, 'Bookmarks.' + IntToStr(j) + '.Y', -1);
      if (X > -1) and (Y > -1) then
      begin
        Editor.Bookmarks[i].X := X;
        Editor.Bookmarks[i].Y := Y;
        Editor.Bookmarks[j].Valid := True;
      end
      else
        Editor.Bookmarks[j].Valid := False;
    end;
  end;
  CurrentView := raProject.ReadInteger(EditWindowSection, 'CurrentView', 0);
  if EditorCount > 0 then
  begin
    TabControl.ActivePage := Editors[Min(CurrentView, EditorCount)].Parent as
      TTabSheet;
    TabControlChange(nil);
  end;
end;

procedure TRAFDEditWindow.ecClosePageClick(Sender: TObject);
begin
  if ActiveEditor <> nil then
  begin
    ActiveFileEditor.Check4Save;
    ActiveEditor.Close;
  end;
end;

procedure TRAFDEditWindow.TabControlChange(Sender: TObject);
begin
  ODS('TRAFDEditWindow.TabControlChange');
  if Closing then Exit;
  ActiveFileEditor := ActiveEditor;
  if TabControl.PageCount = 0 then
    Close
  else
    if ActiveFileEditor <> nil then
      UpdateStatus;
  ODS('TRAFDEditWindow.TabControlChange....Done');
end;

procedure TRAFDEditWindow.UpdateStatus;
const
  Modi : array[boolean] of string[10] = ('', 'Modified');
  Modes : array[boolean] of string[10] = ('Overwrite', 'Insert');
begin
  if ActiveEditor = nil then Exit;
  Caption := ExtractRelativePath(ExtractFilePath(ProjectManager.GetProjectName),
    ActiveEditor.FileName);
  with StatusBar, ActiveEditor do
  begin
    Panels[0].Text := IntToStr(CaretY) + ':' + IntToStr(CaretX);
    Panels[1].Text := Modi[Modified];
    if ReadOnly then Panels[2].Text := 'Read only'
    else Panels[2].Text := Modes[InsertMode];
  end;
  if Assigned(FOnChangeStatus) then FOnChangeStatus(Self);
end;

procedure TRAFDEditWindow.ecOpenFileAtCursorClick(Sender: TObject);
begin
  with ActiveEditor do
    OpenFile(GetWordOnPos(Lines.Text, SelStart), true);
end;

procedure TRAFDEditWindow.FormResize(Sender: TObject);
begin
  TabControl.Top := 0;
  TabControl.Left := 0;
  TabControl.Width := ClientWidth;
  TabControl.Height := ClientHeight - StatusBar.Height;
end;

procedure TRAFDEditWindow.EditorLocalMenuPopup(Sender: TObject);
begin
  if ActiveEditor <> nil then
    ActiveEditor.pmEditorPopup;
end;

procedure TRAFDEditWindow.miTogleBreakPointClick(Sender: TObject);
begin
  if ActiveEditor <> nil then
    ActiveEditor.TogleBreakPoint;
end;

procedure TRAFDEditWindow.DeleteAllErrorPoint;
var
  i : integer;
begin
  for i := 0 to EditorCount-1 do
    Editors[i].ShowErrorPoint(-1);
end;

procedure TRAFDEditWindow.ShowBreakPoint(index : integer);
begin
//  with Main.InterObj.IDebugObject.BreakPoints[index] do
//    if OpenFile(UnitName, false) then
//      ActiveTab.FileEditor.ShowBreakPoint(PosBeg, PosEnd);
end;

procedure TRAFDEditWindow.ecReadOnlyItemClick(Sender: TObject);
begin
  if ActiveEditor <> nil then begin
    ecReadOnlyItem.Checked := not ecReadOnlyItem.Checked;
    ActiveEditor.ReadOnly := ecReadOnlyItem.Checked;
  end;
end;

procedure TRAFDEditWindow.miEvaluateModifyClick(Sender: TObject);
begin
{  Console.memMess.SelStart := Length(Console.memMess.Text);
  Console.Visible := true;
  Console.memMess.SetFocus; }
end;

procedure TRAFDEditWindow.WMGetMinMaxInfo(var M : TWMGetMinMaxInfo);
begin
  inherited;
  if Visible then begin
    M.MinMaxInfo^.ptMinTrackSize.Y := 200;
    M.MinMaxInfo^.ptMinTrackSize.X := 200;
  end;
end;

procedure TRAFDEditWindow.FormActivate(Sender: TObject);
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
end;


{************************ TFileEditor ***********************}

constructor TFileEditor.CreateF(AOwner : TComponent; ATab: TTabSheet;
  AFileName : TFileName; Source : string);
begin
  ODS('TFileEditor.Create');
  inherited Create(AOwner);
  FExecBegY := -1;
  FStackBegY := -1;
  FErrY := -1;
  FclErrorFC := clWhite;
  FclErrorBC := clRed;
  FEditWindow := AOwner as TRAFDEditWindow;
	Color := clWhite;
  Tab := ATab;
  Parser := TIParser.Create;
  Align := alClient;
  Parent := Tab;
  BoundsRect := Parent.ClientRect;
	Lines.Text := Source;
 	GutterWidth := 16;
 	Font := FEditWindow.lblFont.Font;
	ScrollBars := ssBoth;
	TabStop := False;
  LongTokens := True;
  FFileName := AFileName;
  UnitName := ExtractFileName(FileName);
  if Cmp(ExtractFileExt(UnitName), '.pas') or
     Cmp(ExtractFileExt(UnitName), '.dpr') then
    UnitName := ChangeFileExt(UnitName, '');
  Tab.Caption := UnitName;
  UpdateSettings;
  FileEditorList.Add(Self);
end;

destructor TFileEditor.Destroy;
begin
  ODS('TFileEditor.Destroy');
  FileEditorList.Remove(Self);
  ActiveFileEditor := nil;
  Parser.Free;
  if Assigned(FModuleInterface) then
    FModuleInterface.Release;
  inherited Destroy;
end;    { Destroy }

procedure TFileEditor.UpdateSettings;
begin
  ExtensionChanged;
  if Assigned(FEditWindow.FOnGetProperties) then
    FEditWindow.FOnGetProperties(Self);
  Invalidate;
end;    { UpdateSettings }

function TFileEditor.Modified2: Boolean;
begin
  Result := Modified or ((Designer <> nil) and
    (Designer as TRAFormDesigner).Changed);
end;

function TFileEditor.Save: Boolean;
begin
  if IsNewFile then begin Result := SaveAs; Exit; end;
  if IsStrings then begin SavePropValue; Result := True; Exit; end;
  Result := False;
  if Assigned(ModuleInterface) then
    ModuleInterface.Notify(ncBeforeSave);
  if Assigned(FEditWindow.FOnSaveFile) then
    FEditWindow.FOnSaveFile(Self, FFileName, Lines.Text, Result);
  if Result then
  begin
    Modified := false;
    if Designer <> nil then
      with (Designer as TRAFormDesigner) do
        if Changed then Save;
    IsNewFile := False;
    FEditWindow.UpdateStatus;
    if Assigned(ModuleInterface) then
      ModuleInterface.Notify(ncAfterSave);
  end;
end;

function TFileEditor.SaveAs: Boolean;
begin
	Result := False;
  if Assigned(ModuleInterface) then
    ModuleInterface.Notify(ncBeforeSave);
	if Assigned(FEditWindow.FOnSaveFileAs) then
		FEditWindow.FOnSaveFileAs(Self, FFileName, Lines.Text, Result);
	if Result then
	begin
    ExtensionChanged;
		Modified := false;
		UnitName := ExtractFileName(FileName);
		if Cmp(ExtractFileExt(UnitName), '.pas') then
			UnitName := ChangeFileExt(UnitName, '');
		Tab.Caption := UnitName;
		if Designer <> nil then
			(Designer as TRAFormDesigner).SaveToFile(ChangeFileExt(FileName, '.dfm'));
		FEditWindow.TabControlChange(nil);
		IsNewFile := False;
    IsStrings := False;
    FormFileEditor := nil;
		FEditWindow.UpdateStatus;
    if Assigned(ModuleInterface) then
      ModuleInterface.Notify(ncAfterSave);
	end;
end;

procedure TFileEditor.Check4Save;
var
  i: integer;
begin
  //Проверить изменен ли файл
  if Assigned(FEditWindow.FOnFileCloseQuery) then
    FEditWindow.FOnFileCloseQuery(Self);
 { check Strings editors, assotiated with this form file }
  for i := 0 to FEditWindow.EditorCount - 1 do
    if (FEditWindow.Editors[i] <> Self) and
       (FEditWindow.Editors[i].FormFileEditor = Self) then
      FEditWindow.Editors[i].Check4Save;

  FEditWindow.TabControlChange(nil);
end;

procedure TFileEditor.Close;
var
  i: integer;
  F : TRAFDEditWindow;
begin
  if Assigned(ModuleInterface) and not IsProjectFile then
  begin
    ModuleInterface.Notify(ncEditorSelected);
    ModuleInterface.Notify(ncModuleDeleted);
  end;
 { close Strings editors, assotiated with this form file }
  i := 0;
  while i <= FEditWindow.EditorCount - 1 do
    if (FEditWindow.Editors[i] <> Self) and
       (FEditWindow.Editors[i].FormFileEditor = Self) then
      FEditWindow.Editors[i].Close
    else
      inc(i);

 // Modified := False;
  FEditWindow.TabControl.ActivePage :=
    FEditWindow.TabControl.FindNextPage(Tab, false, true);
  if Assigned(FDesigner) then
    (FDesigner as TRAFormDesigner).FileEditorClosed;
  if FEditWindow.TabControl.PageCount <= 1 then
   { prevent displaying empty editor form, later it will be freed }
    FEditWindow.Hide;
  Parent := nil;
  Tab.Free;
  F := FEditWindow;
  Free;
  F.TabControlChange(nil);
end;

procedure TFileEditor.Changed;
begin
  if FErrY > -1 then
  begin
    FErrY := -1;
    Invalidate;
  end;
  inherited Changed;
  if Assigned(FEditWindow.FOnChangeStatus) then
    FEditWindow.FOnChangeStatus(FEditWindow);
  if Assigned(ModuleInterface) then
    ModuleInterface.Notify(ncEditorModified);
end;

procedure TFileEditor.StatusChanged;
begin
  inherited StatusChanged;
  FEditWindow.UpdateStatus;
end;

procedure TFileEditor.ExtensionChanged;
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  dfm := False;
  if Cmp(Ext, '.pas') or Cmp(Ext, '.dpr') or Cmp(Ext, '.inc') or
     Cmp(Ext, '.int') or Cmp(Ext, '.dpk') then
    HighLighter := hlPascal
  else if Cmp(Ext, '.sql') then
    HighLighter := hlSQL
  else if Cmp(Ext, '.cpp') or Cmp(Ext, '.hpp') or
     Cmp(Ext, '.c') or Cmp(Ext, '.h') then
    HighLighter := hlCBuilder
  else if Cmp(Ext, '.dfm') then
  begin
    HighLighter := hlPascal;
    dfm := True;
  end
  else if Cmp(Ext, '.ini') or Cmp(Ext, '.dsk') or
     Cmp(Ext, '.cfg') then
    HighLighter := hlIni
  else
    HighLighter := hlNone;
end;    { ExtensionChanged }

function TFileEditor.GetReservedWord(const Token: string;
  var Reserved: Boolean): Boolean;
const
  DfmReservedWords = ' object end ';
begin
  if dfm then
    Reserved := Pos(' ' + Token + ' ', DfmReservedWords) > 0
  else
    Reserved := False;
  Result := Reserved;
end;

function TFileEditor.UserReservedWords: boolean;
begin
  Result := dfm;
end;

procedure TFileEditor.SetParser(const S: string);
begin
  Parser.pcProgram := PChar(S);
  Parser.PCPos := Parser.pcProgram;
end;    {  }

procedure TFileEditor.SetModuleInterface(AModuleInterface: TIRAModuleInterface);
begin
  FModuleInterface := AModuleInterface;
  if Assigned(FModuleInterface) then
    FModuleInterface.AddRef;
end;

procedure TFileEditor.ShowPoint(PosBeg, PosEnd : integer;
  var BegX, BegY, EndX, EndY : integer);
var
  OldBegY, OldEndY ,iR : integer;
begin
  FErrY := -1;
  OldBegY := BegY; OldEndY := EndY;
  CaretFromPos(PosBeg, BegX, BegY);
  CaretFromPos(PosEnd+1, EndX, EndY);
  if (BegY < 0) and (OldBegY < 0) then exit;
  if (BegY > -1) and (BegY < TopRow) or (BegY > LastVisibleRow) then
    SetLeftTop(LeftCol, BegY - VisibleRowCount div 2);
  {Нужно ещё в бочок сдвигать...}
  {оптимизировано}
  for iR := OldBegY to OldEndY do PaintLine(iR, -1, -1);
  if BegY < OldBegY then OldBegY := BegY else OldBegY := OldEndY+1;
  if EndY > OldEndY then OldEndY := EndY else OldEndY := OldBegY-1;
  for iR := OldBegY to OldEndY do PaintLine(iR, -1, -1);
  {подрисовать на Gutter'е}
  Gutter.Paint;
end;

procedure TFileEditor.ShowExecutionPoint(PosBeg, PosEnd : integer);
begin
  ShowPoint(PosBeg, PosEnd, FExecBegX, FExecBegY, FExecEndX, FExecEndY);
end;

procedure TFileEditor.ShowCallStackPoint(PosBeg, PosEnd : integer);
begin
  ShowPoint(PosBeg, PosEnd, FStackBegX, FStackBegY, FStackEndX, FStackEndY);
end;

procedure TFileEditor.ShowBreakPoint(PosBeg, PosEnd : integer);
var
  BegX, BegY, v3, v4 : integer;
begin
  ShowPoint(PosBeg, PosEnd, BegX, BegY, v3, v4);
  CaretFromPos(PosBeg, BegX, BegY);
  SetCaret(BegX, BegY);
  SetFocus;
end;

procedure TFileEditor.ShowErrorPoint(const ErrPos: Integer);
var
  X, Y : integer;
begin
  CaretFromPos(ErrPos, X, Y);
  FErrY := Y;
  if (Y > -1) and (Y < TopRow) or (Y > LastVisibleRow) then
    SetLeftTop(LeftCol, Y - VisibleRowCount div 2);
  {Нужно ещё в бочок сдвигать...}
  if (Y > -1) then
    SetCaret(X, Y);
  {подрисовать на Gutter'е}
  Gutter.Paint;
end;

procedure TFileEditor.ChangeAttr(Line, ColBeg, ColEnd : integer);

  procedure Change(BegX, BegY, EndX, EndY : integer; clFC, clBC : TColor);

    procedure DoChange(const iBeg, iEnd : integer);
    var i : integer;
    begin
      for i := iBeg to iEnd do begin
        if clFC <> clDontChange then LineAttrs[i+1].FC := clFC;
        if clBC <> clDontChange then LineAttrs[i+1].BC := clBC;
      end;
    end;

  begin
    if (Line = BegY) and (Line = EndY) then DoChange(BegX, EndX-1)
    else begin
      if Line = BegY then DoChange(BegX, BegX + VisibleColCount);
      if (Line > BegY) and (Line < EndY) then DoChange(ColBeg, ColEnd);
      if Line = EndY then DoChange(ColBeg, EndX-1);
    end;
  end;

//var
//  i, BegX, BegY, EndX, EndY : integer;
begin
 {Show BreakPoints}
//  with Main.InterObj.IDebugObject do
//    for i := 0 to BreakPoints.Count-1 do with BreakPoints[i] do begin
//      if Cmp(PChar((Owner as TTab).UnitName), PChar(UnitName))
//      then begin
//        CaretFromPos(PosBeg, BegX, BegY);
//        CaretFromPos(PosEnd, EndX, EndY);
//        Change(BegX, BegY, EndX, EndY, FclBreakFC, FclBreakBC);
//      end;
//    end;
 {ShowExecutionPoint}
//  Change(FExecBegX, FExecBegY, FExecEndX, FExecEndY, FclExecFC, FclExecBC);
 {ShowCallStackPoint}
//  Change(FStackBegX, FStackBegY, FStackEndX, FStackEndY, FclStackFC, FclStackBC);
 {ShowErrorPoint}
  Change(0, FErrY, ColEnd, FErrY, FclErrorFC, FclErrorBC);
end;

procedure TFileEditor.GutterPaint(Canvas : TCanvas);

  procedure Draw(Images: TImageList; Y, ImageIndex : integer);
  var
    Ro : integer;
    R : TRect;
  begin
    if Y <> -1 then
    begin
      Ro := Y - TopRow;
      R := CalcCellRect(0, Ro);
      Images.Draw(Canvas, GutterWidth -GutterRightMargin -
        Images.Width{R.Left}, R.Top +
        (CellRect.Height - Images.Height) div 2 +1, ImageIndex);
    end;
  end;

var
  i: Integer;
//  v, BegY : integer;
begin
  {показать значок точки останова...}
//  with Main.InterObj.IDebugObject do
//    for i := 0 to BreakPoints.Count-1 do with BreakPoints[i] do begin
//      if Cmp(PChar((Owner as TTab).UnitName), PChar(UnitName))
//      then begin
//        CaretFromPos(PosBeg, v, BegY);
//        if (BegY <> FExecBegY) and (BegY <> FStackBegY) then
//          Draw(BegY, imBreak);
//      end;
//    end;
  if FErrY <> -1 then Draw(FEditWindow.Images, FErrY, imError);
  if FStackBegY <> -1 then Draw(FEditWindow.Images, FStackBegY, imStack);
  if FExecBegY <> -1 then Draw(FEditWindow.Images, FExecBegY, imExecute);
  for i := 0 to 9 do
    if Bookmarks[i].Valid then
      Draw(FEditWindow.imBookmark, Bookmarks[i].Y, i);
end;

procedure TFileEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
//var
//  P : integer;
//  W, W1 : string;
//  R : TRect;
begin
  inherited MouseMove(Shift, X, Y);
//  XX := X; YY := Y;
//  {$IFDEF DEBUG}
//  Tracer.Writeln(IntToStr(X)+', '+IntToStr(Y));
//  {$ENDIF}
//  P := PosFromMouse(X, Y);
//  if P = -1 then exit;
//  W := GetWordOnPos(Lines.Text, P);
//  if Main.InterObj.IDebugObject.EvaluateModify(W, W1, dfHint) then begin
//    R := ClientRect;
//    OffsetRect(R, ClientOrigin.X, ClientOrigin.Y);
//    (Owner as TTab).FForm.RAHint1.ActivateHint(R, W+' = '+W1);
//  end;
end;

procedure TRAFDEditWindow.WMSize(var Message: TWMSize);
begin
  inherited;
 { if ActiveTab <> nil then
    if Message.SizeType = SIZE_MAXIMIZED then
      ActiveTab.FileEditor.Paint
    else if Message.SizeType = SIZE_MINIMIZED then
      ActiveTab.FileEditor.NeedRepaint := true; }
end;

function TFileEditor.BPIndex(PBeg, PEnd : integer) : integer;
//var
//  i : integer;
begin
  Result := -1;
//  with Main.InterObj.IDebugObject do
//    for i := 0 to BreakPoints.Count-1 do with BreakPoints[i] do begin
//      if Cmp(PChar((Owner as TTab).UnitName), PChar(UnitName)) and
//       ((PosBeg <= PBeg) and (PEnd <= PosEnd))
//      then begin Result := i; exit end;
//    end;
end;

procedure TFileEditor.pmEditorPopup;
var
  P : integer;
begin
  P := PosFromCaret(CaretX, CaretY);
  FEditWindow.miTogleBreakPoint.Enabled := P <> -1;
  if (P = -1) or (BPIndex(P, P) = -1) then
    FEditWindow.miTogleBreakPoint.Caption := 'Add &breakpoint'
  else
    FEditWindow.miTogleBreakPoint.Caption := 'Remove &breakpoint';
  FEditWindow.ecReadOnlyItem.Checked := ReadOnly;
end;

procedure TFileEditor.TogleBreakPoint;
//var
//  P, PBeg, PEnd, Ind, iR : integer;
//  BegY, EndY : integer;
//  BP : TBreakPoint;
begin
//  P := PosFromCaret(CaretX, CaretY);
//  Ind := BPIndex(P, P);
//  if Ind = -1 then begin
//    ProgGetBlockByPos(iString(Lines.Text), P, PBeg, PEnd);
//    if (PBeg = -1) or (PEnd = -1) then exit;
//    BP := TBreakPoint.Create;
//    BP.UnitName := (Owner as TTab).UnitName;
//    BP.PosBeg := PBeg;
//    BP.PosEnd := PEnd;
//    Main.InterObj.IDebugObject.BreakPoints.Add(BP);
//  end else
//    with Main.InterObj.IDebugObject do begin
//      PBeg := BreakPoints[Ind].PosBeg;
//      PEnd := BreakPoints[Ind].PosEnd;
//      BreakPoints.Delete(Ind);
//    end;
//  CaretFromPos(PBeg, iR, BegY);
//  CaretFromPos(PEnd, iR, EndY);
//  PaintCaret(false);
//  for iR := BegY to EndY do PaintLine(iR, -1, -1);
//  PaintCaret(true);
//  {подрисовать на Gutter'е}
//  Gutter.Paint;
//  BreakPoints.UpdateBreakPoints;
end;

procedure TFileEditor.TextModified(Pos : integer; Action : TModifiedAction; Text : string);
//var
//  i : integer;
//  Modi : boolean;
begin
  inherited TextModified(Pos, Action, Text);
//  Modi := false;
  {сдвигаем все точки останова}
//  with Main.InterObj.IDebugObject do
//    for i := 0 to BreakPoints.Count-1 do with BreakPoints[i] do
//      if Cmp(PChar((Owner as TTab).UnitName), PChar(UnitName)) then
//        if Action = maInsert then begin
//          if PosBeg >= Pos then begin
//            inc(PosBeg, Length(Text));
//            inc(PosEnd, Length(Text));
//            Modi := true;
//          end else
//          if (PosBeg < Pos) and (Pos <= PosEnd) then begin
//            inc(PosEnd, Length(Text));
//            Modi := true;
//          end
//        end else {Action = maDelete}
//          if PosBeg >= Pos then begin
//            dec(PosBeg, Length(Text));
//            dec(PosEnd, Length(Text));
//            Modi := true;
//          end else
//          if (PosBeg < Pos) and (Pos <= PosEnd) then begin
//            dec(PosEnd, Length(Text));
//            Modi := true;
//          end;
// {!!!!}
//  if Modi then begin
//    BreakPoints.UpdateBreakPoints;
//   // NeedRepaint := true;
//    Invalidate;//Repaint;
//  end;
end;



{----------------------------------------------------------}
{----------------- Source manipulation --------------------}
{----------------------------------------------------------}
{  not very smart:                                         }
{    comments and non-standard indent styles not responsed }
{  to do this we must use TIParser                         }
{    may be in next release ...                            }
{----------------------------------------------------------}

procedure TFileEditor.InitParser;
begin
  ParserText := Lines.Text;
  SetParser(ParserText);
end;    { InitParser }

procedure TFileEditor.ReplaceToken(const NewToken: string);
var
  BegPos: Integer;
  EndPos: Integer;
begin
  BegPos := Parser.PosBeg[0];
  EndPos := Parser.PosEnd[0];
  ParserText := Copy(ParserText, 1, BegPos) + NewToken +
    Copy(ParserText, EndPos + 1, Length(ParserText));
  Parser.PCProgram := PChar(ParserText);
  Parser.PCPos := Parser.PCProgram + BegPos + Length(NewToken);
end;    { ReplaceToken }

procedure TFileEditor.FindFormDecl(var Line: Integer);
var
  FormDeclaration: string;
begin
  FormDeclaration := 'T' + Designer.Form.Name + ' = class(' +
    Designer.Form.ClassParent.ClassName + ')';
  Line := 0;
  while Line < Lines.Count do
    if Cmp(Trim(Lines[Line]), FormDeclaration) then
      Exit else
      inc(Line);
  Error('Form declaration not found');
end;    { FormDeclPos }

procedure TFileEditor.FindMethodSection(var Line: Integer);
var
  Token: string;
begin
  while Line < Lines.Count do
  begin
    SetParser(Lines[Line]);
		Token := Parser.Token;
    if Cmp(Token, 'procedure') or Cmp(Token, 'function') or
       Cmp(Token, 'end') or Cmp(Token, 'private') or Cmp(Token, 'protected') or
       Cmp(Token, 'public') or Cmp(Token, 'published') then
      Break else
      inc(Line);
    if Cmp(Token, 'implementation') then
      Error('Error in form declaration');
  end;
end;    { FormDeclPos }

procedure TFileEditor.FindUnitEnd(var Line: Integer);
begin
  while Line < Lines.Count do
    if ANSIStrLIComp(PChar(Lines[Line]), 'end.', 4) = 0 then
      Exit else
      inc(Line);
  Error('Unit end not found');
end;    { FindUnitEnd }

procedure TFileEditor.FindToken(const Token: string);
begin
  while not Cmp(Parser.Token, Token) do ;
end;    { FindToken }

procedure TFileEditor.RenameForm(const CurName, NewName: string);
var
  Token: string;
  CurNameClass, NewNameClass: string;
begin
  InitParser;
  CurNameClass := 'T' + CurName;
  NewNameClass := 'T' + NewName;
  Token := Parser.Token;
  while Token <> '' do
  begin
    if Cmp(Token, CurNameClass) then
    begin
      ReplaceToken(NewNameClass);
    end
    else if Cmp(Token, CurName) then
    begin
      ReplaceToken(NewName);
    end;
    Token := Parser.Token;
  end;
  SetLockText(ParserText);
  TextAllChanged;
  NotUndoable; { !!! }
end;    { RenameForm }

procedure TFileEditor.RenameUnit(const NewName: string);
var
  Token: string;
begin
  InitParser;
  Token := Parser.Token;
  while Token <> '' do
  begin
    if Cmp(Token, 'unit') then
    begin
      Parser.Token;
      ReplaceToken(NewName);
      SetLockText(ParserText);
      TextAllChanged;
      NotUndoable; { !!! }
      Exit;
    end;
    Token := Parser.Token;
  end;    { while }
end;    { RenameUnit }

{ create component declaration in source }
procedure TFileEditor.CreateComponent(Component: TComponent; const NewName: string);
var
  Declaration: string;
  Line: Integer;
  CY, T1: Integer;
begin
  CY := CaretY;
  T1 := TopRow;
  Declaration := '    ' + NewName + ': ' + Component.ClassName + ';';
  FindFormDecl(Line);
  inc(Line);
  BeginUpdate;
  try
    Lines.Insert(Line, Declaration);
    if T1 > Line then
    begin
      SetLeftTop(LeftCol, T1 + 1);
      CaretY := CY + 1;
    end;
  finally { wrap up }
    EndUpdate;
  end;    { try/finally }
end;

{ delete component declaration from source }
procedure TFileEditor.DeleteComponent(Component: TComponent);
var
  Declaration: string;
  Line: Integer;
  CY, T1: Integer;
begin
  DeleteStrings(Component);
  CY := CaretY;
  T1 := TopRow;
  Declaration := Component.Name + ': ' + Component.ClassName + ';';
  FindFormDecl(Line);
  while Line < Lines.Count do
    if Cmp(Trim(Lines[Line]), Declaration) then
    begin
      BeginUpdate;
      try
        Lines.Delete(Line);
        if T1 > Line then
        begin
          SetLeftTop(LeftCol, T1 - 1);
          CaretY := CY - 1;
        end;
      finally { wrap up }
        EndUpdate;
      end;    { try/finally }
      Break;
    end else
      inc(Line);
end;

{ rename component declaration in source }
procedure TFileEditor.RenameComponent(Component: TComponent; const NewName: string);
var
  Declaration: string;
  NewDeclaration: string;
  Line: Integer;
begin
  Declaration := Component.Name + ': ' + Component.ClassName + ';';
  NewDeclaration := '    ' + NewName + ': ' + Component.ClassName + ';';
  FindFormDecl(Line);
  while Line < Lines.Count do
    if Cmp(Trim(Lines[Line]), Declaration) then
    begin
      Lines[Line] := NewDeclaration;
      Break;
    end else
      inc(Line);
end;

{ find method in source }
function TFileEditor.FindMethod(const Name: string): Integer;
var
  Declaration: string;
  Line: Integer;
  S: string;
begin
  Result := -1;
  Declaration := 'T' + Designer.Form.Name + '.' + Name;
  Line := 0;
  while Line < Lines.Count do
  begin
    S := Lines[Line];
    if ( (ANSIStrLIComp(PChar(S), PChar('procedure ' + Declaration),
         Length('procedure ' + Declaration)) = 0) and
         (Length(S) > Length('procedure ' + Declaration)) and
         not (S[Length('procedure ' + Declaration) + 1] in StIdSymbols) ) or
       ( (ANSIStrLIComp(PChar(S), PChar('function ' + Declaration),
         Length('procedure ' + Declaration)) = 0) and
         (Length(S) > Length('function ' + Declaration)) and
         not (S[Length('function ' + Declaration) + 1] in StIdSymbols) ) then
    begin
      Result := Line;
      Exit;
    end
    else
      inc(Line);
  end;
end;

{ find method declaration in form declaration }
function TFileEditor.FindMethodDecl(const Name: string): Integer;
var
  Line: Integer;
  Token: string;
begin
  Result := -1;
  FindFormDecl(Line);
  if Line = -1 then
    RAFDError(deFormDeclNotFound);
  while Line < Lines.Count do
  begin
    SetParser(Lines[Line]);
    Token := Parser.Token;
    if Cmp(Token, 'end') then Exit;
    if Cmp(Token, 'procedure') or Cmp(Token, 'function') then
    begin
      Token := Parser.Token;
      if Cmp(Token, Name) then
      begin
        Result := Line;
        Exit;
      end;
    end;
    inc(Line);
  end;
end;


{ show method in source }
procedure TFileEditor.ShowMethod(const Name: string);
var
  Line: Integer;
begin
  ToggleToFront;
  Line := FindMethod(Name);
  if Line > -1 then
  begin
    SetLeftTop(LeftCol, Line);
    while Line < Lines.Count do
    	if Cmp(TrimRight(Lines[Line]), 'begin') then
    		Break else
    		inc(Line);
    CaretY := Line + 1;
    SelLength := 0;
    SetFocus;
    Invalidate;
  end;
end;

function TFileEditor.MethodExists(const Name: string): Boolean;
begin
  Result := FindMethod(Name) > -1;
end;

{ rename method in source }
procedure TFileEditor.RenameMethod(const CurName, NewName: string);
var
  Impl, Decl: Integer;
  SImpl, SDecl: string;
  Token: string;
begin
  ToggleToFront;
  Impl := FindMethod(CurName);
  if Impl = -1 then
    RAFDErrorN(deCantRenameMethod, CurName);
	SImpl := Lines[Impl];
	SetParser(SImpl);
	Parser.Token; { skip "procedure" or "function" word }
	Parser.Token; { skip class name }
	Token := Parser.Token; { skip "." }
	if Token <> '.' then
		RAFDErrorN(deCantRenameMethod, CurName);
	Token := Parser.Token; { method name }
	SImpl := Copy(SImpl, 1, Parser.PosBeg[0]) + NewName +
		Copy(SImpl, Parser.PosEnd[0] + 1, Length(SImpl));

  Decl := FindMethodDecl(CurName);
  if Decl = -1 then
    RAFDErrorN(deCantRenameMethod, CurName);
	SDecl := Lines[Decl];
	SetParser(SDecl);
	Parser.Token; { skip "procedure" or "function" word }
	Token := Parser.Token; { method name }
  if not Cmp(Token, CurName) then
    RAFDErrorN(deCantRenameMethod, CurName);
	SDecl := Copy(SDecl, 1, Parser.PosBeg[0]) + NewName +
		Copy(SDecl, Parser.PosEnd[0] + 1, Length(SDecl));

  Lines[Impl] := SImpl;
  Lines[Decl] := SDecl;
end;

{ parameter list not checked }
procedure TFileEditor.GetMethods(TypeData: PTypeData; Proc: TGetStrProc);
var
  Declaration: string;
  Line: Integer;
  S: string;
  Token: string;
begin
  { !!!! DEBUG VERSION !!!! }
  Declaration := 'T' + Designer.Form.Name + '.';
  SetParser(Lines.Text);
  FindToken('implementation');
  Line := 0;
  while Line < Lines.Count do
  begin
    S := Lines[Line];
    if (ANSIStrLIComp(PChar(S), PChar('procedure ' + Declaration),
         Length('procedure ' + Declaration)) = 0) or
       (ANSIStrLIComp(PChar(S), PChar('function ' + Declaration),
         Length('procedure ' + Declaration)) = 0) then
    begin
      Parser.pcPos := Parser.pcProgram +
        PosFromCaret(Length('procedure ' + Declaration), Line);
      Token := Parser.Token;
      if IsIdentifer(Token) then
        Proc(Token);
    end;
    inc(Line);
  end;
end;

{ CreateMethod - create method in source }
function TFileEditor.CreateMethod(const Name: string; TypeData: PTypeData)
  : TMethod;
var
  Declaration: string;
  Params: string;

  procedure MakeParams;
  var
    P: PChar;
    i: integer;
    ParamName, ParamType: string;
  begin
    case TypeData^.MethodKind of
      mkProcedure:
        Declaration := 'procedure ';
      mkFunction:
        Declaration := 'function ';
    end;
    Params := '';
    P := TypeData^.ParamList;
    for i := 1 to TypeData^.ParamCount do    { Iterate }
    begin
      inc(P); // skip #8 char
     // ParamName := ShortString(P^);
      SetString(ParamName, P + 1, Integer(P[0]));
      inc(P, Length(ParamName) + 1);
      SetString(ParamType, P + 1, Integer(P[0]));
      inc(P, Length(ParamType) + 1);
      Params := ConcatSep(Params, ParamName + ': ' + ParamType, '; ');
    end;    { for }
    if Params <> '' then
      Params := '(' + Params + ')';
  end;    { MakeParams }

var
  MLine, ELine: Integer;
  ImplDeclaration: string;
begin
  ToggleToFront;
  { !!!! DEBUG VERSION !!!! }
  if Trim(Name) = '' then Exit;

  FindFormDecl(MLine);
  FindMethodSection(MLine);
  ELine := MLine;
  FindUnitEnd(ELine);
  MakeParams;
  Declaration := '    procedure ' + Name + Params + ';';
  ImplDeclaration := 'procedure T' + Designer.Form.Name + '.' + Name +
    Params + ';';
  BeginUpdate;
  try
    Lines.Insert(MLine, Declaration);
    inc(ELine);
    Lines.Insert(ELine, '');
    Lines.Insert(ELine, 'end;');
    Lines.Insert(ELine, '');
    Lines.Insert(ELine, 'begin');
    Lines.Insert(ELine, ImplDeclaration);
		SetLeftTop(LeftCol, ELine);
		CaretY := ELine;
    CaretX := 0;
  finally { wrap up }
    EndUpdate;
  end;    { try/finally }
end;

{ strings edit support }

procedure TFileEditor.EditStrings(AInstance: TPersistent; AStrings: TSTrings;
  APropInfo: PPropInfo; ACurrentValue: TStrings);
var
  Editor: TFileEditor;
begin
  Editor := FEditWindow.MakeTab(Designer.Form.Name + '.' +
    Designer.GetObjectName(AInstance) + '.' + APropInfo^.Name, ACurrentValue.Text);
  Editor.IsStrings := True;
  Editor.StringsComponent := AInstance;
  Editor.Strings := AStrings;
  Editor.FormFileEditor := Self;
  PostMessage(Editor.Handle, WM_RESETFOCUS, 0, 0);
end;    { EditStrings }

function TFileEditor.ActivateStrings(AInstance: TPersistent; AStrings: TSTrings;
  APropInfo: PPropInfo): Boolean;
var
  i: integer;
begin
 { check Strings editors, assotiated with this form file }
  for i := 0 to FEditWindow.EditorCount - 1 do
    if (FEditWindow.Editors[i] <> Self) and
       (FEditWindow.Editors[i].FormFileEditor = Self) and
       (FEditWindow.Editors[i].Strings = AStrings) then
    begin
      PostMessage(FEditWindow.Editors[i].Handle, WM_RESETFOCUS, 0, 0);
      Result := True;
      Exit;
    end;
  Result := False;
end;    { EditStrings }

procedure TFileEditor.SavePropValue;
begin
  Strings.Assign(Lines);
  Modified := False;
end;    { SavePropValue }

procedure TFileEditor.WMResetFocus(var Message: TMessage);
begin
  ToggleToFront;
end;    { WMResetFocus }

procedure TFileEditor.ToggleToFront;
begin
//  ODS('TFileEditor.ToggleToFront');
  FEditWindow.TabControl.ActivePage := Tab;
  FEditWindow.TabControlChange(nil);
  if FEditWindow.WindowState = wsMinimized then
    FEditWindow.WindowState := wsNormal;
  SetFocus;
end;

procedure TFileEditor.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Assigned(ModuleInterface) then
    ModuleInterface.Notify(ncEditorSelected);
end;

procedure TFileEditor.DeleteStrings(AComponent: TComponent);
var
  i: integer;
begin
 { check Strings editors, assotiated with this form file }
	i := 0;
	while i <= FEditWindow.EditorCount - 1 do
		if (FEditWindow.Editors[i] <> Self) and
			 (FEditWindow.Editors[i].FormFileEditor = Self) and
			 (FEditWindow.Editors[i].StringsComponent = AComponent) then
		begin
			FEditWindow.Editors[i].Modified := False;
			FEditWindow.Editors[i].Close;
		end
		else
			inc(i);
end;



procedure TRAFDEditWindow.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
//
end;

procedure TRAFDEditWindow.FormKeyPress(Sender: TObject; var Key: Char);
begin
//
end;

procedure TRAFDEditWindow.ecPropertiesItemClick(Sender: TObject);
begin
  AppBuilder.ToolsEditorOptionsItem.Click;
end;


initialization
  EditWindowList := TList.Create;
  FileEditorList := TFileEditorList.Create;
finalization
  EditWindowList.Free;
  FileEditorList.Free
end.

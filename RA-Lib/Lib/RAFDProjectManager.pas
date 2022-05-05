{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       description : Project Manager

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDProjectManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls,
  ToolIntf, EditIntf, VirtIntf, ExptIntf, IParser, RARegAuto,
  RAFDCompat, RAFDEditor;

type

  TRAProjectItem = class
  private
    FFileName: TFileName;
    FUnitName: string;
    FFormName: string;
  public
    property FileName: TFileName read FFileName;
    property UnitName: string read FUnitName;
    property FormName: string read FFormName;
  end;

  TRAProjectParser = class(TIParser)
  private
    FFiles: TList;
    FFileName: TFileName;
    FProgramName: string;
    FSource: string;
    FModified: Boolean;
    procedure Clear;
    procedure Update;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRAProjectManager = class(TForm)
    Panel2: TPanel;
    FileHeader: THeaderControl;
    FileList: TListBox;
    StatusBar: TStatusBar;
    ProjManPopup: TPopupMenu;
    SaveProject1: TMenuItem;
    SaveAsTemplateItem: TMenuItem;
    N2: TMenuItem;
    NewUnit1: TMenuItem;
    NewForm1: TMenuItem;
    AddfileItem: TMenuItem;
    RemoveFileItem: TMenuItem;
    N1: TMenuItem;
    PrjUnitItem: TMenuItem;
    PrjFormItem: TMenuItem;
    EditProject1: TMenuItem;
    N3: TMenuItem;
    Options1: TMenuItem;
    UpdateItem: TMenuItem;
    LocalMenuItem: TMenuItem;
    ZoomWindowItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure FileListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FileHeaderSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure UpdateItemClick(Sender: TObject);
  private
    FProjectName: string;
    FFileEditor: TFileEditor;
    FModuleInterface: TIModuleInterface;
    FOnProjectOpen: TNotifyEvent;
    FOnProjectClose: TNotifyEvent;
    FParser: TRAProjectParser;
  public
    procedure OpenProject(const FileName: TFileName);
    procedure CloseProject;
    procedure ShowSource;
    procedure SourceClosing;
    function IsProjectOpen: boolean;
    function ProjectTitle: string;
    function GetProjectName: string;
    function GetUnitCount: Integer;
    function GetUnitName(Index: Integer): string;
    function GetFormCount: Integer;
    function GetFormName(Index: Integer): string;
    function EnumProjectUnits(EnumProc: TProjectEnumProc; Param: Pointer): Boolean;
    procedure SetModuleInterface(AModuleInterface: TIModuleInterface);
    function GetNewModuleName(var UnitIdent, FileName: string): Boolean;
    function GetNewModuleAndClassName(const Prefix: string; var UnitIdent,
      ClassName, FileName: string): Boolean;
    function CreateModule(const ModuleName: string;
      Source, Form: TIStream; CreateFlags: TCreateModuleFlags): Boolean;
    function ProjectCreate(ProjectCreator: TIProjectCreator;
      CreateFlags: TCreateProjectFlags): TIModuleInterface;
    function ModuleCreate(ModuleCreator: TIModuleCreator;
      CreateFlags: TCreateModuleFlags): TIModuleInterface;
    procedure RestoreDesktop(raProject: TRegAuto);
    procedure WriteDesktop(raProject: TRegAuto);
    procedure UpdateFromSource;
    function GetProjectItem(Index: Integer): TRAProjectItem;
    function IsFileInProject(const AFileName: TFileName): Boolean;
    function GetFileUnitName(const AFileName: TFileName): string;
    function GetFileFormName(const AFileName: TFileName): string;
    property FileEditor: TFileEditor read FFileEditor;
    property ModuleInterface: TIModuleInterface read FModuleInterface;
    property OnProjectOpen: TNotifyEvent read FOnProjectOpen write FOnProjectOpen;
    property OnProjectClose: TNotifyEvent read FOnProjectClose write FOnProjectClose;
  end;

  TProjectManager = class(TRAProjectManager);

  procedure Create;
  procedure Free;

var
  ProjectManager: TProjectManager;

implementation

uses RAUtils, iMTracer, RAFD, RAFDDesktop, RAFDIDE, RAFDAppBuilder,
  RAFDAlignPalette, RAFDPropertyInspector;

{$R *.dfm}

procedure Create;
begin
  if ProjectManager = nil then
    ProjectManager := TProjectManager.Create(Application);
end;    { Create }

procedure Free;
begin
  ProjectManager.Free;
  ProjectManager := nil
end;


{ TRAProjectParser }
 
constructor TRAProjectParser.Create;
begin
  inherited Create;
  FFiles := TList.Create;
end;

destructor TRAProjectParser.Destroy;
begin
  ClearList(FFiles);
  inherited Destroy;
end;

procedure TRAProjectParser.Clear;
begin
  ClearList(FFiles);
end;

procedure TRAProjectParser.Update;
var
  ProjectPath: TFileName;
  Token1, UnitName, UnitFileName, FormName: string;
  ProjectItem: TRAProjectItem;
begin
  ClearList(FFiles);
  ProjectPath := ExtractFilePath(FFileName);
  pcProgram := PChar(FSource);
  PCPos := pcProgram;
  Token1 := Token;
  if not Cmp(Token1, 'program') then
    RAFDErrorN2(deClauseIncorrect, FFileName, 'program');
  FProgramName := Token;
  if not Cmp(Token, ';') then
    RAFDErrorN2(deClauseIncorrect, FFileName, 'program');
  if not Cmp(Token, 'uses') then
    RAFDErrorN2(deClauseIncorrect, FFileName, 'uses');
  while True do
  begin
    ReturnComments := False;
    UnitName := Token;
    UnitFileName := AddPath(UnitName + '.pas', ProjectPath);
    if not IsIdentifer(UnitName) then
      RAFDErrorN2(deClauseIncorrect, FFileName, 'uses');
    Token1 := Token;
    if Token1 = 'in' then
    begin
      UnitFileName := AddPath(GetStringValue(Token), ProjectPath);
      ReturnComments := True;
      Token1 := Token;
      if (Token1 <> '') and (Token1[1] = '{') then
      begin
        FormName := Trim(Copy(Token1, 2, Length(Token1) - 2));
        Token1 := Token;
      end
      else
        FormName := '';
      ProjectItem := TRAProjectItem.Create;
      ProjectItem.FUnitName := UnitName;
      ProjectItem.FFileName := UnitFileName;
      ProjectItem.FFormName := FormName;
      FFiles.Add(ProjectItem);
    end;
    if Token1 = ',' then
     { Ok! }
    else if Token1 = ';' then
      Break
    else
      RAFDErrorN2(deClauseIncorrect, FFileName, 'uses');
  end;
  //FileList.Items.Assign(FUnits);
end;    { Update }


{ TRAFDProjectManager }

procedure TRAProjectManager.FormCreate(Sender: TObject);
begin
  FParser := TRAProjectParser.Create;
  Caption := ResStr(deProjectManager, Caption);
  FileHeader.Sections[0].Text := ResStr(dePrjMngrUnit, FileHeader.Sections[0].Text);
  FileHeader.Sections[1].Text := ResStr(dePrjMngrForm, FileHeader.Sections[1].Text);
  FileHeader.Sections[2].Text := ResStr(dePrjMngrPath, FileHeader.Sections[2].Text);
  LocaleMenuItem(UpdateItem, dePrjMngrUpdateItem, deNoHint);
end;

procedure TRAProjectManager.FormDestroy(Sender: TObject);
begin
  ProjectManager := nil;
  FParser.Free;
end;

function TRAProjectManager.ProjectTitle: string;
begin
  Result := ChangeFileExt(ExtractFileName(FProjectName), '');
end;

function TRAProjectManager.GetProjectName: string;
begin
  Result := FProjectName;
end;

procedure TRAProjectManager.OpenProject(const FileName: TFileName);
var
  Cancel: Boolean;
begin
  ODS('ProjectManager.OpenProject');
  CloseProject;
  if Assigned(ActiveEditWindow) then
    ActiveEditWindow.Close;
  if Assigned(RAToolServices) then
  begin
    Cancel := False;
    RAToolServices.FileNotification(fnProjectOpening, FileName, Cancel);
    if Cancel then Abort;
  end;
  FProjectName := FileName;
  FParser.FFileName := FProjectName;
  UpdateFromSource;
  if Assigned(RAToolServices) then
    RAToolServices.FileNotification(fnProjectOpened, FileName, Cancel);
  if Assigned(FOnProjectOpen) then FOnProjectOpen(Self);
  AppBuilder.RestoreProjectDesktop;
end;

procedure TRAProjectManager.CloseProject;
var
  Cancel: Boolean;
begin
  if FProjectName = '' then Exit;
  ODS('ProjectManager.CloseProject');
  AppBuilder.WriteProjectDesktop;
  if Assigned(RAToolServices) then
  begin
    Cancel := False;
    RAToolServices.FileNotification(fnProjectClosing, FProjectName, Cancel);
    if Cancel then Abort;
  end;
  if FParser.FModified then
  begin
    {SAVE !!!!!!!!!!!!!}
  end;
  if Assigned(ActiveEditWindow) then
    ActiveEditWindow.Close;
  FProjectName := '';
  Close;
  FParser.Clear;
  if Assigned(FOnProjectClose) then FOnProjectClose(Self);
end;

function TRAProjectManager.GetUnitCount: Integer;
begin
  Result := FParser.FFiles.Count;
end;

function TRAProjectManager.GetUnitName(Index: Integer): string;
begin
  Result := GetProjectItem(Index).FUnitName;
end;

function TRAProjectManager.GetProjectItem(Index: Integer): TRAProjectItem;
begin
  Result := TRAProjectItem(FParser.FFiles[Index]);
end;

function TRAProjectManager.EnumProjectUnits(EnumProc: TProjectEnumProc;
  Param: Pointer): Boolean;
var
  i: Integer;
begin
  for i := 0 to FParser.FFiles.Count - 1 do
    with TRAProjectItem(FParser.FFiles[i]) do
      EnumProc(Param, FFileName, FUnitName, FFormName);
  Result := True;
end;

function TRAProjectManager.GetFormCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FParser.FFiles.Count - 1 do
    if TRAProjectItem(FParser.FFiles[i]).FFormName <> '' then
      Inc(Result);
end;

function TRAProjectManager.GetFormName(Index: Integer): string;
var
  i: Integer;
begin
  for i := 0 to FParser.FFiles.Count - 1 do
  begin
    Result := TRAProjectItem(FParser.FFiles[i]).FFormName;
    if Result <> '' then
      if Index = 0 then
        Exit
      else
        Dec(Index);
  end;
  Result := '';
end;

function TRAProjectManager.IsFileInProject(const AFileName: TFileName): Boolean;
var
  i: Integer;
begin
  for i := 0 to FParser.FFiles.Count - 1 do
    with TRAProjectItem(FParser.FFiles[i]) do
      if Cmp(FFileName, AFileName) then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TRAProjectManager.GetFileUnitName(const AFileName: TFileName): string;
var
  i: Integer;
begin
  for i := 0 to FParser.FFiles.Count - 1 do
    with TRAProjectItem(FParser.FFiles[i]) do
      if Cmp(FFileName, AFileName) then
      begin
        Result := FUnitName;
        Exit;
      end;
  Result := '';
end;

function TRAProjectManager.GetFileFormName(const AFileName: TFileName): string;
var
  i: Integer;
begin
  for i := 0 to FParser.FFiles.Count - 1 do
    with TRAProjectItem(FParser.FFiles[i]) do
      if Cmp(FFileName, AFileName) then
      begin
        Result := FFormName;
        Exit;
      end;
  Result := '';
end;

procedure TRAProjectManager.ShowSource;
begin
  if FProjectName = '' then Exit;
  if not Assigned(FileEditor) then
  begin
    AppBuilder.CheckActiveEditWindow;
    if not ActiveEditWindow.OpenFile(FProjectName, False) then
  		Error('Can''t open file');
    FFileEditor := ActiveFileEditor;
    FFileEditor.IsProjectFile := True;
    FFileEditor.Lines.Text := FParser.FSource;
    FFileEditor.SetModuleInterface(ModuleInterface as TIRAModuleInterface);
  end;
  FFileEditor.ToggleToFront;
end;    { ShowSource }

procedure TRAProjectManager.SourceClosing;
begin
  if Assigned(FileEditor) then
  begin
    FParser.FSource := FileEditor.Lines.Text;
    FFileEditor := nil;
  end;
end;    { CloseSource }

procedure TRAProjectManager.SetModuleInterface(AModuleInterface: TIModuleInterface);
begin
  FModuleInterface := AModuleInterface;
end;

function TRAProjectManager.IsProjectOpen: boolean;
begin
  Result := FProjectName <> '';
end;

function TRAProjectManager.GetNewModuleName(var UnitIdent, FileName: string)
  : Boolean;
var
  ClassName: string;
begin
  Result := GetNewModuleAndClassName('', UnitIdent, ClassName, FileName);
end;

function TRAProjectManager.GetNewModuleAndClassName(const Prefix: string;
  var UnitIdent, ClassName, FileName: string): Boolean;
begin
 { !!! NOT COMPLETE !!! }
  UnitIdent := 'Unit1';
  FileName := ExtractFilePath(FProjectName) + UnitIdent + '.pas';
  ClassName := 'Form1';
  Result := True;
end;

function TRAProjectManager.CreateModule(const ModuleName: string;
	Source, Form: TIStream; CreateFlags: TCreateModuleFlags): Boolean;
var
  TmpFile: string;
  TmpStream1: TStringStream;
  TmpStream2: TStringStream;
  FileStream: TFileStream;
  Buffer: string;
  P: Integer;
begin
  AppBuilder.CheckActiveEditWindow;
 { read source }
  ActiveEditWindow.NewFile(ModuleName);
  SetLength(Buffer, 4096); P := 4096;
  while P = 4096 do
  begin
   {$IFDEF RA_D3}
    P := Source.Read(Buffer[Length(Buffer) + 1 - 4096], 4096);
   {$ELSE}
    NotImplemented('CreateModule');
   {$ENDIF}
    SetLength(Buffer, Length(Buffer) + P);
  end;    { while }
  ActiveFileEditor.Lines.Text := Buffer;
 { read form }
  SetLength(Buffer, 4096); P := 4096;
  while P = 4096 do
  begin
   {$IFDEF RA_D3}
    P := Form.Read(Buffer[Length(Buffer) + 1 - 4096], 4096);
   {$ELSE}
    NotImplemented('CreateModule');
   {$ENDIF}
    SetLength(Buffer, Length(Buffer) + P);
  end;    { while }
  TmpFile := GenTempFileNameExt('', '.dfm');
	TmpStream1 := TStringStream.Create(Buffer);
	TmpStream2 := TStringStream.Create('');
  try
   { i don't know why creating form directly from Form stream sometimes
     raises error, but converting-reconvering solve this problem }

    ObjectResourceToText(TmpStream1, TmpStream2);
		TmpStream1.Position := 0; TmpStream2.Position := 0;
		ObjectTextToResource(TmpStream2, TmpStream1);
    FileStream := TFileStream.Create(TmpFile, fmOpenWrite or fmCreate);
    try
  		FileStream.CopyFrom(TmpStream1, 0);
    finally
  		FileStream.Free;
    end;
  	AppBuilder.NewDesigner(TmpFile, ActiveFileEditor);
  finally
		TmpStream1.Free;
		TmpStream2.Free;
    DeleteFile(TmpFile);
  end;    { try/finally }
  AppBuilder.UpdateState;
  Result := True;
end;

function TRAProjectManager.ProjectCreate(ProjectCreator: TIProjectCreator;
	CreateFlags: TCreateProjectFlags): TIModuleInterface;
begin
  Result := nil;
end;

function TRAProjectManager.ModuleCreate(ModuleCreator: TIModuleCreator;
	CreateFlags: TCreateModuleFlags): TIModuleInterface;
begin
  Result := nil;
end;

procedure TRAProjectManager.FileListDblClick(Sender: TObject);
begin
  if (FileList.ItemIndex > - 1) and Assigned(ToolServices) then
    ToolServices.OpenFile(TRAProjectItem(FileList.Items.Objects[FileList.ItemIndex]).FFileName);
end;

procedure TRAProjectManager.WriteDesktop(raProject: TRegAuto);
begin
  FDDesktop.WriteWindowPos(raProject, 'ProjectManager', Self);
end;

procedure TRAProjectManager.RestoreDesktop(raProject: TRegAuto);
begin
  FDDesktop.RestoreWindowPos(raProject, 'ProjectManager', Self);
end;

procedure TRAProjectManager.FileListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

  procedure DrawItem(const Left, Width: Integer; const Caption: string);
  var
    R: TRect;
  begin
    R := Bounds(Left, Rect.Top, Width, Rect.Bottom - Rect.Top);
    DrawText((Control as TListBox).Canvas.Handle,
      PChar(Caption),
      -1,
      R,
      DT_END_ELLIPSIS);
  end;

var
  S: string;
  ProjectItem: TRAProjectItem;
begin
  ProjectItem := TRAProjectItem(FileList.Items.Objects[Index]);
  (Control as TListBox).Canvas.FillRect(Rect);
  DrawItem(FileHeader.Sections[0].Left + 2, FileHeader.Sections[0].Width - 4,
    ProjectItem.FUnitName);
  DrawItem(FileHeader.Sections[1].Left + 2, FileHeader.Sections[1].Width - 4,
    ProjectItem.FFormName);
  S := ExtractFilePath(ProjectItem.FFileName);
  if Cmp(S, ExtractFilePath(GetProjectName)) then
    S := '';
  if (Length(S) > 3) and (S[Length(S)] = '\') then
    Delete(S, Length(S), 1);
  DrawItem(FileHeader.Sections[2].Left + 2, FileHeader.Sections[2].Width - 4,
    PChar(S));
end;

procedure TRAProjectManager.FileHeaderSectionResize(
  HeaderControl: THeaderControl; Section: THeaderSection);
begin
  FileList.Invalidate;
end;

procedure TRAProjectManager.UpdateFromSource;
var
  i: Integer;
begin
  if Assigned(FileEditor) then
    FParser.FSource := FileEditor.Lines.Text
  else
    FParser.FSource := LoadTextFile(FProjectName);
  FParser.Update;
  FileList.Items.Clear;
  for i := 0 to FParser.FFiles.Count - 1 do
    FileList.Items.AddObject('', FParser.FFiles[i]);
  Invalidate;
end;

procedure TRAProjectManager.UpdateItemClick(Sender: TObject);
begin
  UpdateFromSource;
end;


initialization
finalization
  Free;
end.

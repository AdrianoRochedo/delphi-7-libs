{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       form        : TRAFDPalette
       description : Component palette

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDPalette;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Buttons, RAComponentPanel,
  DsgnIntf, RARegAuto, RAButtons;

type
  TRAFDPalette = class(TForm)
    TabControl: TTabControl;
    Palette: TRAComponentPanel;
    RegAuto1: TRegAuto;
    RACaptionButton1: TRACaptionButton;
    RACaptionButton2: TRACaptionButton;
    procedure FormCreate(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure PaletteClick(Sender: TObject; Button: Integer);
    procedure RACaptionButton1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Minim: Boolean;
    FButtonDown: Boolean;
    FComponentClass: TComponentClass;
    FUpdateLock: Integer;
    procedure WMGetMinMaxInfo(var M : TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    procedure UpdatePalette;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResetButton;
    function GetControl: TControl;
    procedure HideFromWin;
    procedure PackageUnloaded(const Module: HModule);
    property ButtonDown: Boolean read FButtonDown;
    property ComponentClass: TComponentClass read FComponentClass;
  end;

  TPaletteEntry = class
  private
    FModule: HModule;
    FPage: string;
    FComponentClass: TComponentClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadBitmap(Bitmap: TBitmap);
    property Module: HModule read FModule;
    property Page: string read FPage;
    property ComponentClass: TComponentClass read FComponentClass;
  end;

  TPaletteComponentList = class(TList)
  private
    function GetItem(Index: Integer): TPaletteEntry;
  public
    property Items[Index: Integer]: TPaletteEntry read GetItem; default;
  end;


  TDesignPackage = class
  private
    FFileName: TFileName;
    FDescription: string;
    FModule: HModule;
    FEditorGroup: Integer;
    FLoaded: Boolean;
    FIDEPackage: Boolean;
  public
    property FileName: TFileName read FFileName;
    property Description: string read FDescription;
    property Module: HModule read FModule;
    property Loaded: Boolean read FLoaded;
    property IDEPackage: Boolean read FIDEPackage;
  end;

  TDesignPackageList = class(TList)
  private
    function GetItem(Index: Integer): TDesignPackage;
  public
    procedure Add(const FileName: TFileName; const Load,
      IDEPackage: Boolean; const Description: string);
    procedure Delete(const Index: Integer);
    procedure LoadPackage(const Index: Integer);
    procedure UnloadPackage(const Index: Integer);
    destructor Destroy; override;
    property Items[Index: Integer]: TDesignPackage read GetItem; default;
  end;

  procedure Create;
  procedure Free;

  function NoIcon(AComponent: TComponent): Boolean;

var
  ComponentPalette: TRAFDPalette;
  DesignPackageList: TDesignPackageList;
  ComponentList: TPaletteComponentList;
  NoIconList: TPaletteComponentList;

implementation

uses RAUtils, iMTracer, RAFDDesigner, RAFD, RAFDAppBuilder;

{$R *.DFM}

procedure Create;
begin
  if ComponentPalette = nil then
    ComponentPalette := TRAFDPalette.Create(Application);
end;

procedure Free;
begin
  ComponentPalette.Free;
  ComponentPalette := nil;
end;


procedure LoadComponentBitmap(Component: TComponent; Bitmap: TBitmap);
var
  i: integer;
begin
	for i := 0 to ComponentList.Count - 1 do
		with TPaletteEntry(ComponentList[i]) do
  		if Cmp(Component.ClassName, FComponentClass.ClassName) then
  		begin
        LoadBitmap(Bitmap);
        Exit;
  		end;
end;    { LoadComponentBitmap }

function ComponentRegistered(AComponentClass: TComponentClass): Boolean;
var
  i: integer;
begin
	for i := 0 to ComponentList.Count - 1 do
		if TPaletteEntry(ComponentList[i]).FComponentClass = AComponentClass then
    begin
      Result := True;
      Exit;
    end;
  Result := False;  
end;    { ComponentRegistered }

procedure RARegisterComponentsProc(const Page: string;
    ComponentClasses: array of TComponentClass);
var
  i: integer;
  PaletteEntry: TPaletteEntry;
begin
  for i := Low(ComponentClasses) to High(ComponentClasses) do    { Iterate }
    if not ComponentRegistered(ComponentClasses[i]) then
    begin
      PaletteEntry := TPaletteEntry.Create;
      PaletteEntry.FPage := Page;
      PaletteEntry.FComponentClass := ComponentClasses[i];
      PaletteEntry.FModule := ActiveModule;
      ComponentList.Add(PaletteEntry);
      RegisterClasses([ComponentClasses[i]]);
    end;    { for }
end;

procedure RARegisterNonActiveXProc(ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);
begin
  { stub }
end;

procedure RARegisterNoIconProc(ComponentClasses: array of TComponentClass);
var
  i: integer;
begin
  for i := Low(ComponentClasses) to High(ComponentClasses) do    { Iterate }
  begin
    NoIconList.Add(ComponentClasses[i]);
    RegisterClasses([ComponentClasses[i]]);
  end;
end;

function NoIcon(AComponent: TComponent): Boolean;
var
  i: Integer;
begin
  for i := 0 to NoIconList.Count - 1 do    { Iterate }
  begin
    if AComponent is TComponentClass(NoIconList[i]) then
    begin
      Result := True;
      Exit;
    end;
  end;    { for }
  Result := False;
end;

procedure RARegisterCustomModuleProc(Group: Integer;
    ComponentBaseClass: TComponentClass;
    CustomModuleClass: TCustomModuleClass);
begin
  { stub }
end;

procedure RAFreeCustomModulesProc(Group: Integer);
begin
  { stub }
end;

procedure RACreateVCLComObjectProc(Component: TComponent);
begin
  { stub }
end;

{ TPaletteEntry }
constructor TPaletteEntry.Create;
begin
end;    { Create }

destructor TPaletteEntry.Destroy;
begin
  inherited Destroy;
end;    { Destroy }

{ TRAFDPalette }
procedure TRAFDPalette.FormCreate(Sender: TObject);
begin
  Name := 'Palette';
  RegAuto1.RegPath := BaseRegKey;
  RAFDDesigner.LoadComponentBitmapProc := LoadComponentBitmap;
  UpdatePalette;
end;

function TRAFDPalette.GetControl: TControl;
var
  Form: TRAFDPalette;
begin
  Form := FindFormByClass(TRAFDPalette) as TRAFDPalette;
  if Form = nil then
    Form := TRAFDPalette.Create(Application);
	Result := Form.TabControl;
  FButtonDown := False;
end;

procedure TRAFDPalette.HideFromWin;
var
  Form: TRAFDPalette;
begin
  Form := FindFormByClass(TRAFDPalette) as TRAFDPalette;
  if Form = nil then Exit;
	Form.TabControl.Parent := Form;
  FButtonDown := False;
end;

procedure TRAFDPalette.ResetButton;
var
  Form: TRAFDPalette;
begin
  Form := FindFormByClass(TRAFDPalette) as TRAFDPalette;
  if Form = nil then Exit;
  FButtonDown := False;
  Form.Palette.SetMainButton;
end;

procedure TRAFDPalette.UpdatePalette;
var
  i: integer;
begin
  if FUpdateLock > 0 then Exit;
  TabControl.Tabs.Clear;
  for i := 0 to ComponentList.Count - 1 do
  begin
    if TabControl.Tabs.IndexOf(TPaletteEntry(ComponentList[i]).FPage) = -1 then
      TabControl.Tabs.Add(TPaletteEntry(ComponentList[i]).FPage);
  end;
  TabControlChange(nil);
end;    { UpdatePalette }

procedure TRAFDPalette.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TRAFDPalette.EndUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then
    UpdatePalette;
end;

procedure TRAFDPalette.TabControlChange(Sender: TObject);
var
  i: integer;
  Button: TSpeedButton;
  TabName: string;
  Num: Integer;
begin
  FButtonDown := False;
  Palette.FirstVisible := 0;
  if TabControl.TabIndex < 0 then Exit;
  TabName := TabControl.Tabs[TabControl.TabIndex];
  Num := 0;
	for i := 0 to ComponentList.Count - 1 do
		if Cmp(TPaletteEntry(ComponentList[i]).FPage, TabName) then
      inc(Num);

  Palette.BeginUpdate;
  try
    Palette.ButtonCount := Num;
    Num := 0;
    for i := 0 to ComponentList.Count - 1 do
    begin
      with TPaletteEntry(ComponentList[i]) do
      if Cmp(FPage, TabName) then
      begin
       // ComponentPanel.ButtonCount := ComponentPanel.ButtonCount + 1;
        Button := Palette.Buttons[Num];
        LoadBitmap(Button.Glyph);
        Button.Hint := ComponentClass.ClassName;
        if (Length(Button.Hint) > 1) and (Button.Hint[1] = 'T') then
          Button.Hint := Copy(Button.Hint, 2, Length(Button.Hint));
        Button.Tag := Integer(ComponentList[i]);
        inc(Num);
      end;
    end;
  finally { wrap up }
    Palette.EndUpdate;
  end;    { try/finally }
end;

procedure TRAFDPalette.PaletteClick(Sender: TObject;
  Button: Integer);
begin
  FButtonDown := Button > -1;
  if ButtonDown then
    FComponentClass := TPaletteEntry(Palette.Buttons[Button].Tag).FComponentClass;
end;

procedure TRAFDPalette.WMGetMinMaxInfo(var M : TWMGetMinMaxInfo);
begin
  inherited;
  if Visible then
  begin
    if Minim then
      M.MinMaxInfo^.ptMinTrackSize.Y := Height - ClientHeight + TabControl.Top
    else
      M.MinMaxInfo^.ptMinTrackSize.Y := Height - ClientHeight + TabControl.Top + TabControl.Height;
    M.MinMaxInfo^.ptMaxTrackSize.Y := M.MinMaxInfo^.ptMinTrackSize.Y;
    M.MinMaxInfo^.ptMinTrackSize.X := 80;
  end;
end;

procedure TRAFDPalette.RACaptionButton1Click(Sender: TObject);
begin
  Minim := not Minim;
  if ClientHeight = 0 then
    ClientHeight := TabControl.Top + TabControl.Height
  else
    ClientHeight := 0;
end;

procedure TRAFDPalette.FormResize(Sender: TObject);
begin
  TabControl.Width := ClientWidth;
end;

procedure TRAFDPalette.FormShow(Sender: TObject);
begin
  Height := TabControl.Top + TabControl.Height;
end;

procedure TRAFDPalette.PackageUnloaded(const Module: HModule);
var
  i: Integer;
  Entry: TPaletteEntry;
begin
  i := 0;
  while i < ComponentList.Count - 1 do
  begin
    Entry := TPaletteEntry(ComponentList[i]);
    if Entry.FModule = Module then
    begin
      Entry.Free;
      ComponentList.Delete(i);
    end
    else
      Inc(i);
  end;
end;


{ TPaletteEntry }

procedure TPaletteEntry.LoadBitmap(Bitmap: TBitmap);
begin
  try
    Bitmap.LoadFromResourceName(FModule, FComponentClass.ClassName);
  except
    Bitmap.LoadFromResourceName(hInstance, PChar(deDefaultComponentBitmap));
  end;
end;

{ TPaletteComponentList }

function TPaletteComponentList.GetItem(Index: Integer): TPaletteEntry;
begin
  Result := TPaletteEntry(inherited Items[Index]);
end;


{************************* Packages *************************}

procedure PackageInfoProc(const Name: string; NameType: TNameType; Flags: Byte;
  Param: Pointer);
var
  Proc: procedure;
begin
  if NameType = ntContainsUnit then
  begin
   {$IFDEF RA_D3}
    Proc := GetProcAddress(TDesignPackage(Param).FModule,
      PChar(Name + '.Register@51F89FF7'));
   {$ELSE}
    Proc := GetProcAddress(TDesignPackage(Param).FModule,
      PChar('@' + ANSIUpperCase(Name)[1] +
      ANSILowerCase(Copy(Name, 2, Length(Name))) + '@Register$qqrv'));
   {$ENDIF}
    if Assigned(Proc) then
      try
        ODS('  found Register function in unit ' + Name);
        Proc;
      except
      end;
  end;
end;

procedure TDesignPackageList.Add(const FileName: TFileName;
  const Load, IDEPackage: Boolean; const Description: string);
var
  PackageRec: TDesignPackage;
begin
  ODS('Add package ' + FileName);
  PackageRec := TDesignPackage.Create;
  PackageRec.FFileName := FileName;
  PackageRec.FIDEPackage := IDEPackage;
  PackageRec.FDescription := DefStr(Description, FileName);
  inherited Add(PackageRec);
  if Load then
    LoadPackage(Count - 1);
end;    { Add }

procedure TDesignPackageList.Delete(const Index: Integer);
begin
  UnloadPackage(Index);
  Items[Index].Free;
  inherited Delete(Index);
end;    { Delete }

procedure TDesignPackageList.LoadPackage(const Index: Integer);
var
  PackageRec: TDesignPackage;
  Flags: Integer;
begin
  PackageRec := TDesignPackage(Items[Index]);
  ODS('Loading package ' + PackageRec.FFileName);
  PackageRec.FEditorGroup := NewEditorGroup;
  try
    PackageRec.FModule := SysUtils.LoadPackage(PackageRec.FileName);
    PackageRec.FDescription := GetPackageDescription(PChar(PackageRec.FileName));
    SetLength(PackageRec.FFileName, 260);
    SetLength(PackageRec.FFileName, GetModuleFileName(PackageRec.FModule,
      PChar(PackageRec.FFileName), 260));
  except
    FreeEditorGroup(PackageRec.FEditorGroup);
    raise;
  end;
	ActiveModule := PackageRec.FModule;
	GetPackageInfo(PackageRec.FModule, PackageRec, Flags, PackageInfoProc);
  PackageRec.FLoaded := True;
  if ComponentPalette <> nil then
    ComponentPalette.UpdatePalette;
end;    { LoadPackage }

procedure TDesignPackageList.UnloadPackage(const Index: Integer);
var
  PackageRec: TDesignPackage;
begin
  PackageRec := TDesignPackage(Items[Index]);
  if AppBuilder <> nil then
    AppBuilder.HideDesigners;
  if ComponentPalette <> nil then
    ComponentPalette.PackageUnloaded(PackageRec.FModule);
  if PackageRec.FLoaded then
  begin
    ODS('Unloading package ' + PackageRec.FFileName);
    FreeEditorGroup(PackageRec.FEditorGroup);
    SysUtils.UnloadPackage(PackageRec.FModule);
    PackageRec.FLoaded := False;
  end;
  if ComponentPalette <> nil then
    ComponentPalette.UpdatePalette;
end;    { UnloadPackage }

destructor TDesignPackageList.Destroy;
begin
  while Count > 0 do
    Delete(Count - 1);
  inherited Destroy;
end;    { Destroy }

function TDesignPackageList.GetItem(Index: Integer): TDesignPackage;
begin
  Result := TDesignPackage(inherited Items[Index]);
end;    { GetItem }


initialization
  ComponentList := TPaletteComponentList.Create;
  NoIconList := TPaletteComponentList.Create;
  RegisterComponentsProc := RARegisterComponentsProc;
  RegisterNonActiveXProc := RARegisterNonActiveXProc;
  RegisterNoIconProc := RARegisterNoIconProc;
  RegisterCustomModuleProc := RARegisterCustomModuleProc;
  FreeCustomModulesProc := RAFreeCustomModulesProc;
  CreateVCLComObjectProc := RACreateVCLComObjectProc;
  DesignPackageList := TDesignPackageList.Create;
finalization
  Free;
  ClearList(ComponentList);
  ComponentList.Free;
  NoIconList.Free;
  DesignPackageList.Free;
end.

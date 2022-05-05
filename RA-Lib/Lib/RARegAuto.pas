{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov
       Portion copyright (C) Sergey Orlik

       component   : RARegAuto
       description : registry and ini-file storage
                     for other components properties

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm.htm
************************************************************}

{$INCLUDE RA.INC}

unit RARegAuto;
                
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  TypInfo, Registry, IniFiles
  {$IFDEF RA_D3H} , RegStr{$ENDIF RA_D3H} ;

type

  TIniStrings = class;
  TMyIniFile = class;
  TRegAutoOperation = (roBeforeLoad, roAfterLoad, roBeforeSave, roAfterSave);
  TRegAutoEvent = procedure (Sender : TObject; Operation : TRegAutoOperation) of object;

  TRegAuto = class(TComponent)
  private
    ObjProp : string;
    TypeInf : PTypeInfo;
    PropInf : PPropInfo;
    PropTyp : TTypeKind;
    Obj : TObject;
    ComponentName, PropertyName : string;
    FLoaded : boolean;
    FProps : TStrings;
    FAutoMode : boolean;
    FSaveWindowPlace : boolean;

    FUseIni : boolean;
    FUseReg : boolean;
    FUseStr : boolean;

    FRegPath : string;
    FIniFile : string;
    FIniStrings : TStrings;
    FGlobalSettings : boolean;
    FSection: string;

    FormOnCreate    : TNotifyEvent;
    FormOnDestroy   : TNotifyEvent;

    FNotifiers : TList;

    FBeforeLoad : TNotifyEvent;
    FBeforeSave : TNotifyEvent;
    FAfterLoad  : TNotifyEvent;
    FAfterSave  : TNotifyEvent;

    OldIniFile : string;
    OldRegPath : string;

    procedure LoadPropInf(lObjProp : string);
    function GetOrdPrp : longint;
    procedure SetOrdPrp(Value : longint);
    function GetStrPrp : string;
    procedure SetStrPrp(Value : string);
    function GetFloatPrp : extended;
    procedure SetFloatPrp(Value : extended);
    {************ Для Property ************}
    procedure SetFProps(lProps : TStrings);
    procedure SetSaveWindowPlace(F : boolean);
    procedure SetIniStrings(AIniStrings : TStrings);
    {############ Для Property ############}
    procedure NewFormOnCreate(Sender : TObject);
    procedure NewFormOnDestroy(Sender : TObject);
    procedure GenerateRegistryName;
  protected
    Reg : TRegIniFile;
    Ini : TMyIniFile;
    Str : TIniStrings;
    procedure CreateFile;
    procedure DestroyFile;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    procedure SaveWindowPlacement;
    procedure LoadWindowPlacement;

    function ReadRootString(const Section, Ident, Default: string): string;
    function ReadRootInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteRootString(const Section, Ident, Value: string);
    procedure WriteRootInteger(const Section, Ident: string; Value: Longint);

    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    procedure ReadStrings(const Section, Ident : string; Strings : TStrings);
    procedure WriteStrings(const Section, Ident: string; Value: TStrings);
    procedure ReadSection(const Section : string; Ss : TStrings);
    procedure ReadSectionValues(const Section: string; Ss: TStrings);
    procedure ReadSections(Ss : TStrings);
    procedure ReadWholeSection(const Section : string; Ss : TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: string);

    procedure AddNotify(ANotify : TRegAutoEvent);
    procedure RemoveNotify(ANotify : TRegAutoEvent);
  published
    // Путь в реестре
    property RegPath : string read FRegPath write FRegPath;
    // Имя ini-файла
    property IniFile : string read FIniFile write FIniFile;
    //
    property IniStrings : TStrings read FIniStrings write SetIniStrings;

    // Список сохраняемых свойств
    property Props : TStrings read FProps write SetFProps;
    property AutoMode : boolean read FAutoMode write FAutoMode default true;
    // Сохранять размер и положение окна
    property SaveWindowPlace : boolean read FSaveWindowPlace write SetSaveWindowPlace;
    property BeforeLoad : TNotifyEvent read FBeforeLoad write FBeforeLoad;
    property AfterLoad  : TNotifyEvent read FAfterLoad  write FAfterLoad;
    property BeforeSave : TNotifyEvent read FBeforeSave write FBeforeSave;
    property AfterSave  : TNotifyEvent read FAfterSave  write FAfterSave;
    property UseIni : boolean read FUseIni write FUseIni default false;
    property UseReg : boolean read FUseReg write FUseReg default true;
    property UseStr : boolean read FUseStr write FUseStr default false;
    property GlobalSettings : boolean read FGlobalSettings write FGlobalSettings default true;
    property Section: string read FSection write FSection;
  end;

  TIniStrings = class
  private
    FStrings : TStrings;
  public
    constructor Create(AStrings : TStrings);

    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    function ReadSection(const Section : string; Ss : TStrings) : boolean;
    procedure ReadSectionValues(const Section: string; Ss: TStrings);
    procedure ReadWholeSection(const Section : string; Ss : TStrings);
    procedure ReadSections(Ss : TStrings);
  end;

  TMyIniFile = class(TIniFile)
  public
    procedure ReadWholeSection(const Section : string; Ss : TStrings);
   {$IFNDEF RA_D35H}
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
   {$ENDIF RA_D35H}
  end;

  ERegAutoError = class(Exception);

var
  GlobalIniFile : string = ''; {if <> '', used by all RegAutos}
  GlobalRegPath : string = ''; {if <> '', used by all RegAutos}

implementation

uses RAUtils, RADsgnIntf;

{$IFDEF RA_D2}
const
  REGSTR_PATH_SETUP           = 'Software\Microsoft\Windows\CurrentVersion';
  REGSTR_VAL_REGORGANIZATION  = 'RegisteredOrganization';
{$ENDIF RA_D2}


constructor TRegAuto.Create(AOwner: TComponent);
var
  ProjName : string;
begin
  inherited Create(AOwner);
  FNotifiers := TList.Create;
  UseReg := true;
  FGlobalSettings := true;
  FProps := TStringList.Create;
  FIniStrings := TStringList.Create;
  ProjName := '';
  if (csDesigning in ComponentState) and Assigned(GetProjectNameProc) then
  begin
    ProjName := GetProjectNameProc;
    ProjName := ExtractFileName(ProjName);
    ProjName := Copy(ProjName, 1, Length(ProjName)-4);
    if ProjName = '' then ProjName := 'Проба';
    with TRegIniFile.Create('') do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        FRegPath := 'Software\' + ReadString(REGSTR_PATH_SETUP, REGSTR_VAL_REGORGANIZATION, '') +'\'+ ProjName;
      finally
        Reg.Free;
      end;
  end else
    FRegPath := 'Software\Unknown Delphi Application';
  FAutoMode := true;
  FSaveWindowPlace := false;
end;

destructor TRegAuto.Destroy;
begin
  FProps.Free;
  FIniStrings.Free;
  FNotifiers.Free;
  inherited Destroy;
end;

procedure TRegAuto.Loaded;

  function EqualAddr(Addr1, Addr2: TNotifyEvent): Boolean;
  begin
    Result := CompareMem(@TMethod(Addr1), @TMethod(Addr2), 8);
  end;

begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and FAutoMode then
    if (Owner is TForm) and
        not EqualAddr((Owner as TForm).OnCreate, NewFormOnCreate) then
    begin
      FormOnCreate := (Owner as TForm).OnCreate;
      (Owner as TForm).OnCreate := NewFormOnCreate;
      FormOnDestroy := (Owner as TForm).OnDestroy;
      (Owner as TForm).OnDestroy := NewFormOnDestroy;
    end else if (Owner is TDataModule) and
        not EqualAddr((Owner as TDataModule).OnCreate, NewFormOnCreate) then
    begin
      FormOnCreate := (Owner as TDataModule).OnCreate;
      (Owner as TDataModule).OnCreate := NewFormOnCreate;
      FormOnDestroy := (Owner as TDataModule).OnDestroy;
      (Owner as TDataModule).OnDestroy := NewFormOnDestroy;
    end;
end;    { Loaded }

procedure TRegAuto.NewFormOnCreate(Sender : TObject);
begin
  if Assigned(FormOnCreate) then FormOnCreate(Self);
  if AutoMode then Load;
end;

procedure TRegAuto.NewFormOnDestroy(Sender : TObject);
begin
  if AutoMode then Save;
  if Assigned(FormOnDestroy) then FormOnDestroy(Self);
end;

procedure TRegAuto.LoadPropInf(lObjProp : string);
var
  PropName : string;
  i        : integer;
  Last     : boolean;
begin
  FLoaded := false;
  ObjProp := lObjProp;
  if Length(ObjProp) < 2 then exit;
  ComponentName := Copy(ObjProp, 1, Pos('.', ObjProp)-1);
  PropertyName  := Copy(ObjProp, Pos('.', ObjProp)+1, 255);
  if ComponentName = Owner.Name then begin
    ComponentName := '';
    ObjProp := Copy(ObjProp, Pos('.', ObjProp), 255);
  end;
  if ComponentName = '' then Obj := Owner
  else Obj := Owner.FindComponent(ComponentName);
  if Obj = nil then exit;
  
  // Ищем вложенные свойства
  i := Pos('.', ObjProp);
  Last := false;
  while not Last do begin
    PropName := '';
    inc(i);
    while i <= Length(ObjProp) do begin
      if ObjProp[i] = '.' then break;
      PropName := PropName + ObjProp[i];
      inc(i);
    end;
    Last := not ((Length(ObjProp) >= i) and (ObjProp[i] = '.'));
    TypeInf := Obj.ClassInfo;
    PropInf := GetPropInfo(TypeInf, PropName);
    if PropInf = nil then exit;
    PropTyp := PropInf^.PropType^.Kind;
    if Obj = nil then exit;
    if PropTyp = tkClass then begin
      FLoaded := true;
      Obj := TObject(GetOrdPrp);
      FLoaded := false;
    end
    else break; // Loop End
  end;
  FLoaded := true;
end;

procedure TRegAuto.GenerateRegistryName;
begin
  if FSection = '' then
    ComponentName := Owner.Name
  else
    ComponentName := FSection;
  PropertyName  := ObjProp;
end;

function TRegAuto.GetOrdPrp : longint;
begin
  Result := 0;
  case PropTyp of
    tkInteger,
    tkChar,
    tkWChar,
    tkClass,
    tkEnumeration :
      if FLoaded then Result:= GetOrdProp(Obj, PropInf);
  end;
end;

procedure TRegAuto.SetOrdPrp(Value : longint);
begin
  case PropTyp of
    tkInteger,
    tkChar,
    tkWChar,
    tkEnumeration :
      if FLoaded then SetOrdProp(Obj, PropInf, Value);
  end;
end;

function TRegAuto.GetStrPrp : string;
begin
  Result := '';
  case PropTyp of
    tkString,
    tkLString{$IFDEF RA_D3H},
    tkWString{$ENDIF RA_D3H} :
      if FLoaded then Result:= GetStrProp(Obj, PropInf);
  end;
end;

procedure TRegAuto.SetStrPrp(Value : string);
begin
  case PropTyp of
    tkString,
    tkLString{$IFDEF RA_D3H},
    tkWString{$ENDIF RA_D3H} :
      if FLoaded then SetStrProp(Obj, PropInf, Value);
  end;
end;

function TRegAuto.GetFloatPrp : extended;
begin
  Result := 0;
  case PropTyp of
    tkFloat :
      if FLoaded then Result:= GetFloatProp(Obj, PropInf);
  end;
end;

procedure TRegAuto.SetFloatPrp(Value : extended);
begin
  case PropTyp of
    tkFloat :
      if FLoaded then SetFloatProp(Obj, PropInf, Value);
  end;
end;

procedure TRegAuto.Save;

  procedure SaveOrdPrp;
  var
    Value : longint;
  begin
    Value := GetOrdPrp;
    WriteInteger(ComponentName, PropertyName, Value);
  end;
  procedure SaveFloatPrp;
  var
    Value : extended;
  begin
    Value := GetFloatPrp;
    WriteFloat(ComponentName, PropertyName, Value);
  end;
  procedure SaveStrPrp;
  var
    Value : string;
  begin
    Value := GetStrPrp;
    WriteString(ComponentName, PropertyName, Value);
  end;

var
  i : integer;
begin
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roBeforeSave);
  if Assigned(FBeforeSave) then FBeforeSave(Self);
 try
  for i:= 0 to FProps.Count - 1 do begin
    LoadPropInf(FProps[i]);
    if not FLoaded then continue;
    GenerateRegistryName;
    case PropTyp of
      tkInteger,
      tkChar,
      tkWChar,
      tkEnumeration : SaveOrdPrp;
      tkFloat       : SaveFloatPrp;
      tkString,
      tkLString{$IFDEF RA_D3H},
      tkWString{$ENDIF RA_D3H}    : SaveStrPrp;
    end;
  end;
  if SaveWindowPlace then SaveWindowPlacement;
 except
   Application.MessageBox(PChar('TRegAuto: проблемы c ' + ObjProp)
     , PChar(Application.Title), MB_OK + MB_ICONERROR);
 end;
  if Assigned(FAfterSave) then FAfterSave(Self);
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roAfterSave);
end;

procedure TRegAuto.Load;

  procedure LoadOrdPrp;
  var
    Value : longint;
  begin
    Value := GetOrdPrp;
    Value := ReadInteger(ComponentName, PropertyName, Value);
    SetOrdPrp(Value);
  end;
  procedure LoadFloatPrp;
  var
    Value : extended;
  begin
    Value := GetFloatPrp;
    Value := ReadFloat(ComponentName, PropertyName, Value);
    SetFloatPrp(Value);
  end;
  procedure LoadStrPrp;
  var
    Value : string;
  begin
    Value := GetStrPrp;
    Value := ReadString(ComponentName, PropertyName, Value);
    SetStrPrp(Value);
  end;

var
  i : integer;
begin
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roBeforeLoad);
  if Assigned(FBeforeLoad) then FBeforeLoad(Self);
 try
  if SaveWindowPlace then LoadWindowPlacement;
  for i:= 0 to FProps.Count - 1 do begin
    LoadPropInf(FProps[i]);
    if not FLoaded then continue;
    GenerateRegistryName;
    case PropTyp of
      tkInteger,
      tkChar,
      tkWChar,
      tkEnumeration : LoadOrdPrp;
      tkFloat       : LoadFloatPrp;
      tkString,
      tkLString{$IFDEF RA_D3H},
      tkWString{$ENDIF RA_D3H}     : LoadStrPrp;
    end;
  end;
 except
   on E : Exception do begin
     E.Message := 'Проблемы c TRegAuto' + E.Message;
     raise ERegAutoError.Create(E.Message);
   end;
//   Application.MessageBox('Проблемы c TRegAuto', PChar(Application.Title), MB_OK + MB_ICONERROR);
 end;
  if Assigned(FAfterLoad) then FAfterLoad(Self);
  for i := 0 to FNotifiers.Count - 1 do
    TRegAutoEvent(FNotifiers[i]^)(Self, roAfterLoad);
end;

procedure TRegAuto.LoadWindowPlacement;
var
  W : TWINDOWPLACEMENT;
  Form : TForm;
const
  Vis : array[boolean] of integer = (SW_HIDE, SW_SHOW);
begin
  Form := Owner as TForm;
  W.length := sizeof(TWINDOWPLACEMENT);
  GetWindowPlacement(Form.Handle, @W);
  W.showCmd := Vis[Form.Visible];
  GenerateRegistryName;
  with W.rcNormalPosition do begin
    Left   := ReadInteger(ComponentName, '.Left', Form.Left);
    Top    := ReadInteger(ComponentName, '.Top', Form.Top);
    if Form.BorderStyle in [bsSizeable, bsSizeToolWin] then
    begin
      Right  := ReadInteger(ComponentName, '.Right', Right);
      Bottom := ReadInteger(ComponentName, '.Bottom', Bottom);
    end else
    begin
      Right  := Left + Form.Width;
      Bottom := Top + Form.Height;
    end;
  end;
  SetWindowPlacement(Form.Handle, @W);
  Form.WindowState := TWindowState(ReadInteger(ComponentName, '.WindowState', integer(Form.WindowState)));
end;

procedure TRegAuto.SaveWindowPlacement;
var
  W : TWINDOWPLACEMENT;
  Form : TForm;
begin
  Form := Owner as TForm;
  W.length := sizeof(TWINDOWPLACEMENT);
  GetWindowPlacement(Form.Handle, @W);
  GenerateRegistryName;
  with W.rcNormalPosition do begin
    WriteInteger(ComponentName, '.Left', Left);
    WriteInteger(ComponentName, '.Top', Top);
    WriteInteger(ComponentName, '.Right', Right);
    WriteInteger(ComponentName, '.Bottom', Bottom);
  end;
  WriteInteger(ComponentName, '.WindowState', integer(Form.WindowState));
end;

procedure TRegAuto.SetFProps(lProps : TStrings);
begin
  FProps.Assign(lProps);
end;

function TRegAuto.ReadRootString(const Section, Ident, Default: string): string;
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  Result := RegIni1.ReadString(Section, Ident, Default);
  RegIni1.Free;
end;

function TRegAuto.ReadRootInteger(const Section, Ident: string; Default: Longint): Longint;
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  Result := RegIni1.ReadInteger(Section, Ident, Default);
  RegIni1.Free;
end;

procedure TRegAuto.WriteRootString(const Section, Ident, Value: string);
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  RegIni1.WriteString(Section, Ident, Value);
  RegIni1.Free;
end;

procedure TRegAuto.WriteRootInteger(const Section, Ident: string; Value: Longint);
var
  RegIni1 : TRegIniFile;
begin
  RegIni1 := TRegIniFile.Create('');
  RegIni1.WriteInteger(Section, Ident, Value);
  RegIni1.Free;
end;

procedure TRegAuto.SetSaveWindowPlace(F : boolean);
begin
  if Owner is TWinControl then FSaveWindowPlace := F;
end;

procedure TRegAuto.SetIniStrings(AIniStrings : TStrings);
begin
  IniStrings.Assign(AIniStrings);
end;

{**************************************************}
procedure TRegAuto.CreateFile;
begin
  OldIniFile := FIniFile;
  OldRegPath := FRegPath;
  if FGlobalSettings then begin
    if GlobalIniFile <> '' then FIniFile := GlobalIniFile;
    if GlobalRegPath <> '' then FRegPath := GlobalRegPath;
  end;
  if FUseIni then Ini := TMyIniFile.Create(AddPath(FIniFile, ExePath));
  if FUseReg then Reg := TRegIniFile.Create(FRegPath);
  if FUseStr then Str := TIniStrings.Create(FIniStrings);
end;

procedure TRegAuto.DestroyFile;
begin
  Reg.Free; Reg := nil;
  Ini.Free; Ini := nil;
  Str.Free; Str := nil;
  if FGlobalSettings then begin
    FIniFile := OldIniFile;
    FRegPath := OldRegPath;
  end;  
end;

procedure TRegAuto.EraseSection(const Section: string);
begin
  CreateFile;
  if FUseIni then Ini.EraseSection(FSection + Section);
  if FUseReg then Reg.EraseSection(FSection + Section);
  DestroyFile;
end;

procedure TRegAuto.DeleteKey(const Section, Ident: String);
begin
  CreateFile;
  if FUseIni then Ini.DeleteKey(FSection + Section, Ident);
  if FUseReg then Reg.DeleteKey(FSection + Section, Ident);
  DestroyFile;
end;

function TRegAuto.ReadString(const Section, Ident, Default: string): string;
begin
  Result := Default;
  CreateFile;
  if FUseIni then Result := Ini.ReadString(FSection + Section, Ident, Default);
  if FUseReg then Result := Reg.ReadString(FSection + Section, Ident, Default);
  if FUseStr then Result := Str.ReadString(FSection + Section, Ident, Default);
  DestroyFile;
end;

procedure TRegAuto.WriteString(const Section, Ident, Value: string);
begin
  CreateFile;
  if FUseIni then Ini.WriteString(FSection + Section, Ident, Value);
  if FUseReg then Reg.WriteString(FSection + Section, Ident, Value);
  if FUseStr then Str.WriteString(FSection + Section, Ident, Value);
  DestroyFile;
end;

function TRegAuto.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
begin
  Result := Default;
  CreateFile;
  if FUseIni then Result := Ini.ReadInteger(FSection + Section, Ident, Default);
  if FUseReg then Result := Reg.ReadInteger(FSection + Section, Ident, Default);
  if FUseStr then Result := Str.ReadInteger(FSection + Section, Ident, Default);
  DestroyFile;
end;

procedure TRegAuto.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  CreateFile;
  if FUseIni then Ini.WriteInteger(FSection + Section, Ident, Value);
  if FUseReg then Reg.WriteInteger(FSection + Section, Ident, Value);
  if FUseStr then Str.WriteInteger(FSection + Section, Ident, Value);
  DestroyFile;
end;

function TRegAuto.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := Default;
  CreateFile;
  if FUseIni then Result := Ini.ReadBool(FSection + Section, Ident, Default);
  if FUseReg then Result := Reg.ReadBool(FSection + Section, Ident, Default);
  if FUseStr then Result := Str.ReadBool(FSection + Section, Ident, Default);
  DestroyFile;
end;

procedure TRegAuto.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  CreateFile;
  if FUseIni then Ini.WriteBool(FSection + Section, Ident, Value);
  if FUseReg then Reg.WriteBool(FSection + Section, Ident, Value);
  if FUseStr then Str.WriteBool(FSection + Section, Ident, Value);
  DestroyFile;
end;

function TRegAuto.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  Result := Default;
  CreateFile;
  if FUseIni then Result := Ini.ReadFloat(FSection + Section, Ident, Default);
  if FUseReg then Result := StrToFloat(Reg.ReadString(FSection + Section, Ident, FloatToStr(Default)));
  if FUseStr then Result := Str.ReadFloat(FSection + Section, Ident, Default);
  DestroyFile;
end;

procedure TRegAuto.WriteFloat(const Section, Ident: string; Value: Double);
begin
  CreateFile;
  if FUseIni then Ini.WriteFloat(FSection + Section, Ident, Value);
  if FUseReg then Reg.WriteString(FSection + Section, Ident, FloatToStr(Value));
  if FUseStr then Str.WriteFloat(FSection + Section, Ident, Value);
  DestroyFile;
end;

procedure TRegAuto.ReadStrings(const Section, Ident : string; Strings : TStrings);
var
  S : string;
begin
  S := Strings.Text;
  S := ReplaceString(S, #13#10, '|');
  S := ReadString(FSection + Section, Ident, S);
  S := ReplaceString(S, '|', #13#10);
  Strings.Text := S;
end;

procedure TRegAuto.WriteStrings(const Section, Ident: string; Value: TStrings);
var
  S : string;
begin
  S := Value.Text;
  S := ReplaceString(S, #13#10, '|');
  WriteString(FSection + Section, Ident, S);
end;

procedure TRegAuto.ReadSection(const Section : string; Ss : TStrings);
begin
  CreateFile;
  if FUseIni then Ini.ReadSection(FSection + Section, Ss);
  if FUseReg then Reg.ReadSection(FSection + Section, Ss);
  if FUseStr then Str.ReadSection(FSection + Section, Ss);
  DestroyFile;
end;

procedure TRegAuto.ReadSectionValues(const Section: string; Ss: TStrings);
begin
  CreateFile;
  if FUseIni then Ini.ReadSectionValues(FSection + Section, Ss);
  if FUseReg then Reg.ReadSectionValues(FSection + Section, Ss);
  if FUseStr then Str.ReadSectionValues(FSection + Section, Ss);
  DestroyFile;
end;

procedure TRegAuto.ReadWholeSection(const Section : string; Ss : TStrings);
begin
  CreateFile;
  if FUseIni then Ini.ReadWholeSection(FSection + Section, Ss);
  if FUseReg then Reg.ReadSection(FSection + Section, Ss); { ReadWholeSection not supported for registry }
  if FUseStr then Str.ReadWholeSection(FSection + Section, Ss);
  DestroyFile;
end;

procedure TRegAuto.ReadSections(Ss : TStrings);
begin
  CreateFile;
  if FUseIni then Ini.ReadSections(Ss);
  if FUseReg then Reg.ReadSections(Ss);
  if FUseStr then Str.ReadSections(Ss);
  DestroyFile;
end;

procedure TRegAuto.AddNotify(ANotify : TRegAutoEvent);
var
  Notify : ^TRegAutoEvent;
begin
  New(Notify);
  Notify^ := ANotify;
  FNotifiers.Add(Notify);
end;

procedure TRegAuto.RemoveNotify(ANotify : TRegAutoEvent);
var
  i : Integer;
  Notify : ^TRegAutoEvent;
begin
  for i := 0 to FNotifiers.Count - 1 do
  begin
    Notify := FNotifiers[i];
    if (TMethod(Notify^).Code = TMethod(ANotify).Code) and
      (TMethod(Notify^).Data = TMethod(ANotify).Data) then
    begin
      Dispose(Notify);
      FNotifiers.Delete(i);
      break;
    end;
  end;
end;


{********************* TMyIniFile **********************}
procedure TMyIniFile.ReadWholeSection(const Section : string; Ss : TStrings);
var
  TmpSS: TStringList;
  TmpIniSS: TIniStrings;
begin
  TmpSS := TStringList.Create;
  try
    TmpSS.LoadFromFile(FileName);
    TmpIniSS := TIniStrings.Create(TmpSS);
    try
      TmpIniSS.ReadWholeSection(Section, SS);
    finally
      TmpIniSS.Free;
    end;
  finally
    TmpSS.Free;
  end;
end;

{$IFNDEF RA_D35H}
function TMyIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  Result := StrToFloat(ReadString(Section, Ident, FloatToStr(Default)));
end;

procedure TMyIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value));
end;
{$ENDIF RA_D35H}

{##################### TMyIniFile #####################}

{********************* TIniStrings **********************}
constructor TIniStrings.Create(AStrings : TStrings);
begin
  inherited Create;
  FStrings := AStrings;
end;

function TIniStrings.ReadString(const Section, Ident, Default: string): string;
var
  i : integer;
  S : string;
  P : integer;
begin
  Result := Default;
  i := FStrings.IndexOf('['+Section+']');
  if i = -1 then exit;
  inc(i);
  while i < FStrings.Count do begin
    S := FStrings[i];
    inc(i);
    if Length(S) = 0 then continue;
    if S[1] = '[' then exit;
    if ANSIStrLIComp(PChar(Ident), PChar(S), Length(Ident)) = 0 then begin
      P := Pos('=', S);
      if P <> 0 then
        Result := Copy(S, P+1, Length(S));
      exit;
    end;
  end;
end;

procedure TIniStrings.WriteString(const Section, Ident, Value: string);
var
  F: integer;
  S: string;
begin
  FStrings.BeginUpdate;
  F := FStrings.IndexOf('[' + Section + ']');
  if F > -1 then
  begin
    Inc(F);
    while F < FStrings.Count do
    begin
      S := Trim(FStrings[F]);
      if ((Length(S) > 0) and (Trim(S[1]) = '[')) or (Trim(S) = '') then
      begin
        FStrings.Insert(F, Ident + '=' + Value);
        Break;
      end
      else
        if UpperCase(Copy(S, 1, Pos('=', S) - 1)) = UpperCase(Ident) then
        begin
          FStrings[F] := Ident + '=' + Value;
          Break;
        end;
      Inc(F);
    end;
    if F >= FStrings.Count then FStrings.Add(Ident + '=' + Value);
  end
  else
  begin
    FStrings.Add('');
    FStrings.Add('[' + Section + ']');
    FStrings.Add(Ident + '=' + Value);
  end;
  FStrings.EndUpdate;
end;

function TIniStrings.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
begin
  try
    Result := StrToInt(ReadString(Section, Ident, IntToStr(Default)));
  except
    Result := Default;
  end;
end;

procedure TIniStrings.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TIniStrings.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
var
  S : string;
begin
  S := Trim(ReadString(Section, Ident, IntToStr(integer(Default))));
  Result := (S = '1') or (ANSICompareText(S, 'on') = 0)  or (ANSICompareText(S, 'yes') = 0);
end;

procedure TIniStrings.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, IntToStr(integer(Value)));
end;

function TIniStrings.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  try
    Result := StrToFloat(ReadString(Section, Ident, FloatToStr(Default)));
  except
    Result := Default;
  end;
end;

procedure TIniStrings.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value));
end;

function TIniStrings.ReadSection(const Section : string; Ss : TStrings) : boolean;
var
  F : integer;
  S : string;

  procedure ReadSection1;
  begin
		inc(F);
		while F < FStrings.Count do begin
			S := FStrings[F];
			if (Length(S) > 0) and (Trim(S[1])= '[') then break;
      if Trim(S) <> '' then
  			Ss.Add(S);
			inc(F);
		end;
  end;    { ReadSection1 }

begin
  Ss.BeginUpdate;
  try
    Ss.Clear;
    F := FStrings.IndexOf('['+Section+']');
    Result := F > -1;
    if Result then
    begin
      ReadSection1;
      while F < FStrings.Count do
      begin
        S := Trim(FStrings[F]);
        if S = '['+Section+']' then
          ReadSection1
        else
          inc(F);
      end;    { while }
    end;
	finally
		Ss.EndUpdate;
	end;
end;

procedure TIniStrings.ReadSections(Ss : TStrings);
var
  i : integer;
  S : string;
begin
  Ss.Clear;
  for i := 0 to FStrings.Count - 1 do
  begin
    S := Trim(FStrings[i]);
    if (Length(S) > 0) and (S[1]= '[') and (S[Length(S)]= ']') then
      Ss.Add(Copy(S, 2, Length(S) - 2));
  end;
end;

procedure TIniStrings.ReadSectionValues(const Section: string; Ss: TStrings);
var
  F: integer;
  S: string;

  procedure ReadSection1;
  begin
    inc(F);
    while F < FStrings.Count do
    begin
      S := FStrings[F];
      if (Length(S) > 0) and (Trim(S[1]) = '[') then break;
      if Trim(S) <> '' then
        Ss.Add(S);
      inc(F);
    end;
  end; { ReadSection1 }

begin
  Ss.BeginUpdate;
  try
    Ss.Clear;
    F := FStrings.IndexOf('[' + Section + ']');
    if F > -1 then
    begin
      ReadSection1;
      while F < FStrings.Count do
      begin
        S := Trim(FStrings[F]);
        if S = '[' + Section + ']' then
          ReadSection1
        else
          inc(F);
      end; { while }
    end;
  finally
    Ss.EndUpdate;
  end;
end;

procedure TIniStrings.ReadWholeSection(const Section : string; Ss : TStrings);
var
  F : integer;
  S : string;
begin
  with FStrings do
  begin
  	F := IndexOf('['+Section+']');
  //	Result := F > -1;
  	if F > -1 then begin
  		Ss.BeginUpdate;
  		try
  			Ss.Clear;
  			inc(F);
  			while F < Count do begin
  				S := Strings[F];
  				if (Length(S) > 0) and (Trim(S[1])= '[') then break;
  				Ss.Add(S);
  				inc(F);
  			end;
  		finally
  			Ss.EndUpdate;
  		end;
  	end;
  end;  
end;



end.


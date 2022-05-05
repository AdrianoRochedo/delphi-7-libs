unit psBase;

{
  AUTOR .............................. Adriano Rochedo Conceição
}

interface
uses Windows,
     Classes,
     Contnrs,
     Lists,
     SysUtils,
     SysUtilsEx;

  procedure setGlobalProgressBar(PB: IProgressBar);
  function getGlobalProgressBar(): IProgressBar;

const
  // Categorias
  cCatConversao        = 'Conversão de Valores';
  cCatSistema          = 'Sistema';
  cCatDialogo          = 'Diálogos';
  cCatMatematica       = 'Matemática e Trigonometria';
  cCatString           = 'Strings';
  cCatFile             = 'Arquivos';
  cCatPlanilhas        = 'Planilhas';
  cCatListas           = 'Listas';

type
  TVariableType = (pvtInteger, pvtBoolean, pvtReal, pvtString, pvtObject, pvtNull);
  TVariableCat  = (vcCopy, vcVar, cvOut, vcConst);

  TVariable = Class
  public
    Name        : String;        // Nome da variável
    vType       : TVariableType; // Tipo da variável
    ObjectClass : TClass;        // Caso a variável seja do tipo OBJECT
    Value       : Variant;       // Valor
    Locked      : Boolean;       // Indica para o mecanismo que ele não deverá
                                 // remover esta variável ao compilar o código.
                                 // Pode ser interpretado como variável pré-declarada.

    Constructor Create(const varName: String;
                             varType: TVariableType;
                             varValue: Variant;
                             varClassType: TClass;
                             varLocked: Boolean);
  End;

  {Objeto TStack (Pilha de operadores auxiliar nos cálculos)}
  TopStack = Class(TStack)
  public
     Destructor Destroy; override;
     Procedure Clear;
  end;

  TexeStack = Class(TStack)
  public
    function  getSelf: TObject;
    function  AsInteger(i: byte): Integer;
    function  AsFloat(i: byte): Real;
    function  AsString(i: byte): String;
    function  AsObject(i: byte): TObject;
    function  AsBoolean(i: byte): Boolean;
    function  AsChar(i: byte): Char;
    function  AsReferency(i: byte): TVariable;

    procedure PushBoolean(Value: Boolean);
    procedure PushInteger(Value: Integer);
    procedure PushFloat(Value: Real);
    procedure PushString(Value: String);
    procedure PushObject(Value: TObject);
  end;

  TAccessMethod = procedure(Const Func_Name: String; Stack: TexeStack) of object;

  TFunctionObject = class
  private
    FPars     : Byte;
    FName     : String;
    FComment  : String;
    FCategory : String;
    FAM       : TAccessMethod;
    FPT       : Array of TVariableType;
    FPC       : Array of Boolean;
    FPClass   : Array of TClass;
    FRT       : TVariableType;
    FOC       : TClass; // caso o retorno seja Object

    function getParType(i: byte): TVariableType;
    function getParCat(i: byte): Boolean;
    function getParClass(i: byte): TClass;
  public
    constructor Create(Const Name, Comment, Category: String;
                       const ParTypes: Array of TVariableType;
                       const ParClasses: Array of TClass;
                       const ParCats: Array of Boolean;
                       ResType: TVariableType;
                       ResObjectClass: TClass;
                       AM: TAccessMethod);

    property Name             : String        read FName;      // Nome da função
    property Comment          : String        read FComment;   // Comentário sobre esta função
    property Category         : String        read FCategory;  // Categoria da rotina
    property Parameters       : Byte          read FPars;      // Número de Parâmetros
    property ParType [i: byte]: TVariableType read getParType; // Tipo dos Parâmetros
    property ParClass[i: byte]: TClass        read getParClass;// Tipo da classe se o parâmetro for do tipo Object
    property ParCat  [i: byte]: Boolean       read getParCat;  // Categoria do Parametro: True = por Referência; False = Por Cópia
    property ResType          : TVariableType read FRT;        // Tipo da função;
    property ResObjectClass   : TClass        read FOC;        // Caso o tipo de retorno seja Object
    property AccessMethod     : TAccessMethod read FAM;        // Método que fornecerá o serviço
  end;

  TProcObject = TFunctionObject;

  TFunctionList = class
  private
    FML   : TList;        // guarda somente as rotinas criadas por esta classe
    FSL   : TStrings;
    FCats : TStrings;
    FEconomize: Boolean;  // Guarda ou não as informações de ajuda das rotinas
    function getFunction(i: Integer): TFunctionObject;
    function getCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    procedure Add(Func: TFunctionObject); overload;
    procedure Add(Const Name: String;
                  Comment, Category: String;
                  const ParTypes: Array of TVariableType;
                  const ParClasses: Array of TClass;
                  const ParCats: Array of Boolean;
                  ResType: TVariableType;
                  ResObjectClass: TClass;
                  AM: TAccessMethod); overload;

    function  FunctionByName(const Name: String): TFunctionObject;
    function  IndexOf(const Name: String): Integer;

    property  Count       : Integer  read getCount;
    property  Categories  : TStrings read FCats      write FCats;
    property  Economize   : Boolean  read FEconomize write FEconomize;

    property  Func[i: Integer]: TFunctionObject read getFunction; default;
    property  OwnerFuncs: TList read FML;
  end;

  TProcList = TFunctionList;

  TFunctionServices = class
  public
    class procedure AddFunctionsIn (Functions: TFunctionList); virtual; abstract;
    class procedure AddProcsIn (Procs: TProcList); virtual; abstract;
  end;

  TClassList = Class;

  TpsClass = class
  private
    FComment      : String;               // Nome do Objeto
    FCategory     : String;               // Categoria do objeto
    FObjectClass  : TClass;               // Tipo da Classe
    FParent       : TClass;               // Tipo do pai da classe
    FFunctions    : TFunctionList;
    FProcs        : TProcList;
    FPT           : Array of TVariableType;
    FPC           : Array of Boolean;
    FPClass       : Array of TClass;
    FPars         : Byte;
    FCanCreate    : Boolean;

    function getName: String;
    procedure DoInheritence(aParent: TClass; CL: TClassList);
    procedure InheritMethods(aClass: TpsClass);
    function getParCat(i: byte): Boolean;
    function getParClass(i: byte): TClass;
    function getParType(i: byte): TVariableType;
  public
    constructor Create(Name, Parent: TClass;
                       const Comment, Category: String;
                       const ParTypes: Array of TVariableType;
                       const ParClasses: Array of TClass;
                       const ParCats: Array of Boolean;
                       CanCreate: Boolean;
                       CL: TClassList);
    destructor  Destroy; override;

    function CreateObject(Stack: TexeStack): TObject; virtual; abstract;
    procedure AddMethods; virtual; abstract; // deve ser implementado em cada classe

    property Name             : String          read getName;
    property Comment          : String          read FComment;
    property Category         : String          read FCategory;
    property Parameters       : Byte            read FPars;      // Número de Parâmetros
    property ParType [i: byte]: TVariableType   read getParType; // Tipo dos Parâmetros
    property ParClass[i: byte]: TClass          read getParClass;// Tipo da classe se o parâmetro for do tipo Object
    property ParCat  [i: byte]: Boolean         read getParCat;  // Categoria do Parametro: True = por Referência; False = Por Cópia
    property CanCreate        : Boolean         read FCanCreate;
    property ObjectClass      : TClass          read FObjectClass;
    property Parent           : TClass          read FParent;
    property Functions        : TFunctionList   read FFunctions;
    property Procs            : TProcList       read FProcs;
  end;

  TClassList = class
  private
    FSL   : TStrings;
    FCats : TStrings;
    FEconomize: Boolean;
    function getObject(i: Integer): TpsClass;
    function getCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    procedure Add(Obj: TpsClass); overload;
    function  ClassByName(const Name: String): TpsClass;
    function  IndexOf(const Name: String): Integer;

    property  Obj[i: Integer]: TpsClass read getObject; default;

    property  Count       : Integer  read getCount;
    property  Categories  : TStrings read FCats      write FCats;
    property  Economize   : Boolean  read FEconomize write FEconomize;
  end;

  TLib = class;

  TAPI_Proc = procedure(Lib: TLib);

  TLib = class
  private
    //FCats : TStrings;                 // Categorias das rotinas ou objetos
    FF    : TFunctionList;
    FP    : TProcList;
    FC    : TClassList;
    FSP   : TStrings;                 // Search Path
    FLL   : TList;                    // Lista das bibliotecas carregadas (handles)

    procedure AddLibrary(h: THandle);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Include(API: TAPI_Proc);

    // Este método aceita ou um arquivo "ConfigFile" com caminhos para as
    // bibliotecas ou aceita diretamente um caminho atraves de "Path"
    // Tambem aceita os dois.
    procedure Load_APIs(const ConfigFile, Path: String);

    property Functions  : TFunctionList read FF;
    property Procs      : TProcList     read FP;
    property Classes    : TClassList    read FC;
  end;

implementation

{ TVariable }

Constructor TVariable.Create(const varName: String;
                                   varType: TVariableType;
                                   varValue: Variant;
                                   varClassType: TClass;
                                   varLocked: Boolean);
Begin
  Inherited Create;
  Name   := varName;
  vType  := varType;
  Value  := varValue;
  Locked := varLocked;
  ObjectClass := varClassType;
End;

{ TopStack }

Procedure TopStack.Clear;
var i: Integer;
Begin
  For i := 0 to Count - 1 do Dispose(PVarRec(List[i]));
  List.Clear;
End;

Destructor TopStack.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

{ TFunctionObject }

constructor TFunctionObject.Create(const Name, Comment, Category: String;
                                   const ParTypes: Array of TVariableType;
                                   const ParClasses: Array of TClass;
                                   const ParCats: Array of Boolean;
                                   ResType: TVariableType;
                                   ResObjectClass: TClass;
                                   AM: TAccessMethod);
var i: Shortint	;
begin
  inherited Create;
  FName     := Name;
  FComment  := Comment;
  FCategory := Category;
  FPars     := Length(ParTypes);
  FAM       := AM;
  FRT       := ResType;
  FOC       := ResObjectClass;

  SetLength(FPT, FPars);
  for i := 0 to FPars-1 do FPT[i] := ParTypes[i];

  SetLength(FPClass, FPars);
  for i := 0 to FPars-1 do FPClass[i] := ParClasses[i];

  SetLength(FPC, FPars);
  for i := 0 to FPars-1 do FPC[i] := ParCats[i];
end;

function TFunctionObject.getParCat(i: byte): Boolean;
begin
  Result := FPC[i];
end;

function TFunctionObject.getParClass(i: byte): TClass;
begin
  Result := FPClass[i];
end;

function TFunctionObject.getParType(i: byte): TVariableType;
begin
  Result := FPT[i];
end;

{ TFunctionList }

procedure TFunctionList.Add(Func: TFunctionObject);
begin
  if not FEconomize then
     if (FCats <> nil) and (Func.Category <> '') then FCats.Add(Func.Category);

  FSL.AddObject(Func.Name, Func);
end;

procedure TFunctionList.Add(const Name: String;
                            Comment, Category: String;
                            const ParTypes: Array of TVariableType;
                            const ParClasses: Array of TClass;
                            const ParCats: Array of Boolean;
                            ResType: TVariableType;
                            ResObjectClass: TClass;
                            AM: TAccessMethod);
var FO: TFunctionObject;
begin
  if not FEconomize then
     if (FCats <> nil) and (Category <> '') then FCats.Add(Category);

  if FEconomize then
     begin
     Comment := '';
     Category := '';
     end;

  FO := TFunctionObject.Create(Name, Comment, Category, ParTypes, ParClasses,
                               ParCats, ResType, ResObjectClass, AM);
  FSL.AddObject(Name, FO);
  FML.Add(FO);
end;

constructor TFunctionList.Create;
begin
  inherited;
  FEconomize := True;
  FSL := TStringList.Create;
  FML := TList.Create;
  FCats := TStringList.Create;
  TStringList(FCats).Duplicates := dupIgnore;
  TStringList(FCats).Sorted := True;
end;

destructor TFunctionList.Destroy;
var i: Integer;
begin
  for i := 0 to FML.Count-1 do TObject(FML[i]).Free;
  FSL.Free;
  FML.Free;
  FCats.Free;
  inherited;
end;

function TFunctionList.FunctionByName(const Name: String): TFunctionObject;
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i > -1 then
     Result := TFunctionObject(FSL.Objects[i])
  else
     Result := nil;
end;

function TFunctionList.getCount: Integer;
begin
  Result := FSL.Count;
end;

function TFunctionList.getFunction(i: Integer): TFunctionObject;
begin
  Result := TFunctionObject(FSL.Objects[i]);
end;

function TFunctionList.IndexOf(const Name: String): Integer;
begin
  Result := FSL.IndexOf(Name);
end;

{ TpsClass }

constructor TpsClass.Create(Name, Parent: TClass;
                            const Comment, Category: String;
                            const ParTypes: Array of TVariableType;
                            const ParClasses: Array of TClass;
                            const ParCats: Array of Boolean;
                            CanCreate: Boolean;
                            CL: TClassList);
var i: Integer;
begin
  inherited Create;

  FObjectClass := Name;
  FParent      := Parent;

  if CL.Economize then
     begin
     FComment   := '';
     FCategory  := '';
     end
  else
     begin
     FComment   := Comment;
     FCategory  := Category;
     end;

  FPars := Length(ParTypes);

  SetLength(FPT, FPars);
  for i := 0 to FPars-1 do FPT[i] := ParTypes[i];

  SetLength(FPClass, FPars);
  for i := 0 to FPars-1 do FPClass[i] := ParClasses[i];

  SetLength(FPC, FPars);
  for i := 0 to FPars-1 do FPC[i] := ParCats[i];

  FCanCreate := CanCreate;

  FFunctions := TFunctionList.Create;
  FFunctions.Economize := CL.Economize;

  FProcs := TProcList.Create;
  FProcs.Economize := CL.Economize;

  DoInheritence(Parent, CL);
  AddMethods;
  CL.Add(Self);
end;

destructor TpsClass.Destroy;
begin
  FFunctions.Free;
  FProcs.Free;
  inherited;
end;

procedure TpsClass.InheritMethods(aClass: TpsClass);
var i: Integer;
begin
  for i := 0 to aClass.Functions.Count-1 do
    FFunctions.Add(aClass.Functions[i]);

  for i := 0 to aClass.Procs.Count-1 do
    FProcs.Add(aClass.Procs[i]);
end;

procedure TpsClass.DoInheritence(aParent: TClass; CL: TClassList);
var C: TpsClass;
begin
  if aParent <> nil then
     begin
     C := CL.ClassByName(aParent.ClassName);
     if C <> nil then
        begin
        DoInheritence(C.Parent, CL);
        InheritMethods(C);
        end;
     end;
end;

function TpsClass.getName: String;
begin
  Result := FObjectClass.ClassName;
end;

function TpsClass.getParCat(i: byte): Boolean;
begin
  Result := FPC[i];
end;

function TpsClass.getParClass(i: byte): TClass;
begin
  Result := FPClass[i];
end;

function TpsClass.getParType(i: byte): TVariableType;
begin
  Result := FPT[i];
end;

{ TClassList }

procedure TClassList.Add(Obj: TpsClass);
begin
  if (FCats <> nil) and (Obj.Category <> '') then FCats.Add(Obj.Category);
  FSL.AddObject(Obj.Name, Obj);
end;

constructor TClassList.Create;
begin
  inherited;
  FEconomize := True;
  FSL := TStringList.Create;
  FCats := TStringList.Create;
  TStringList(FCats).Duplicates := dupIgnore;
  TStringList(FCats).Sorted := True;
end;

destructor TClassList.Destroy;
var i: Integer;
begin
  for i := 0 to FSL.Count-1 do FSL.Objects[i].Free;
  FSL.Free;
  FCats.Free;
  inherited;
end;

function TClassList.getCount: Integer;
begin
  Result := FSL.Count;
end;

function TClassList.getObject(i: Integer): TpsClass;
begin
  Result := TpsClass(FSL.Objects[i]);
end;

function TClassList.IndexOf(const Name: String): Integer;
begin
   Result := FSL.IndexOf(Name);
end;

function TClassList.ClassByName(const Name: String): TpsClass;
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i > -1 then
     Result := TpsClass(FSL.Objects[i])
  else
     Result := nil;
end;

{ TexeStack }

function TexeStack.AsBoolean(i: byte): Boolean;
var p: ^Boolean;
begin
  p := self.List[self.List.Count-i];
  Result := p^;
end;

function TexeStack.AsChar(i: byte): Char;
var p: pString;
begin
  p := self.List[self.List.Count-i];
  if p^ = '' then Result := #0 else Result := P^[1];
end;

function TexeStack.AsFloat(i: byte): Real;
var p: ^Real;
begin
  p := self.List[self.List.Count-i];
  Result := p^;
end;

function TexeStack.AsInteger(i: byte): Integer;
begin
  Result := Trunc(Self.AsFloat(i));
end;

function TexeStack.AsObject(i: byte): TObject;
var p: Pointer;
    o: Integer;
begin
  p := self.List[self.List.Count-i];
  o := TVariable(p^).Value;
  Result := TObject(o);
end;

function TexeStack.AsReferency(i: byte): TVariable;
var p: Pointer;
begin
  p := self.List[self.List.Count-i];
  Result := TVariable(p^);
end;

function TexeStack.AsString(i: byte): String;
var p: pString;
begin
  p := self.List[self.List.Count-i];
  Result := p^;
end;

function TexeStack.getSelf: TObject;
var p: Pointer;
    o: Integer;
begin
  p := self.List[0];
  o := TVariable(p^).Value;
  Result := TObject(o);
end;

procedure TexeStack.PushBoolean(Value: Boolean);
var p: ^Boolean;
begin
  new(p);
  p^ := Value;
  Self.PushItem(p);
end;

procedure TexeStack.PushFloat(Value: Real);
var p: ^Real;
begin
  new(p);
  p^ := Value;
  Self.PushItem(p);
end;

procedure TexeStack.PushInteger(Value: Integer);
begin
  Self.PushFloat(Value);
end;

procedure TexeStack.PushObject(Value: TObject);
var p: ^TVariable;
begin
  new(p);
  p^ := TVariable.Create('', pvtObject, Integer(Value), nil, False);
  if Value <> nil then p^.ObjectClass := Value.ClassType;
  Self.PushItem(p);
end;

procedure TexeStack.PushString(Value: String);
var p: pString;
begin
  new(p);
  p^ := Value;
  Self.PushItem(p);
end;

{ TLib }

constructor TLib.Create;
begin
  inherited;

  //FCats := TStringList.Create;
  //TStringList(FCats).Duplicates := dupIgnore;
  //TStringList(FCats).Sorted := True;

  FF := TFunctionList.Create;
  FP := TProcList.Create;
  FC := TClassList.Create;

  //FC.FCats := FCats;
  //FF.FCats := FCats;
  //FP.FCats := FCats;

  FSP := TStringList.Create;
  TStringList(FSP).Duplicates := dupIgnore;
end;

destructor TLib.Destroy;
var i: Integer;
begin
  FSP.Free;
  FF.Free;
  FP.Free;
  FC.Free;
  //FCats.Free;

  if FLL <> nil then
     for i := 0 to FLL.Count-1 do
       FreeLibrary(THandle(FLL[i]));

  inherited;
end;

procedure TLib.Include(API: TAPI_Proc);
begin
  API(self);
end;

function FindFiles(const PathMask: String; var FL: TStrings): Boolean;
var SR    : TSearchRec;
    Error : SmallInt;
begin
  if FL = nil then FL := TStringList.Create;
  FL.Clear;
  Error := FindFirst(PathMask, faAnyFile, SR);
  while Error = 0 do
    begin
    FL.Add(SR.Name);
    Error := FindNext(SR);
    end;
  SysUtils.FindClose(SR);
end;

type
  TProc_PascalScript_API = procedure (Lib: TLib); stdcall;

procedure TLib.AddLibrary(h: THandle);
begin
  if FLL = nil then FLL := TList.Create;
  FLL.Add(pointer(h));
end;

procedure TLib.Load_APIs(const ConfigFile, Path: String);
var i, k : Integer;
    FL: TStrings;
    h: THandle;
    p: TProc_PascalScript_API;
    s: string;
begin
  FSP.Clear();

  if FileExists(ConfigFile) then
     FSP.LoadFromFile(ConfigFile);

  if SysUtils.DirectoryExists(Path) then
     FSP.Add(Path);

  FL := nil;
  for i := 0 to FSP.Count-1 do
    begin
    s := FSP[i];
    if SysUtilsEx.LastChar(s) <> '\' then s := s + '\';
    FindFiles(s + '*.DLL', FL);
    for k := 0 to FL.Count-1 do
      begin
      h := LoadLibrary( PChar(s + FL[k]) );
      if h <> 0 then p := GetProcAddress(h, 'PascalScript_API');
      if @p <> nil then
         begin
         AddLibrary(h);
         p(self);
         end;
      end; // for k
    end; // for i
  FL.Free;
end;

// Variavel global privada utilada em barras de progresso
var
  FPB: IProgressBar = nil;

procedure setGlobalProgressBar(PB: IProgressBar);
begin
  FPB := PB;
end;

function getGlobalProgressBar(): IProgressBar;
begin
  result := FPB;
end;

end.

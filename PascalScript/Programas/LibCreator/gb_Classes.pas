unit gb_Classes;

interface
uses classes;

type

  TFunctionObject = class
  private
    FPars     : Byte;
    FName     : String;
    FComment  : String;
    FCategory : String;
    FAM       : String;
    FPT       : Array of String;
    FPC       : Array of String;
    FPClass   : Array of String;
    FRT       : String;
    FOC       : String;
    FCode: TStrings; // caso o retorno seja Object

    function getParType(i: byte): String;
    function getParCat(i: byte): String;
    function getParClass(i: byte): String;
  public
    constructor Create; overload;
    constructor Create(const Name, Comment, Category: String;
                       const ParTypes, ParClasses, ParCats: Array of String;
                       const ResType, ResObjectClass, AM: String); overload;

    destructor Destroy; override;

    procedure setParTypes(const ParTypes: Array of String);
    procedure setParClasses(const ParClasses: Array of String);
    procedure setParCats(const ParCats: Array of String);

    property Name             : String  read FName       write FName;      // Nome da função
    property Comment          : String  read FComment    write FComment;   // Comentário sobre esta função
    property Category         : String  read FCategory   write FCategory;  // Categoria da rotina

    property ResType          : String  read FRT         write FRT;        // Tipo da função;
    property ResObjectClass   : String  read FOC         write FOC;        // Caso o tipo de retorno seja Object
    property AccessMethod     : String  read FAM         write FAM;        // Método que fornecerá o serviço

    property Parameters       : Byte    read FPars       write FPars;      // Número de Parâmetros

    property ParType [i: byte]: String  read getParType;  // Tipo dos Parâmetros
    property ParClass[i: byte]: String  read getParClass; // Tipo da classe se o parâmetro for do tipo Object
    property ParCat  [i: byte]: String  read getParCat;   // Categoria do Parametro: True = por Referência; False = Por Cópia

    property Code: TStrings read FCode;
  end;

  TProcObject = TFunctionObject;
{
  TFunctionList = class
  private
    FML   : TList;    // guarda somente as rotinas criadas por esta classe
    FSL   : TStrings;
    FCats : TStrings;
    function getFunction(i: Integer): TFunctionObject;
    function getCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    procedure Add(Func: TFunctionObject); overload;
    procedure Remove(const Name: String);
    function  FunctionByName(const Name: String): TFunctionObject;
    function  IndexOf(const Name: String): Integer;

    property  Count: Integer read getCount;
    property  Func[i: Integer]: TFunctionObject read getFunction; default;
  end;

  TProcList = TFunctionList;

  TClassList = Class;
}
  TpsClass = class
  private
    FName         : String;
    FComment      : String;               // Nome do Objeto
    FCategory     : String;               // Categoria do objeto
    FParent       : String;               // Tipo do pai da classe
    //FFunctions    : TFunctionList;
    //FProcs        : TProcList;
    FCMN          : String;               // Nome do método de criação
    FInternalName : String;
    FPT           : Array of String;
    FPC           : Array of String;
    FPClass       : Array of String;
    FPars: Shortint;
    function getParCat(i: byte): String;
    function getParClass(i: byte): String;
    function getParType(i: byte): String;
    procedure SetName(const Value: String);
  public
    constructor Create; overload;
    constructor Create(const Name, Parent, Comment, Category, CMN: String); overload;
    destructor  Destroy; override;

    function CreateObject: TObject; virtual; abstract;
    procedure AddMethods; virtual; abstract; // deve ser implementado em cada classe

    procedure setParTypes(const ParTypes: Array of String);
    procedure setParClasses(const ParClasses: Array of String);
    procedure setParCats(const ParCats: Array of String);

    property Name             : String          read FName         write SetName;
    property InternalName     : String          read FInternalName write FInternalName;
    property Parent           : String          read FParent       write FParent;
    property Comment          : String          read FComment      write FComment;
    property Category         : String          read FCategory     write FCategory;
    property CMN              : String          read FCMN          write FCMN;
    //property Functions        : TFunctionList   read FFunctions    write FFunctions;
    //property Procs            : TProcList       read FProcs        write FProcs;
    property Parameters       : Shortint        read FPars       write FPars;      // Número de Parâmetros

    property ParType [i: byte]: String  read getParType;  // Tipo dos Parâmetros
    property ParClass[i: byte]: String  read getParClass; // Tipo da classe se o parâmetro for do tipo Object
    property ParCat  [i: byte]: String  read getParCat;   // Categoria do Parametro: True = por Referência; False = Por Cópia

  end;
{
  TClassList = class
  private
    FSL   : TStrings;
    FCats : TStrings;
    function getObject(i: Integer): TpsClass;
    function getCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    procedure Add(Obj: TpsClass);
    function  ClassByName(const Name: String): TpsClass;
    function  IndexOf(const Name: String): Integer;
    procedure Remove(const Name: String);

    property  Obj[i: Integer]: TpsClass read getObject; default;
    property  Count: Integer read getCount;
  end;
}
implementation

{ TFunctionObject }

constructor TFunctionObject.Create(const Name, Comment, Category: String;
                                   const ParTypes, ParClasses, ParCats: Array of String;
                                   const ResType, ResObjectClass, AM: String);
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
  FCode     := TStringList.Create;

  SetLength(FPT, FPars);
  for i := 0 to FPars-1 do FPT[i] := ParTypes[i];

  SetLength(FPClass, FPars);
  for i := 0 to FPars-1 do FPClass[i] := ParClasses[i];

  SetLength(FPC, FPars);
  for i := 0 to FPars-1 do FPC[i] := ParCats[i];
end;

constructor TFunctionObject.Create;
begin
  inherited Create;
  FCode := TStringList.Create;
end;

destructor TFunctionObject.Destroy;
begin
  FCode.Free;
  inherited;
end;

function TFunctionObject.getParCat(i: byte): String;
begin
  Result := FPC[i];
end;

function TFunctionObject.getParClass(i: byte): String;
begin
  Result := FPClass[i];
end;

function TFunctionObject.getParType(i: byte): String;
begin
  Result := FPT[i];
end;

procedure TFunctionObject.setParCats(const ParCats: array of String);
var i: Integer;
begin
  SetLength(FPC, FPars);
  for i := 0 to FPars-1 do FPC[i] := ParCats[i];
end;

procedure TFunctionObject.setParClasses(const ParClasses: array of String);
var i: Integer;
begin
  SetLength(FPClass, FPars);
  for i := 0 to FPars-1 do FPClass[i] := ParClasses[i];
end;

procedure TFunctionObject.setParTypes(const ParTypes: array of String);
var i: Integer;
begin
  SetLength(FPT, FPars);
  for i := 0 to FPars-1 do FPT[i] := ParTypes[i];
end;
(*
{ TFunctionList }

procedure TFunctionList.Add(Func: TFunctionObject);
begin
  if (FCats <> nil) and (Func.Category <> '') then FCats.Add(Func.Category);
  FSL.AddObject(Func.Name, Func);
end;

constructor TFunctionList.Create;
begin
  inherited;
  FSL := TStringList.Create;
  FML := TList.Create;
end;

destructor TFunctionList.Destroy;
var i: Integer;
begin
  for i := 0 to FML.Count-1 do TObject(FML[i]).Free;
  FSL.Free;
  FML.Free;
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

procedure TFunctionList.Remove(const Name: String);
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i > -1 then
     begin
     FSL.Objects[i].Free;
     FSL.Delete(i);
     end;
end;
*)
{ TpsClass }

constructor TpsClass.Create(const Name, Parent, Comment, Category, CMN: String);
begin
  inherited Create;

  FName         := Name;
  FParent       := Parent;
  FComment      := Comment;
  FCategory     := Category;
  FInternalName := FName;
  FPars         := -1;

  Insert('ps_', FInternalName, 2);

  //FFunctions   := TFunctionList.Create;
  //FProcs       := TProcList.Create;

  //CL.Add(Self);
end;

constructor TpsClass.Create;
begin
  inherited Create;
  FPars := -1;
end;

destructor TpsClass.Destroy;
begin
  //FFunctions.Free;
  //FProcs.Free;
  inherited;
end;

function TpsClass.getParCat(i: byte): String;
begin
  Result := FPC[i];
end;

function TpsClass.getParClass(i: byte): String;
begin
  Result := FPClass[i];
end;

function TpsClass.getParType(i: byte): String;
begin
  Result := FPT[i];
end;

procedure TpsClass.SetName(const Value: String);
begin
  FName := Value;
  FInternalName := FName;
  Insert('ps_', FInternalName, 2);
end;

procedure TpsClass.setParCats(const ParCats: array of String);
var i: Integer;
begin
  SetLength(FPC, FPars);
  for i := 0 to FPars-1 do FPC[i] := ParCats[i];
end;

procedure TpsClass.setParClasses(const ParClasses: array of String);
var i: Integer;
begin
  SetLength(FPClass, FPars);
  for i := 0 to FPars-1 do FPClass[i] := ParClasses[i];
end;

procedure TpsClass.setParTypes(const ParTypes: array of String);
var i: Integer;
begin
  SetLength(FPT, FPars);
  for i := 0 to FPars-1 do FPT[i] := ParTypes[i];
end;
(*
{ TClassList }

procedure TClassList.Add(Obj: TpsClass);
begin
  if (FCats <> nil) and (Obj.Category <> '') then FCats.Add(Obj.Category);
  FSL.AddObject(Obj.Name, Obj);
end;

constructor TClassList.Create;
begin
  inherited;
  FSL := TStringList.Create;
end;

destructor TClassList.Destroy;
var i: Integer;
begin
  for i := 0 to FSL.Count-1 do FSL.Objects[i].Free;
  FSL.Free;
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

procedure TClassList.Remove(const Name: String);
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i > -1 then
     begin
     FSL.Objects[i].Free;
     FSL.Delete(i);
     end;
end;
*)
end.

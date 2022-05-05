unit Lib_GlobalObjects;

interface
uses Classes,
     psBase,
     MessageManager;

var
  GO_CHANGE       : Integer;
  GO_CLOSE_DIALOG : Integer;
  
type
  TGlobalObjects = Class
  private
    FSL: TStrings;
    function getCount: Integer;
    function getObject(i: Integer): TObject;
    function getName(i: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;

    function  Add(const Name: String; aObject: TObject): String;
    function  ObjectByName(const Name: String): TObject;
    function  Exists(const Name: String): Boolean;

    procedure Remove(const Name: String);
    procedure RemoveAndFree(const Name: String);
    procedure Clear;

    property Count: Integer read getCount;
    property Objects[i: Integer]: TObject read getObject; default;
    property Name[i: Integer]: String read getName;
  end;

  procedure API(Lib: TLib);

implementation
uses SysUtils;

type
  Tps_GlobalObjects = class(TpsClass)
  public
    procedure AddMethods; override;

    // procedimentos da classe
    class procedure am_Remove        (Const Func_Name: String; Stack: TexeStack);
    class procedure am_RemoveAndFree (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Clear         (Const Func_Name: String; Stack: TexeStack);

    // funções da classe
    class procedure am_GetCount      (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Add           (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Get           (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Exists        (Const Func_Name: String; Stack: TexeStack); 
  end;

procedure API(Lib: TLib);
begin
  Tps_GlobalObjects.Create(
    TGlobalObjects,
    nil,
    'Gerenciador de objetos (variáveis globais)',
    cCatSistema,
    [], [], [],
    False,
    Lib.Classes);
end;

{ Tps_GlobalObjects }

procedure Tps_GlobalObjects.AddMethods;
begin
  with Procs do
    begin
    Add('Clear',
        'Remove todos os objetos, inclusive, liberando a memória associada a cada um deles',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        am_Clear);

    Add('Remove',
        'Remove um objeto da lista de variáveis globais',
        '',
        [pvtString],
        [nil      ],
        [False    ],
        pvtNull,
        TObject,
        am_Remove);

    Add('RemoveAndFree',
        'Remove um Objeto e libera sua memória',
        '',
        [pvtString],
        [nil      ],
        [False    ],
        pvtNull,
        TObject,
        am_RemoveAndFree);
    end;

  with Functions do
    begin
    Add('Count',
        'Retorna a quantidade de objetos armazenados',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_GetCount);

    Add('Add',
        'Adiciona um Objeto',
        '',
        [pvtString, pvtObject],
        [nil      , TObject  ],
        [False    , True     ],
        pvtString,
        TObject,
        am_Add);

    Add('Get',
        'Obtem um objeto pelo nome',
        '',
        [pvtString],
        [nil      ],
        [False    ],
        pvtObject  ,
        TObject    ,
        am_Get);

    Add('Exists',
        'Verifica a existência de um objeto dado seu nome',
        '',
        [pvtString],
        [nil      ],
        [False    ],
        pvtBoolean ,
        TObject    ,
        am_Exists);
    end;
end;

class procedure Tps_GlobalObjects.am_Add(const Func_Name: String; Stack: TexeStack);
var s: String;
begin
  s := TGlobalObjects(Stack.AsObject(3)).Add(Stack.AsString(1), Stack.AsObject(2));
  Stack.PushString(s);
end;

class procedure Tps_GlobalObjects.am_Clear(const Func_Name: String; Stack: TexeStack);
begin
  TGlobalObjects(Stack.AsObject(1)).Clear;
end;

class procedure Tps_GlobalObjects.am_Exists(const Func_Name: String; Stack: TexeStack);
var b: Boolean;
begin
  b := TGlobalObjects(Stack.AsObject(2)).Exists(Stack.AsString(1));
  Stack.PushBoolean(b);
end;

class procedure Tps_GlobalObjects.am_Get(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := TGlobalObjects(Stack.AsObject(2)).ObjectByName(Stack.AsString(1));
  Stack.PushObject(o);
end;

class procedure Tps_GlobalObjects.am_GetCount(const Func_Name: String; Stack: TexeStack);
var i: Integer;
begin
  i := TGlobalObjects(Stack.AsObject(1)).Count;
  Stack.PushInteger(i);
end;

class procedure Tps_GlobalObjects.am_Remove(const Func_Name: String; Stack: TexeStack);
begin
  TGlobalObjects(Stack.AsObject(2)).Remove(Stack.AsString(1));
end;

class procedure Tps_GlobalObjects.am_RemoveAndFree(const Func_Name: String; Stack: TexeStack);
begin
  TGlobalObjects(Stack.AsObject(2)).RemoveAndFree(Stack.AsString(1));
end;
{ TGlobalObjects }

constructor TGlobalObjects.Create;
begin
  inherited;
  FSL := TStringList.Create;
  TStringList(FSL).Sorted := True;
end;

destructor TGlobalObjects.Destroy;
begin
  GetMessageManager.SendMessage(GO_CLOSE_DIALOG, [self]);
  Clear;
  FSL.Free;
  inherited;
end;

function TGlobalObjects.Add(const Name: String; aObject: TObject): String;
var i: Integer;
begin
  Result := Name;
  i := 0;
  while FSL.IndexOf(Result) <> -1 do
    begin
    inc(i);
    Result := Name + IntToStr(i);
    end;
  FSL.AddObject(Result, aObject);
  GetMessageManager.SendMessage(GO_CHANGE, [self]);
end;

procedure TGlobalObjects.Clear;
var i: Integer;
begin
  for i := 0 to FSL.Count-1 do
    FSL.Objects[i].Free;

  FSL.Clear;
  GetMessageManager.SendMessage(GO_CHANGE, [self]);
end;

function TGlobalObjects.Exists(const Name: String): Boolean;
begin
  Result := (FSL.IndexOf(Name) <> -1);
end;

function TGlobalObjects.getCount: Integer;
begin
  Result := FSL.Count;
end;

function TGlobalObjects.ObjectByName(const Name: String): TObject;
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i <> -1 then
     Result := FSL.Objects[i]
  else
     Raise Exception.Create('Variável desconhecida: ' + Name);
end;

procedure TGlobalObjects.Remove(const Name: String);
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i <> -1 then
     begin
     FSL.Delete(i);
     GetMessageManager.SendMessage(GO_CHANGE, [self]);
     end;
end;

procedure TGlobalObjects.RemoveAndFree(const Name: String);
var i: Integer;
begin
  i := FSL.IndexOf(Name);
  if i <> -1 then
     begin
     FSL.Objects[i].Free;
     FSL.Delete(i);
     GetMessageManager.SendMessage(GO_CHANGE, [self]);
     end;
end;

function TGlobalObjects.getObject(i: Integer): TObject;
begin
  Result := FSL.Objects[i];
end;

function TGlobalObjects.getName(i: Integer): String;
begin
  Result := FSL[i];
end;

initialization
  GO_CHANGE       := GetMessageManager.RegisterMessageID('GO_CHANGE');
  GO_CLOSE_DIALOG := GetMessageManager.RegisterMessageID('GO_CLOSE_DIALOG');

end.

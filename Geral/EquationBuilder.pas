unit EquationBuilder;

interface
uses Classes, SysUtils, Contnrs, SysUtilsEx;

type
  TIdentType   = (itDecision, itState, itConst);
  TIdentSignal = (isPlus = ord('+'), isMinus = ord('-'));

  // Identifica o possuidor do Identificador
  // O possuidor é aquele que fornece um prefixo e um valor para o Identificador
  IIdentificatorOwner = interface
    function io_getOwnerName(): string;
    function io_getValue(const FieldName: string): string;
  end;

  // Representa um dos identificadores de uma equação
  // Um identificador pode ser uma variável ou uma constante
  TIdentificator = class
  private
    FOwner       : IIdentificatorOwner;
    FName        : string;
    FSignal      : TIdentSignal;
    FType        : TIdentType;
    FPreview     : boolean;
    FIndexOffset : integer;

    function getValue(): string;
  public
    constructor Create(FieldName  : String;                  // Nome do Identificador sem o prefixo
                       Signal     : TIdentSignal;            // Positivo ou Negativo
                       FieldType  : TIdentType;              // itDecision, itState ou itConst
                       IndexOffset: integer;                 // Deslocamento do Indice
                       Owner      : IIdentificatorOwner);    // Possuidor

    // Representa o identificador como uma string()
    function ToString(aIndex: integer): string;

    // Nome do Identificador sem o prefixo
    property FieldName : string read FName write FName;

    // itDecision, itState ou itConst
    property FieldType : TIdentType read FType write FType;

    // Positivo ou Negativo
    property Signal : TIdentSignal read FSignal write FSignal;

    // Deslocamento do Indice
    property IndexOffset: integer read FIndexOffset write FIndexOffset;

    // Possuidor do identificador
    property Owner : IIdentificatorOwner read FOwner write FOwner;

    // Em modo "Preview" Os tipos "itConst" são escritos como variáveis
    property Preview : boolean read FPreview write FPreview;

    // Valor atual do identificador
    property Value : string read getValue;
  end;

  // Representa um conjunto de identificadores
  TIdentificatorList = class
  private
    FList: TObjectList;
    function getCount: integer;
    function getItem(i: integer): TIdentificator;
  public
    constructor Create();
    destructor Destroy(); override;

    // Adiciona um Identificador
    procedure Add(Item: TIdentificator);

    // Retorna nil se nao encontrar
    function IdentByName(const Name: string): TIdentificator;

    property Count : integer read getCount;
    property Item[i: integer] : TIdentificator read getItem;
  end;

  // Representa uma equação ou seja, um conjunto de identificadores
  TEquation = class(TIdentificatorList)
  private
    FPreview: boolean;
    procedure setPreview(const Value: boolean);
  public
    function ToString(aIndex: integer): string;

    // Em modo "Preview" Os tipos "itConst" são escritos como variáveis
    property Preview : boolean read FPreview write setPreview;
  end;

  // Representa um copnjunto de equações
  TEquationList = class
  private
    FList: TObjectList;
    FPreview: boolean;
    function getCount: integer;
    function getItem(i: integer): TEquation;
    procedure setPreview(const Value: boolean);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Add(Item: TEquation);
    procedure ToStrings(Text: TStrings; aIndex: integer; var GeralIndex: integer);

    // Em modo "Preview" Os tipos "itConst" são escritos como variáveis
    property Preview : boolean read FPreview write setPreview;

    // Número de equações
    property Count : integer read getCount;

    // Retorna a enégima equação
    property Item[i: integer] : TEquation read getItem;
  end;

  // Interface para quem quiser implementar um Gerador de Equações
  IEquationBuilder = interface
    procedure eb_Generate(Text: TStrings);
    procedure eb_Init(Text: TStrings);
    procedure eb_Finalize(Text: TStrings);
    //procedure eb_InitIdents(Idents: TIdentificatorList; Text: TStrings);
  end;

  // Mecanismo central do gerador de equações
  // Se utiliza de um implementador de "IEquationBuilder"
  TEquationBuilder = class
  private
    FIdents   : TIdentificatorList;
    FBuilder  : IEquationBuilder;
    FText     : TStrings;
    //FInitFile : string;
  public
    constructor Create(Builder: IEquationBuilder);
    destructor Destroy(); override;

    // Adiciona o conteúdo de um arquivo as equações
    procedure AppendFile(const Name: string);

    // Dá a partida no mecanismo
    procedure Generate();

    // Implementador do gerador
    property Builder : IEquationBuilder read FBuilder;

    // Guarda as equações geradas
    property Text : TStrings read FText;
  end;

implementation

{ TIdentificator }

constructor TIdentificator.Create(FieldName: String; Signal: TIdentSignal;
  FieldType: TIdentType; IndexOffset: integer; Owner: IIdentificatorOwner);
begin
  inherited Create();
  FName := FieldName;
  FType := FieldType;
  FSignal := Signal;
  FOwner := Owner;
  FIndexOffset := IndexOffset;
end;
                                                   
function TIdentificator.getValue(): string;
begin
  Result := FOwner.io_getValue(FName);
end;

function TIdentificator.ToString(aIndex: integer): string;
var s: TIdentSignal;
begin
  // Se eh variavel de decisao inverte o sinal
  if FType = itDecision then
     if FSignal = isPlus then
        s := isMinus
     else
        s := isPlus
  else
     s := FSignal;

  Result := char(s) + ' ';
  if (FType = itConst) and not FPreview then
     Result := Result + getValue()
  else
     Result := Result + FName + FOwner.io_getOwnerName() + intToStr(aIndex + FIndexOffset);
end;

{ TEquation }

constructor TIdentificatorList.Create();
begin
  inherited Create();
  FList := TObjectList.Create(true);
end;

destructor TIdentificatorList.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TIdentificatorList.Add(Item: TIdentificator);
begin
  FList.Add(Item);
end;

function TIdentificatorList.getCount(): integer;
begin
  Result := FList.Count;
end;

function TIdentificatorList.getItem(i: integer): TIdentificator;
begin
  Result := TIdentificator(FList[i]);
end;

{ TEquation }

procedure TEquation.setPreview(const Value: boolean);
var i: Integer;
begin
  if FPreview <> Value then
     begin
     FPreview := Value;
     for i := 0 to Count-1 do
       getItem(i).Preview := Value
     end;
end;

// Gera uma equacao no formato:
//   Variaveis de Decisao = Variaveis de Estado ou Constantes
function TEquation.ToString(aIndex: integer): string;
var i: integer;
    sr, sl: string;
    id: TIdentificator;
begin
  sr := '';
  sl := '';
  for i := 0 to self.Count-1 do
    begin
    id := self.Item[i];
    case id.FieldType of
      itDecision       : sl := sl + id.ToString(aIndex) + ' ';
      itState, itConst : sr := sr + id.ToString(aIndex) + ' ';
      end;
    end;

  if sl = '' then sl := '0 ';
  if sr = '' then sr := '0 ';

  Result := sl + '= ' + sr;

  // ajusta o resultado esteticamente
  if (Result[1] = '+') then System.Delete(Result, 1, 2);
  i := System.Pos('=', Result) + 2;
  if (Result[i] = '+') then System.Delete(Result, i, 2);
end;

{ TEquationList }

constructor TEquationList.Create();
begin
  inherited Create();
  FList := TObjectList.Create(true);
end;

destructor TEquationList.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TEquationList.Add(Item: TEquation);
begin
  FList.Add(Item);
end;

function TEquationList.getCount(): integer;
begin
  Result := FList.Count;
end;

function TEquationList.getItem(i: integer): TEquation;
begin
  Result := TEquation(FList[i]);
end;

procedure TEquationList.ToStrings(Text: TStrings; aIndex: integer; var GeralIndex: integer);
var i: Integer;
begin
  for i := 0 to getCount-1 do
    begin
    inc(GeralIndex);
    Text.Add('E' + intToStr(GeralIndex) + ') ' + getItem(i).ToString(aIndex));
    end;
end;

procedure TEquationList.setPreview(const Value: boolean);
var i: Integer;
begin
  if FPreview <> Value then
     begin
     FPreview := Value;
     for i := 0 to getCount-1 do
       getItem(i).Preview := Value
     end;
end;

{ TEquationBuilder }

constructor TEquationBuilder.Create(Builder: IEquationBuilder);
begin
  inherited Create();
  FText := TStringList.Create();
  FBuilder := Builder;
end;

destructor TEquationBuilder.Destroy();
begin
  FIdents.Free();
  FText.Free();
  inherited;
end;

procedure TEquationBuilder.Generate();
begin
  FText.Clear();
  FBuilder.eb_Init(FText);
  try
    FBuilder.eb_Generate(FText);
  finally
    FBuilder.eb_Finalize(FText);
  end;  
end;

procedure TEquationBuilder.AppendFile(const Name: string);
var SL: TStrings;
begin
  SL := SysUtilsEx.LoadTextFile(Name);
  FText.Add('// File: ' + Name);
  FText.AddStrings(SL);
  SL.Free;
end;

function TIdentificatorList.IdentByName(const Name: string): TIdentificator;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    begin
    result := TIdentificator(FList[i]);
    if CompareText(result.FieldName, Name) = 0 then
       Exit;
    end;

  result := nil;
end;

end.

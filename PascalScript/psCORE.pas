unit psCORE;

// Versão 3.04

{ HISTORICO
    2.01:
      - Bug na passagem de parâmetros Booleanos concertado

    2.10:
      - Substituição da indexação das variáveis por endereçamento direto

    3.00:
      - Implementação de sub-rotinas
      - Melhor gerenciamento de erros

   3.01:
      - Bug em código do tipo concertado
        var x: string;
        begin
          x := 'x';
        end.

   3.02:
      - Bug na geracao de erros concertado.
        Havia uma demora excessiva do compilador no reconhecimento de erros
        pois, dependendo do erro, a producao SC era chamada recursivamente.
        Esta demora foi evitada limitando-se as chamadas recursivas a SC em
        ocorrencias de erros de sintaxe.

   3.03
      - Erro corrigido na geração de código de retorno de funções
        Ex:
               Program Erro;
               var A: real;
                   B: real;
                   C: real;

                   function X(A: real): real;
                   begin
                     Result := A + A;
                   end;

               begin
                 A := 1.1;
                 B := SQR( X(A) );  // ADD PAR RESULT  (RESULT sem endereco)      <------- ERRO
                 C := X(A); // := RESULT C  (RESULT com endereco)
               end.

   3.04
      - Erro na identificacao de erros do compilador.
        Em uma das situacoes onde o usuario indicava um numero incorreto de
        parametros para uma funcao dava erro de formatacao na mensagem de erro pois
        internamente esta mensagem precisava de dois pars. e somente um era fornecido.
}

{
  AUTOR .............................. Adriano Rochedo Conceição

  MODO DE USO:

    - Criar um objeto do tipo TPascalScript

    - Definir o texto a ser compilado e executado através da propriedade:
        TPascalScript.Text (TStrings)

    - Definir as Funções e Procedimentos que poderão ser chamadas no código.
        - Criar objetos que possuam Métodos do tipo:
            procedure(Const Func_Name: String; Stack: TStack);

        - Adicionar as Funções ou Procedimentos através das Propriedades:
            TPascalScript.Functions ou
            TPascalScript.Procs

    - Chamar os métodos: TPascalScript.Compile ou TPascalScript.Execute.

  EXEMPLO:

    type

    TFuncoesMatematicas = class
      procedure AddFunctionsIn(Functions: TFunctionList);
      procedure SQR   (Const Func_Name: String; Stack: TStack);
      procedure POWER (Const Func_Name: String; Stack: TStack);
    end;

    Tpr = ^Real;

    procedure TFuncoesMatematicas.AddFunctionsIn(Functions: TFunctionList);
    begin
      with Functions do
        begin
        Add('SQR'  , 'Calcula o quadrado de um número', 1, SQR);
        Add('POWER', 'Eleva um número a outro', 2, POWER);
        end;
    end;

    procedure TFuncoesMatematicas.POWER(const Func_Name: String; Stack: TStack);
    var x: Real;
    begin
      x := POWER(Stack.AsFloat(1), Stack.AsFloat(2));
      Stack.PushFloat(x);
    end;

    procedure TFuncoesMatematicas.SQR(const Func_Name: String; Stack: TStack);
    var x: Real;
    begin
      x := SQR(Stack.AsFloat(1));
      Stack.PushFloat(x);
    end;

    ...

    var C: TPascalScript;
    begin
      C := TPascalScript.Create;

      // Definicao das funcoes
      TFuncoesMatematicas.AddFunctionsIn(C.Functions);

      C.Text := Memo1.Lines;
      if C.Compile then C.Execute;
      ...

      C.Free;
    end;

  COMO CRIAR UMA BIBLIOTECA

    Como definir objetos:

      1) Descenda uma classe de TObjectService
      2) Defina os métodos de criação dos objetos  (comXXX)
      3) Escreva o médodo de definições de Objetos (AddObjectsIn)

    Como definir Funções e Procedimentos

      1) Descenda uma classe de TFunctionServices para as funções e outra
         para os procedimentos

      2) Defina os métodos de acesso para cada uma das funções e dos procedimentos

      3) Escreva o método de definição de funções e o método de definição de procedimentos


  REGRAS DE ACESSO AOS PARÂMETROS:

    1) Todo acesso a parâmetros através dos métodos Stack.AsXXX(i) deve ser feito
       antes de um Stack.PushXXX(resultado)

    2) O usuário deverá utilizar o método correto Stack.AsXXX(i) de acordo com
       o tipo do parâmetro i

    3) Para melhor performance o usuário deverá utilizar os métodos Stack.AsXXX(i)
       apenas uma vez. Caso seja necessário, copie o valor retornado para uma
       variável do tipo correspondente

    4) O primeiro parâmetro tem ordem 1

    5) Parâmetros do Tipo Object sempre são passados por referência.
       Utilize o método Stack.AsObject

  UTILIZAÇÃO DOS MÉTODOS DA PILHA (STACK):

    Leitura dos Parâmetros por categoria de passagem (referência/valor):

      Integer (valor)      --> Stack.AsInteger (ordem do parâmetro)
      Real    (valor)      --> Stack.AsFloat   (ordem do parâmetro)
      Boolean (valor)      --> Stack.AsBoolean (ordem do parâmetro)
      String  (valor)      --> Stack.AsString  (ordem do parâmetro)
      Object  (referencia) --> Stack.AsObject  (ordem do parâmetro)

      Integer, Real, Boolean, String (referência) -->
        Stack.AsReferency(ordem do parâmetro).Value

      OBS: O método Stack.AsReferency retorna um Objeto do tipo TVariable

    Retorno de Valor:

      Integer  --> Stack.PushInteger
      Real     --> Stack.PushFloat
      Boolean  --> Stack.PushBoolean
      String   --> Stack.PushString
      Object   --> Stack.PushObject
}

interface
uses Windows,
     Classes,
     Contnrs,
     Lists,
     SysUtils,
     psBASE;

const
  // Mensagens de erro
  cMsgTiposNaoCombinam        = 'Tipos não combinam: %s com %s';
  cMsg_Param_TiposNaoCombinam = 'Parâmetro %d: Tipos não combinam (%s com %s)';
  cMsgVariavelDesconhecida    = 'Variável desconhecida: %s';
  cMsgFaltaDeIdentificador    = 'Identificador esperado';
  cMgsFaltaPV                 = '";" esperado';
  cMsgFaltaOpen               = '"(" esperado';
  cMsgFaltaClose              = '")" esperado';
  cMsgFaltaVirgula            = '"," esperada';
  cMsgFaltaPonto              = '"." esperado';
  cMsgFalta2Pontos            = '":" esperado';
  cMsgFaltaDO                 = '"DO" esperado';
  cMsgFaltaTHEN               = '"THEN" esperado';
  cMsgFaltaEND                = '"END" ou ";" esperado';
  cMsgFaltaBEGIN              = '"BEGIN" esperado';
  cMsgFaltaAtrib_ou_Ponto     = '":=" ou "." esperado';
  cMsgParams                  = '. A rotina %s possui %d parâmetro(s)';
  cMsgClassParams             = '. O construtor da classe %s possui %d parâmetro(s)';
  cMsgTypeCastingInvalido     = 'Type-Casting inválido';
  cMsgFaltaTipoRes            = 'Tipo de resultado para a Função esperado';
  cMsgFaltaTipoVar            = 'Tipo da(s) variável(s) esperado';
  cMsgErroParVAR              = 'Parâmetro %d: Somente variáveis são permitidas em Parâmetros VAR';

type
  CharSet    = Set Of Char;

Const
  Operators  : CharSet = ['-','+','*','#','@','/','|','>','<','&',
                          '^','=', '$','~','!','?', ':', ',', '.'];

Type
  TTokenType = (ttInteger, ttFloat, ttFunction, ttProc, ttSymbol, ttComment,
                ttString, ttChar, ttOperator, ttVariable, ttKeyWord, ttClass, ttNull);

  TvmOperators = (operIF,    operGOTO,    operPLUS,   operMINUS, operSTAR,    operSLASH, operMINUSU,
                  operATRIB, operADDPAR,  operCALL,   operCO,    operNEG,     operAND,   operOR,
                  operEQUAL, operNOEQUAL, operGT,     operLT,    operGTE,     operLTE,   operHALT,
                  operDIV,   operMOD,     operRETURN, operLCALL, operADDLPAR, operNOP);

  PVarRec = ^TVarRec;
  TVarRec = Record
            Symbol      : String;
            vType       : TVariableType;
            ObjectClass : TClass;        // Caso a variável seja do tipo OBJECT
            Ender       : Integer;
            IsVar       : Boolean;
            End;

  TProducao = function: Boolean of object;

  function  getStrVarType(T: TVariableType): String;

type
  TPascalScript = Class;

  TSymbolTable = Class(TStringList)
  private
    function getVariable(i: Integer): TVariable;
  public
    Constructor Create;
    Destructor  Destroy; Override;
    Function    Exist(Const S: String): Boolean;
    Function    VarByName(Const Name: String): TVariable;
    Procedure   AddVar(V: TVariable);
    procedure   Delete(Const Name: String);
    procedure   Clear(All: Boolean);

    property Variable[i: Integer]: TVariable read getVariable;
  End;

  TLocalFunc = class;

  TLocalFuncList = Class(TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  // Implementa o escopo de uma sub-rotina ou do bloco principal
  TScope = class
  private
    FProcs: TLocalFuncList;
    FFuncs: TLocalFuncList;
    FVariables: TSymbolTable;
    FSuperScope: TScope;
  public
    constructor Create(SuperScope: TScope);
    destructor Destroy; override;

    procedure Clear;

    // Corrige os endereços das sub-rotinas locais diminuindo de 1 os endereços
    // que são maiores do que "BaseEddress".
    procedure CorrectRoutinesEddress(BaseEddress: Integer);

    // Procura por uma sub-rotina em todo o escopo do bloco, retornando "nil" se não encontrar.
    // SubType é o tipo de lista: 0 para Procs, 1 para Funcs
    function SubByName(const Name: String; SubType: byte): TLocalFunc;

    // Procura por uma Variável em todo o escopo do bloco, retornando "nil" se não encontrar.
    function VarByName(const Name: String): TVariable;

    property SuperScope : TScope         read FSuperScope;
    property Variables  : TSymbolTable   read FVariables;
    property Procs      : TLocalFuncList read FProcs;
    property Funcs      : TLocalFuncList read FFuncs;
  end;

  pParameter = ^TParameter;
  TParameter = record
                 Variable : TVariable;
                 Category : TVariableCat;
               end;

  TParams = class
  private
    FList: TList;
    function getPars(i: byte): pParameter;
    function GetCount: byte;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const Par: TParameter);

    property Pars[i: byte]: pParameter read getPars; default;
    property Count: byte read GetCount;
  end;

  // Implementa as sub-rotinas locais
  TLocalFunc = class
  private
    FScope: TScope;
    FEnder: Integer;
    FResType: TVariableType;
    FName: String;
    FParams: TParams;
  public
    constructor Create(SuperScope: TScope);
    destructor Destroy; override;

    property Name : String read FName write FName;
    property Scope: TScope read FScope;
    property Ender: Integer read FEnder write FEnder;
    property ResType: TVariableType read FResType write FResType;
    property Params: TParams read FParams;
  end;

  TToken = class
  Private
    FType : TTokenType;
    FText : String;   {Representação Interna}
    FLin  : Integer;
    FCol  : Integer;

    Function GetFloat: Extended;
    Function GetString: String;
    Function GetInteger: Integer;
  Public
    Constructor Create(Const Token: String; TokenType: TTokenType; Lin,Col: Integer);

    Property Lin       : Integer    Read FLin        Write FLin;
    Property Col       : Integer    Read FCol        Write FCol;
    Property TokenType : TTokenType Read FType;
    Property AsString  : String     Read GetString;
    Property AsInteger : Integer    Read GetInteger;
    Property AsFloat   : Extended   Read GetFloat;
  End;

  TAbstractScanner = class
  private
    FDestruirLib : Boolean;
    FNullToken   : TToken;
    FLib         : TLib;
    FStack       : TopStack;
    FKeyWords    : TStrings;      {If, Then, Begin}
    FText        : TStrings;      {Texto a ser analisado}
    FTokens      : TList;         {Lista de Tokens depois da chamada de Scan}
    FPos         : Integer;       {Posição do Token Atual}
    FLin         : Integer;       {Linha do Token}
    FLinhaAtual  : Integer;       {Linha atual que o Scanner está lendo}
    FCompiler    : TPascalScript;

    Procedure SetToken(Index: Integer; Token: TToken);
    Function  GetToken(Index: Integer): TToken;
    Function  GetTokensCount: Integer;
    procedure SetText(const Value: TStrings);
  protected
    FState     : Byte;          {Estado atual do autômato}
    PosInic    : Integer;       {Posição inicial do Token atual}
    FListError : TStrings;      {Lista de Erros}
  public
    Constructor Create(Compiler: TPascalScript; aLib: TLib);
    Destructor Destroy; Override;

    Procedure Scan;
    Function  Analise_Lexica(Const Expr: String; Lin: Integer): Integer;
    Procedure ReplaceToken(i: Integer; Const S: String; TokenType: TTokenType);
    Function  GotoToken(n: Integer): TToken;
    Function  MakeString: String;
    Procedure ClearTokens;
    Function  First: TToken;
    Function  Next : TToken;
    Function  Prev : TToken;
    Function  MoveBy(Delta: Integer): TToken;
    Procedure Advance;
    Procedure AdvanceUntil(Const ST: Array of String);

    Function  Automaton {Parser's Brain}
       (Const Expr: String; Var Pos: Integer; Var Token: String): Boolean; Virtual; Abstract;

    Property Token[Index: Integer]: TToken Read GetToken         Write SetToken;
    Property TokensCount: Integer          Read GetTokensCount;

    Property Stack      : TopStack         Read FStack           Write FStack;
    Property Pos        : Integer          Read FPos             Write FPos;
    Property Lib        : TLib             Read FLib             Write FLib;
    Property KeyWords   : TStrings         Read FKeyWords        Write FKeyWords;
    Property Lin        : Integer          Read FLin             Write FLin;
    Property ListError  : TStrings                               Write FListError;
    property Text       : TStrings         Read FText            Write SetText;

    //property Compiler   : TPascalScript    Read FCompiler        Write FCompiler;
  end {TAbstractParser};

  TPascalScanner = Class(TAbstractScanner)
  public
    Function Automaton
      (Const Expr: String; Var Pos: Integer; Var Token: String): Boolean; Override;
  End;

  PLineCode  = ^TLineCode;
  TLineCode  = Record
                 Operacao: TvmOperators;
                 Op1     : String;
                 Op2     : String;
                 Res     : Integer;
                 i_Op1   : Integer;
                 i_Op2   : Integer;
                 T1      : Shortint;
                 T2      : Shortint;
               End;

  TByteCode = Class(TList)
  Private
    FNTemps: Integer;
    FLockTemps: TIntegerList;
    FCompiler: TPascalScript;

    function  GetLineCode(i: Integer): pLinecode;
    procedure RemoveLine(i: Integer);
  Public
    Operacao     : TvmOperators;
    Op1          : String;
    Op2          : String;
    TMP          : String;
    Res          : Integer;
    i_Op1        : Integer;
    i_Op2        : Integer;
    T1           : Shortint;
    T2           : Shortint;
    MaxTemp      : Integer;

    Constructor Create(Compiler: TPascalScript);
    Destructor Destroy; Override;

    Procedure  Init;
    Procedure  Clear;
    Procedure  AddLine; overload;
    Procedure  AddLine(const Code: TLineCode); overload;
    function   CurrentOrdTemp: Integer;
    Function   NewTemp: String;
    procedure  Lock(OrdTemp: Integer);
    procedure  UnLock(OrdTemp: Integer);
    procedure  Optimize;
    function   OperToStr(Oper: TvmOperators): String;

    Property   LineCode[I: Integer]: pLineCode Read GetLineCode;
    //property   Compiler: TPascalScript read FCompiler;
  End;

  // indica a mudança do Program Count (linha atual de execução)
  TPC_Change = procedure(Sender: TObject; PC: Integer) of Object;

  // indica o método de execução de uma operação
  TExecuteMethod = procedure(Code: pLineCode; var PC: Integer) of Object;

  TVirtualMachine = class
  private
    FBC: TByteCode;
    FLib: TLib;
    FPC_Change: TPC_Change;
    FAE: TNotifyEvent;
    FBE: TNotifyEvent;
    FTerminated: Boolean;

    Code         : pLineCode;    // Linha atual do ByteCode
    Stack        : TexeStack;    // Pilha de Execução
    FReturnStack : TStack;       // Pilha com os enderecos de retorno das rotinas locais


    // array com os endereções dos métodos dos operadores
    FExecute : Array[TvmOperators] of TExecuteMethod;

    procedure ExecuteIF      (Code: pLineCode; var PC: Integer);   // IF
    procedure ExecuteGOTO    (Code: pLineCode; var PC: Integer);   // GOTO
    procedure ExecutePLUS    (Code: pLineCode; var PC: Integer);   // +
    procedure ExecuteMINUS   (Code: pLineCode; var PC: Integer);   // -
    procedure ExecuteSTAR    (Code: pLineCode; var PC: Integer);   // *
    procedure ExecuteSLASH   (Code: pLineCode; var PC: Integer);   // /
    procedure ExecuteMINUSU  (Code: pLineCode; var PC: Integer);   // - (UNARIO)
    procedure ExecuteATRIB   (Code: pLineCode; var PC: Integer);   // :=
    procedure ExecuteDIV     (Code: pLineCode; var PC: Integer);   // DIV
    procedure ExecuteMOD     (Code: pLineCode; var PC: Integer);   // MOD
    procedure ExecuteADDPAR  (Code: pLineCode; var PC: Integer);   // ADD PAR
    procedure ExecuteADDLPAR (Code: pLineCode; var PC: Integer);   // ADD LPAR
    procedure ExecuteCALL    (Code: pLineCode; var PC: Integer);   // CALL
    procedure ExecuteLCALL   (Code: pLineCode; var PC: Integer);   // LCALL
    procedure ExecuteCO      (Code: pLineCode; var PC: Integer);   // CO
    procedure ExecuteNEG     (Code: pLineCode; var PC: Integer);   // ~
    procedure ExecuteAND     (Code: pLineCode; var PC: Integer);   // AND
    procedure ExecuteOR      (Code: pLineCode; var PC: Integer);   // OR
    procedure ExecuteEQUAL   (Code: pLineCode; var PC: Integer);   // =
    procedure ExecuteNOEQUAL (Code: pLineCode; var PC: Integer);   // <>
    procedure ExecuteGT      (Code: pLineCode; var PC: Integer);   // >
    procedure ExecuteLT      (Code: pLineCode; var PC: Integer);   // <
    procedure ExecuteGTE     (Code: pLineCode; var PC: Integer);   // >=
    procedure ExecuteLTE     (Code: pLineCode; var PC: Integer);   // <=
    procedure ExecuteHALT    (Code: pLineCode; var PC: Integer);   // HALT
    procedure ExecuteRETURN  (Code: pLineCode; var PC: Integer);   // RETURN
    procedure ExecuteNOP     (Code: pLineCode; var PC: Integer);   // NOP

    function  ObtemOperador(Code: pLineCode; OP: Byte): Real;
    function  ObtemOperadorInt(Code: pLineCode; OP: Byte): Integer;
    function  ObtemOperadorBool(Code: pLineCode; OP: Byte): Boolean;
    function  ObtemString(Code: pLineCode; OP: Byte): String;
    procedure LiberaParametros(Rotina: TFunctionObject; Eh_Metodo: Boolean);
    procedure LiberaParametrosDaClasse(c: TpsClass);
  public
    constructor Create(ByteCode: TByteCode; Lib: TLib);
    destructor Destroy; override;

    procedure Execute;
    procedure Stop;

    property ByteCode  : TByteCode  read FBC   write FBC;
    property Lib       : TLib       read FLib  write FLib;

    // Eventos
    property OnPC_Change     : TPC_Change     read FPC_Change write FPC_Change;
    property OnBeforeExecute : TNotifyEvent   read FBE        write FBE;
    property OnAfterExecute  : TNotifyEvent   read FAE        write FAE;
  end;

  TPascalScript = Class
  private
    FFilename: string;
    function GetCurrentToken: String;
    function GetCurrentTokenType: TTokenType;
  private
    FScope         : TScope;                   // Escopo do bloco principal
    FPar           : TParameter;               // Usado no reconhecimemto de parâmetros
    FLib           : TLib;                     // Guarda as informações das bibliotecas
    FAssignedLib   : Boolean;                  // Diz se uma biblioteca foi associada ao script
    FBC            : TByteCode;                // Guarda o código gerado pela compilação
    FScanner       : TPascalScanner;           // Obem os tokens
    FGerCODE       : Boolean;                  // Indicador para geração de código
    FErro          : Integer;                  // Indicador de FErro de sintaxe
    FErros         : TStrings;                 // Guarda os erros durante a compilação
    FITV           : Integer;                  // Índices dos primeiros Tokens Variáveis reconhecidos
    FCompiled      : Boolean;                  // Indica se o codigo foi compilado
    FVM            : TVirtualMachine;          // Executora do código
    FOptimize      : Boolean;                  // Otimizar código
    FGlobalObjects : TObject;
    FEconomize     : Boolean;

    Function ReconhecePrograma: Boolean;
//    Function GetError(Token: TToken): String;
    Function ObtemEnderOper(const OP: String): Integer;
    Function obtem_vmOperator(const Oper: String): TvmOperators;
    Function AddVar(const Name: String): TVariable;
    
    Function  S                            : Boolean;

    Function  BD                           : Boolean; // Bloco de declaração
    function  VarSection                   : Boolean;
    function  VarDecl(Params: TParams)     : Boolean; // Declaração de Params. de Rotinas ou Vars. Globais/Locais
    Function  IdentList(Params: TParams)   : Boolean; // Lista de Identificadores
    Function  DSR                          : Boolean; // Declaração de Sub-Rotinas
    Function  Tipo(out vt: TVariableType)  : Boolean;

    Function  FormalParams(Sub: TLocalFunc): Boolean;
    Function  Parameters(Sub: TLocalFunc)  : Boolean;
    Function  Parameter(Sub: TLocalFunc)   : Boolean;

    Function   BC                          : Boolean;
    Function   SC                          : Boolean;
    Function    C                          : Boolean;
    Function   CO                          : Boolean;
    Function   CC                          : Boolean;
    Function   ST                          : Boolean;

    Function    E                          : Boolean;
    Function   El                          : Boolean;
    Function    F                          : Boolean;
    Function   Fl                          : Boolean;
    Function    G                          : Boolean;
    Function   Gl                          : Boolean;
    Function    O                          : Boolean;
    Function    T                          : Boolean;

    Function CC_Bloco                       : Boolean;
    Function CC_IF                          : Boolean;
    Function CC_WHILE                       : Boolean;
    Function CC_FOR                         : Boolean;

    procedure GerarRETURN;
    procedure GerarHALT;
    procedure GerarGOTO(Linha: Integer);

    procedure AvaliaObjeto(v: TVariable; var Symbol: String);

    procedure GeraCodigoParaParametrosDaClasse(c: TpsClass);
    function  GeraCodigoParaClasse(v: TVariable; const Symbol: String): Boolean;
    procedure GeraCodigoParaRotinaLocal(Sub: TLocalFunc);
    procedure GeraCodigoParaMetodo(v: TVariable; var Symbol: String);
    procedure GeraCodigoParaRotina(F: TFunctionObject; const TipoRotina: String; IndRot,
                                   IndObj: Integer; const ObjectName: String);

    function  AtribuiTipo(I1, I2: Integer; vt: TVariableType): Boolean;
    function  Combinam(T1, T2: TVariableType; const Op: String): TVariableType;
    function  getText: TStrings;
    function  VerificaCompatibilidade(t1, t2: TVariableType; ParOrd: byte = 0): Boolean;
//    function  VerificaCompatibilidadeVal(t: TVariableType): Boolean;
    function  VerificaCompatibilidadeDeClasses(c1, c2: TClass; ParOrd: Integer): Boolean;
//    function  VerificaDeclaracaoVar(const Nome: String): TVariable;

    procedure AddError(Erro: String;
                       const AdvanceUntil: array of String;
                       Producoes: array of TProducao);
                       
    procedure AddErrorFmt(Erro: String;
                          const Pars: Array of const;
                          const AdvanceUntil: array of String;
                          Producoes: array of TProducao);

    procedure InverteCodigo(BCTemp, BC: TByteCode);
    procedure SetGlobalObjects(const Value: TObject);
    procedure SetEconomize(const Value: Boolean);
    function  GetVariables: TSymbolTable;

    property  CurrentToken: String read GetCurrentToken;
    property  CurrentTokenType: TTokenType read GetCurrentTokenType;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure AssignLib(Lib: TLib);

    function  Compile(): Boolean;
    procedure Execute();
    procedure Stop();

    procedure Include(API: TAPI_Proc);

    property Variables      : TSymbolTable    read GetVariables;
    property Scanner        : TPascalScanner  read FScanner;
    property ByteCode       : TByteCode       read FBC;
    property VirtualMachine : TVirtualMachine read FVM;

    property Lib       : TLib     read FLib;
    property Errors    : TStrings read FErros;
    property Code      : TStrings read getText;
    property Compiled  : Boolean  read FCompiled  write FCompiled;
    property GerCODE   : Boolean  read FGerCODE   write FGerCODE;    // false
    property Optimize  : Boolean  read FOptimize  write FOptimize;   // false
    property Economize : Boolean  read FEconomize write SetEconomize;  // true
    property Filename  : string   read FFilename  write FFilename;

    property GlobalObjects : TObject read FGlobalObjects write SetGlobalObjects;
  end;

implementation
uses Messages,
     Lib_GlobalObjects;

function getStrVarType(T: TVariableType): String;
begin
  case T of
    pvtReal     : Result := 'Real';
    pvtInteger  : Result := 'Integer';
    pvtBoolean  : Result := 'Boolean';
    pvtString   : Result := 'String';
    pvtObject   : Result := 'Object';
    pvtNull     : Result := 'Tipo desconhecido';
    else
      Result := 'Tipo desconhecido';
  end;
end;

{ TSymbolTable }

Constructor TSymbolTable.Create;
Begin
  Inherited Create;
  Sorted := True;
End;

Destructor TSymbolTable.Destroy;
Begin
  Clear(True);
  Inherited Destroy;
End;

function TSymbolTable.getVariable(i: Integer): TVariable;
begin
  Result := TVariable(Objects[i]);
end;

Function TSymbolTable.Exist(Const S: String): Boolean;
Begin
  Result := (IndexOf(S) <> -1);
End;

Function TSymbolTable.VarByName(Const Name: String): TVariable;
Var i: Integer;
Begin
  i := IndexOf(Name);
  If (i > -1) Then
     Result := TVariable(Objects[i])
  Else
     Result := nil;
End;

Procedure TSymbolTable.AddVar(V: TVariable);
Begin
  if IndexOf(V.Name) = -1 then  
     AddObject(V.Name, V)
  else
     V.Free;
End;

procedure TSymbolTable.Delete(const Name: String);
var i: Integer;
begin
  i := IndexOf(Name);
  if i > -1 then
     begin
     TVariable(Objects[i]).Free;
     Inherited Delete(i);
     end;
end;

procedure TSymbolTable.Clear(All: Boolean);
var i: Integer;
begin
  For i := Count-1 DownTo 0 do
    if All or not TVariable(Objects[i]).Locked then
       begin
       TVariable(Objects[i]).Free;
       inherited Delete(i);
       end;
end;

{ TToken }

Constructor TToken.Create(Const Token: String; TokenType: TTokenType; Lin,Col: Integer);
Begin
  Inherited Create;
  FLin := Lin;
  FCol := Col;
  FText := Token;
  FType := TokenType;
End;

Function TToken.GetFloat: Extended;
Begin
  Try
    Result := StrToFloat(FText);
  Except
    Raise Exception.Create('Erro de Conversão' + #13#10 +
    'Token não pode ser convertido para um Ponto Flutuante (Real)');
  End;
End;

Function TToken.GetString: String;
Begin
  Result := FText;
End;

Function TToken.GetInteger: Integer;
Begin
  Try
    Result := StrToInt(FText);
  Except
    Raise Exception.Create('Erro de Conversão' + #13#10 +
    'Token não pode ser convertido para um Inteiro');
  End;
End;

{ TAbstractParser }

Constructor TAbstractScanner.Create(Compiler: TPascalScript; aLib: TLib);
Begin
  Inherited Create;
  FCompiler := Compiler;
  FPos := -1;

  if aLib <> nil then
     FLib := aLib
  else
     begin
     FDestruirLib := True;
     FLib := TLib.Create;
     end;

  FKeyWords  := TStringList.Create;
  FText      := TStringList.Create;
  FTokens    := TList.Create;
  FStack     := TopStack.Create;
  FNullToken := TToken.Create('', ttNull, 0, 0);
End;

Destructor TAbstractScanner.Destroy;
Begin
  FNullToken.Free;
  if FDestruirLib then FLib.Free;
  FKeyWords.Free;
  FText.Free;
  ClearTokens;
  FTokens.Free;
  FStack.Free;
  Inherited Destroy;
End;

Procedure TAbstractScanner.ClearTokens;
Var i: Integer;
Begin
  FPos := -1;
  FLinhaAtual := -1;
  For i := 0 to FTokens.Count-1 do
    TToken(FTokens[i]).Free;
  FTokens.Clear;
End;

Procedure TAbstractScanner.Scan;
Var i : Integer;
Begin
  ClearTokens;
  FState := 0; {Estado inicial}
  For i := 0 to FText.Count-1 do
    Analise_Lexica(FText[i] + #0, i);
End;

Function TAbstractScanner.Analise_Lexica(Const Expr: String; Lin: Integer): Integer;
var Pos     : Integer;
    Erro    : Integer;

   Function Lexico(Var Pos: Integer; Var Erro: Integer): Boolean;
   const
       ZERO = 48;
       NOVE = 57;
   var
       Tip         : TTokenType;
       Token       : String;
       b           : byte;

     function IsSpecialChar(const Token: String; out b: byte): boolean;
     var i: Integer;
     begin
       Result := False;
       if Token[1] = '#' then
          begin
          i := StrToInt(copy(Token, 2, 4));
          Result := (i <= $FF);
          end;

       if Result then b := byte(i);
     end;

     Function IsFunction(Const Token: String): Boolean;
     begin
       Result := (FLib.Functions.IndexOf(Token) > -1);
     end;

     Function IsProc(Const Token: String): Boolean;
     begin
       Result := (FLib.Procs.IndexOf(Token) > -1);
     end;

     Function IsKeyWord(Const Token: String): Boolean;
     begin
       Result := (FKeyWords.IndexOf(Token) > -1);
     end;

     Function IsClass(Const Token: String): Boolean;
     begin
       Result := (FLib.Classes.IndexOf(Token) > -1);
     end;

     Function Tipo(var Token: String): TTokenType;
     begin
       Result := ttNull;
       if (ord(Token[1]) >= ZERO) and (ord(Token[1]) <= NOVE) then
          {
          Try
            StrToInt(Token);
            Result := ttInteger;
          Except
            Result := ttFloat;
          End
          }
          begin
          if System.Pos('.', Token) = 0 then
             Result := ttInteger
          else
             Result := ttFloat;
          end

       else if (Token[1] = '(') or (Token[1] = ')') or (Token[1] = ';') Then
         Result := ttSymbol

       else if (Token[1] = '{') Then
         Result := ttComment

       else if (Token[1] = #39) then
         Result := ttString

       else if IsSpecialChar(Token, b) Then
         begin
         Token := ' ' + char(b) + ' ';
         Result := ttString;
         end

       else if Token[1] in Operators then
         Result := ttOperator

       else if IsFunction(Token) then
          Result := ttFunction

       else if IsProc(Token) then
          Result := ttProc

       else if IsKeyWord(Token) then
          Result := ttKeyWord

       else if IsClass(Token) then
          Result := ttClass   

       else if ((ord(Token[1]) >= Ord('A')) and (ord(Token[1]) <= Ord('Z'))) or
               ((ord(Token[1]) >= Ord('a')) and (ord(Token[1]) <= Ord('z'))) then
               Result := ttVariable

       else If (Token[1] = #0) Then
         {Final da String}

       else
         Raise Exception.CreateFmt('Token < %s > não reconhecido', [Token]);
     end; { TipoToken }

  Begin { Lexico }
    PosInic := Pos;
    If Automaton(Expr, Pos, Token) Then
       Begin
       Tip := Tipo(Token);

       If Tip = ttString Then
          FTokens.Add(TToken.Create(Token, ttString, Lin+1, PosInic))
       Else
          FTokens.Add(TToken.Create(UpperCase(Token), Tip, Lin+1, PosInic));

       Result := True;
       End
    Else
       Result := False;
  End; { Lexico }

Begin {Analise_Lexica}
  Pos := 1;
  Erro := 0;
  While (Pos <= Length(Expr)) and (Erro = 0) Do Lexico(Pos, Erro);
  Result := Erro;
End; { Analise_Lexica }

Function TAbstractScanner.GotoToken(n: Integer): TToken;
Begin
  Result := Nil;
  If (n > -1) and (n < FTokens.Count-1) Then
    Begin
    FPos := n;
    Result := TToken(FTokens[FPos]);
    End;
End;

Procedure TAbstractScanner.ReplaceToken(i: Integer; Const S: String; TokenType: TTokenType);
Begin
  Try
    TToken(FTokens[i]).Free;
    FTokens[i] := TToken.Create(S, TokenType, 0, 0);
  Except
    Raise Exception.Create('Método <ReplaceToken>.'+#13#10+'Índice do Token Inválido');
  End;
End;

Function TAbstractScanner.MakeString: String;
Var i: Integer;
Begin
  Result := '';
  For i:= 0 to FTokens.Count-1 do
    Result := Result + TToken(FTokens[i]).AsString;
End;

Function  TAbstractScanner.First: TToken;
Begin
  FPos := -1;
  FLinhaAtual := -1;
  Result := Next;
End;

Function  TAbstractScanner.Next: TToken;
Begin
  Inc(FPos);
  Try
    Result := TToken(FTokens[FPos]);
  Except
    Inc(FLinhaAtual);
    While Trim(FText[FLinhaAtual]) = '' Do Inc(FLinhaAtual);
    Analise_Lexica(FText[FLinhaAtual], FLinhaAtual);
    Result := TToken(FTokens[FPos]);
  End;
End;

Function  TAbstractScanner.Prev: TToken;
Begin
  Dec(FPos);
  Try
    Result := TToken(FTokens[FPos])
  Except
    FPos := 0; {Primeiro}
    Result := Nil;
  End;
End;

Procedure TAbstractScanner.Advance;
Begin
  If FPos < FTokens.Count - 1 Then Inc(FPos);
End;

Procedure TAbstractScanner.AdvanceUntil(Const ST: Array of String);

  Function Achou: Boolean;
  var i: Byte;
  Begin
    Result := False;
    For i := Low(ST) to High(ST) do
      If CompareText(TToken(FTokens[FPos]).AsString, ST[i]) = 0 Then
         Begin
         Result := True;
         Break;
         End;
  End;

Begin
  While Not Achou and (FPos < FTokens.Count-1) Do Inc(FPos);
End;

Function  TAbstractScanner.MoveBy(Delta: Integer): TToken;
Var Aux,i: Integer;
Begin
  Aux := FPos;
  FPos := FPos + Delta;
  Try
    Result := TToken(FTokens[FPos]);
  Except
    FPos := Aux;
    For i := (FPos + 1) to (FPos + Delta) do
       Result := Next;
  End;
End;

Procedure TAbstractScanner.SetToken(Index: Integer; Token: TToken);
Begin
  TToken(FTokens[Index]).Free;
  FTokens[Index] := Token;
End;

Function  TAbstractScanner.GetToken(Index: Integer): TToken;
Begin
  if index < FTokens.Count then
     Result := TToken(FTokens[Index])
  else
     Result := FNullToken;
End;

Function  TAbstractScanner.GetTokensCount: Integer;
Begin
  Result := FTokens.Count;
End;

{ TPascalScanner }

Function TPascalScanner.Automaton
  (Const Expr: String; Var Pos: Integer; Var Token: String): Boolean;

var p: Byte;
Begin
  Token := '';
  p := Pos-1;
  Repeat
    inc(p);
    case FState of
      0:
      begin
      case Expr[p] of
        #32, #8: {Nada};

        #0: Begin
            Result := False;
            Pos := Length(Expr) + 1; {Para forçar uma próxima linha}
            FState := 0; {Estado inicial}
            Exit;
            End;

        'A'..'Z', 'a'..'z', '_'                     : FState := 1;
        '0'..'9'                                    : FState := 2;
        ':', '>'                                    : FState := 7;
        '<'                                         : FState := 17;
        '{'                                         : FState := 11;
        #39                                         : FState := 12;
        '+', '-', '=', '(', ')', '*', ',', '.', ';' : FState := 6;
        '/'                                         : FState := 15;
        '#'                                         : FState := 10;
        else
          FState := 8;
        end;
      if (Expr[p] <> #32) then Token := Expr[p];
      end;

      1  :case Expr[p] of
              'A'..'Z', 'a'..'z', '0'..'9', '_'  :Token := Token + Expr[p];
              else
                FState := 5;
              end;

      2  :case Expr[p] of
             '0'..'9': Token := Token + Expr[p];
             '.': begin
                  Token := Token + Expr[p];
                  FState := 3;
                  end;
             else
               FState := 5;
             end;

       3  :case Expr[p] of
             '0'..'9': begin
                       Token := Token + Expr[p];
                       FState := 4;
                       end;
             else
               FState := 8;
             end;

       4  :case Expr[p] of
             '0'..'9': Token := Token + Expr[p];
             'E', 'e': Begin
                       Token := Token + Expr[p];
                       FState := 9;
                       End;
             else
               FState := 5;
             end;

       // Estado Final com devolução do último caracter lido
       5  :begin
             pos := p-1;
             Result := True;
             FState := 0; {Estado inicial}
             exit;
           end;

       // Estado Final sem devolução do último caracter lido
       6  :begin
             pos := p;
             Result := True;
             FState := 0; {Estado inicial}
             exit;
          end;

       7  :case Expr[p] of
             '=' : begin
                   Token := Token + Expr[p];
                   FState := 6;
                   end;
             else
               FState := 5;
             end;

       8  :begin
             Pos := p;
             If Assigned(FListError) Then
                FListError.Add(Format('Erro: Caracter não reconhecido. ' +
                'Expressão: < %s > Posição [%d, %d] ', [Expr, Lin, PosInic + 1]));
             Result := False;
             FState := 0; {Estado inicial}
             Exit;
           end;

       9  :case Expr[p] of
             '-', '+',
             '0'..'9': Begin
                       Token := Token + Expr[p];
                       FState := 10;
                       End;
             else
               FState := 5;
             end;

       10 :case Expr[p] of
             '0'..'9' :Token := Token + Expr[p];
             else
               FState := 5;
             end;

       11 :case Expr[p] of
             '}': FState := 0;
             #0 : begin
                  Pos := Length(Expr) + 1;
                  Exit;
                  end;
             Else
               FState := 11;
           end;

       12 :case Expr[p] of
             #39 : Begin
                   FState := 6;
                   Token := Token + Expr[p];
                   End;
             #0: FState := 14;
             else
               Token := Token + Expr[p];
             end;

       13 :begin
             Pos := p;
             If Assigned(FListError) Then
                FListError.Add(Format('Erro: Comentário inválido. Caracter < } > não encontrado. ' +
                'Token: < %s >  [Lin: %d, Col %d]', [Token, Lin, PosInic + 1]));
             Result := False;
             FState := 0; {Estado inicial}
             Exit;
           end;

       14 :begin
             Pos := p;
             If Assigned(FListError) Then
                FListError.Add(Format('Erro: String inválida. Caracter < " ou '+ #39 + '> não encontrado. ' +
                'Token: < %s >  [Lin: %d, Col %d]', [Token, Lin, PosInic + 1]));
             Result := False;
             FState := 0; {Estado inicial}
             Exit;
           end;

       15: case Expr[p] of
             '/': FState := 16;
             else
               begin
               Token := '/';
               FState := 5;
               end;
             end;

       16: case Expr[p] of
             #0: begin
                 FState := 0;
                 Pos := Length(Expr) + 1;
                 Exit;
                 end;
             else
               FState := 16;
             end;

       17: case Expr[p] of
             '=', '>' : begin
                        Token := Token + Expr[p];
                        FState := 6;
                        end;
             else
               FState := 5;
             end;
      End;
  Until FALSE;
End; { TPascalParser.Automaton }

procedure TAbstractScanner.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
  FCompiler.Compiled := False;
end;

{ TByteCode }

constructor TByteCode.Create(Compiler: TPascalScript);
begin
  Inherited Create;
  FCompiler := Compiler;
  i_Op1 := -1;
  i_Op2 := -1;
  T1 := -1;
  T2 := -1;
end;

Destructor TByteCode.Destroy;
Begin
  Clear;
  if FLockTemps <> nil then FLockTemps.Free;
  Inherited Destroy;
End;

Procedure TByteCode.Init;
Begin
  FNTemps := 0;
End;

Procedure TByteCode.Clear;
var i: Integer;
Begin
  Init;
  For i := 0 to Count - 1 do Dispose(PLineCODE(Items[i]));
  Inherited Clear;
End;

Function TByteCode.GetLineCode(i: Integer): pLineCode;
Begin
  Result := pLineCode(Items[i]);
End;

Function TByteCode.NewTemp: String;
var i: Integer;
Begin
  if FLockTemps <> nil then
     repeat
       Inc(FNTemps);
     until not FLockTemps.Exists(FNTemps, i)
  else
     Inc(FNTemps);

  If FNTemps > MaxTemp Then MaxTemp := FNTemps;
  TMP := '@@' + IntToStr(FNTemps);
  Result := TMP;

  if not FCompiler.Variables.Exist(Result) then
     FCompiler.Variables.AddVar(TVariable.Create(Result, pvtReal, 0, nil, False));
End;

Procedure TByteCode.AddLine;
var P: PLineCODE;
Begin
  New(P);

  P.Operacao := Operacao;
  P.Op1      := Op1;
  P.Op2      := Op2;
  P.Res      := Res;
  P.i_Op1    := i_Op1;
  P.i_Op2    := i_Op2;
  P.T1       := T1;
  P.T2       := T2;

  Add(P);
End;

procedure TByteCode.RemoveLine(i: Integer);
var ii: Integer;
    LC: PLineCode;
begin
  // Recalcula o endereço das sub-rotinas locais
  FCompiler.FScope.CorrectRoutinesEddress(i);

  Dispose(PLineCODE(Items[i]));
  Delete(i);
  for ii := 0 to Count-1 do
    begin
    LC := PLineCode(items[ii]);
    if (LC.Operacao = operIF) then
       begin
       if LC.i_Op1 > i then dec(LC.i_Op1);
       if LC.i_Op2 > i then dec(LC.i_Op2);
       end
    else
    if (LC.Operacao = operGOTO) then
       if LC.i_Op1 > i then dec(LC.i_Op1);
    end;
end;

procedure TByteCode.Optimize;
var i: Integer;
    LCAt, LCAn: PLineCode;
begin
  // Remoção de códogo inútil
  i := 1;
  while i < Count-1 do
    begin
    LCAn := PLineCode(items[i-1]);
    LCAt := PLineCode(items[i]);

    // Remoção de temporários inúteis
    if (LCAt.Operacao = operATRIB) and (LCAn.Operacao <> operLCALL) and
       (LCAt.Res <> -1) and (LCAt.i_op1 <> -1) and (LCAt.i_op1 = LCAn.Res) then
       begin
       LCAn.Res := LCAt.Res;
       RemoveLine(i);
       Continue;
       end;

    // Remoção de comandos de atribuição inúteis
    if (LCAn.Operacao = operATRIB) and (LCAt.Operacao = operATRIB) then
       if (LCAt.Res <> -1) and (LCAn.Res = LCAt.Res) then
          begin
          RemoveLine(i-1);
          Continue;
          end;

    inc(i);
    end; // while
end;

function TByteCode.CurrentOrdTemp: Integer;
begin
  Result := FNTemps;
end;

procedure TByteCode.Lock(OrdTemp: Integer);
begin
  if FLockTemps = nil then
     FLockTemps := TIntegerList.Create;

  FLockTemps.Add(OrdTemp);
end;

procedure TByteCode.UnLock(OrdTemp: Integer);
begin
  if FLockTemps <> nil then
     begin
     FLockTemps.DeleteVal(OrdTemp);
     if FLockTemps.Count = 0 then FreeAndNil(FLockTemps);
     end;
end;

procedure TByteCode.AddLine(const Code: TLineCode);
var P: PLineCODE;
Begin
  New(P);
  P^ := Code;
  Add(P);
End;

function TByteCode.OperToStr(Oper: TvmOperators): String;
begin
  case Oper of
    operIF      : Result := 'IF';
    operGOTO    : Result := 'GOTO';
    operPLUS    : Result := '+';
    operMINUS   : Result := '-';
    operSTAR    : Result := '*';
    operSLASH   : Result := '/';
    operMINUSU  : Result := '!';
    operATRIB   : Result := ':=';
    operDIV     : Result := 'DIV';
    operMOD     : Result := 'MOD';
    operADDPAR  : Result := 'ADD PAR';
    operADDLPAR : Result := 'ADD LPAR';
    operCALL    : Result := 'CALL';
    operLCALL   : Result := 'LCALL';
    operCO      : Result := 'CO';
    operNEG     : Result := 'NEG';
    operAND     : Result := 'AND';
    operOR      : Result := 'OR';
    operEQUAL   : Result := '=';
    operNOEQUAL : Result := '<>';
    operGT      : Result := '>';
    operLT      : Result := '<';
    operGTE     : Result := '>=';
    operLTE     : Result := '<=';
    operHALT    : Result := 'HALT';
    operRETURN  : Result := 'RETURN';
    operNOP     : Result := 'NOP';
  end;
end;

{ TVirtualMachine }

constructor TVirtualMachine.Create(ByteCode: TByteCode; Lib: TLib);
begin
  inherited Create;
  FBC := ByteCode;
  FLib := Lib;
  Stack := TexeStack.Create;
  FReturnStack := TStack.Create;

  // inicialização do vetor com os endereços dos métodos de execução das operações
  FExecute[operIF     ] := ExecuteIF;        // IF
  FExecute[operGOTO   ] := ExecuteGOTO;      // GOTO
  FExecute[operPLUS   ] := ExecutePLUS;      // +
  FExecute[operMINUS  ] := ExecuteMINUS;     // -
  FExecute[operSTAR   ] := ExecuteSTAR;      // *
  FExecute[operSLASH  ] := ExecuteSLASH;     // /
  FExecute[operMINUSU ] := ExecuteMINUSU;    // - (UNARIO)
  FExecute[operATRIB  ] := ExecuteATRIB;     // :=
  FExecute[operDIV    ] := ExecuteDIV;       // DIV
  FExecute[operMOD    ] := ExecuteMOD;       // MOD
  FExecute[operADDPAR ] := ExecuteADDPAR;    // ADD PAR
  FExecute[operADDLPAR] := ExecuteADDLPAR;   // ADD LPAR
  FExecute[operCALL   ] := ExecuteCALL;      // CALL
  FExecute[operLCALL  ] := ExecuteLCALL;     // LCALL
  FExecute[operCO     ] := ExecuteCO;        // CO
  FExecute[operNEG    ] := ExecuteNEG;       // ~
  FExecute[operAND    ] := ExecuteAND;       // AND
  FExecute[operOR     ] := ExecuteOR;        // OR
  FExecute[operEQUAL  ] := ExecuteEQUAL;     // =
  FExecute[operNOEQUAL] := ExecuteNOEQUAL;   // <>
  FExecute[operGT     ] := ExecuteGT;        // >
  FExecute[operLT     ] := ExecuteLT;        // <
  FExecute[operGTE    ] := ExecuteGTE;       // >=
  FExecute[operLTE    ] := ExecuteLTE;       // <=
  FExecute[operHALT   ] := ExecuteHALT;      // HALT
  FExecute[operRETURN ] := ExecuteRETURN;    // RETURN
  FExecute[operNOP    ] := ExecuteNOP;       // NOP
end;

destructor TVirtualMachine.Destroy;
begin
  FReturnStack.Free;
  Stack.Free;
  inherited;
end;

function TVirtualMachine.ObtemOperador(Code: pLineCode; OP: Byte): Real;
var s: String;
    i: Integer;
    Cod: Integer;
begin
  if OP = 1 then
     begin
     s := Code.Op1;
     i := Code.i_Op1;
     end
  else
     begin
     s := Code.Op2;
     i := Code.i_Op2;
     end;

  if i > -1 then
     Result := TVariable(i).Value 
  else
     Val(s, Result, Cod);
end;

function TVirtualMachine.ObtemOperadorInt(Code: pLineCode; OP: Byte): Integer;
var s: String;
    i: Integer;
    Cod: Integer;
begin
  if OP = 1 then
     begin
     s := Code.Op1;
     i := Code.i_Op1;
     end
  else
     begin
     s := Code.Op2;
     i := Code.i_Op2;
     end;

  if i > -1 then
     Result := TVariable(i).Value 
  else
     Val(s, Result, Cod);
end;

function TVirtualMachine.ObtemOperadorBool(Code: pLineCode; OP: Byte): Boolean;
var s: String;
    i: Integer;
    ii: byte;
    Cod: Integer;
begin
  if OP = 1 then
     begin
     s := Code.Op1;
     i := Code.i_Op1;
     end
  else
     begin
     s := Code.Op2;
     i := Code.i_Op2;
     end;

  if i > -1 then
     Result := TVariable(i).Value 
  else
     begin
     Val(s, ii, Cod);
     Result := boolean(ii);
     end;
end;

function TVirtualMachine.ObtemString(Code: pLineCode; OP: Byte): String;
var i: Integer;
begin
  if OP = 1 then
     begin
     Result := Code.Op1;
     i := Code.i_Op1;
     end
  else
     begin
     Result := Code.Op2;
     i := Code.i_Op2;
     end;

  if i > -1 then
     Result := TVariable(i).Value 
end;

procedure TVirtualMachine.ExecuteATRIB(Code: pLineCode; var PC: Integer);
begin
  if Code.T1 = byte(pvtString) then
     TVariable(Code.Res).Value := ObtemString(Code, 1)
  else
     TVariable(Code.Res).Value := ObtemOperador(Code, 1);

  inc(PC);
end;

procedure TVirtualMachine.ExecuteNOP(Code: pLineCode; var PC: Integer);
begin
  inc(PC);
end;

procedure TVirtualMachine.ExecuteRETURN(Code: pLineCode; var PC: Integer);
begin
  PC := Integer(FReturnStack.Pop);
end;

procedure TVirtualMachine.ExecuteHALT(Code: pLineCode; var PC: Integer);
begin
  FTerminated := True;
  inc(PC);
end;

procedure TVirtualMachine.ExecuteADDPAR(Code: pLineCode; var PC: Integer);
var pr : ^Real;
    pb : ^Boolean;
    ps : pString;
    pv : ^TVariable;
begin
  if Boolean(Code.T2) then // é passagem por referência
     begin
     New(pv);
     pv^ := TVariable(Code.i_Op1); 
     Stack.Push(pv);
     end
  else
     Case TVariableType(Code.T1) of
       pvtString:
         begin
         New(ps);
         ps^ := ObtemString(Code, 1);
         Stack.Push(ps);
         end;

       pvtBoolean:
         begin
         New(pb);
         pb^ := ObtemOperadorBool(Code, 1);
         Stack.Push(pb);
         end;

       else
         begin
         New(pr);
         pr^ := ObtemOperador(Code, 1);
         Stack.Push(pr);
         end;
     end; // Case

  inc(PC);
end;

procedure TVirtualMachine.ExecuteCALL(Code: pLineCode; var PC: Integer);
var pr        : ^Real;
    ps        : pString;
    pv        : ^TVariable;
    pb        : ^Boolean;
    v         : TVariable;
    F         : TFunctionList;
    P         : TProcList;
    Eh_Metodo : Boolean;
begin
  Eh_Metodo := (Code.i_Op1 <> -1);
  if Eh_Metodo then
     begin
     F := FLib.Classes[Code.i_Op1].Functions;
     P := FLib.Classes[Code.i_Op1].Procs;
     end
  else
     begin
     F := FLib.Functions;
     P := FLib.Procs;
     end;

  if (Code.Op2[1] = 'F') then
     begin
     F[Code.i_Op2].AccessMethod(Code.Op1, Stack);

     if F[Code.i_Op2].ResType = pvtString then
        begin
        ps := Stack.Pop;
        if (Code.Res <> -1) then TVariable(Code.Res).Value := ps^;
        Dispose(ps);
        end
     else
     if F[Code.i_Op2].ResType = pvtObject then
        begin
        pv := Stack.Pop;
        if (Code.Res <> -1) then
           begin
           v  := TVariable(Code.Res); 
           v.vType       := TVariable(pv^).vType;
           v.ObjectClass := TVariable(pv^).ObjectClass;
           v.Value       := TVariable(pv^).Value;
           end;
        TVariable(pv^).Free;
        Dispose(pv);
        end
     else
     if F[Code.i_Op2].ResType = pvtBoolean then
        begin
        pb := Stack.Pop;
        if (Code.Res <> -1) then TVariable(Code.Res).Value := pb^;
        Dispose(pb);
        end
     else
        begin // São números: Real ou Inteiro
        pr := Stack.Pop;
        if (Code.Res <> -1) then TVariable(Code.Res).Value := pr^;
        Dispose(pr);
        end;

     LiberaParametros(F[Code.i_Op2], Eh_Metodo);
     end
  else
     begin
     P[Code.i_Op2].AccessMethod(Code.Op1, Stack);
     LiberaParametros(P[Code.i_Op2], Eh_Metodo);
     end;

  inc(PC);
end;

procedure TVirtualMachine.ExecuteCO(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value :=
    Integer(FLib.Classes[Code.i_Op1].CreateObject(Stack));

  LiberaParametrosDaClasse(FLib.Classes[Code.i_Op1]);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteEQUAL(Code: pLineCode; var PC: Integer);
var r1, r2: Real;
    s1, s2: String;
begin
  if Code.T1 = byte(pvtString) then
     begin
     s1 := ObtemString(Code, 1);
     s2 := ObtemString(Code, 2);
     TVariable(Code.Res).Value := byte(s1 = s2);
     end
  else
     begin
     r1 := ObtemOperador(Code, 1);
     r2 := ObtemOperador(Code, 2);
     TVariable(Code.Res).Value := byte(r1 = r2);
     end;

  inc(PC);
end;

procedure TVirtualMachine.ExecuteNOEQUAL(Code: pLineCode; var PC: Integer);
var r1, r2: Real;
    s1, s2: String;
begin
  if Code.T1 = byte(pvtString) then
     begin
     s1 := ObtemString(Code, 1);
     s2 := ObtemString(Code, 2);
     TVariable(Code.Res).Value := byte(s1 <> s2);
     end
  else
     begin
     r1 := ObtemOperador(Code, 1);
     r2 := ObtemOperador(Code, 2);
     TVariable(Code.Res).Value := byte(r1 <> r2);
     end;

  inc(PC);
end;

procedure TVirtualMachine.ExecuteGT(Code: pLineCode; var PC: Integer);
var r1, r2: Real;
begin
  r1 := ObtemOperador(Code, 1);
  r2 := ObtemOperador(Code, 2);
  TVariable(Code.Res).Value := byte(r1 > r2);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteGTE(Code: pLineCode; var PC: Integer);
var r1, r2: Real;
begin
  r1 := ObtemOperador(Code, 1);
  r2 := ObtemOperador(Code, 2);
  TVariable(Code.Res).Value := byte(r1 >= r2);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteIF(Code: pLineCode; var PC: Integer);
begin
  if Boolean(TVariable(Code.Res).Value) then
     PC := Code.i_Op1
  else
     PC := Code.i_Op2;
end;

procedure TVirtualMachine.ExecuteGOTO(Code: pLineCode; var PC: Integer);
begin
  PC := Code.i_Op1;
end;

procedure TVirtualMachine.ExecuteLT(Code: pLineCode; var PC: Integer);
var r1, r2: Real;
begin
  r1 := ObtemOperador(Code, 1);
  r2 := ObtemOperador(Code, 2);
  TVariable(Code.Res).Value := byte(r1 < r2);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteLTE(Code: pLineCode; var PC: Integer);
var r1, r2: Real;
begin
  r1 := ObtemOperador(Code, 1);
  r2 := ObtemOperador(Code, 2);
  TVariable(Code.Res).Value := byte(r1 <= r2);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteNEG(Code: pLineCode; var PC: Integer);
var b: Boolean;
begin
  b := not Boolean(Trunc(ObtemOperador(Code, 1)));
  TVariable(Code.Res).Value := byte(b);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteAND(Code: pLineCode; var PC: Integer);
var b1, b2: Boolean;
begin
  b1 := Boolean(Trunc(ObtemOperador(Code, 1)));
  b2 := Boolean(Trunc(ObtemOperador(Code, 2)));
  TVariable(Code.Res).Value := byte(b1 and b2);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteOR(Code: pLineCode; var PC: Integer);
var b1, b2: Boolean;
begin
  b1 := Boolean(Trunc(ObtemOperador(Code, 1)));
  b2 := Boolean(Trunc(ObtemOperador(Code, 2)));
  TVariable(Code.Res).Value := byte(b1 or b2);
  inc(PC);
end;

procedure TVirtualMachine.ExecutePLUS(Code: pLineCode; var PC: Integer);
begin
  if Code.T1 = byte(pvtString) then
     TVariable(Code.Res).Value :=
         ObtemString(Code, 1) + ObtemString(Code, 2)
  else
     TVariable(Code.Res).Value :=
         ObtemOperador(Code, 1) + ObtemOperador(Code, 2);

  inc(PC);
end;

procedure TVirtualMachine.ExecuteMINUS(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value :=
      ObtemOperador(Code, 1) - ObtemOperador(Code, 2);

  inc(PC);
end;

procedure TVirtualMachine.ExecuteMINUSU(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value := -ObtemOperador(Code, 1);
  inc(PC);
end;

procedure TVirtualMachine.ExecuteSLASH(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value :=
      ObtemOperador(Code, 1) / ObtemOperador(Code, 2);

  inc(PC);
end;

procedure TVirtualMachine.ExecuteSTAR(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value :=
      ObtemOperador(Code, 1) * ObtemOperador(Code, 2);

  inc(PC);
end;

procedure TVirtualMachine.ExecuteDIV(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value :=
      ObtemOperadorInt(Code, 1) DIV ObtemOperadorInt(Code, 2);

  inc(PC);
end;

procedure TVirtualMachine.ExecuteMOD(Code: pLineCode; var PC: Integer);
begin
  TVariable(Code.Res).Value :=
      ObtemOperadorInt(Code, 1) MOD ObtemOperadorInt(Code, 2);

  inc(PC);
end;

procedure TVirtualMachine.Execute;

  procedure ProcessMessages;
  var FMsg: TMsg;
  begin
    While PeekMessage(FMsg, 0, 0, 0, PM_REMOVE) do
      begin
      If FMsg.Message = WM_QUIT then halt(FMsg.wParam);
      TranslateMessage(FMsg); DispatchMessage(FMsg);
      end;
  end;

var i, ii: Integer;
begin
  if FBC = nil then Exit;
  If Assigned(FBE) then FBE(Self);
  Try
    i := 0; ii := 0;
    FTerminated := False;
    while (i < FBC.Count) and Not FTerminated do
      begin
      Code := FBC.LineCode[i];

      try
        FExecute[Code.Operacao](Code, i);
        except
          on E: Exception do
             Raise Exception.CreateFmt(E.Message + '.'#13#13 +
                                       'Erro ocorrido na linha %d do byte-code.'#13 +
                                       'Instrução: < %s >'#13 +
                                       'Operando 1: < %s >'#13 +
                                       'Operando 2: < %s >'#13,
                                       [i, FBC.OperToStr(Code.Operacao),
                                       Code.Op1, Code.Op2]);
        end; // try

      if @FPC_Change <> nil then FPC_Change(Self, i);
      inc(ii); if (ii mod 100) = 0 then ProcessMessages;
      end; // while i
  Finally
    If Assigned(FAE) then FAE(Self);
  end;
end;

procedure TVirtualMachine.Stop;
begin
  FTerminated := True;
end;

procedure TVirtualMachine.LiberaParametros(Rotina: TFunctionObject; Eh_Metodo: Boolean);
var i: Integer;
    s: pString;
    r: ^Real;
    p: Pointer;
    b: ^Boolean;
    v: ^TVariable;
begin
  for i := 0 to Rotina.Parameters-1 do
    if not Rotina.ParCat[i] then // Por cópia
       case Rotina.ParType[i] of
         pvtObject:
           begin
           p := Stack.Pop;
           Dispose(p);
           end;

         pvtInteger, pvtReal:
           begin
           r := Stack.Pop;
           Dispose(r);
           end;

         pvtString:
           begin
           s := Stack.Pop;
           Dispose(s);
           end;

         pvtBoolean:
           begin
           b := Stack.Pop;
           Dispose(b);
           end;
       end
    else
       begin // Por referência
       v := Stack.Pop;
       Dispose(v);
       end;

  if Eh_Metodo then
     begin
     p := Stack.Pop;
     Dispose(p);
     end;
end;

procedure TVirtualMachine.LiberaParametrosDaClasse(c: TpsClass);
var i: Integer;
    s: pString;
    r: ^Real;
    p: Pointer;
    b: ^Boolean;
    v: ^TVariable;
begin
  for i := 0 to c.Parameters-1 do
    if not c.ParCat[i] then // Por cópia
       case c.ParType[i] of
         pvtObject:
           begin
           p := Stack.Pop;
           Dispose(p);
           end;

         pvtInteger, pvtReal:
           begin
           r := Stack.Pop;
           Dispose(r);
           end;

         pvtString:
           begin
           s := Stack.Pop;
           Dispose(s);
           end;

         pvtBoolean:
           begin
           b := Stack.Pop;
           Dispose(b);
           end;
       end
    else
       begin // Por referência
       v := Stack.Pop;
       Dispose(v);
       end;
end;

procedure TVirtualMachine.ExecuteLCALL(Code: pLineCode; var PC: Integer);
begin
  FReturnStack.Push(Pointer(PC + 1));
  PC := TLocalFunc(Code.i_Op1).Ender;
end;

procedure TVirtualMachine.ExecuteADDLPAR(Code: pLineCode; var PC: Integer);
begin
  ExecuteATRIB(Code, PC);
end;

{ TPascalScript }

procedure TPascalScript.AddError(Erro: String;
                                 const AdvanceUntil: array of String;
                                 Producoes: array of TProducao);
var i: Integer;
begin
  Erro := Format('(Lin: %d  Col: %d)  ',
            [FScanner.Token[FScanner.Pos].Lin,
            FScanner.Token[FScanner.Pos].Col]) + Erro;

  if FErros.Count < 100 then
     begin
     FErros.AddObject(Erro, FScanner.Token[FScanner.Pos]);

     if Length(AdvanceUntil) > 0 then
        FScanner.AdvanceUntil(AdvanceUntil);

     for i := 0 to High(Producoes) do
        Producoes[i];
     end;
end;

procedure TPascalScript.AddErrorFmt(Erro: String;
                                    const Pars: Array of const;
                                    const AdvanceUntil: array of String;
                                    Producoes: array of TProducao);
var i: Integer;
begin
  Erro := Format(Erro, Pars);
  Erro := Format('(Lin: %d  Col: %d)  ',
            [FScanner.Token[FScanner.Pos].Lin,
            FScanner.Token[FScanner.Pos].Col]) + Erro;

  if FErros.Count < 100 then
     begin
     FErros.AddObject(Erro, FScanner.Token[FScanner.Pos]);

     if Length(AdvanceUntil) > 0 then
        FScanner.AdvanceUntil(AdvanceUntil);

     for i := 0 to High(Producoes) do
       Producoes[i];
     end;  
end;
(*
Function TPascalScript.GetError(Token: TToken): String;
Const F = 'Falta ';
      L = 'na linha: ';
      C = '  coluna: ';
Begin
  Case FErro of
     1: Result := F + '"PROGRAM" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
     4: Result := F + '"END" ou ";" ' + L + IntToStr(Token.Lin);
     5: Result := F + '"BEGIN" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
     7: Result := F + '"REAL" ou "INTEGER" ou "BOOLEAN" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
     2: Result :=     'Erro na Expressão: Falta de Identificador ' + L + IntToStr(Token.Lin);
     3: Result := F + '";" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
     6: Result := F + '":" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
     8: Result := F + '":=" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
     9: Result :=     'Erro na Expressão: Falta de ")" ' + L + IntToStr(Token.Lin);
    11: Result := F + '"DO" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
    12: Result := F + '"THEN" ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col + 2);
    13: Result := F + 'Operador ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
    14: Result := F + 'Comando ' + L + IntToStr(Token.Lin) + C + IntToStr(Token.Col);
    15: Result := F + 'de Identificador ' + L + IntToStr(Token.Lin-1) + C + IntToStr(Token.Col);
    16: Result := F + '"." ' + L + IntToStr(Token.Lin-1) + C + IntToStr(Token.Col);
    17: Result :=     'Variável desconhecida ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    18: Result :=     'Tipos não combinam ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col-2);
    19: Result :=     'Falta " ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    20: Result :=     'Falta ")" ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    21: Result :=     'Erro no comando ESCREVA ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    22: Result := F + 'o "." de finalização da unidade';
    23: Result :=     'Erro no comando LEIA ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    24: Result :=     'Falta "," ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    25: Result :=     'Falta "(" ' + L + IntToStr(Token.Lin)+ C +IntToStr(Token.Col);
    End;
End;
*)
Function TPascalScript.AtribuiTipo(I1, I2: Integer; vt: TVariableType): Boolean;
var i: Integer;
    v: TVariable;
Begin
  if I1 >= 0 then
     For i := I1 to I2 do
       If FScanner.Token[i].TokenType = ttVariable Then
          Begin
          v := FScope.VarByName(FScanner.Token[i].AsString);
          if v <> nil then v.vType := vt;
          End;
End;

Function TPascalScript.Combinam(T1, T2 : TVariableType; Const Op: String): TVariableType;
Begin
  Case T1 Of
    pvtReal :
      Case T2 Of
        pvtReal,
        pvtInteger : If (Op = '+' ) or (Op = '-' ) or (Op = '*' ) or (Op = '/' ) Then
                        Result := pvtReal
                     Else
                     If (Op = '<>') or (Op = '=' ) or (Op = '<' ) or (Op = '<=') or
                        (Op = '>' ) or (Op = '>=') Then
                        Result := pvtBoolean
                     Else
                        Result := pvtNull;

        pvtBoolean,
        pvtString,
        pvtObject  : Result := pvtNull;
        End; {Case T2}

    pvtInteger :
      Case T2 Of
        pvtReal    : If (Op = '+' ) or (Op = '-' ) or (Op = '*' ) or (Op = '/' ) Then
                        Result := pvtReal
                     Else
                     If (Op = '<>') or (Op = '=' ) or (Op = '<' ) or (Op = '<=') or
                        (Op = '>' ) or (Op = '>=') Then
                        Result := pvtBoolean
                     Else
                        Result := pvtNull;

        pvtInteger : If (Op = '+'  ) or (Op = '-'  ) or (Op = '*' ) or (Op = '/' ) or
                        (Op = 'DIV') or (Op = 'MOD') Then
                        Result := pvtInteger
                     Else
                     If (Op = '<>') or (Op = '=' ) or (Op = '<' ) or (Op = '<=') or
                        (Op = '>' ) or (Op = '>=') Then
                        Result := pvtBoolean
                     Else
                        Result := pvtNull;

        pvtBoolean,
        pvtString,
        pvtObject  : Result := pvtNull;
        End; {Case T2}

    pvtBoolean  :
      Case T2 Of
        pvtBoolean : If (Op = '+' ) or (Op = '-' ) or (Op = '*' ) or (Op = '/' ) Then
                        Result := pvtNull
                     Else
                     If (Op = '<>') or (Op = '=' ) or (Op = '<' ) or (Op = '<=' ) or
                        (Op = '>' ) or (Op = '>=') or (Op = 'OR') or (Op = 'AND') Then
                        Result := pvtBoolean
                     Else
                        Result := pvtNull;

        pvtInteger,
        pvtReal,
        pvtObject,
        pvtString     : Result := pvtNull;
        End; {Case T2}

    pvtString:
      Case T2 Of
        pvtString : If (Op = '+') then
                       Result := pvtString
                    Else
                    if (Op = '=') or (Op = '<>') Then
                       Result := pvtBoolean
                    else
                       Result := pvtNull;

        pvtBoolean,
        pvtInteger,
        pvtReal,
        pvtObject  : Result := pvtNull;
        End; {Case T2}

    pvtObject:
      Case T2 Of
        pvtString, pvtInteger, pvtReal, pvtBoolean: Result := pvtNull;
        pvtObject  : If (Op = '=') or (Op = '<>') Then
                       Result := pvtBoolean
                    Else
                       Result := pvtNull;
        End; {Case T2}
    End; {Case T1}

  If (T1 = pvtNull) or (T2 = pvtNull) Then Result := pvtNull;
End; {Combinam}

Function TPascalScript.ReconhecePrograma: Boolean;
Begin
  FScope.Procs.Clear;
  FScope.Funcs.Clear;

  // Só apaga as que não estão bloqueadas
  FScope.Variables.Clear(False);

  FScanner.ClearTokens;
  FScanner.Stack.Clear;
  FBC.Clear;
  FErros.Clear;
  FErro := 0;
  FITV := -1;

  // Variáveis pré-inicializadas
  FScope.Variables.AddVar(TVariable.Create('TRUE' , pvtBoolean, 1, nil, False));
  FScope.Variables.AddVar(TVariable.Create('FALSE', pvtBoolean, 0, nil, False));
  if FGlobalObjects <> nil then
     FScope.Variables.AddVar(
       TVariable.Create('GlobalObjects',
                        pvtObject,
                        Integer(FGlobalObjects),
                        TGlobalObjects,
                        False));

  FScanner.Scan;
  FScanner.FTokens.Add(TToken.Create('.', ttSymbol, 0, 0));

  Result := S and (FErro = 0);
  if Result and FOptimize then FBC.Optimize;
End;

Function TPascalScript.S : Boolean;
var s: String;
Begin
  Result := False;
  s := FScanner.First.AsString;

  // Identificação do Programa {Opcional}
  If s = 'PROGRAM' Then
     Begin
     FScanner.Advance;
     If CurrentTokenType = ttVariable Then
        Begin
        FScanner.Advance;
        If CurrentToken = ';' Then
           FScanner.Advance
        else
           AddError(cMgsFaltaPV, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], []);
        end
     Else
        AddError(cMsgFaltaDeIdentificador, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], []);
     end;

  // Ponto de entrada
  GerarGOTO(-1);

  BD; // Bloco de Declaração de Variáveis e Rotinas

  // Ajuste do salto inicial para o ponto de entrada
  FBC.LineCode[0].i_Op1 := FBC.Count;

  // Bloco principal do programa
  If BC Then
     Begin
     FScanner.Advance;
     If CurrentToken = '.' Then
        Begin
        Result := True;
        If FGerCODE Then GerarHALT;
        End
     else
        AddError(cMsgFaltaPonto, [], [])
     End;
End;

Function  TPascalScript.IdentList(Params: TParams): Boolean;
Begin
  Result := False;
  If CurrentTokenType = ttVariable Then
     Begin
     Result := True;

     // Marca a posição inicial para atribuição dos tipos das variáveis
     if FITV = -1 then FITV := FScanner.Pos;

     // Adiciona a variável no escopo atual
     FPar.Variable := AddVar(CurrentToken);

     // Se for parâmetro, adiciona-o na lista de parâmetros da Sub-Rotina que está sendo declarada
     if Params <> nil then Params.Add(FPar);

     // Verifica os identificadores opcionais
     if (FScanner.Token[FScanner.Pos + 1].AsString = ',') then
        begin
        FScanner.Pos := FScanner.Pos + 2;
        Result := IdentList(Params);
        end
     End
  Else
     AddError(cMsgFaltaDeIdentificador, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);
End;

function TPascalScript.VarDecl(Params: TParams): Boolean;
var vt: TVariableType;
begin
  Result := False;
  if IdentList(Params) then
     begin
     FScanner.Advance;
     if CurrentToken = ':' then
        begin
        FScanner.Advance;
        if Tipo(vt) then
           begin
           Result := True;
           AtribuiTipo(FITV, FScanner.Pos - 2, vt);
           FITV := -1;

           // Verifica as declarações opcionais
           if (FScanner.Token[FScanner.Pos + 1].AsString = ';') and
              (FScanner.Token[FScanner.Pos + 2].TokenType = ttVariable) then
              begin
              FScanner.Pos := FScanner.Pos + 2;
              Result := VarDecl(Params)
              end
           end
        end
     else
        AddError(cMsgFalta2Pontos, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);
     end
end;

function TPascalScript.VarSection: Boolean;
begin
  Result := False;
  if VarDecl(nil) then
     begin
     FScanner.Advance;
     if CurrentToken = ';' then
        begin
        Result := True;
        FScanner.Advance;
        BD;
        end
     else
        AddError(cMgsFaltaPV, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);
     end;
end;

function TPascalScript.FormalParams(Sub: TLocalFunc): Boolean;
begin
  if CurrentToken = '(' then
     begin
     Result := False;
     FScanner.Advance;
     if Parameters(Sub) then
        begin
        FScanner.Advance;
        if CurrentToken = ')' then
           Result := True
        else
           AddError(cMsgFaltaClose, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC])
        end
     end
end;

function TPascalScript.Parameter(Sub: TLocalFunc): Boolean;
begin
  Result := False;
  if VarDecl(Sub.Params) then Result := True;
end;

function TPascalScript.Parameters(Sub: TLocalFunc): Boolean;
var s: String;
begin
  Result := False;
  s := CurrentToken;
  if (s = 'VAR') then
     begin
     FPar.Category := vcVAR;
     FScanner.Advance;
     end
  else
     FPar.Category := vcCOPY;

  if Parameter(Sub) then
     begin
     Result := True;

     // Verifica as declarações opcionais
     if (FScanner.Token[FScanner.Pos + 1].AsString = ';') then
        begin
        FScanner.Pos := FScanner.Pos + 2;
        Result := Parameters(Sub);
        end
     end
end;

function  TPascalScript.DSR : Boolean;
var IsFunc: Boolean;
    FuncOk: Boolean;
    vt: TVariableType;
    SaveScope: TScope;
    Func: TLocalFunc;
    SubList: TLocalFuncList;
begin
  Result := False;
  IsFunc := (FScanner.Token[FScanner.Pos-1].AsString = 'FUNCTION');

  SaveScope := FScope;
  Func := TLocalFunc.Create(FScope);
  FScope := Func.Scope;

  if IsFunc then
     SubList := Func.Scope.SuperScope.Funcs
  else
     SubList := Func.Scope.SuperScope.Procs;

  If CurrentTokenType = ttVariable Then
     begin
     // Adiciona a Sub-rotina na lista de rotinas deste escopo
     Func.Name := CurrentToken;
     SubList.AddObject(Func.Name, Func);

     // PARAMETROS OPCIONAIS
     if FScanner.Token[FScanner.Pos + 1].AsString = '(' then
        begin
        FScanner.Advance;
        if not FormalParams(Func) then Exit;
        end;

     if IsFunc then
        begin
        FuncOk := False;
        FScanner.Advance;
        If CurrentToken = ':' Then
           begin
           FScanner.Advance;
           if Tipo(vt) then
              begin
              Func.ResType := vt;
              FuncOk := True;
              FScope.Variables.AddVar(TVariable.Create('RESULT', vt, 0, nil, False));
              end
           else
              AddError(cMsgFaltaTipoRes, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC])
           end
        else
           AddError(cMsgFalta2Pontos, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC])
        end
     else
        FuncOk := True;

     if FuncOk then
        begin
        FScanner.Advance;
        If CurrentToken = ';' Then
           begin
           FScanner.Advance;

           BD; // Declaração de Variáveis e Rotinas no Escopo atual

           // Ponto de entrada da Sub-Rotina
           Func.Ender := FBC.Count;

           if BC then
              begin
              FScanner.Advance;
              if CurrentToken = ';' then
                 begin
                 Result := True;
                 FScanner.Advance;
                 If FGerCODE Then GerarRETURN;
                 end
              else
                 AddError(cMgsFaltaPV, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);
              end;
           end
        else
           AddError(cMgsFaltaPV, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);
        end; // if FuncOk
     end
  else
     AddError(cMsgFaltaDeIdentificador, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);

  FScope := SaveScope;
end;

Function  TPascalScript.BD : Boolean;
var s: String;
Begin
  s := CurrentToken;

  if s = 'VAR' then
     begin
     FScanner.Advance;
     Result := VarSection
     end else
  if (s = 'PROCEDURE') or (s = 'FUNCTION') then
     begin
     FScanner.Advance;
     Result := DSR and BD;
     end;
end;

Function TPascalScript.Tipo(out vt: TVariableType): Boolean;
var s: String;
Begin
  Result := True;
  s := CurrentToken;

  If s = 'INTEGER' Then
     vt := pvtInteger else

  If s = 'BOOLEAN' Then
     vt := pvtBoolean else

  if s = 'REAL' then
     vt := pvtReal else

  if s = 'STRING' then
     vt := pvtString else

  if s = 'OBJECT' then
     vt := pvtObject

  Else
     AddError(cMsgFaltaTipoVar, ['VAR', 'PROCEDURE', 'FUNCTION', 'BEGIN'], [BD, BC]);
End;

Function   TPascalScript.BC : Boolean;
var s: String;
Begin
  Result := False;
  s := CurrentToken;

  If s = 'BEGIN' Then
     Begin
     If SC Then
        Begin
        FScanner.Advance;
        s := CurrentToken;

        If s = 'END' Then
           Result := True
        Else
           AddError(cMsgFaltaEND, ['BEGIN'], [BC]);
        End
     Else
        {SC - nada}
     end
  Else
     AddError(cMsgFaltaBEGIN, [], []);
End;

Function TPascalScript.SC : Boolean;
Begin
  Result := False;
  If C Then
     If CO Then
        Result := True;
End;

Function TPascalScript.C : Boolean;
var Save    : Integer;
    s       : String;
    v, vTmp : TVariable;
    Op      : PVarRec;
    i       : Integer;
    Proc    : TProcObject;
    LProc   : TLocalFunc;
Begin
  Result := False;
  Save := FScanner.Pos;
  FScanner.Advance;
  s := CurrentToken;

  {Comando de Atribuição ou Chamada de método}
  If CurrentTokenType = ttVariable Then
     Begin
     // Procedimentos Locais
     LProc := FScope.SubByName(s, 0);
     if LProc <> nil then
        begin
        GeraCodigoParaRotinaLocal(LProc);
        Result := True;
        Exit;
        end;

     v := FScope.VarByName(s);
     if v = nil then
        begin
        AddErrorFmt(cMsgVariavelDesconhecida, [s], [';', 'END'], [SC]);
        exit;
        end;

     // Verifica se é chamada de método de objeto
     if (v.vType = pvtObject) and (FScanner.Token[FScanner.Pos + 1].AsString = '.') then
        begin
        // Faz uma cópia da variável
        vTmp := TVariable.Create(v.Name, v.vType, v.Value, v.ObjectClass, False);
        AvaliaObjeto(vTmp, s); // método procedimento
        vTmp.Free;
        Result := True;
        end
     else
        begin
        FScanner.Advance;
        If (CurrentToken = ':=') Then
           begin
           If FGerCODE then FBC.Init;
           Save := FScanner.Pos;
           FScanner.Advance;
           if CurrentToken = 'CREATEOBJECT' then
              begin
              Result := GeraCodigoParaClasse(v, s);
              if Result then Exit;

              if FErro = 1 then
                 begin
                 FErro := 0;
                 FScanner.AdvanceUntil([';']);
                 if C then
                    begin
                    Result := True;
                    Exit;
                    end;
                 end;
              end
           else
              FScanner.Pos := Save;

           If E Then
              Begin
              {Semântico}
              Op := FScanner.Stack.Pop;
              if VerificaCompatibilidade(v.vType, Op.vType) then
                 if v.vType = pvtObject then
                    v.ObjectClass := Op.ObjectClass;

              If FGerCODE Then
                 Begin
                 FBC.Operacao := operATRIB;
                 FBC.Op1      := Op.Symbol;
                 FBC.Op2      := '';

                 if Op.IsVar then
                    FBC.i_Op1 := Op.Ender // Endereco da variável Result se uma função for chamada
                 else
                    if Op.vType = pvtString then
                       FBC.i_Op1 := -1
                    else
                       FBC.i_Op1 := ObtemEnderOper(Op.Symbol);

                 FBC.i_Op2    := -1;
                 FBC.T1       := byte(Op.vType);
                 FBC.T2       := -1;
                 FBC.Res      := ObtemEnderOper(s);
                 FBC.AddLine;
                 End;

              Dispose(PVarRec(Op));
              {Semântico}
              Result := True;
              End
           Else
              Begin
              FScanner.AdvanceUntil(['BEGIN', ';']);
              SC;
              End
           End
        Else
           AddError(cMsgFaltaAtrib_ou_Ponto, ['BEGIN', ';'], [SC]);
        end;
     end
  else
     // Procedimentos Pré-definidos
  if s = 'EXIT' then
     begin
     Result := True;
     If FGerCODE Then GerarHALT
     end
  else
     // Procedimentos
     begin
     i := FLib.Procs.IndexOf(s);
     if i > -1 then
        begin
        Proc := FLib.Procs[i];
        GeraCodigoParaRotina(Proc, 'P', i, -1, '');
        Result := True;
        end
     else
        // Pode ser Comando Composto
        begin
        FScanner.Pos := Save;
        Result := True;
        If not CC Then FScanner.Pos := Save;
        end;
     end; // Procedimentos
end;

Function   TPascalScript.CO : Boolean;
Var Save: Integer;
Begin
  Result := False;
  Save := FScanner.Pos;
  FScanner.Advance;

  If CurrentToken = ';' Then
     if SC then
        Begin
        Result := True;
        Exit;
        End;

  FScanner.Pos := Save;
  Result := True;
End;

function TPascalScript.CC_Bloco: Boolean;
begin
  If SC Then
     Begin
     FScanner.Advance;
     If CurrentToken = 'END' Then
        Result := True
     Else
        AddError(cMsgFaltaEND, ['BEGIN', ';'], [SC]);
     End
  else
     Result := False;
end;

// 'FOR' Ident ':=' E 'TO' E ['STEP' E] 'DO'
function TPascalScript.CC_FOR: Boolean;
Var s          : String;
    tmp        : String;
    Ident      : String;
    Ini        : String;
    Fim        : String;
    Step       : String;
    i_tmp_Fim  : Integer;
    i_tmp_Step : Integer;
    Op         : PVarRec;
    ENR0       : Integer;  {Endereço Não Resolvido 1: Teste do fim do Laço}
    ENR1       : Integer;  {Endereço Não Resolvido 2: Saída do Laço}
    v          : TVariable;
begin
  Result := False;
  If FGerCODE then FBC.Init;

  i_tmp_Step := -1;
  i_tmp_Fim  := -1;

  FScanner.Advance;

  if CurrentTokenType = ttVariable then
     begin
     Ident := CurrentToken;

     v := FScope.VarByName(Ident);
     if v = nil then
        AddErrorFmt(cMsgVariavelDesconhecida, [Ident], [], [])
     else
        if not ((v.vType = pvtInteger) or (v.vType = pvtReal)) then
           AddError('Variável <' + Ident + '> tem que ser do tipo "INTEGER" ou "REAL"', [], []);

     FScanner.Advance;
     if CurrentToken = ':=' then
        if E then
           begin
           {Semântico}
           Op  := FScanner.Stack.Pop;
           Ini := Op.Symbol;
           if v <> nil then VerificaCompatibilidade(v.vType, Op.vType);
           Dispose(PVarRec(Op));
           {Semântico}

           FScanner.Advance;
           s := CurrentToken;
           if (s = 'TO') or (s = 'DOWNTO') then
              if E then
                 begin
                 {Semântico}
                 Op  := FScanner.Stack.Pop;
                 Fim := Op.Symbol;
                 if v <> nil then VerificaCompatibilidade(v.vType, Op.vType);
                 Dispose(PVarRec(Op));
                 {Semântico}

                 FScanner.Advance;

                 if CurrentToken = 'STEP' then
                    if E then
                       begin
                       {Semântico}
                       Op   := FScanner.Stack.Pop;
                       Step := Op.Symbol;
                       if v <> nil then VerificaCompatibilidade(v.vType, Op.vType);
                       Dispose(PVarRec(Op));
                       {Semântico}

                       // Se Step é um temporário
                       if (Step <> '') and (Step[1] = '@') then
                          begin
                          i_tmp_Step := FBC.CurrentOrdTemp;
                          FBC.Lock(i_tmp_Step);
                          end;

                       FScanner.Advance; // Avança para o teste do "DO"
                       end
                    else
                       begin
                       // Erro na Expressão já processado
                       FScanner.AdvanceUntil(['BEGIN', ';']);
                       if C then
                          begin
                          Result := True;
                          Exit;
                          end;
                       end
                 else
                    Step := '1';

                 if CurrentToken = 'DO' then
                    begin
                    {Geração de Código}
                    If FGerCODE Then
                       Begin
                       // := Ini Ident
                       FBC.Operacao := operATRIB;
                       FBC.Op1      := Ini;
                       FBC.Op2      := '';
                       FBC.i_Op1    := ObtemEnderOper(Ini);
                       FBC.i_Op2    := -1;
                       FBC.T1       := -1;
                       FBC.T2       := -1;
                       FBC.Res      := ObtemEnderOper(Ident);
                       FBC.AddLine;

                       // := Fim tmp
                       FBC.Operacao := operATRIB;
                       FBC.Op1      := Fim;
                       FBC.Op2      := '';
                       FBC.i_Op1    := ObtemEnderOper(Fim);
                       FBC.i_Op2    := -1;
                       FBC.T1       := -1;
                       FBC.T2       := -1;

                       tmp          := FBC.NewTemp;
                       i_tmp_Fim    := FBC.CurrentOrdTemp;
                       FBC.Res      := ObtemEnderOper(tmp);

                       FBC.AddLine;
                       FBC.Lock(i_tmp_Fim);

                       // <= Ident tmp tmp2
                       if s = 'TO' then
                          FBC.Operacao := operLTE
                       else
                          FBC.Operacao := operGTE;

                       FBC.Op1      := Ident;
                       FBC.Op2      := tmp;
                       FBC.i_Op1    := ObtemEnderOper(Ident);
                       FBC.i_Op2    := ObtemEnderOper(tmp);
                       FBC.T1       := -1;
                       FBC.T2       := -1;
                       tmp          := FBC.NewTemp;
                       FBC.Res      := ObtemEnderOper(tmp);
                       ENR0         := FBC.Count;
                       FBC.AddLine;

                       // IF Linha_Seguinte ?? tmp2
                       FBC.Operacao := operIF;
                       FBC.Op1      := '';
                       FBC.Op2      := '';
                       FBC.i_Op1    := FBC.Count + 1;
                       FBC.i_Op2    := -1;
                       FBC.T1       := -1;
                       FBC.T2       := -1;
                       FBC.Res      := ObtemEnderOper(tmp);
                       ENR1         := FBC.Count;
                       FBC.AddLine;
                       End;
                    {Geração de Código}

                    if C then
                       begin
                       Result := True;

                       {Geração de Código}
                       If FGerCODE Then
                          Begin
                          // + Ident Step Ident
                          if s = 'TO' then
                             FBC.Operacao := operPLUS
                          else
                             FBC.Operacao := operMINUS;

                          FBC.Op1      := Ident;
                          FBC.Op2      := Step;
                          FBC.i_Op1    := ObtemEnderOper(Ident);
                          FBC.i_Op2    := ObtemEnderOper(Step);
                          FBC.T1       := -1;
                          FBC.T2       := -1;
                          FBC.Res      := ObtemEnderOper(Ident);
                          FBC.AddLine;

                          GerarGOTO(ENR0);

                          // Resolvendo o endereço de saída do Laço
                          FBC.LineCode[ENR1].i_Op2 := FBC.Count;
                          end;
                       {Geração de Código}
                       end;
                    end
                 else
                    AddError(cMsgFaltaDO, ['BEGIN', ';'], [SC]);
                 end
              else // E (Final)
                 begin
                 // Erro na expressão já processado
                 FScanner.AdvanceUntil(['BEGIN', ';']);
                 SC;
                 end
           end
        else // E (Inicio)
           begin
           // Erro na expressão já processado
           FScanner.AdvanceUntil(['BEGIN', ';']);
           SC;
           end
     else // :=
        AddError(cMsgFaltaAtrib_ou_Ponto, ['BEGIN', ';'], [SC]);
     end
  else // ttVariable --> Ident
     AddError(cMsgFaltaDeIdentificador, ['BEGIN', ';'], [SC]);

  if i_tmp_Step > -1 then FBC.UnLock(i_tmp_Step);
  if i_tmp_Fim > -1 then FBC.UnLock(i_tmp_Fim);
end;

// 'IF' E 'THEN' C ['ELSE' C]
function TPascalScript.CC_IF: Boolean;
Var Save   : Integer;
    s      : String;
    Op     : PVarRec;
    ENR1   : Integer;  {Endereço Não Resolvido 1}
begin
  Result := False;
  If FGerCODE then FBC.Init();
  If E() Then
     Begin
     {Semântico e Código Intermediário}
     Op := FScanner.Stack.Pop;
     VerificaCompatibilidade(Op.vType, pvtBoolean);

     If FGerCODE Then
        Begin
        FBC.Operacao := operIF;
        FBC.Op1      := '';
        FBC.Op2      := '';
        FBC.i_Op1    := FBC.Count + 1;
        FBC.i_Op2    := -1;
        FBC.T1       := -1;
        FBC.T2       := -1;
        ENR1         := FBC.Count;

        if Op.IsVar then
           FBC.Res := Op.Ender
        else
           FBC.Res := ObtemEnderOper(Op.Symbol);

        FBC.AddLine;
        End;
     Dispose(PVarRec(Op));
     {Semântico e Código Intermediário}

     FScanner.Advance;
     If CurrentToken = 'THEN' Then
        If C Then
           Begin
           Save := FScanner.Pos;
           FScanner.Advance;
           If CurrentToken = 'ELSE' Then
              Begin
              {Geração de Código}
              If FGerCODE Then
                 Begin
                 // Resolvendo o GOTO anterior
                 FBC.LineCode[ENR1].i_Op2 := FBC.Count + 1;
                 ENR1 := FBC.Count;
                 GerarGOTO(-1);
                 End;
              {Geração de Código}

              If C Then
                 Begin
                 Result := True;
                 If FGerCODE Then
                    {Arrumando o GOTO não resolvido}
                    FBC.LineCode[ENR1].i_Op1 := FBC.Count;
                 End
              Else
                 Result := True;
              End
           Else
              Begin
              Result := True;
              If FGerCODE Then
                 {Arrumando o GOTO não resolvido}
                 FBC.LineCode[ENR1].i_Op2 := FBC.Count;
              FScanner.Pos := Save;
              End;


           // Este bloco é incluído para separar o último comando do bloco IF dos
           // demais comandos. Isto é importante para o otimizador de código.
           FBC.Operacao := operNOP;
           FBC.Op1      := '';
           FBC.Op2      := '';
           FBC.i_Op1    := -1;
           FBC.i_Op2    := -1;
           FBC.T1       := -1;
           FBC.T2       := -1;
           FBC.Res      := -1;

           FBC.AddLine;
           End
        Else
           Result := True
     Else
        AddError(cMsgFaltaTHEN, ['BEGIN', ';'], [SC]);
     End
  Else {Erro na expressão}
     Begin
     // O erro é detectado na avaliação da expressão E
     FScanner.AdvanceUntil(['BEGIN', ';', 'THEN']);
     s := CurrentToken;

     If (s = 'BEGIN') or (s = ';') or (s = 'THEN') Then
        If C Then
           Result := True
     End;
end;

function TPascalScript.CC_WHILE: Boolean;
Var s      : String;
    Op     : PVarRec;
    ENR1   : Integer;  {Endereço Não Resolvido 1}
    ENR2   : Integer;  {Endereço Não Resolvido 2}
begin
  Result := False;
  If FGerCODE then begin FBC.Init; ENR2 := FBC.Count end;
  If E Then
     Begin
     {Semântico}
     Op := FScanner.Stack.Pop;
     VerificaCompatibilidade(Op.vType, pvtBoolean);

     If FGerCODE Then
        Begin
        FBC.Operacao := operIF;
        FBC.Op1      := '';
        FBC.Op2      := '';
        FBC.i_Op1    := FBC.Count + 1;
        FBC.i_Op2    := -1;
        FBC.T1       := -1;
        FBC.T2       := -1;

        if Op.IsVar then
           FBC.Res := Op.Ender
        else
           FBC.Res := ObtemEnderOper(Op.Symbol);

        FBC.AddLine;
        ENR1 := FBC.Count;
        End;

     Dispose(PVarRec(Op));
     {Semântico}

     FScanner.Advance;
     If CurrentToken = 'DO' Then
        If C Then
           Begin
           Result := True;
           {Geração de Código}
           If FGerCODE Then
              Begin
              FBC.LineCode[ENR1-1].i_Op2 := FBC.Count + 1;
              GerarGOTO(ENR2);
              End;
           {Geração de Código}
           // Exit;
           End
        Else  {Nada}
     Else
        AddError(cMsgFaltaDO, ['BEGIN', ';'], [SC]);
     End
  Else {Erro na expressão}
     Begin
     // O erro é detectado na avaliação da expressão E
     FScanner.AdvanceUntil(['BEGIN', ';', 'DO']);

     s := CurrentToken;
     If (s = ';') or (s = 'BEGIN') or (s = 'DO') Then
        If C Then
           Result := True;
     End;
end;

Function TPascalScript.CC : Boolean;
Var Save : Integer;
    s    : String;
Begin
  Save := FScanner.Pos;

  FScanner.Advance;
  s := CurrentToken;

  if s = 'BEGIN' then
     Result := CC_Bloco

  else
  if s = 'WHILE' then
     Result := CC_WHILE

  else
  if s = 'IF' then
     Result := CC_IF

  else
  if s = 'FOR' then
     Result := CC_FOR

  else
     begin {Comando Vazio}
     FScanner.Pos := Save;
     Result := True; 
     end;
End;

Function TPascalScript.ST : Boolean;
Var Save: Integer;
    v   : TVariable;
    tt  : TTokenType;
     s  : String;
Begin
  Result := False;
  FScanner.Advance;
  tt := CurrentTokenType;
  s  := CurrentToken;
  If (tt = ttString) or (tt = ttVariable) Then
     Begin
     If (tt = ttVariable) Then
        Begin
        v := FScope.VarByName(s);
        if v = nil then AddErrorFmt(cMsgVariavelDesconhecida, [s], [';', 'END'], [SC]);
        End;

     If FGerCODE Then
        Begin
        FBC.Op1      := s ;
        FBC.Op2      := '';
        FBC.i_Op1    := -1;
        FBC.i_Op2    := -1;
        //FBC.T1       := byte(pvtString);
        FBC.Res      := -1;
        FBC.AddLine;
        End;

     Save := FScanner.Pos;
     FScanner.Advance;
     If CurrentToken = ',' Then
        If ST Then
           Result := True
        Else
           {Nada}
     Else
        Begin
        Result := True;
        FScanner.Pos := Save;
        End
     End
End;

Function    TPascalScript.E : Boolean;
Begin
  Result := False;
  If F Then
     If El Then Result := True;
End;

Function   TPascalScript.El : Boolean;
Var Save: Integer;
    s: String;
    p, op1, op2: PVarRec;
Begin
  Result := False;
  Save := FScanner.Pos;
  FScanner.Advance;
  s := CurrentToken;
  If (s = '<') or (s = '<=') or (s = '>') or (s = '>=') or (s = '=') or (s = '<>') Then
     If F Then
        Begin
        {Semântico}
        New(p);
        Op2 := FScanner.Stack.Pop;
        Op1 := FScanner.Stack.Pop;
        p.vType := Combinam(PVarRec(Op1)^.vType, PVarRec(Op2)^.vType, s);
        If FGerCODE Then p.Symbol := FBC.NewTemp;
        p.Ender := Integer(FScope.VarByName(FBC.TMP));
        p.IsVar := True;
        FScanner.Stack.Push(p);
        {Semântico}

        If FGerCODE Then
           Begin
           FBC.Operacao := obtem_vmOperator(s);
           FBC.Op1      := Op1.Symbol;
           FBC.Op2      := Op2.Symbol;

           if Op1.IsVar then
              FBC.i_Op1 := Op1.Ender
           else
              if Op1.vType = pvtString then
                 FBC.i_Op1 := -1
              else
                 FBC.i_Op1 := ObtemEnderOper(Op1.Symbol);

           if Op2.IsVar then
              FBC.i_Op2 := Op2.Ender
           else
              if Op2.vType = pvtString then
                 FBC.i_Op2 := -1
              else
                 FBC.i_Op2 := ObtemEnderOper(Op2.Symbol);

           FBC.T1       := byte(Op1.vType);
           FBC.T2       := byte(Op2.vType);
           FBC.Res      := ObtemEnderOper(FBC.TMP);
           FBC.AddLine;
           End;

        Dispose(PVarRec(Op1));
        Dispose(PVarRec(Op2));

        If El Then
           Result := True
        Else
           Begin
           Result := False;
           FScanner.Pos := Save;
           End
        End
     Else
        Begin
        Result := False;
        FScanner.Pos := Save;
        End
  Else
     Begin
     Result := True;
     FScanner.Pos := Save;
     End;
End;

Function    TPascalScript.F : Boolean;
Begin
  Result := False;
  If G Then
     If Fl Then Result := True;
End;

Function   TPascalScript.Fl : Boolean;
Var Save: Integer;
    s: String;
    p, op1, op2: PVarRec;
Begin
  Result := False;
  Save := FScanner.Pos;
  FScanner.Advance;

  s := CurrentToken;
  If (s = '+') or (s = '-')  or (s = 'OR') Then
     If G Then
        Begin
        {Semântico}
        New(p);
        Op2 := FScanner.Stack.Pop;
        Op1 := FScanner.Stack.Pop;
        p.vType := Combinam(PVarRec(Op1)^.vType, PVarRec(Op2)^.vType, s);
        If FGerCODE Then p.Symbol := FBC.NewTemp;
        p.Ender := Integer(FScope.VarByName(FBC.TMP));
        p.IsVar := True;
        FScanner.Stack.Push(p);
        {Semântico}

        If FGerCODE Then
           Begin
           FBC.Operacao := obtem_vmOperator(s);
           FBC.Op1      := Op1.Symbol;
           FBC.Op2      := Op2.Symbol;

           if Op1.IsVar then
              FBC.i_Op1 := Op1.Ender
           else
              if Op1.vType = pvtString then
                 FBC.i_Op1 := -1
              else
                 FBC.i_Op1 := ObtemEnderOper(Op1.Symbol);

           if Op2.IsVar then
              FBC.i_Op2 := Op2.Ender
           else
              if Op2.vType = pvtString then
                 FBC.i_Op2 := -1
              else
                 FBC.i_Op2 := ObtemEnderOper(Op2.Symbol);

           FBC.T1       := byte(Op1.vType);
           FBC.T2       := byte(Op2.vType);
           FBC.Res      := ObtemEnderOper(FBC.TMP);
           FBC.AddLine;
           End;

        Dispose(PVarRec(Op1));
        Dispose(PVarRec(Op2));

        If Fl Then
           Result := True
        Else
           Begin
           Result := False;
           FScanner.Pos := Save;
           End
        End
     Else
        Begin
        Result := False;
        FScanner.Pos := Save;
        End
  Else
     Begin
     Result := True;
     FScanner.Pos := Save;
     End;
End;

Function    TPascalScript.G : Boolean;
Begin
  Result := False;
  If O Then
     If Gl Then Result := True;
End;

Function   TPascalScript.Gl : Boolean;
Var Save: Integer;
    s: String;
    p, op1, op2: PVarRec;
Begin
  Result := False;
  Save := FScanner.Pos;
  FScanner.Advance;

  s := CurrentToken;
  If (s = '*') or (s = '/') or (s = 'AND') or (s = 'DIV') or (s = 'MOD') Then
     If O Then
        Begin
        {Semântico}
        New(p);
        Op2 := FScanner.Stack.Pop;
        Op1 := FScanner.Stack.Pop;
        p.vType := Combinam(PVarRec(Op1)^.vType, PVarRec(Op2)^.vType, s);
        If FGerCODE Then p.Symbol := FBC.NewTemp;
        p.Ender := Integer(FScope.VarByName(FBC.TMP));
        p.IsVar := True;
        FScanner.Stack.Push(p);
        {Semântico}

        If FGerCODE Then
           Begin
           FBC.Operacao := obtem_vmOperator(s);
           FBC.Op1      := Op1.Symbol;
           FBC.Op2      := Op2.Symbol;

           if Op1.IsVar then
              FBC.i_Op1 := Op1.Ender
           else
              if Op1.vType = pvtString then
                 FBC.i_Op1 := -1
              else
                 FBC.i_Op1 := ObtemEnderOper(Op1.Symbol);

           if Op2.IsVar then
              FBC.i_Op2 := Op2.Ender
           else
              if Op2.vType = pvtString then
                 FBC.i_Op2 := -1
              else
                 FBC.i_Op2 := ObtemEnderOper(Op2.Symbol);

           FBC.T1       := byte(Op1.vType);
           FBC.T2       := byte(Op2.vType);
           FBC.Res      := ObtemEnderOper(FBC.TMP);
           FBC.AddLine;
           End;

        Dispose(PVarRec(Op1));
        Dispose(PVarRec(Op2));

        If Gl Then
           Result := True
        Else
           Begin
           Result := False;
           FScanner.Pos := Save;
           End
        End
     Else
        Begin
        Result := False;
        FScanner.Pos := Save;
        End
  Else
     Begin
     Result := True;
     FScanner.Pos := Save;
     End;
End;

Function   TPascalScript.O : Boolean;
Var Save: Integer;
    s: String;
    p, op1: PVarRec;
Begin
  Result := False;
  Save := FScanner.Pos;
  FScanner.Advance;
  s := CurrentToken;
  If (s = '-') or (s = 'NOT') Then
     If O Then
        Begin
        {Semântico}
        New(p);
        Op1 := FScanner.Stack.Pop;

        if ((s = '-'  ) and ((Op1.vType = pvtReal) or (Op1.vType = pvtInteger))) or
           ((s = 'NOT') and (Op1.vType = pvtBoolean)) then
           p.vType := Op1.vType
        else
           p.vType := pvtNull;

        If FGerCODE Then p.Symbol := FBC.NewTemp;
        p.Ender := Integer(FScope.VarByName(FBC.TMP));
        p.IsVar := True;

        FScanner.Stack.Push(p);
        {Semântico}

        If FGerCODE Then
           Begin
           If (Op1.vType = pvtBoolean) then
              FBC.Operacao := operNEG
           Else
              FBC.Operacao := operMINUSU;

           FBC.Op1      := Op1.Symbol;
           FBC.Op2      := '';

           if Op1.IsVar then
              FBC.i_Op1 := Op1.Ender
           else
              FBC.i_Op1 := ObtemEnderOper(Op1.Symbol);

           FBC.i_Op2    := -1;
           FBC.T1       := byte(Op1.vType);
           FBC.T2       := -1;
           FBC.Res      := ObtemEnderOper(FBC.TMP);
           FBC.AddLine;
           End;

        Dispose(PVarRec(Op1));
        Result := True;
        Exit;
        End;

  FScanner.Pos := Save;
  If T Then Result := True;
End;

Function TPascalScript.T : Boolean;
Var v          : TVariable;
    vTmp       : TVariable;
    i          : Integer;
    p, op      : PVarRec;
    s          : String;
    Func       : TFunctionObject;
    LFunc      : TLocalFunc;
    Clas       : TpsClass;
Begin
  Result := False;
  FScanner.Advance;
  s := CurrentToken;

  If CurrentTokenType = ttVariable Then
     Begin
     // Procedimentos Locais
     LFunc := FScope.SubByName(s, 1);
     if LFunc <> nil then
        begin
        GeraCodigoParaRotinaLocal(LFunc);
        Result := True;
        Exit;
        end;

     v := FScope.VarByName(s);
     if v <> nil then
        begin
        Result := True;
        // Faz uma cópia de TVariable
        vTmp := TVariable.Create('', v.vType, 0, v.ObjectClass, False);
        if vTmp.vType = pvtObject then AvaliaObjeto(vTmp, s);

        New(p);
        p.Symbol      := s;
        p.Ender       := Integer(FScope.VarByName(s));
        p.vType       := vTmp.vType;
        p.ObjectClass := vTmp.ObjectClass;
        p.IsVar       := True;

        FScanner.Stack.Push(p);
        vTmp.Free;
        end
     else
        AddErrorFmt(cMsgVariavelDesconhecida, [s], [';', 'END'], [SC])
     End
  Else

  If CurrentTokenType = ttInteger Then
     Begin
     Result := True;

     {Semântico}
     New(p);
     p.Symbol := s;
     p.vType := pvtInteger;
     p.IsVar := False;
     FScanner.Stack.Push(p);
     End

  Else
  If CurrentTokenType = ttFloat Then
     Begin
     Result := True;

     {Semântico}
     New(p);
     p.Symbol := s;
     p.vType := pvtReal;
     p.IsVar := False;
     FScanner.Stack.Push(p);
     End

  Else
  If CurrentTokenType = ttString Then
     Begin
     Result := True;

     {Semântico}
     New(p);
     p.Symbol := Copy(s, 2, Length(s)-2);
     p.vType  := pvtString;
     p.IsVar  := False;
     FScanner.Stack.Push(p);
     End

  Else
     If s = '(' Then
       If E Then
          Begin
          FScanner.Advance;
          If CurrentToken = ')' Then
             Result := True
          Else
             AddError(cMsgFaltaClose, ['BEGIN', ';'], [SC])
          End
       Else

  Else
     If CurrentTokenType = ttFunction Then
        begin
        i := FLib.Functions.IndexOf(s);
        Func := FLib.Functions[i];
        GeraCodigoParaRotina(Func, 'F', i, -1, '');
        Result := True;
        end // ttFunction

  Else
     If CurrentTokenType = ttClass Then
        begin
        Clas := FLib.Classes[FLib.Classes.IndexOf(s)];
        FScanner.Advance;
        If CurrentToken = '(' Then
          If E Then
             Begin
             FScanner.Advance;
             If CurrentToken = ')' Then
                begin
                Op := FScanner.Stack.Peek;
                if Op.vType = pvtObject then
                   begin
                   Op.ObjectClass := Clas.ObjectClass;
                   Result := True;
                   end
                else
                   AddError(cMsgTypeCastingInvalido, [], [])
                end
             Else
                AddError(cMsgFaltaClose, ['BEGIN', ';'], [SC])
             End
          else
        else
           AddError(cMsgFaltaOpen, ['BEGIN', ';'], [SC])
        end // ttClass

  Else
     AddError(cMsgFaltaDeIdentificador, [], []);
End;

constructor TPascalScript.Create();
begin
  Inherited Create;

  FGerCODE := True;
  FEconomize := True;

  FLib     := TLib.Create;
  FScope   := TScope.Create(nil);
  FScanner := TPascalScanner.Create(Self, FLib);

  With FScanner.KeyWords do
    Begin
    // Comandos
    ADD('BEGIN'  ); ADD('END'    );
    ADD('IF'     ); ADD('THEN'   ); ADD('ELSE'  );
    ADD('DIV'    ); ADD('MOD'    );
    ADD('WHILE'  ); ADD('DO'     );
    ADD('FOR'    ); ADD('TO'     ); ADD('DOWNTO'); ADD('STEP');
    ADD('NOT'    ); ADD('OR'     ); ADD('AND'    );
    ADD('EXIT'   );

    // Comandos especiais
    ADD('CREATEOBJECT');

    // Tipo de variáveis
    ADD('REAL'); ADD('INTEGER'); ADD('BOOLEAN'); ADD('STRING'); ADD('OBJECT');

    // Declarações
    ADD('PROGRAM'); ADD('VAR'); ADD('PROCEDURE'); ADD('FUNCTION');
    End;

  FBC    := TByteCode.Create(Self);
  FVM    := TVirtualMachine.Create(FBC, FLib);
  FErros := TStringList.Create;
end;

destructor TPascalScript.Destroy;
begin
  if not FAssignedLib then FLib.Free;
  FScope.Free;
  FScanner.Free;
  FBC.Free;
  FVM.Free;
  FErros.Free;
  inherited;
end;

function TPascalScript.Compile(): Boolean;
begin
  FCompiled := ReconhecePrograma() and (FErros.Count = 0);
  result := FCompiled;
end;

procedure TPascalScript.Execute;
begin
  if FCompiled then
     FVM.Execute()
  else
     if Compile() then FVM.Execute();
end;

procedure TPascalScript.Stop();
begin
  FVM.Stop();
end;

function TPascalScript.VerificaCompatibilidade(t1, t2: TVariableType; ParOrd: byte = 0): Boolean;
begin
  Result := (t1 = t2) or ( (t1 = pvtReal) and (t2 = pvtInteger) );
  If not Result Then
     if ParOrd = 0 then
        AddErrorFmt(cMsgTiposNaoCombinam, [getStrVarType(t1), getStrVarType(t2)], [], [])
     else
        AddErrorFmt(cMsg_Param_TiposNaoCombinam, [ParOrd, getStrVarType(t1), getStrVarType(t2)], [], [])
end;
(*
function TPascalScript.VerificaCompatibilidadeVal(t: TVariableType): Boolean;
begin
  Result := ((t = pvtReal) or (t = pvtInteger));
  If not Result Then
     AddErrorFmt(cMsgTiposNaoCombinam, [getStrVarType(t), '(Integer ou Real)'], [], []);
end;

function TPascalScript.VerificaDeclaracaoVar(const Nome: String): TVariable;
begin
  Result := FScope.VarByName(Nome);
  if Result = nil then AddErrorFmt(cMsgVariavelDesconhecida, [Nome], [';', 'END'], [SC]);
end;
*)
function TPascalScript.VerificaCompatibilidadeDeClasses(c1, c2: TClass; ParOrd: Integer): Boolean;

  // Utilizado por causa do uso das bibliotecas em DLLs, as classes perdem algumas informações básicas
  function InheritsFrom(c1, c2: TClass): Boolean;
  var s: String;
      c: TClass;
  begin
    Result := False;
    c := c1;
    s := c2.ClassName;
    while c <> nil do
      begin
      if c.ClassName = s then
         begin
         Result := True;
         Exit;
         end;
      c := c.ClassParent;
      end;
  end;

var s1, s2 : String;
begin
  if c1 <> nil then s1 := c1.ClassName else s1 := 'Tipo desconhecido';
  if c2 <> nil then s2 := c2.ClassName else s2 := 'Tipo desconhecido';

  if (c1 = nil) or (c2 = nil) then
     AddErrorFmt(cMsg_Param_TiposNaoCombinam, [ParOrd, s1, s2], [], [])
  else
    if not InheritsFrom(c1, c2) then
       AddErrorFmt(cMsg_Param_TiposNaoCombinam, [ParOrd, s1, s2], [], []);
end;

procedure TPascalScript.InverteCodigo(BCTemp, BC: TByteCode);
var i: Integer;
begin
  for i := BCTemp.Count-1 downto 0 do
    BC.AddLine(BCTemp.LineCode[i]^);
end;

function TPascalScript.ObtemEnderOper(const OP: String): Integer;
begin
  if OP <> '' then
     begin
     Result := Integer(FScope.VarByName(OP));
     if Result = 0 then Result := -1;
     end
  else
     Result := -1;
end;

function TPascalScript.getText: TStrings;
begin
  Result := FScanner.Text;
end;

procedure TPascalScript.Include(API: TAPI_Proc);
begin
  API(Self.Lib);
end;

procedure TPascalScript.GeraCodigoParaMetodo(v: TVariable; var Symbol: String);
var o          : TpsClass;
    s          : String;
    i, ii      : Integer;
    Op         : pVarRec;
    TipoMetodo : Char;
begin
  s  := CurrentToken;

  if v.ObjectClass <> nil then
     begin
     ii := FLib.Classes.IndexOf(v.ObjectClass.ClassName);

     if ii > -1 then
        begin
        o  := FLib.Classes[ii];

        i  := o.Functions.IndexOf(s);
        if i <> -1 then
           TipoMetodo := 'F'
        else
           begin
           i := o.Procs.IndexOf(s);
           if i <> -1 then
              TipoMetodo := 'P'
           else
              TipoMetodo := 'X';
           end;

        Case TipoMetodo of
          'F':
             begin
             GeraCodigoParaRotina(o.Functions[i], TipoMetodo, i, ii, Symbol);
             Op := FScanner.Stack.Pop;
             v.vType := Op.vType;
             v.ObjectClass := Op.ObjectClass;
             Symbol := Op.Symbol;
             Dispose(pVarRec(Op));
             end;

          'P':
             begin
             GeraCodigoParaRotina(o.Procs[i], TipoMetodo, i, ii, Symbol);
             end;

          'X':
             begin
             v.vType := pvtNull;
             AddError('Método desconhecido: ' + s, ['BEGIN', ';'], [SC]);
             end;
          end // case
        end
     else
        begin
        v.vType := pvtNull;
        AddErrorFmt('Tipo de classe desconhecido "%s"', [v.ObjectClass.ClassName], [], []);
        end;
     end
  else
     begin
     v.vType := pvtNull;
     AddError('Objeto não inicializado (classe não definida)', [], []);
     end;
end;

procedure TPascalScript.AvaliaObjeto(v: TVariable; var Symbol: String);
var i: Integer;
begin
  i := FScanner.Pos + 1; // Token Seguinte
  if FScanner.Token[i].AsString = '.' then
     begin
     FScanner.Pos := i + 1; // método
     GeraCodigoParaMetodo(v, Symbol);
     AvaliaObjeto(v, Symbol);
     end
  else
     // Nada
end;

procedure TPascalScript.GeraCodigoParaParametrosDaClasse(c: TpsClass);
var FBCTemp : TByteCode;
    i, ii   : Integer;
    op      : PVarRec;
    v       : TVariable;
begin
  if c.Parameters > 0 then
     begin
     FScanner.Advance;
     if CurrentToken <> ',' then
        AddErrorFmt(cMsgFaltaVirgula + cMsgClassParams, [c.Name, c.Parameters], [], [])
     else
        begin
        FBCTemp := TByteCode.Create(Self);
        for i := 0 to c.Parameters-1 do
          begin
          if E then
             begin
             Op := FScanner.Stack.Pop;

             // Testa se o parâmetro é por referência, se for testa se é variável
             if c.ParCat[i] and (Op.Symbol <> '') then
                Case Op.Symbol[1] of
                   #29, '0'..'9': AddErrorFmt(cMsgErroParVAR, [i+1], [], []);
                   else
                      // Se variável e parâmetro for tipo Object então verifica o subtipo
                      begin
                      v := FScope.VarByName(Op.Symbol); // <<<
                      if v = nil then Exit; // <<<
                      if (v.vType = pvtObject) then
                         VerificaCompatibilidadeDeClasses(v.ObjectClass, c.ParClass[i], i+1);
                      end;
                   end;

             if VerificaCompatibilidade(c.ParType[i], Op.vType, i+1) then
                If FGerCODE Then
                   Begin

                  if Op.IsVar then
                     ii := Op.Ender // Endereco da variável Result se uma função for chamada
                  else
                     if Op.vType = pvtString then
                        ii := -1
                     else
                        ii := ObtemEnderOper(Op.Symbol);

                   FBCTemp.Operacao := operADDPAR;
                   FBCTemp.Op1      := Op.Symbol;
                   FBCTemp.Op2      := '';
                   FBCTemp.i_Op1    := ii;
                   FBCTemp.i_Op2    := -1;
                   FBCTemp.T1       := byte(Op.vType);
                   FBCTemp.T2       := byte(c.ParCat[i]);
                   FBCTemp.Res      := -1;
                   FBCTemp.AddLine;
                   End;
             Dispose(pVarRec(Op));

             if i < c.Parameters-1 then
                begin
                FScanner.Advance;
                if CurrentToken <> ',' Then
                   AddErrorFmt(cMsgFaltaVirgula + cMsgClassParams, [c.Name, c.Parameters], [], []);
                end;
             end; // if E
          end; // for i

        InverteCodigo(FBCTemp, FBC);
        FBCTemp.Free;
        end; // else Token = ','
     end; // if c.Parameters > 0
end;

function TPascalScript.GeraCodigoParaClasse(v: TVariable; const Symbol: String): Boolean;
var s2: String;
    i : Integer;
begin
  FErro := 0;
  Result := False;
  FScanner.Advance;
  if CurrentToken = '(' then
     begin
     FScanner.Advance;
     if CurrentTokenType = ttClass then
        begin
        s2 := CurrentToken;
        i  := FLib.Classes.IndexOf(s2);

        if i = -1 then
           AddErrorFmt('Classe "%s" não registrada', [s2], [], [])
        else
           if FLib.Classes[i].CanCreate then
              begin
              GeraCodigoParaParametrosDaClasse(FLib.Classes[i]);

              If FGerCODE Then
                 Begin
                 FBC.Operacao := operCO;
                 FBC.Op1      := s2;
                 FBC.Op2      := '';
                 FBC.i_Op1    := i; // índice da classe
                 FBC.i_Op2    := -1;
                 FBC.T1       := -1;
                 FBC.T2       := -1;
                 FBC.Res      := ObtemEnderOper(Symbol);
                 FBC.AddLine;
                 End
              else
                 // Nada
              end
           else // <<<
              AddErrorFmt('A classe "%s" não pode ser instanciada diretamente',
                          [FLib.Classes[i].Name], [], []);

        FScanner.Advance;
        if CurrentToken = ')' then
           if v.vType <> pvtNull then
              if v.vType = pvtObject then
                 begin
                 if i > -1 then v.ObjectClass := FLib.Classes[i].ObjectClass;
                 Result := True;
                 end
              else
                 begin
                 AddErrorFmt('Variável "%s" não é do tipo Object', [Symbol], [], []);
                 FErro := 1;
                 end
           else
              // nada, já houve um erro de variável desconhecida
        else
           AddError(cMsgFaltaClose, ['BEGIN', ';'], [SC]);
        end
     else
        AddError(cMsgFaltaDeIdentificador, ['BEGIN', ';'], [SC]);
     end
  else
     AddError(cMsgFaltaOpen, ['BEGIN', ';'], [SC]);
end;

procedure TPascalScript.GeraCodigoParaRotina(F: TFunctionObject; const TipoRotina: String;
                                        IndRot, IndObj: Integer; const ObjectName: String);
var FBCTemp : TByteCode;
    i, ii   : Integer;
    p, op   : PVarRec;
    v       : TVariable;
begin
  FBCTemp := TByteCode.Create(Self);

  if F.Parameters > 0 then
     begin
     FScanner.Advance;
     if CurrentToken = '(' Then
        for i := 1 to F.Parameters do
          begin
          if E then
             begin
             Op := FScanner.Stack.Pop;

             // Testa se o parâmetro é por referência, se for testa se é variável
             if F.ParCat[i-1] and (Op.Symbol <> '') then
                Case Op.Symbol[1] of
                   #29, '0'..'9': AddErrorFmt(cMsgErroParVAR, [i], [], []);
                   else
                      // Se variável e parâmetro for tipo Object então verifica o subtipo
                      begin
                      v := FScope.VarByName(Op.Symbol); // <<<
                      if v = nil then Exit; // <<<
                      if (v.vType = pvtObject) then
                         VerificaCompatibilidadeDeClasses(v.ObjectClass, F.ParClass[i-1], i);
                      end;
                   end;

             if VerificaCompatibilidade(F.ParType[i-1], Op.vType, i) then
                If FGerCODE Then
                   Begin

                   if Op.IsVar then
                      ii := Op.Ender // Endereco da variável Result se uma função for chamada
                   else
                      if Op.vType = pvtString then
                         ii := -1
                      else
                         ii := ObtemEnderOper(Op.Symbol);

                   FBCTemp.Operacao := operADDPAR;
                   FBCTemp.Op1      := Op.Symbol;
                   FBCTemp.Op2      := '';
                   FBCTemp.i_Op1    := ii;
                   FBCTemp.i_Op2    := -1;
                   FBCTemp.T1       := byte(Op.vType);
                   FBCTemp.T2       := byte(F.ParCat[i-1]);
                   FBCTemp.Res      := -1;
                   FBCTemp.AddLine;
                   End;
             Dispose(pVarRec(Op));

             FScanner.Advance;
             if i = F.Parameters then
                if CurrentToken = ')' Then
                   // Tudo certo
                else
                   AddErrorFmt(
                     cMsgFaltaClose + cMsgParams, [F.Name, F.Parameters], [], [])
             else
                if CurrentToken <> ',' Then
                   AddErrorFmt(cMsgFaltaVirgula + cMsgParams, [F.Name, F.Parameters], [], []);
             end;
          end // for
     else
        AddErrorFmt(cMsgFaltaOpen + cMsgParams, [F.Name, F.Parameters], [], []);
     end; // if parameters > 0

   if ObjectName <> '' then
      If FGerCODE Then
         Begin
         FBCTemp.Operacao := operADDPAR;
         FBCTemp.Op1      := ObjectName;
         FBCTemp.Op2      := '';
         FBCTemp.i_Op1    := ObtemEnderOper(ObjectName);
         FBCTemp.i_Op2    := -1;
         FBCTemp.T1       := byte(pvtObject);
         FBCTemp.T2       := byte(True); // referência
         FBCTemp.Res      := -1;
         FBCTemp.AddLine;
         End;

   InverteCodigo(FBCTemp, FBC);
   If FGerCODE Then
      Begin
      FBC.Operacao := operCALL;
      FBC.i_Op1    := IndObj;
      FBC.i_Op2    := IndRot;
      FBC.T1       := -1;
      FBC.T2       := -1;

      if IndObj > -1 then
         FBC.Op1 := FLib.Classes[IndObj].Name + '.' + F.Name
      else
         FBC.Op1 := F.Name;

      FBC.Op2 := TipoRotina;

      if TipoRotina = 'F' then
         begin
         FBC.Res := ObtemEnderOper(FBC.NewTemp);

         New(p);
         p.Symbol      := FBC.TMP;
         p.Ender       := Integer(FScope.VarByName(FBC.TMP));
         p.vType       := F.ResType;
         p.ObjectClass := F.ResObjectClass;
         p.IsVar       := True;

         FScanner.Stack.Push(p);
         end
      else
         FBC.Res := -1;

      FBC.AddLine;
      End;

  FBCTemp.Free;
end;

function TPascalScript.obtem_vmOperator(const Oper: String): TvmOperators;
begin
  if Oper = '+'   then Result := operPLUS    else
  if Oper = '-'   then Result := operMINUS   else
  if Oper = '*'   then Result := operSTAR    else
  if Oper = '/'   then Result := operSLASH   else
  if Oper = '='   then Result := operEQUAL   else
  if Oper = '<>'  then Result := operNOEQUAL else
  if Oper = 'AND' then Result := operAND     else
  if Oper = 'OR'  then Result := operOR      else
  if Oper = '>'   then Result := operGT      else
  if Oper = '>='  then Result := operGTE     else
  if Oper = '<'   then Result := operLT      else
  if Oper = '<='  then Result := operLTE     else
  if Oper = 'DIV' then Result := operDIV     else
  if Oper = 'MOD' then Result := operMOD;
end;

procedure TPascalScript.AssignLib(Lib: TLib);
begin
  if not FAssignedLib and (Lib <> nil) then
     begin
     FAssignedLib := True;
     FLib.Free;
     FLib := Lib;
     FScanner.FLib := FLib;
     FVM.FLib := FLib;
     end
end;

procedure TPascalScript.SetGlobalObjects(const Value: TObject);
begin
  if Value = nil then
     FGlobalObjects := nil
  else
     begin
     if not (Value is TGlobalObjects) then
        Raise Exception.Create('Objeto não é do tipo TGlobalObjects');

     FGlobalObjects := Value;
     If FLib.Classes.IndexOf('TGlobalObjects') = -1 then
        Include(Lib_GlobalObjects.API);
     end;
end;

procedure TPascalScript.SetEconomize(const Value: Boolean);
begin
  FEconomize := Value;
  FLib.Functions.Economize := Value;
  FLib.Procs.Economize := Value;
  FLib.Classes.Economize := Value;
end;

procedure TPascalScript.GerarRETURN;
begin
  FBC.Operacao := operRETURN;
  FBC.Op1      := '';
  FBC.Op2      := '';
  FBC.i_Op1    := -1;
  FBC.i_Op2    := -1;
  FBC.T1       := -1;
  FBC.T2       := -1;
  FBC.Res      := -1;
  FBC.AddLine;
end;

procedure TPascalScript.GerarHALT;
begin
  FBC.Operacao := operHALT;
  FBC.Op1      := '';
  FBC.Op2      := '';
  FBC.i_Op1    := -1;
  FBC.i_Op2    := -1;
  FBC.T1       := -1;
  FBC.T2       := -1;
  FBC.Res      := -1;
  FBC.AddLine;
end;

procedure TPascalScript.GerarGOTO(Linha: Integer);
begin
  FBC.Operacao := operGOTO;
  FBC.Op1      := '';
  FBC.Op2      := '';
  FBC.i_Op1    := Linha;
  FBC.i_Op2    := -1;
  FBC.T1       := -1;
  FBC.T2       := -1;
  FBC.Res      := -1;
  FBC.AddLine;
end;

function TPascalScript.GetVariables: TSymbolTable;
begin
  Result := FScope.Variables;
end;

function TPascalScript.AddVar(const Name: String): TVariable;
var i: Integer;
begin
  i := FScope.Variables.IndexOf(Name);
  If i < 0 then
     begin
     Result := TVariable.Create(Name, pvtNull, 0, nil, False);
     FScope.Variables.AddVar(Result);
     end
  else
     Result := FScope.Variables.Variable[i]; // <<<
end;

procedure TPascalScript.GeraCodigoParaRotinaLocal(Sub: TLocalFunc);
var i, ii   : Integer;
    p, op   : PVarRec;
    v       : TVariable;
    FBCTemp : TByteCode;
begin
  FBCTemp := nil;
  if Sub.Params.Count > 0 then
     begin
     FBCTemp := TByteCode.Create(self);
     FScanner.Advance;
     if CurrentToken = '(' Then
        for i := 0 to Sub.Params.Count-1 do
          begin
          if E then
             begin
             Op := FScanner.Stack.Pop;

             // Testa se o parâmetro é por referência, se for testa se é variável
             if (Sub.Params[i].Category = vcVAR) and (Op.Symbol <> '') then
                Case Op.Symbol[1] of
                   #29, '0'..'9': AddErrorFmt(cMsgErroParVAR, [i + 1], [], []);
                   else
                      // Se variável e parâmetro for tipo Object então verifica o subtipo
                      begin
                      v := FScope.VarByName(Op.Symbol); // <<<
                      if v = nil then Exit; // <<<

                      FBCTemp.Operacao := operATRIB;
                      FBCTemp.Op1      := Sub.Params[i].Variable.Name;
                      FBCTemp.Op2      := '';
                      FBCTemp.i_Op1    := Integer(Sub.Params[i].Variable);
                      FBCTemp.i_Op2    := -1;
                      FBCTemp.T1       := byte(Sub.Params[i].Variable.vType);
                      FBCTemp.T2       := -1;
                      FBCTemp.Res      := Integer(v);
                      FBCTemp.AddLine;

                      if (v.vType = pvtObject) then
                         VerificaCompatibilidadeDeClasses(
                           v.ObjectClass, Sub.Params[i].Variable.ObjectClass, i + 1);
                      end;
                   end;

             if VerificaCompatibilidade(Sub.Params[i].Variable.vType, Op.vType, i + 1) then
                If FGerCODE Then
                   Begin
                   if Op.IsVar then
                      ii := Op.Ender // Endereco da variável Result se uma função for chamada
                   else
                      if Op.vType = pvtString then
                         ii := -1
                      else
                         ii := ObtemEnderOper(Op.Symbol);

                   FBC.Operacao := operADDLPAR;
                   FBC.Op1      := Op.Symbol;
                   FBC.Op2      := '';
                   FBC.i_Op1    := ii;
                   FBC.i_Op2    := -1;
                   FBC.T1       := byte(Op.vType);
                   FBC.T2       := -1;
                   FBC.Res      := integer(Sub.Params[i].Variable);
                   FBC.AddLine;
                   End;
             Dispose(pVarRec(Op));

             FScanner.Advance;
             if i = Sub.Params.Count-1 then
                if CurrentToken = ')' Then
                   // Tudo certo
                else
                   AddErrorFmt(cMsgFaltaClose + cMsgParams, [Sub.Name, Sub.Params.Count], [], [])
             else
                if CurrentToken <> ',' Then
                   AddErrorFmt(cMsgFaltaVirgula + cMsgParams, [Sub.Name, Sub.Params.Count], [], []);
             end;
          end // for
     else
        AddErrorFmt(cMsgFaltaOpen + cMsgParams, [Sub.Name, Sub.Params.Count], [], []);
     end; // if parameters > 0

  FBC.Operacao := operLCALL;
  FBC.Op1      := Sub.Name;
  FBC.Op2      := '';
  FBC.i_Op1    := Integer(Sub);
  FBC.i_Op2    := -1;
  FBC.T1       := -1;
  FBC.T2       := -1;

  if Sub.ResType <> pvtNull then
     begin
     FBC.Res := Integer(Sub.Scope.VarByName('RESULT'));

     New(p);
     p.Symbol      := 'RESULT';
     p.vType       := Sub.ResType;
     p.Ender       := FBC.Res;
     p.ObjectClass := TVariable(FBC.Res).ObjectClass;
     p.IsVar       := True;

     FScanner.Stack.Push(p);
     end
  else
     FBC.Res := -1;

  FBC.AddLine;

  // Parâmetros VAR
  if FBCTemp <> nil then
     begin
     for i := 0 to FBCTemp.Count-1 do
       FBC.AddLine(FBCTemp.LineCode[i]^);

     FBCTemp.Free;
     end;
end;

function TPascalScript.GetCurrentToken: String;
begin
  Result := FScanner.Token[FScanner.Pos].AsString;
end;

function TPascalScript.GetCurrentTokenType: TTokenType;
begin
  Result := FScanner.Token[FScanner.Pos].TokenType;
end;

{ TScope }

constructor TScope.Create(SuperScope: TScope);
begin
  inherited Create;
  FSuperScope := SuperScope;
  FProcs := TLocalFuncList.Create;
  FFuncs := TLocalFuncList.Create;
  FVariables := TSymbolTable.Create;
end;

destructor TScope.Destroy;
begin
  FProcs.Free;
  FFuncs.Free;
  FVariables.Free;
  inherited;
end;

procedure TScope.Clear;
begin
  FProcs.Clear;
  FFuncs.Clear;
  FVariables.Clear(True);
end;

// Corrige os endereços das sub-rotinas locais diminuindo de 1 os endereços
// que são maiores do que "BaseEddress".
procedure TScope.CorrectRoutinesEddress(BaseEddress: Integer);
var i: Integer;
    LF: TLocalFunc;
begin
  for i := 0 to FProcs.Count-1 do
    begin
    LF := TLocalFunc(FProcs.Objects[i]);
    if LF.Ender > BaseEddress then LF.FEnder := LF.FEnder - 1;
    end;

  for i := 0 to FFuncs.Count-1 do
    begin
    LF := TLocalFunc(FFuncs.Objects[i]);
    if LF.Ender > BaseEddress then LF.FEnder := LF.FEnder - 1;
    end;

  if SuperScope <> nil then
     SuperScope.CorrectRoutinesEddress(BaseEddress);
end;

// Procura por uma sub-rotina em todo o escopo do bloco retornando "nil" se não encontrar.
// SubType é o tipo de lista: 0 para Procs, 1 para Funcs
function TScope.SubByName(const Name: String; SubType: byte): TLocalFunc;
var Sub: TLocalFuncList;
    i: Integer;
begin
  if SubType = 0 then Sub := FProcs else Sub := FFuncs;

  i := Sub.IndexOf(Name);
  if i >= 0 then
     Result := TLocalFunc(Sub.Objects[i])
  else
     if FSuperScope <> nil then
        Result := FSuperScope.SubByName(Name, SubType)
     else
        Result := nil;
end;

function TScope.VarByName(const Name: String): TVariable;
var v: TVariable;
begin
  v := FVariables.VarByName(Name);
  if v <> nil then
     Result := v
  else
     if FSuperScope <> nil then
        Result := FSuperScope.VarByName(Name)
     else
        Result := nil;
{
  if Result = nil then
     Raise Exception.CreateFmt('Variável desconhecida: %s', [Name]); }
end;

{ TLocalFunc }

constructor TLocalFunc.Create(SuperScope: TScope);
begin
  inherited Create;
  FScope := TScope.Create(SuperScope);
  FParams := TParams.Create;
  FResType := pvtNull;
end;

destructor TLocalFunc.Destroy;
begin
  FParams.Free;
  FScope.Free;
  inherited;
end;

{ TLocalFuncList }

constructor TLocalFuncList.Create;
begin
  inherited;
  Sorted := True;
end;

destructor TLocalFuncList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLocalFuncList.Clear;
var i: Integer;
begin
  for i := 0 to Count-1 do TLocalFunc(Objects[i]).Free;
  inherited Clear;
end;

{ TParams }

constructor TParams.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TParams.Destroy;
var i: integer;
begin
  for i := 0 to FList.Count-1 do Dispose(pParameter(FList[i]));
  FList.Free;
  inherited;
end;

procedure TParams.Add(const Par: TParameter);
var p: pParameter;
begin
  New(p);
  p^ := Par;
  FList.Add(p);
end;

function TParams.getPars(i: byte): pParameter;
begin
  Result := pParameter(FList[i]);
end;

function TParams.GetCount: byte;
begin
  Result := FList.Count;
end;

end.

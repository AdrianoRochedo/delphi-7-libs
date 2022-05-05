unit hidro_Classes;

{
    Esta unidade implementa as classes básicas que encapsulam objetos hidrogáficos tais como
    Sub-bacias, Postos de controle, Trechos-Dáguas, Demandas, Derivações, etc.

    Estas classes não devem ser instanciadas, descenda classes específicas para cada
    THidroComponente e paralelamente classes descendentes para os dados (THidroDados)
    e implemente todos os métodos abstratos.

    Dados totalmente genéricos tais como "Nome", "Comentários" podem ser implementados
    diretamente na Hierarquia de THidroComponente, mas dados hidrológicos deverão ser
    implementados paralelamente na Hierarquia de THidroDados.

    Desta maneira conseguimos a separação ideal e o máximo aproveitamento do mecanismo
    visual e dos dados. Assim, podemos ter varios conjunto de dados aproveitando a mesma
    hierarquia visual.
}

interface
uses // Delphi
     Windows, Types, Messages, Classes, SysUtils, ExtCtrls, Forms,
     Graphics, Controls, ComCtrls, Menus, Dialogs, FileCTRL, Math,

     // Geral
     MessageManager,
     Simulation,
     SysUtilsEx,
     Lists,
     ErrosDLG,
     Shapes,
     MapObjects,
     MapObjectsEx,

     // Projeto
     hidro_Tipos,
     hidro_Dialogo;

type
  THidroComponente = Class;
  TPC              = Class;
  TSubBacia        = Class;
  TDerivacao       = Class;
  TDemanda         = Class;
  TProjeto         = Class;
  TListaDePCs      = Class;

  // Classes básicas -----------------------------------------------------------------

  // Representa os dados Hidrológicos do Objeto
  THidroDados = class
  protected
    // Realiza a leitura das informações do componente.
    procedure LerDoArquivo(Ini: TIF; Const Secao: String); Virtual;

    // Salva as informações do componente em um arquivo
    procedure SalvarEmArquivo(Ini: TIF; Const Secao: String); Virtual;

    // Obtem as informações editadas pelo usuário no diálodo
    procedure PegarDadosDoDialogo(d: THidroDialogo); Virtual;

    // Mostra as propriedades do componente nos campos do diálogo
    procedure PorDadosNoDialogo(d: THidroDialogo); Virtual;

    // Realiza a validação dos dados utilizando o Diálogo de Erros para informá-los.
    // Caso algum erro exista, o parâmetro TudoOK deverá ser setado para Falso.
    // Completo informa uma queremos realizar uma validação mais rigorosa.
    procedure ValidarDados(var TudoOk: Boolean;
                              DialogoDeErros: TErros_DLG;
                              Completo: Boolean = False); Virtual;
  end;

  // Classe abstrata para gerenciamento dos resultados tais como plotagem de gráficos,
  // planilhas, relatórios, etc.
  // As ações para os métodos serão fornecidas através de menus.
  TGerenteDeResultados = class
  private
    FProjeto: TProjeto;
  public
    constructor Create(Projeto: TProjeto);
    procedure ObterAcoes(MenuDestino: TMenu; const ID_Obj: String); virtual; abstract;
    property Projeto : TProjeto read FProjeto;
  end;

  // Objeto visual com capacidade de Stream
  THidroComponente = class(T_NRC_InterfacedObject, IMessageReceptor)
  private
    FNome                   : String;
    FComentarios            : TStrings;
    FVisitado               : Boolean;
    FModificado             : Boolean;
    FBloqueado              : Boolean;

    FProjeto                : TProjeto;
    TN                      : TStrings;
    FAvisarQueVaiSeDestruir : Boolean;
    FImagemDoComponente     : TdrBaseShape;
    FDialogo                : THidroDialogo;
    FMenu                   : TPopupMenu;
    FPos                    : ImoPoint;

    procedure SetNome(const Value: String);
    function  GetRect: TRect;
    function  GetScreenPos: TPoint;
    function  GetPos: ImoPoint;
    procedure SetPos(Const Value: ImoPoint);
    procedure DuploClick(Sender: TObject);

    // Fornece uma identificação única para o componente
    function ObterNome: String;
    function getVisivel: Boolean;
    procedure setVisivel(const Value: Boolean);
  protected
    { ******************** Dados ********************}

    // Poderá ser nil se nenhuma informação extra precisar ser incorporada.
    // A classe que quiser expor os dados deverá fazê-lo através da definição de uma propriedade
    // HidroDados: TXXX read getXXX onde getXXX fará o type-casting apropriado sobre FDados.
    FDados: THidroDados;

    // Fornece a instância específica dos dados
    // nil por default - Indica que nenhuma informação extra precisa ser incorporada
    function CriarDados: THidroDados; virtual;

    procedure SetModificado(const Value: Boolean); virtual;

    { *************** Métodos de Stream **************** }

    // Realiza a leitura das informações do componente.
    procedure LerDoArquivo(Ini: TIF; Const Secao: String); Virtual;

    // Salva as informações do componente em um arquivo
    procedure SalvarEmArquivo(Ini: TIF); Virtual;

    { *************** Fim Métodos de Stream **************** }

    // Receptor de mensagens enviadas pelo sistema
    function ReceiveMessage(Const MSG: TadvMessage): Boolean; virtual;

    // Atualiza as informações mostradas quando o cursor está sobre o componente
    procedure AtualizarHint; virtual;

    { ********************** Imagem *************************}

    // Deverá ser implementado para fornecer uma instância gráfica que representará o
    // componente hidrológico. Neste método o usuário poderá definir a classe bem como
    // os atributos de cor, tamanho, etc.
    function CriarImagemDoComponente: TdrBaseShape; virtual;

    // Responsável por criar e conectar o componente ao projeto ativo.
    // Deverá ser chamado no Create de cada descendente de THidroComponente que desejar
    // possuir uma imagem para representá-lo. Este método chama CriarImagemDoComponente.
    procedure CriarComponenteVisual(Pos: ImoPoint);

    { ********************** Fim Imagem *************************}

    // Prepara um menu Popup para o componente
    procedure PrepararMenu; virtual;

    // Fornece um prefixo padrão para o nome do componente
    Function ObterPrefixo: String; Virtual;

    { *************** Métodos para gerenciamento do Diálogo **************** }

    // Cria um diálogo para edição das propriedades do componente e de seus hidro-dados
    // Se nenhum diálogo for instanciado, um default será fornecido.
    // Todo diálogo deverá ser obrigatóriamente descendente de THidroDialogo
    function CriarDialogo: THidroDialogo; Virtual;

    // Obtem as informações editadas pelo usuário em um diálodo
    procedure PegarDadosDoDialogo(d: THidroDialogo); Virtual;

    // Mostra as propriedades do componente nos campos em um diálogo
    procedure PorDadosNoDialogo(d: THidroDialogo); Virtual;

    { *************** Fim Métodos para gerenciamento do Diálogo **************** }

    // Realiza as inicializações para as simulações
    // Este método é chamado através de um evento disparado antes da simulação
    procedure PrepararParaSimulacao; virtual;

    procedure ValidarDados(var TudoOk: Boolean;
                              DialogoDeErros: TErros_DLG;
                              Completo: Boolean = False); virtual;
  public
    constructor Create(UmaTabelaDeNomes: TStrings; Projeto: TProjeto);
    destructor  Destroy; override;

    // Rotinas para tratamento de caminhos de arquivo
    function  VerificarCaminho(var Arquivo: String): Boolean;
    procedure RetirarCaminhoSePuder(var Arquivo: String);

    // Obtem o nome da secao do Arquivo Ini para este objeto
    function ObterSecaoDoIni: String;

    // Avisa outros componentes que está prestes a se destruir
    procedure Avisar_e_Destruir;

    // Mostra um diálogo para edição das informações do componente
    function  MostrarDialogo(const Caption: String = ''): Integer;

    // Mostra um Menu para escolha de opções pelo usuário
    procedure MostrarMenu(x: Integer = -1; y: Integer = -1);

    // Cria uma conecção de um componente a este componente
    function  ConectarObjeto(Obj: THidroComponente): Integer; Virtual;

    // Desconecta todos os componentes ligados a este componente
    procedure DesconectarObjetos; Virtual;

    // Realiza o diagnóstico dos dados (Erros Fatais e Avisos)
    procedure Diagnostico(var TudoOk: Boolean; Completo: Boolean = False);

    // Permite que nomes únicos sejam atribuídos aos componentes
    property  TabNomes : TStrings read TN write TN;

    // Nome do Objeto
    property  Nome : String read FNome write SetNome;

    // Comentários, descrição, etc
    property  Comentarios : TStrings read FComentarios;

    // Projeto a que este objeto pertence
    property Projeto : TProjeto read FProjeto write FProjeto;

    // Indica, se numa interação, se este objeto foi visitado
    property Visitado : Boolean read FVisitado write FVisitado;

    // visibilidade do componente gráfico
    property Visivel : Boolean read getVisivel write setVisivel;

    // Indica se o objeto foi modificado
    property Modificado : Boolean read FModificado write SetModificado;

    // Indica se o objeto pode ser modificado
    property Bloqueado : Boolean read FBloqueado write FBloqueado;

    // Visual

    property ImagemDoComponente : TdrBaseShape       read FImagemDoComponente;
    property Regiao             : TRect              read GetRect;
    property ScreenPos          : TPoint             read GetScreenPos;

    property Pos                : ImoPoint           read FPos          write SetPos;
    property Menu               : TPopupMenu         read FMenu         write FMenu;
    property Dialogo            : THidroDialogo      read FDialogo      write FDialogo;

    {
    Indica ao objeto que ele deve enviar uma mensagem para o sistema avisando
    outros objetos que ele está prestes a ser destruído.
    O valor por falta é verdadeiro (true)
    }
    property AvisarQueVaiSeDestruir: Boolean
       read  FAvisarQueVaiSeDestruir
       write FAvisarQueVaiSeDestruir;
  end;

  // Mantém uma lista com referências a objetos
  TListaDeObjetos = Class
  private
    FList: TList;
    FLiberarObjetos: Boolean;
    function  getObjeto(index: Integer): THidroComponente;
    procedure setObjeto(index: Integer; const Value: THidroComponente);
    function  getNumObjetos: Integer;
  public
    constructor Create;
    Destructor  Destroy; override;

    procedure RemoverNulos;

    function  IndiceDo  (Objeto: THidroComponente): Integer;
    function  Remover   (Objeto: THidroComponente): Integer;
    function  Adicionar (Objeto: THidroComponente): Integer;

    procedure Deletar (Indice: Integer);
    procedure Ordenar (FuncaoDeComparacao: TListSortCompare);

    // Iniciada em False, quando True, destroi os objetos referenciados
    property LiberarObjetos : Boolean read FLiberarObjetos write FLiberarObjetos;

    property Objeto[i: Integer] : THidroComponente read getObjeto write setObjeto; default;
    property Objetos            : Integer          read getNumObjetos;
  end;

  // Representa um trecho de água entre dois PCs (Montante e Jusante)
  TTrechoDagua = class(THidroComponente)
  private
    FPC_aJusante: TPC;
    FPC_aMontante: TPC;
  protected
    Function ObterPrefixo: String; override;
    function ReceiveMessage(const MSG: TadvMessage): Boolean; override;
    procedure SalvarEmArquivo(Ini: TIF); override;
  public
    constructor Create(PC1, PC2: TPC; UmaTabelaDeNomes: TStrings; Projeto: TProjeto);
    destructor Destroy; override;

    property PC_aMontante : TPC  read FPC_aMontante write FPC_aMontante;
    property PC_aJusante  : TPC  read FPC_aJusante  write FPC_aJusante;
  end;

  // Ponto de Controle
  TPC = Class(THidroComponente)
  private
    FVisivel        : Boolean;
    FHierarquia     : Integer;
    FSubRede        : Integer;
    FPCs_aMontante  : TListaDeObjetos;
    FSubBacias      : TListaDeObjetos;
    FDerivacoes     : TListaDeObjetos;
    FDemandas       : TListaDeObjetos;
    FTD             : TTrechoDagua;

    function  GetPC_aMontante(Index: Integer): TPC;
    function  GetPCs_aMontante: Integer;
    function  GetNumSubBacias: Integer;
    function  GetPC_aJusante: TPC;
    function  GetSubBacia(index: Integer): TSubBacia;
    procedure SetVisivel(Value: Boolean);
    procedure SetPC_aJusante(const Value: TPC);
    function GetDerivacao(i: Integer): TDerivacao;
    function GetNumDerivacoes: Integer;
    function GetDemanda(i: Integer): TDemanda;
    function GetNumDemandas: Integer;
  protected
    // Permite a criação polimórfica de um trecho dágua
    function CriarTrechoDagua(ConectarEm: TPC): TTrechoDagua; virtual;

    procedure BalancoHidrico; virtual; abstract;
    function  ObterVazoesDeMontante: Real; virtual; abstract;
    function  ObterVazaoAfluenteSBs: Real; virtual; abstract;

    procedure AtualizarHint; override;
    function  ReceiveMessage(Const MSG: TadvMessage): Boolean; override;
    function  CriarImagemDoComponente: TdrBaseShape; override;
    procedure SalvarEmArquivo(Ini: TIF); override;
  public
    constructor Create(Pos: ImoPoint; Projeto: TProjeto; UmaTabelaDeNomes: TStrings);
    destructor  Destroy; override;

    function  ConectarObjeto(Obj: THidroComponente): Integer; override;
    procedure DesconectarObjetos; override;

    // Retorna verdadeiro se existe objetos conectados, DEVE ser implementado nos descendentes
    function  PossuiObjetosConectados: Boolean; virtual; abstract;

    // Verifica se um PC é o PC que está a montante deste
    function  Eh_umPC_aMontante(PC: THidroComponente): Boolean;

    // Remove o trecho de água deste PC
    procedure RemoverTrecho;

    // Remove o PC que está a montante deste (atrás)
    procedure DesconectarPC_aMontante(PC: TPC);

    // Conecta um PC a montante deste
    procedure ConectarPC_aMontante(PC: TPC);

    property SubRede    : Integer read FSubRede    write FSubRede;
    property Hierarquia : Integer read FHierarquia write FHierarquia;
    property Visivel    : Boolean read FVisivel    write SetVisivel;

    // Número de componentes conectados
    property PCs_aMontante : Integer read GetPCs_aMontante;
    property SubBacias     : Integer read GetNumSubBacias;
    property Derivacoes    : Integer read GetNumDerivacoes;
    property Demandas      : Integer read GetNumDemandas;

    // Componentes conectados
    property TrechoDagua               : TTrechoDagua read FTD;
    property PC_aJusante               : TPC          read GetPC_aJusante write SetPC_aJusante;
    property PC_aMontante [i: Integer] : TPC          read GetPC_aMontante;
    property SubBacia     [i: Integer] : TSubBacia    read GetSubBacia;
    property Derivacao    [i: Integer] : TDerivacao   read GetDerivacao;
  end;

  TReservatorio = class(TPC)
  protected
    function CriarImagemDoComponente: TdrBaseShape; override;
    function ObterPrefixo: String; Override;
  end;

  // Este tipo de objeto pode estar conectado a vários outros, por isso é que existe
  // a propriedade HCs (Hidro-Componentes) e a propriedade CCs (Coeficientes de Contribuição)
  TSubBacia = Class(THidroComponente)
  private
    FHCs : TListaDeObjetos; // Hidro-Componentes
    FCCs : TDoubleList;   // Coeficientes de Contribuição para cada Hidro-Componente
  protected
    Function  ObterPrefixo: String; override;
    function  CriarImagemDoComponente: TdrBaseShape; override;
    function  ReceiveMessage(const MSG: TadvMessage): Boolean; override;
    procedure SalvarEmArquivo(Ini: TIF); override;
    procedure LerDoArquivo(Ini: TIF; Const Secao: String); override;
  public
    constructor Create(Pos: ImoPoint; Projeto: TProjeto; UmaTabelaDeNomes: TStrings);
    destructor Destroy; override;

    function ObterVazaoAfluente(PC: TPC): Real; virtual; abstract;

    property HCs : TListaDeObjetos read FHCs;
    property CCs : TDoubleList   read FCCs;
  end;

  // Derivações são conectadas em PCs
  TDerivacao = class(THidroComponente)
  protected
    Function ObterPrefixo: String; Override;
    function CriarImagemDoComponente: TdrBaseShape; override;
    function ReceiveMessage(const MSG: TadvMessage): boolean; override;
  public
    constructor Create(Pos: ImoPoint; Projeto: TProjeto; UmaTabelaDeNomes: TStrings);
    destructor Destroy; override;
  end;

  // Demandas

  TDemanda = class(THidroComponente)
  private
    FSincronizaDados : Boolean;
    FSalvarLerBitmap : Boolean;
    FBitmapMudou     : Boolean;
    FHabilitada      : Boolean;
    FCategoria       : String;

    FDadosDeSinc     : TStrings;

    function  GetBitmap: TBitmap;
    procedure SetBitmap(B: TBitmap);
    procedure BitmapChange(Sender: TObject);
    function  getVisivel: Boolean;
    procedure setVisivel(const Value: Boolean);
    function getEh_Padrao: Boolean;
  protected
    // Obtem o nome para a seção do arquivo ini
    function  GetBitmapName: String; virtual;

    // Deverá ser sobreecrito em cada descendente para compatibilização com o mecanismo
    // de sincronização. "DadosParaSincronizar" será sempre um sub-conjunto de "DadosDeSincronizacao"
    // "Criando" indica que é uma nova demanda
    procedure AtribuirDados(Demanda: TDemanda; Criando: Boolean; DadosParaSincronizar: TStrings); virtual;

    procedure PegarDadosDoDialogo(d: THidroDialogo); override;
    procedure PorDadosNoDialogo(d: THidroDialogo); override;
    procedure SalvarEmArquivo(Ini: TIF); override;
    procedure LerDoArquivo(Ini: TIF; const Secao: String); override;
    function  CriarImagemDoComponente: TdrBaseShape;  override;
    function  CriarDialogo: THidroDialogo; override;
    function  ObterPrefixo: String; override;
    function  ReceiveMessage(const MSG: TadvMessage): Boolean; override;
  public
    constructor Create(Pos: ImoPoint; Projeto: TProjeto;
                       UmaTabelaDeNomes: TStrings;
                       const Categoria: String);

    destructor Destroy; override;

    // Verifica se um dado é para ser sincronizado com o dado da Demanda Padrao
    function Sincronizar(const Dado: String; DadosParaSincronizar: TStrings): Boolean;

    // Associa os dados de um Padrão com a Demanda que está sendo criada ou com
    // uma Demanda já existente.
    procedure Atribuir(Demanda: TDemanda; Criando: Boolean; DadosParaSincronizar: TStrings);

    // Indica que o usuário deseja sincronizar os dados com as demandas que batem
    // com o padrão desta.
    property SincronizarDados : Boolean read FSincronizaDados;

    // Indica se o usuário trocou a imagem que representa o padrão desta demanda
    property BitmapMudou : Boolean read FBitmapMudou write FBitmapMudou;

    // Imagem que representa a Categoria da Demanda se demanda padrão
    property Bitmap : TBitmap read GetBitmap write SetBitmap;

    // Identificação da Categoria a que esta Demanda pertence.
    // Se branco, significa que é uma demanda padrão sendo seu nome a identificação
    property Categoria : String read FCategoria;

    // Verifica se esta demanda é padrão
    property Eh_Padrao : Boolean read getEh_Padrao;

    // Indica se a demanda está visível ou não
    property Visivel : Boolean read getVisivel write setVisivel;

    // Indica se esta demanda deverá ser considerada em um projeto
    property Habilitada : Boolean read FHabilitada write FHabilitada;

    // Define os dados que podem ser sincronizados
    property DadosDeSincronizacao : TStrings read FDadosDeSinc;
  end;

  TEvento_Mudanca = TNotifyEvent;

  TListaDeDemandasPadroes = class
  private
    FTN       : TStrings;
    FList     : TStrings;
    FEM       : TEvento_Mudanca;
    FProjeto  : TProjeto;

    function GetPadroes: Integer;
    function getPadrao(i: Integer): TDemanda;
    function getBitmap(i: Integer): TBitmap;
    procedure RemoverObjeto(indice: Integer);
  public
    constructor Create(Projeto: TProjeto; TabelaDeNomes: TStrings);
    destructor Destroy; override;

    procedure LerDoArquivo(Arquivo: TIF);
    procedure SalvarEmArquivo(Arquivo: TIF);

    procedure Adicionar(DM: TDemanda);
    procedure Editar(DM: TDemanda);
    function  Remover(DM: TDemanda): Boolean;

    // Limpa a lista
    procedure Limpar;

    // Obtém o Padrao de Demanda dado o identificador do Padrao
    // Se nenhum for achado "nil" será retornado
    function ObterPeloNome(Const Padrao: String): TDemanda;

    // Retorna o número de padrões existentes
    property Padroes : Integer read GetPadroes;

    // Notifica que houve uma mudança na lista
    property Evento_Mudanca : TEvento_Mudanca read FEM write FEM;

    // Obtém o padrão dado seu índice
    property Padrao[i: Integer]: TDemanda read getPadrao; default;

    // Obtém a imagem que representa o padrão dado seu índice
    property Bitmap[i: Integer]: TBitmap read getBitmap;
  end;

  // Representa todos os PCs de um Projeto
  // Esta lista deve ser mantida por ordem crescente de Hierarquia
  TListaDePCs = Class
  private
    FList : TListaDeObjetos;
    function GetPC(Index: Integer): TPC;
    function GetNumPCs: Integer;
    procedure SetPC(index: Integer; const Value: TPC);
    procedure Ordenar;
  public
    constructor Create;
    destructor Destroy; override;

    function Adicionar (PC: TPC): Integer;
    function Remover   (PC: TPC): boolean;
    function IndiceDo  (PC: TPC): Integer;

    // Realiza o algorítimo de cálculo de hierarquia para cada PC e já ordena-os.
    // Também já define as Sub-Redes e atualiza o hint dos PCs
    procedure CalcularHierarquia;

    property PCs: Integer read GetNumPCs;
    property PC[index: Integer]: TPC read GetPC write SetPC; Default;
  end;

  TEventoDeNotificacao = TNotifyEvent;

  TProcPSB = procedure(Projeto: TProjeto; SB: TSubBacia);
  TProcDer = procedure(Projeto: TProjeto; Der: TDerivacao);

  TStatusProjeto = (sFazendoNada, sSimulando, sOtimizando);

  {Todo Projeto está associado a uma Área de Projeto.
  Esta classe encapsula a "rede hidrológica" possuindo assim todos os componentes que a formam.}
  TProjeto = Class(THidroComponente)
  private
    FMap                : TMapEx;                  // Aponta para o Mapa da área de Projeto
    FIni                : TIF;
    FNomeArqFundo       : String;
    FFundoBmp           : TBitmap;
    FDirSaida           : String;                  // temporário para cada simulação (DOS)
    FNomeArquivo        : String;
    FPCs                : TListaDePCs;
    FDirSai             : String;                  // Dir. de saída de resultados
    FDirTrab            : String;                  // Dir. de trabalho (database)
    FAreaDeProjeto      : TForm;                   // Onde a rede é mostrada
    FSubBacias          : TList;                   // Auxiliar para obtenção das Sub-Bacias de um projeto
    FDerivacoes         : TList;                   // Auxiliar para obtenção das Derivações de um projeto
    FDemandas           : TLIst;                   // Auxiliar para obtenção das Demandas de um projeto
    FSimulador          : TSimulation;             // Controlador do mecanismo de simulação
    FStatus             : TStatusProjeto;          // Armazena o status de execução do projeto
    FRes                : TGerenteDeResultados;    // Resultados da simulação
    FDemandasPadroes    : TListaDeDemandasPadroes; // Lista dos padrões de demandas definidos pelo usuário

    procedure SetFundo(Value: String);
    function GetDirTrab: String;
    function GetDirSai: String;

    // Métodos internos de simulação
    procedure priv_EventoDeTempo(Sender: TObject; out EventID: Integer);
    procedure priv_EventoDeSimulacao(Sender: TObject; const EventID: Integer);
  protected
    // Responsável pela criação do Gerenciador de Resultados específico
    // Caso um não seja fornecido uma exceção será gerada.
    function CriarGerenteDeResultados: TGerenteDeResultados; virtual;

    { Métodos para implementação de uma simulação *********************** }

    // Sobreecreva este método para controlar o relógio de simulação
    procedure EventoDeTempo; virtual;

    // Sobreecreva este método para controlar a simulação
    procedure EventoDeSimulacao; virtual;

    { Fim Métodos para implementação de uma simulação *********************** }

    procedure SetModificado(const Value: Boolean); override;
    Function  ObterPrefixo: String; override;
    procedure LerDoArquivo(Ini: TIF; const Secao: String); Overload; override;
    procedure SalvarEmArquivo(Ini: TIF); Overload; override;
    procedure PegarDadosDoDialogo(d: THidroDialogo); override;
    procedure PorDadosNoDialogo(d: THidroDialogo); override;
    function  CriarDialogo: THidroDialogo; override;
    function  ReceiveMessage(Const MSG: TadvMessage): Boolean; override;
    procedure ValidarDados(var TudoOk: Boolean;
                              DialogoDeErros: TErros_DLG;
                              Completo: Boolean = False); override;
  public
    constructor Create(UmaTabelaDeNomes: TStrings; AreaDeProjeto: TForm);
    destructor  Destroy; override;

    { GIS Methods ------------------------------------------------------------ }

    // Para este método trabalhar o diretório de trabalho precisará já estar
    // apontando para um diretório válido.
    function AddLayer(FileName: String): IDispatch;

    // Métodos de conversão TPoint <--> ImoPoint
    function moPointToPoint(p: ImoPoint): TPoint;
    function PointTo_moPoint(p: TPoint): ImoPoint;

    { GIS Methods ------------------------------------------------------------ }


    // Cria um descendente de THidroComponente.
    // Este método deverá ser obrigatoriamente implementado pelos descendentes e deverá
    // fornecer pelo menos a possibilidade de criação de PCs, Reservatórios, Sub-Bacias,
    // Derivações e Demandas.
    // Os valores que "ID" poderá assumir são:
    //   "PC", "Reservatorio", "Sub-Bacia", "Derivacao", "Demanda" ou diretamante o nome
    //   das classes dos objetos, por exemplo, "Txx_PC", ""Txx_RES".
    // "Pos" é a posição na tela onde o objeto será criado.
    function CriarObjeto(const ID: String; Pos: ImoPoint): THidroComponente; virtual;

    function  ObterSubBacias: TList;
    function  ObterDerivacoes: TList;

    procedure LerDoArquivo(const Nome: String); overload;
    procedure SalvarEmArquivo(const Nome: String); overload;
    procedure PercorrerSubBacias(ITSB: TProcPSB);
    procedure PercorrerDerivacoes(ITDer: TProcDer);
    function  ObterDiretorioDoProjeto: String;

    // Mecanismo de Simulação
    function  RealizarDiagnostico(Completo: Boolean = False): Boolean; virtual;
    procedure ExecutarSimulacao; virtual; abstract;
    procedure TerminarSimulacao; virtual;

    // Retorna o PC dado seu nome
    function PCPeloNome(const Nome: String): TPC;

    // Dir. de saída de resultados
    property DirSai : String read GetDirSai write FDirSai;

    // Dir. de trabalho (database)
    property DirTrab : String read GetDirTrab write FDirTrab;

    // Nome dos arquivos
    property NomeArquivo  : String read FNomeArquivo   write FNomeArquivo;
    property NomeArqFundo : String read FNomeArqFundo  write SetFundo;

    // Mapa (fundo da Janela)
    property Map : TMapEx read FMap;

    // PCs que formam a rede hidrológica
    property PCs : TListaDePCs read FPCs write FPCs;

    // Situação atual do projeto
    property Status : TStatusProjeto read FStatus write FStatus;

    property FundoBmp  : TBitmap     read FFundoBmp;
    property Simulador : TSimulation read FSimulador;
    property SubBacias : TList       read FSubBacias;

    // Armazena as demendas padrões definidas pelo usuário
    property DemandasPadroes : TListaDeDemandasPadroes read FDemandasPadroes;

    // Área de Projeto ao qual este projeto está conectado
    property AreaDeProjeto : TForm read FAreaDeProjeto;

    // Resultados da simulação
    property Resultados : TGerenteDeResultados read FRes;
  end;

  // Gera uma excessão de método não implementado
  procedure NaoImplementado(const Metodo, Classe: String);

  // Rotinas auxiliares para salvamento de objetos
  procedure SalvarSubBacia(Projeto: TProjeto; SB: TSubBacia);
  procedure SalvarDerivacao(Projeto: TProjeto; Der: TDerivacao);

  procedure SetGlobalStatus(TudoCorreto: Boolean);

  procedure ObterDemandasPeloPadrao(const Padrao: String; var Demandas: TStrings; Projeto: TObject);

implementation
uses WinUtils,
     FileUtils,
     GraphicUtils,
     hidro_AreaDeProjeto,
     hidro_Constantes,
     hidro_Variaveis,

     // Diálogos básicos
     hidro_Dialogo_Projeto,
     hidro_Dialogo_Demanda,
     hidro_Dialogo_DemandaPadrao;

procedure NaoImplementado(const Metodo, Classe: String);
begin
  raise Exception.CreateFmt(cMsgError07, [Metodo, Classe]);
end;

procedure SetGlobalStatus(TudoCorreto: Boolean);
begin
  if TudoCorreto then
     Hidro_Variaveis.gLeds.Picture.Bitmap.Handle := LoadBitmap(hInstance, 'LED_GREEN')
  else
     Hidro_Variaveis.gLeds.Picture.Bitmap.Handle := LoadBitmap(hInstance, 'LED_RED');
end;

function PossuiCaminho(const Arquivo: String): Boolean;
begin
  Result := System.Pos('\', Arquivo) > 0;
end;

// Realiza um type-casting, só isso
Function AP(F: TForm): THidroAreaDeProjeto;
begin
  Result := THidroAreaDeProjeto(F);
end;

function ObterObjetoPeloNome(const Nome: String; Projeto: TObject): TObject;
var p: pRecObjeto;
begin
  New(p);
  p.Obj := nil;

  if Nome <> '' then
     GetMessageManager.SendMessage(UM_OBTER_OBJETO, [@Nome, p, Projeto]);

  Result := p.Obj;
  Dispose(p);
end;

procedure ObterDemandasPeloPadrao(const Padrao: String; var Demandas: TStrings; Projeto: TObject);
begin
  Demandas.Clear;
  GetMessageManager.SendMessage(UM_OBTER_DEMANDA_PELO_PADRAO, [@Padrao, Demandas, Projeto]);
end;

{ TListaDeObjetos }

function TListaDeObjetos.Adicionar(Objeto: THidroComponente): Integer;
begin
  Result := FList.Add(Objeto);
end;

constructor TListaDeObjetos.Create;
begin
  Inherited Create;
  FList := TList.Create;
end;

procedure TListaDeObjetos.Deletar(Indice: Integer);
begin
  FList.Delete(Indice);
end;

destructor TListaDeObjetos.Destroy;
var i: Integer;
begin
  if FLiberarObjetos then
     for i := 0 to FList.Count-1 do
       THidroComponente(FList[i]).Free;

  FList.Free;
  Inherited Destroy;
end;

function TListaDeObjetos.getNumObjetos: Integer;
begin
  Result := FList.Count;
end;

function TListaDeObjetos.getObjeto(index: Integer): THidroComponente;
begin
  Result := THidroComponente(FList[index]);
end;

function TListaDeObjetos.IndiceDo(Objeto: THidroComponente): Integer;
begin
  Result := FList.IndexOf(Objeto);
end;

procedure TListaDeObjetos.Ordenar(FuncaoDeComparacao: TListSortCompare);
begin
  FList.Sort(FuncaoDeComparacao);
end;

function TListaDeObjetos.Remover(Objeto: THidroComponente): Integer;
begin
  Result := FList.Remove(Objeto);
end;

procedure TListaDeObjetos.RemoverNulos;
begin
  FList.Pack;
end;

procedure TListaDeObjetos.setObjeto(index: Integer; const Value: THidroComponente);
begin
  if FList.Count > 0 then
     FList[index] := Value;
end;

{ TListaDePCs }

constructor TListaDePCs.Create;
begin
  Inherited Create;
  FList := TListaDeObjetos.Create;
end;

destructor TListaDePCs.Destroy;
var i: Integer;
begin
  for i := 0 to FList.Objetos-1 do
    begin
    PC[i].DesconectarObjetos;
    PC[i].Free;
    end;

  FList.Free;
  Inherited Destroy;
end;

function TListaDePCs.Adicionar(PC: TPC): Integer;
begin
  PC.Modificado := True;
  Result := FList.Adicionar(PC);
end;

function TListaDePCs.Remover(PC: TPC): boolean;
var PS  : TPC; // PC seguinte
    i   : Integer;
begin
  Result := False;

  if PC.PossuiObjetosConectados then
     MessageDLG('Primeiro remova todos os objetos conectados a este PC.',
                 mtInformation, [mbOK], 0)
  else
     begin
     PS := PC.PC_aJusante;

     if PS <> nil then
        begin
        PS.DesconectarPC_aMontante(PC);
        for i := 0 to PC.PCs_aMontante - 1 do
           PC.PC_aMontante[i].ConectarObjeto(PS);
        end
     else
        while PC.PCs_aMontante > 0 do
           PC.PC_aMontante[0].RemoverTrecho;

     FList.Remover(PC);
     PC.DesconectarObjetos;
     PC.Free;
     CalcularHierarquia;
     Result := True;
     end;
end;

function TListaDePCs.GetNumPCs: Integer;
begin
  Result := FList.Objetos;
end;

function TListaDePCs.GetPC(Index: Integer): TPC;
begin
  try
    Result := TPC(FList[Index]);
  except
    Raise Exception.Create('Índice do PC inválido: ' + IntToStr(Index));
  end;
end;

// Realiza o algorítimo de cálculo de hierarquia para cada PC e já ordena-os.
// Também já define as Sub-Redes e atualiza o hint dos PCs
procedure TListaDePCs.CalcularHierarquia;

   procedure AtribuiIDparaSubRede(PC: TPC; k: Integer);
   var i: Integer;
   begin
     PC.SubRede := k;
     PC.AtualizarHint;
     for i := 0 to PC.PCs_aMontante-1 do
       AtribuiIDparaSubRede(PC.PC_aMontante[i], k);
   end;

var i, k: Integer;
    PC: TPC;
begin
  // Primero calcula a hierarquia de cada PC
  for i := 0 to PCs-1 do
    if Self.PC[i].PCs_aMontante = 0 then
       begin
       k := 1;
       PC := Self.PC[i];
       PC.Hierarquia := k;
       while PC.PC_aJusante <> nil do
         begin
         inc(k);
         PC := PC.PC_aJusante;
         if PC.Hierarquia < k then PC.Hierarquia := k;

         // previne a entrada em loop infinito caso o usuário feche um ciclo
         if k = 1000 then Raise Exception.Create('Erro: Um ciclo recursivo foi fechado.'#13 +
                                                 'Por favor, desfaça.');
         end;
       end;

  Ordenar;

  // Calcula as Sub-Redes
  k := 0;
  for i := 0 to PCs-1 do
    if Self.PC[i].PC_aJusante = nil then // ultimo PC da i-egima sub-Rede
       begin
       inc(k);

       // Varre esta Sub-Rede de traz para frente e lhe atribui um ID de Sub-Rede
       AtribuiIDparaSubRede(Self.PC[i], k);
       end;
end;

function FuncaoDeComparacao(pc1, pc2: Pointer): Integer;
begin
  Result := TPC(pc1).Hierarquia - TPC(pc2).Hierarquia;
end;

procedure TListaDePCs.Ordenar;
begin
  FList.Ordenar(FuncaoDeComparacao);
end;

function TListaDePCs.IndiceDo(PC: TPC): Integer;
begin
  Result := FList.IndiceDo(PC);
end;

procedure TListaDePCs.SetPC(index: Integer; const Value: TPC);
begin
  try
    FList[Index] := Value;
  except
    Raise Exception.Create('Índice de PC inválido: ' + IntToStr(Index));
  end;
end;

{ THidroDados }

procedure THidroDados.LerDoArquivo(Ini: TIF; const Secao: String);
begin
  NaoImplementado('LerDoArquivo', ClassName);
end;

procedure THidroDados.PegarDadosDoDialogo(d: THidroDialogo);
begin
  NaoImplementado('PegarDadosDoDialogo', ClassName);
end;

procedure THidroDados.PorDadosNoDialogo(d: THidroDialogo);
begin
  NaoImplementado('PorDadosNoDialogo', ClassName);
end;

procedure THidroDados.SalvarEmArquivo(Ini: TIF; const Secao: String);
begin
  NaoImplementado('SalvarEmArquivo', ClassName);
end;

procedure THidroDados.ValidarDados(var TudoOk: Boolean;
                      DialogoDeErros: TErros_DLG;
                      Completo: Boolean);
begin
  NaoImplementado('ValidarDados', ClassName);
end;

{ THidroComponente }

constructor THidroComponente.Create(UmaTabelaDeNomes: TStrings; Projeto: TProjeto);
begin
  inherited Create;

  FAvisarQueVaiSeDestruir := True;

  self.FProjeto := Projeto;
  TN            := UmaTabelaDeNomes;
  FNome         := ObterNome;
  FComentarios  := TStringList.Create;
  FDados        := CriarDados;

  GetMessageManager.RegisterMessage(UM_SET_VISIBILITY, Self);
  GetMessageManager.RegisterMessage(UM_RESET_VISIT, Self);
  GetMessageManager.RegisterMessage(UM_OBJETO_SE_DESTRUINDO, self);
  GetMessageManager.RegisterMessage(UM_OBTER_OBJETO, self);
  GetMessageManager.RegisterMessage(UM_INICIAR_SIMULACAO, self);
  GetMessageManager.RegisterMessage(UM_BLOQUEAR_OBJETOS, self);

  if TN <> nil then TN.Add(FNome);
  AtualizarHint;
end;

destructor THidroComponente.Destroy;
var i: Integer;
begin
  FComentarios.Free;

  GetMessageManager.UnRegisterMessage(UM_SET_VISIBILITY, Self);
  GetMessageManager.UnRegisterMessage(UM_RESET_VISIT, Self);
  GetMessageManager.UnRegisterMessage(UM_OBTER_OBJETO, self);
  GetMessageManager.UnRegisterMessage(UM_OBJETO_SE_DESTRUINDO, self);
  GetMessageManager.UnRegisterMessage(UM_INICIAR_SIMULACAO, self);
  GetMessageManager.UnRegisterMessage(UM_BLOQUEAR_OBJETOS, self);

  if TN <> nil then
     begin
     i := TN.IndexOf(FNome);
     if i > -1 then TN.Delete(i);
     end;

  FImagemDoComponente.Free;
  FDados.Free;

  Inherited Destroy;
end;

procedure THidroComponente.Avisar_e_Destruir;
begin
  if AvisarQueVaiSeDestruir then
     GetMessageManager.SendMessage(UM_OBJETO_SE_DESTRUINDO, [Self]);

  Free;
end;

procedure THidroComponente.SetModificado(const Value: Boolean);
begin
  FModificado := Value;
  if FProjeto <> nil then
     FProjeto.Modificado := Value;
end;

function THidroComponente.ObterSecaoDoIni: String;
begin
  Result := 'Dados ' + FNome;
end;

procedure THidroComponente.SetNome(const Value: String);
var i: Integer;
begin
  if CompareText(Value, FNome) <> 0 then
     begin
     i := TN.IndexOf(FNome);
     if i > -1 then TN.Delete(i);
     FNome := Value;
     TN.Add(FNome);
     end;
end;

function THidroComponente.ReceiveMessage(const MSG: TadvMessage): Boolean;
var i: Integer;
    s: String;
begin
  if MSG.ID = UM_SET_VISIBILITY then
     setVisivel(MSG.ParamAsBoolean(0))
  else

  if MSG.ID = UM_RESET_VISIT then
     Visitado := False
  else

  if MSG.ID = UM_BLOQUEAR_OBJETOS then
     begin
     if MSG.ParamAsObject(1) = self.Projeto then
        FBloqueado := MSG.ParamAsBoolean(0);
     end else

  if MSG.ID = UM_OBTER_OBJETO then
     {Verifica se o nome do objeto atual bate com o parâmetro passado.
      Se opcionalmente um terceiro parâmetro for passado (projeto) verificamos
      se este objeto pertence a este projeto.}
     begin
     s := MSG.ParamAsString(0);

     if (CompareText(s, Nome) = 0) then
        if (Length(s) = 3) then
           if (MSG.ParamAsObject(2) = FProjeto) then
              pRecObjeto(MSG.ParamAsObject(1)).Obj := Self
           else
              {nada}
        else
           pRecObjeto(MSG.ParamAsObject(1)).Obj := Self
     end else

    if MSG.ID = UM_INICIAR_SIMULACAO then
       if MSG.ParamAsObject(0) = FProjeto then
          PrepararParaSimulacao
       else
    else
end;

function THidroComponente.MostrarDialogo(const Caption: String = ''): Integer;
var i: Integer;
begin
  if FProjeto.Simulador <> nil then Exit;
  Result := mrNone;

  StartWait;
  try
    Dialogo := CriarDialogo;
    if Caption <> '' then Dialogo.Caption := Caption;
  finally
    StopWait;
  end;

  i := TN.IndexOf(FNome);
  if i <> -1 then TN.Delete(i);
  Try
    PorDadosNoDialogo(Dialogo);
    Dialogo.Bloqueado := FBloqueado;
    Dialogo.Hide;
    Result := Dialogo.ShowModal;
    if (Result = mrOk) and (not FBloqueado) then
       begin
       SetModificado(True);
       PegarDadosDoDialogo(Dialogo);
       end;
  Finally
    if i <> -1 then TN.Add(FNome);
    Dialogo.Free;
    Dialogo := nil;
  End;
end;

procedure THidroComponente.PorDadosNoDialogo(d: THidroDialogo);
begin
  d.edNome.Text       := FNome;
  d.mComentarios.Text := FComentarios.Text;

  if FDados <> nil then
     FDados.PorDadosNoDialogo(d);
end;

procedure THidroComponente.PegarDadosDoDialogo(d: THidroDialogo);
var s: String;
begin
  if FComentarios.Text <> d.mComentarios.Text then
     begin
     FComentarios.Text := d.mComentarios.Text;
     GetMessageManager.SendMessage(UM_COMENTARIO_OBJETO_MUDOU, [Self]);
     end;

  if FNome <> d.edNome.Text then
     begin
     s := FNome;
     FNome := d.edNome.Text;
     GetMessageManager.SendMessage(UM_NOME_OBJETO_MUDOU, [Self, @s, @FNome]);
     end;

  if FDados <> nil then
     FDados.PegarDadosDoDialogo(d);

  AtualizarHint;
end;

procedure THidroComponente.DuploClick(Sender: TObject);
begin
  MostrarDialogo;
end;

procedure THidroComponente.SalvarEmArquivo(Ini: TIF);
var s, Secao: String;
begin
  inherited;

  Secao := ObterSecaoDoIni;
  with Ini do
    begin
    WriteString (Secao, 'Nome'     , FNome);
    WriteString (Secao, 'Classe'   , Self.ClassName);
    WriteBool   (Secao, 'Bloqueado', FBloqueado);

    { Isto é necessário devido a um possível bug em (TStrings.CommaText).
      Quando tentamos atribuir uma string no formato ["aaaaaa bbbb ccccc"] ele
      se perde. Se houver uma única (,) no final, isto já é suficiente para o
      erro não acontecer. Ex: ["aaaaa bbbb cccc",]}
    s := FComentarios.CommaText;
    if Length(s) > 0 then
       if s[Length(s)] <> ',' then s := s + ',';
    WriteString(Secao, 'Comentarios', s);

    if FImagemDoComponente <> nil then
       begin
       WriteFloat (Secao, 'x', FPos.x);
       WriteFloat (Secao, 'y', FPos.y);
       end;

    if FDados <> nil then
       FDados.SalvarEmArquivo(Ini, Secao);
    end;
end;

function THidroComponente.CriarDialogo: THidroDialogo;
begin
  Result := THidroDialogo.Create(TN, self);
end;

function THidroComponente.ObterNome: String;
var i: Integer;
    Prefixo: String;
begin
  if TN <> nil then
     begin
     i := 0;
     Prefixo := ObterPrefixo;
     repeat
       inc(i);
       Result := Prefixo + IntToStr(i);
       until TN.IndexOf(Result) = -1
     end;
end;

function THidroComponente.ObterPrefixo: String;
begin
  Result := 'Objeto_';
end;

procedure THidroComponente.Diagnostico(var TudoOk: Boolean; Completo: Boolean = False);
begin
  if FDados <> nil then
     FDados.ValidarDados(TudoOk, gErros, Completo);
end;

procedure THidroComponente.CriarComponenteVisual(Pos: ImoPoint);
begin
  FImagemDoComponente := CriarImagemDoComponente; // Método virtual
  if FImagemDoComponente <> nil then
     begin
     FImagemDoComponente.Tag         := Integer(self);
     FImagemDoComponente.Parent      := FProjeto.Map;
     FImagemDoComponente.OnMouseDown := AP(FProjeto.AreaDeProjeto).Map_MouseDown;
     FImagemDoComponente.OnMouseMove := AP(FProjeto.AreaDeProjeto).Map_MouseMove;
     FImagemDoComponente.OnMouseUp   := AP(FProjeto.AreaDeProjeto).Map_MouseUp;
     FImagemDoComponente.OnClick     := AP(FProjeto.AreaDeProjeto).Map_Click;
     FImagemDoComponente.OnDblClick  := DuploClick;
     SetPos(Pos);
     end
  else
     NaoImplementado('CriarImagemDoComponente', ClassName);
end;

procedure THidroComponente.LerDoArquivo(Ini: TIF; Const Secao: String);
var p: ImoPoint;
begin
  FNome      := Ini.ReadString (Secao, 'Nome',      '');
  FBloqueado := Ini.ReadBool   (Secao, 'Bloqueado', False);

  FComentarios.CommaText := Ini.ReadString(Secao, 'Comentarios', '');

  if FImagemDoComponente <> nil then
     begin
     p := MapObjects.CoPoint.Create;
     p.x := Ini.ReadFloat(Secao, 'X', 0);
     p.y := Ini.ReadFloat(Secao, 'Y', 0);
     SetPos(p);
     end;

  if FDados <> nil then
     FDados.LerDoArquivo(Ini, Secao);

  AtualizarHint;
end;

function THidroComponente.GetPos: ImoPoint;
begin
  if FImagemDoComponente <> nil then
     Result := FPos
  else
     Result := nil;
end;

function THidroComponente.GetRect: TRect;
begin
  if FImagemDoComponente <> nil then
     with FImagemDoComponente do
        Result := Classes.Rect(Left, Top, Left + Width, Top + Height)
  else
     Result := Rect(0, 0, 0, 0);
end;

procedure THidroComponente.SetPos(const Value: ImoPoint);
var p: TPoint;
begin
  if FImagemDoComponente <> nil then
     begin
     SetModificado(True);
     FPos := Value;
     p := GetScreenPos;
     FImagemDoComponente.Left := p.x - FImagemDoComponente.Width  div 2;
     FImagemDoComponente.Top  := p.y - FImagemDoComponente.Height div 2;
     end;
end;

procedure THidroComponente.AtualizarHint;
var s: String;
begin
  if FImagemDoComponente <> nil then
     begin
     s := FNome;
     if FComentarios.Count > 0 then s := s + #13#10 + FComentarios.Text;
     FImagemDoComponente.Hint := s;
     end;
end;

procedure THidroComponente.PrepararParaSimulacao;
begin
end;

procedure THidroComponente.MostrarMenu(x, y: Integer);
var p, p2: TPoint;
begin
  if FMenu <> nil then
     begin
     PrepararMenu;
     if x <> -1 then
        FMenu.Popup(x, y)
     else
        begin
        p  := FProjeto.AreaDeProjeto.ClientToScreen(Types.Point(0, 0));
        p2 := GetScreenPos;
        FMenu.Popup(p.x + p2.x, p.y + p2.y);
        end;
     end;
end;

function THidroComponente.VerificarCaminho(var Arquivo: String): Boolean;
begin
  if Arquivo <> '' then
     begin
     if not PossuiCaminho(Arquivo) then
        begin
        if FProjeto.DirTrab <> '' then
           SetCurrentDir(FProjeto.DirTrab)
        else
           SetCurrentDir(ExtractFilePath(FProjeto.NomeArquivo));

        Arquivo := ExpandFileName(Arquivo);
        end;

     Result := FileExists(Arquivo);
     end
  else
     Result := False;
end;

procedure THidroComponente.RetirarCaminhoSePuder(var Arquivo: String);
var s: String;
begin
  if PossuiCaminho(Arquivo) then
     begin
     s := ExtractFilePath(FProjeto.NomeArquivo);
     if LastChar(s) = '\' then DeleteLastChar(s);

     if CompareText(s, Arquivo) = 0 then
        Arquivo := ''
     else
        Arquivo := ExtractRelativePath(s + '\', Arquivo);
     end;
end;

procedure THidroComponente.PrepararMenu;
begin
  // Nada por enquanto
end;

function THidroComponente.ConectarObjeto(Obj: THidroComponente): Integer;
begin
  // Nada
end;

function THidroComponente.CriarImagemDoComponente: TdrBaseShape;
begin
  Result := nil;
end;

procedure THidroComponente.DesconectarObjetos;
begin
  // Nada
end;

function THidroComponente.CriarDados: THidroDados;
begin
  Result := nil;
end;

procedure THidroComponente.ValidarDados(var TudoOk: Boolean;
                                        DialogoDeErros: TErros_DLG;
                                        Completo: Boolean);
begin
  // Nada
end;

function THidroComponente.GetScreenPos: TPoint;
var x, y: Single;
begin
  if FPos <> nil then
     begin
     FProjeto.Map.FromMapPoint(FPos, x, y);
     Result.x := Trunc(x);
     Result.y := Trunc(y);
     end
  else
     Result := Types.Point(0, 0);
end;

function THidroComponente.getVisivel: Boolean;
begin
  if FImagemDoComponente <> nil then
     Result := FImagemDoComponente.Visible
  else
     Result := false;
end;

procedure THidroComponente.setVisivel(const Value: Boolean);
begin
  if FImagemDoComponente <> nil then
     FImagemDoComponente.Visible := Value;
end;

{ TTrechoDagua }

constructor TTrechoDagua.Create(PC1, PC2: TPC; UmaTabelaDeNomes: TStrings; Projeto: TProjeto);
begin
  inherited Create(UmaTabelaDeNomes, Projeto);
  GetMessageManager.RegisterMessage(UM_TROCAR_REFERENCIA, self);
  PC_aMontante := PC1;
  PC_aJusante  := PC2;
end;

destructor TTrechoDagua.Destroy;
begin
  GetMessageManager.UnRegisterMessage(UM_TROCAR_REFERENCIA, self);
  inherited Destroy;
end;

function TTrechoDagua.ObterPrefixo: String;
begin
  Result := 'TrechoDagua_';
end;

function TTrechoDagua.ReceiveMessage(const MSG: TadvMessage): Boolean;
var i: Integer;
begin
  if MSG.ID = UM_TROCAR_REFERENCIA then
     begin
     if MSG.ParamAsObject(0) = PC_aJusante then
        PC_aJusante := TPC(MSG.ParamAsObject(1)) else

     if MSG.ParamAsObject(0) = PC_aMontante then
        PC_aMontante := TPC(MSG.ParamAsObject(1));
     end;

  inherited ReceiveMessage(MSG);
end;

procedure TTrechoDagua.SalvarEmArquivo(Ini: TIF);
var Secao: String;
begin
  Inherited SalvarEmArquivo(Ini);
  Secao := ObterSecaoDoIni;
  with Ini do
    begin
    WriteString (Secao, 'PM', PC_aMontante.Nome);
    WriteString (Secao, 'PJ', PC_aJusante. Nome);
    end;
end;

{ TPC }

constructor TPC.Create(Pos: ImoPoint; Projeto: TProjeto; UmaTabelaDeNomes: TStrings);
begin
  Inherited Create(UmaTabelaDeNomes, Projeto);

  FHierarquia  := -1;

  FPCs_aMontante  := TListaDeObjetos.Create;
  FSubBacias      := TListaDeObjetos.Create;
  FDerivacoes     := TListaDeObjetos.Create;
  FDemandas       := TListaDeObjetos.Create;

  GetMessageManager.RegisterMessage(UM_REPINTAR_OBJETO, self);
  GetMessageManager.RegisterMessage(UM_TROCAR_REFERENCIA, self);

  CriarComponenteVisual(Pos);
end;

{Teoricamente, as listas deverão estar vazias}
destructor TPC.Destroy;
begin
  GetMessageManager.UnRegisterMessage(UM_REPINTAR_OBJETO, self);
  GetMessageManager.UnRegisterMessage(UM_TROCAR_REFERENCIA, self);

  FPCs_aMontante.Free;
  FSubBacias.Free;
  FDerivacoes.Free;
  FDemandas.Free;
  FTD.Free;

  Inherited Destroy;
end;

function TPC.ConectarObjeto(Obj: THidroComponente): Integer;
var PC: TPC;
    SB: TSubBacia;
begin
  SetModificado(True);

  // Objeto é uma Sub-Bacia ----------------------------------------------------------
  if Obj is TSubBacia then
     begin
     SB := TSubBacia(Obj);
     FSubBacias.Adicionar(SB);
     SB.HCs.Adicionar(Self);
     if SB.FHCs.Objetos > SB.CCs.Count then
        if SB.CCs.Count = 0 then
           SB.CCs.Add(1.0)
        else
           SB.CCs.Add(0.0);
     end
  else

  // Objeto é uma Derivação ----------------------------------------------------------
  if Obj is TDerivacao then
     begin
     FDerivacoes.Adicionar(Obj);
     end
  else

  // Objeto é uma Derivação ----------------------------------------------------------
  if Obj is TDemanda then
     begin
     FDemandas.Adicionar(Obj);
     end
  else

  // Objeto é um PC ------------------------------------------------------------------
  if Obj is TPC then
     begin
     PC := TPC(Obj);
     PC.ConectarPC_aMontante(Self);
     Self.PC_aJusante := PC;
     end;
end;

procedure TPC.DesconectarObjetos;
var i, k: Integer;
begin
  // Derivações

  for i := 0 to GetNumDerivacoes-1 do
    begin
    FDerivacoes[i].Free;
    FDerivacoes[i] := nil;
    end;

  FDerivacoes.RemoverNulos;

  // Demandas

  for i := 0 to GetNumDemandas-1 do
    begin
    FDemandas[i].Free;
    FDemandas[i] := nil;
    end;

  FDemandas.RemoverNulos;

  // Sub-Bacias

  for i := 0 to SubBacias - 1 Do
    if (SubBacia[i] <> nil) then
       begin
       if (SubBacia[i].FHCs.Objetos = 1) then
          begin
          SubBacia[i].DesconectarObjetos;
          SubBacia[i].Free;
          FSubBacias[i] := nil;
          end
       else
          begin
          k := SubBacia[i].FHCs.IndiceDo(Self);
          SubBacia[i].FHCs.Deletar(k);
          SubBacia[i].CCs.Delete(k);
          end;
       end;

  FSubBacias.RemoverNulos;
end;

procedure TPC.ConectarPC_aMontante(PC: TPC);
begin
  FPCs_aMontante.Adicionar(PC);
end;

function TPC.ReceiveMessage(const MSG: TadvMessage): Boolean;
var i: Integer;
begin
  if MSG.ID = UM_OBJETO_SE_DESTRUINDO then
    {Verifica se o objeto que está se destruindo está conectado a este PC.
     Se está, elimina a referência}
     begin
     i := FSubBacias.IndiceDo(THidroComponente(MSG.ParamAsObject(0)));
     if i > -1 then
        begin
        FSubBacias[i] := nil;
        FSubBacias.RemoverNulos;
        end;

     i := FDerivacoes.IndiceDo(THidroComponente(MSG.ParamAsObject(0)));
     if i > -1 then
        begin
        FDerivacoes[i] := nil;
        FDerivacoes.RemoverNulos;
        end;
{
     i := FDemandas.IndiceDo(THidroComponente(MSG.ParamAsObject(0)));
     if i > -1 then
        begin
        FDemandas[i] := nil;
        FDemandas.RemoverNulos;
        end;
}
     end
  else

  if MSG.ID = UM_REPINTAR_OBJETO then
     begin
     SetPos(FPos);
     FImagemDoComponente.Paint;
     end
  else

  if MSG.ID = UM_TROCAR_REFERENCIA then
     begin
     i := FPCs_aMontante.IndiceDo(THidroComponente(MSG.ParamAsObject(0)));
     if i > -1 then FPCs_aMontante.Objeto[i] := THidroComponente(MSG.ParamAsObject(1));
     end;

  inherited ReceiveMessage(MSG);
end;

procedure TPC.SalvarEmArquivo(Ini: TIF);
var i: Integer;
    Secao: String;
begin
  Inherited;
  Secao := ObterSecaoDoIni;
  with Ini do
    begin
    if TrechoDagua <> nil then
       WriteString (Secao, 'TD', TrechoDagua.Nome);

    WriteInteger (Secao, 'SubBacias', SubBacias);
    for i := 0 to SubBacias - 1 do
       WriteString (Secao, 'SB'+IntToStr(i+1), SubBacia[i].Nome);

    WriteInteger (Secao, 'Derivacoes', Derivacoes);
    for i := 0 to Derivacoes - 1 do
       WriteString (Secao, 'DER'+IntToStr(i+1), Derivacao[i].Nome);
    end;
end;

procedure TPC.SetVisivel(Value: Boolean);
begin
  FVisivel := Value;
  FImagemDoComponente.Visible := FVisivel;
end;

function TPC.GetPCs_aMontante: Integer;
begin
  Result := FPCs_aMontante.Objetos;
end;

function TPC.GetNumSubBacias: Integer;
begin
  FSubBacias.RemoverNulos;
  Result := FSubBacias.Objetos;
end;

function TPC.GetPC_aJusante: TPC;
begin
  if FTD <> nil then
     Result := FTD.PC_aJusante
  else
     Result := nil;
end;

procedure TPC.SetPC_aJusante(const Value: TPC);
begin
  if FTD <> nil then
     FTD.PC_aJusante := Value
  else
     FTD := CriarTrechoDagua(Value);
end;

function TPC.CriarTrechoDagua(ConectarEm: TPC): TTrechoDagua;
begin
  Result := TTrechoDagua.Create(Self, ConectarEm, TN, FProjeto);
end;

function TPC.GetPC_aMontante(Index: Integer): TPC;
begin
  Result := TPC(FPCs_aMontante[Index]);
end;

function TPC.GetSubBacia(index: Integer): TSubBacia;
begin
  Result := TSubBacia(FSubBacias[Index]);
end;

// Remove a coneccao do PC que está a montante deste PC
procedure TPC.DesconectarPC_aMontante(PC: TPC);
begin
  FPCs_aMontante.Remover(PC);
end;

procedure TPC.RemoverTrecho;
begin
  if FTD <> nil then
     begin
     SetModificado(True);
     FTD.PC_aJusante.DesconectarPC_aMontante(FTD.PC_aMontante);
     FTD.Free;
     FTD := nil;
     end;
end;

function TPC.CriarImagemDoComponente: TdrBaseShape;
begin
  Result := TdrRectangle.Create(nil);
  Result.Width := 10;
  Result.Height := 10;
end;

function TPC.Eh_umPC_aMontante(PC: THidroComponente): Boolean;
begin
  Result := (FPCs_aMontante.IndiceDo(PC) > -1);
end;

procedure TPC.AtualizarHint;
begin
  inherited;
  if FImagemDoComponente <> nil then
     FImagemDoComponente.Hint := FImagemDoComponente.Hint + #13#10 +
       Format('Sub-Rede: %d   Hierarquia: %d', [FSubRede, FHierarquia]);
end;

function TPC.GetDerivacao(i: Integer): TDerivacao;
begin
  Result := TDerivacao(FDerivacoes[i]);
end;

function TPC.GetNumDerivacoes: Integer;
begin
  FDerivacoes.RemoverNulos;
  Result := FDerivacoes.Objetos;
end;

function TPC.GetDemanda(i: Integer): TDemanda;
begin
  Result := TDemanda(FDemandas[i]);
end;

function TPC.GetNumDemandas: Integer;
begin
  FDemandas.RemoverNulos;
  Result := FDemandas.Objetos;
end;

{ TSubBacia }

constructor TSubBacia.Create(Pos: ImoPoint; Projeto: TProjeto; UmaTabelaDeNomes: TStrings);
begin
  inherited Create(UmaTabelaDeNomes, Projeto);

  FHCs := TListaDeObjetos.Create;
  FCCs := TDoubleList.Create;

  GetMessageManager.RegisterMessage(UM_REPINTAR_OBJETO, self);
  GetMessageManager.RegisterMessage(UM_TROCAR_REFERENCIA, self);

  CriarComponenteVisual(Pos);
end;

destructor TSubBacia.Destroy;
begin
  GetMessageManager.UnRegisterMessage(UM_REPINTAR_OBJETO, self);
  GetMessageManager.UnRegisterMessage(UM_TROCAR_REFERENCIA, self);

  FHCs.Free;
  FCCs.Free;
  inherited Destroy;
end;

procedure TSubBacia.SalvarEmArquivo(Ini: TIF);
var i: Integer;
    Secao: String;
begin
  Inherited SalvarEmArquivo(Ini);
  Secao := ObterSecaoDoIni;
  with Ini do
    begin
    WriteInteger (Secao, 'Coefs. de Contribuicao' , FCCs.Count);
    for i := 0 to FCCs.Count - 1 do
       WriteFloat (Secao, 'Coef'+IntToStr(i+1), FCCs[i]);
    end;
end;

function TSubBacia.ReceiveMessage(const MSG: TadvMessage): Boolean;
var i: Integer;
begin
  if MSG.ID = UM_REPINTAR_OBJETO then
     begin
     SetPos(FPos);
     FImagemDoComponente.Paint;
     end
  else

  if MSG.ID = UM_TROCAR_REFERENCIA then
     begin
     i := FHCs.IndiceDo(THidroComponente(MSG.ParamAsObject(0)));
     if i > -1 then FHCs.Objeto[i] := THidroComponente(MSG.ParamAsObject(1));
     end;

  inherited ReceiveMessage(MSG);
end;

function TSubBacia.ObterPrefixo: String;
begin
  Result := 'SubBacia_';
end;

function TSubBacia.CriarImagemDoComponente: TdrBaseShape;
begin
  Result := TdrBitmap.Create(nil, 'SUB_BACIA_20X20');
  Result.Width := 20;
  Result.Height := 20;
  TdrBitmap(Result).DrawFrame := True;
end;

procedure TSubBacia.LerDoArquivo(Ini: TIF; Const Secao: String);
var i, ii: Integer;
begin
  Inherited LerDoArquivo(Ini, Secao);
  with Ini do
    begin
    ii := ReadInteger (Secao, 'Coefs. de Contribuicao' , FCCs.Count);
    for i := 1 to ii do
       FCCs.Add(ReadFloat (Secao, 'Coef'+IntToStr(i), 0.0));
    end;
end;

{ TProjeto }

function TProjeto.ReceiveMessage(Const MSG: TadvMessage): Boolean;
begin
  inherited ReceiveMessage(MSG);
end;

function TProjeto.PCPeloNome(const Nome: String): TPC;
begin
  Result := TPC(ObterObjetoPeloNome(Nome, self));
end;

constructor TProjeto.Create(UmaTabelaDeNomes: TStrings; AreaDeProjeto: TForm);
begin
  inherited Create(UmaTabelaDeNomes, nil);
  FProjeto := Self;
  FAreaDeProjeto := AreaDeProjeto;
  FMap := AP(AreaDeProjeto).Map;
  FPCs := TListaDePCs.Create;
  FRes := CriarGerenteDeResultados;
end;

destructor TProjeto.Destroy;
begin
  FSubBacias.Free;
  FPCs.Free;
  inherited Destroy;
end;

function TProjeto.CriarDialogo: THidroDialogo;
begin
  Result := THidroDialogo_Projeto.Create(TN, self);
end;

procedure TProjeto.LerDoArquivo(Ini: TIF; const Secao: String);
//const Origem: TPoint = (X: 0; Y: 0);

   function LerSubBacia(const Nome: String): TSubBacia;
   var Tipo, Secao: String;
       x, y: Integer;
   begin
     Secao := 'Dados ' + Nome;
     Result := TSubBacia(CriarObjeto('Sub-Bacia', nil));
     Result.LerDoArquivo(Ini, Secao);
   end;

   procedure LerTrechoDaguaDoPC(const NomePC: String);
   var Secao, s : String;
       i, ii    : Integer;
       pc1, pc2 : TPC;
       SB       : TSubBacia;
   begin
     pc1 := nil;
     // Faz a leitura dos trechos dágua
     Secao := Ini.ReadString('Dados ' + NomePC, 'TD', '');
     if Secao <> '' then
        begin
        Secao := 'Dados ' + Secao;
        pc1 := TPC(ObterObjetoPeloNome(Ini.ReadString(Secao, 'PM', ''), self));
        pc2 := TPC(ObterObjetoPeloNome(Ini.ReadString(Secao, 'PJ', ''), self));
        pc1.ConectarObjeto(pc2); // E finalmente conecta os PCs
        pc1.TrechoDagua.LerDoArquivo(Ini, Secao);
        s := Ini.ReadString(Secao, 'SubBacia', '');
        if s <> '' then
           begin
           SB := LerSubBacia(s);
           pc1.TrechoDagua.ConectarObjeto(SB);
           end;
        end;
   end;

   function LerPC(const NomeDoPC: String): TPC;
   var Tipo, Secao: String;
       i: Integer;
       s: String;

       function LerDerivacao(const Nome: String; PC: TPC): TDerivacao;
       begin
         Result := TDerivacao(CriarObjeto('Derivacao', nil));
         Result.LerDoArquivo(Ini, 'Dados ' + Nome);
       end;

   begin
     Result := nil;
     if NomeDoPC = '' then Exit;
     Secao := 'Dados ' + NomeDoPC;
     Tipo  := Ini.ReadString(Secao, 'Classe', '');
     if Tipo <> '' then
        begin
        Result := TPC(CriarObjeto(Tipo, nil));
        FPCs.Adicionar(Result);

        Result.LerDoArquivo(Ini, Secao);
        for i := 1 to Ini.ReadInteger(Secao, 'SubBacias', -1) do
          begin
          s := Ini.ReadString(Secao, 'SB' + IntToStr(i), '');
          Result.ConectarObjeto(LerSubBacia(s));
          end;

        Result.LerDoArquivo(Ini, Secao);
        for i := 1 to Ini.ReadInteger(Secao, 'Derivacoes', -1) do
          begin
          s := Ini.ReadString(Secao, 'Der' + IntToStr(i), '');
          Result.ConectarObjeto(LerDerivacao(s, Result));
          end;
        end;
   end;

var i, ii: Integer;
    s: String;
    PC: TPC;
    v: Variant;
    LEI: Tmoec_Layer;
begin
  Inherited;

  with Ini do
    begin
    FNomeArqFundo := ReadString (Secao, 'Fundo',         '');
    FDirSai       := ReadString (Secao, 'Dir. Saida',    '');
    FDirTrab      := ReadString (Secao, 'Dir. Trabalho', '');
    ii            := ReadInteger(Secao, 'Leyers', 0);

    // Lê as informações do Mapa
    for i := ii downto 1 do
      begin
      s := Secao + ' - Layer ' + IntToStr(i);
      v := AddLayer(ReadString(s, 'Name', ''));
      LEI := FMap.Layers.Layer_ExtraInfo[0];

      v.Visible := ReadBool(s, 'Visible', True);

      if v.LayerType = moMapLayer then
         begin
         v.Symbol.Color := ReadInteger(s, 'Symbol.Color', 0);
         v.Symbol.Size  := ReadInteger(s, 'Symbol.Size' , 4);
         v.Symbol.Style := ReadInteger(s, 'Symbol.Style', 0);
         end
      else
         begin
         //...
         end;

      LEI.UseScaleForVisibility := ReadBool(s, 'UseScaleForVisibility', False);
      LEI.MinScale := ReadInteger(s, 'MinScale', 0);
      LEI.MaxScale := ReadInteger(s, 'MaxScale', 0);
      end;

    // Lê as informações dos PCs
    ii := ReadInteger('PCs', 'Quantidade', -1);
    for i := 1 to ii do
       begin
       s := ReadString('PCs', 'PC'+intToStr(i), '');
       PC := LerPC(s);
       end;

    // Lê as informações dos Objetos dos PCs
    for i := 1 to ii do
       begin
       s := Ini.ReadString('PCs', 'PC'+intToStr(i), '');
       LerTrechoDaguaDoPC(s);
       end;
    end; // with
end;

procedure TProjeto.LerDoArquivo(const Nome: String);
var DS: Char;
    Secao: String;
begin
  FNomeArquivo := Nome;
  FIni := TIF.Create(Nome);

  AP(FAreaDeProjeto).Lendo := True;

  DS := DecimalSeparator;
  Secao := 'Dados ' + FIni.ReadString('Projeto', 'Nome', '');
  DecimalSeparator := FIni.ReadString(Secao, 'Separador Decimal', '.')[1];
  try
    LerDoArquivo(FIni, Secao);
    AP(FAreaDeProjeto).LerDoArquivo(FIni);
    SetGlobalStatus(RealizarDiagnostico);
    FPCs.CalcularHierarquia;
    SetModificado(False);
    AP(FAreaDeProjeto).DesenharRede;
  finally
    DecimalSeparator := DS;
    AP(FAreaDeProjeto).Lendo := False;
    FreeAndNil(FIni);
  end;
end;

procedure TProjeto.PegarDadosDoDialogo(d: THidroDialogo);
begin
  inherited PegarDadosDoDialogo(d);
  with (d as THidroDialogo_Projeto) do
    begin
    FDirSai  := edDirSai.Text;
    FDirTrab := edDirPes.Text;
    end;
end;

procedure TProjeto.PorDadosNoDialogo(d: THidroDialogo);
begin
  inherited PorDadosNoDialogo(d);
  with (d as THidroDialogo_Projeto) do
    begin
    edDirSai.Text  := FDirSai;
    edDirPes.Text  := FDirTrab;
    end;
end;

procedure TProjeto.SalvarEmArquivo(Ini: TIF);
var i: Integer;
    s, Secao: String;
    ML: ImoMapLayer;
    IL: ImoImageLayer;
    v: Variant;
    LEI: Tmoec_Layer;
begin
  Inherited SalvarEmArquivo(Ini);

  Secao := ObterSecaoDoIni;
  with Ini do
    begin
    // Salva as informações do projeto
    WriteString (Secao, 'Separador Decimal', DecimalSeparator);
    WriteString (Secao, 'Fundo'            , FNomeArqFundo);
    WriteString (Secao, 'Dir. Saida'       , FDirSai);
    WriteString (Secao, 'Dir. Trabalho'    , FDirTrab);
    WriteInteger(Secao, 'Leyers'           , FMap.Layers.Count);

    // Salva as informações do Mapa
    for i := FMap.Layers.Count-1 downto 0 do
      begin
      v := FMap.Layers[i];
      LEI := FMap.Layers.Layer_ExtraInfo[i];

      s := Secao + ' - Layer ' + IntToStr(i+1);

      WriteInteger(s, 'Type'   , v.LayerType);
      WriteString (s, 'Name'   , v.Name);
      WriteBool   (s, 'Visible', v.Visible);

      if v.LayerType = moMapLayer then
         begin
         ML := ImoMapLayer(FMap.Layers[i]);
         WriteInteger(s, 'Symbol.Color', ML.Symbol.Color);
         WriteInteger(s, 'Symbol.Size' , ML.Symbol.Size);
         WriteInteger(s, 'Symbol.Style', ML.Symbol.Style);
         end
      else
         begin
         IL := ImoImageLayer(FMap.Layers[i]);
         //...
         end;

      WriteBool(s, 'UseScaleForVisibility', LEI.UseScaleForVisibility);
      WriteInteger(s, 'MinScale', LEI.MinScale);
      WriteInteger(s, 'MaxScale', LEI.MaxScale);
      end;

    // Salva as informações de cada Sub-Bacia
    PercorrerSubBacias(SalvarSubBacia);

    // Salva as informações de cada Derivação
    PercorrerDerivacoes(SalvarDerivacao);

    // Nome dos PCs
    WriteInteger('PCs', 'Quantidade', PCs.PCs);
    for i := 0 to PCs.PCs - 1 do
      WriteString('PCs', 'PC' + intToStr(i + 1), PCs[i].Nome);

    // Salva as informações de cada PC e de seu respectivo Trechos-Dagua
    for i := 0 to PCs.PCs - 1 do
      begin
      // Informações dele proprio
      PCs[i].SalvarEmArquivo(Ini);

      // Trecho-Dagua
      if PCs[i].TrechoDagua <> nil then
         PCs[i].TrechoDagua.SalvarEmArquivo(Ini);
      end;

    SetModificado(false);
    end;
end;

procedure TProjeto.SalvarEmArquivo(const Nome: String);
var i: Integer;
    s: String;
begin
  s := ChangeFileExt(Nome, '.bak');
  DeleteFile(s);
  RenameFile(Nome, s);

  FIni := TIF.Create(Nome);
  try
    FIni.WriteString('Projeto', 'Nome', FNome);
    SalvarEmArquivo(FIni);
    AP(FAreaDeProjeto).SalvarEmArquivo(FIni);
    FIni.UpdateFile;
    FNomeArquivo := Nome;
  finally
    FreeAndNil(FIni);
  end;
end;

procedure SalvarSubBacia(Projeto: TProjeto; SB: TSubBacia);
begin
  SB.SalvarEmArquivo(Projeto.FIni);
end;

procedure SalvarDerivacao(Projeto: TProjeto; Der: TDerivacao);
begin
  Der.SalvarEmArquivo(Projeto.FIni);
end;

procedure TProjeto.ValidarDados(var TudoOk: Boolean;
                                DialogoDeErros: TErros_DLG;
                                Completo: Boolean = False);
begin
  Inherited;

  if Dialogo <> nil then Exit;

  if (FDirSai <> '') and not DirectoryExists(FDirSai) then
     begin
     DialogoDeErros.Add(etError,
     Format('Projeto: %s'#13 + 'Diretório de saída inválido: %s', [Nome, FDirSai]));
     TudoOk := False;
     end;

  if (FDirTrab <> '') and not DirectoryExists(FDirTrab) then
     begin
     DialogoDeErros.Add(etError,
     Format('Projeto: %s'#13 + 'Diretório de pesquisa inválido: %s', [Nome, FDirTrab]));
     TudoOk := False;
     end;
end;

// Proc. auxiliar para retorno das subBacias de um projeto
procedure Proc_ObterSubBacias(Projeto: TProjeto; SB: TSubBacia);
begin
  Projeto.FSubBacias.Add(SB);
end;

// Proc. auxiliar para retorno das subBacias de um projeto
procedure Proc_ObterDerivacoes(Projeto: TProjeto; DM: TDerivacao);
begin
  Projeto.FDerivacoes.Add(DM);
end;

// Proc. auxiliar para retorno das Demandas de um projeto
procedure Proc_ObterDemandas(Projeto: TProjeto; DM: TDemanda);
begin
  Projeto.FDemandas.Add(DM);
end;

procedure TProjeto.PercorrerSubBacias(ITSB: TProcPSB);
var i, j: Integer;
begin
  GetMessageManager.SendMessage(UM_RESET_VISIT, [0]);
  for i := 0 to PCs.PCs-1 do
    for j := 0 to PCs[i].SubBacias-1 do
      if not PCs[i].SubBacia[j].Visitado then
         begin
         ITSB(Self, PCs[i].SubBacia[j]);
         PCs[i].SubBacia[j].Visitado := True;
         end;
end;

procedure TProjeto.PercorrerDerivacoes(ITDer: TProcDer);
var i, j: Integer;
begin
  GetMessageManager.SendMessage(UM_RESET_VISIT, [0]);
  for i := 0 to PCs.PCs-1 do
    for j := 0 to PCs[i].Derivacoes-1 do
      if not PCs[i].Derivacao[j].Visitado then
         begin
         ITDer(Self, PCs[i].Derivacao[j]);
         PCs[i].Derivacao[j].Visitado := True;
         end;
end;
{
procedure TProjeto.PercorrerDemandas(ITDM: TProcDM);
var i, j: Integer;
begin
  GetMessageManager.SendMessage(UM_RESET_VISIT, [0]);
  for i := 0 to PCs.PCs-1 do
    for j := 0 to PCs[i].Demandas-1 do
      if not PCs[i].Demanda[j].Visitado then
         begin
         ITDM(Self, PCs[i].Demanda[j]);
         PCs[i].Demanda[j].Visitado := True;
         end;
end;
}
// Retorna as subBacias de um projeto
function TProjeto.ObterSubBacias: TList;
begin
  if FSubBacias = nil then FSubBacias := TList.Create;
  FSubBacias.Clear;
  PercorrerSubBacias(Proc_ObterSubBacias);
  Result := FSubBacias;
end;

// Retorna as Derivações de um Projeto
function TProjeto.ObterDerivacoes: TList;
begin
  if FDerivacoes = nil then FDerivacoes := TList.Create;
  FDerivacoes.Clear;
  PercorrerDerivacoes(Proc_ObterDerivacoes);
  Result := FDerivacoes;
end;
{
// Retorna as Derivações de um Projeto
function TProjeto.ObterDemandas: TList;
begin
  if Demandas = nil then Demandas := TList.Create;
  Demandas.Clear;
  PercorrerDemandas(Proc_ObterDemandas);
  Result := FDemandas;
end;
}
procedure TProjeto.SetFundo(Value: String);
var s, s1: String;
begin
  if Value <> '' then
     begin
     s  := ExtractFilePath(NomeArquivo);
     s1 := ExtractFileName(Value);

     if not PossuiCaminho(Value) then
        Value := s + Value;

     if not FileExists(Value) then
        Value := DirTrab + s1;

     if FileExists(Value) then
        try
          FFundoBmp := TBitmap.Create;
          FFundoBmp.LoadFromFile(Value);

          if CompareText(s, ExtractFilePath(Value)) = 0 then
             FNomeArqFundo := s1
          else
             FNomeArqFundo := Value;

          SetModificado(True);
        except
          FFundoBmp.Free;
          FFundoBmp := nil;
          FNomeArqFundo := '';
        end
     end
  else
     begin
     FFundoBmp.Free;
     FFundoBmp := nil;
     FNomeArqFundo := '';
     SetModificado(True);
     end;
end;

procedure TProjeto.TerminarSimulacao;
begin
  if FSimulador <> nil then
     FSimulador.Terminate;
end;

function TProjeto.GetDirTrab: String;
begin
  if FDirTrab <> '' then
     Result := FDirTrab
  else
     Result := ObterDiretorioDoProjeto;
end;

function TProjeto.GetDirSai: String;
begin
  if FDirSai <> '' then
     Result := FDirSai
  else
     Result := ObterDiretorioDoProjeto;
end;

function TProjeto.ObterDiretorioDoProjeto: String;
begin
  Result := ExtractFilePath(NomeArquivo);
  If LastChar(Result) = '\' then Delete(Result, Length(Result), 1);
end;

procedure TProjeto.SetModificado(const Value: Boolean);
begin
  FModificado := Value;
  if Value then PCs.CalcularHierarquia;
end;

function TProjeto.RealizarDiagnostico(Completo: Boolean): Boolean;
begin
  Result := True;
end;

function TProjeto.CriarGerenteDeResultados: TGerenteDeResultados;
begin
  NaoImplementado('CriarGerenteDeResultados', ClassName);
end;

function TProjeto.CriarObjeto(const ID: String; Pos: ImoPoint): THidroComponente;
begin
  NaoImplementado('CriarObjeto', ClassName);
end;

procedure TProjeto.EventoDeSimulacao;
begin
  NaoImplementado('EventoDeSimulacao', ClassName);
end;

procedure TProjeto.EventoDeTempo;
begin
  NaoImplementado('EventoDeTempo', ClassName);
end;

procedure TProjeto.priv_EventoDeSimulacao(Sender: TObject; const EventID: Integer);
begin
  EventoDeSimulacao
end;

procedure TProjeto.priv_EventoDeTempo(Sender: TObject; out EventID: Integer);
begin
  EventoDeTempo;
end;

function TProjeto.ObterPrefixo: String;
begin
  Result := 'Proj_';
end;

function TProjeto.moPointToPoint(p: ImoPoint): TPoint;
var x, y: Single;
begin
  if p <> nil then
     begin
     FMap.FromMapPoint(p, x, y);
     Result.X := Trunc(x);
     Result.Y := Trunc(y);
     end
  else
     Result := Types.Point(0, 0);
end;

function TProjeto.PointTo_moPoint(p: TPoint): ImoPoint;
begin
  Result := FMap.ToMapPoint(p.x, p.y);
end;

function TProjeto.AddLayer(FileName: String): IDispatch;
var sDataBase : string;
    dc        : ImoDataConnection;
    ML        : ImoMapLayer;
    IL        : ImoImageLayer;
    ext       : String;
begin
  Result := nil;
  sDataBase := ExtractFilePath(FileName);

  if sDataBase = '' then
     sDataBase := getDirTrab
  else
     FileName := ExtractFileName(FileName);

  if DirectoryExists(sDataBase) then
     begin
     ext := LowerCase(ExtractFileExt(FileName));

     // Camadas Vetorias
     if (ext = '.shp') then
        begin
        dc := CoDataConnection.Create;
        dc.Database := sDataBase;
        if dc.Connect then
           begin
           ML := CoMapLayer.Create;
           ML.GeoDataset := dc.FindGeoDataset(FileName);
           if FMap.Layers.Add(ML) then Result := ML;
           end
        else
           raise Exception.Create('O sistema não conseguiu se conectar ao banco de dados:'#13 +
                                  sDataBase + #13 +
                                  'Erro ' + IntToStr(dc.ConnectError));
        end
     else

     // Camadas Raster
     if (ext = '.jpg') or (ext = '.bmp') or (ext = '.gif') or (ext = '.tiff') or
        (ext = '.tff') or (ext = '.tif') or (ext = '.sid') or (ext = '.ntf') or
        (ext = '.sun') or (ext = '.ras') or (ext = '.ovr') or (ext = '.img') then
        begin
        IL := CoImageLayer.Create;
        IL.File_ := sDataBase + '\' + FileName;
        if IL.Valid and FMap.Layers.Add(IL) then Result := IL;
        end

     else
        raise Exception.Create('Formato de Arquivo Desconhecido');

     end
  else
     raise Exception.Create('Diretório de Trabalho Inválido');

  getMessageManager.SendMessage(UM_LM_UPDATE_LAYER, [FMap, FMap]);
end;

{ TDerivacao }

constructor TDerivacao.Create(Pos: ImoPoint; Projeto: TProjeto; UmaTabelaDeNomes: TStrings);
begin
  inherited Create(UmaTabelaDeNomes, Projeto);
  GetMessageManager.RegisterMessage(UM_REPINTAR_OBJETO, self);
  CriarComponenteVisual(Pos);
end;

destructor TDerivacao.Destroy;
begin
  GetMessageManager.UnRegisterMessage(UM_REPINTAR_OBJETO, self);
  inherited Destroy;
end;

function TDerivacao.CriarImagemDoComponente: TdrBaseShape;
begin
  Result := TdrBitmap.Create(nil, 'DERIVACAO_20X20');
  Result.Width := 20;
  Result.Height := 20;
  TdrBitmap(Result).DrawFrame := True;
end;

function TDerivacao.ObterPrefixo: String;
begin
  Result := 'Der_';
end;

function TDerivacao.ReceiveMessage(const MSG: TadvMessage): Boolean;
begin
  if MSG.ID = UM_REPINTAR_OBJETO then
     begin
     SetPos(FPos);
     FImagemDoComponente.Paint;
     end
  else
     inherited ReceiveMessage(MSG);
end;

{ TGerenteDeResultados }

constructor TGerenteDeResultados.Create(Projeto: TProjeto);
begin
  inherited Create;
  FProjeto := Projeto;
end;

{ TReservatorio }

function TReservatorio.CriarImagemDoComponente: TdrBaseShape;
begin
  Result := TdrTriangle.Create(nil);
  Result.Width := 20;
  Result.Height := 20;
end;

function TReservatorio.ObterPrefixo: String;
begin
  Result := 'Res_';
end;

{ TDemanda }

constructor TDemanda.Create(Pos: ImoPoint;
                            Projeto: TProjeto;
                            UmaTabelaDeNomes: TStrings;
                            const Categoria: String);
begin
  inherited Create(UmaTabelaDeNomes, Projeto);

  FSalvarLerBitmap := True;
  FSincronizaDados := True;
  FSalvarLerBitmap := False;
  FHabilitada      := True;
  FCategoria       := Categoria;

  FDadosDeSinc := TStringList.Create;
  FDadosDeSinc.Add('Bitmap');
  FDadosDeSinc.Add('Comentários');

  GetMessageManager.RegisterMessage(UM_NOME_OBJETO_MUDOU, self);
  GetMessageManager.RegisterMessage(UM_REPINTAR_OBJETO, self);
  GetMessageManager.RegisterMessage(UM_OBTER_DEMANDA_PELO_PADRAO, self);
  GetMessageManager.RegisterMessage(UM_DEMANDA_PADRAO_MUDOU, self);
  GetMessageManager.RegisterMessage(UM_HABILITAR_DEMANDAS, self);
  GetMessageManager.RegisterMessage(UM_DESABILITAR_DEMANDAS, self);
  GetMessageManager.RegisterMessage(UM_DESBLOQUEAR_DEMANDAS, self);

  CriarComponenteVisual(Pos);
end;

destructor TDemanda.Destroy;
begin
  FDadosDeSinc.Free;

  GetMessageManager.SendMessage(UM_OBJETO_SE_DESTRUINDO, [Self]);

  GetMessageManager.UnRegisterMessage(UM_DESBLOQUEAR_DEMANDAS, self);
  GetMessageManager.UnRegisterMessage(UM_DESABILITAR_DEMANDAS, self);
  GetMessageManager.UnRegisterMessage(UM_HABILITAR_DEMANDAS, self);
  GetMessageManager.UnRegisterMessage(UM_OBTER_DEMANDA_PELO_PADRAO, self);
  GetMessageManager.UnRegisterMessage(UM_DEMANDA_PADRAO_MUDOU, self);
  GetMessageManager.UnRegisterMessage(UM_REPINTAR_OBJETO, self);
  GetMessageManager.UnRegisterMessage(UM_NOME_OBJETO_MUDOU, self);

  inherited;
end;

procedure TDemanda.Atribuir(Demanda: TDemanda; Criando: Boolean; DadosParaSincronizar: TStrings);
begin
  // Copia os dados do parâmetro aos dados desta instância
  if Demanda <> nil then
     begin
     if Demanda.Eh_Padrao then FCategoria := Demanda.Nome;
     SetModificado(True);
     AtribuirDados(Demanda, Criando, DadosParaSincronizar);
     AtualizarHint;
     end;
end;

function TDemanda.ReceiveMessage(const MSG: TadvMessage): Boolean;
var i: Integer;
    s: String;
    o: TObject;
begin
  if MSG.ID = UM_DESBLOQUEAR_DEMANDAS then
     begin
     if (MSG.ParamAsObject(0) = self.Projeto) then
        FBloqueado := False;
     end else

  if MSG.ID = UM_REPINTAR_OBJETO then
     begin
     SetPos(FPos);
     FImagemDoComponente.Paint;
     end
  else

  if MSG.ID = UM_HABILITAR_DEMANDAS then
     begin
     s := MSG.ParamAsString(0);
     if (CompareText(s, FCategoria) = 0) and (MSG.ParamAsObject(2) = FProjeto) then
        Self.FHabilitada := True;
     end else

  if MSG.ID = UM_DESABILITAR_DEMANDAS then
     begin
     s := MSG.ParamAsString(0);
     if (CompareText(s, FCategoria) = 0) and (MSG.ParamAsObject(2) = FProjeto) then
        Self.FHabilitada := False;
     end else

  if MSG.ID = UM_OBTER_DEMANDA_PELO_PADRAO then
     begin
     s := MSG.ParamAsString(0);
     if (CompareText(s, FCategoria) = 0) and (MSG.ParamAsObject(2) = FProjeto) then
        TStrings(MSG.ParamAsObject(1)).AddObject(FNome, Self);
     end else

  if MSG.ID = UM_DEMANDA_PADRAO_MUDOU then
     begin
     s := MSG.ParamAsString(0);
     if (CompareText(s, FCategoria) = 0) and
        (MSG.ParamAsObject(2) = self.Projeto) and
        FSincronizaDados then
        Atribuir(TDemanda(MSG.ParamAsObject(1)), False, TStrings(MSG.ParamAsObject(3)));
     end
  else

  if MSG.ID = UM_NOME_OBJETO_MUDOU then
     begin
     o := MSG.ParamAsObject(0);
     if (o is TDemanda) and (TDemanda(o).Projeto = self.Projeto) and
        (MSG.ParamAsString(1) = FCategoria) then
        FCategoria := MSG.ParamAsString(2);
     end;

  inherited ReceiveMessage(MSG);
end;

function TDemanda.Sincronizar(const Dado: String; DadosParaSincronizar: TStrings): Boolean;
begin
  Result := (DadosParaSincronizar.IndexOf(Dado) > -1)
end;

function TDemanda.getVisivel: Boolean;
begin
  Result := FImagemDoComponente.Visible;
end;

procedure TDemanda.setVisivel(const Value: Boolean);
begin
  FImagemDoComponente.Visible := Value;
end;

procedure TDemanda.BitmapChange(Sender: TObject);
begin
  SetModificado(True);
  BitmapMudou := True;
end;

function TDemanda.CriarDialogo: THidroDialogo;
begin
  Result := THidroDialogo_Demanda.Create(TabNomes, self);
end;

function TDemanda.CriarImagemDoComponente: TdrBaseShape;
begin
  Result := TdrBitmap.Create(nil, 'DEMANDA_IRRIGACAO_20X20');
  Result.Width := 16;
  Result.Height := 16;
  with TdrBitmap(Result).Bitmap do
    begin
    Transparent := True;
    TransparentMode := tmAuto;
    OnChange := BitmapChange;
    end;
end;

function TDemanda.GetBitmap: TBitmap;
begin
  Result := TdrBitmap(ImagemDoComponente).Bitmap;
end;

function TDemanda.GetBitmapName: String;
begin
  Result := 'Bitmap ' + FNome;
end;

function TDemanda.ObterPrefixo: String;
begin
  if Eh_Padrao then
     Result := 'DemandaPadrao_'
  else
     Result := 'Demanda';
end;

procedure TDemanda.PegarDadosDoDialogo(d: THidroDialogo);
begin
  inherited;
  with (d as THidroDialogo_DemandaPadrao) do
    begin
{
    //if FImagemMudou then
       Bitmap.Assign(Image.Picture);

    if FLigada <> boolean(cbStatus.ItemIndex) then
       begin
       FLigada := boolean(cbStatus.ItemIndex);
       if FLigada then
          GetMessageManager.SendMessage(UM_HABILITAR_DEMANDAS, [@FNome, self, FProjeto])
       else
          GetMessageManager.SendMessage(UM_DESABILITAR_DEMANDAS, [@FNome, self, FProjeto]);
       end;

    FSincronizaDados := DLG_OPC.cbSincronizar.Checked;

    GetMessageManager.SendMessage(UM_DEMANDA_PADRAO_MUDOU,
        [@FNome, self, FProjeto, DLG_SD.DstList.Items]);
}
    end;
end;

procedure TDemanda.PorDadosNoDialogo(d: THidroDialogo);
begin
  inherited;
  with (d as THidroDialogo_DemandaPadrao) do
    begin
{
    Image.Picture.Assign(Bitmap);
    cbStatus.ItemIndex := integer(FLigada);
    cbSincronizar.Checked := FSincronizaDados;
    cbStatus.Enabled := FHabilitada;
    edPadrao.Text := FPadrao;
}
    end;
end;

procedure TDemanda.LerDoArquivo(Ini: TIF; const Secao: String);
var DM: TDemanda;
begin
  inherited;
  with Ini do
    begin
{
    FLigada := ReadBool(ObterSecao, 'Status', False);
    FSincronizaDados := ReadBool(ObterSecao, 'Sincronizar', true);
    if FSalvarLerBitmap then
       Bitmap := LerBitmapDoArquivo(Ini, 'Bitmap ' + FNome);
}
    FCategoria := ReadString (ObterSecaoDoIni, 'FCategoria',  '');
    DM := FProjeto.DemandasPadroes.ObterPeloNome(FCategoria);
    if DM <> nil then
       begin
       Bitmap := TBitmap.Create;
       Bitmap.Assign(DM.Bitmap);
       end;
    end;
end;

procedure TDemanda.SalvarEmArquivo(Ini: TIF);
begin
  inherited;
  with Ini do
    begin
    //WriteBool(ObterSecaoDoIni, 'Status', FLigada);
    WriteBool(ObterSecaoDoIni, 'Sincronizar', FSincronizaDados);
    WriteString(ObterSecaoDoIni, 'Categoria', FCategoria);
    if FSalvarLerBitmap then
       SalvarBitmapEmArquivo(Bitmap, Ini, 'Bitmap ' + FNome);
    end;
end;

procedure TDemanda.SetBitmap(B: TBitmap);
begin
  if B <> Nil then
     TdrBitmap(ImagemDoComponente).Bitmap.Assign(B);
end;

procedure TDemanda.AtribuirDados(Demanda: TDemanda;
                                 Criando: Boolean;
                                 DadosParaSincronizar: TStrings);
begin
  if Sincronizar('Comentários', DadosParaSincronizar) or Criando then
     Comentarios.Text := Demanda.Comentarios.Text;

  if Sincronizar('Bitmap', DadosParaSincronizar) or Criando then
     Bitmap := Demanda.Bitmap;
end;

function TDemanda.getEh_Padrao: Boolean;
begin
  Result := (Categoria = '');
end;

{ TListaDeDemandasPadroes }

constructor TListaDeDemandasPadroes.Create(Projeto: TProjeto; TabelaDeNomes: TStrings);
begin
  Inherited Create;
  FList := TStringList.Create;
end;

destructor TListaDeDemandasPadroes.Destroy;
begin
  FEM := nil;
  Limpar;
  FList.Free;
  inherited;
end;

procedure TListaDeDemandasPadroes.Adicionar(DM: TDemanda);
begin
  FProjeto.FModificado := True;
  FList.AddObject(DM.Nome, DM);
  if assigned(FEM) then FEM(Self);
end;

procedure TListaDeDemandasPadroes.Editar(DM: TDemanda);
begin
  if FList.IndexOfObject(DM) > -1 then
     begin
     DM.MostrarDialogo;
     if Assigned(FEM) then FEM(Self);
     end;
end;

function TListaDeDemandasPadroes.getBitmap(i: Integer): TBitmap;
begin
  Result := TDemanda(FList.Objects[i]).Bitmap;
end;

function TListaDeDemandasPadroes.getPadrao(i: Integer): TDemanda;
begin
  Result := TDemanda(FList.Objects[i]);
end;

function TListaDeDemandasPadroes.GetPadroes: Integer;
begin
  Result := FList.Count;
end;

procedure TListaDeDemandasPadroes.LerDoArquivo(Arquivo: TIF);
var Demanda  : TDemanda;
    BM       : TBitmap;
    nDEMs    : Integer;
    i        : Integer;
    Secao    : String;
    auxEM    : TEvento_Mudanca;
begin
  auxEM := FEM;
  FEM := nil;
  Limpar;
  nDEMs := Arquivo.ReadInteger('Demandas Padroes', 'Quantidade', 0);
  for i := 0 to nDEMs-1 do
    begin
    Secao := Arquivo.ReadString('Demanda Padroes', intToStr(i+1), '');
    if Secao <> '' then
       begin
       Secao := 'Dados ' + Secao;
       Demanda := FProjeto.CriarObjeto('TDemanda', nil) as TDemanda;
       Demanda.LerDoArquivo(Arquivo, Secao);
       Secao := Arquivo.ReadString(Secao, 'Bitmap', '');
       Adicionar(Demanda);
       end;
    end;
  FEM := auxEM;
end;

procedure TListaDeDemandasPadroes.Limpar;
begin
  While FList.Count > 0 do RemoverObjeto(FList.Count-1);
end;

function TListaDeDemandasPadroes.ObterPeloNome(const Padrao: String): TDemanda;
var i: Integer;
begin
  i := FList.IndexOf(Padrao);
  if i > -1 then
     Result := TDemanda(FList.Objects[i])
  else
     Result := nil;
end;

function TListaDeDemandasPadroes.Remover(DM: TDemanda): Boolean;
var i: Integer;
    Demandas: TStrings;
begin
  Result := False;
  Demandas := TStringList.Create;
  try
    i := FList.IndexOfObject(DM);
    if i > -1 then
       begin
       ObterDemandasPeloPadrao(DM.Nome, Demandas, FProjeto);
       if Demandas.Count = 0 then
          begin
          AP(FProjeto.AreaDeProjeto).ObjetoSelecionado := nil;
          RemoverObjeto(i);
          Result := True;
          end
       else
          MessageDLG(cMsgInfo02, mtInformation, [mbOk], 0);
       end;
  finally
    Demandas.Free;
  end;
end;

procedure TListaDeDemandasPadroes.RemoverObjeto(indice: Integer);
begin
  FProjeto.FModificado := True;
  TObject(FList.Objects[indice]).Free;
  FList.Delete(indice);
  if Assigned(FEM) then FEM(Self);
end;

procedure TListaDeDemandasPadroes.SalvarEmArquivo(Arquivo: TIF);
var i: Integer;
begin
  Arquivo.WriteInteger('Demandas Padroes', 'Quantidade', Padroes);
  for i := 0 to Padroes-1 do
    begin
    Arquivo.WriteString('Demandas Padroes', intToStr(i+1), Padrao[i].Nome);
    Padrao[i].SalvarEmArquivo(Arquivo);
    end;
end;

end.


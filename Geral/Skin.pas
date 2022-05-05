unit Skin;

interface
uses Forms,
     Controls,
     SysUtils,
     SysUtilsEx;

type
  TDialog = TForm;
  TDialogClass = class of TDialog;

  // Representa a pele (skin) de uma instancia, isto é, o diálogo deste objeto
  TSkin = class(T_NRC_InterfacedObject)
  private
    FText: string;
    FDescription: string;
    FDialog: TDialog;
  protected
    // Indica se houve erro apos a execucao de "Execute()"
    FOk: boolean;

    // Utilize este método para criação do Diálogo
    function internalCreateDialog(DialogClass: TDialogClass): TDialog; virtual;

    // Sobreescreva este método para obter os dados do diálogo
    procedure getDataFromDialog(); virtual;

    // Sobreescreva este método para inicializar o diálogo
    procedure initDialog(); virtual;
  public
    // Cria uma instancia, Text sera usado como titulo padrao no dialogo
    constructor Create(const Text: string);

    // Deverá ser Sobreescrito para definição do diálogo e os descendentes NAO deverao
    // chamar o ancestral
    function createDialog(): TDialog; virtual;

    // Ação específica da classe
    // O ancestral devera ser chamado
    function Execute(): integer; virtual;

    // Mostra o dialogo associado a esta instancia retornando o codigo de fechamento
    function ShowDialog(): integer;

    // Texto que aparecerá no nó
    property Text : string read FText write FText;

    // Descrição da skin
    property Description : string read FDescription write FDescription;

    // Diálogo dos dados
    property Dialog : TDialog read FDialog;

    // Informa se após a execucao de "Execute()" nao houve erro
    property Ok : boolean read FOk;
  end;

implementation

{ TSkin }

constructor TSkin.Create(const Text: string);
begin
  inherited Create();
  FText := Text
end;

function TSkin.createDialog(): TDialog;
begin
  VirtualError(self, 'createDialog()');
end;

function TSkin.Execute(): integer;
begin
  FOk := true;
  result := -1;
end;

procedure TSkin.getDataFromDialog();
begin
  // Nada
end;

procedure TSkin.initDialog();
begin
  FDialog.Caption := ' ' + FText;
end;

function TSkin.internalCreateDialog(DialogClass: TDialogClass): TDialog;
begin
  Result := DialogClass.Create(nil);

  // FDialog será destruído pelo chamador
  FDialog := Result;

  // Faz as inicializações dos campos
  initDialog();
end;

function TSkin.ShowDialog(): integer;
begin
  FDialog := createDialog();
  initDialog();
  result := FDialog.ShowModal();
  if result = mrOK then getDataFromDialog();
  FreeAndNil(FDialog);
end;

end.

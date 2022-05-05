unit Skin;

interface
uses Forms,
     Controls,
     SysUtils,
     SysUtilsEx;

type
  TDialog = TForm;
  TDialogClass = class of TDialog;

  // Representa a pele (skin) de uma instancia, isto �, o di�logo deste objeto
  TSkin = class(T_NRC_InterfacedObject)
  private
    FText: string;
    FDescription: string;
    FDialog: TDialog;
  protected
    // Indica se houve erro apos a execucao de "Execute()"
    FOk: boolean;

    // Utilize este m�todo para cria��o do Di�logo
    function internalCreateDialog(DialogClass: TDialogClass): TDialog; virtual;

    // Sobreescreva este m�todo para obter os dados do di�logo
    procedure getDataFromDialog(); virtual;

    // Sobreescreva este m�todo para inicializar o di�logo
    procedure initDialog(); virtual;
  public
    // Cria uma instancia, Text sera usado como titulo padrao no dialogo
    constructor Create(const Text: string);

    // Dever� ser Sobreescrito para defini��o do di�logo e os descendentes NAO deverao
    // chamar o ancestral
    function createDialog(): TDialog; virtual;

    // A��o espec�fica da classe
    // O ancestral devera ser chamado
    function Execute(): integer; virtual;

    // Mostra o dialogo associado a esta instancia retornando o codigo de fechamento
    function ShowDialog(): integer;

    // Texto que aparecer� no n�
    property Text : string read FText write FText;

    // Descri��o da skin
    property Description : string read FDescription write FDescription;

    // Di�logo dos dados
    property Dialog : TDialog read FDialog;

    // Informa se ap�s a execucao de "Execute()" nao houve erro
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

  // FDialog ser� destru�do pelo chamador
  FDialog := Result;

  // Faz as inicializa��es dos campos
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

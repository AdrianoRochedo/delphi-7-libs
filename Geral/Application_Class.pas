unit Application_Class;

interface
uses Dialogs,
     SysUtils,
     Classes,
     ActnList,
     ComCtrls,
     IniFiles,
     FolderUtils,
     Forms;

type

  // Classe Gerenciadora da aplicação
  TApplication = class(TDataModule)
  private
    FFileTemps: TStrings;
    FClasses: TStrings;

    // Caminhos
    FLastDir: String;
    FAppDataDir: string;
    FApplicationDir: String;
    FApplicationName: String;
    FApplicationVersion: String;
    FActiveObject: TObject;

    function getAppTitle: String;
    procedure setAppTitle(const Value: String);
    procedure notImplemented(const MethodName: string);
    function getMainForm(): TForm;

    // Executa a aplicação
    procedure Run(); 
  protected
    destructor Destroy(); override;
    procedure CreateMainForm(); virtual;
    procedure BeforeRun(); virtual;
    procedure PostRun(); virtual;

    // Inicializações e finalizações de variáveis globais
    procedure ReadGlobalsOptions(ini: TIniFile); virtual;
    procedure SaveGlobalsOptions(ini: TIniFile); virtual;
  public
    constructor Create(const Title, Version: string);

    // Abre um arquivo da aplicacao
    procedure OpenFile(const Filename: string); virtual;

    // Istancia um formulario
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);

    // Arranja as janelas filhas em uma aplicacao MDI
    procedure ArrangeChildrens();

    // Da tempo a outros processos
    procedure ProcessMessages();

    // Notifica a criação de um arquivo temporário.
    // É utilizado no controle dos arquivos gerados
    procedure AddTempFile(const Name: String);

    // Retorna um nome de arquivo temporário
    // O arquivo é criado no diretório temporário do Windows
    function NewTempFile(const Prefixo, Ext: string): string;

    // Obtem o caminho relativo a aplicacao
    function getRelativePath(const s: string): string; virtual;

    // Expande o caminho relativo a aplicacao
    function ExpandFileName(const s: string): string; virtual;

    // Registra uma classe
    procedure RegisterClass(aClass: TClass);

    // Retorna uma classe registrada pelo nome
    function getClass(const Name: string): TClass;

    // Mostra um erro em uma janela
    procedure ShowError(const msg: string);

    // Armazena o último diretório acessado
    property LastDir : String read FLastDir write FLastDir;

    // Armazena o caminho da aplicação
    property AppDir : String read FApplicationDir;

    // Retorna o caminho para os dados da aplicacao
    property AppDataDir : string read FAppDataDir;

    // Armazena o nome da aplicação sem o caminho e a extensão
    property AppName : String read FApplicationName;

    // Armazena o nome da aplicação sem o caminho e a extensão
    property Title : String read getAppTitle write setAppTitle;

    // Versão da aplicação
    property Version : String read FApplicationVersion write FApplicationVersion;

    // Retorna a janela principal desta aplicacao
    property MainForm : TForm read getMainForm;

    // Utilizado para relatório de erros
    // Ex. Indicar qual objeto gerou erro no salvamento da rede.
    property ActiveObject : TObject read FActiveObject write FActiveObject;
  end;

  TSystem = class
    // Associa Instance e roda a aplicacao
    class procedure Run(Applic: TApplication); overload;
    class function getAppInstance(): TApplication;
  end;

  function Applic(): TApplication;

implementation
uses SysUtilsEx,
     FileUtils;

{$R *.dfm}

function Applic(): TApplication;
begin
  result := TApplication( TSystem.getAppInstance() );
end;

var
  // Instancia global privada da aplicacao
  Instance: TApplication = nil;

{ TApplication }

constructor TApplication.Create(const Title, Version: string);
begin
  Forms.Application.Initialize();

  inherited Create(nil);

  FClasses := TStringList.Create();
  FAppDataDir := FolderUtils.GetAppDataFolder();
  FApplicationDir := ExtractFilePath(Forms.Application.ExeName);
  FApplicationName := ChangeFileExt(ExtractFileName(Forms.Application.ExeName), '');
  FApplicationVersion := Version;
  SetAppTitle(Title);
  SysUtilsEx.SetApplicationDir(FApplicationDir);

  if FAppDataDir = '' then
     FAppDataDir := FApplicationDir
  else
     begin
     FAppDataDir := FAppDataDir + FApplicationName + '\';
     if not SysUtils.ForceDirectories(FAppDataDir) then
        FAppDataDir := FApplicationDir;
     end;

  CreateMainForm();

  if (MainForm <> nil) and (MainForm.Caption = '') then
     MainForm.Caption := ' ' + Title + ' ' + Version;
end;

destructor TApplication.Destroy();
var i: Integer;
begin
  FClasses.Free();

  // Remove os arquivos temporários criados durante a sessão do programa
  if FFileTemps <> nil then
     for i := 0 to FFileTemps.Count-1 do
       DeleteFile(FFileTemps[i]);

  inherited Destroy();
end;

procedure TApplication.ReadGlobalsOptions(ini: TIniFile);
begin
  FLastDir := ini.ReadString('Paths', 'LastDir', '');
end;

procedure TApplication.SaveGlobalsOptions(ini: TIniFile);
begin
  ini.WriteString('Paths', 'LastDir', FLastDir);
end;

procedure TApplication.AddTempFile(const Name: String);
begin
  if FFileTemps = nil then FFileTemps := TStringList.Create;
  FFileTemps.Add(Name);
end;

procedure TApplication.Run();
begin
  BeforeRun();
  Forms.Application.Run();
  PostRun();
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
begin
  Forms.Application.CreateForm(InstanceClass, Reference);
end;

function TApplication.getAppTitle: String;
begin
  Result := Forms.Application.Title;
end;

procedure TApplication.setAppTitle(const Value: String);
begin
  Forms.Application.Title := Value;
end;

procedure TApplication.BeforeRun();
var ini: TIniFile;
begin
  ini := TIniFile.Create(FAppDataDir + FApplicationName + '.ini');
  ReadGlobalsOptions(ini);
  ini.Free();
end;

procedure TApplication.PostRun();
var ini: TIniFile;
begin
  ini := TIniFile.Create(FAppDataDir + FApplicationName + '.ini');
  SaveGlobalsOptions(ini);
  ini.Free();
end;

function TApplication.getMainForm(): TForm;
begin
  result := Forms.Application.MainForm;
end;

procedure TApplication.ProcessMessages;
begin
  Forms.Application.ProcessMessages();
end;

function TApplication.NewTempFile(const Prefixo, Ext: string): string;
begin
  result := FileUtils.GetTempFile('', Prefixo, Ext);
  AddTempFile(result);
end;

procedure TApplication.CreateMainForm();
begin
  Dialogs.MessageDLG('Método: TApplication.CreateMainForm() não definido',
                      mtInformation, [mbOk], 0);
end;

procedure TApplication.OpenFile(const Filename: string);
begin
  notImplemented('OpenFile');
end;

procedure TApplication.notImplemented(const MethodName: string);
begin
  raise Exception.Create(
    '"' + MethodName + '" not implemented in class "' + ClassName + '"');
end;

function TApplication.getClass(const Name: string): TClass;
var i: Integer;
begin
  i := FClasses.IndexOf(Name);
  if i <> -1 then
     result := TClass(FClasses.Objects[i])
  else
     result := nil;
end;

procedure TApplication.RegisterClass(aClass: TClass);
begin
  FClasses.AddObject(aClass.ClassName, pointer(aClass));
end;

procedure TApplication.ArrangeChildrens();
begin
  MainForm.Cascade();
end;

procedure TApplication.ShowError(const msg: string);
var s: string;
begin
(*
  if FActiveObject is TComponente then
     s := Format('Objeto: %s'#13 +
                 'Erro: %s',
                 [
                   TComponente(FActiveObject).Nome,
                   msg
                 ])
  else
     s := 'Erro: ' + msg;

  Dialogs.ShowMessage(s);
*)
{TODO 1 -cProgramacao: ShowError}
end;

function TApplication.getRelativePath(const s: string): string;
begin
  result := SysUtils.ExtractRelativePath(self.AppDir, s);
end;

function TApplication.ExpandFileName(const s: string): string;
begin
  SysUtils.SetCurrentDir(self.AppDir);
  result := SysUtils.ExpandFileName(s);
end;

{ TSystem }

class function TSystem.getAppInstance(): TApplication;
begin
  if Instance <> nil then
     result := Instance
  else
     raise Exception.Create('Instância da Applicação não Inicializada');
end;

class procedure TSystem.Run(Applic: TApplication);
begin
  Instance := Applic;
  Instance.Run();
  Instance.Free();
  Instance := nil;
end;

end.

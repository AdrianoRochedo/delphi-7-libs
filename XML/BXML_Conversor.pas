unit BXML_Conversor;

{ Autor ........................ Adriano Rochedo Conceição

  Padrão do arquivo XML .................................................................
    - Os arquivos XML deverão possuir blocos lógicos (filhos do nó raiz) identificados
      por um "NameSpace" seguido da palavra "block".
      Ex:  <Estilo:block>
             ... Qualquer coisa ...
           </Estilo:block>

    - Os NameSpaces deverão estar declarados no nó raiz
      Ex:  <?xml ...>
           <blocks xmlns:Control="xxx"
                   xmlns:Estilo="xxx">

             <Control:block>
               ... Comandos de controle para manipulação dos arquivos destino ...
               ... Olhar documentação: Control.bxml ...
             </Control:block>

             <Estilo:block>
               ... Qualquer coisa ...
             </Estilo:block>

           </blocks>

    - Os NameSpaces serão utilizados para acesso acesso aos tradutores.
      Ex: NameSpace = Estilo
          O software procurará um estilo "Estilo.XSL" ou um plugin "Estilo.DLL" no
          diretório de plugins para utilizar como manipulador do bloco.

  Versão ....................... 1.0 (12/02/2002)
    - Protótipo funcional com capacidade de utilização de plugins (DLLs) e Estilos (XSLs)
      utilizando a biblioteca MSXML4 da Microsoft.
}

interface
uses Classes, xmldom, XMLIntf, XMLDoc, MSXML4, SysUtils,
     BXML_DLL_Processor, Variants;

type
  // Classe base para os conversores (tradutores) dos blocos XML lógicos
  TBXML_Conversor = class
  private
    FName: String;
    FPluginName: String;
    FPluginType: String;
  public
    constructor Create(const Name, PluginName: String);

    property Name       : String read FName;
    property PluginName : String read FPluginName;
    property PluginType : String read FPluginType;
  end;

  // Tradutor baseado em arquivos XSL
  TBXML_XSL_Conversor = class(TBXML_Conversor)
  private
    FXSL: IXMLDOMDocument;
  public
    constructor Create(const Name, PluginName: String);

    // não é necessário liberar Interfaces. O compilador o faz automaticamente
    property XSL : IXMLDOMDocument read FXSL;
  end;

  TProcNewDLLProcessor = function: TBase_BXML_DLL_Processor; stdcall;

  // Tradutor baseado em DLL (plugin)
  TBXML_DLL_Conversor = class(TBXML_Conversor)
  private
    FDLL: THandle;
    FProcessor: TBase_BXML_DLL_Processor;
    NewDLLProcessor: TProcNewDLLProcessor;
    function getBuffer: TStrings;
    function getXMLData: WideString;
    procedure setBuffer(const Value: TStrings);
    procedure setXMLData(Value: WideString);
  public
    constructor Create(const Name, PluginName: String);
    destructor Destroy; override;

    property Buffer    : TStrings                 read getBuffer write setBuffer;
    property XML       : WideString               read getXMLData write setXMLData;
    property Processor : TBase_BXML_DLL_Processor read FProcessor;
  end;

  // Gerenciador dos tradutores
  TBXML_Conversors = class
  private
    FList: TStringList;
  public
    constructor Create(const PluginsPath: String);
    destructor Destroy; override;
    procedure Add(Conversor: TBXML_Conversor);
    function ByName(const Name: String): TBXML_Conversor;
  end;

  // Mecanismo de tradução de arquivos XML multi-blocos
  TBXML_MultiConversor = class
  private
    FXMLFile: WideString;
    FPluginsPath: WideString;
    FOutputFile: WideString;
  protected
    // Realiza a análise do doc. XML verificando os "NameSpaces" utilizados.
    // Para cada "NameSpace" encontrado é feito o carregamento do plugin correspondente.
    // Os plugins são gerenciados pelo parâmetro <Conversors>
    procedure DoAnalize(XMLDoc: IXMLDOMDocument; out Conversors: TBXML_Conversors);

    // Realiza as transformações aplicando os plugins aos respectivos blocos de código
    // Um único documento é gerado e armazenado em <HTMBuffer>
    procedure DoTransform(XMLDoc: IXMLDOMDocument; Conversors: TBXML_Conversors);
  public
    constructor Create(const PluginsPath, XMLFile: String);

    // Realiza a transformação
    function Transform: Boolean;

    property PluginsPath : WideString read FPluginsPath write FPluginsPath;
    property XMLFile     : WideString read FXMLFile write FXMLFile;
    property OutputFile  : WideString read FOutputFile;
  end;

implementation
uses Windows, SysUtilsEx, Forms, XML_Utils;

{ TBXML_Conversors }

constructor TBXML_Conversors.Create(const PluginsPath: String);
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
end;

destructor TBXML_Conversors.Destroy;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    FList.Objects[i].Free;
  FList.Free;
  inherited;
end;

procedure TBXML_Conversors.Add(Conversor: TBXML_Conversor);
begin
  FList.AddObject(Conversor.Name, Conversor);
end;

function TBXML_Conversors.ByName(const Name: String): TBXML_Conversor;
var i: Integer;
begin
  i := FList.IndexOf(Name);
  if i > -1 then
     Result := TBXML_Conversor(FList.Objects[i])
  else
     Result := nil;
end;

{ TBXML_MultiConversor }

constructor TBXML_MultiConversor.Create(const PluginsPath, XMLFile: String);
begin
  inherited Create;
  FPluginsPath := PluginsPath;
  if LastChar(FPluginsPath) <> '\' then FPluginsPath := FPluginsPath + '\';
  FXMLFile := XMLFile;
end;

procedure TBXML_MultiConversor.DoAnalize(XMLDoc: IXMLDOMDocument; out Conversors: TBXML_Conversors);
var i: Integer;
    ns, PluginName: String;
    No: IXMLDomNode;
    C: TBXML_Conversor;
begin
  Conversors := TBXML_Conversors.Create(FPluginsPath);
  for i := 0 to XMLDoc.DocumentElement.Attributes.Length-1 do
    begin
    No := XMLDoc.DocumentElement.Attributes.Item[i];
    if CompareText(No.Prefix, 'xmlns') = 0 then
       begin
       C := nil;
       ns := SubString(No.XML, ':', '=');

       if Conversors.ByName(ns) = nil then
          begin
          // Verifica se existe um conversor tipo XSL
          PluginName := FPluginsPath + ns + '.xsl';
          if FileExists(PluginName) then
             C := TBXML_XSL_Conversor.Create(ns, PluginName);

          // Verifica se existe um conversor tipo DLL
          PluginName := FPluginsPath + ns + '.dll';
          if FileExists(PluginName) then
             begin
             C := TBXML_DLL_Conversor.Create(ns, PluginName);
             TBXML_DLL_Conversor(C).Processor.SetInputFile(FXMLFile);
             end;

          if C <> nil then Conversors.Add(C);
          end;
       end;
    end;
end;

procedure TBXML_MultiConversor.DoTransform(XMLDoc: IXMLDOMDocument; Conversors: TBXML_Conversors);
var XMLBlock : IXMLDOMDocument; // não é necessário liberar Interfaces. O compilador o faz automaticamente
    no       : IXMLDomNode;
    Buffer   : TStrings;
    i        : Integer;
    ic       : Boolean; // isControl;
    C        : TBXML_Conversor;
    D        : TBXML_DLL_Conversor;
begin
  Buffer := nil;
  XMLBlock := CreateDOMDocument;
  for i := 0 to XMLDoc.DocumentElement.ChildNodes.Length-1 do
    begin
    no := XMLDoc.DocumentElement.ChildNodes.Item[i];
    C := Conversors.ByName(no.Prefix);
    if C <> nil then
       begin
       XMLBlock.LoadXML(no.XML);

       if C.PluginType = 'xsl' then
          with TBXML_XSL_Conversor(C) do
            begin
            if Buffer = nil then
               Raise Exception.Create('Buffer não definido. Bloco: ' + no.Prefix);
            Buffer.Add(XMLBlock.transformNode(XSL.documentElement));
            end
       else
       if C.PluginType = 'dll' then
          begin
          D := TBXML_DLL_Conversor(C);

          ic := (CompareText(C.Name, 'control') = 0);

          // Informa para os outros plugins o nome do arquivo destino
          if not ic then D.Processor.SetOutputFile(FOutputFile);

          // não importa se igual a nil
          D.Buffer := Buffer;

          // O processamento ocorre neste momento
          D.XML := XMLBlock.documentElement.XML;

          // o buffer retornado pode ser diferente do original
          Buffer := D.Buffer;

          // Obtem o nome do primeiro arquivo destino obtido pelo bloco de controle
          if ic then
             FOutputFile := D.Processor.getOutputFile;
          end
       end // C <> nil
    else
       // Plugin não encontrado !
    end;
end;

function TBXML_MultiConversor.Transform: Boolean;
var XMLDoc: IXMLDOMDocument;
    Conversors: TBXML_Conversors;
begin
  Result := False;
  XMLDoc := XML_Utils.OpenXMLDocument(FXMLFile);
  try
    DoAnalize(XMLDoc, {out} Conversors);
    DoTransform(XMLDoc, Conversors);
    Result := True;
  finally
    // não é necessário liberar Interfaces. O compilador o faz automaticamente
    Conversors.Free;
  end;
end;

{ TBXML_Conversor }

constructor TBXML_Conversor.Create(const Name, PluginName: String);
begin
  inherited Create;
  FName := Name;
  FPluginName := PluginName;
  FPluginType := ExtractFileExt(FPluginName);
  Delete(FPluginType, 1, 1); // Apaga o "." da extensão
end;

{ TBXML_XSL_Conversor }

constructor TBXML_XSL_Conversor.Create(const Name, PluginName: String);
begin
  inherited;
  FXSL := CreateDOMDocument;
  FXSL.Load(PluginName);
end;

{ TBXML_DLL_Conversor }

constructor TBXML_DLL_Conversor.Create(const Name, PluginName: String);
begin
  inherited;
  FDLL := LoadLibrary(pChar(PluginName));
  @NewDLLProcessor := GetProcAddress(FDLL, 'NewProcessor');
  FProcessor := NewDLLProcessor; // Cria uma intancia do Processador
end;

destructor TBXML_DLL_Conversor.Destroy;
begin
  //FProcessor.Free;
  FreeLibrary(FDLL);
  inherited;
end;

function TBXML_DLL_Conversor.getBuffer: TStrings;
begin
  Result := FProcessor.GetBuffer;
end;

function TBXML_DLL_Conversor.getXMLData: WideString;
begin
  Result := FProcessor.GetXMLDoc;
end;

procedure TBXML_DLL_Conversor.setBuffer(const Value: TStrings);
begin
  FProcessor.SetBuffer(Value);
end;

procedure TBXML_DLL_Conversor.setXMLData(Value: WideString);
begin
  FProcessor.SetXMLDoc(Value);
end;

end.

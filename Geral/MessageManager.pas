{Gerenciador de Mensagens entre Objetos}
unit MessageManager;

{
  AUTOR : Adriano Rochedo Conceição
  DATA  : 19/04/2002
}

{ AJUDA: Como utilizar este mecanismo ?

    RECEBENDO MENSAGENS:
    - Faça a classe que deseja receber mensagens:
      - Implementar a interface "IMessageReceptor"
      - Registre as mensagens que deseja receber na inicialização da instância utilizando
        "GetMessageManager.RegisterMessage(ID)" e de-registre na finalização utilizando
        "GetMessageManager.UnregisterMessage(ID)"

    ENVIANDO MENSAGENS:
    - Para enviar uma mensagem utilize "GetMessageManager.SendMessage(ID, [... parâmetros ...])"
      - Deverão ser enviados os endereços dos parâmetros a menos que o parâmetro seja um objeto,
        pois um objeto ja é um endereço.
        Ex: var s: String; r: Real; o: TForm;
            SendMessage(ID, [@s, o, @r]);


    REGISTRANDO UM IDENTIFICADOR DE MENSAGEM:
    - Declare uma variável global inteira. Ex: MyMessageID: Integer
    - Registre um identificador utilizando uma "string"
      Ex: MyMessageID := GetMessageManager.RegisterMessageID('MyMessageID String');
      O melhor lugar para este registro ser feito é na seção "initialization" de uma "unit".
}

interface
uses Classes,
     Messages;

const
  ErrorMessage1 = 'Mensagem não registrada';
  ErrorMessage2 = 'Mensagem já registrada';

Type
  pRecObjeto                  = ^TRecPonteiroParaUmObjeto;  // <<<<< verificar utilidade
  TRecPonteiroParaUmObjeto    = record
                                  Obj: TObject;
                                end;

  TadvMessage = class
  private
    FID: Integer;                    // ID da mensagem
    FParams: Array of Pointer;       // parâmetros (opcional)
    function getParamCount: Integer;     
  public
    function ParamAsString(ParamIndex: Integer): String;
    function ParamAsObject(ParamIndex: Integer): TObject;
    function ParamAsPointer(ParamIndex: Integer): Pointer;
    function ParamAsInteger(ParamIndex: Integer): Integer;
    function ParamAsBoolean(ParamIndex: Integer): Boolean;
    function ParamAsReal(ParamIndex: Integer): Real;
    property ID: Integer read FID;
    property ParamCount: Integer read getParamCount;
  end;

  IMessageReceptor = interface
    function ReceiveMessage(const MSG: TadvMessage): Boolean;
  end;

  // Não deverá ser instanciada.
  // Utilise a função GetMessageManager para obter o gerenciador padrão
  TMessageManager = class
  private
    FList : TList;
    FMessages: TStrings; // Variável privada que guarda as mensagens registradas
  public
    Constructor Create;
    destructor Destroy; override;

    // Possibilita o registro de identificadores "string" para IDs de mensagens.
    function RegisterMessageID(const MessageName: String): Integer;
    function GetMessageID(const MessageName: String): Integer;

    // Os objetos que quiserem receber mensagens deverão registrar e desregistrar os IDs
    procedure RegisterMessage(MSG: Cardinal; aObject: IMessageReceptor);
    function  UnregisterMessage(MSG: Cardinal; aObject: IMessageReceptor): Integer;

    function  IndexMessage(MSG: Cardinal): Integer;

    // Todos os parâmetros deverão ser passados por endereço exceto as instâncias de objetos.
    // Ex: var r: Real; ... r := 3.45; ... SendMessage(ID, [@r, UmaInstancia, @i, ...]); ...
    function SendMessage(const MSG: TadvMessage): Integer; overload;
    function SendMessage(MSG: Cardinal; const Params: Array of Pointer): Integer; overload;
  end;

  // retorna o gerenciador Padrão de mensagens
  function GetMessageManager(): TMessageManager;

implementation
uses SysUtils;

type
  pTRecMessageList = ^TRecMessageList;
  TRecMessageList = record
    CodMsg : Cardinal;
    ObjList: TList;
  end;

var
   FMessageManager: TMessageManager = nil;

function GetMessageManager: TMessageManager;
begin
  if FMessageManager = nil then
     FMessageManager := TMessageManager.Create;
  Result := FMessageManager;
end;

{ TMessageManager }

constructor TMessageManager.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TMessageManager.Destroy;
begin
  FMessages.Free;
  FList.Free;
  inherited;
end;

Function TMessageManager.SendMessage(MSG: Cardinal; const Params: Array of Pointer): Integer;
var i    : Integer;
    aMSG : TadvMessage;
begin
  // configura a mensagem
  aMSG := TadvMessage.Create;
  aMSG.FID := MSG;
  SetLength(aMSG.FParams, Length(Params));
  for i := 0 to high(Params) do aMSG.FParams[i] := Params[i];

  // Transmite a mensagem
  Result := 0;
  try
    Result := SendMessage(aMSG);
  finally
    aMSG.Free;
  end;
end;

function TMessageManager.GetMessageID(const MessageName: String): Integer;
begin
  if FMessages <> nil then
     Result := FMessages.IndexOf(MessageName)
  else
     Result := -1;
end;

function TMessageManager.RegisterMessageID(const MessageName: String): Integer;
begin
  if FMessages = nil then FMessages := TStringList.Create;
  if FMessages.IndexOf(MessageName) = -1 then
     Result := FMessages.Add(MessageName)
  else
     raise Exception.Create(ErrorMessage2);
end;

procedure TMessageManager.RegisterMessage(MSG: Cardinal; aObject: IMessageReceptor);
var i,ii: Integer;
    p: pTRecMessageList;
begin
  ii := IndexMessage(MSG);

  if ii > -1 then // a mensagem já está registrada
     begin
     i := TRecMessageList(FList[ii]^).ObjList.IndexOf(pointer(aObject));
     if i = -1 then // registra o objeto se ele ainda não foi registrado
        TRecMessageList(FList[ii]^).ObjList.Add(pointer(aObject));
     end
  else
     begin // registra a mensagem e o objeto
     New(p);
     p^.CodMsg := MSG;
     p^.ObjList := TList.Create;
     p^.ObjList.Add(pointer(aObject));
     FList.Add(p);
     end;
end;

function TMessageManager.UnregisterMessage(MSG: Cardinal; aObject: IMessageReceptor): Integer;
var ii: Integer;
begin
  ii := IndexMessage(MSG);

  if ii > -1 then
     begin
     TRecMessageList(FList[ii]^).ObjList.Remove(pointer(aObject));
     if TRecMessageList(FList[ii]^).ObjList.Count = 0 then
        begin
        TRecMessageList(FList[ii]^).ObjList.Free;
        Dispose(pTRecMessageList(FList[ii]));
        FList.Delete(ii);
        end;
     end;

  Result := FList.Count;
end;

function TMessageManager.SendMessage(const MSG: TadvMessage): Integer;
var ii, i: Integer;
    o: IMessageReceptor;
begin
  ii := IndexMessage(MSG.ID);

  Result := 0;
  if ii = -1 then Exit;

  for i := 0 to TRecMessageList(FList[ii]^).ObjList.Count - 1 do
    begin
    o := IMessageReceptor(TRecMessageList(FList[ii]^).ObjList[i]);
    if (o <> nil) then
       try
       if o.ReceiveMessage(MSG) then
          inc(Result);
       except
       end;
    end;
end;

function TMessageManager.IndexMessage(MSG: Cardinal): Integer;
var i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if TRecMessageList(FList[i]^).CodMsg = MSG then
       begin
       Result := i;
       Exit;
       end;
  Result := -1;
end;

{---------------------------------------------------------------------------}

{ TadvMessage }

function TadvMessage.getParamCount: Integer;
begin
  Result := Length(FParams);
end;

function TadvMessage.ParamAsBoolean(ParamIndex: Integer): Boolean;
begin
  Result := Boolean(FParams[ParamIndex]^);
end;

function TadvMessage.ParamAsInteger(ParamIndex: Integer): Integer;
begin
  Result := Integer(FParams[ParamIndex]^);
end;

function TadvMessage.ParamAsObject(ParamIndex: Integer): TObject;
begin
  Result := FParams[ParamIndex];
end;

function TadvMessage.ParamAsPointer(ParamIndex: Integer): Pointer;
begin
  Result := FParams[ParamIndex];
end;

function TadvMessage.ParamAsReal(ParamIndex: Integer): Real;
begin
  Result := Real(FParams[ParamIndex]^);
end;

function TadvMessage.ParamAsString(ParamIndex: Integer): String;
begin
  Result := string(FParams[ParamIndex]^);
end;

initialization
  // nada

finalization
  GetMessageManager.Free();

end.

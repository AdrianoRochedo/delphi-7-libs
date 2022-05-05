unit ObjectListEx;

{
  As instancias desta classe possuem a capacidade de remover objetos
  pelo simples recebimento de uma mensagem "OL_RemoveObject", sendo
  assim, os objetos nao necessitam conhecer a lista onde estao armazenados.
}

{
  ATENCAO:
  Verificar que os objetos que estao sendo adicionados e os que
  sao enviados via parametros nas mensagens de destruicao sao os
  mesmos.

  Nos objetos que utilizam esta classe, quando a destruicao for manual,
  eh necessario o envio da mensagem "OL_RemoveObject" antes da destruicao
  para que este objeto seja retirado da lista.
}

interface
uses Classes,
     SysUtils,
     MessageManager,
     SysUtilsEx;

type
  TObjectListEx = class(T_NRC_InterfacedObject, IMessageReceptor)
  private
    FList: TList;
    function ReceiveMessage(const MSG: TadvMessage): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Add(aObj: TObject);
  end;

var
  OL_RemoveObject : integer;

implementation

{ TObjectListEx }

constructor TObjectListEx.Create();
begin
  inherited Create();
  FList := TList.Create();
  getMessageManager.RegisterMessage(OL_RemoveObject, self);
end;

destructor TObjectListEx.Destroy();
var i: Integer;
begin
  getMessageManager.UnregisterMessage(OL_RemoveObject, self);
  for i := 0 to FList.Count-1 do TObject(FList[i]).Free();
  FList.Free();
  inherited;
end;

procedure TObjectListEx.Add(aObj: TObject);
begin
  FList.Add(aObj);
end;

function TObjectListEx.ReceiveMessage(const MSG: TadvMessage): Boolean;
var o: TObject;
begin
  if MSG.ID = OL_RemoveObject then
     begin
     o := MSG.ParamAsObject(0);
     FList.Remove(o);
     end;
end;

initialization
  OL_RemoveObject := getMessageManager.RegisterMessageID('OL_RemoveObject');

end.

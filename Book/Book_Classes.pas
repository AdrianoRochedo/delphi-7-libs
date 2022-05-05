unit Book_Classes;

{
  ATENCAO:
    Para as paginas serem registradas automaticamente, as unidades de codigo
    precisam ser inicializadas, ou seja, utilizadas atraves da palavra chave
    "uses" antes da chamada do método Book.NewPage().
}

interface
uses Classes,
     Book_Interfaces;

type
  TPagesFactory = {private} class
  private
    FTypeList: TStrings;
    FIntfList: TInterfaceList;
    constructor Create();
    destructor Destroy(); override;
  public
    procedure RegisterPage(const aType: string; Factory: IPageFactory);
    function ByType(const aType: string): IPageFactory;
  end;

  function getPagesFactory(): TPagesFactory;

implementation

  var
    PagesFactory: TPagesFactory = nil;

  function getPagesFactory(): TPagesFactory;
  begin
    if PagesFactory = nil then
       PagesFactory := TPagesFactory.Create();
       
    Result := PagesFactory;
  end;

{ TPagesFactory }

function TPagesFactory.ByType(const aType: string): IPageFactory;
var i: Integer;
begin
  i := FTypeList.IndexOf(aType); // not case
  if i > -1 then
     result := IPageFactory(FIntfList[i])
  else
     result := nil;
end;

constructor TPagesFactory.Create();
begin
  inherited Create();
  FTypeList := TStringList.Create();
  FIntfList := TInterfaceList.Create();
end;

destructor TPagesFactory.Destroy();
begin
  FTypeList.Free();
  FIntfList.Free();
  inherited;
end;

procedure TPagesFactory.RegisterPage(const aType: string; Factory: IPageFactory);
begin
  FTypeList.Add(aType);
  FIntfList.Add(Factory);
end;

end.

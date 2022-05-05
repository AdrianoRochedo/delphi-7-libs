unit XML_Interfaces;

interface
uses classes
     {$ifdef MSXML}
     ,MSXML4
     {$endif}
     ;

type
  {Objetos que queiram ser formatados/salvos em XML por mecanismos genéricos
   deverão implementar esta interface.}
  IToXML = interface
    ['{FBED49F7-D530-4783-BD8E-32D6ED256F21}']
    procedure ToXML(Buffer: TStrings; Ident: Integer);
    function GetClassName(): String;
  end;

  {Objetos que queiram ser lidos de arquivos XML por mecanismos genéricos
   deverão implementar esta interface.}
  IFromXML = interface
    {$ifdef MSXML}
    procedure FromXMLNodeList(NodeList: IXMLDOMNodeList);
    {$endif}
    function GetObjectReference(): TObject; {Result := Self}
    function ToString(): string;
  end;

  {Objetos que queiram ser formatados/salvos em BXML por mecanismos genéricos
   deverão implementar esta interface.}
  IToBXML = interface(IToXML)
    function GetBlockName: String;
    function GetBlockNameComment: String;
    procedure ToBXML(Buffer: TStrings; Ident: Integer);
  end;

  {Objetos que necessitarem serem fábricas de objetos que implementam a interface "IFromXML"
  deverão implementar esta interface.}
  IFromXML_Factory = interface
    function CreateObject(const ClassName: String): IFromXML;
  end;

implementation

end.

unit BXML_DLL_Interfaces;

interface
uses classes, XMLDoc;

type
  IBXML_DLL_Processor = interface
    procedure Release; stdcall;
    procedure SetBuffer(Buffer: TStrings); stdcall;
    function GetBuffer: TStrings; stdcall;
    procedure SetXMLDoc(Doc: TXMLDocument); stdcall;
    function GetXMLDoc: TXMLDocument; stdcall;
  end;

implementation

end.

unit BXML_DLL_Processor;

interface
uses classes;

type
  TBase_BXML_DLL_Processor = class
  protected
    FActiveBuffer: TStrings;
    FXML: WideString;
    FInputFile: WideString;
    FOutputFile: WideString;
    procedure DoProcess; virtual; abstract;
    function GetModuleName: WideString; virtual; abstract;
  public
    procedure SetBuffer(Buffer: TStrings);
    function GetBuffer: TStrings;
    procedure SetXMLDoc(XML: WideString);
    function GetXMLDoc: WideString;
    procedure SetInputFile(Name: WideString);
    function GetInputFile: WideString;
    function GetOutputFile: WideString; virtual;
    procedure SetOutputFile(Name: WideString);
  end;

implementation
uses SysUtils;

{ TXML_DLL_Processor }

function TBase_BXML_DLL_Processor.GetBuffer: TStrings;
begin
  Result := FActiveBuffer;
end;

function TBase_BXML_DLL_Processor.GetInputFile: WideString;
begin
  Result := FInputFile;
end;

function TBase_BXML_DLL_Processor.GetOutputFile: WideString;
begin
  Result := FOutputFile;
end;

function TBase_BXML_DLL_Processor.GetXMLDoc: WideString;
begin
  Result := FXML;
end;

procedure TBase_BXML_DLL_Processor.SetBuffer(Buffer: TStrings);
begin
  FActiveBuffer := Buffer;
end;

procedure TBase_BXML_DLL_Processor.SetInputFile(Name: WideString);
begin
  FInputFile := Name;
end;

procedure TBase_BXML_DLL_Processor.SetOutputFile(Name: WideString);
begin
  FOutputFile := Name;
end;

procedure TBase_BXML_DLL_Processor.SetXMLDoc(XML: WideString);
begin
  FXML := XML;
  if FXML <> '' then
     try
       DoProcess;
     except
       on e: Exception do
          begin
          GetBuffer.Add('<p><font color="red">Plugin:<b>' + GetModuleName + '</b><br>');
          GetBuffer.Add('Error: ' + e.Message);
          GetBuffer.Add('</font></p>');
          end;
     end;
end;

end.

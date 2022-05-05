unit XML_Utils;

  { Atenção: Para utilizar as rotinas que utilizam recursos do mecanismo
             XML da Microsoft declare a variável de compilação "MSXML" }

interface
uses {$ifdef MSXML}
     XML_Interfaces,
     MSXML4,
     {$endif}
     Classes,
     SysUtilsEx;

type
  TXML_Writer = class
  private
    FIdentSize: Integer;
    FIdent: ShortString;
    FBuffer: TStrings;
    FDecimalSeparator: Char;
    procedure SetIdentSize(Value: Integer);
    function GetParamValue(const Params: array of Const; i: Integer): String;
  public
    constructor Create(Buffer: TStrings; IdentSize: Integer = 0);

    function beginIdent(): Integer;
    function endIdent(): Integer;

    // Tags
    procedure WriteTag(const TagName: String);
    procedure beginTag(const TagName: String); overload;
    procedure beginTag(const TagName: String;
                       const ParamNames: Array of String;
                       const ParamValues: Array of Const); overload;
    procedure endTag(const TagName: String);

    // Este método não codifica os caracteres especiais
    procedure Write(const Text: String = ''); overload;

    // Utilize este método quando a codificação é condicional
    procedure Write(const TagName, Content: String; EncodeChars: Boolean); overload;

    // Este método sempre codifica os caracteres especiais
    procedure Write(const TagName: String; const Content: String); overload;

    procedure Write(const TagName: String; const Content: Integer); overload;
    procedure Write(const TagName: String; const Content: Boolean); overload;
    procedure Write(const TagName: String; const Content: Real); overload;
    procedure Write(const TagName: String; const Content: TStrings); overload;
    procedure Write(const TagName: String; const Content: TFloatArray); overload;
    procedure Write(const TagName: String; const Content: TIntArray); overload;

    procedure Write(const TagName: String;
                    const ParamNames: Array of String;
                    const ParamValues: Array of Const;
                    const Content: String = ''); overload;

    procedure WriteProperties(const TagName: String; const Content: TStrings); overload;

    procedure WriteHeader(const MethodName: String; const Header: array of string);

    property Buffer           : TStrings read FBuffer           write FBuffer;
    property IdentSize        : Integer  read FIdentSize        write SetIdentSize;
    property DecimalSeparator : Char     read FDecimalSeparator write FDecimalSeparator;
  end;

{$ifdef MSXML}
  TXML_StringList = class(TStringList)
  public
    procedure ToXML(Buffer: TStrings; Ident: Integer); overload;
    procedure ToXML(const tagName: string; Buffer: TStrings; Ident: Integer); overload;

    procedure FromXML(node: IXMLDomNode); overload;
    procedure FromXML(nodeList: IXMLDomNodeList); overload;
  end;
{$endif}

  {Escreve o cabeçalho de um arquivo XML.
   "MethodName" é utilizado em um comentário que forma o cabeçalho.}
  procedure WriteXMLHeader(Buffer: TStrings; const MethodName: String; const Header: array of string);

{$ifdef MSXML}
  {Se o documento é válido o documento é aberto senão uma exceção é disparada.}
  function OpenXMLDocument(const FileName: String): IXMLDOMDocument;

  {Salva um objeto que implementa a interface "IToXML" em um arquivo XML}
  procedure SaveObjectToXML(Obj: IToXML; const FileName: String);

  {Lê um objeto que implementa a interface "IFromXML" de um arquivo XML}
  procedure LoadObjectFromXML(Obj: IFromXML; const FileName: String);

  function CreateObjectFromXML(const FileName: String; Factory: IFromXML_Factory): TObject;
  procedure CreateObjectsFromXML(const FileName: String; Factory: IFromXML_Factory);

  // Le os valores dos sub-nós para um TStrings
  procedure StringsFromXML(node: IXMLDomNode; Strings: TStrings);

  // Le os sub-nós como propriedades de uma TStrings
  // Ex: <x>y</x> --> x=y
  procedure PropertiesFromXML(node: IXMLDomNode; Properties: TStrings);

  // Retorna um array de floats dado um determinado tipo de no
  // Ex: <xxx e0="1.2" e1="2.3" e2="3.4" e3="3.4"/> --> [1.2, 2.3, 3.4, 3.4]
  function LoadFloatArray(node: IXMLDomNode): TFloatArray;
{$endif}

  // Converte uma lista de Strings para um formato XML específico
  procedure StringsToXML(SL, Buffer: TStrings; Ident: Integer);

  // Codifica os caracteres acima
  function XML_EncodeChars(const s: String; IsAttribute: Boolean): String;

implementation
uses SysUtils;

procedure WriteXMLHeader(Buffer: TStrings; const MethodName: String; const Header: array of string);
var i: integer;
begin
  Buffer.Add('<?xml version="1.0" encoding="ISO-8859-1" ?>');
  if MethodName <> '' then
     Buffer.Add('<!-- Arquivo gerado pela rotina ' + MethodName + ' -->');
  Buffer.Add('<!-- Autor: Adriano Rochedo Conceicao -->');
  for i := 0 to High(Header) do
    Buffer.Add('<!-- ' + Header[i] + ' -->');
  Buffer.Add('');
end;

{$ifdef MSXML}

procedure SaveObjectToXML(Obj: IToXML; const FileName: String);
var SL: TStrings;
    Ident: Integer;
begin
  Ident := 2;
  SL := TStringList.Create;
  try
    WriteXMLHeader(SL, 'XML_Utils.SaveObjectToXML', []);
    SL.Add('<Object class="' + Obj.GetClassName + '">');
       Obj.ToXML(SL, Ident);
    SL.Add('</Object>');
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

function OpenXMLDocument(const FileName: String): IXMLDOMDocument;
begin
  Result := MSXML4.CreateDOMDocument();
  Result.Load(FileName);
  Result.preserveWhiteSpace := true;
  if Result.parseError.errorCode <> 0 then
     raise Exception.Create('Invalid Document: ' + Result.parseError.reason + #13 +
                                                   Result.parseError.srcText);
end;

procedure LoadObjectFromXML(Obj: IFromXML; const FileName: String);
var Doc: IXMLDOMDocument;
begin
  Doc := OpenXMLDocument(FileName);

  if Doc.DocumentElement.nodeName = 'Object' then
     Obj.FromXMLNodeList(Doc.DocumentElement.childNodes)
  else
     raise Exception.Create('Invalid Document Element: ' + Doc.DocumentElement.nodeName);
end;

function CreateObjectFromXML(const FileName: String; Factory: IFromXML_Factory): TObject;
var Obj: IFromXML;
    Doc: IXMLDOMDocument;
begin
  Doc := OpenXMLDocument(FileName);

  if Doc.DocumentElement.nodeName = 'Object' then
     begin
     Obj := Factory.CreateObject(Doc.DocumentElement.attributes[0].Text);
     Obj.FromXMLNodeList(Doc.DocumentElement.childNodes);
     Result := Obj.GetObjectReference();
     end
  else
     raise Exception.Create('Invalid Document Element: ' + Doc.DocumentElement.nodeName);
end;

procedure CreateObjectsFromXML(const FileName: String; Factory: IFromXML_Factory);
var Obj : IFromXML;
    Doc : IXMLDomDocument;
    No  : IXMLDomNode;
    i   : Integer;
begin
  Doc := OpenXMLDocument(FileName);

  if Doc.DocumentElement.nodeName = 'Objects' then
     for i := 0 to Doc.DocumentElement.childNodes.length-1 do
       begin
       No := Doc.DocumentElement.childNodes[i];
       Obj := Factory.CreateObject(No.attributes[0].Text);
       Obj.FromXMLNodeList(No.childNodes);
       end
  else
     raise Exception.Create('Invalid Document Element: ' + Doc.DocumentElement.nodeName);
end;

// Le os valores dos sub-nós para um TStrings
procedure StringsFromXML(node: IXMLDomNode; Strings: TStrings);
var i: Integer;
begin
  Strings.Clear();
  for i := 0 to node.childNodes.length-1 do
    Strings.Add(node.childNodes[i].text);
end;

// Le os sub-nós como propriedades de uma TStrings
// Ex: <x>y</x> --> x=y
procedure PropertiesFromXML(node: IXMLDomNode; Properties: TStrings);
var i: Integer;
    e: IXMLDomNode;
begin
  Properties.Clear();
  for i := 0 to node.childNodes.length-1 do
    begin
    e := node.childNodes[i];
    Properties.Add(e.nodeName + '=' + e.text);
    end;
end;

{$endif}

procedure StringsToXML(SL, Buffer: TStrings; Ident: Integer);
var s: String;
    i: Integer;
begin
  s := StringOfChar(' ', Ident);
  for i := 0 to SL.Count-1 do
    Buffer.Add(s + '<string>' + XML_EncodeChars(SL[i], False) + '</string>');
end;

{ TXML_Writer }

constructor TXML_Writer.Create(Buffer: TStrings; IdentSize: Integer = 0);
begin
  inherited Create;
  FIdentSize := IdentSize;
  FIdent := StringOfChar(' ', FIdentSize);
  FBuffer := Buffer;
  FDecimalSeparator := SysUtils.DecimalSeparator;
end;

function TXML_Writer.beginIdent(): Integer;
begin
  SetIdentSize(FIdentSize + 2);
  Result := FIdentSize;
end;

function TXML_Writer.endIdent(): Integer;
begin
  SetIdentSize(FIdentSize - 2);
  Result := FIdentSize;
end;

procedure TXML_Writer.SetIdentSize(Value: Integer);
begin
  if Value > 0 then
     FIdentSize := Value
  else
     FIdentSize := 0;

  FIdent := StringOfChar(' ', FIdentSize);
end;

procedure TXML_Writer.Write(const TagName: String; const Content: TFloatArray);
var s: string;
    i: integer;
begin
  s := FIdent + '<' + TagName;

  // elementos escritos como atributos
  for i := low(Content) to high(Content) do
    s := s + ' e' + toString(i) + '="' + toString(Content[i]) + '"';

  FBuffer.Add(s + '/>');
end;

procedure TXML_Writer.Write(const TagName: String; const Content: TIntArray);
var s: string;
    i: integer;
begin
  s := FIdent + '<' + TagName;

  // elementos escritos como atributos
  for i := low(Content) to high(Content) do
    s := s + ' e' + toString(i) + '="' + toString(Content[i]) + '"';

  FBuffer.Add(s + '/>');
end;

procedure TXML_Writer.Write(const TagName, Content: String);
begin
  FBuffer.Add(FIdent + '<' + TagName + '>' + XML_EncodeChars(Content, False) + '</' + TagName + '>');
end;

procedure TXML_Writer.Write(const TagName, Content: String; EncodeChars: Boolean);
begin
  if EncodeChars then
     Write(TagName, Content)
  else
     FBuffer.Add(FIdent + '<' + TagName + '>' + Content + '</' + TagName + '>');
end;

function TXML_Writer.GetParamValue(const Params: array of Const; i: Integer): String;
var dc: Char;
begin
  case Params[i].vType of
    vtString     : Result := XML_EncodeChars(Params[i].vString^, True); // String curta
    vtAnsiString : Result := XML_EncodeChars(string(Params[i].VAnsiString), True); // String longa
    vtInteger    : Result := IntToStr(Params[i].vInteger);
    vtBoolean    : Result := IntToStr(byte(Params[i].vBoolean));
    vtChar       : Result := Params[i].vChar;
    vtExtended   : begin
                   dc := Sysutils.DecimalSeparator;
                   Sysutils.DecimalSeparator := FDecimalSeparator;
                   Result := FloatToStr(Params[i].vExtended^);
                   Sysutils.DecimalSeparator := dc;
                   end;
    vtInt64      : Result := IntToStr(Params[i].VInt64^);
    end; {Case}
end;

procedure TXML_Writer.Write(const TagName: String;
                            const ParamNames: array of String;
                            const ParamValues: array of Const;
                            const Content: String = '');
var s: String;
    i: Integer;
begin
  if High(ParamNames) <> High(ParamValues) then
     Raise Exception.Create('TXML_Writer: Invalid count of ParamNames or ParamValues');

  s := FIdent + '<' + TagName;
  for i := 0 to High(ParamNames) do
      s := s + ' ' + ParamNames[i] + '="' + GetParamValue(ParamValues, i) + '"';

  if Content <> '' then
     s := s + '>' + XML_EncodeChars(Content, False) + '</' + TagName + '>'
  else
     s := s + '/>';

  FBuffer.Add(s);
end;

procedure TXML_Writer.beginTag(const TagName: String);
begin
  FBuffer.Add(FIdent + '<' + TagName + '>');
end;

procedure TXML_Writer.endTag(const TagName: String);
begin
  FBuffer.Add(FIdent + '</' + TagName + '>');
end;

procedure TXML_Writer.beginTag(const TagName: String;
                               const ParamNames: array of String;
                               const ParamValues: array of Const);
var s: String;
    i: Integer;
begin
  if High(ParamNames) <> High(ParamValues) then
     Raise Exception.Create('TXML_Writer: Invalid count of ParamNames or ParamValues');

  s := FIdent + '<' + TagName;
  for i := 0 to High(ParamNames) do
      s := s + ' ' + ParamNames[i] + '="' + GetParamValue(ParamValues, i) + '"';
  s := s + '>';

  FBuffer.Add(s);
end;

procedure TXML_Writer.Write(const TagName: String; const Content: Integer);
begin
  Write(TagName, IntToStr(Content), false);
end;

procedure TXML_Writer.Write(const TagName: String; const Content: Boolean);
begin
  Write(TagName, IntToStr(byte(Content)), false);
end;

procedure TXML_Writer.Write(const TagName: String; const Content: Real);
begin
  Write(TagName, FloatToStr(Content), false);
end;

procedure TXML_Writer.Write(const Text: String);
begin
  FBuffer.Add(FIdent + Text);
end;

procedure TXML_Writer.WriteTag(const TagName: String);
begin
  FBuffer.Add(FIdent + '<' + TagName + '/>');
end;

procedure TXML_Writer.Write(const TagName: String; const Content: TStrings);
var i: Integer;
begin
  beginTag(TagName);
    beginIdent();
    for i := 0 to Content.Count-1 do
      self.Write('string', Content[i]);
    endIdent();
  endTag(TagName);
end;

procedure TXML_Writer.WriteProperties(const TagName: String; const Content: TStrings);
var i: Integer;
begin
  beginTag(TagName);
    beginIdent();
    for i := 0 to Content.Count-1 do
      self.Write(Content.Names[i], Content.ValueFromIndex[i]);
    endIdent();
  endTag(TagName);
end;

procedure TXML_Writer.WriteHeader(const MethodName: String; const Header: array of string);
begin
  WriteXMLHeader(FBuffer, MethodName, Header);
end;

(* Caracteres especiais e seus códigos

    <  &lt;         ;  &#59;
    à  &agrave;     ®  &#174;
    >  &gt;         =  &#61;
    â  &acirc;      "  &#34;
    {  &#123;       á  &aacute;
    ã  &atilde;     "  &quot;
    }  &#125;       @  &#64;
    ü  &uuml;       ª  &ordf;
    :  &#58;        $  &#36;
    &  &#38;        º  &ordm;
    %  &#37;        ?  &#63;

*)

const
  XML_SpecialChars : set of Char = ['<', '>', '&', '"'];

// Codifica os caracteres acima
function XML_EncodeChars(const s: String; IsAttribute: Boolean): String;
var i, k: Integer;
begin
  // Determino o tamanho da String destino
  k := 0;
  for i := 1 to Length(s) do
    if s[i] in XML_SpecialChars then
       case s[i] of
         '<', '>': inc(k, 4);
         '"': if IsAttribute then inc(k, 5) else inc(k);
         '&': inc(k, 5);
       end
    else
       inc(k);

  if Length(s) = k then // Nada precisa ser substituído
     begin
     Result := s;
     Exit;
     end;

  // Seto o tamanho da String de destino
  SetLength(Result, k);

  // Troco os caracteres
  i := 1;
  k := 1;
  while k <= Length(s) do
    begin
    case s[k] of
      '<': begin
           Result[i+0] := '&';
           Result[i+1] := 'l';
           Result[i+2] := 't';
           Result[i+3] := ';';
           inc(i, 4);
           end;

      '>': begin
           Result[i+0] := '&';
           Result[i+1] := 'g';
           Result[i+2] := 't';
           Result[i+3] := ';';
           inc(i, 4);
           end;

      '&': begin
           Result[i+0] := '&';
           Result[i+1] := '#';
           Result[i+2] := '3';
           Result[i+3] := '8';
           Result[i+4] := ';';
           inc(i, 5);
           end;

      '"': if IsAttribute then
              begin
              Result[i+0] := '&';
              Result[i+1] := '#';
              Result[i+2] := '3';
              Result[i+3] := '4';
              Result[i+4] := ';';
              inc(i, 5);
              end
           else
               begin
               Result[i] := s[k];
               inc(i);
               end;

      else
         begin
         Result[i] := s[k];
         inc(i);
         end;

      end; // case global

    inc(k); // Percorro carac. a carac. a String de entrada
    end;
end;

{$ifdef MSXML}

function LoadFloatArray(node: IXMLDomNode): TFloatArray;
var i: Integer;
begin
  System.SetLength(result, node.attributes.length);
  for i := 0 to node.attributes.length-1 do
    result[i] := toFloat(node.attributes.item[i].text, 0);
end;

{ TXML_StringList }

procedure TXML_StringList.FromXML(NodeList: IXMLDomNodeList);
var no: IXMLDomNode;
begin
  self.Clear();
  no := NodeList.nextNode();
  while (no <> nil) do
    begin
    self.Add(no.Text);
    no := NodeList.nextNode();
    end;
end;

procedure TXML_StringList.FromXML(node: IXMLDomNode);
begin
  self.FromXML(node.childNodes);
end;

procedure TXML_StringList.ToXML(Buffer: TStrings; Ident: Integer);
begin
  StringsToXML(self, Buffer, Ident + 2);
end;

procedure TXML_StringList.ToXML(const tagName: string; Buffer: TStrings; Ident: Integer);
var s: string;
begin
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<' + tagName + '>');
    self.toXML(Buffer, Ident + 2);
  Buffer.Add(s + '</' + tagName + '>');
end;

{$endif}

end.

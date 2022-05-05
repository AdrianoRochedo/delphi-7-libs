unit Demand_Requirements;

interface
uses classes,
     SysUtils,
     wsVec,
     wsMatrix;

type
  TDemandRequirements = class
  private
    FReqs: TwsDataSet;
    function gerReq(i, j: Integer): Real;
    function getYear(i: Integer): Integer;
    function getYearAsString(i: Integer): String;
    function getYears: Integer;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: String);
    procedure SaveAsText(const FileName: String);

    property Years                      : Integer read getYears;
    property Year[i: Integer]           : Integer read getYear;
    property YearAsString[i: Integer]   : String  read getYearAsString;
    property Requirement[i, j: Integer] : Real    read gerReq;

  end;

implementation
uses DateUtils,
     XML_Utils,
     MSXML4,
     dialogs;

{ TDemandRequirements }

constructor TDemandRequirements.Create(const FileName: String);
var i: Integer;
begin
  inherited Create;

  // Cria um dataset com 12 columas (uma para cada mes do ano)
  FReqs := TwsDataSet.Create('Demand_Requirements');
  FReqs.ColIdentName := 'Ano';
  for i := 1 to 12 do FReqs.Struct.AddNumeric(ShortMonthNames[i], '');

  LoadFromFile(FileName);
end;

destructor TDemandRequirements.Destroy;
begin
  FReqs.Free;
  inherited;
end;

function TDemandRequirements.gerReq(i, j: Integer): Real;
var x: Double;
begin
  if not FReqs.IsMissValue(i, j, x) then
     Result := x
  else
     Result := 0;
end;

function TDemandRequirements.getYear(i: Integer): Integer;
begin
  Result := StrToInt(getYearAsString(i));
end;

function TDemandRequirements.getYearAsString(i: Integer): String;
begin
  Result := FReqs.RowName[i];
end;

function TDemandRequirements.getYears: Integer;
begin
  Result := FReqs.nRows;
end;

procedure TDemandRequirements.LoadFromFile(const FileName: String);
var dom    : IXMLDOMDocument;
    no     : IXMLDOMNode;
    necess : IXMLDOMNode;
    i      : Integer;
    anoIni : Integer;
    anoFim : Integer;
    mes    : Integer;
    ano    : Integer;
    val    : Real;
    v      : TwsVec;
    dc     : Char;
begin
  if not FileExists(FileName) then
     raise Exception.Create('File not Found'#13'Filename: ' + FileName);

  dom := OpenXMLDocument(FileName);

  dc := DecimalSeparator;
  DecimalSeparator := '.';

  try
    no := dom.selectSingleNode('data/month_requirements');
    if no <> nil then
       begin
       // Cria as linhas que conterão as necessidades mensais
       if no.childNodes.length >= 2 then
          begin
          anoIni := StrToInt(no.childNodes.item[0].Text); // 1. no
          anoFim := StrToInt(no.childNodes.item[1].Text); // 2. no
          for i := anoIni to anoFim do
            begin
            v := TwsDFVec.Create(12);
            v.Name := IntToStr(i);
            v.Fill(0);
            FReqs.MAdd(v);
            end;
          end;

       // Obtem as necessidades mensais (3. no em diante)
       for i := 2 to no.childNodes.length-1 do
         begin
         necess := no.childNodes.item[i];
         mes := StrToInt(necess.attributes.item[0].text);
         ano := StrToInt(necess.attributes.item[1].text);
         if mes < 99 then
            begin
            val := StrToFloatDef(necess.attributes.item[2].text, 0);
            if val <> 0 then
               begin
               val := val / DaysInAMonth(ano, mes) / 8.64; // Hectômetro cúbico
               FReqs[ano - anoIni + 1, mes] := val;
               end;
            end;
         end;
       end;
  finally
    DecimalSeparator := dc;
  end;
end;

procedure TDemandRequirements.SaveAsText(const FileName: String);
var SL: TStrings;
begin
  SL := TStringList.Create;
  FReqs.Print(SL);
  SL.SaveToFile(FileName);
  SL.Free;
end;

end.

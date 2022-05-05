unit BXML_Output;

interface
uses classes,
     XML_Interfaces;

type

  TBlockXML = class
  private
    FNameSpaceComment: String;
    FNameSpace: String;
    FBlock: TStrings;
  public
    constructor Create(const NameSpace, NameSpaceComment: String; Block: TStrings);
    destructor Destroy; override;

    property NameSpace: String read FNameSpace;
    property NameSpaceComment: String read FNameSpaceComment;
    property Block: TStrings read FBlock;
  end;

  TBXML_Output = class
  private
    FList: TStrings;
    FActiveBlock: TBlockXML;
    FFileName: String;
    FTitle: string;
    function GetBlock(i: Integer): TBlockXML;
    function GetCount: Integer;
    function GetNewBlockName(BlockName: String): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function  Add(intf: IToBXML; BlockName: String = ''): Integer;

    // Texto ...
    procedure BeginBlock(const BlockName, NameSpace, NameSpaceComment: String);
    procedure EndBlock;

    function ActivateBlock(const Name: String): TBlockXML;

    procedure SaveToFile(const Name: String);
    function  SaveToTempFile(): String;

    procedure ShowCode();

    property ActiveBlock: TBlockXML read FActiveBlock;
    property Block[i: Integer]: TBlockXML read GetBlock;
    property Count: Integer read GetCount;
    property FileName: String read FFileName write FFileName;
    property Title : string read FTitle write FTitle;
  end;

implementation
uses SysUtils,
     FileUtils,
     ShellAPI,
     Windows;

{ TBlockXML }

constructor TBlockXML.Create(const NameSpace, NameSpaceComment: String; Block: TStrings);
begin
  inherited Create;
  FNameSpaceComment := NameSpaceComment;
  FNameSpace := NameSpace;
  FBlock := Block;
end;

destructor TBlockXML.Destroy;
begin
  FBlock.Free;
  inherited;
end;

{ TBXML_Output }

constructor TBXML_Output.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TBXML_Output.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TBXML_Output.ActivateBlock(const Name: String): TBlockXML;
var i: Integer;
begin
  i := FList.IndexOf(Name);
  if i > -1 then
     FActiveBlock := TBlockXML(FList.Objects[i])
  else
     FActiveBlock := nil;

  Result := FActiveBlock;
end;

function TBXML_Output.Add(Intf: IToBXML; BlockName: String = ''): Integer;
var SL: TStrings;
    k: Integer;
begin
  if BlockName = '' then
     BlockName := 'BlockXML_' + IntToStr(FList.Count);

  // Acha um nome que não exista na lista
   BlockName := GetNewBlockName(BlockName);

  // Gera o novo bloco
  k := 2;
  SL := TStringList.Create;
  intf.ToBXML(SL, k);
  FActiveBlock := TBlockXML.Create(intf.GetBlockName, intf.GetBlockNameComment, SL);
  Result := FList.AddObject(BlockName, FActiveBlock);
end;

procedure TBXML_Output.Clear;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    FList.Objects[i].Free;
  FList.Clear;
end;

procedure TBXML_Output.BeginBlock(const BlockName, NameSpace, NameSpaceComment: String);
var SL: TStrings;
    s: String;
begin
  // Gera o novo bloco
  s := GetNewBlockName(BlockName);

  SL := TStringList.Create;
  FActiveBlock := TBlockXML.Create(NameSpace, NameSpaceComment, SL);
  FList.AddObject(s, FActiveBlock);

  FActiveBlock.Block.Add('  <' + NameSpace + ':block>');
end;

procedure TBXML_Output.EndBlock;
begin
  FActiveBlock.Block.Add('  </' + FActiveBlock.NameSpace + ':block>');
end;

function TBXML_Output.GetBlock(i: Integer): TBlockXML;
begin
  if (i >= 0) and (i < FList.Count) then
     Result := TBlockXML(FList.Objects[i])
  else
     Raise Exception.Create('TBXML_Output: Invalid Block Index: ' + IntToStr(i));
end;

function TBXML_Output.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TBXML_Output.SaveToFile(const Name: String);
var SL: TStrings;
    SL2, SL3: TStringList;
    i, k: Integer;
    s: String;
begin
  SL := TStringList.Create;
  try
    // Cabeçalho
    SL.Add('<?xml version="1.0"');
    SL.Add('      encoding="ISO-8859-1"?>');
    SL.Add('');
    SL.Add('<!--');
    SL.Add('	Copyright 2002 Rochedo Software');
    SL.Add('-->');
    SL.Add('');
    SL.Add('<blocks');

    // NameSpaces
    SL2 := TStringList.Create;
    SL2.Sorted := True;
    SL2.Duplicates := dupIgnore;

    // NameSpaces Comments
    SL3 := TStringList.Create;

    SL2.Add('Control');
    SL3.Add('bloco de controle');
    for i := 0 to FList.Count-1 do
      try
        k := SL2.Add(TBlockXML(FList.Objects[i]).NameSpace);
        SL3.Insert(k, TBlockXML(FList.Objects[i]).NameSpaceComment);
      except
        // Nada
      end;
    for i := 0 to SL2.Count-1 do
      begin
      s := '  xmlns:' + SL2[i] + '="' + SL3[i] + '"';
      if (i < SL2.Count-1) then SL.Add(s) else SL.Add(s + '>');
      end;
    SL2.Free;
    SL3.Free;

    SL.Add('');

    // Bloco de controle obrigatório
    SL.Add('  <Control:block>');
    SL.Add('    <NewDoc Name="A"/> <ActiveDoc Name="A"/>');
    SL.Add('  </Control:block>');

    SL.Add('');

    // Blocos
    for i := 0 to FList.Count-1 do
      begin
      SL.AddStrings(TBlockXML(FList.Objects[i]).Block);
      if i < SL.Count-1 then SL.Add('');
      end;

    // Bloco de controle obrigatório
    SL.Add('  <Control:block>');
    SL.Add('    <SaveDoc Name="A"/>');
    SL.Add('  </Control:block>');
    SL.Add('</blocks>');

    FFileName := Name;
    SL.SaveToFile(FFileName);
  finally
    SL.Free;
  end;
end;

procedure TBXML_Output.ShowCode;
var h: THandle;
    s: String;
begin
  s := FFileName;
  FFileName := GetTempFile('', 'BXML_OP', 'txt');
  SaveToFile(FFileName);
  h := GetModuleHandle(pChar(ParamStr(0)));
  ShellExecute(h, 'open', pChar(FFileName), '', '', SW_SHOWNORMAL);
  FFileName := s;
end;

function TBXML_Output.SaveToTempFile(): String;
begin
  Result := GetTempFile('', 'wsop', 'bxml');
  SaveToFile(Result);
end;

function TBXML_Output.GetNewBlockName(BlockName: String): String;
var k, i: Integer;
begin
  if BlockName = '' then BlockName := 'Block';
  k := 0;
  repeat
    inc(k);
    if k = 1 then
       begin
       Result := BlockName;
       i := FList.IndexOf(Result);
       end
    else
       begin
       Result := BlockName + '_' + IntToStr(k);
       i := FList.IndexOf(Result);
       end;
    until i = -1;
end;

end.

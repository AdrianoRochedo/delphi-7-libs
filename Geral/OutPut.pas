unit OutPut;

{ INSTRUÇÕES:

  Por default, TOutPut gerará todas as saídas em formato HTML, para escolher o formato
  desejado use a propriedade DocType.

  Para gerar uma saída especifique um nome de arquivo em FileName. Mas cuidado, se o
  arquivo já existir será sobreescrito.

  Para mostrar a sáida gerada, chame os métodos Save e Show em sequência.
}

{05/09/00 Leonardo Silveira -> Adicao da classe TPath, que será utilizada para armazenar os caminhos de dados
do winstat. Existira um objeto global chamado gPath na Unit wsConst}

interface
uses Classes,
     SysUtils,
     Graphics;

type
  TEnumDocType = (dtHTML, dtTEXT, dtSHEET);
  TEnumAlign   = (aLeft, aRight, aCenter, amiddle, abottom, abaseline);
  TFontStyle   = (aBold, aItalic, aBoldItalic, nBoldItalic);
  TArray       =  array[0..5] of string;
  // Gerencia as propriedades de um documento HTML

const
  EnumAlign : TArray       = ('Left', 'Right', 'Center', 'Middle', 'Bottom', 'Baseline');
  ActOut    : TEnumDocType = dtTEXT;  // Estabelece formato atual de saida.

type
  {$ifdef prj_WinStat}
  TPath = Class
  private
    FData    : String;
    FTxt     : String;
    FMatVet  : String;
    FTempPath: String;
    FJpgName : String;
    function GetData   : String;
    function GetTxt    : String;
    function GetMatVet : String;

    procedure SetData    (Value : String);
    procedure SetTxt     (Value : String);
    procedure SetMatVet  (Value : String);

  public
    constructor Create;

    property Data     : String   read GetData    write SetData;
    property Txt      : String   read GetTxt     write SetTxt;
    property MatVet   : String   read GetMatVet  write SetMatVet;
    property TempPath : String   read FTempPath  write FTempPath;
    property JpgName  : String   read FJpgName   Write FJpgName;
  end;
  {$endif}

  THTML_Doc = class
  private
    FBI : String;
    FALC: TColor;
    FTC : TColor;
    FLC : TColor;
    FVLC: TColor;
    FBC : TColor;
    FTitle: String;
  public
      constructor Create;

    procedure BeginDoc(Text: TStrings);
    procedure EndDoc(Text: TStrings);

    property Title              : String read FTitle write FTitle;
    property Background_Image   : String read FBI    write FBI;
    property Background_Color   : TColor read FBC    write FBC;
    property Text_Color         : TColor read FTC    write FTC;
    property Link_Color         : TColor read FLC    write FLC;
    property Visited_Link_Color : TColor read FVLC   write FVLC;
    property Active_Link_Color  : TColor read FALC   write FALC;
  end;

  // Gerencia as propriedades do texto HTML
  THTML_Font = class
  private
    FItalic: Boolean;
    FBold: Boolean;
    FSize: Integer;
    FName: String;
    FColor: TColor;
    FAlign: TEnumAlign;
  public
    constructor Create;

    procedure BeginFont(Text: TStrings);
    procedure EndFont(Text: TStrings);

    procedure SaveProperties;
    procedure RetrieveProperties;

    property Name   : String       read FName   write FName;
    property Size   : Integer      read FSize   write FSize;
    property Color  : TColor       read FColor  write FColor;
    property Bold   : Boolean      read FBold   write FBold;
    property Italic : Boolean      read FItalic write FItalic;
    property Align  : TEnumAlign   read FAlign  write FAlign;
  end;


  // Propriedades de uma tabela definidas na linguagem HTML \\
  TTable_Properties = class
  private
    FAlign        : TEnumAlign;        //Alinhamento
    FBorder       : integer;           //Largura da borda
    FCellPadding  : integer;           //Quantidade de espaço entre as bordas de uma célula e seu conteúdo \\
    FCellSpacing  : integer;           //Quantidade de espaço inserido entre as células individuais \\
    FWidth        : integer;           //Largura de uma tabela em termos absolutos
    FAlignCaption : TEnumAlign;        // Alinhamento do caption
    FAlignH       : TEnumAlign;        // Alinhamento do cabeçalho
  public
    constructor create;

    procedure BeginTable(Text : Tstrings);
    procedure EndTable(Text : Tstrings);

    procedure BeginCaption(Text : TStrings);
    procedure EndCaption(Text : TStrings);

    procedure BeginH(Text : TStrings);
    procedure EndH(Text : TStrings);

    procedure SetProperties(aAlign : TEnumAlign;aNowRap: Boolean;aAlignH : TenumAlign);

    property Align       : TEnumAlign  read  FAlign        write  FAlign;
    property Border      : integer     read  FBorder       write  FBorder;
    property CellPadding : integer     read  FCellPadding  write  FCellPadding;
    property CellSpacing : integer     read  FCellSpacing  write  FCellSpacing;
    property Width       : integer     read  FWidth        write  FWidth;
    property AlignCaption: TEnumAlign  read  FAlignCaption write  FAlignCaption;
    property AlignH      : TEnumAlign  read  FAlignH       write  FAlignH;
  end;

  //Classe generica para propriedades de uma linha ou de uma célula \\
  TRow_Properties = Class
  private
    FWidth    : integer; // Largura da Célula
  public
    constructor create;

    procedure BeginRow(Text : TStrings);
    //procedure EndRow(Text : TStrings);
    property Width   : integer    read FWidth   write FWidth;
  end;

  TCell_Properties = class
  private
    FAlign    : TEnumAlign;
    FColSpan  : integer; // Quantas colunas da tabela uma célula deverá abranger\\
    FHeight   : integer; // Altura da celula
    FNowRap   : boolean; // Permite que o broser mostre todo o conteudo da cedula em uma unica linha\\
    FRowSpan  : integer; // Quantas linhas da tabela uma célula deverá abranger\\
    FWidth    : integer;
    FVAlign   : TEnumAlign;
    // Controla a colocação do conteúdo da células nas partes superior,
    //média ou inferior da céulua ou para alinhar todos
    // os elementos a uma linha comum
  public
    constructor create;

    procedure BeginCell(Text : TStrings);
    //procedure EndCell(Text : TStrings);

    property Align   : TEnumAlign read FAlign   write FAlign;
    property ColSpan : integer    read FColSpan write FColSpan;
    property Height  : integer    read FHeight  write FHeight;
    property NowRap  : boolean    read FNowRap  write FNowRap;
    property RowSpan : integer    read FRowSpan write FRowspan;
    property VAlign  : TEnumAlign read FVAlign  write FVAlign;
    property Width   : integer    read FWidth   write FWidth;
  end;

  // Gerencia a saída dos dados em vários formatos possíveis
  TOutPut = class
  private
    FFileName  : String;
    FDocType   : TEnumDocType;
    FHD        : THTML_Doc;
    FHF        : THTML_Font;
    FTP        : TTable_Properties;
    FRP        : TRow_Properties;
    FCP        : TCell_Properties;
    FText      : TStrings; //Buffer acumulado
    FPath      : String;
    FAppend    : Boolean;
    FJaIniciou : Boolean;

    {$ifdef prj_WinStat}
    FBuffer    : TStrings; //Buffer atual
    {$endif}

    procedure SetFontName(const Name : string);
    procedure SetFontSize(Size : integer);
    procedure SetFontColor(Color : TColor);
    procedure SetFontStyle(Style : TFontStyle); overload;
    procedure SetFontAlign(Align : TEnumAlign);
    //procedure SetAppend(Value : Boolean);
    function  GetFontName  : string ;
    function  GetFontSize  : integer;
    function  GetFontColor : TColor;
    function  GetFontStyle : TFontStyle;
    function  GetFontAlign : TEnumAlign;
    function  GetAppend    : Boolean;
    //procedure SetFileName(const Value: String);

  public
    Constructor Create(DocType: TEnumDocType = dtTEXT; const FileName: String = '');
    Destructor Destroy; override;

    procedure Write       (const s: String = ''; NewLine: Boolean = True); overload;
    procedure Write       (Image: TGraphic); overload;
    procedure Write       (SL: TStrings); overload;

    procedure Write       (const Text     : String;
                                 Color    : TColor;
                                 Size     : Integer;
                                 Center   : Boolean;
                                 Bold     : Boolean;
                                 Italic   : Boolean;
                                 NewLine  : Boolean = True;
                           const FontName : String = 'ARIAL'); overload;

    procedure WriteBold   (const s: String; NewLine: Boolean = True);
    procedure WriteItalic (const s: String; NewLine: Boolean = True);
    procedure WriteBI     (const s: String; NewLine: Boolean = True);
    procedure WriteCenter (const s: String; Len: Word = 110);
    procedure WriteTitle  (const s: String; aColor: TColor=clBlue; aSize: byte=1; aCap: Boolean=False);
    procedure WriteLine   (N : integer = 1);
    procedure WriteLink   (const Text, Link: String; NewLine: Boolean = True);
    procedure Warning     (Aviso: string);

    procedure WriteSeparator;
    procedure WriteSpaces(N: Integer);

    // Somente HTML
    procedure BeginDoc;
    procedure EndDoc;
    procedure BeginParagraph;
    procedure EndParagraph;
    procedure BeginFont;
    procedure EndFont;
    procedure BeginTable;
    procedure EndTable;
    procedure BeginRow(Usa : Boolean = false);
    procedure EndRow;
    procedure BeginCell(Usa : Boolean = false);
    procedure EndCell;
    procedure BeginTableCaption;
    procedure EndTableCaption;
    procedure BeginTableHeader;
    procedure EndTableHeader;

    procedure SetFont(const Name: string; Color: TColor; Size: Integer; Style: TFontStyle);
    procedure SetTableProperties(aAlign : TEnumAlign;aNowRap: Boolean;aAlignH : TenumAlign);

    // Só salva se existe um nome
    // Troca a extensão do arquivo para o tipo determinado se o nome está definido
    procedure Save;

    procedure Clear;
    procedure Show(ClearBuffer: Boolean = True);
    procedure ShowHTML; virtual;
    procedure ShowTEXT; virtual;
    procedure InitDoc;
    procedure SaveDoc;

    {$ifdef prj_WinStat}
    procedure ShowTmp;
    procedure SaveTmp;
    {$endif}

    property DocType           : TEnumDocType      read FDocType        write FDocType;
    property FileName          : String            read FFileName       write FFileName;
    property Text              : TStrings          read FText           write FText;

    property HTML_Body         : THTML_Doc         read FHD;

    property Table_Properties : TTable_Properties   read FTP ;
    property Row_Properties   : TRow_Properties     read FRP ;
    property Cell_Properties  : TCell_Properties    read FCP ;

    property HtmlFontName  : string     read GetFontName  write SetFontName;
    property HtmlFontSize  : integer    read GetFontSize  write SetFontSize;
    property HtmlFontStyle : TFontStyle read GetFontStyle write SetFontStyle;
    property HtmlFontColor : TColor     read GetFontColor write SetFontColor;
    property HtmlFontAlign : TEnumAlign read GetFontAlign write SetFontAlign;

    property Append    : Boolean  read FAppend      write FAppend;
    property JaIniciou : Boolean  read FJaIniciou   write FJaIniciou;
  end;

implementation
uses Windows,
     {$ifdef prj_WinStat}
     Forms,
     wsConstTypes,
     {$endif}
     FileCtrl,
     jpeg,
     ShellAPI,
     SysUtilsEx,
     FileUtils
     {$ifdef NoForms}
        ;
     {$else}
        ,OutPut_HTML_DLG;
     {$endif}

function GetRGBColor(Color: TColor): String;
begin
  Result := intToHex(getRValue(Color), 2) +
            intToHex(getGValue(Color), 2) +
            intToHex(getBValue(Color), 2);
end;

{$ifdef prj_WinStat}

{ TPath }

//------------------------------------FUNCTION GETDATA------------------------\\
constructor TPath.Create;
  var DirApp : String;
      TmpPth : array[0..199] of char;
  begin
  DirApp  := ExtractFilePath(Application.ExeName);
  FData   := DirApp + 'Dados\';
  FTxt    := DirApp + 'DadosTXT\';
  FMatVet := DirApp + 'MatVet\';
  FJpgName:= 'img';
  GetTempPath(SizeOf(TmpPth), TmpPth);
  FTempPath := TmpPth;
end;

function TPath.GetData: String;
  begin
  GetData := FData;
end;
//------------------------------------FUNCTION GETMATVET----------------------\\
function TPath.GetMatVet: String;
  begin
  GetMatVet := FMatVet;
end;
//------------------------------------FUNCTION GETTXT-------------------------\\
function TPath.GetTxt: String;
  begin
  GetTxt := FTxt;
end;
//-----------------------------------PROCEDURE SETDATA------------------------\\
procedure TPath.SetData(Value: String);
  begin
  FData := Value;
end;
//-----------------------------------PROCEDURE SETMATVET----------------------\\
procedure TPath.SetMatVet(Value: String);
  begin
  FMatVet := Value;
end;
//----------------------------------PROCEDURE SETTXT--------------------------\\
procedure TPath.SetTxt(Value: String);
  begin
  FTxt := Value;
end;
{$endif}

{ THTML_Doc }

procedure THTML_Doc.BeginDoc(Text: TStrings);
begin
  Text.Add('<html>');
  Text.Add('<head>');
  Text.Add('  <title>');
  Text.Add('    ' + FTitle);
  Text.Add('  </title>');
  Text.Add('</head>');

  Text.Add('<body');
  if Background_Image <> '' then
     Text.Add(Format('  background= "%s"', [Background_Image]));

  if Background_Color <> -1 then
     Text.Add(Format('  bgcolor= "#%s"', [GetRGBColor(Background_Color)]));

  if Text_Color <> -1 then
     Text.Add(Format('  text= "#%s"', [GetRGBColor(Text_Color)]));

  if Link_Color <> -1 then
     Text.Add(Format('  link= "#%s"', [GetRGBColor(Link_Color)]));

  if Visited_Link_Color <> -1 then
     Text.Add(Format('  vlink= "#%s"', [GetRGBColor(Visited_Link_Color)]));

  if Active_Link_Color <> -1 then
     Text.Add(Format('  alink="#%s"', [GetRGBColor(Active_Link_Color)]));

  Text.Add('>');
end;

constructor THTML_Doc.Create;
begin
  inherited;

  FALC := -1;
  FTC  := -1;
  FLC  := -1;
  FVLC := -1;
  FBC  := -1;
end;

procedure THTML_Doc.EndDoc(Text: TStrings);
begin
  Text.Add('</body>');
  Text.Add('</HTML>');
end;

procedure THTML_Font.RetrieveProperties;
begin
 // <<<
end;

procedure THTML_Font.SaveProperties;
begin
  // <<<
end;

{ THTML_Font }

procedure THTML_Font.BeginFont(Text: TStrings);
begin
  Text.Add('<font');
  Text.Add(Format('   face  = "%s"'  , [FName]));
  Text.Add(Format('   size  = "%s"'  , [IntToStr(FSize)]));
  Text.Add(Format('   color = "#%s">', [GetRGBColor(FColor)]));

  if FAlign <> aLeft then
     begin
     Text.Add('<div');
     if FAlign = aRight then Text.Add('align = "right"');
     if FAlign = aCenter then Text.Add('align = "center"');
     end;

  if FItalic then Text.Add('<i>');
  if FBold then Text.Add('<b>');
end;

constructor THTML_Font.Create;
begin
  inherited;
  FSize   := 1;
  FName   := 'Courier New';
  FColor  := clBlack;
  FAlign  := aLeft;
end;

procedure THTML_Font.EndFont(Text: TStrings);
begin
  if FItalic then Text.Add('</i>');
  if FBold then Text.Add('</b>');
  if FAlign <> aLeft then Text.Add('</div>');
  Text.Add('</font>');
end;

{ TOutPut }

procedure TOutPut.BeginDoc;
begin
  {$ifdef prj_WinStat}
  FBuffer.Clear;
  {$endif}
  
  if not Append then FText.Clear;
  Case FDocType of
    dtHTML :  begin
              if (not Append) or ((Append) and (not JaIniciou)) then FHD.BeginDoc(FText);
              {$ifdef prj_WinStat}
              FHD.BeginDoc(FBuffer);
              {$endif}
              end;
  end; // case
end;

procedure TOutPut.BeginFont;
begin
  if FDocType = dtHTML then
     begin
     FHF.BeginFont(FText);
     {$ifdef prj_WinStat}
     FHF.BeginFont(FBuffer);
     {$endif}
     end;
end;

procedure TOutPut.BeginParagraph;
begin
  if FDocType = dtHTML then
     begin
     FText.Add('<P>');
     {$ifdef prj_WinStat}
     FBuffer.Add('<P>');
     {$endif}
     end;
end;

constructor TOutPut.Create(DocType: TEnumDocType = dtTEXT; const FileName: String = '');
begin
  inherited Create;

  {$ifdef prj_WinStat}
  FDocType  := ActOut;
  if FDocType = dtTEXT then
     FFileName := ExtractFilePath(Application.ExeName) + 'SaidaTxt\OutPut.txt';

  FHD       := THTML_Doc.Create;
  FHF       := THTML_Font.Create;
  FTP       := TTable_Properties.Create;
  FRP       := TRow_Properties.Create;
  FCP       := TCell_Properties.Create;
  FText     := TStringList.Create;

  FBuffer   := TStringList.Create;
  FFileName := ExtractFilePath(Application.ExeName) + 'SaidaHTML\OutPut.htm';
  FAppend   := False;

  EnumAlign[0] := 'Left';
  EnumAlign[1] := 'Right';
  EnumAlign[2] := 'Center';
  EnumAlign[3] := 'Middle';
  EnumAlign[4] := 'Bottom';
  EnumAlign[5] := 'BaseLine';
  {$else}
  FDocType := DocType;

  if FileName = '' then
     if FDocType = dtTEXT then
        FFileName := GetTempFile('', 'OutPut_', 'TXT')
     else
        FFileName := GetTempFile('', 'OutPut_', 'HTM')
  else
     FFileName := FileName;

  if FFileName <> '' then
     FPath := ExtractFilePath(FFileName);

  FHD       := THTML_Doc.Create;
  FHF       := THTML_Font.Create;
  FTP       := TTable_Properties.Create;
  FRP       := TRow_Properties.Create;
  FCP       := TCell_Properties.Create;
  FText     := TStringList.Create;
  {$endif}
end;

destructor TOutPut.Destroy;
begin
  FHD.Free;
  FHF.Free;
  FTP.Free;
  FRP.Free;
  FCP.Free;
  FText.Free;

  {$ifdef prj_WinStat}
  FBuffer.Free;
  {$endif}

  inherited;
end;

procedure TOutPut.EndDoc;
begin
  Case FDocType of
    dtHTML :  begin
              if not Append then FHD.EndDoc(FText);
              {$ifdef prj_WinStat}
              FHD.EndDoc(FBuffer);
              {$endif}
              end;
  end;
end;

procedure TOutPut.EndFont;
begin
  if FDocType = dtHTML then
    begin
    FHF.EndFont(FText);
    {$ifdef prj_WinStat}
    FHF.EndFont(FBuffer);
    {$endif}
  end;
end;

procedure TOutPut.EndParagraph;
begin
  if FDocType = dtHTML then
    begin
    FText.Add('</P>');
    {$ifdef prj_WinStat}
    FBuffer.Add('</P>');
    {$endif}
  end;
end;

function TOutPut.GetFontAlign: TEnumAlign;
begin
  result := FHF.Align;
end;

function TOutPut.GetFontColor: TColor;
begin
  result := FHF.Color;
end;

function TOutPut.GetFontName: string;
begin
  result := FHF.Name;
end;

function TOutPut.GetFontSize: integer;
begin
  result := FHF.Size ;
end;

function TOutPut.GetFontStyle: TFontStyle;
begin
  if FHF.Bold then
     if FHF.Italic then
        result := aBoldItalic
     else
        result := abold
  else
     if FHF.Italic then
        result := aItalic
     else
        result := nBoldItalic
end;

procedure TOutPut.SetFont(const Name: string; Color: TColor; Size: integer; Style: TFontStyle);
begin
  FHF.Name      := Name;
  FHF.Size      := Size;
  HtmlFontColor := Color;
  HtmlFontStyle := Style;
end;

procedure TOutPut.SetFontAlign(Align: TEnumAlign);
begin
  FHF.Align := Align;
end;

procedure TOutPut.SetFontColor(Color: TColor);
begin
  FHF.Color := Color;
end;

procedure TOutPut.SetFontName(const Name: string);
begin
  FHF.Name := Name;
end;

procedure TOutPut.SetFontSize(Size: integer);
begin
  FHF.Size := Size;
end;

procedure TOutPut.SetFontStyle(Style: TFontStyle);
begin
  if Style = aBoldItalic then
    begin
    FHF.Bold   := true;
    FHF.Italic := true;
    end
  else if Style = abold then
    begin
    FHF.Bold   := true;
    FHF.Italic := false;
    end
  else if Style = aItalic then
    begin
    FHF.Bold   := false;
    FHF.Italic := true;
    end
  else
    begin
    FHF.Bold   := false;
    FHF.Italic := false;
    end;
end;

procedure TOutPut.Save;
begin
  if Append then FHD.EndDoc(FText);

  if FFileName <> '' then
     FText.SaveToFile(FFileName);
  end;

(*
// Se o diretório do arquivo não existir, será criado um diretório dentro
// do diretorio de saidas.
procedure TOutPut.SetFileName(const Value: String);
begin
  {$ifdef prj_WinStat}
  FFileName := Value;
  FPath := ExtractFilePath(FFileName);
  if not DirectoryExists(FPath) then
     begin
     FPath := ExtractFilePath(Application.ExeName) + 'SaidaHTML\';
     FFileName := FPath + ExtractFileName(FFileName);
     end;
  {$else}
  FPath := ExtractFilePath(Value);
  if DirectoryExists(FPath) then
     FFileName := Value
  else
     Raise Exception.Create('Classe: TOutPut'#13 +
                            'Diretório Inválido !'#13 + FPath);
  {$endif}
end;
*)

procedure TOutPut.Show(ClearBuffer: Boolean = True);
begin
{$ifdef prj_WinStat}
  if FFileName <> '' then
    if FDocType = dtHTML then
       ShowHTML
    else
      if FDocType = dtTEXT then
         ShowTEXT
{$else}
  case FDocType of
    dtHTML: ShowHTML;
    dtTEXT: ShowTEXT;
  end;
  if ClearBuffer then Clear;
{$endif}
end;

procedure TOutPut.ShowHTML;
{$ifndef NoForms}
var d: TOutPutHTML_DLG;
{$endif}
begin
  Save;
{$ifdef NoForms}
  ShellExecute(MainInstance,
               'OPEN',
               pChar(FFileName),
               '',
               '',
               sw_SHOW);
{$else}
  d := TOutPutHTML_DLG.Create(nil);
  d.Show;
  d.Open(FFileName);
{$endif}
end;

procedure TOutPut.ShowTEXT;
begin
  ShellExecute(MainInstance,
               'OPEN',
               pChar(FFileName),
               '',
               '',
               sw_SHOW);
end;

procedure TOutPut.BeginTable;
begin
  if FDocType = dtHTML then
     begin
     Table_Properties.BeginTable(FText);
     {$ifdef prj_WinStat}
     Table_Properties.BeginTable(FBuffer);
     {$endif}
     end;
end;

procedure TOutPut.EndTable;
begin
  if FDocType = dtHTML then
     begin
     Table_Properties.EndTable(FText);
     {$ifdef prj_WinStat}
     Table_Properties.EndTable(FBuffer);
     {$endif}
     end;
end;

procedure TOutPut.BeginRow(Usa : Boolean = false);
begin
 if FDocType = dtHTML then
   if Usa then
     begin
     Row_Properties.BeginRow(FText);
     {$ifdef prj_WinStat}
     Row_Properties.BeginRow(FBuffer);
     {$endif}
     end
   else
     begin
     FText.Add('<TR>');
     {$ifdef prj_WinStat}
     FBuffer.Add('<TR>');
     {$endif}
     end;
end;

procedure TOutPut.EndRow;
begin
  if FDocType = dtHTML then
    begin
    FText.Add('</TR>');
    {$ifdef prj_WinStat}
    FBuffer.Add('</TD>');
    {$endif}
    end;
end;

procedure TOutPut.BeginCell(Usa : boolean = False);
begin
  if FDocType = dtHTML then
    begin
    if Usa then
      begin
      Cell_Properties.BeginCell(FText);
      {$ifdef prj_WinStat}
      Cell_Properties.BeginCell(FBuffer);
      {$endif}
      end
    else
      begin
      FText.Add('<TD');
      {$ifdef prj_WinStat} FBuffer.Add('<TD'); {$endif}

      if Cell_Properties.NowRap then
        begin
        FText.Add('NowRap');
        {$ifdef prj_WinStat} FBuffer.Add('NowRap'); {$endif}
        end;
      FText.Add('>');
      {$ifdef prj_WinStat} FBuffer.Add('>'); {$endif}
      end;
    end;
end;

procedure TOutPut.EndCell;
begin
  if FDocType = dtHTML then
    begin
    FText.Add('</TD>');
    {$ifdef prj_WinStat} FBuffer.Add('</TD>'); {$endif}
    end;
end;

procedure TOutPut.BeginTableCaption;
begin
  if FDocType = dtHTML then
    begin
    Table_Properties.BeginCaption(FText);
    {$ifdef prj_WinStat} Table_Properties.BeginCaption(FBuffer); {$endif}
    end;
end;

procedure TOutPut.EndTableCaption;
begin
  if FDocType = dtHTML then
    begin
    Table_Properties.EndCaption(FText);
    {$ifdef prj_WinStat} Table_Properties.EndCaption(FBuffer); {$endif}
    end;
end;

procedure TOutPut.BeginTableHeader;
begin
  if FDocType = dtHTML then
    begin
    Table_Properties.BeginH(FText);
    {$ifdef prj_WinStat} Table_Properties.BeginH(FBuffer); {$endif}
    end;
end;

procedure TOutPut.EndTableHeader;
begin
  if FDocType = dtHTML then
    begin
    Table_Properties.EndH(FText);
    {$ifdef prj_WinStat} Table_Properties.EndH(FBuffer); {$endif}
    end;
end;

procedure TOutPut.Write(Image: TGraphic);
var s   : String;
    jpg : TJPEGImage;
begin
  if FDocType <> dtHTML then Exit;

  {$ifdef prj_WinStat}
  //s := getTempFile(ExtractFilePath(Application.ExeName) + 'SaidaHTML\JPG\', gPath.JpgName, 'jpg'); <<<<<
  {$else}
  s := getTempFile(FPath, 'img', 'jpg');
  {$endif}

  jpg := TJPEGImage.Create;
  try
    jpg.Assign(Image);
    jpg.CompressionQuality := 100;
    jpg.SaveToFile(s);
  finally
    jpg.Free;
  end;

  FText.Add('<img');
  FText.Add(Format('  src="%s"'    , [s]));
  FText.Add(Format('  width="%d" ' , [Image.Width]));
  FText.Add(Format('  height="%d" ', [Image.Height]));
  FText.Add('  border="0"');
  FText.Add('>');

  {$ifdef prj_WinStat}
  FBuffer.Add('<img');
  FBuffer.Add(Format('  src="%s"'    , [s]));
  FBuffer.Add(Format('  width="%d" ' , [Image.Width]));
  FBuffer.Add(Format('  height="%d" ', [Image.Height]));
  FBuffer.Add('  border="0"');
  FBuffer.Add('>');
  {$endif}
end;

procedure TOutPut.Write(const s: String = ''; NewLine: Boolean = True);
begin
  if FDocType = dtHTML then
    begin
    if NewLine then FText.Add(s + '<BR>') else FText.Add(s);
    {$ifdef prj_WinStat} FBuffer.Add(s + '<BR>'); {$endif}
    end
  else
    if FDocType = dtTEXT then
      begin
      if NewLine or (FText.Count = 0) then
         FText.Add(s)
      else
         FText[FText.Count-1] := FText[FText.Count-1] + s;
      {$ifdef prj_WinStat} FBuffer.Add(s); {$endif}
      end;
end;

procedure TOutPut.WriteCenter(const s: String; Len: Word=110);
begin
  if FDocType = dtHTML then
     begin
     BeginFont;
       FText.Add('<center>');
       FText.Add(s + '<BR>');
       FText.Add('</center>');

       {$ifdef prj_WinStat}
       FBuffer.Add('<center>');
       FBuffer.Add(s + '<BR>');
       FBuffer.Add('</center>');
       {$endif}
     EndFont;
     end
  else
    if FDocType = dtTEXT then
      begin
      FText.Add(strCenter(s, Len));
      {$ifdef prj_WinStat} FBuffer.Add(strCenter(s, Len)); {$endif}
      end;
end;

{ Objetivo
    Escreve uma sequência de caracteres no objeto de saída
  Parâmetros
    s     : Sequência de caracteres para saída
    aColor: Cor desejada para os caracteres. Default: azul (clBlue)
    aSize : Tamanho dos caracteres. Default: 1
    aCap  : True se for título; false caso contrário. Default: false
}
procedure TOutPut.WriteTitle(const s:String; aColor:TColor=clBlue; aSize:byte=1; aCap:Boolean=False);
var Cor: TColor;
    tc : byte;
begin
  HtmlFontStyle := nBoldItalic;
  Cor := HTMLFontColor;
  tc := HtmlFontSize;
  HTMLFontColor := aColor;
  HtmlFontSize := aSize;
  if aCap then
    begin
    BeginTableCaption;
    BeginFont;
    Write(S);
    EndFont;
    EndTableCaption;
    end
  else
    begin
    BeginFont;
    WriteCenter(s);
    EndFont;
    end;
  HTMLFontColor:=Cor;
  HtmlFontSize := tc;
end;

{ TTABLE PROPERTIES}
constructor TTable_Properties.Create;
begin
  Align        := aCenter;
  Border       :=       1;
  CellPadding  :=       1;
  CellSpacing  :=       1;
  Width        :=       1;
  AlignCaption := aCenter;
end;

procedure TTable_Properties.BeginTable(Text : TStrings);
begin
  Text.Add('<Table');
  Text.Add(Format('Align       = "%s"',[EnumAlign[byte(Align)]]));
  Text.Add(Format('Border      = "%d"',[Border]));
  Text.Add(Format('CellPadding = "%d"',[CellPadding]));
  Text.Add(Format('CellSpacing = "%d"',[CellSpacing]));
  Text.Add(Format('Width       = "%d"',[Width]));
  Text.Add('>');
end;

procedure TTable_Properties.EndTable(Text : Tstrings);
begin
  Text.Add('</Table>');
end;

procedure TTable_Properties.BeginCaption(Text : TStrings);
begin
  Text.Add(Format('<CAPTION ALIGN = "%s">',[EnumAlign[byte(AlignCaption)]]));
end;

procedure TTable_Properties.EndCaption(Text : TStrings);
begin
  Text.Add('</CAPTION>');
end;

 {ROW PROPERTIES}

procedure TTable_Properties.BeginH(Text : TStrings);
begin
  Text.Add('<TH>');
end;

procedure TTable_Properties.EndH(Text : Tstrings);
begin
  Text.Add('</TH>');
end;

constructor TRow_Properties.Create;
begin
  inherited;
  //Align   := aCenter;
  //VAlign  := aMiddle;
end;

procedure TRow_Properties.BeginRow(Text : TStrings);
begin
  Text.Add('<TR>');
end;

  {CELL PROPERTIES}

constructor TCell_Properties.Create;
begin
  Align   := aCenter;
  ColSpan := 1;
  Height  := 1;
  NowRap  := true;
  RowSpan := 1;
  VAlign  := aMiddle;
  Width   := 5;
end;

procedure TCell_Properties.BeginCell(Text : TStrings);
begin
  Text.Add('<TD');
  Text.Add(Format('  Align = "%s"',[EnumAlign[byte(Align)]])) ;
  //Text.Add(Format('ColSpan = %d',[ColSpan]));
  //Text.Add(Format('Height = %d',[Height]));
  //Text.Add(Format('VAlign = %s%s%s',[chr(39),EnumAlign[byte(VAlign)],chr(39)]));
  //Text.Add(Format('RowSpan = %d',[RowSpan]));
  if NowRap then Text.Add('  NowRap');
  Text.Add('>');
end;

procedure TOutPut.Warning(Aviso: string);
var s1 : string;
    j  : cardinal;
    ch : tcharset;
begin
  HtmlFontColor := clred;
  BeginFont;
  Write('AVISO: ');
  HtmlFontColor := clblack;
  ch := ['/'];
  j := 1;
  s1 := StrToken(Aviso,j,ch);
  Write(s1);
  s1 := StrToken(Aviso,j,ch);
  while s1 <> '' do
    begin
    s1  := '.......' + s1;
    Write(s1);
    s1 := StrToken(Aviso,j,ch);
    end;
  EndFont;
end;

procedure TOutPut.InitDoc;
begin
  if FDocType = dtHTML then BeginDoc;
end;

procedure TOutPut.SaveDoc;
begin
  if DocType = dtHTML then
    begin
    EndDoc;
    Save;
    end;

  Show;
end;

{$ifdef prj_WinStat}
procedure TOutPut.ShowTmp;
var d: TOutPutHTML_DLG;
begin
  if FFileName <> '' then
     if FDocType = dtHTML then
        begin
        d := TOutPutHTML_DLG.Create(Application);
//        d.Open(gPath.TempPath + HTMTMPFILE);
        d.Show;
        end
     else
        Show;
end;

procedure TOutPut.SaveTmp;
begin
  if DocType = dtHTML then
     begin
     EndDoc;
     if Append then FHD.EndDoc(FText);
     if FFileName <> '' then
//        FBuffer.SaveToFile(gPath.TempPath + HTMTMPFILE);
     end
  else
     if FFileName <> '' then
//        FBuffer.SaveToFile(gPath.TempPath + TXTTMPFILE); <<<<<

  ShowTmp;
end;
{$endif}

procedure TOutPut.WriteLine(N: integer = 1);
var
  i : integer;
  st: String[4];
begin
  if FDocType = dtHTML then
     st := '<BR>'
  else
     st := '';

  for i := 1 to N do Write(st);
end;

procedure TOutPut.WriteLink(const Text, Link: String; NewLine: Boolean = True);
var s: String[4];
begin
  if NewLine then s := '<BR>' else s := '';
  if FDocType = dtHTML then
     Write('<a href = "' + Link + '">' + Text + '</a>' + s);
end;

function TOutPut.GetAppend: Boolean;
begin
  GetAppend := FAppend;
end;
(*
procedure TOutPut.SetAppend(Value: Boolean);
begin
  FAppend := Value;
end;
*)
procedure TOutPut.Clear;
begin
  FText.Clear;
end;

procedure TOutPut.Write(const Text: String; Color: TColor;
                        Size: Integer; Center, Bold, Italic: Boolean;
                        NewLine: Boolean = True; const FontName: String = 'ARIAL');
begin
  FHF.SaveProperties;

  HtmlFontName   := FontName;
  HtmlFontSize   := Size;
  HtmlFontColor  := Color;

  if Center then
     HtmlFontAlign := aCenter
  else
     HtmlFontAlign := aLeft;

  if italic and bold then
     HtmlFontStyle := aBoldItalic
  else
     begin
     if bold then
        HtmlFontStyle := abold;

     if italic then
        HtmlFontStyle := aItalic;
     end;

  BeginFont;
    Write(Text, NewLine);
  EndFont;

  FHF.RetrieveProperties;
end;

procedure TOutPut.SetTableProperties(aAlign: TEnumAlign; aNowRap: Boolean; aAlignH: TenumAlign);
begin
  Table_Properties.SetProperties(aAlign, aNowRap, aAlignH);
end;

procedure TOutPut.WriteSeparator;
begin
  Write('<HR>');
end;

procedure TOutPut.WriteSpaces(N: Integer);
var s: String;
    i: Integer;
begin
  if FDocType = dtHTML then
     begin
     s := '';
     for i := 1 to N do s := s + '&nbsp;';
     end
  else
     s := StringOfChar(' ', N);
  FText.Add(s);
end;

procedure TOutPut.Write(SL: TStrings);
var i: Integer;
begin
  for i := 0 to SL.Count-1 do
    Write(SL[i]);
end;

procedure TOutPut.WriteBold(const s: String; NewLine: Boolean);
var ss: String;
begin
  ss := '<b>' + s + '</b>';
  if NewLine then ss := ss + '<BR>';
  FText.Add(ss);
end;

procedure TOutPut.WriteItalic(const s: String; NewLine: Boolean);
var ss: String;
begin
  ss := '<i>' + s + '</i>';
  if NewLine then ss := ss + '<BR>';
  FText.Add(ss);
end;

procedure TOutPut.WriteBI(const s: String; NewLine: Boolean);
var ss: String;
begin
  ss := '<b><i>' + s + '</i></b>';
  if NewLine then ss := ss + '<BR>';
  FText.Add(ss);
end;

{ TTable_Properties }

procedure TTable_Properties.SetProperties(aAlign: TEnumAlign;
                                          aNowRap: Boolean; aAlignH: TenumAlign);
begin
  FAlign  := aAlign;
  FAlignH := aAlignH;
end;

end.

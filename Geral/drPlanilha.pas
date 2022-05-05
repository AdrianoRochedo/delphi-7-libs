unit drPlanilha;

{ Como Utilizar este componente em programas ?

    Adicione ao Path do Delphi ou ao seu projeto os caminhos:
      - x:\xxx\Comum\Lib
      - x:\xxx\Comum\WinStat_Lib

    Declare na "unit" que você está utilizando a unidade "drPlanilha"

    Declare uma variável ou campo de um objeto do tipo "TPlanilha"

    Crie uma instância de TPlanilha. (Não esqueça de destruí-la ao final)
      Ex:
          p := TPlanilha.Create;
          p.Write(1, 1, 'célula 1,1');
          p.Show;
          p.SaveToFile('c:\teste.xsl');
          p.LoadFromFile('c:\teste.xsl');
          ...
          p.Free;

      O melhor lugar para se criar instâncias de campos privados é no
      constructor da classe possuidora ou no evento "OnCreate" de uma
      classe descendente de "TForm". E o melhor lugar para destruição
      é no destructor ou no evento "OnDestroy" de um descendente de "TForm"
      respectivamente.

    Para saber o que cada método/propriedade realiza olhe a descrição de
    cada um abaixo.

  Como utilizar seu programa em outros computadores ?

    Como este componente de planilha é um objeto OCX, ele precisa ser
    registrado no Windows da máquina cliente senão ao tenter executar o
    programa você receberá a mensagem "Classe não Registrada".

    Como registrar meus componentes OCX ?

      Utilizando um Instalador:

        Crie um "Instalador" para seu programa certificando-se que ele
        irá instalar todos os arquivos necessários. O registro será feito
        automaticamente pelo instalador.

      Fazendo a instalação manual:

        Copie seu programa e todos os arquivos que ele necessita para
        um diretório a sua escolha.

        Verifique no manual de seu componente OCX quais arquivos fazem
        parte dele e copie todos para o diretório do Windows. No caso deste
        componente precisamos copiar os seguintes arquivos:

          - Mfcans32.dll
          - oc30.dll
          - vcf132.ocx

        Na linha de comando de um console DOS registre seu componente
        através do comando:

          regsvr32.exe vcf132.ocx

        Isto criará as entradas no registro do Windows.

        Para removê-lo utilize:

          regsvr32.exe /u vcf132.ocx

      Como você verificou é muito mais fácil utilizar um instalador.
      No Delphi o InstallShield Express é fornecido para este tipo de tarefa.
      Ele precisa ser instalado separadamente.

  -----------------------------------------------------------------------------

  Utilizando o componente de planilha diretamente

    Em um formulário descendente de "TForm" coloque um componente do tipo
    TF1Book (Delphi - paleta ActiveX - Componente F1Book).

    Utilize o Help "VCF1.HLP" localizado na pasta:
      dir. Principal do Delphi\Ocx\Vci\Form1
      
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AxCtrls, OleCtrls, vcf1, Menus, wsVec, DiretivasDeCompilacao;

type
  TPlanilha = class;

  TfoPlanilha = class(TForm)
    X: TF1Book;
    Menu: TPopupMenu;
    Menu_Copiar: TMenuItem;
    Menu_Colar: TMenuItem;
    Menu_Recortar: TMenuItem;
    N1: TMenuItem;
    Menu_Abrir: TMenuItem;
    Menu_Salvar: TMenuItem;
    N2: TMenuItem;
    Menu_Imprimir: TMenuItem;
    Save: TSaveDialog;
    Load: TOpenDialog;
    N3: TMenuItem;
    Menu_Destruir: TMenuItem;
    Menu_SempreNoTopo: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Menu_ColarClick(Sender: TObject);
    procedure Menu_RecortarClick(Sender: TObject);
    procedure Menu_AbrirClick(Sender: TObject);
    procedure Menu_SalvarClick(Sender: TObject);
    procedure Menu_ImprimirClick(Sender: TObject);
    procedure XKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Menu_DestruirClick(Sender: TObject);
    procedure Menu_SempreNoTopoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPlanilha : TPlanilha;
    FDestruir : Boolean;
  public
    property Destruir : Boolean read FDestruir write FDestruir;
  end;

  TPlanilha = class
  private
    FForm: TfoPlanilha;
    function getCaption: String;
    procedure SetCaption(const Value: String);
    function GetColText(i: integer): String;
    function GetRowText(i: integer): String;
    procedure SetColText(i: integer; const Value: String);
    procedure SetRowText(i: integer; const Value: String);
    function getTopLeftText: String;
    procedure SetTopLeftText(const Value: String);
    function getnCols: Integer;
    function getnRows: Integer;
    procedure setnCols(const Value: Integer);
    procedure setnRows(const Value: Integer);
    function GetColWidth(i: integer): Integer;
    procedure SetColWidth(i: integer; const Value: Integer);
    function getSCH: Boolean;
    function getSRH: Boolean;
    procedure setSCH(const Value: Boolean);
    procedure setSRH(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // métodos de escrita
    procedure Write(Row, Col: Integer; const s: String); overload;
    procedure Write(Row, Col: Integer; const r: Real; DecimalPlaces: byte = 2); overload;
    procedure WriteCenter(Row, Col: Integer; const s: String); overload;
    procedure WriteCenter(Row, Col: Integer; const r: Real); overload;
    procedure WriteVecInCol(V: TwsVec; Col, IniRow: Integer; DecimalPlaces: byte = 2);
    procedure WriteVecInRow(V: TwsVec; Row, IniCol: Integer; DecimalPlaces: byte = 2);

    // métodos para obtenção de valores
    // Retornam o valor de uma célula
    function GetEntry (Row, Col: Integer): String;
    function GetText  (Row, Col: Integer): String;
    function GetFloat (Row, Col: Integer): Real;

    // Transcrevem um vetor para uma linha/coluna dada uma posição inicial
    function ColToVec (Col, BeginRow, EndRow: Integer): TwsVec; overload;
    function ColToVec (Col, BeginRow: Integer): TwsVec; overload;
    function RowToVec (Row, BeginCol, EndCol: Integer): TwsVec; overload;
    function RowToVec (Row, BeginCol: Integer): TwsVec; overload;

    // métodos para formatação de células
    procedure SetDefaultFont(const Name: String; Size: Integer);
    procedure SetCellFont(Row, Col       : Integer;
                          const FontName : String;
                          Size           : Smallint;
                          Color          : TColor;
                          Bold           : Boolean = False;
                          Italic         : Boolean = False;
                          Underline      : Boolean = False);

    // clipboard
    procedure Cut;
    procedure Copy;
    procedure Paste;

    // métodos gerais
    procedure Clear;
    procedure Print(ShowDialog: Boolean = False);
    procedure Prepare;
    procedure Show(FormStyle: TFormStyle = fsNormal);
    procedure ShowModal;

    // leitura e salvamento
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile  (const FileName: String; const FileType: String = 'XLS'); // XLS ou TXT

    // Cabeçalhos de linhas e colunas da Planilha
    property TopLeftText         : String  read getTopLeftText write setTopLeftText;
    property RowText[i: integer] : String  read getRowText     write setRowText;
    property ColText[i: integer] : String  read getColText     write setColText;
    property ShowColHeading      : Boolean read getSCH         write setSCH;
    property ShowRowHeading      : Boolean read getSRH         write setSRH;

    // Titulo da janela
    property Caption: String read getCaption write SetCaption;

    // Número de Colunas e Linhas com valores válidos
    property nCols : Integer read getnCols write setnCols;
    property nRows : Integer read getnRows write setnRows;

    // Tamanho de uma coluna
    //property ColWidth[i: integer] : Integer read GetColWidth write SetColWidth;

    // Acessa diretamente a janela que contém a planilha
    property Window : TfoPlanilha read FForm;
  end;

  procedure SetCellsFont(P              : Tf1Book;
                         Cells          : Array of Integer;
                         const FontName : String;
                         Size           : Smallint;
                         Color          : TColor;
                         Bold           : Boolean = False;
                         Italic         : Boolean = False;
                         Underline      : Boolean = False);

implementation
uses MessageManager,
     WindowsManagerMessages;

{$R *.DFM}

procedure SetCellsFont(P              : Tf1Book;
                       Cells          : Array of Integer;
                       const FontName : String;
                       Size           : Smallint;
                       Color          : TColor;
                       Bold           : Boolean = False;
                       Italic         : Boolean = False;
                       Underline      : Boolean = False);
var i: Integer;
    R, C: Integer;
begin
  i := 0;
  Repeat
    R := Cells[i];
    C := Cells[i+1];
    P.SetActiveCell(R, C);
    P.SetFont(FontName, Size, Bold, Italic, Underline, False, Color, False, False);
    inc(i, 2);
  until i >= Length(Cells);
end;

{ TfoPlanilha }

procedure TfoPlanilha.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FDestruir then
     begin
     Action := caFree;
     GetMessageManager.SendMessage(WINMANAGER_REMOVE, [self]);
     end
  else
     Action := caHide;
end;

procedure TfoPlanilha.Menu_CopiarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  FPlanilha.Copy;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TfoPlanilha.Menu_ColarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  FPlanilha.Paste;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TfoPlanilha.Menu_RecortarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  FPlanilha.Cut;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TfoPlanilha.Menu_AbrirClick(Sender: TObject);
begin
  try
    if Load.Execute Then
       begin
       Screen.Cursor := crHourGlass;
       FPlanilha.LoadFromFile(Load.FileName);
       end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfoPlanilha.Menu_SalvarClick(Sender: TObject);
begin
  if Save.Execute then
     Case Save.FilterIndex of
       1 : FPlanilha.SaveToFile(Save.FileName, 'TXT');
       2 : FPlanilha.SaveToFile(Save.FileName, 'XLS');
       end;
end;

procedure TfoPlanilha.Menu_ImprimirClick(Sender: TObject);
begin
  FPlanilha.Print(True);
end;

procedure TfoPlanilha.XKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var c: Char;
begin
  if (Shift = [ssCtrl]) then
     begin
     c := upCase(char(Key));
     case c of
       'C': FPlanilha.Copy;
       'V': FPlanilha.Paste;
       'X': FPlanilha.Cut;
       'P': FPlanilha.Print(True);
       end;
     end;
end;

{ TPlanilha }

constructor TPlanilha.Create;
begin
  inherited Create;
  FForm := TfoPlanilha.Create(Application);
  FForm.FPlanilha := self;
  Prepare;
  GetMessageManager.SendMessage(WINMANAGER_ADD, [FForm]);
end;

destructor TPlanilha.Destroy;
begin
  GetMessageManager.SendMessage(WINMANAGER_REMOVE, [FForm]);
  if FForm <> nil then
     begin
     FForm.FPlanilha := nil;
     FForm.Free;
     end;
  inherited;
end;

procedure TPlanilha.Write(Row, Col: Integer; const s: String);
begin
  FForm.x.TextRC[Row, Col] := s;
end;

procedure TPlanilha.SetCellFont(Row, Col: Integer; const FontName: String;
  Size: Smallint; Color: TColor; Bold, Italic, Underline: Boolean);
begin
  FForm.x.SetActiveCell(Row, Col);
  FForm.x.SetFont(FontName, Size, Bold, Italic, Underline, False, Color, False, False);
end;

procedure TPlanilha.Write(Row, Col: Integer; const r: Real; DecimalPlaces: byte = 2);
begin
  if DecimalPlaces > 0 then
     begin
     FForm.x.SetActiveCell(Row, Col);
     FForm.X.NumberFormat := '#' + DecimalSeparator + StringOfChar('#', DecimalPlaces);
     FForm.x.Number := r;
     end
  else
     FForm.x.NumberRC[Row, Col] := r;
end;

procedure TPlanilha.WriteCenter(Row, Col: Integer; const s: String);
begin
  FForm.x.SetActiveCell(Row, Col);
  FForm.x.SetAlignment(F1HAlignLeft, False, F1VAlignCenter, 0);
  FForm.x.TextRC[Row, Col] := s;
end;

procedure TPlanilha.WriteCenter(Row, Col: Integer; const r: Real);
begin
  FForm.x.SetActiveCell(Row, Col);
  FForm.x.SetAlignment(F1HAlignLeft, False, F1VAlignCenter, 0);
  FForm.x.NumberRC[Row, Col] := r;
end;

procedure TPlanilha.Cut;
begin
  {$IF dir_NivelDeRestricao < 1}
  FForm.x.EditCut;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TPlanilha.Copy;
begin
  {$IF dir_NivelDeRestricao < 1}
  FForm.x.EditCopy;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TPlanilha.Paste;
begin
  {$IF dir_NivelDeRestricao < 1}
  FForm.x.EditPaste;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TPlanilha.Clear;
begin
  FForm.x.ClearRange(1, 1, FForm.x.MaxRow, FForm.x.MaxCol, F1ClearAll);
end;

procedure TPlanilha.LoadFromFile(const FileName: String);
var FT: SmallInt;
begin
  FForm.x.Read(FileName, FT);
end;

procedure TPlanilha.SaveToFile(const FileName, FileType: String);
begin
  {$IF dir_NivelDeRestricao < 2}
  if CompareText(FileType, 'TXT') = 0 then
     FForm.x.Write(FileName, F1FileTabbedText)
  else
     FForm.x.Write(FileName, F1FileExcel5);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TPlanilha.Prepare();
begin
  FForm.Left := -3000;
  FForm.Top  := -3000;
  FForm.Show;
end;

procedure TPlanilha.Print(ShowDialog: Boolean);
begin
  FForm.x.FilePrint(ShowDialog);
end;

procedure TPlanilha.Show(FormStyle: TFormStyle = fsNormal);
begin
  FForm.Top  := 100;
  FForm.Left := 100;
  if FormStyle = fsMDIChild then
     begin
     FForm.FormStyle := FormStyle;
     FForm.SetBounds(100, 100, 400, 300);
     end
  else
     FForm.Show;
end;

procedure TPlanilha.ShowModal;
begin
  FForm.Hide;
  FForm.Top  := 100;
  FForm.Left := 100;
  FForm.ShowModal;
end;

procedure TPlanilha.WriteVecInCol(V: TwsVec; Col, IniRow: Integer; DecimalPlaces: byte);
var i: Integer;
    x: Double;
begin
  dec(IniRow);
  for i := 1 to V.Len do
    if not V.IsMissValue(i, x) then
       Write(IniRow + i, Col, x, DecimalPlaces);
end;

procedure TPlanilha.WriteVecInRow(V: TwsVec; Row, IniCol: Integer; DecimalPlaces: byte = 2);
var i: Integer;
    x: Double;
begin
  dec(IniCol);
  for i := 1 to V.Len do
    if not V.IsMissValue(i, x) then
       Write(Row, IniCol + i, x, DecimalPlaces);
end;

procedure TfoPlanilha.Menu_DestruirClick(Sender: TObject);
begin
  Menu_Destruir.Checked := not Menu_Destruir.Checked;
  FDestruir := Menu_Destruir.Checked;
end;

procedure TfoPlanilha.Menu_SempreNoTopoClick(Sender: TObject);
begin
  Menu_SempreNoTopo.Checked := not Menu_SempreNoTopo.Checked;
  if Menu_SempreNoTopo.Checked then
     self.FormStyle := fsStayOnTop
  else
     self.FormStyle := fsNormal;
end;

procedure TfoPlanilha.FormDestroy(Sender: TObject);
begin
  if FPlanilha <> nil then
     begin
     FPlanilha.FForm := nil;
     FreeAndNil(FPlanilha);
     end;
end;

procedure TfoPlanilha.FormCreate(Sender: TObject);
begin
  FDestruir := True;
end;

function TPlanilha.getCaption: String;
begin
  Result := FForm.Caption;
end;

procedure TPlanilha.SetCaption(const Value: String);
begin
  FForm.Caption := Value;
end;

procedure TPlanilha.SetDefaultFont(const Name: String; Size: Integer);
begin
  FForm.x.SetDefaultFont(Name, Size);
end;

function TPlanilha.GetColText(i: integer): String;
begin
  Result := FForm.X.ColText[i];
end;

function TPlanilha.GetRowText(i: integer): String;
begin
  Result := FForm.X.RowText[i];
end;

procedure TPlanilha.SetColText(i: integer; const Value: String);
begin
  FForm.X.ColText[i] := Value;
end;

procedure TPlanilha.SetRowText(i: integer; const Value: String);
begin
  FForm.X.RowText[i] := Value;
end;

function TPlanilha.getTopLeftText: String;
begin
  Result := FForm.x.TopLeftText;
end;

procedure TPlanilha.SetTopLeftText(const Value: String);
begin
  FForm.x.TopLeftText := Value;
end;

function TPlanilha.GetFloat(Row, Col: Integer): Real;
begin
  Result := FForm.X.NumberRC[Row, Col];
end;

function TPlanilha.GetText(Row, Col: Integer): String;
begin
  Result := FForm.X.TextRC[Row, Col];
end;

function TPlanilha.GetEntry(Row, Col: Integer): String;
begin
  Result := FForm.X.EntryRC[Row, Col];
end;

function TPlanilha.getnCols: Integer;
begin
  Result := FForm.x.MaxCol;
end;

function TPlanilha.getnRows: Integer;
begin
  Result := FForm.x.MaxRow;
end;

procedure TPlanilha.setnCols(const Value: Integer);
begin
  FForm.x.MaxCol := Value;
end;

procedure TPlanilha.setnRows(const Value: Integer);
begin
  FForm.x.MaxRow := Value;
end;

function TPlanilha.ColToVec(Col, BeginRow, EndRow: Integer): TwsVec;
var k, i: Integer;
begin
  k := 0;
  Result := TwsDFVec.Create(EndRow - BeginRow + 1);
  for i := BeginRow to EndRow do
    begin
    inc(k);
    Result[k] := FForm.x.NumberRC[i, Col];
    end
end;

function TPlanilha.RowToVec(Row, BeginCol, EndCol: Integer): TwsVec;
var k, i: Integer;
begin
  k := 0;
  Result := TwsDFVec.Create(EndCol - BeginCol + 1);
  for i := BeginCol to EndCol do
    begin
    inc(k);
    Result[k] := FForm.x.NumberRC[Row, i];
    end
end;

function TPlanilha.ColToVec(Col, BeginRow: Integer): TwsVec;
var i: Integer;
    EndRow: Integer;
begin
  // Descobre qual a última linha com um valor
  for i := BeginRow to FForm.x.LastRow do
    if FForm.x.TextRC[i, Col] = '' then
       begin
       EndRow := i - 1;
       Break;
       end;

  Result := ColToVec(Col, BeginRow, EndRow);
end;

function TPlanilha.RowToVec(Row, BeginCol: Integer): TwsVec;
begin
  Result := RowToVec(Row, BeginCol, FForm.x.LastColForRow[Row]);
end;

function TPlanilha.GetColWidth(i: integer): Integer;
begin
  Result := FForm.X.ColWidth[i];
end;

procedure TPlanilha.SetColWidth(i: integer; const Value: Integer);
begin
  FForm.X.ColWidth[i] := Value;
end;

function TPlanilha.getSCH: Boolean;
begin
  Result := FForm.X.ShowColHeading;
end;

function TPlanilha.getSRH: Boolean;
begin
  Result := FForm.X.ShowRowHeading;
end;

procedure TPlanilha.setSCH(const Value: Boolean);
begin
  FForm.X.ShowColHeading := Value;
end;

procedure TPlanilha.setSRH(const Value: Boolean);
begin
  FForm.X.ShowRowHeading := Value;
end;

end.

unit SpreadSheetBook_Frame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  cxControls, cxSSheet, cxSSTypes, cxSSUtils, cxSSRes, cxExcelAccess,
  Book_Interfaces,
  SheetWrapper,
  DiretivasDeCompilacao;

type
  TStyleValue = (svAlign, svFontName, svSize, svBold, svItalic, svUnderline, svStrikeOut, svNull);
  TStyleValueSet = set of TStyleValue;

  TSpreadSheetBookFrame = class(TFrame, IBookPage, IPage, ISheetPage)
    SS: TcxSpreadSheetBook;
    Menu: TPopupMenu;
    Menu_Copiar: TMenuItem;
    Menu_Colar: TMenuItem;
    Menu_Recortar: TMenuItem;
    N1: TMenuItem;
    Menu_Abrir: TMenuItem;
    Menu_Salvar: TMenuItem;
    Menu_Destruir: TMenuItem;
    Menu_SempreNoTopo: TMenuItem;
    Save: TSaveDialog;
    Load: TOpenDialog;
    N3: TMenuItem;
    Menu_FormatCells: TMenuItem;
    Menu_CopiarTudo: TMenuItem;
    N4: TMenuItem;
    Menu_AjustarTexto: TMenuItem;
    Menu_Esquerda: TMenuItem;
    Menu_Centro: TMenuItem;
    Menu_Direita: TMenuItem;
    Menu_Editar: TMenuItem;
    Menu_Arquivo: TMenuItem;
    Menu_Estilo: TMenuItem;
    Menu_Negrito: TMenuItem;
    Menu_Italico: TMenuItem;
    Menu_NenhumEstilo: TMenuItem;
    Menu_Sublinhado: TMenuItem;
    Menu_Transpor: TMenuItem;
    procedure SSActiveSheetChanging(Sender: TcxCustomSpreadSheetBook; const ActiveSheet: Integer; var CanSelect: Boolean);
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Menu_ColarClick(Sender: TObject);
    procedure Menu_RecortarClick(Sender: TObject);
    procedure Menu_AbrirClick(Sender: TObject);
    procedure Menu_SalvarClick(Sender: TObject);
    procedure Menu_DestruirClick(Sender: TObject);
    procedure Menu_SempreNoTopoClick(Sender: TObject);
    procedure Menu_FormatCellsClick(Sender: TObject);
    procedure Menu_CopiarTudoClick(Sender: TObject);
    procedure Menu_EsquerdaClick(Sender: TObject);
    procedure Menu_CentroClick(Sender: TObject);
    procedure Menu_DireitaClick(Sender: TObject);
    procedure Menu_NegritoClick(Sender: TObject);
    procedure Menu_ItalicoClick(Sender: TObject);
    procedure Menu_SublinhadoClick(Sender: TObject);
    procedure Menu_NenhumEstiloClick(Sender: TObject);
    procedure Menu_TransporClick(Sender: TObject);
  private
    FSSW: TSheet;
    FFreeForm: boolean;

    function getActiveSheetIndex: integer;
    function getSheetCount: integer;
    procedure setActiveSheetIndex(const Value: integer);
    function getControl(): TWinControl;

    // IPage interface
    procedure setReadOnly(b: boolean);
    procedure setFontName(const Name: string);
    procedure setFontColor(const Color: TColor);
    procedure setFontSize(const Size: integer);
    procedure Write(row, col: integer; const s: string);

    procedure SetTokenStyle(AStyleValue: TStyleValue; AFontStyle: TFontStyle);
    procedure SetCellsStyle(AValuesSet: TStyleValueSet; AAlign: TcxHorzTextAlign;
      AFontSize: Integer; const AFontName: string; AStyles: TFontStyles);
  public
    constructor Create(AOwner: TComponent); override;

    // leitura e salvamento
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile  (const FileName: String);

    // Folhas
    procedure NewSheet(const aPageName: string = '');

    // Atualizacao
    procedure BeginUpdate();
    procedure EndUpdate();

    // Folhas
    property ActiveSheet      : TSheet read FSSW;
    property ActiveSheetIndex : integer read getActiveSheetIndex write setActiveSheetIndex;
    property SheetCount       : integer read getSheetCount;

    // Se verdadeiro, o formulario pai será destruído ao ser fechado
    property FreeForm : boolean read FFreeForm write FFreeForm;
  end;

  TFactory = class(TInterfacedObject, IPageFactory)
    function CreatePage(aOwner: TComponent): IBookPage;
  end;

implementation
uses Book_Classes, SysUtilsEx, CellPosDialog;

{$R *.dfm}

{ TSpreadSheetBookFrame }

constructor TSpreadSheetBookFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFreeForm := true;
  FSSW := TSheet.Create(SS);
  SS.ActiveSheet.Caption := 'Folha 1'
end;

function TSpreadSheetBookFrame.getActiveSheetIndex: integer;
begin
  result := SS.ActivePage;
end;

function TSpreadSheetBookFrame.getSheetCount: integer;
begin
  result := SS.PageCount;
end;

procedure TSpreadSheetBookFrame.LoadFromFile(const FileName: String);
begin
  SS.LoadFromFile(FileName);
  FSSW.Sheet := SS.ActiveSheet;
end;

procedure TSpreadSheetBookFrame.NewSheet(const aPageName: string);
begin
  SS.AddSheetPage(aPageName);
  setActiveSheetIndex(SS.PageCount-1);
end;

procedure TSpreadSheetBookFrame.SaveToFile(const FileName: String);
begin
  {$IF dir_NivelDeRestricao < 2}
  SS.SaveToFile(FileName);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetBookFrame.setActiveSheetIndex(const Value: integer);
begin
  SS.ActivePage := Value;
  FSSW.Sheet := SS.ActiveSheet;
end;

procedure TSpreadSheetBookFrame.SSActiveSheetChanging(Sender: TcxCustomSpreadSheetBook;
  const ActiveSheet: Integer; var CanSelect: Boolean);
begin
  FSSW.Sheet := SS.ActiveSheet;
end;

procedure TSpreadSheetBookFrame.Menu_CopiarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  FSSW.Copy();
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetBookFrame.Menu_ColarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  FSSW.Paste();
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetBookFrame.Menu_RecortarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  FSSW.Cut();
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetBookFrame.Menu_AbrirClick(Sender: TObject);
begin
  try
    if Load.Execute Then
       begin
       Screen.Cursor := crHourGlass;
       LoadFromFile(Load.FileName);
       end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSpreadSheetBookFrame.Menu_SalvarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 2}
  if Save.Execute then
     SaveToFile(Save.FileName);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetBookFrame.Menu_DestruirClick(Sender: TObject);
begin
  Menu_Destruir.Checked := not Menu_Destruir.Checked;
  FFreeForm := Menu_Destruir.Checked;
end;

procedure TSpreadSheetBookFrame.Menu_SempreNoTopoClick(Sender: TObject);
begin
  Menu_SempreNoTopo.Checked := not Menu_SempreNoTopo.Checked;
  if Menu_SempreNoTopo.Checked then
     TForm(self.Parent).FormStyle := fsStayOnTop
  else
     TForm(self.Parent).FormStyle := fsNormal;
end;

procedure TSpreadSheetBookFrame.Menu_FormatCellsClick(Sender: TObject);
begin
  FSSW.FormatCells(FSSW.SelectionRect);
end;

function TSpreadSheetBookFrame.getControl(): TWinControl;
begin
  result := self;
end;

procedure TSpreadSheetBookFrame.Write(row, col: integer; const s: string);
begin
  ActiveSheet.Write(row, col, s);
end;

procedure TSpreadSheetBookFrame.setFontColor(const Color: TColor);
begin

end;

procedure TSpreadSheetBookFrame.setFontName(const Name: string);
begin

end;

procedure TSpreadSheetBookFrame.setFontSize(const Size: integer);
begin

end;

procedure TSpreadSheetBookFrame.setReadOnly(b: boolean);
begin
  SS.ReadOnly := b;
end;

procedure TSpreadSheetBookFrame.Menu_CopiarTudoClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  ActiveSheet.CopyAll();
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetBookFrame.SetCellsStyle(AValuesSet: TStyleValueSet;
  AAlign: TcxHorzTextAlign; AFontSize: Integer; const AFontName: string; AStyles: TFontStyles);

  procedure SetValue(AFlag: TStyleValue; ANeedStyle: TFontStyle; var ASetStyles: TFontStyles);
  begin
    if AFlag in AValuesSet then
       if ANeedStyle in AStyles then
          Include(ASetStyles, ANeedStyle)
       else
          Exclude(ASetStyles, ANeedStyle);
  end;

var
  I, J: Integer;
  AStyle: TFontStyles;

begin
  with SS do
  try
    BeginUpdate;
    with ActiveSheet do
    begin
      for I := SelectionRect.Left to SelectionRect.Right do
        for J := SelectionRect.Top to SelectionRect.Bottom do
        with GetCellObject(I, J) do
        try
          with Style do
            begin
            AStyle := Font.Style;

            if svNull in AValuesSet then
               AStyle := []
            else
               begin
               if svFontName in AValuesSet then
                  Font.Name := AFontName;

               if svSize in AValuesSet then
                  Font.Size := AFontSize;

               if svAlign in AValuesSet then
                  HorzTextAlign := AAlign;

               SetValue(svBold, fsBold, AStyle);
               SetValue(svItalic, fsItalic, AStyle);
               SetValue(svUnderline, fsUnderline, AStyle);
               SetValue(svStrikeOut, fsStrikeOut, AStyle);
               end;

            Font.Style := AStyle;
            end;
        finally
          Free;
        end;
    end;
  finally
    EndUpdate;
    UpdateControl;
  end;
end;

procedure TSpreadSheetBookFrame.SetTokenStyle(AStyleValue: TStyleValue; AFontStyle: TFontStyle);
begin
  if AStyleValue = svNull then
     SetCellsStyle([AStyleValue], haGeneral, 0, '', [])
  else
     SetCellsStyle([AStyleValue], haGeneral, 0, '', [AFontStyle]);
end;

procedure TSpreadSheetBookFrame.BeginUpdate();
begin
  SS.BeginUpdate();
end;

procedure TSpreadSheetBookFrame.EndUpdate();
begin
  SS.EndUpdate();
end;

procedure TSpreadSheetBookFrame.Menu_EsquerdaClick(Sender: TObject);
begin
  SetCellsStyle([svAlign], haLeft, 0, self.Font.Name, []);
end;

procedure TSpreadSheetBookFrame.Menu_CentroClick(Sender: TObject);
begin
  SetCellsStyle([svAlign], haCenter, 0, self.Font.Name, []);
end;

procedure TSpreadSheetBookFrame.Menu_DireitaClick(Sender: TObject);
begin
  SetCellsStyle([svAlign], haRight, 0, self.Font.Name, []);
end;

procedure TSpreadSheetBookFrame.Menu_NegritoClick(Sender: TObject);
begin
  SetTokenStyle(svBold, fsBold);
end;

procedure TSpreadSheetBookFrame.Menu_ItalicoClick(Sender: TObject);
begin
  SetTokenStyle(svItalic, fsItalic);
end;

procedure TSpreadSheetBookFrame.Menu_SublinhadoClick(Sender: TObject);
begin
  SetTokenStyle(svUnderline, fsUnderline);
end;

procedure TSpreadSheetBookFrame.Menu_NenhumEstiloClick(Sender: TObject);
begin
  SetTokenStyle(svNull, fsBold);
end;

procedure TSpreadSheetBookFrame.Menu_TransporClick(Sender: TObject);
var p: TSheet;
    r: TRect;
    m: TStringMat;
    i: integer;
    j: integer;
    x: integer;
    y: integer;
    L: integer;
    C: integer;
begin
  p := self.ActiveSheet;
  r := p.SelectionRect;

  // Verifica se somente uma celula esta selecionada
  if (r.Left = r.Right) and (r.Top = r.Bottom) then
     Exit;

  L := r.Top + 1;
  C := r.Left + 1;
  if not TCellPosDialog.getCellPos(
     'Indique a partir de qual célula a transposição será realizada.', L, C) then
     Exit;

  // Copia a selecao para a memoria ...

  m := TStringMat.Create(r.Bottom - r.Top + 1, r.Right - r.Left + 1);
  //m.CheckBounds := false;

  y := 0;
  for i := r.Top to r.Bottom do
    begin
    inc(y);
    x := 0;
    for j := r.Left to r.Right do
      begin
      inc(x);
      m[y, x] := p.GetText(i+1, j+1);
      end;
    end;

  BeginUpdate();
  try
    // Limpa a area selecionada
    SS.ActiveSheet.ClearCells(r, true);

    // Transpoe os dados para a planilha
    y := C;
    for i := 1 to m.Rows do
      begin
      x := L;
      for j := 1 to m.Cols do
        begin
        p.Write(x, y, m[i, j]);
        inc(x);
        end;
      inc(y);
      end;

    // Seleciona a celula escolhida pelo usuario
    p.SelectionRect := Rect(L, C, L, C);
  finally
    EndUpdate();
  end;  
end;

{ TFactory }

function TFactory.CreatePage(aOwner: TComponent): IBookPage;
begin
  result := TSpreadSheetBookFrame.Create(aOwner);
end;

initialization
  getPagesFactory.RegisterPage('sheet book', TFactory.Create());

end.

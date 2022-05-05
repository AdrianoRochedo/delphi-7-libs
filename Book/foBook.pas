unit foBook;
{
  ATENCAO:
    Para as paginas serem registradas automaticamente, as unidades de codigo
    precisam ser inicializadas, ou seja, utilizadas atraves da palavra chave
    "uses" antes da chamada do método Book.NewPage().

    Os tipos existentes sao:
      - 'memo'
      - 'rtf'
      - 'sheet'
      - 'sheet book'
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Book_Interfaces;

type
  TBook = class(TForm)
    PG: TPageControl;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FCloseMessage: String;
    FCloseConfirmation: Boolean;
    FFreeOnClose: boolean;
    function getActivePageIndex: integer;
    procedure setActivePageIndex(const Value: integer);
    function getTextPage: ITextPage;
  public
    // Cria uma instancia deste componente
    constructor Create(const Caption: String; FormStyle: TFormStyle = fsNormal);

    // Estabelece á mensagem de fechamento se existir
    procedure SetCloseMessage(const aMessage: String);

    // aType = ('memo', 'rtf', 'sheet', 'sheet book');
    procedure NewPage(const aType, aCaption: string); overload;
    procedure NewPage(const aType, aCaption, Filename: string); overload;

    // Ativa a ultima pagina
    procedure goLastPage();

    // Estabelece ou retorna a página ativa
    property ActivePageIndex : integer read getActivePageIndex write setActivePageIndex;

    // Retorna a página ativa como uma página de texto ou gera uma exceção
    property TextPage : ITextPage read getTextPage;
                                                                                  
    // Estabelece o pedido de confirmação para o fechamento da janela
    property CloseConfirmation: Boolean read FCloseConfirmation write FCloseConfirmation;

    // Informa se o editor deve ser destruido quando a janela eh fechada pelo usuario
    // por default eh true
    property FreeOnClose: boolean read FFreeOnClose write FFreeOnClose;
  end;

implementation
uses Book_Classes;

{$R *.dfm}

{ TBook }

constructor TBook.Create(const Caption: String; FormStyle: TFormStyle);
begin
  inherited Create(nil);
  FCloseConfirmation := false;
  FCloseMessage := 'Você tem certeza ?';
  self.Caption := Caption;
  self.FormStyle := FormStyle;
  FFreeOnClose := true;
end;

procedure TBook.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FFreeOnClose then
     Action := caFree
  else
     Action := caHide;
end;

procedure TBook.SetCloseMessage(const aMessage: String);
begin
  FCloseMessage := aMessage;
end;

procedure TBook.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FCloseConfirmation then
     CanClose := (MessageDLG(FCloseMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  else
     CanClose := True;
end;

procedure TBook.NewPage(const aType, aCaption: string);
var p: IBookPage;
    f: IPageFactory;
    Page: TTabSheet;
begin
  f := getPagesFactory.byType(aType);
  if f <> nil then
     begin
     Page := TTabSheet.Create(PG);
     Page.PageControl := PG;
     Page.Caption := aCaption;

     // Criação da página propriamente dita
     p := f.CreatePage(self);
     p.getControl().Name := '';
     p.getControl().Parent := Page;
     p.getControl().Align := alClient;

     // Estabelece esta pagina como a ativa
     goLastPage();
     end
  else
     raise Exception.Create('Book page not defined: ' + aType);
end;

function TBook.getActivePageIndex: integer;
begin
  result := PG.ActivePageIndex;
end;

procedure TBook.setActivePageIndex(const Value: integer);
begin
  if Value < PG.PageCount then
     PG.ActivePageIndex := Value
  else
     goLastPage();
end;

function TBook.getTextPage(): ITextPage;
var c: TObject;
begin
  c := PG.ActivePage.Controls[0];
  if not c.GetInterface(ITextPage, result) then
     raise Exception.Create('A página ativa não implementa a interface "ITextPage"');
end;

procedure TBook.goLastPage();
begin
  PG.ActivePageIndex := PG.PageCount - 1;
end;

procedure TBook.NewPage(const aType, aCaption, Filename: string);
var c: TObject;
    p: IPage;
begin
  NewPage(aType, aCaption);
  goLastPage();
  c := PG.ActivePage.Controls[0];
  if c.GetInterface(IPage, p) then
     p.LoadFromFile(Filename);
end;

end.

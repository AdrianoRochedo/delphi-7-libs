unit SpreadSheet_Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, cxControls, cxSSheet, Book_Interfaces, Menus, DiretivasDeCompilacao;

type
  TSpreadSheetFrame = class(TFrame, IBookPage)
    SS: TcxSpreadSheet;
    Menu: TPopupMenu;
    Menu_Copiar: TMenuItem;
    Menu_Colar: TMenuItem;
    Menu_Recortar: TMenuItem;
    N1: TMenuItem;
    Menu_Abrir: TMenuItem;
    Menu_Salvar: TMenuItem;
    N3: TMenuItem;
    Menu_FormatCells: TMenuItem;
    Save: TSaveDialog;
    Load: TOpenDialog;
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Menu_ColarClick(Sender: TObject);
    procedure Menu_RecortarClick(Sender: TObject);
    procedure Menu_AbrirClick(Sender: TObject);
    procedure Menu_SalvarClick(Sender: TObject);
    procedure Menu_FormatCellsClick(Sender: TObject);
  private
    function getControl(): TWinControl;
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
  public
    { Public declarations }
  end;

  TFactory = class(TInterfacedObject, IPageFactory)
    function CreatePage(aOwner: TComponent): IBookPage;
  end;

implementation
uses Book_Classes;

{$R *.dfm}

{ TSpreadSheetFrame }

function TSpreadSheetFrame.getControl: TWinControl;
begin
  result := self;
end;

procedure TSpreadSheetFrame.LoadFromFile(const Filename: string);
begin
  SS.LoadFromFile(Filename);
end;

procedure TSpreadSheetFrame.SaveToFile(const Filename: string);
begin
  SS.SaveToFile(Filename);
end;

procedure TSpreadSheetFrame.Menu_CopiarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  SS.Sheet.Copy(SS.SelectionRect, false);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetFrame.Menu_ColarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  SS.Sheet.Paste(SS.ActiveCell);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetFrame.Menu_RecortarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  SS.Sheet.Copy(SS.SelectionRect, true);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetFrame.Menu_AbrirClick(Sender: TObject);
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

procedure TSpreadSheetFrame.Menu_SalvarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 2}
  if Save.Execute then
     SaveToFile(Save.FileName);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TSpreadSheetFrame.Menu_FormatCellsClick(Sender: TObject);
begin
  SS.Sheet.FormatCells(SS.SelectionRect);
end;

{ TFactory }

function TFactory.CreatePage(aOwner: TComponent): IBookPage;
begin
  result := TSpreadSheetFrame.Create(aOwner);
end;

initialization
  getPagesFactory.RegisterPage('sheet', TFactory.Create());

end.

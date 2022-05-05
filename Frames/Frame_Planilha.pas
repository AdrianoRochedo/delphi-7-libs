unit Frame_Planilha;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AxCtrls, OleCtrls, vcf1, Menus, Buttons, ExtCtrls, DiretivasDeCompilacao;

type
  TFramePlanilha = class(TFrame)
    Tab: TF1Book;
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
    Panel1: TPanel;
    btnAbrir: TSpeedButton;
    btnSalvar: TSpeedButton;
    btnFechar: TSpeedButton;
    btnCopiar: TSpeedButton;
    btnRecortar: TSpeedButton;
    btnColar: TSpeedButton;
    btnImprimir: TSpeedButton;
    N3: TMenuItem;
    Menu_Fechar: TMenuItem;
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Menu_ColarClick(Sender: TObject);
    procedure Menu_RecortarClick(Sender: TObject);
    procedure Menu_AbrirClick(Sender: TObject);
    procedure Menu_SalvarClick(Sender: TObject);
    procedure Menu_ImprimirClick(Sender: TObject);
    procedure TabKeyPress(Sender: TObject; var Key: Char);
    procedure Menu_FecharClick(Sender: TObject);
  private
    FDS_O: Char;
  public
    procedure SetDecimalSeparator(SourceChar: Char);
  end;

implementation

{$R *.DFM}

procedure TFramePlanilha.Menu_CopiarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Tab.EditCopy;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TFramePlanilha.Menu_ColarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Tab.EditPaste;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TFramePlanilha.Menu_RecortarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Tab.EditCut;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TFramePlanilha.Menu_AbrirClick(Sender: TObject);
var FT: SmallInt;
begin
  try
    if Load.Execute Then
       begin
       Screen.Cursor := crHourGlass;
       Tab.Read(Load.FileName, FT);
       end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFramePlanilha.Menu_SalvarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 2}
  if Save.Execute then
     Case Save.FilterIndex of
       1 : Tab.Write(Save.FileName, F1FileTabbedText);
       2 : Tab.Write(Save.FileName, F1FileExcel5);
       end;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TFramePlanilha.Menu_ImprimirClick(Sender: TObject);
begin
  Tab.FilePrint(True);
end;

procedure TFramePlanilha.SetDecimalSeparator(SourceChar: Char);
begin
  FDS_O := SourceChar;
end;

procedure TFramePlanilha.TabKeyPress(Sender: TObject; var Key: Char);
begin
  //if Key in ['.', ','] then Key := FDS_O;
end;

procedure TFramePlanilha.Menu_FecharClick(Sender: TObject);
begin
  if Parent is TForm then
     TForm(Parent).Close;
end;

end.

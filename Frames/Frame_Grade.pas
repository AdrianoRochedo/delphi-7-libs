unit Frame_Grade;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, gridx32, DiretivasDeCompilacao;

type
  TFrame_Grade = class(TFrame)
    Tab: TdrStringAlignGrid;
    Menu: TPopupMenu;
    Menu_Salvar: TMenuItem;
    Menu_Ler: TMenuItem;
    N2: TMenuItem;
    Menu_Editar: TMenuItem;
    Menu_Selecionar: TMenuItem;
    N3: TMenuItem;
    Menu_Copiar: TMenuItem;
    Menu_Colar: TMenuItem;
    Load: TOpenDialog;
    Save: TSaveDialog;
    procedure Menu_SalvarClick(Sender: TObject);
    procedure Menu_LerClick(Sender: TObject);
    procedure Menu_EditarClick(Sender: TObject);
    procedure Menu_SelecionarClick(Sender: TObject);
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Menu_ColarClick(Sender: TObject);
  private
    FModificado: Boolean;
    FDir: String;
    { Private declarations }
  public
    property Dir : String read FDir write FDir;
    property Modificado : Boolean read FModificado write FModificado;
  end;

implementation

{$R *.DFM}

procedure TFrame_Grade.Menu_SalvarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 2}
  Save.InitialDir := fDir;
  If Save.Execute Then
     Try
       fDir := ExtractFilePath(Save.FileName);
       if Save.FilterIndex = 1 then
          {$ifdef Excel}
          Tab.SaveToXLS(Save.FileName)
          {$endif}
       else
          Tab.SaveToFile(Save.FileName);
     Except
       Raise Exception.Create('Erro ao Salvar Dados');
     End;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TFrame_Grade.Menu_LerClick(Sender: TObject);
begin
  Load.InitialDir := fDir;
  If Load.Execute Then
     Try
       fDir := ExtractFilePath(Load.FileName);

       if Save.FilterIndex = 1 then
          {$ifdef Excel}
          Tab.LoadFromXLS(Load.FileName)
          {$endif}
       else
          Tab.LoadFromFile(Load.FileName);

       fModificado := True;
     Except
       Raise Exception.Create('Erro de Leitura');
     End;
end;

procedure TFrame_Grade.Menu_EditarClick(Sender: TObject);
begin
  Tab.Options := Tab.Options + [goEditing] - [goDrawFocusSelected];
  Menu_Editar.Checked := True;
end;

procedure TFrame_Grade.Menu_SelecionarClick(Sender: TObject);
begin
  Tab.Options := Tab.Options - [goEditing] + [goDrawFocusSelected];
  Tab.FocusColor := clHighLight;
  Tab.FocusTextColor := clHighLightText;
  Menu_Selecionar.Checked := True;
end;

procedure TFrame_Grade.Menu_CopiarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Tab.Copy;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TFrame_Grade.Menu_ColarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Tab.Paste;
  FModificado := True;
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

end.

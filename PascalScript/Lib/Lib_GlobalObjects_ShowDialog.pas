unit Lib_GlobalObjects_ShowDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, Menus,
  MessageManager,
  Lib_GlobalObjects;

type
  TGlobalObjects_ShowDialog = class(TForm, IMessageReceptor)
    Arvore: TTreeView;
    Imagens: TImageList;
    Menu: TPopupMenu;
    Menu_Remover: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Menu_RemoverClick(Sender: TObject);
    procedure ArvoreChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    FRaiz: TTreeNode;
    FDados: TGlobalObjects;
    function AchaNoDaClasse(const Classe: String): TTreeNode;
    function  ReceiveMessage(Const MSG: TadvMessage): Boolean; 
  public
    procedure Mostrar(Dados: TGlobalObjects; const Caption: String = '');
    procedure Limpar;
  end;

implementation

{$R *.DFM}

{ TGlobalObjects_ShowDialog }

procedure TGlobalObjects_ShowDialog.Mostrar(Dados: TGlobalObjects; const Caption: String = '');
var no: TTreeNode;
     i: Integer;
begin
  self.Caption := 'Variáveis Globais';
  if Caption <> '' then self.Caption := self.Caption + ': ' + Caption;

  Arvore.Items.BeginUpdate;

  FDados := Dados;
  FRaiz.DeleteChildren;
  FRaiz.Text := 'Classes';

  for i := 0 to Dados.Count - 1 do
    begin
    no := AchaNoDaClasse(Dados.Objects[i].ClassName);
    
    if no = FRaiz then
       no := Arvore.Items.AddChild(no, Dados.Objects[i].ClassName);

    no := Arvore.Items.AddChild(no, Dados.Name[i]);
    no.ImageIndex := 1;
    no.SelectedIndex := 1;
    end;
  FRaiz.Expand(True);

  Arvore.Items.EndUpdate;
  Show;
end;

procedure TGlobalObjects_ShowDialog.FormCreate(Sender: TObject);
begin
  FRaiz := Arvore.Items[0];
  GetMessageManager.RegisterMessage(GO_CHANGE, Self);
  GetMessageManager.RegisterMessage(GO_CLOSE_DIALOG, Self);
end;

procedure TGlobalObjects_ShowDialog.Limpar;
begin
  FRaiz.DeleteChildren;
end;

function TGlobalObjects_ShowDialog.AchaNoDaClasse(const Classe: String): TTreeNode;
var i: Integer;
begin
  for i := 0 to Arvore.Items.Count-1 do
    begin
    Result := Arvore.Items[i];
    if Result.Text = Classe then Exit;
    end;
  Result := FRaiz;
end;

procedure TGlobalObjects_ShowDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TGlobalObjects_ShowDialog.FormDestroy(Sender: TObject);
begin
  GetMessageManager.UnRegisterMessage(GO_CHANGE, Self);
  GetMessageManager.UnRegisterMessage(GO_CLOSE_DIALOG, Self);
end;

procedure TGlobalObjects_ShowDialog.ArvoreChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := (Node.Level = 2);
end;

procedure TGlobalObjects_ShowDialog.Menu_RemoverClick(Sender: TObject);
var No: TTreeNode;
begin
  No := Arvore.Selected;
  if (No <> nil) and (No.Level = 2) then
     FDados.Remove(No.Text);
end;

function TGlobalObjects_ShowDialog.ReceiveMessage(const MSG: TadvMessage): Boolean;
begin
  if MSG.ID = GO_CHANGE then
     Mostrar(TGlobalObjects(MSG.ParamAsObject(0))) else

  if MSG.ID = GO_CLOSE_DIALOG then
     if MSG.ParamAsObject(0) = FDados then
        Close;
end;

end.

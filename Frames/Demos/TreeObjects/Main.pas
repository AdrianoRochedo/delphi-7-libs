unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, actnList, Frame_TreeViewObjects, SysUtilsEx, TreeViewUtils,
  WinUtils;

type
  Tx = class(T_NRC_InterfacedObject, ITreeNode)
  private
    FTreeNode: TTreeNode;

    // ITreeNode interface
    function getRef(): TObject;
    function getDescription(): string;
    function CanEdit(): boolean;
    function getImageIndex(): integer;
    function getSelectedImageIndex(): integer;
    function getNodeText(): string;
    procedure ExecuteDefaultAction();
    procedure setEditedText(var Text: string);
    procedure getActions(Actions: TActionList);
    procedure setNode(Node: TTreeNode);

    procedure RemoveAction(Sender: TObject);
    destructor Destroy(); override;
  public
  end;

  TfoMain = class(TForm)
    frTree: TTreeViewObjects;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  foMain: TfoMain;

implementation

{$R *.dfm}

{ Tx }

function Tx.CanEdit(): boolean;
begin
  result := true;
end;

destructor Tx.Destroy();
begin
  if FTreeNode <> nil then FTreeNode.Delete();
  inherited;
end;

procedure Tx.ExecuteDefaultAction();
begin
  ShowMessage('Oi');
end;

procedure Tx.getActions(Actions: TActionList);
var act: TAction;
begin
  CreateAction(Actions, nil, 'Remover', false, RemoveAction, nil);
  CreateAction(Actions, nil, '-', false, nil, nil);
  Act := CreateAction(Actions, nil, 'Manegements', false, nil, nil);
          CreateAction(Actions, Act, 'Plot', false, nil, nil);
end;

function Tx.getDescription(): string;
begin
  result := 'teste';
end;

function Tx.getImageIndex(): integer;
begin
  result := 1;
end;

function Tx.getNodeText: string;
begin
  result := ClassName;
end;

function Tx.getRef(): TObject;
begin
  result := self;
end;

procedure TfoMain.FormShow(Sender: TObject);
var no: TTreeNode;
begin
  no := frTree.Tree.Items.AddChildObject(nil, 'No 1', Tx.Create());
     frTree.Tree.Items.AddChildObject(no, 'No 2', Tx.Create());
     frTree.Tree.Items.AddChildObject(no, 'Janela', self);
end;

function Tx.getSelectedImageIndex(): integer;
begin
  result := getImageIndex();
end;

procedure Tx.RemoveAction(Sender: TObject);
begin
  Free();
end;

procedure Tx.setEditedText(var Text: string);
begin
  ShowMessage(Text);
end;

procedure Tx.setNode(Node: TTreeNode);
begin
  FTreeNode := Node;
end;

end.

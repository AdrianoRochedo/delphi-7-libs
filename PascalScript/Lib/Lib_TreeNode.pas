unit LIB_TreeNode;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses psBASE;

  procedure API(Lib: TLib);

const
  cCatTreeView = 'TreeView';

type
  Tps_TreeNode = class(TpsClass)
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
    class procedure am_ChildNodes(Const Func_Name: String; Stack: TexeStack);
    class procedure am_getChildNode(Const Func_Name: String; Stack: TexeStack);
    class procedure am_getObject(Const Func_Name: String; Stack: TexeStack);
    class procedure am_getText(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setObject(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setText(Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses ComCtrls;

  procedure API(Lib: TLib);
  begin
    Tps_TreeNode.Create(TTreeNode,
                        nil,
                        '',
                        cCatTreeView,
                        [],
                        [],
                        [],
                        True,
                        Lib.Classes);
  end;

{ Tps_TreeNode }

function Tps_TreeNode.CreateObject(Stack: TExeStack): TObject;
begin
  Result := TTreeNode.Create(nil);
end;

class procedure Tps_TreeNode.am_ChildNodes(Const Func_Name: String; Stack: TexeStack);
var o: TTreeNode;
begin
  o := TTreeNode(Stack.AsObject(1));
  Stack.PushInteger(o.Count);
end;

class procedure Tps_TreeNode.am_getChildNode(Const Func_Name: String; Stack: TexeStack);
var o: TTreeNode;
begin
  o := TTreeNode(Stack.AsObject(2));
  Stack.PushObject( o.Item[Stack.AsInteger(1)] )
end;

class procedure Tps_TreeNode.am_getObject(Const Func_Name: String; Stack: TexeStack);
var o: TTreeNode;
begin
  o := TTreeNode(Stack.AsObject(1));
  Stack.PushObject(o.Data)
end;

class procedure Tps_TreeNode.am_getText(Const Func_Name: String; Stack: TexeStack);
var o: TTreeNode;
begin
  o := TTreeNode(Stack.AsObject(1));
  Stack.PushString(o.Text)
end;

class procedure Tps_TreeNode.am_setObject(Const Func_Name: String; Stack: TexeStack);
var o: TTreeNode;
begin
  o := TTreeNode(Stack.AsObject(2));
  o.Data := Stack.AsObject(1);
end;

class procedure Tps_TreeNode.am_setText(Const Func_Name: String; Stack: TexeStack);
var o: TTreeNode;
begin
  o := TTreeNode(Stack.AsObject(2));
  o.Text := Stack.AsString(1);
end;

procedure Tps_TreeNode.AddMethods;
begin
  with Procs do
    begin
    Add('setObject',
        'Associa um objeto ao nó',
        '',
        [pvtObject],
        [TObject],
        [True],
        pvtNull,
        TObject,
        am_setObject);

    Add('setText',
        'Estabelece o texto do nó',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setText);

    end;

  with Functions do
    begin
    Add('ChildNodes',
        'Retorna o número de nós filhos',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_ChildNodes);

    Add('getChildNode',
        'Retorna o i-égimo filho do nó',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TTreeNode,
        am_getChildNode);

    Add('getObject',
        'Retorna o objeto que está associado ao nó',
        '',
        [],
        [],
        [],
        pvtObject,
        TObject,
        am_getObject);

    Add('getText',
        'Retorna o Texto do nó',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        am_getText);

    end;
end;

end.

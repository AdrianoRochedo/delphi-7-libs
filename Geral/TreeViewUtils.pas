unit TreeViewUtils;

interface
uses ComCtrls,
     actnList,
     {$ifdef MSXML}
     MSXML4,
     XML_Interfaces,
     {$endif}
     FileUtils,
     Classes;

  // Busca de Informações - Retornam nil se não encontrarem
  function FindNode(Root: TTreeNode; const Text: String) : TTreeNode; overload; // Não recursiva
  function FindNode(Root: TTreeNode; const Data: Pointer): TTreeNode; overload; // Não recursiva

  // Imagens
  procedure SetImageIndex(Node: TTreeNode; Index: Integer);

  // Libera os nós e os objetos associados
  procedure FreeNodes(Nodes: TTreeNodes);

  // Libera os nós filhos e seus objetos
  procedure FreeChildNodes(Node: TTreeNode);

  // Cria os Nos diacordo com o caminho (Path)
  // Ex: B\D\E --> Root
  //               |-B
  //               | |-D
  //               | | |-E
  // Retorna o último nó adicionado, no caso do exemplo "E"
  function PathToTree(Tree: TTreeView;
                      Root: TTreeNode;
                      const Path: string;
                      Separator: char = '\'): TTreeNode;

  {<description>
   Dado um "No" de uma TreeView a função retorna uma string contendo o caminho completo até
   o Nível especificado no parâmetro "UntilLevel".
   Ex:
       <code>
       raiz
         primeiro filho
           filho do filho
         segundo filho

       MakePath(filho do filho, 0) --> raiz\primeiro filho\filho do filho
       MakePath(filho do filho, 1) --> primeiro filho\filho do filho
       </code>
   </description>}
  function MakePath(Node: TTreeNode;
                    UntilLevel: byte = 0;
                    Separator: char = '\'): String; overload;

  function MakeSubPath(Nodes: TTreeNodes; const Path: String): TTreeNode;

{$ifdef MSXML}

  type IXMLtoTree_Factory = interface
    ['{357F4F2D-EAFD-4D53-8D73-6349E37D82F8}']
    function CreateObject(const ClassName: String; Node: TTreeNode): IFromXML;
  end;

  {Salva o conteúdo de uma TreeView para XML.
   Se os nós são objetos que implementam a interface IToXML, então estes objetos também
   são salvos.}
  procedure TreeToXML(const FileName: String;
                            TreeView: TTreeView);

  {Restaura o conteúdo salvo em XML de um TreeView, inclusive os objetos salvos.
   "Factory" é uma instância que implementa a interface "IFromXML_Factory" e é o
   responsável por criar as instâncias das classes encontradas no arquivo.}
  procedure XMLtoTree(const FileName: String;
                            TreeView: TTreeView;
                            Factory: IXMLtoTree_Factory);
{$endif}

  // Formata a TTreeView em modo texto
  procedure TreeToText(TreeView: TTreeView; Text: TStrings);

  // Adiciona um nó em uma árvore
  function AddNode(Tree       : TTreeView;
                   ParentNode : TTreeNode;
                   const Text : string;
                   ImageIndex : integer = -1;
                   Obj        : TObject = nil): TTreeNode;

type

  // Os objetos implementadores deverao possuir um atributo "Node" que
  // representara o "TreeNode" da arvore que o armazena.
  // Este "Node" eh estabelecido atraves do metodo "setNode" chamado pelo
  // mecanismo.
  // Na destruicao do objeto implementador faca:
  //    ...
  //    if Node <> nil then Node.Data := nil;
  //    ...
  // Desta maneira tornamos nossos objetos imunes a erros gerados pela ordem
  // de destruicao dos objetos.
  ITreeNode = interface
    ['{CE577ED2-2829-4890-81D2-76BBC8642EE4}']

    // Geralmente estaticos
    function getRef(): TObject;
    procedure setNode(Node: TTreeNode);

    // Geralmente virtuais
    function canEdit(): boolean;
    function getDescription(): string;
    function getNodeText(): string;
    function getImageIndex(): integer;
    function getSelectedImageIndex(): integer;
    procedure setEditedText(var Text: string);
    procedure getActions(Actions: TActionList);
    procedure executeDefaultAction();
  end;

implementation
uses SysUtils, SysUtilsEx, XML_Utils;

function FindNode(Root: TTreeNode; const Text: String): TTreeNode;
var CurItem: TTreeNode;
begin
  Result := nil;
  CurItem := Root;
  while (CurItem <> nil) and (Result = nil) do
    begin
    if CurItem.Text = Text then Result := CurItem;
    CurItem := CurItem.GetNext;
    end;
end;

function FindNode(Root: TTreeNode; const Data: Pointer): TTreeNode; overload;
var CurItem: TTreeNode;
begin
  Result := nil;
  CurItem := Root;
  while (CurItem <> nil) and (Result = nil) do
    begin
    if CurItem.Data = Data then Result := CurItem;
    CurItem := CurItem.GetNext;
    end;
end;

procedure SetImageIndex(Node: TTreeNode; Index: Integer);
begin
  Node.SelectedIndex := Index;
  Node.ImageIndex := Index;
end;

procedure FreeNodes(Nodes: TTreeNodes);
var CurItem: TTreeNode;
begin
  CurItem := Nodes.GetFirstNode;
  while CurItem <> nil do
    begin
    if CurItem.Data <> nil then TObject(CurItem.Data).Free;
    CurItem := CurItem.GetNext;
    end;
  Nodes.Clear;
end;

{$ifdef MSXML}
procedure TreeToXML(const FileName: String; TreeView: TTreeView);
var F: TStrings;
    no: TTreeNode;
    x: TTextFileWriter;

    procedure WriteNode(no: TTreeNode);
    var Ident: Integer;
        I    : IToXML;
        s    : String;
    begin
      Ident := no.Level * 2 + 2;

      s := StringOfChar(' ', Ident);
      x.WriteLN(s + Format('<node Text="%s" Level="%d" II="%d" SI="%d">',
                    [no.Text, no.Level, no.ImageIndex, no.SelectedIndex]));

      if TObject(no.Data) is TObject then
         if TObject(no.Data).GetInterface(IToXML, I) then
            begin
            inc(Ident, 2);
            s := StringOfChar(' ', Ident);

            x.WriteLN(s + Format('<Object class="%s">', [I.GetClassName]));

            F.Clear();
            I.ToXML(F, Ident);
            x.WriteLN(F);

            x.WriteLN(s + '</Object>');

            dec(Ident, 2);
            s := StringOfChar(' ', Ident);
            end;

      x.WriteLN(s + '</node>');
    end;

begin
  F := TStringList.Create;

  x := TTextFileWriter.Create(Filename, true);
  try
    WriteXMLHeader(F, 'TreeViewUtils.TreeToXML', []);
    x.WriteLN(F);
    x.WriteLN('<TreeNodes>');
    no := TreeView.Items.GetFirstNode();
    while no <> nil do
      begin
      WriteNode(no);
      no := no.GetNext();
      end;
    x.WriteLN('</TreeNodes>');
  finally
    x.Free();
    F.Free();
  end;
end;

procedure XMLtoTree(const FileName: String;
                          TreeView: TTreeView;
                          Factory: IXMLtoTree_Factory);

var TN, NextTN: TTreeNode;
    s: String;

  procedure ReadNode(no: IXMLDomNode);
  var Level: Integer;
      s: String;
      Obj: IFromXML;
  begin
    s := no.Attributes[0].Text;
    Level := StrToInt(no.Attributes[1].text);

    if TN = nil then
       TN := TreeView.Items.AddChild(nil, s) else

    if TN.Level = Level then
       TN := TreeView.Items.AddChild(TN.Parent, s) else

    if TN.Level = (Level - 1) then
       TN := TreeView.Items.AddChild(TN, s) else

    if TN.Level > Level then
       begin
       NextTN := TN.Parent;
       while NextTN.Level > Level do
         NextTN := NextTN.Parent;
       TN := TreeView.Items.AddChild(NextTN.Parent, s);
       end
    else
       Raise Exception.Create('TreeNodes Read Error');

    TN.ImageIndex := StrToInt(no.Attributes[2].Text);
    TN.SelectedIndex := StrToInt(no.Attributes[3].Text);

    // Verificar se existe objeto conectado a este nó
    if no.hasChildNodes then
       begin
       no  := no.ChildNodes[0];
       Obj := Factory.CreateObject(no.Attributes[0].Text, TN);
       if Obj <> nil then
          begin
          Obj.FromXMLNodeList(no.childNodes);
          TN.Data := Obj.GetObjectReference();
          s := Obj.ToString();
          if s <> '' then TN.Text := s;
          end;
       end;
  end;

  procedure NavigateNodes(no: IXMLDomNode);
  var CN: IXMLDomNode;
  begin
    if no.hasChildNodes then
       begin
       TN := nil;
       CN := no.ChildNodes[0];
       while CN <> nil do
         begin
         ReadNode(CN);
         CN := CN.NextSibling;
         end;
       end;
  end;

var Doc: IXMLDOMDocument;
begin
  Doc := XML_Utils.OpenXMLDocument(FileName);
  s := Doc.DocumentElement.nodeName;

  if (s = 'TreeNodes') or (s = 'TreeViewNodes') then
     try
       TreeView.Items.BeginUpdate();
       if TreeView.Items.Count > 0 then FreeNodes(TreeView.Items);
       NavigateNodes(Doc.documentElement);
     finally
       TreeView.Items.EndUpdate();
     end
  else
     raise Exception.Create('Invalid Document Element' + Doc.DocumentElement.nodeName);
end;
{$endif}

function MakePath(Node: TTreeNode;
                  UntilLevel: byte = 0;
                  Separator: char = '\'): String;

  procedure MakeSubPath;
  begin
    if Node.Level = UntilLevel then
       Result := Node.Text + Separator + Result
    else if Node.Level > UntilLevel then
       if Result = '' then
          Result := Node.Text
       else
          Result := Node.Text + Separator + Result;
  end; {MakeSubPath}

begin
  Result := '';
  MakeSubPath;
  while Node.Parent <> nil do
    begin
    Node := Node.Parent;
    MakeSubPath();
    end;
end; {MakePath}

function MakeSubPath(Nodes: TTreeNodes; const Path: String): TTreeNode;
var SL     : TStrings;
    i      : Integer;
    Raiz   : TTreeNode;
    SubDir : String;
begin
  SL := nil;
  Split(Path, SL, ['\']);
  try
    if Nodes.Count > 0 then Raiz := Nodes[0] else Raiz := Nodes.AddChild(nil, 'Raiz');
    for i := 0 to SL.Count-1 do
      begin
      SubDir := SL[i];
      Result := FindNode(Raiz, SubDir);
      if Result = nil then
         begin
         Result := Raiz;
         break;
         end
      else
         Raiz := Result;
      end;
    Result := Nodes.AddChild(Result, SubDir);
  finally
    SL.Free;
  end;
end;

procedure TreeToText(TreeView: TTreeView; Text: TStrings);
const
  TabChar = #9;
  EndOfLine = #13#10;
var
  i: Integer;
  ANode: TTreeNode;
  NodeStr: string;
begin
  Text.Clear;
  if TreeView.Items.Count > 0 then
  begin
    ANode := TreeView.Items[0];
    while ANode <> nil do
    begin
      NodeStr := '';
      for i := 0 to ANode.Level - 1 do NodeStr := NodeStr + TabChar;
      NodeStr := NodeStr + ANode.Text{ + EndOfLine};
      Text.Add(NodeStr);
      ANode := ANode.GetNext;
    end;
  end;
end;

procedure FreeChildNodes(Node: TTreeNode);
var i: Integer;
begin
  for i := 0 to Node.Count-1 do
    if Node[i].Data <> nil then
       TObject(Node[i].Data).Free;

  Node.DeleteChildren();
end;

function PathToTree(Tree: TTreeView;
                    Root: TTreeNode;
                    const Path: string;
                    Separator: char = '\'): TTreeNode;
var
  SL: TStrings;

    function FindNode(Root: TTreeNode; const Text: string): TTreeNode;
    var i: Integer;
    begin
      result := nil;
      for i := 0 to Root.Count-1 do
        if Root.Item[i].Text = Text then
           begin
           result := Root.Item[i];
           break;
           end;
    end;

    procedure InsertAll(var Root: TTreeNode; Pos: integer);
    var i: Integer;
    begin
      for i := Pos to SL.Count-1 do
        Root := Tree.Items.AddChild(Root, SL[i]);
    end;

var i  : integer;
    no : TTreeNode;
begin
  SL := nil;
  Split(Path, SL, [Separator]);
  i := 0;
  while i < SL.Count do
    begin
    no := FindNode(Root, SL[i]);
    if no = nil then
       begin
       InsertAll({var} Root, i);
       break;
       end
    else
       begin
       Root := no;
       inc(i);
       end;
    end;
  result := Root;
end;

function AddNode(Tree       : TTreeView;
                 ParentNode : TTreeNode;
                 const Text : string;
                 ImageIndex : integer = -1;
                 Obj        : TObject = nil): TTreeNode;
begin
  result := Tree.Items.AddChildObject(ParentNode, Text, Obj);
  SetImageIndex(result, ImageIndex);
end;

end.

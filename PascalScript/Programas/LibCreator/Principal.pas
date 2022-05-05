unit Principal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, gb_Classes, Menus,
  psEditorBaseComp, psEditorComp;

type
  TformPrincipal = class(TForm)
    Book: TPageControl;
    tsGeral: TTabSheet;
    Label1: TLabel;
    G_edNU: TEdit;
    Label2: TLabel;
    G_mCG: TMemo;
    tsClasses: TTabSheet;
    tsMetodos: TTabSheet;
    tsOutPut: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    C_edNome: TEdit;
    C_cbAncestral: TComboBox;
    C_mComment: TMemo;
    C_edCat: TEdit;
    C_edNMC: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    M_edNome: TEdit;
    M_mComment: TMemo;
    M_edCat: TEdit;
    GbParametros: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    M_cbTipoPar: TComboBox;
    M_btnAdicionarPar: TButton;
    M_btnRemoverPar: TButton;
    M_btnModificarPar: TButton;
    M_lbParametros: TListBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    M_cbResultado: TComboBox;
    M_edMA: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    M_btnAdicionaRot: TButton;
    M_btnRemoveRot: TButton;
    M_btnModificaRot: TButton;
    M_lbRotinas: TListBox;
    Label16: TLabel;
    M_cbClasse: TComboBox;
    M_rbC: TRadioButton;
    M_rbR: TRadioButton;
    GroupBox3: TGroupBox;
    C_btnAdicionar: TButton;
    C_btnRemover: TButton;
    C_btnModificar: TButton;
    C_lbClasses: TListBox;
    G_btnGerar: TButton;
    M_cbTipoParClasse: TComboBox;
    M_cbClasseRes: TComboBox;
    S_mSaida: TScriptPascalEditor;
    Label17: TLabel;
    G_edRotPrefix: TEdit;
    Save: TSaveDialog;
    Menu: TPopupMenu;
    Menu_SalvarComo: TMenuItem;
    btnInfo: TButton;
    procedure G_btnGerarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure C_btnAdicionarClick(Sender: TObject);
    procedure C_btnRemoverClick(Sender: TObject);
    procedure C_btnModificarClick(Sender: TObject);
    procedure M_btnAdicionaRotClick(Sender: TObject);
    procedure M_btnRemoveRotClick(Sender: TObject);
    procedure M_btnModificaRotClick(Sender: TObject);
    procedure M_btnAdicionarParClick(Sender: TObject);
    procedure M_btnRemoverParClick(Sender: TObject);
    procedure M_btnModificarParClick(Sender: TObject);
    procedure C_lbClassesClick(Sender: TObject);
    procedure M_cbClasseChange(Sender: TObject);
    procedure M_cbTipoParChange(Sender: TObject);
    procedure M_cbResultadoChange(Sender: TObject);
    procedure BookChange(Sender: TObject);
    procedure C_cbAncestralDropDown(Sender: TObject);
    procedure M_edNomeExit(Sender: TObject);
    procedure Menu_SalvarComoClick(Sender: TObject);
    procedure M_lbRotinasClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
  private
    Funcs   : TFunctionList;
    Procs   : TProcList;
    Classes : TClassList;

    procedure MostrarRotinas;
    procedure LimparRotinas;
    function  ObtemParametrosDaRotina: String;
  public
    { Public declarations }
  end;

var
  formPrincipal: TformPrincipal;

implementation
uses SysUtilsEx, WinUtils;

{$R *.DFM}

procedure TformPrincipal.G_btnGerarClick(Sender: TObject);
var SLC: TStrings;

  procedure GeraCabecalhoDaRotina(Rot: TFunctionObject; PularLinha: Boolean);
  var i: Integer;
      s: String;
  begin
    with S_mSaida.Lines do
      begin
      Add('    Add(' + QuotedStr(Rot.Name) + ',');

      if Rot.Comment <> '' then
         begin
         SLC.Text := Rot.Comment;
         for i := 0 to SLC.Count-1 do
           if i < SLC.Count-1 then
              Add('        ' + QuotedStr(SLC[i]) +  '#13 +')
           else
              Add('        ' + QuotedStr(SLC[i]) + ',');
         end
      else
         Add('        ' + QuotedStr('') + ',');

      Add('        ' + QuotedStr(Rot.Category) + ',');

      // Tipo dos parâmetros
      s := '[';
      for i := 0 to Rot.Parameters-1 do
        begin
        s := s + Rot.ParType[i];
        if i < Rot.Parameters-1 then s := s + ', ';
        end;
      s := s + '],';
      Add('        ' + s);

      // Tipo dos Objetos se parametro = pvtObject
      s := '[';
      for i := 0 to Rot.Parameters-1 do
        begin
        s := s + Rot.ParClass[i];
        if i < Rot.Parameters-1 then s := s + ', ';
        end;
      s := s + '],';
      Add('        ' + s);

      // Categoria dos parâmetros
      s := '[';
      for i := 0 to Rot.Parameters-1 do
        begin
        s := s + Rot.ParCat[i];
        if i < Rot.Parameters-1 then s := s + ', ';
        end;
      s := s + '],';
      Add('        ' + s);

      Add('        ' + Rot.ResType + ',');
      Add('        ' + Rot.ResObjectClass + ',');
      Add('        ' + Rot.AccessMethod + ');');

      if PularLinha then Add('');
      end;
  end;

  procedure GeraMioloDaRotina(Classe: TpsClass; Rot: TFunctionObject);
  var s: String;
  begin
    if Classe <> nil then
       S_mSaida.Lines.Add(format('  o := Stack.AsObject(%d); // self',
                                [Rot.Parameters + 1]));

    S_mSaida.Lines.Add('  // ...');

    if Rot.ResType <> 'pvtNull' then
       begin
       s := '  Stack.Push';
       if Rot.ResType = 'pvtObject'  then s := s + 'Object'  else
       if Rot.ResType = 'pvtInteger' then s := s + 'Integer' else
       if Rot.ResType = 'pvtReal'    then s := s + 'Float'   else
       if Rot.ResType = 'pvtString'  then s := s + 'String'  else
       if Rot.ResType = 'pvtBoolean' then s := s + 'Boolean' else
       if Rot.ResType = 'pvtChar'    then s := s + 'Char';
       s := s + '(...);';
       S_mSaida.Lines.Add(s);
       end;
  end;

  procedure GeraMioloDaAPI;
  var s, Espacos: String;
      i, k: Integer;
  begin
    with S_mSaida.Lines do
      begin
      if Procs.Count > 0 then
         begin
         s := G_edRotPrefix.Text;
         if s = '' then s := '    TGeral_Procs' else s := '    ' + s + 'Procs';
         Add(s + '.AddProcsIn(Lib.Procs);');
         end;

      if Funcs.Count > 0 then
         begin
         s := G_edRotPrefix.Text;
         if s = '' then s := '    TGeral_Functions' else s := '    ' + s + 'Functions';
         Add(s + '.AddFunctionsIn(Lib.Functions);');
         end;

      for i := 0 to Classes.Count-1 do
        begin
        s := Classes[i].InternalName;
        s := '    ' + s + '.Create(';
        Espacos := StringOfChar(' ', Length(s));

        Add(s + Classes[i].Name + ',');
        Add(Espacos + Classes[i].Parent + ',');

        if Classes[i].Comment <> '' then
           begin
           SLC.Text := Classes[i].Comment;
           for k := 0 to SLC.Count-1 do
             if k < SLC.Count-1 then
                Add(Espacos + QuotedStr(SLC[k]) +  '#13 +')
             else
                Add(Espacos + QuotedStr(SLC[k]) + ',');
           end
        else
           Add(Espacos + QuotedStr('') + ',');

        Add(Espacos + Classes[i].Category + ',');
        Add(Espacos + QuotedStr(Classes[i].CMN) + ',');
        Add(Espacos + 'Lib.Classes);');
        end;
      end;
  end;

var i: Integer;
    k: Integer;
    s: String;
    c: TpsClass;
begin
  Book.ActivePageIndex := 3;
  BookChange(Book);

  SLC := TStringList.Create;

  with S_mSaida, S_mSaida.Lines do
    begin
    BeginUpdate;
    Clear;

    // Cabeçalho
    Add('unit ' + G_edNU.Text + ';');
    Add('');
    Add('// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>');

    // Comentários Gerais
    if G_mCG.Lines.Count > 0 then
       begin
       Add('{');
       for i := 0 to G_mCG.Lines.Count-1 do Add('   ' + G_mCG.Lines[i]);
       Add('}');
       end;

    Add('');
    Add('interface');
    Add('uses drPascal;');
    Add('');
    Add('  procedure API(Lib: TLib);');
    Add('');
    Add('type');

    // Rotinas
    if Procs.Count > 0 then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Procs' else s := s + 'Procs';

       Add('  ' + s + ' = class(TFunctionServices)');
       Add('  public');
       Add('    class procedure AddProcsIn(Procs: TProcList); override;');

       for i := 0 to Procs.Count-1 do
         Add('    class procedure ' + Procs[i].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');

       Add('  end;');
       Add('');
       end;

    if Funcs.Count > 0 then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Functions' else s := s + 'Functions';

       Add('  ' + s + ' = class(TFunctionServices)');
       Add('  public');
       Add('    class procedure AddFunctionsIn(Functions: TFunctionList); override;');

       for i := 0 to Funcs.Count-1 do
         Add('    class procedure ' + Funcs[i].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');

       Add('  end;');
       Add('');
       end;

    // Classes
    for i := 0 to Classes.Count-1 do
      begin
      c := Classes[i];
      if c.Parent = '' then s := 'TpsClass' else s := c.Parent;
      Add('  ' + c.InternalName + ' = class(' + s + ')');

      if c.CMN <> '' then
         Add('    function CreateObject: TObject; override;');

      Add('    procedure AddMethods; override;');

      for k := 0 to c.Procs.Count-1 do
        Add('    class procedure ' + c.Procs[k].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');

      for k := 0 to c.Functions.Count-1 do
        Add('    class procedure ' + c.Functions[k].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');

      Add('  end;');
      Add('');
      end;

    Add('implementation');
    Add('');
    Add('  procedure API(Lib: TLib);');
    Add('  begin');
    GeraMioloDaAPI;
    Add('  end;');
    Add('');

    // Rotinas (procedimentos)
    if Procs.Count > 0 then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Procs' else s := s + 'Procs';

       Add('{ ' + s + ' }');
       Add('');

       for i := 0 to Procs.Count-1 do
         begin
         Add('class procedure ' + s + '.' + Procs[i].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
         Add('begin');
         Add('  // ...');
         Add('end;');
         Add('');
         end;

       Add('class procedure ' + s + '.AddProcsIn(Procs: TProcList);');
       Add('begin');
       Add('  with Procs do');
       Add('    begin');
       for i := 0 to Procs.Count-1 do GeraCabecalhoDaRotina(Procs[i], (i < Procs.Count-1));
       Add('    end;');
       Add('end;');
       Add('');
       end;

    // Rotinas (funções)
    if Funcs.Count > 0 then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Functions' else s := s + 'Functions';

       Add('{ ' + s + ' }');
       Add('');

       for i := 0 to Funcs.Count-1 do
         begin
         Add('class procedure ' + s + '.' + Funcs[i].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
         Add('begin');
         GeraMioloDaRotina(nil, Funcs[i]);
         Add('end;');
         Add('');
         end;

       Add('class procedure ' + s + '.AddFunctionsIn(Functions: TFunctionList);');
       Add('begin');
       Add('  with Functions do');
       Add('    begin');
       for i := 0 to Funcs.Count-1 do GeraCabecalhoDaRotina(Funcs[i], (i < Funcs.Count-1));
       Add('    end;');
       Add('end;');
       Add('');
       end;

    // Classes
    for i := 0 to Classes.Count-1 do
      begin
      c := Classes[i];

      Add('{ ' + c.InternalName + ' }');
      Add('');

      if c.CMN <> '' then
         begin
         Add(format('function %s.CreateObject: TObject;', [c.InternalName]));
         Add('begin');
         Add('  Result := Txxx.Create;');
         Add('end;');
         Add('');
         end;

      for k := 0 to c.Procs.Count-1 do
        begin
        Add('class procedure ' + c.InternalName + '.' + c.Procs[k].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
        Add('begin');
        GeraMioloDaRotina(c, c.Procs[k]);
        Add('end;');
        Add('');
        end;

      for k := 0 to c.Functions.Count-1 do
        begin
        Add('class procedure ' + c.InternalName + '.' + c.Functions[k].AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
        Add('begin');
        GeraMioloDaRotina(c, c.Functions[k]);
        Add('end;');
        Add('');
        end;

      Add(format('procedure %s.AddMethods;', [c.InternalName]));
      Add('begin');
      Add('  with Procs do');
      Add('    begin');
      for k := 0 to c.Procs.Count-1 do GeraCabecalhoDaRotina(c.Procs[k], (k < c.Procs.Count-1));
      Add('    end;');
      Add('');
      Add('  with Functions do');
      Add('    begin');
      for k := 0 to c.Functions.Count-1 do GeraCabecalhoDaRotina(c.Functions[k], (k < c.Functions.Count-1));
      Add('    end;');
      //...
      Add('end;');
      Add('');
      end;

    Add('end.');
    EndUpdate;
    end;

  SLC.Free;
end;

procedure TformPrincipal.FormCreate(Sender: TObject);
begin
  Funcs   := TFunctionList.Create;
  Procs   := TProcList.Create;
  Classes := TClassList.Create;

  M_cbClasse.ItemIndex := 0; // Nenhuma classe

  M_cbTipoPar.ItemIndex := 0; // pvtObject
  M_cbTipoParChange(M_cbTipoPar);

  BookChange(Book);
end;

procedure TformPrincipal.FormDestroy(Sender: TObject);
begin
  Funcs.Free;
  Procs.Free;
  Classes.Free;
end;

// Classes -------------------------------------------------------------------------

procedure TformPrincipal.C_btnAdicionarClick(Sender: TObject);
var c: TpsClass;
    s: String;
begin
  if C_edNome.Text = '' then
     ShowErrorAndGoto(['Digite o nome da Classe'], C_edNome);

  if C_lbClasses.Items.IndexOf(C_edNome.Text) > -1 then
     ShowErrorAndGoto(['Classe já declarada'], C_edNome);

  s := C_cbAncestral.Text;
  if s = '' then s := 'nil';

  c := TpsClass.Create(C_edNome.Text,
                       s,
                       C_mComment.Lines.Text,
                       C_edCat.Text,
                       C_edNMC.Text,
                       Classes);

  C_lbClasses.Items.Add(c.Name);
end;

procedure TformPrincipal.C_btnRemoverClick(Sender: TObject);
begin
  if C_lbClasses.ItemIndex > -1 then
     begin
     Classes.Remove(C_lbClasses.Items[C_lbClasses.ItemIndex]);
     C_lbClasses.Items.Delete(C_lbClasses.ItemIndex);
     Clear([C_edNome, C_cbAncestral, C_mComment, C_edCat, C_edNMC]);
     end;
end;

procedure TformPrincipal.C_btnModificarClick(Sender: TObject);
var c: TpsClass;
begin
  c := Classes.ClassByName(C_lbClasses.Items[C_lbClasses.ItemIndex]);

  c.Name      := C_edNome.Text;
  c.Parent    := C_cbAncestral.Text;
  c.Comment   := C_mComment.Lines.Text;
  c.Category  := C_edCat.Text;
  c.CMN       := C_edNMC.Text;
end;

// Métodos/Rotinas ---------------------------------------------------------------------

procedure TformPrincipal.M_btnAdicionaRotClick(Sender: TObject);
var f: TFunctionObject;
    a1, a2, a3: Array of String;
    i: Integer;
    sl: TStrings;
    fl: TFunctionList;
    s1, s2: String;
begin
  if M_cbClasse.ItemIndex < 1 {nenhuma classe} then
     if M_cbResultado.ItemIndex < 1 then
        fl := Procs
     else
        fl := Funcs
  else
     if M_cbResultado.ItemIndex < 1 then
        fl := Classes.ClassByName(M_cbClasse.Text).Procs
     else
        fl := Classes.ClassByName(M_cbClasse.Text).Functions;

  if fl.IndexOf(M_edNome.Text) > -1 then
     ShowErrorAndGoto(['Rotina ou Método já declarado'], M_edNome);

  if M_edNome.Text = '' then
     ShowErrorAndGoto(['Digite o nome do Método ou Rotina'], M_edNome);

  if M_edMA.Text = '' then
     ShowErrorAndGoto(['Digite o nome do método de acesso'], M_edMA);

  s1 := M_cbResultado.Text;
  s2 := M_cbClasseRes.Text;
  if s1 = '' then s1 := 'pvtNull';
  if s1 <> 'pvtObject' then s2 := 'TObject';

  if (s1 = 'pvtObject') and (s2 = '') then
     ShowErrorAndGoto(['Classe do Resultado não definida'], M_cbClasseRes);

  sl := nil;
  i := M_lbParametros.Items.Count;
  setLength(a1, i);
  setLength(a2, i);
  setLength(a3, i);
  for i := 0 to i-1 do
    begin
    StringToStrings(M_lbParametros.Items[i], sl, [':']);
    a1[i] := sl[0];
    a2[i] := sl[1];
    a3[i] := sl[2];
    end;
  sl.Free;

  f := TFunctionObject.Create(
         M_edNome.Text,
         M_mComment.Lines.Text,
         M_edCat.Text,
         a1,
         a2,
         a3,
         s1,
         s2,
         M_edMA.Text);

  fl.Add(f);

  MostrarRotinas;
  LimparRotinas;
end;

procedure TformPrincipal.M_btnRemoveRotClick(Sender: TObject);
var Rots: TFunctionList;
    s: String;
begin
  s := M_lbRotinas.Items[M_lbRotinas.ItemIndex];

  if s[1] = 'f' then
     begin
     s := copy(s, 10, Length(s));

     if M_cbClasse.ItemIndex < 1 then
        Rots := Funcs
     else
        Rots := Classes.ClassByName(M_cbClasse.Text).Functions
     end
  else
     begin
     s := copy(s, 11, Length(s));

     if M_cbClasse.ItemIndex < 1 then
        Rots := Procs
     else
        Rots := Classes.ClassByName(M_cbClasse.Text).Procs;
     end;

  Rots.Remove(s);
  LimparRotinas;
  MostrarRotinas;
end;

procedure TformPrincipal.M_btnModificaRotClick(Sender: TObject);
begin
  //
end;

procedure TformPrincipal.M_btnAdicionarParClick(Sender: TObject);
begin
  if (M_cbTipoPar.ItemIndex = 0) and (M_cbTipoParClasse.Text = '') then
     ShowErrorAndGoto(['Classe do Parâmetro não definida'], M_cbTipoParClasse);

  M_lbParametros.Items.Add(ObtemParametrosDaRotina);
end;

procedure TformPrincipal.M_btnRemoverParClick(Sender: TObject);
begin
  if M_lbParametros.ItemIndex > -1 then
     M_lbParametros.Items.Delete(M_lbParametros.ItemIndex);
end;

procedure TformPrincipal.M_btnModificarParClick(Sender: TObject);
begin
  if M_lbParametros.ItemIndex > -1 then
     M_lbParametros.Items[M_lbParametros.ItemIndex] := ObtemParametrosDaRotina;
end;

procedure TformPrincipal.C_lbClassesClick(Sender: TObject);
var c: TpsClass;
begin
  c := Classes.ClassByName(C_lbClasses.Items[C_lbClasses.ItemIndex]);

  C_edNome.Text      := c.Name;
  C_cbAncestral.Text := c.Parent;
  C_mComment.Text    := c.Comment;
  C_edCat.Text       := c.Category;
  C_edNMC.Text       := c.CMN;
end;

procedure TformPrincipal.MostrarRotinas;

  procedure Mostrar(Fs, Ps: TFunctionList);
  var i: Integer;
  begin
    M_lbRotinas.Clear;

    for i := 0 to Fs.Count-1 do
      M_lbRotinas.Items.AddObject('function ' + Fs[i].Name, Fs[i]);

    for i := 0 to Ps.Count-1 do
      M_lbRotinas.Items.AddObject('procedure ' + Ps[i].Name, Ps[i]);
  end;

var c: TpsClass;
begin
  if M_cbClasse.ItemIndex < 1 {nenhuma classe} then
     Mostrar(Funcs, Procs)
  else
     begin
     c := Classes.ClassByName(M_cbClasse.Text);
     Mostrar(c.Functions, c.Procs);
     end;
end;

procedure TformPrincipal.M_cbClasseChange(Sender: TObject);
begin
  LimparRotinas;
  MostrarRotinas;
end;

procedure TformPrincipal.LimparRotinas;
begin
  Clear([M_edNome,
         M_mComment,
         M_edCat,
         M_cbResultado,
         M_cbClasseRes,
         M_edMA,
         M_lbParametros]);
end;

function TformPrincipal.ObtemParametrosDaRotina: String;
begin
  Result := M_cbTipoPar.Text + ':';

  if M_cbTipoPar.ItemIndex = 0 then
     Result := Result + M_cbTipoParClasse.Text + ':'
  else
     Result := Result + 'nil:';

  if M_rbC.Checked then
     Result := Result + 'False'
  else
     Result := Result + 'True';
end;

procedure TformPrincipal.M_cbTipoParChange(Sender: TObject);
begin
  M_cbTipoParClasse.Enabled := M_cbTipoPar.ItemIndex = 0;
  if not M_cbTipoParClasse.Enabled then Clear([M_cbTipoParClasse]);
  M_rbR.Checked := M_cbTipoParClasse.Enabled;
  M_rbR.Enabled := not M_cbTipoParClasse.Enabled;
  M_rbC.Checked := not M_rbR.Checked;
end;

procedure TformPrincipal.M_cbResultadoChange(Sender: TObject);
begin
  M_cbClasseRes.Enabled := (M_cbResultado.ItemIndex = 1) and
                           (M_cbClasseRes.Items.Count > 0);

  if M_cbClasseRes.Enabled then M_cbClasseRes.ItemIndex := 0;
end;

procedure TformPrincipal.BookChange(Sender: TObject);
begin
  case Book.ActivePageIndex of
    0: begin
       width  := 559;
       height := 264;
       end;

    1: begin
       width  := 632;
       height := 324;
       end;

    2: begin
       width  := 767;
       height := 459;
       M_cbClasse.Items.Assign(C_lbClasses.Items);
       M_cbClasse.Items.Insert(0, '');
       M_cbTipoParClasse.Items.Assign(C_lbClasses.Items);
       M_cbClasseRes.Items.Assign(C_lbClasses.Items);
       end;

    3: begin
       width  := 767;
       height := 459;
       end;
  end;
end;

procedure TformPrincipal.C_cbAncestralDropDown(Sender: TObject);
begin
  C_cbAncestral.Items.Assign(C_lbClasses.Items);
  C_cbAncestral.Items.Insert(0, '');
end;

procedure TformPrincipal.M_edNomeExit(Sender: TObject);
begin
  M_edMA.Text := 'am' + M_edNome.Text
end;

procedure TformPrincipal.Menu_SalvarComoClick(Sender: TObject);
begin
  Save.FileName := G_edNU.Text + '.pas';
  if Save.Execute then
     S_mSaida.Lines.SaveToFile(Save.FileName);
end;

procedure TformPrincipal.M_lbRotinasClick(Sender: TObject);
var f : TFunctionObject;
    i : Integer;
    s : String;
begin
  f := TFunctionObject(M_lbRotinas.Items.Objects[M_lbRotinas.ItemIndex]);

  M_edNome.Text         := f.Name;
  M_edCat.Text          := f.Category;
  M_mComment.Lines.Text := f.Comment;
  M_cbResultado.Text    := f.ResType;
  M_cbClasseRes.Text    := f.ResObjectClass;
  M_edMA.Text           := f.AccessMethod;

  M_lbParametros.Clear;
  for i := 0 to f.Parameters-1 do
    M_lbParametros.Items.Add(
      Format('%s:%s:%s', [f.ParType[i], f.ParClass[i], f.ParCat[i]]));
end;

procedure TformPrincipal.btnInfoClick(Sender: TObject);
begin
  ShowMessage('Gerador de Bibliotecas do Pascal Script'#13 +
              '                      Versão 1.01'#13 +
              ''#13 +
              'Direitos: Adriano Rochedo Conceição, 1999 - 2000'#13 +
              'EMail: rochedo@ufpel.tche.br');
end;

end.

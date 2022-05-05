unit Principal_2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons,
  gb_Classes, Frame_Parametros, RAEditor, RAHLEditor, ImgList, Menus;

type
  TDLG_Principal = class(TForm)
    Panel1: TPanel;
    Arvore: TTreeView;
    Book: TNotebook;
    Splitter1: TSplitter;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    C_edNome: TEdit;
    C_cbAncestral: TComboBox;
    C_mComment: TMemo;
    C_edCat: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label17: TLabel;
    G_edNU: TEdit;
    G_mCG: TMemo;
    G_edRotPrefix: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    M_edNome: TEdit;
    M_mComment: TMemo;
    M_edCat: TEdit;
    M_cbResultado: TComboBox;
    M_edMA: TEdit;
    M_cbClasseRes: TComboBox;
    Label16: TLabel;
    btnAdicionar: TSpeedButton;
    btnRemover: TSpeedButton;
    btnAtualizar: TSpeedButton;
    C_cbInstancia: TCheckBox;
    C_Parametros: Tfrm_Parametros;
    M_Parametros: Tfrm_Parametros;
    M_Codigo: TRAHLEditor;
    S_mSaida: TRAHLEditor;
    Imagens: TImageList;
    Save: TSaveDialog;
    Menu: TPopupMenu;
    Menu_SalvarComo: TMenuItem;
    MenuCodigo: TPopupMenu;
    MenuItem1: TMenuItem;
    procedure ArvoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAdicionarClick(Sender: TObject);
    procedure btnRemoverClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure C_cbInstanciaClick(Sender: TObject);
    procedure M_CodigoCompletionDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Menu_SalvarComoClick(Sender: TObject);
    procedure M_edNomeKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItem1Click(Sender: TObject);
    procedure M_ParametrosbtnAdicionarParClick(Sender: TObject);
    procedure ArvoreChange(Sender: TObject; Node: TTreeNode);
  private
    FnoMestre, FnoClasse: TTreeNode;
    No_Rot, No_Clas: TTreeNode;

    procedure DesabilitarOpcoes;
    procedure MostraNo(no: TTreeNode);
    procedure MostraRotina(Rot: TFunctionObject);
    procedure MostraClasse(Clas: TpsClass);
    procedure AtualizaRotina(Rot: TFunctionObject);
    procedure AtualizaClasse(Clas: TpsClass);
    procedure MostraCodigo;
  public
    { Public declarations }
  end;

var
  DLG_Principal: TDLG_Principal;

implementation
uses WinUtils, SysUtilsEx;

{$R *.DFM}

procedure TDLG_Principal.ArvoreClick(Sender: TObject);
var no: TTreeNode;
begin
  DesabilitarOpcoes;
  FnoClasse := nil;
  no := Arvore.Selected;
  if no.Level = 0 then
     begin
     FnoMestre := no;

     if (no.Text = 'Rotinas') or (no.Text = 'Classes') then
        begin
        Book.PageIndex := 0;
        btnAdicionar.Enabled := True;
        end else

     if no.Text = 'Unidade' then
        Book.PageIndex := 0 else

     if no.Text = 'Código' then
        begin
        Book.PageIndex := 3;
        MostraCodigo;
        end;
     end
  else
     begin
     FnoMestre := no.Parent;
     FnoMestre.AlphaSort;
     btnRemover.Enabled := True;
     btnAtualizar.Enabled := True;
     MostraNo(no);
     end;
end;

procedure TDLG_Principal.DesabilitarOpcoes;
begin
  btnAdicionar.Enabled := False;
  btnRemover.Enabled := False;
  btnAtualizar.Enabled := False;
end;

procedure TDLG_Principal.FormCreate(Sender: TObject);
begin
  No_Rot   := Arvore.Items[1];
  No_Clas  := Arvore.Items[2];

  Book.PageIndex := 0;
end;

procedure TDLG_Principal.MostraNo(no: TTreeNode);
begin
  if FNoMestre.Text = 'Classes' then
     begin
     FnoClasse := no;
     btnAdicionar.Enabled := True;
     end;

  if TObject(no.Data) is TFunctionObject then
     begin
     Book.PageIndex := 1;
     MostraRotina(TFunctionObject(no.Data));
     end else

  if TObject(no.Data) is TpsClass then
     begin
     Book.PageIndex := 2;
     MostraClasse(TpsClass(no.Data));
     end
end;

procedure TDLG_Principal.btnAdicionarClick(Sender: TObject);
var no, noPai: TTreeNode;
    s: String;
   obj: TObject;
   ImageIndex: Integer;
begin
  if FnoClasse <> nil then
     begin
     noPai := FnoClasse;
     s := 'Metodo' + IntToStr(noPai.Count + 1);
     obj := TFunctionObject.Create;
     TFunctionObject(obj).Name := s;
     TFunctionObject(obj).AccessMethod := 'am_' + s;
     ImageIndex := 1;
     end
  else
     begin
     noPai := FnoMestre;
     if noPai.Text = 'Rotinas' then
        begin
        s := 'Rotina' + IntToStr(noPai.Count + 1);
        obj := TFunctionObject.Create;
        TFunctionObject(obj).Name := s;
        TFunctionObject(obj).AccessMethod := 'am_' + s;
        ImageIndex := 1;
        end
     else
        begin
        s := 'TClasse' + IntToStr(noPai.Count + 1);
        obj := TpsClass.Create;
        TpsClass(obj).Name := s;
        ImageIndex := 4;
        end;
     end;

  no := Arvore.Items.AddChildObject(noPai, s, obj);
  no.ImageIndex := ImageIndex;
  no.SelectedIndex := ImageIndex;
  no.Selected := True;
  ArvoreClick(nil);
end;

procedure TDLG_Principal.btnAtualizarClick(Sender: TObject);
var no: TTreeNode;
begin
  no := Arvore.Selected;
  if TObject(no.Data) is TFunctionObject then
     begin
     AtualizaRotina(TFunctionObject(no.Data));
     no_Rot.AlphaSort;
     end
  else
  if TObject(no.Data) is TpsClass then
     begin
     AtualizaClasse(TpsClass(no.Data));
     no_Clas.AlphaSort;
     end;
end;

procedure TDLG_Principal.btnRemoverClick(Sender: TObject);
var no: TTreeNode;
    i: Integer;
begin
  no := Arvore.Selected;

  if TObject(no.Data) is TFunctionObject then
     TObject(no.Data).Free
  else
  if TObject(no.Data) is TpsClass then
     begin
     // Destroi todos os métodos desta classe
     for i := 0 to no.Count-1 do
       TObject(no.Item[i].Data).Free;

     // Destroi a classe
     TObject(no.Data).Free;
     end;

  no.Delete;
  ArvoreClick(nil);
end;

procedure TDLG_Principal.MostraClasse(Clas: TpsClass);
var i: Integer;
begin
  C_edNome.Text      := Clas.Name;
  C_cbAncestral.Text := Clas.Parent;
  C_mComment.Text    := Clas.Comment;
  C_edCat.Text       := Clas.Category;

  C_cbInstancia.Checked := (Clas.Parameters > -1);

  C_Parametros.lbParametros.Clear;
  for i := 0 to Clas.Parameters-1 do
    C_Parametros.lbParametros.Items.Add(
      Format('%s:%s:%s', [Clas.ParType[i], Clas.ParClass[i], Clas.ParCat[i]]));
end;

procedure TDLG_Principal.MostraRotina(Rot: TFunctionObject);
var i : Integer;
begin
  M_edNome.Text         := Rot.Name;
  M_edCat.Text          := Rot.Category;
  M_mComment.Lines.Text := Rot.Comment;
  M_cbResultado.Text    := Rot.ResType;
  M_cbClasseRes.Text    := Rot.ResObjectClass;
  M_edMA.Text           := Rot.AccessMethod;

  M_Codigo.BeginUpdate;
  M_Codigo.Lines.Assign(Rot.Code);
  M_Codigo.EndUpdate;

  M_Parametros.lbParametros.Clear;
  for i := 0 to Rot.Parameters-1 do
    M_Parametros.lbParametros.Items.Add(
      Format('%s:%s:%s', [Rot.ParType[i], Rot.ParClass[i], Rot.ParCat[i]]));
end;

procedure TDLG_Principal.FormDestroy(Sender: TObject);
var i, j: Integer;
    no: TTreeNode;
begin
  // Destroi todas as rotinas
  for i := 0 to no_Rot.Count-1 do
    TObject(no_Rot.Item[i].Data).Free;

  for i := 0 to no_Clas.Count-1 do
    begin
    no := no_Clas.Item[i];

    // Destroi todos os métodos desta classe
    for j := 0 to no.Count-1 do
      TObject(no.Item[j].Data).Free;

    // Destroi a classe
    TObject(no.Data).Free;
    end
end;

procedure TDLG_Principal.AtualizaClasse(Clas: TpsClass);
var a1, a2, a3: Array of String;
    i: Integer;
    sl: TStrings;
    s: String;
begin
  s := C_cbAncestral.Text;
  if s = '' then s := 'nil';

  Clas.Name       := C_edNome.Text;
  Clas.Parent     := s;
  Clas.Comment    := C_mComment.Lines.Text;
  Clas.Category   := C_edCat.Text;

  if C_cbInstancia.Checked then
     begin
     sl := nil;
     i := C_Parametros.lbParametros.Items.Count;
     Clas.Parameters := i;
     setLength(a1, i);
     setLength(a2, i);
     setLength(a3, i);
     for i := 0 to i-1 do
       begin
       StringToStrings(C_Parametros.lbParametros.Items[i], sl, [':']);
       a1[i] := sl[0];
       a2[i] := sl[1];
       a3[i] := sl[2];
       end;
     sl.Free;

     Clas.setParTypes(a1);
     Clas.setParClasses(a2);
     Clas.setParCats(a3);
     end
  else
     Clas.Parameters := -1;

  Arvore.Selected.Text := Clas.Name;
end;

procedure TDLG_Principal.AtualizaRotina(Rot: TFunctionObject);
var a1, a2, a3: Array of String;
    i: Integer;
    sl: TStrings;
    s1, s2: String;
begin
  s1 := M_cbResultado.Text;
  s2 := M_cbClasseRes.Text;
  if s1 = '' then s1 := 'pvtNull';
  if s1 <> 'pvtObject' then s2 := 'TObject';

  if (s1 = 'pvtObject') and (s2 = '') then
     ShowErrorAndGoto(['Classe do Resultado não definida'], M_cbClasseRes);

  sl := nil;
  i := M_Parametros.lbParametros.Items.Count;
  Rot.Parameters := i;
  setLength(a1, i);
  setLength(a2, i);
  setLength(a3, i);
  for i := 0 to i-1 do
    begin
    StringToStrings(M_Parametros.lbParametros.Items[i], sl, [':']);
    a1[i] := sl[0];
    a2[i] := sl[1];
    a3[i] := sl[2];
    end;
  sl.Free;

  Rot.Name            := M_edNome.Text;
  Rot.Comment         := M_mComment.Lines.Text;
  Rot.Category        := M_edCat.Text;
  Rot.ResType         := s1;
  Rot.ResObjectClass  := s2;
  Rot.AccessMethod    := M_edMA.Text;

  Rot.setParTypes(a1);
  Rot.setParClasses(a2);
  Rot.setParCats(a3);

  Rot.Code.Assign(M_Codigo.Lines);

  Arvore.Selected.Text := Rot.Name;

  //fl.Add(f);

  //MostrarRotinas;
  //LimparRotinas;
end;

procedure TDLG_Principal.MostraCodigo;
var SLC: TStrings;
    c: TpsClass;
    Func: TFunctionObject;
    TemProc, TemFunc: Boolean;

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
      i: Integer;
  begin
    if Func.Code.Count = 0 then
       begin
       S_mSaida.Lines.Add('begin');
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
       S_mSaida.Lines.Add('end;');   
       end
    else
       for i := 0 to Func.Code.Count-1 do
         S_mSaida.Lines.Add(Func.Code[i]);
  end;

  procedure GeraMioloDaAPI;
  var s, Espacos: String;
      i, k: Integer;
  begin
    with S_mSaida.Lines do
      begin
      if TemProc then
         begin
         s := G_edRotPrefix.Text;
         if s = '' then s := '    TGeral_Procs' else s := '    ' + s + 'Procs';
         Add(s + '.AddProcsIn(Lib.Procs);');
         end;

      if TemFunc then
         begin
         s := G_edRotPrefix.Text;
         if s = '' then s := '    TGeral_Functions' else s := '    ' + s + 'Functions';
         Add(s + '.AddFunctionsIn(Lib.Functions);');
         end;

      for i := 0 to No_Clas.Count-1 do
        begin
        c := TpsClass(No_Clas[i].Data);

        s := c.InternalName;
        s := '    ' + s + '.Create(';
        Espacos := StringOfChar(' ', Length(s));

        Add(s + c.Name + ',');
        Add(Espacos + c.Parent + ',');

        if c.Comment <> '' then
           begin
           SLC.Text := c.Comment;
           for k := 0 to SLC.Count-1 do
             if k < SLC.Count-1 then
                Add(Espacos + QuotedStr(SLC[k]) +  '#13 +')
             else
                Add(Espacos + QuotedStr(SLC[k]) + ',');
           end
        else
           Add(Espacos + QuotedStr('') + ',');

        Add(Espacos + c.Category + ',');

        // Tipo dos parâmetros
        s := '[';
        for k := 0 to c.Parameters-1 do
          begin
          s := s + c.ParType[k];
          if k < c.Parameters-1 then s := s + ', ';
          end;
        s := s + '],';
        Add(Espacos + s);

        // Tipo dos Objetos se parametro = pvtObject
        s := '[';
        for k := 0 to c.Parameters-1 do
          begin
          s := s + c.ParClass[k];
          if k < c.Parameters-1 then s := s + ', ';
          end;
        s := s + '],';
        Add(Espacos + s);

        // Categoria dos parâmetros
        s := '[';
        for k := 0 to c.Parameters-1 do
          begin
          s := s + c.ParCat[k];
          if k < c.Parameters-1 then s := s + ', ';
          end;
        s := s + '],';
        Add(Espacos + s);

        if c.Parameters = -1 then s := 'False' else s := 'True';
        Add(Espacos + s + ',');
        Add(Espacos + 'Lib.Classes);');
        end;
      end;
  end;

var i: Integer;
    k: Integer;
    s, sp: String;
begin
  TemProc := False;
  TemFunc := False;
  for i := 0 to No_Rot.Count-1 do
    begin
    Func := TFunctionObject(No_Rot[i].Data);
    if Func.ResType =  'pvtNull' then TemProc := True;
    if Func.ResType <> 'pvtNull' then TemFunc := True;
    end;

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
    Add('uses drBase;');
    Add('');
    Add('  procedure API(Lib: TLib);');
    Add('');
    Add('type');

    // Rotinas
    if TemProc then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Procs' else s := s + 'Procs';

       Add('  ' + s + ' = class(TFunctionServices)');
       Add('  public');
       Add('    class procedure AddProcsIn(Procs: TProcList); override;');

       for i := 0 to No_Rot.Count-1 do
         begin
         Func := TFunctionObject(No_Rot[i].Data);
         if Func.ResType = 'pvtNull' then
            Add('    class procedure ' + Func.AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
         end;

       Add('  end;');
       Add('');
       end;

    if TemFunc then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Functions' else s := s + 'Functions';

       Add('  ' + s + ' = class(TFunctionServices)');
       Add('  public');
       Add('    class procedure AddFunctionsIn(Functions: TFunctionList); override;');

       for i := 0 to No_Rot.Count-1 do
         begin
         Func := TFunctionObject(No_Rot[i].Data);
         if Func.ResType <> 'pvtNull' then
            Add('    class procedure ' + Func.AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
         end;

       Add('  end;');
       Add('');
       end;

    // Classes
    for i := 0 to No_Clas.Count-1 do
      begin
      c := TpsClass(No_Clas[i].Data);

      if c.Parent = '' then s := 'TpsClass' else s := c.Parent;
      Add('  ' + c.InternalName + ' = class(' + s + ')');

      if c.Parameters <> -1 then
         Add('    function CreateObject(Stack: TexeStack): TObject; override;');

      Add('    procedure AddMethods; override;');

      for k := 0 to No_Clas[i].Count-1 do
        begin
        Func := TFunctionObject(No_Clas[i][k].Data);
        Add('    class procedure ' + Func.AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
        end;

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
    if TemProc then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Procs' else s := s + 'Procs';

       Add('{ ' + s + ' }');
       Add('');

       for i := 0 to No_Rot.Count-1 do
         begin
         Func := TFunctionObject(No_Rot[i].Data);
         if Func.ResType = 'pvtNull' then
            begin
            Add('class procedure ' + s + '.' + Func.AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
            GeraMioloDaRotina(nil, Func);
            Add('');
            end;
         end;

       Add('class procedure ' + s + '.AddProcsIn(Procs: TProcList);');
       Add('begin');
       Add('  with Procs do');
       Add('    begin');

       for i := 0 to No_Rot.Count-1 do
         begin
         Func := TFunctionObject(No_Rot[i].Data);
         if Func.ResType = 'pvtNull' then
            GeraCabecalhoDaRotina(Func, {(i < Funcs.Count-1)} True);
         end;

       Add('    end;');
       Add('end;');
       Add('');
       end;

    // Rotinas (funções)
    if TemFunc then
       begin
       s := G_edRotPrefix.Text;
       if s = '' then s := 'TGeral_Functions' else s := s + 'Functions';

       Add('{ ' + s + ' }');
       Add('');

       for i := 0 to No_Rot.Count-1 do
         begin
         Func := TFunctionObject(No_Rot[i].Data);
         if Func.ResType <> 'pvtNull' then
            begin
            Add('class procedure ' + s + '.' + Func.AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
            GeraMioloDaRotina(nil, Func);
            Add('');
            end;
         end;

       Add('class procedure ' + s + '.AddFunctionsIn(Functions: TFunctionList);');
       Add('begin');
       Add('  with Functions do');
       Add('    begin');

       for i := 0 to No_Rot.Count-1 do
         begin
         Func := TFunctionObject(No_Rot[i].Data);
         if Func.ResType <> 'pvtNull' then
            GeraCabecalhoDaRotina(Func, {(i < Funcs.Count-1)} True);
         end;

       Add('    end;');
       Add('end;');
       Add('');
       end;

    // Classes
    for i := 0 to No_Clas.Count-1 do
      begin
      c := TpsClass(No_Clas[i].Data);

      Add('{ ' + c.InternalName + ' }');
      Add('');

      if c.Parameters <> -1 then
         begin
         Add(format('function %s.CreateObject(Stack: TExeStack): TObject;', [c.InternalName]));
         Add('begin');
         s := Format('  Result := %s.Create', [c.Name]);

         if c.Parameters > 0 then
            begin
            s := s + '(';
            for k := 0 to c.Parameters-1 do
              begin
              if c.ParType[k] = 'pvtInteger' then sp := Format('Stack.AsInteger(%d)', [K+1]) else
              if c.ParType[k] = 'pvtFloat'   then sp := Format('Stack.AsFloat(%d)',   [K+1]) else
              if c.ParType[k] = 'pvtString'  then sp := Format('Stack.AsString(%d)',  [K+1]) else
              if c.ParType[k] = 'pvtBoolean' then sp := Format('Stack.AsBoolean(%d)', [K+1]) else
              if c.ParType[k] = 'pvtObject'  then sp := Format('Stack.AsObject(%d)',  [K+1]);
              if k > 0 then s := s + ', ' + sp else s := s + sp;
              end;
            s := s + ')';
            end;

         Add(s);
         Add('end;');
         Add('');
         end;

      for k := 0 to No_Clas[i].Count-1 do
        begin
        Func := TFunctionObject(No_Clas[i][k].Data);
        Add('class procedure ' + c.InternalName + '.' + Func.AccessMethod + '(Const Func_Name: String; Stack: TexeStack);');
        GeraMioloDaRotina(c, Func);
        Add('');
        end;

      Add(format('procedure %s.AddMethods;', [c.InternalName]));
      Add('begin');
      Add('  with Procs do');
      Add('    begin');

      for k := 0 to No_Clas[i].Count-1 do
        begin
        Func := TFunctionObject(No_Clas[i][k].Data);
        if Func.ResType = 'pvtNull' then
           GeraCabecalhoDaRotina(Func, {(k < c.Procs.Count-1)} true);
        end;

      Add('    end;');
      Add('');
      Add('  with Functions do');
      Add('    begin');

      for k := 0 to No_Clas[i].Count-1 do
        begin
        Func := TFunctionObject(No_Clas[i][k].Data);
        if Func.ResType <> 'pvtNull' then
           GeraCabecalhoDaRotina(Func, {(k < c.Procs.Count-1)} true);
        end;

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

procedure TDLG_Principal.C_cbInstanciaClick(Sender: TObject);
begin
  C_Parametros.Visible := C_cbInstancia.Checked;
end;

procedure TDLG_Principal.M_CodigoCompletionDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Offset, W  : Integer;
  ImageIndex : integer;
  S          : string;
begin
  Offset := 3;
  with Control as TListBox, (Control.Owner as TRAHLEditor).Completion do
    begin
    Canvas.FillRect(Rect);
    case Mode of
      cmIdentifers :
        begin
        ImageIndex := StrToInt(Trim(SubStr(Items[Index], 2, Separator))) - 1;
        Imagens.Draw(Canvas, Rect.Left + 2, Rect.Top, ImageIndex);
        Canvas.TextOut(Rect.Left + 3*Offset + Imagens.Width, Rect.Top +2, SubStr(Items[Index], 0, Separator));
        S := Trim(SubStr(Items[Index], 1, Separator));
        W := Canvas.TextWidth(S);
        Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2, S);
        end;

      cmTemplates :
        begin
        Canvas.TextOut(Rect.Left + Offset, Rect.Top +2, SubStr(Items[Index], 1, Separator));
        Canvas.Font.Style := [fsBold];
        S := SubStr(Items[Index], 0, Separator);
        W := Canvas.TextWidth(S);
        Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2, S);
        end;
      end; // case
    end; // with
end;

procedure TDLG_Principal.Menu_SalvarComoClick(Sender: TObject);
begin
  Save.FileName := G_edNU.Text + '.pas';
  if Save.Execute then
     if MessageDLG('Arquivo já existente. Tem certeza ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        S_mSaida.Lines.SaveToFile(Save.FileName);
end;

procedure TDLG_Principal.M_edNomeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  M_edMA.Text := 'am_' + M_edNome.Text;
end;

procedure TDLG_Principal.MenuItem1Click(Sender: TObject);
var s, Classe, Metodo, Pars: String;
    no: TTreeNode;
    sl: TStrings;
    i: Integer;
begin
  no := Arvore.Selected;
  if TObject(no.Parent.Data) is TpsClass then
     Classe := no.Parent.Text
  else
     Classe := '';

  with M_Codigo.Lines do
    begin
    M_Codigo.BeginUpdate;
    Clear;
    if Classe <> '' then Add('var o: ' + Classe + ';');
    Add('begin');
    if Classe <> '' then
       begin
       Metodo := 'o.' + M_edNome.Text;
       Add('  o := ' + Classe + '(Stack.AsObject(' +
           IntToStr(M_Parametros.lbParametros.Items.Count + 1) +'));');
       end
    else
       Metodo := M_edNome.Text;

    if M_Parametros.lbParametros.Items.Count > 0 then
       begin
       Pars := '(';
       sl := nil;
       for i := 0 to M_Parametros.lbParametros.Items.Count-1 do
         begin
         StringToStrings(M_Parametros.lbParametros.Items[i], sl, [':']);
         if sl[0] = 'pvtObject'  then s := Format('%s(Stack.AsObject(%d))', [sl[1], i+1]) else
         if sl[0] = 'pvtInteger' then s := Format('Stack.AsInteger(%d)',    [i+1]) else
         if sl[0] = 'pvtReal'    then s := Format('Stack.AsFloat(%d)',      [i+1]) else
         if sl[0] = 'pvtBoolean' then s := Format('Stack.AsBoolean(%d)',    [i+1]) else
         if sl[0] = 'pvtString'  then s := Format('Stack.AsString(%d)',     [i+1]);
         if i = 0 then Pars := Pars + s else Pars := Pars + ', ' + s;
         end;
       sl.Free;
       Pars := Pars + ')';
       end
    else
       Pars := '';

    Metodo := Metodo + Pars;

    if M_cbResultado.ItemIndex > 0 then
       case M_cbResultado.ItemIndex of
         1: s := '  Stack.PushObject('  + Metodo + ')';
         2: s := '  Stack.PushInteger(' + Metodo + ')';
         3: s := '  Stack.PushFloat('   + Metodo + ')';
         4: s := '  Stack.PushBoolean(' + Metodo + ')';
         5: s := '  Stack.PushString('  + Metodo + ')';
         end
    else
       s := Metodo;

    Add(s);
    Add('end;');
    M_Codigo.EndUpdate;
    end;
end;

procedure TDLG_Principal.M_ParametrosbtnAdicionarParClick(Sender: TObject);
begin
  M_Parametros.btnAdicionarParClick(Sender);
end;

procedure TDLG_Principal.ArvoreChange(Sender: TObject; Node: TTreeNode);
begin
  ArvoreClick(nil);
end;

end.

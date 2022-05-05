// versão 2.2
{
  As bibliotecas do PascalScript precisam estar definidas no arquivo
  "PascalScriptLibs.ini" localizado na Pasta do executavel ou/e dentro
  da sub-pasta "DLLs".
}

unit psEditor;

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, Graphics, Controls, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, ToolWin, ImgList, Menus,
  Execfile,
  Lib_GlobalObjects,
  psBase,
  psCore,
  psEditorBaseComp,
  psEditorComp;

type
  TpsEditor = class(TForm)
    Panel1: TPanel;
    btnSave: TSpeedButton;
    btnNew: TSpeedButton;
    btnCompile: TSpeedButton;
    btnRun: TSpeedButton;
    btnByteCode: TSpeedButton;
    btnHelp: TSpeedButton;
    Info: TListBox;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    btnOpen: TSpeedButton;
    Open: TOpenDialog;
    Save: TSaveDialog;
    Panel2: TPanel;
    btnStop: TSpeedButton;
    PainelPai: TPanel;
    cbOtimizar: TCheckBox;
    btnSalvarComo: TSpeedButton;
    btnFechar: TSpeedButton;
    Arvore: TTreeView;
    Splitter8: TSplitter;
    Imagens: TImageList;
    Menu_Arvore: TPopupMenu;
    ilCompletions: TImageList;
    GutterImages: TImageList;
    Codigo: TScriptPascalEditor;
    btnImprimir: TSpeedButton;
    PrintDialog: TPrintDialog;
    Menu_Loc: TMenuItem;
    Menu_Documentar: TMenuItem;
    Menu_ComoHTML: TMenuItem;
    Menu_ComoTexto: TMenuItem;
    procedure Codigo_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Codigo_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
    procedure btnByteCodeClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure Codigo_Change(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure lbFunctions_Click(Sender: TObject);
    procedure lbProcs_Click(Sender: TObject);
    procedure InfoDblClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnFecharClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ArvoreClick(Sender: TObject);
    procedure ArvoreDblClick(Sender: TObject);
    procedure Menu_ExportarHTMLClick(Sender: TObject);
    procedure Menu_ExportarTextoClick(Sender: TObject);
    procedure Codigo_PaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure Codigo_CompletionDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ArvoreChange(Sender: TObject; Node: TTreeNode);
    procedure btnImprimirClick(Sender: TObject);
    procedure Menu_LocClick(Sender: TObject);
    procedure Menu_ArvorePopup(Sender: TObject);
  private
    FCompiler    : TPascalScript;
    FFileName    : String;
    FRoot        : String;
    FMostrarVars : Boolean;

    No_Classes  : TTreeNode;
    No_Funcs    : TTreeNode;
    No_Vars     : TTreeNode;
    No_Vars_Loc : TTreeNode;
    No_Vars_Pre : TTreeNode;

    procedure ListaVariaveis;
    procedure SetFN(const Value: String);

    procedure SetCanRun(Value: Boolean);
    procedure SetCanStop(Value: Boolean);
    procedure SetCanShowByteCode(Value: Boolean);
    procedure SetCanOptimize(Value: Boolean);
    procedure SetOptimize(Value: Boolean);

    // Eventos
    procedure BeforeRun(Sender: TObject);
    procedure AfterRun(Sender: TObject);

    procedure MostraBiblioteca();
    procedure MostraFuncoes(Funcs: TList; Lugar: TTreeNode; Categorizar: boolean);
    procedure MostraProcs(Procs: TList; Lugar: TTreeNode; Categorizar: Boolean);
    procedure MostraClasses(Classes: TClassList);
  public
    constructor Create();
    destructor Destroy; override;

    property Compiler: TPascalScript read FCompiler;
    property FileName: String read FFileName write SetFN;
    property Root: String read FRoot write FRoot;

    property CanRun          : Boolean write SetCanRun;
    property CanStop         : Boolean write SetCanStop;
    property CanShowByteCode : Boolean write SetCanShowByteCode;
    property CanOptimize     : Boolean write SetCanOptimize;

    property Optimize        : Boolean write SetOptimize;
  end;

  function EditScript(APIs: Array of TAPI_Proc; Const FileName: String;
                      CODE: TStrings; Variables: Array of TVariable;
                      GlobalObjects: TGlobalObjects): String; overload;

  function EditScript(Lib: TLib; Const FileName: String;
                      CODE: TStrings; Variables: Array of TVariable;
                      GlobalObjects: TGlobalObjects): String; overload;

  function RunScript(APIs: Array of TAPI_Proc; Const FileName: String;
                     CODE: TStrings; Variables: Array of TVariable;
                     GlobalObjects: TGlobalObjects): String; overload;

  function RunScript(Lib: TLib; Const FileName, Root: String;
                     CODE: TStrings; Variables: Array of TVariable;
                     GlobalObjects: TGlobalObjects;
                     Modal: Boolean): String; overload;
implementation
uses SysUtilsEx, ShellAPI, TreeViewUtils, fileCTRL, OutPut, ByteCodeDLG;

  function EditScript(APIs: Array of TAPI_Proc; Const FileName: String;
                      CODE: TStrings; Variables: Array of TVariable;
                      GlobalObjects: TGlobalObjects): String;
  var d: TpsEditor;
      i: Integer;
  begin
    d := TpsEditor.Create();
    try
      if FileName <> '' then d.FileName := FileName;

      for i := 0 to High(APIs) do
        d.Compiler.Include(APIs[i]);

      for i := 0 to High(Variables) do
        d.Compiler.Variables.AddVar(Variables[i]);

      d.Compiler.GlobalObjects := GlobalObjects;

      d.CanRun           := True;
      d.CanStop          := False;
      d.CanShowByteCode  := True;
      d.Optimize         := False;
      d.CanOptimize      := False;
      d.btnCompile.Hint  := ' Verificar Script ';
      d.Compiler.GerCODE := True;

      d.MostraBiblioteca();
      d.ListaVariaveis();

      if CODE <> nil then d.Codigo.Lines.Assign(CODE);

      d.ShowModal;
      Result := d.FileName;
    finally
      d.Free;
    end;
  end;

  function RunScript(APIs: Array of TAPI_Proc; Const FileName: String;
                     CODE: TStrings; Variables: Array of TVariable;
                     GlobalObjects: TGlobalObjects): String;
  var d: TpsEditor;
      i: Integer;
  begin
    d := TpsEditor.Create();
    try
      if FileName <> '' then
         d.FileName := FileName;

      for i := 0 to High(APIs) do
        d.Compiler.Include(APIs[i]);

      for i := 0 to High(Variables) do
        d.Compiler.Variables.AddVar(Variables[i]);

      d.Compiler.GlobalObjects := GlobalObjects;

      d.CanRun           := True;
      d.CanStop          := True;
      d.CanShowByteCode  := True;
      d.Optimize         := True;
      d.CanOptimize      := True;
      d.btnCompile.Hint  := ' Compilar Script ';
      d.Compiler.GerCODE := True;

      d.MostraBiblioteca();
      d.ListaVariaveis();

      if CODE <> nil then d.Codigo.Lines.Assign(CODE);

      d.ShowModal;
      Result := d.FileName;
    finally
      d.Free;
    end;
  end;

  function EditScript(Lib: TLib; Const FileName: String;
                      CODE: TStrings; Variables: Array of TVariable;
                      GlobalObjects: TGlobalObjects): String;
  var d: TpsEditor;
      i: Integer;
  begin
    d := TpsEditor.Create();
    try
      if FileName <> '' then d.FileName := FileName;

      d.Compiler.AssignLib(Lib);

      for i := 0 to High(Variables) do
        d.Compiler.Variables.AddVar(Variables[i]);

      d.Compiler.GlobalObjects := GlobalObjects;

      d.CanRun           := True;
      d.CanStop          := False;
      d.CanShowByteCode  := True;
      d.Optimize         := False;
      d.CanOptimize      := False;
      d.btnCompile.Hint  := ' Verificar Script ';
      d.Compiler.GerCODE := True;

      d.MostraBiblioteca();
      d.ListaVariaveis();

      if CODE <> nil then
         d.Codigo.Lines.Assign(CODE);

      d.ShowModal();
      Result := d.FileName;
    finally
      d.Free;
    end;
  end;

  function RunScript(Lib: TLib; Const FileName, Root: String;
                     CODE: TStrings; Variables: Array of TVariable;
                     GlobalObjects: TGlobalObjects;
                     Modal: Boolean): String;
  var d: TpsEditor;
      i: Integer;
  begin
    d := TpsEditor.Create();
    try
      if FileName <> '' then d.FileName := FileName;
      d.Root := Root;

      d.Compiler.AssignLib(Lib);

      for i := 0 to High(Variables) do
        d.Compiler.Variables.AddVar(Variables[i]);

      d.Compiler.GlobalObjects := GlobalObjects;

      d.CanRun           := True;
      d.CanStop          := True;
      d.CanShowByteCode  := True;
      d.Optimize         := True;
      d.CanOptimize      := True;
      d.btnCompile.Hint  := ' Compilar Script ';
      d.Compiler.GerCODE := True;

      d.MostraBiblioteca();
      d.ListaVariaveis();

      if CODE <> nil then
         d.Codigo.Lines.Assign(CODE);

      if Modal then
         d.ShowModal()
      else
         d.Show();

      Result := d.FileName;
    finally
      if Modal then d.Free;
    end;
  end;

procedure DocumentLib(Lib: TLib; Const DirName: String);

  procedure DescreveRotina(Rot: TFunctionObject; Saida: TOutPut);
  var i: Integer;
      o: TStrings;
      s, s2: String;
  begin
    // Nome
    s := Rot.Name;

    if Rot.ResType = pvtNull then
       s := 'procedure ' + s
    else
       s := 'function ' + s;

    Saida.Write(s, clBlack, 3, False, True, False, False);

    if Rot.Parameters > 0 then
       begin
       Saida.Write(' (', clBlack, 3, False, True, False, False);
       for i := 0 to Rot.Parameters-1 do
         begin
         if Rot.ParType[i] = pvtObject then
            s2 := ' ' + Rot.ParClass[i].ClassName
         else
            s2 := getStrVarType(Rot.ParType[i]);

         if Rot.ParCat[i] then
            Saida.Write(s2, clRed, 3, False, True, False, False)
         else
            Saida.Write(s2, clBlue, 3, False, True, False, False);

         if i < Rot.Parameters-1 then
            Saida.Write(', ', clBlack, 3, False, True, False, False)
         else
            Saida.Write(') ', clBlack, 3, False, True, False, False);
         end;
       end;

    if Rot.ResType <> pvtNull then
       begin
       if Rot.ResType <> pvtObject then
          s := ': ' + getStrVarType(Rot.ResType)
       else
          s := ': ' + Rot.ResObjectClass.ClassName;

       Saida.Write(s, clBlack, 3, False, True, False, False);
       end;

    //Saida.WriteLine(2);
    Saida.Write;

    // Descrição
    if Rot.Comment <> '' then
       begin
       o := TStringList.Create;
       o.Text := Rot.Comment;
       for i := 0 to o.Count-1 do
          Saida.Write(o[i]);
       o.Free;
       Saida.Write;
       end;

    Saida.WriteSeparator;
  end;

  procedure DescreveClasse(Clas: TpsClass; Arq: String);
  var Saida : TOutPut;
      i     : Integer;
      o     : TStrings;
      s     : String;
  begin
    Saida := TOutPut.Create;
    Saida.DocType := dtHTML;
    Saida.FileName := Arq;

    // Nome
    s := Clas.Name;
    if Clas.Parent <> nil then s := s + ' (' + Clas.Parent.ClassName + ')';
    Saida.Write(s, clBlue, 4, True, True, False);
    Saida.Write;

    // Descrição
    o := TStringList.Create;
    o.Text := Clas.Comment;
    for i := 0 to o.Count-1 do
       Saida.Write(o[i]);
    o.Free;
    Saida.Write;

    // Métodos
    Saida.Write('Métodos', clBlack, 4, False, True, False);
    Saida.Write;

    Saida.Write('Passagem de Parâmetros: ', clBlack, 3, False, False, False);
    Saida.WriteSpaces(4);
    Saida.Write('AZUL = por Cópia ', clBlue, 2, False, False, False);
    Saida.WriteSpaces(4);
    Saida.Write('VERMELHO = por Referência', clRed, 2, False, False, False);
    Saida.Write;
    Saida.Write;

    for i := 0 to Clas.Procs.Count-1 do
       DescreveRotina(Clas.Procs[i], Saida);

    for i := 0 to Clas.Functions.Count-1 do
       DescreveRotina(Clas.Functions[i], Saida);

    Saida.WriteLink('Voltar para a Página Principal', 'Index.HTM');

    Saida.Save;
    Saida.Free;
  end;

  procedure DescreveRotinasDaCategoria(Const Cat, Arq: String);
  var Saida : TOutPut;
      i     : Integer;
  begin
    Saida := TOutPut.Create;
    Saida.DocType := dtHTML;
    Saida.FileName := Arq;

    Saida.Write(Cat, clBlue, 4, True, True, False);
    Saida.Write;

    Saida.Write('Passagem de Parâmetros: ', clBlack, 3, False, False, False);
    Saida.WriteSpaces(4);
    Saida.Write('AZUL = por Cópia ', clBlue, 2, False, False, False);
    Saida.WriteSpaces(4);
    Saida.Write('VERMELHO = por Referência', clRed, 2, False, False, False);
    Saida.Write;

    for i := 0 to Lib.Procs.Count-1 do
      if CompareText(Lib.Procs[i].Category, Cat) = 0 then
         DescreveRotina(Lib.Procs[i], Saida);

    for i := 0 to Lib.Functions.Count-1 do
      if CompareText(Lib.Functions[i].Category, Cat) = 0 then
         DescreveRotina(Lib.Functions[i], Saida);

    Saida.WriteLink('Voltar para a Página Principal', 'Index.HTM');

    Saida.Save;
    Saida.Free;
  end;

var Saida : TOutPut;
    s, s2 : String;
    i     : Integer;
begin
  Saida := TOutPut.Create;

  Saida.DocType := dtHTML;
  Saida.FileName := DirName + '\Index.HTM';

  Saida.BeginDoc;

  Saida.Write('Bibliotecas do Pascal Script', clBlue, 4, True, True, False);
  Saida.Write;

  Saida.Write('Classes', clBlack, 3, False, True, False);
  Saida.Table_Properties.Align := aLeft;
  Saida.Table_Properties.Border := 0;
  Saida.BeginTable;
    Saida.BeginRow;

      Saida.Cell_Properties.Width := 30;
      Saida.BeginCell;
      Saida.EndCell;

      Saida.Cell_Properties.Width := 1;
      Saida.BeginCell;
      for i := 0 to Lib.Classes.Count-1 do
        begin
        s := 'Classe' + IntToStr(i+1) + '.HTM';
        s2 := Lib.Classes[i].Name;
        if Lib.Classes[i].Parent <> nil then
           s2 := s2 + ' (' + Lib.Classes[i].Parent.ClassName + ')';
        Saida.WriteLink(s2, s);
        DescreveClasse(Lib.Classes[i], DirName + '\' + s);
        end;
      Saida.EndCell;

    Saida.EndRow;
  Saida.EndTable;
  Saida.Write;

  Saida.Write('Rotinas', clBlack, 3, False, True, False);
  Saida.Table_Properties.Align := aLeft;
  Saida.Table_Properties.Border := 0;
  Saida.BeginTable;
    Saida.BeginRow;

      Saida.Cell_Properties.Width := 30;
      Saida.BeginCell;
      Saida.EndCell;

      Saida.Cell_Properties.Width := 1;
      Saida.BeginCell;
      for i := 0 to Lib.Functions.Categories.Count-1 do
        begin
        s := 'Cat' + IntToStr(i+1) + '.HTM';
        Saida.WriteLink(Lib.Functions.Categories[i], s);
        DescreveRotinasDaCategoria(Lib.Functions.Categories[i], DirName + '\' + s);
        end;

      for i := 0 to Lib.Procs.Categories.Count-1 do
        begin
        s := 'Cat' + IntToStr(i+1) + '.HTM';
        Saida.WriteLink(Lib.Procs.Categories[i], s);
        DescreveRotinasDaCategoria(Lib.Procs.Categories[i], DirName + '\' + s);
        end;
      Saida.EndCell;

    Saida.EndRow;
  Saida.EndTable;

  Saida.EndDoc;
  Saida.Save;

  Saida.Free;
end;

procedure ExportDocumentation(Lib: TLib; const FileName: String);
var Saida : TStrings;

  procedure DescreveRotina(Rot: TFunctionObject);
  var i: Integer;
      s: String;
  begin
    // Nome
    s := Rot.Name;

    if Rot.ResType = pvtNull then
       s := 'procedure ' + s
    else
       s := 'function ' + s;

    if Rot.Parameters > 0 then
       begin
       s := s + ' (';
       for i := 0 to Rot.Parameters-1 do
         begin
         if Rot.ParType[i] = pvtObject then
            s := s + Rot.ParClass[i].ClassName
         else
            s := s + getStrVarType(Rot.ParType[i]);
         {
         if Rot.ParCat[i] then
            Saida.Write(s2, clRed, 3, False, True, False, False)
         else
            Saida.Write(s2, clBlue, 3, False, True, False, False);
         }
         if i < Rot.Parameters-1 then
            s := s + ', '
         else
            s := s + ')';
         end;
       end;

    if Rot.ResType <> pvtNull then
       if Rot.ResType <> pvtObject then
          s := s + ': ' + getStrVarType(Rot.ResType)
       else
          s := s + ': ' + Rot.ResObjectClass.ClassName;

    // Descrição
    s := s  + ';' + Rot.Comment;
    Saida.Add(s);
  end;

  procedure DescreveClasse(Clas: TpsClass);
  var i: Integer;
      s: String;
  begin
    // Nome
    s := Clas.Name;
    if Clas.Parent <> nil then s := s + ' (' + Clas.Parent.ClassName + ')';
    s := s + ';';

    // Descrição
    s := s + Clas.Comment;
    Saida.Add(s);

    // Métodos
    s := 'Métodos;';
    Saida.Add(s);

    for i := 0 to Clas.Procs.Count-1 do
       DescreveRotina(Clas.Procs[i]);

    for i := 0 to Clas.Functions.Count-1 do
       DescreveRotina(Clas.Functions[i]);
  end;

  procedure DescreveRotinasDaCategoria(Const Cat: String);
  var i: Integer;
  begin
    Saida.Add(Cat + ';');

    for i := 0 to Lib.Procs.Count-1 do
      if CompareText(Lib.Procs[i].Category, Cat) = 0 then
         DescreveRotina(Lib.Procs[i]);

    for i := 0 to Lib.Functions.Count-1 do
      if CompareText(Lib.Functions[i].Category, Cat) = 0 then
         DescreveRotina(Lib.Functions[i]);
  end;

var s     : String;
    i     : Integer;
begin
  Saida := TStringList.Create;

  Saida.Add('Bibliotecas do Pascal Script;');

  Saida.Add('Rotinas;');

  for i := 0 to Lib.Functions.Categories.Count-1 do
    begin
    s := 'Cat' + IntToStr(i+1) + '.HTM';
    DescreveRotinasDaCategoria(Lib.Functions.Categories[i]);
    end;

  for i := 0 to Lib.Procs.Categories.Count-1 do
    begin
    s := 'Cat' + IntToStr(i+1) + '.HTM';
    DescreveRotinasDaCategoria(Lib.Procs.Categories[i]);
    end;

  Saida.Add('Classes;');
  for i := 0 to Lib.Classes.Count-1 do
    DescreveClasse(Lib.Classes[i]);

  Saida.SaveToFile(FileName);
  Saida.Free;
end;

{$R *.DFM}

Procedure getMemoPos(Memo: TMemo; Var Line, Col: Integer);
Begin
  With Memo do
    Begin
    Line := Perform(EM_LINEFROMCHAR, SelStart, 0);
    Col := SelStart - Perform(EM_LINEINDEX, Line, 0);
    End;
End;

procedure setMemoPos(Memo: TMemo; Y, X: Word);
begin
  Memo.SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Y-1, 0) + X;
  SendMessage(Memo.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TpsEditor.Codigo_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var L,C: Integer;
begin
  L := Codigo.CaretY;
  C := Codigo.CaretX;
  StatusBar.Panels[0].Text := 'Lin: ' + IntToStr(L+1);
  StatusBar.Panels[1].Text := 'Col: ' + IntToStr(C+1);
end;

procedure TpsEditor.Codigo_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Inutil1: Word;
    Inutil2: TShiftState;
begin
  Codigo_KeyUp(Nil, Inutil1, Inutil2);
end;                                            

procedure TpsEditor.btnNewClick(Sender: TObject);
begin
  Codigo.Lines.Clear;
  No_Vars_Loc.DeleteChildren;
  No_Vars_Pre.DeleteChildren;
  Info.Clear;
  SetFN('');
end;

procedure TpsEditor.btnOpenClick(Sender: TObject);
begin
  if FRoot = '' then
     Open.InitialDir := ExtractFilePath(Application.ExeName) + '\'
  else
     Open.InitialDir := FRoot;

  If Open.Execute Then
     Begin
     FRoot := ExtractFilePath(Open.FileName);
     Codigo.BeginUpdate;
     btnNewClick(nil);
     Open.InitialDir := ExtractFilePath(Open.FileName);
     SetFN(Open.FileName);
     FCompiler.Compiled := false;
     Codigo.EndUpdate;
     End;
end;

procedure TpsEditor.btnSaveAsClick(Sender: TObject);
begin
  if FRoot = '' then
     Save.InitialDir := ExtractFilePath(Application.ExeName) + '\'
  else
     Save.InitialDir := FRoot;

  If Save.Execute Then
     Begin
     FRoot := ExtractFilePath(Save.FileName);
     Codigo.Lines.SaveToFile(Save.FileName);
     Save.InitialDir := ExtractFilePath(Save.FileName);
     SetFN(Save.FileName);
     StatusBar.Panels[2].Text := '';
     Codigo.Modified := False;
     End;
end;

procedure TpsEditor.btnHelpClick(Sender: TObject);
var s: String;
begin
  s := ExtractFilePath(Application.ExeName);
  ShellExecute(Self.Handle,
               'open',
               'IExplore.Exe',
               pChar(s + 'Help\PascalScript\index.html'),
               '',
               SW_SHOWMAXIMIZED);
end;

procedure TpsEditor.btnCompileClick(Sender: TObject);
var i: longword;
begin
   Info.Clear;

   Arvore.Items.BeginUpdate;

   No_Vars_Loc.DeleteChildren;
   No_Vars_Pre.DeleteChildren;

   FCompiler.Scanner.Text := Codigo.Lines;
   FCompiler.GerCODE  := True;
   FCompiler.Optimize := cbOtimizar.Checked;

   i := GetTickCount;
   FCompiler.Compile;
   i := GetTickCount - i;

   If FCompiler.Errors.Count = 0 Then
      Begin
      Info.Items.Add('Compilado com Sucesso em ' + FloatToStr(i/1000) + ' segundos !');
      if FMostrarVars then ListaVariaveis;
      End
   else
      Info.Items.Assign(FCompiler.Errors);

   Arvore.Items.EndUpdate;
End;

procedure TpsEditor.btnByteCodeClick(Sender: TObject);
var d: TByteCode_DLG;
    i: Integer;
begin
  if FCompiler.Compiled then
     with FCompiler.ByteCode do
       begin
       d := TByteCode_DLG.Create(nil);
       d.IC.RowCount := Count + 1;
       for i := 0 to Count-1 do
         Begin
         d.IC.Cells[0, i+1] := IntToStr(i);
         d.IC.Cells[1, i+1] := OperToStr(LineCode[i].Operacao);
         d.IC.Cells[2, i+1] := LineCode[i].Op1;
         d.IC.Cells[3, i+1] := LineCode[i].Op2;

         if LineCode[i].i_Op1 > -1 then
            d.IC.Cells[4, i+1] := intToStr(LineCode[i].i_Op1)
         else
            d.IC.Cells[4, i+1] := '';

         if LineCode[i].i_Op2 > -1 then
            d.IC.Cells[5, i+1] := intToStr(LineCode[i].i_Op2)
         else
            d.IC.Cells[5, i+1] := '';

         if LineCode[i].Res > -1 then

            try
              d.IC.Cells[6, i+1] := TVariable(LineCode[i].Res).Name;
            except
              d.IC.Cells[6, i+1] := IntToStr(LineCode[i].Res);
            end
         else
            d.IC.Cells[6, i+1] := '';
         End;
       d.Show;
       end;
end;

procedure TpsEditor.btnRunClick(Sender: TObject);
var i: longword;
begin
  FMostrarVars := False;
  try
    if not FCompiler.Compiled then btnCompileClick(nil);
  finally
    FMostrarVars := True;
  end;

  if FCompiler.Compiled then
     begin
     i := GetTickCount;
     FCompiler.Execute();
     i := GetTickCount - i;
     Info.Items.Add('Executado com Sucesso em ' + FloatToStr(i/1000) + ' segundos !');
     ListaVariaveis;
     end;
end;

procedure TpsEditor.Codigo_Change(Sender: TObject);
begin
  FCompiler.Compiled := False;
  StatusBar.Panels[2].Text := 'Modificado';
end;

procedure TpsEditor.ListaVariaveis;
var i: Integer;
    s, Valor: String;
    v: TVariable;
begin
  Arvore.Items.BeginUpdate;

  No_Vars_Loc.DeleteChildren;
  No_Vars_Pre.DeleteChildren;

  for i := 0 to FCompiler.Variables.Count-1 do
    begin
    s := FCompiler.Variables[i];
    if s[1] = '@' then Continue;

    v := FCompiler.Variables.Variable[i];
    if (v.vType <> pvtNull) and (s <> 'TRUE') and (s <> 'FALSE') then
       begin
       Valor := v.Value; // É necessário por que Value é um variant

       s := s + ': ' + getStrVarType(v.vType);

       if v.vType = pvtObject then
          if v.ObjectClass <> nil then
             s := s + ' (' + v.ObjectClass.ClassName + ')'
          else
             s := s + ' (não inicializado)'
       else
          s := s + ' = ' + Valor;

       if v.Locked then
          SetImageIndex(Arvore.Items.AddChild(No_Vars_Pre, s), 1)
       else
          SetImageIndex(Arvore.Items.AddChild(No_Vars_Loc, s), 1)
       end;
    end;
  No_Vars.Expand(True);
  Arvore.Items.EndUpdate;
end;

procedure TpsEditor.btnStopClick(Sender: TObject);
begin
  FCompiler.Stop;
end;

procedure TpsEditor.AfterRun(Sender: TObject);
begin
  btnRun.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TpsEditor.BeforeRun(Sender: TObject);
begin
  btnRun.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TpsEditor.MostraFuncoes(Funcs: TList; Lugar: TTreeNode; Categorizar: boolean);
var i, k  : Integer;
    s, s2 : String;
    M     : TFunctionObject;
    no    : TTreeNode;
begin
  Arvore.Items.BeginUpdate;

  for i := 0 to Funcs.Count-1 do
    begin
    M := TFunctionObject(Funcs[i]);
    with M do
      begin

      if Categorizar then
         begin
         no := FindNode(Lugar, M.Category);
         if no = nil then
            no := Arvore.Items.AddChild(Lugar, M.Category);
         end
      else
         no := Lugar;

      s := Name;
      if Parameters > 0 then
         begin
         s := s + ' (';
         for k := 0 to Parameters-1 do
           begin
           s2 := getStrVarType(ParType[k]);
           if ParType[k] = pvtObject then
              s2 := s2 + ' <' + ParClass[k].ClassName + '>';

           if k = 0 then
              s := s + s2
           else
              s := s + ', ' + s2;
           end;
         s := s + ') ';
         end;
      s := s + ': ' + getStrVarType(ResType);

      if ResType = pvtObject then
         s := s + ' <' + ResObjectClass.ClassName + '>';

      SetImageIndex(Arvore.Items.AddChildObject(no, s, Pointer(Funcs[i])), 2);
      end;
    end;
  Lugar.AlphaSort;
  Arvore.Items.EndUpdate;
end;

procedure TpsEditor.MostraProcs(Procs: TList; Lugar: TTreeNode; Categorizar: Boolean);
var i, k  : Integer;
    s, s2 : String;
    M     : TFunctionObject;
    no    : TTreeNode;
begin
  Arvore.Items.BeginUpdate;
  for i := 0 to Procs.Count-1 do
    begin
    M := TFunctionObject(Procs[i]);
    with M do
      begin

      if Categorizar then
         begin
         no := FindNode(Lugar, M.Category);
         if no = nil then
            no := Arvore.Items.AddChild(Lugar, M.Category);
         end
      else
         no := Lugar;

      s := Name;
      if Parameters > 0 then
         begin
         s := s + ' (';
         for k := 0 to Parameters-1 do
           begin
           s2 := getStrVarType(ParType[k]);
           if ParType[k] = pvtObject then
              s2 := s2 + ' <' + ParClass[k].ClassName + '>';

           if k = 0 then
              s := s + s2
           else
              s := s + ', ' + s2;
           end;
         s := s + ') ';
         end;

      SetImageIndex(Arvore.Items.AddChildObject(no, s, Pointer(Procs[i])), 3);
      end;
    end;
  Lugar.AlphaSort;
  Arvore.Items.EndUpdate;
end;

procedure TpsEditor.MostraClasses(Classes: TClassList);
var i  : Integer;
    s  : String;
    No, No1 : TTreeNode;
begin
  Arvore.Items.BeginUpdate;

  // Limpa o sub-no das classes
  No_Classes.DeleteChildren();

  // Define as categorias
  for i := 0 to Classes.Categories.Count-1 do
    Arvore.Items.AddChild(No_Classes, Classes.Categories[i]);

  for i := 0 to Classes.Count-1 do
    begin
    // Classe
    if Classes[i].Parent <> nil then
       s := Classes[i].Parent.ClassName
    else
       s := Classes[i].Category;

    No := FindNode(NO_Classes, s);
    if No = nil then No := NO_Classes;
    No := Arvore.Items.AddChildObject(No, Classes[i].Name, Pointer(Classes[i]));
    SetImageIndex(No, 4 + byte(Classes[i].CanCreate));

    No1 := nil;

    // Métodos (Procedimentos)
    if Classes[i].Procs.OwnerFuncs.Count > 0 then
       begin
       No1 := Arvore.Items.AddChild(No, 'Métodos');
       MostraProcs(Classes[i].Procs.OwnerFuncs, No1, False);
       end;

    // Métodos (Funções)
    if Classes[i].Functions.OwnerFuncs.Count > 0 then
       begin
       if No1 = nil then No1 := Arvore.Items.AddChild(No, 'Métodos');
       MostraFuncoes(Classes[i].Functions.OwnerFuncs, No1, false);
       end;

    if No1 <> nil then SetImageIndex(No1, 0);
    end;

  No_Classes.AlphaSort;
  Arvore.Items.EndUpdate;
end;

procedure TpsEditor.MostraBiblioteca();
begin
  MostraClasses(FCompiler.Lib.Classes);
  MostraFuncoes(FCompiler.Lib.Functions.OwnerFuncs, No_Funcs, true);
  MostraProcs(FCompiler.Lib.Procs.OwnerFuncs, No_Funcs, true);
end;

procedure TpsEditor.lbFunctions_Click(Sender: TObject);
var Func: TFunctionObject;
    LB: TListBox;
begin
  LB := TListBox(Sender);
  Func := TFunctionObject(LB.Items.Objects[LB.ItemIndex]);
  Info.Items.Text := Func.Name + ': ' + Func.Comment;
end;

procedure TpsEditor.lbProcs_Click(Sender: TObject);
var Proc: TProcObject;
    LB: TListBox;
begin
  LB := TListBox(Sender);
  Proc := TProcObject(LB.Items.Objects[LB.ItemIndex]);
  Info.Items.Text := Proc.Name + ': ' + Proc.Comment;
end;

procedure TpsEditor.InfoDblClick(Sender: TObject);
var T: TToken;
begin
  T := TToken(Info.Items.Objects[Info.ItemIndex]);
  if T <> nil then
     begin
     Codigo.CaretY := T.Lin - 1;
     Codigo.CaretX := T.Col;
     Codigo.SetFocus;
     end;
end;

procedure TpsEditor.SetFN(const Value: String);
const c = 'PascalScript Editor';
begin
  if Value <> '' then
     if FileExists(Value) then
        begin
        Codigo.Lines.LoadFromFile(Value);
        Codigo.SetLeftTop(0, 0);
        Codigo.Modified := False;
        StatusBar.Panels[2].Text := '';
        Caption := c + ' - ' + Value;
        FFileName := Value;
        end
     else
        begin
        FFileName := '';
        Caption := c;
        MessageDLG(Format('Arquivo não existe.'#13'%s', [Value]),
                   mtInformation, [mbOK], 0);
        end
  else
     begin
     FFileName := '';
     Caption := c;
     end;
end;

procedure TpsEditor.btnSalvarClick(Sender: TObject);
begin
  If FFileName <> '' then
     begin
     Codigo.Lines.SaveToFile(FFileName);
     StatusBar.Panels[2].Text := '';
     Codigo.Modified := False;
     end
  else
     btnSaveAsClick(nil);
end;

procedure TpsEditor.SetCanOptimize(Value: Boolean);
begin
  cbOtimizar.Visible := Value;
end;

procedure TpsEditor.SetCanRun(Value: Boolean);
begin
  btnRun.Enabled := Value;
end;

procedure TpsEditor.SetCanShowByteCode(Value: Boolean);
begin
  btnByteCode.Enabled := Value;
end;

procedure TpsEditor.SetCanStop(Value: Boolean);
begin
  btnStop.Enabled := Value;
end;

procedure TpsEditor.SetOptimize(Value: Boolean);
begin
  cbOtimizar.Checked := Value;
end;

procedure TpsEditor.btnFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TpsEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var r: Integer;
begin
  CanClose := True;
  if Codigo.Modified then
     begin
     r := MessageDLG('Código modificado. Deseja salvá-lo ?',
                      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
     Case r of
       mrYes    : btnSalvarClick(nil);
       mrNo     : {Fecha sem salvar};
       mrCancel : CanClose := False;
       end;
     end;

end;

// Mostra as informações sobre os objetos
procedure TpsEditor.ArvoreClick(Sender: TObject);
var no: TTreeNode;
    F : TFunctionObject;
    o : TObject;
    c : TpsClass;
    s : String;
    i : Integer;
begin
  no := Arvore.Selected;
  if no.Data <> nil then
     begin
     o := TObject(no.Data);
     if o.ClassNameIs('TFunctionObject') then
        begin
        F := TFunctionObject(o);
        Info.Items.Text := F.Name + ': ' + F.Comment;
        end else

     if no.HasAsParent(No_Classes) then
        begin
        c := TpsClass(o);
        if c.CanCreate then
           begin
           s := 'Instanciável'#13;
           s := s + 'Criação: CreateObject(' + c.Name;
           for i := 0 to c.Parameters-1 do
             s := s + ', ' + getStrVarType(c.ParType[i]);
           if c.Parameters > 0 then Delete(s, Length(s)+1, 1);
           s := s + ')'#13;
           end
        else
           s := 'Não Instanciável'#13;

        s := s + c.Comment;
        Info.Items.Text := s;
        end;
     end;
end;

// Insere no editor as rotinas onde houveram duplo-click
procedure TpsEditor.ArvoreDblClick(Sender: TObject);
var no: TTreeNode;
    F : TFunctionObject;
    Linha, s : String;
    i, L, C: Integer;
begin
  no := Arvore.Selected;
  if no.Data <> nil then
     if TObject(no.Data).ClassName = 'TFunctionObject' then
        begin
        F := TFunctionObject(no.Data);

        L := Codigo.CaretY;
        C := Codigo.CaretX;

        if L = Codigo.Lines.Count then
           begin
           Linha := '';
           Codigo.BeginUpdate;
           Codigo.Lines.Add('');
           Codigo.EndUpdate;
           end
        else
           Linha := Codigo.Lines[L];

        s := F.Name;
        if F.Parameters > 0 then
           begin
           s := s + '(';
           for i := 1 to F.Parameters-1 do s := s + ' ,';
           if F.Parameters = 1 then s := s + ')' else s := s + ' )';
           end;

        Insert(s, Linha, C+1);

        Codigo.BeginUpdate;
        Codigo.Lines[L] := Linha;
        Codigo.EndUpdate;
        end;
end;

procedure TpsEditor.Menu_ExportarHTMLClick(Sender: TObject);
var s: String;
begin
  s := ExtractFilePath(Application.ExeName) + 'Script-Lib Help';
  ForceDirectories(s);
  DocumentLib(FCompiler.Lib, s);
end;

procedure TpsEditor.Menu_ExportarTextoClick(Sender: TObject);
var s: String;
begin
  s := ExtractFilePath(Application.ExeName) + 'LibDoc.txt';
  ExportDocumentation(FCompiler.Lib, s);
end;

procedure TpsEditor.Codigo_PaintGutter(Sender: TObject; Canvas: TCanvas);

  procedure Draw(Y, ImageIndex : integer);
  var
    Ro : integer;
    R : TRect;
  begin
    if Y <> -1 then
       with Sender as TScriptPascalEditor do
         begin
         Ro := Y - TopRow;
         R := CalcCellRect(0, Ro);
         GutterImages.Draw(Canvas,
           GutterWidth -GutterRightMargin -GutterImages.Width{R.Left},
           R.Top + (CellRect.Height - GutterImages.Height) div 2 +1,
           ImageIndex);
         end;
  end;
var
  i  : integer;
begin
  for i := 0 to 9 do
    if (Sender as TScriptPascalEditor).Bookmarks[i].Valid then
      Draw((Sender as TScriptPascalEditor).Bookmarks[i].Y, i);
end;

procedure TpsEditor.Codigo_CompletionDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Offset, W  : Integer;
  ImageIndex : integer;
  S          : string;
begin
  Offset := 3;
  with Control as TListBox, (Control.Owner as TScriptPascalEditor).Completion do
    begin
    Canvas.FillRect(Rect);
    case Mode of
      cmIdentifers :
        begin
        ImageIndex := StrToInt(Trim(SubStr(Items[Index], 2, Separator))) - 1;
        ilCompletions.Draw(Canvas, Rect.Left + 2, Rect.Top, ImageIndex);
        Canvas.TextOut(Rect.Left + 3*Offset + ilCompletions.Width, Rect.Top +2, SubStr(Items[Index], 0, Separator));
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

procedure TpsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TpsEditor.ArvoreChange(Sender: TObject; Node: TTreeNode);
begin
  ArvoreClick(nil);
end;

procedure TpsEditor.btnImprimirClick(Sender: TObject);
begin
  if PrintDialog.Execute then
     Codigo.Print;
end;

constructor TpsEditor.Create();
var s: string;
begin
  inherited Create(nil);

  FMostrarVars := true;

  s := ExtractFilePath(Application.ExeName);

  FCompiler := TPascalScript.Create();
  FCompiler.Economize := False;
  FCompiler.Lib.Load_APIs(s + 'PascalScriptLibs.ini', s + 'DLLs\');
  FCompiler.Scanner.ListError := Info.Items;

  // Eventos da Máquina Virtual
  FCompiler.VirtualMachine.OnBeforeExecute := BeforeRun;
  FCompiler.VirtualMachine.OnAfterExecute  := AfterRun;

  // Nós da árvore
  No_Classes  := Arvore.Items[0];
  No_Funcs    := Arvore.Items[1];
  No_Vars     := Arvore.Items[2];
  No_Vars_Loc := No_Vars.Item[0];
  No_Vars_Pre := No_Vars.Item[1];
end;

destructor TpsEditor.Destroy;
begin
  FCompiler.Free;
  inherited;
end;

procedure TpsEditor.Menu_LocClick(Sender: TObject);
var no: TTreeNode;
begin
  no := FindNode(No_Classes, TMenuItem(Sender).Hint);
  if no <> nil then
     no.Selected := true;
end;

procedure TpsEditor.Menu_ArvorePopup(Sender: TObject);
var SL: TStrings;
    i: integer;
    m: TMenuItem;
begin
  if Arvore.Selected <> nil then
     begin
     // Obtem todas as sub-strings diferentes entre os caracteres < >
     SL := SubStrings(Arvore.Selected.Text, '<', '>', false);
     Menu_Loc.Clear();
     for i := 0 to SL.Count-1 do
       begin
       m := TMenuItem.Create(nil);
       m.Caption := SL[i];
       m.Hint := SL[i];
       m.OnClick := Menu_LocClick;
       Menu_Loc.Add(m);
       end;
     SL.Free();
     end;
end;

end.


unit Edit;

{
  AUTOR: ........................ Adriano Rochedo Conceição
  VERSÃO: ....................... 5.01
  LOCAL: ........................ Laboratório de Pesquisas da Informática - UFPEL
  ÚLTIMA MODIFIC.: .............. Rochedo, 27/05/1999

  OBJETIVOS: .................... Possibilitar o uso de um editor de textos perso-
    nalizado para saída formatada de resultados, mensagens de erro, resultados de
    cálculos, impressões, etc.

  HISTÓRICO:
    08/04/1998 .................. Incluído os métodos:
                                  - FirstDoc(): Doc;
                                  - LastDoc (): Doc;
                                  - OpenDoc ();
                                  - SaveDoc ();

    07/05/1998 .................. Modificado a forma de fechamento dos documentos

    08/11/1998 .................. Incluído o Método:
                                  WriteStrings();

    15/12/1998 .................. Íncluido os Métodos:
                                  WriteCenter();
                                  WriteNote();

    27/05/1999 .................. Implementação da Função Procurar.Trocar;
                                  Outras pequenas coisas

    01/10/1999 .................. Implementação dos Métodos:
                                  - WriteFmt
                                  - WriteStringsIdent

    08/03/1999 .................. Retirada a variável gEditor.
                                  O Editor precisa ser criado para ser utilizado diretamente ou
                                  criar um objeto TwsOutPut (wsOutPut.pas) que permite acesso
                                  ao editor.

    16/05/2001 .................. Incluído o Método:
                                  Write(const S: String; BreakLineChar: Char; const Ident: String);

}

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Menus, Clipbrd, ExtCtrls, Printers, ShellAPI,
  Buttons, Tabs, FileCtrl;

type
  TEditor = class;

  TFindDlg = class(TForm)
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox2: TCheckBox;
    Bevel1: TBevel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TReplaceDLG = class(TForm)
    Label1: TLabel;
    edProcurar: TEdit;
    edTrocar: TEdit;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    cbCS: TCheckBox;
    cbSPI: TCheckBox;
    Bevel1: TBevel;
    rbBF: TRadioButton;
    rbBT: TRadioButton;
    Panel1: TPanel;
    rbTT: TRadioButton;
    rbTS: TRadioButton;
    btnOk: TBitBtn;
    btnCancelar: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TSalvaEspDLG = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Dir: TDirectoryListBox;
    Drive: TDriveComboBox;
    Nome: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Palavras: TMemo;
    SalvaPalavras: TBitBtn;
    RecuperaPalavras: TBitBtn;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    procedure OKBtnClick(Sender: TObject);
    procedure SalvaPalavrasClick(Sender: TObject);
    procedure RecuperaPalavrasClick(Sender: TObject);
  private
    { Private declarations }
  public
    Editor: TEditor;
  end;

  TEditor = class(TForm)
    MainMenu1: TMainMenu;
    Arquivo1: TMenuItem;
    Novo1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Salvar1: TMenuItem;
    SalvarComo1: TMenuItem;
    N2: TMenuItem;
    Imprimir1: TMenuItem;
    Configurar1: TMenuItem;
    N3: TMenuItem;
    Sair1: TMenuItem;
    Editar1: TMenuItem;
    Copiar1: TMenuItem;
    Cortar1: TMenuItem;
    Colar1: TMenuItem;
    Texto1: TMenuItem;
    Apagar1: TMenuItem;
    N4: TMenuItem;
    Selecionatudo1: TMenuItem;
    Fonte1: TMenuItem;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    FontDialog1: TFontDialog;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Documentos: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Word1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Desfaz1: TMenuItem;
    Refaz1: TMenuItem;
    N5: TMenuItem;
    AutoIdentao1: TMenuItem;
    CriaBackup1: TMenuItem;
    N6: TMenuItem;
    Tab1: TMenuItem;
    N7: TMenuItem;
    Fixa1: TMenuItem;
    Real1: TMenuItem;
    Pequena1: TMenuItem;
    N8: TMenuItem;
    Tamanho1: TMenuItem;
    Procurar1: TMenuItem;
    Busca1: TMenuItem;
    BuscaProximo1: TMenuItem;
    N9: TMenuItem;
    VaiparaLinha1: TMenuItem;
    Limpartudo1: TMenuItem;
    N10: TMenuItem;
    MarcarTexto1: TMenuItem;
    DesmarcarTexto1: TMenuItem;
    VaiparaoMarcador1: TMenuItem;
    TabSet1: TTabSet;
    Arquivo_Fechar: TMenuItem;
    SalvarEspecial: TMenuItem;
    Menu_Trocar: TMenuItem;
    procedure Novo1Click(Sender: TObject);
    procedure Abrir1Click(Sender: TObject);
    procedure Salvar1Click(Sender: TObject);
    procedure SalvarComo1Click(Sender: TObject);
    procedure Imprimir1Click(Sender: TObject);
    procedure Configurar1Click(Sender: TObject);
    procedure Sair1Click(Sender: TObject);
    procedure Cortar1Click(Sender: TObject);
    procedure Copiar1Click(Sender: TObject);
    procedure Colar1Click(Sender: TObject);
    procedure Apagar1Click(Sender: TObject);
    procedure Selecionatudo1Click(Sender: TObject);
    procedure Word1Click(Sender: TObject);
    procedure Fonte1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Memo1ShowStatus(Sender: TObject; LineNum: Longint;
      ColNum: Integer);
    procedure VaiparaLinha1Click(Sender: TObject);
    procedure Busca1Click(Sender: TObject);
    procedure Substitui1Click(Sender: TObject);
    procedure Fixa1Click(Sender: TObject);
    procedure AutoIdentao1Click(Sender: TObject);
    procedure CriaBackup1Click(Sender: TObject);
    procedure Tamanho1Click(Sender: TObject);
    procedure Desfaz1Click(Sender: TObject);
    procedure Refaz1Click(Sender: TObject);
    procedure Procurar1Click(Sender: TObject);
    procedure BuscaProximo1Click(Sender: TObject);
    procedure Editar1Click(Sender: TObject);
    procedure Arquivo1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Limpartudo1Click(Sender: TObject);
    procedure MarcarTexto1Click(Sender: TObject);
    procedure DesmarcarTexto1Click(Sender: TObject);
    procedure VaiparaoMarcador1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure TabSet1Click(Sender: TObject);
    procedure Arquivo_FecharClick(Sender: TObject);
    procedure SalvarEspecialClick(Sender: TObject);
    procedure Menu_TrocarClick(Sender: TObject);
  private
    FovcControl : TovcController;
    Deletando   : Boolean;
    
    Procedure DesmarcaItem;
    Procedure DocumentosClick(Sender : TObject);
  public
    PathName   : TStringList;
    Docs       : TList;
    ActiveDoc  : TOvcTextFileEditor;
    FindDLG    : TFindDlg;
    ReplaceDLG : TReplaceDLG;

    Procedure Clear;
    Procedure NewPage;
    Procedure GoLastLine;

    Procedure Write(Const S: String = ''); overload;
    Procedure Write(const S: String; BreakLineChar: Char; const Ident: String = ''); overload;
    Procedure WriteFmt(Const S: String; Arguments: array of const);
    Procedure WriteCenter(Const S: String; Width: Integer = 110);
    Procedure WriteNote(Const S: String; Center: Boolean = False);
    Procedure WriteStrings(SL: TStrings; Center: Boolean = False);
    Procedure WriteStringsIdent(SL: TStrings; const Ident: String = '    ');
    Procedure WriteInteger(i: Integer);

    Procedure WriteDoc(Index : Integer; S: String);
    Function  LastDoc: TOvcTextFileEditor;
    Function  FirstDoc: TOvcTextFileEditor;
    Function  GetDoc(Index : Integer) : TOvcTextFileEditor;
    Procedure SetDoc(Index : Integer);
    procedure OpenDoc(Const FileName: String);
    procedure SaveDoc(Const FileName: String);
  end;

  TEditAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TBitBtn;
    ProgramIcon: TImage;
    Panel2: TPanel;
    ProductName: TLabel;
    Version: TLabel;
    PBCreditos: TPaintBox;
    Bevel1: TBevel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    Timer1 : TTimer;
    Bitmap : TBitmap;
    i : Integer;
    Altura : Integer;
  public
    { Public declarations }
  end;

implementation
uses SysUtilsEx{, advClasses, advClasses_Messages};

{$R *.DFM}
{$R BUSCATXT.DFM}
{$R Editor_TrocarTexto.DFM}
{$R EDITABOU.DFM}
{$R SAVEESP.DFM}

{Funções comuns}

function IsReadOnly(const FileName: string): Boolean;
begin
  Result := Boolean(FileGetAttr(FileName) and faReadOnly);
  if Result then MessageDlg(Format('%s é somente de leitura.',
    [ExtractFilename(FileName)]), mtWarning, [mbOK], 0);
end;

{Implementação do Objeto TEditAboutBox}

procedure TEditAboutBox.FormActivate(Sender: TObject);
Var R : TRect;
    Centro : Integer;
    Inicio : Integer;
Const
    S1 = 'Copyright';
    S2 = 'Adriano Rochedo';
    S3 = '<rochedo@ufpel.tche.br>';
    S4 = 'Micro Editor de Textos';
    S5 = 'para aplicações em geral';
    S6 = 'Todos os direitos reservados.';
begin
  i := 0;
  Timer1 := TTimer.Create(Self);
  Timer1.OnTimer := Timer1Timer;
  Timer1.Interval := 100;
  Bitmap := TBitmap.Create;
  With Bitmap do Begin
    Width := PBCreditos.Width;
    Altura := 300;
    With Canvas do Begin
      Brush.Color := clBlack;
      R := Rect(0,0,257,Altura);
      FillRect(R);
      Height := Altura;

      Font.Size := 10;
      Inicio := 57;

      Font.Color := clRed;
      Centro := Width div 2 - TextWidth(S1) div 2;
      TextOut(Centro,Inicio,S1);

      Font.Color := clWhite;
      Centro := Width div 2 - TextWidth(S2) div 2;
      Inc(Inicio,20);
      TextOut(Centro,Inicio,S2);

      Font.Name := 'Times New Roman';
      Font.Size := 10;
      Font.Color := clRed;
      Centro := Width div 2 - TextWidth(S3) div 2;
      Inc(Inicio,20);
      TextOut(Centro,Inicio,S3);

      Font.Name := 'System';
      Font.Size := 10;
      Font.Color := clYellow;
      Centro := Width div 2 - TextWidth(S4) div 2;
      Inc(Inicio,20);
      TextOut(Centro,Inicio,S4);

      Font.Color := clWhite;
      Centro := Width div 2 - TextWidth(S5) div 2;
      Inc(Inicio,20);
      TextOut(Centro,Inicio,S5);

      Font.Name := 'Times New Roman';
      Font.Size := 8;
      Font.Color := clRed;
      Centro := Width - TextWidth(S6) - 20;
      Inc(Inicio,20);
      TextOut(Centro,Inicio,S6);
    End;
  End;
end;

procedure TEditAboutBox.FormDeactivate(Sender: TObject);
begin
  Timer1.Free;
  Bitmap.Free;
end;

procedure TEditAboutBox.Timer1Timer(Sender: TObject);
begin
  PBCreditos.Canvas.Draw(0,-i,Bitmap);
  Inc(i,1);
  If i > 170 Then i := -10;
end;

{Implementação do Objeto TEditor}

Procedure TEditor.DesmarcaItem;
Var i : Integer;
Begin
  For i := 0 to Documentos.Count-1 do
    If TMenuItem(Documentos.Items[i]).Checked Then
       Begin
       TMenuItem(Documentos.Items[i]).Checked := False;
       Break;
       End;
End;

Procedure TEditor.Clear;
Begin
  If ActiveDoc = Nil Then Exit;
  ActiveDoc.DeleteAll(True);
  If Visible Then ActiveDoc.ResetScrollBars(True);
End;

Procedure TEditor.NewPage;
Begin
  Novo1Click(Self);
End;

Procedure TEditor.Write(Const S: String = '');
Begin
  If ActiveDoc = Nil Then NewPage;
  ActiveDoc.AppendPara(pChar(S));
End;

Procedure TEditor.GoLastLine;
begin
  If ActiveDoc <> Nil Then
     ActiveDoc.ResetScrollBars(True);
end;

Procedure TEditor.WriteCenter(Const S: String; Width: Integer = 110);
begin
  Write( StrCenter(S, Width) );
end;

Procedure TEditor.WriteNote(Const S: String; Center: Boolean = False);
var i: Longword;
    st: Array[0..255] of Char;
begin
  i := 0;
  repeat
    strToken(s, i, ['|', ';', #13]);

    if s <> '' then
       if Center Then
          WriteCenter(StrPas(st))
       else
          Write(StrPas(st));

  until s = '';
end;

Procedure TEditor.WriteStrings(SL: TStrings; Center: Boolean = False);
var i: Integer;
begin
  if Center Then
     For i := 0 to SL.Count - 1 do WriteCenter(SL[i])
  else
     For i := 0 to SL.Count - 1 do Write(SL[i]);
end;

Procedure TEditor.WriteDoc(Index : Integer; S : String);
Var A : Array[0..255] of Char;
Begin
  If (Index > -1) and (Index < Docs.Count-1) Then
    TOvcTextFileEditor(Docs.Items[Index]).AppendPara(StrPCopy(A,S));
End;

Function TEditor.GetDoc(Index : Integer) : TOvcTextFileEditor;
Begin
  If (Index >= 0) and (Index < Docs.Count) Then
    GetDoc := TOvcTextFileEditor(Docs.Items[Index])
  Else
    GetDoc := ActiveDoc;
End;

Function  TEditor.LastDoc: TOvcTextFileEditor;
Begin
  SetDoc(Docs.Count - 1);
  Result := ActiveDoc;
End;

Function  TEditor.FirstDoc: TOvcTextFileEditor;
Begin
  SetDoc(0);
  Result := ActiveDoc;
End;

Procedure TEditor.SetDoc(Index : Integer);
Var i : Integer;
Begin
  If (Index > -1) and (Index < Docs.Count) and (ActiveDoc <> Nil) Then
     Begin
     ActiveDoc.Visible := False;
     ActiveDoc := TOvcTextFileEditor(Docs.Items[Index]);
     ActiveDoc.Visible := True;
     {gEditor.}Caption := PathName.Strings[ActiveDoc.Tag];
     TabSet1.TabIndex := Index;
     DesmarcaItem;
     For i := 0 to Documentos.Count-1 do
       If TMenuItem(Documentos.Items[i]).Tag = Index Then
         Begin
         TMenuItem(Documentos.Items[i]).Checked := True;
         Break;
         End;
    End;
End;

Procedure TEditor.DocumentosClick(Sender : TObject);
Begin
  { Desmarca o marcado }
  DesmarcaItem;
  { Atualiza }
  With Sender as TMenuItem do Begin
    Checked := True;
    SetDoc(Tag);
  End;
End;

procedure TEditor.Novo1Click(Sender: TObject);
Var Memo : TOvcTextFileEditor;
    NewItem : TMenuItem;
begin
  {Configuração Inicial}
  Memo := TOvcTextFileEditor.Create(Self);
  Memo.Parent := self{gEditor};
  Memo.Align := alClient;
  Memo.WordWrap := False;
  Memo.ScrollBars := ssBoth;
  Memo.FixedFont.Name := 'Courier New';
  Memo.FixedFont.Size := 8;
  Memo.OnShowStatus := Memo1ShowStatus;
  Memo.Tag := Docs.Add(Memo);
  If Self.Visible Then Memo.SetFocus;
  If CriaBackup1.Checked Then
    Memo.MakeBackup := True
  Else
    Memo.MakeBackup := False;


  {Troca das páginas ativas}
  If ActiveDoc <> Nil Then ActiveDoc.Visible := False;
  ActiveDoc := Memo;
  ActiveDoc.Visible := True;

  {Configurações finais}
  PathName.Add('C:\NONAME'+IntToStr(PathName.Count+1)+'.TXT');
  {gEditor.}Caption := PathName.Strings[ActiveDoc.Tag];

  {Insere o item de menu}
  DesmarcaItem;
  NewItem := TMenuItem.Create(Self);
  NewItem.Checked := True;
  NewItem.Caption := ExtractFileName(PathName.Strings[ActiveDoc.Tag]);
  NewItem.Tag := ActiveDoc.Tag;
  NewItem.OnClick := DocumentosClick;
  Documentos.Add(NewItem);

  {Insere uma nova Tab}
  TabSet1.Tabs.Add(ExtractFileName(PathName.Strings[ActiveDoc.Tag]));
  TabSet1.TabIndex := ActiveDoc.Tag;

  Editar1.Enabled     := True;
  Procurar1.Enabled   := True;
  Texto1.Enabled      := True;
  Documentos.Enabled  := True;

end;

procedure TEditor.OpenDoc(Const FileName: String);
Begin
  If not FileExists(FileName) then
     Raise Exception.Create('Arquivo não pode ser aberto:'#13 + FileName);

  If ActiveDoc = Nil Then NewPage;
  PathName.Strings[ActiveDoc.Tag] := FileName;
  ActiveDoc.LoadFromFile(PathName.Strings[ActiveDoc.Tag]);
  {Memo1.SelStart := 0;}
  Caption := PathName.Strings[ActiveDoc.Tag];
  Documentos.Items[ActiveDoc.Tag].Caption := ExtractFileName(Caption);
  TabSet1.Tabs.Strings[ActiveDoc.Tag] := ExtractFileName(Caption);
  ActiveDoc.Modified := False;
End;

procedure TEditor.SaveDoc(Const FileName: String);
Begin
  If ActiveDoc <> Nil Then
     Begin
     PathName.Strings[ActiveDoc.Tag] := Filename;
     Caption := PathName.Strings[ActiveDoc.Tag];
     Documentos.Items[ActiveDoc.Tag].Caption := ExtractFileName(Caption);
     TabSet1.Tabs.Strings[ActiveDoc.Tag] := ExtractFileName(Caption);
     Salvar1Click(nil);
     //SendMessageToObjects(FILE_CREATE, [Integer(Filename)]); <<<<<
     End;
End;

procedure TEditor.Abrir1Click(Sender: TObject);
begin
  if OpenFileDialog.Execute then
     OpenDoc(OpenFileDialog.FileName);
end;

procedure TEditor.Salvar1Click(Sender: TObject);
Var s: String;
begin
  s := PathName.Strings[ActiveDoc.Tag];
  if (s = '') {or IsReadOnly(s)} then
     SalvarComo1Click(Sender)
  else
     begin
     ActiveDoc.SaveToFile(s);
     ActiveDoc.Modified := False;      
     end;
end;

procedure TEditor.SalvarComo1Click(Sender: TObject);
begin
  if SaveFileDialog.Execute then
     SaveDoc(SaveFileDialog.FileName)
end;

procedure TEditor.Imprimir1Click(Sender: TObject);
var
  Line: LongInt;
  Buf: array[0..1023] of Char;
  StartLine, StopLine: LongInt;
  StartCol, StopCol: Integer;
  PrintText: System.Text;
begin
  if ActiveDoc.GetSelection(StartLine, StartCol, StopLine, StopCol) then
     PrintDialog1.Options := PrintDialog1.Options + [poSelection]
  else
     PrintDialog1.Options := PrintDialog1.Options - [poSelection];

  if PrintDialog1.Execute then
    begin
    AssignPrn(PrintText);
    try
      Rewrite(PrintText);
      Printer.Canvas.Font := ActiveDoc.FixedFont.Font;
      case PrintDialog1.PrintRange of
        prAllPages :
          for Line := 1 to ActiveDoc.LineCount do
            begin
            ActiveDoc.GetPrintableLine(Line, Buf, SizeOf(Buf));
            Writeln(PrintText, Buf);
            end;
        prSelection :
          for Line := StartLine to StopLine do
            begin
            ActiveDoc.GetPrintableLine(Line, Buf, SizeOf(Buf));
            if Line = StartLine then
               FillChar(Buf, StartCol, #32);
            if Line = StopLine then
               Buf[StopCol] := #0;
            Writeln(PrintText, Buf);
            end;
      end;
    finally
      System.Close(PrintText);
    end;
  end;
end;

procedure TEditor.Configurar1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TEditor.Sair1Click(Sender: TObject);
begin
  Close;
end;

procedure TEditor.Cortar1Click(Sender: TObject);
begin
  ActiveDoc.CutToClipboard;
end;

procedure TEditor.Copiar1Click(Sender: TObject);
begin
  ActiveDoc.CopyToClipboard;
end;

procedure TEditor.Colar1Click(Sender: TObject);
begin
  ActiveDoc.PasteFromClipboard;
end;

procedure TEditor.Apagar1Click(Sender: TObject);
begin
  {ActiveDoc.ClearSelection;}
end;

procedure TEditor.Selecionatudo1Click(Sender: TObject);
begin
  if ActiveDoc.LineCount = -1 then Exit;
  ActiveDoc.SelectAll(False);
end;

procedure TEditor.Word1Click(Sender: TObject);
begin
  with ActiveDoc do
  begin
    WordWrap := not WordWrap;
    if WordWrap then
      ScrollBars := ssNone
    else
      ScrollBars := ssBoth;
    Word1.Checked := WordWrap;
  end;
end;

procedure TEditor.Fonte1Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(ActiveDoc.FixedFont.Font);
  if FontDialog1.Execute then
    ActiveDoc.FixedFont.Assign(FontDialog1.Font);
end;

procedure TEditor.FormCreate(Sender: TObject);
begin
  FindDlg    := TFindDlg.Create(Self);
  ReplaceDLG := TReplaceDLG.Create(Self);

  PathName   := TStringList.Create;
  ActiveDoc  := Nil;
  Docs       := TList.Create;

  FovcControl := TovcController.Create(nil);
  InsertComponent(FovcControl);
end;

procedure TEditor.FormDestroy(Sender: TObject);
Var i : Integer;
begin
  FindDlg.Free;
  ReplaceDLG.Free;

  For i := 0 to Docs.Count-1 do
    TOvcTextFileEditor(Docs.Items[i]).Free;

  Docs.Free;
  PathName.Free;
end;

procedure TEditor.About1Click(Sender: TObject);
Var Diag : TEditAboutBox;
begin
  Diag := TEditAboutBox.Create(Self);
  Try
    Diag.ShowModal;
  Finally
    Diag.Free;
  End;
end;

procedure TEditor.Memo1ShowStatus(Sender: TObject; LineNum: Longint;
  ColNum: Integer);
begin
  Panel2.Caption := IntToStr(LineNum) + ':' + IntToStr(ColNum);
  Panel2.Update;
  if ActiveDoc.Modified then Panel3.Caption := 'Modificado';
  Panel4.Caption := 'Total: ' + IntToStr(ActiveDoc.LineCount);
  Panel5.Caption := 'Topo: ' + IntToStr(ActiveDoc.TopLine);
  Panel5.Update;
  Panel6.Caption := 'Bytes: ' + IntToStr(ActiveDoc.TextLength);
  if ActiveDoc.InsertMode then
    Panel7.Caption := 'Inserção'
  else
    Panel7.Caption := 'SobreEsc.';
end;

procedure TEditor.VaiparaLinha1Click(Sender: TObject);
var
  S : string;
begin
  S := '1';
  if InputQuery('Vai para Linha Número', 'Entre Número da Linha (1 até ' +
    IntToStr(ActiveDoc.LineCount) + ')', S) then
    try
      ActiveDoc.SetCaretPosition(StrToInt(S), 1);
    except
      Exit;
    end;
end;

procedure TEditor.Busca1Click(Sender: TObject);
var OptionSet : TSearchOptionSet;
begin
  FindDlg.ShowModal;
  if FindDlg.ModalResult = mrOK then
     begin
     OptionSet := [];
     if FindDlg.RadioButton2.Checked then
       OptionSet := OptionSet + [soBackward];
     if FindDlg.Checkbox1.Checked then
       OptionSet := OptionSet + [soMatchCase];
     if FindDlg.Checkbox2.Checked then
       OptionSet := OptionSet + [soGlobal];

     if not ActiveDoc.Search(FindDlg.Edit1.Text, OptionSet) then
       MessageDlg('Sequência procurada ''' + FindDlg.Edit1.Text +
         ''' não encontrada!!', mtInformation, [mbOk], 0);

     Procurar1Click(Self);
     end;
end;

procedure TEditor.Substitui1Click(Sender: TObject);
var
  OptionSet : TSearchOptionSet;
begin
  OptionSet := [];
  if FindDlg.RadioButton2.Checked then
    OptionSet := OptionSet + [soBackward];
  if FindDlg.Checkbox1.Checked then
    OptionSet := OptionSet + [soMatchCase];
  if not ActiveDoc.Search(FindDlg.Edit1.Text, OptionSet) then
    MessageDlg('Sequência procurada ''' + FindDlg.Edit1.Text +
      ''' não encontrada!!', mtInformation, [mbOk], 0);
end;

procedure TEditor.Fixa1Click(Sender: TObject);
begin
  Fixa1.Checked := False;
  Real1.Checked := False;
  Pequena1.Checked := False;
  TMenuItem(Sender).Checked := True;
  if Fixa1.Checked then ActiveDoc.TabType := ttFixed;
  if Real1.Checked then ActiveDoc.TabType := ttReal;
  if Pequena1.Checked then ActiveDoc.TabType := ttSmart;
end;

procedure TEditor.AutoIdentao1Click(Sender: TObject);
begin
  AutoIdentao1.Checked := not(AutoIdentao1.Checked);
  ActiveDoc.AutoIndent := AutoIdentao1.Checked;
end;

procedure TEditor.CriaBackup1Click(Sender: TObject);
begin
  CriaBackup1.Checked := not(CriaBackup1.Checked);
  ActiveDoc.MakeBackup := CriaBackup1.Checked;
end;

procedure TEditor.Tamanho1Click(Sender: TObject);
var
  S : string;
begin
  S := IntToStr(ActiveDoc.TabSize);
  if InputQuery('Tamanho da Tabulação', 'Entre Tamanho', S) then try
    ActiveDoc.TabSize := StrToInt(S);
  except
    Exit;
  end;
end;

procedure TEditor.Desfaz1Click(Sender: TObject);
begin
ActiveDoc.Undo;
end;

procedure TEditor.Refaz1Click(Sender: TObject);
begin
  ActiveDoc.Redo;
end;

procedure TEditor.Procurar1Click(Sender: TObject);
var b: Boolean;
begin
  b := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
  Busca1.Enabled        := b;
  VaiParaLinha1.Enabled := b;
  BuscaProximo1.Enabled := b and (FindDlg.Edit1.Text <> '');
  Menu_Trocar.Enabled   := b;
end;

procedure TEditor.BuscaProximo1Click(Sender: TObject);
var
  OptionSet : TSearchOptionSet;
begin
  OptionSet := [];
  if FindDlg.RadioButton2.Checked then
    OptionSet := OptionSet + [soBackward];
  if FindDlg.Checkbox1.Checked then
    OptionSet := OptionSet + [soMatchCase];
  if not ActiveDoc.Search(FindDlg.Edit1.Text, OptionSet) then
     MessageDlg('Sequência procurada ''' + FindDlg.Edit1.Text +
     ''' não encontrada!!', mtInformation, [mbOk], 0);
end;

procedure TEditor.Editar1Click(Sender: TObject);
var
  StartLine, StopLine: LongInt;
  StartCol, StopCol: Integer;
  IsSelected: Boolean;
begin
  SelecionaTudo1.Enabled := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
  IsSelected := (ActiveDoc <> Nil) and
                (ActiveDoc.GetSelection(StartLine, StartCol, StopLine, StopCol));
  Desfaz1.Enabled := (ActiveDoc <> Nil) and (ActiveDoc.CanUndo);
  Refaz1.Enabled  := (ActiveDoc <> Nil) and (ActiveDoc.CanRedo);
  Copiar1.Enabled := IsSelected;
  Cortar1.Enabled := IsSelected;
  OpenClipboard(Handle);
  Colar1.Enabled := (ActiveDoc <> Nil) and (GetClipboardData(cf_Text) <> 0);
  CloseClipboard;
  Apagar1.Enabled := IsSelected;
End;

procedure TEditor.Arquivo1Click(Sender: TObject);
begin
  Imprimir1.Enabled      := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
  Salvar1.Enabled        := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
  SalvarComo1.Enabled    := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
  SalvarEspecial.Enabled := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
  Arquivo_Fechar.Enabled := (ActiveDoc <> Nil) and (ActiveDoc.TextLength > 0);
end;

procedure TEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  ButtonSelected : Word;
begin
  if (ActiveDoc <> Nil) and (ActiveDoc.Modified) then
     begin
     ButtonSelected := MessageDlg('Texto modificado, salva ?',
       mtInformation, mbYesNoCancel, 0);

     if ButtonSelected in [mrYes] then
        begin
        Salvar1Click(Self);
        CanClose := True;
        end
     else
        CanClose := ButtonSelected in [mrNo];
     end;
end;

procedure TEditor.Limpartudo1Click(Sender: TObject);
begin
  Clear;
end;

procedure TEditor.MarcarTexto1Click(Sender: TObject);
var
  S : string;
begin
  S := '1';
  if InputQuery('Marca texto', 'Entre o Número do Marcador (1 até 10)', S) then
    try
      ActiveDoc.SetMarker(StrToInt(S));
    except
      Exit;
    end;
end;

procedure TEditor.DesmarcarTexto1Click(Sender: TObject);
var
  S : string;
begin
  S := '1';
  if InputQuery('Desmarca texto', 'Entre o Número do Marcador (1 até 10)', S) then
    try
      ActiveDoc.ClearMarker(StrToInt(S));
    except
      Exit;
    end;
end;

procedure TEditor.VaiparaoMarcador1Click(Sender: TObject);
var
  S : string;
begin
  S := '1';
  if InputQuery('Vai para o Marcador', 'Entre o Número do Marcador (1 até 10)', S) then
    try
      ActiveDoc.GotoMarker(StrToInt(S));
    except
      Exit;
    end;
end;

procedure TEditor.FormActivate(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  If Not Deletando then
     If ActiveDoc <> Nil Then ActiveDoc.ResetScrollBars(True);
end;

procedure TEditor.TabSet1Click(Sender: TObject);
begin
  {Rochedo, 07/05/1998}
  If Not Deletando then
     SetDoc(TabSet1.TabIndex);
end;

procedure TEditor.Arquivo_FecharClick(Sender: TObject);
Var Ind,i : Integer;
    Inutil: Boolean;
begin
  Ind := ActiveDoc.Tag;
  Deletando := True;

  {Verifica se o doc. foi modificado, se foi, pergunta se é para salvar.}
  FormCloseQuery(nil, Inutil);

  If TabSet1.TabIndex > -1 Then
     Begin
     ActiveDoc.Free;
     ActiveDoc := Nil;
     Docs.Delete(Ind);
     Documentos.Delete(Ind);
     PathName.Delete(Ind);
     TabSet1.Tabs.Delete(Ind);
     For i := 0 to Docs.Count-1 do
       Begin
       TOvcTextFileEditor(Docs.Items[i]).Tag := i;
       TMenuItem(Documentos.Items[i]).Tag := i;
       End;

     If TabSet1.Tabs.Count > 0 Then
        If Ind > 0 Then
           Begin
           ActiveDoc := TOvcTextFileEditor(Docs.Items[Ind-1]);
           SetDoc(Ind-1)
           End
        Else
           Begin
           ActiveDoc := TOvcTextFileEditor(Docs.Items[0]);
           SetDoc(0);
           End
     Else
        Begin
        ActiveDoc           := Nil;
        Editar1.Enabled     := False;
        Procurar1.Enabled   := False;
        Texto1.Enabled      := False;
        Documentos.Enabled  := False;
        Caption             := ' Editor';
        For i := 2 to 7 do
          TPanel(FindComponent('Panel'+IntToStr(i))).Caption := '';
        End;
     End;
  (*
  Else
     Begin               {Rochedo, 07/05/1998}
     Clear;
     PathName[Ind] := 'C:\NONAME1.TXT';
     End;
  *)
  Deletando := False;
end;

procedure TEditor.SalvarEspecialClick(Sender: TObject);
Var Diag : TSalvaEspDLG;
begin
  Diag := TSalvaEspDLG.Create(Self);
  Try
    Diag.Editor := self;
    Diag.ShowModal;
  Finally
    Diag.Free;
  End;
end;

procedure TSalvaEspDLG.OKBtnClick(Sender: TObject);

    Function ImprimeLinha(Const Linha: String): Boolean;
    Var i : Integer;
    Begin
      Result := True;
      For i := 0 to Palavras.Lines.Count-1 do
        If System.Pos(Palavras.Lines[i], Linha) <> 0 Then Begin
           Result := False;
           Break;
        End;
    End;

Var F : TextFile;
    i : Longint;

begin
  If Not FileExists(Dir.Directory + '\' + Nome.Text) Then
  Begin
    AssignFile(F, Dir.Directory + '\' + Nome.Text);

    Rewrite(F);
    For i := 1 to Editor.ActiveDoc.LineCount do
    begin
      If ImprimeLinha(Editor.ActiveDoc.Lines[i]) Then
         WriteLn(F, Editor.ActiveDoc.Lines[i]);
    end;
    CloseFile(F);

  End Else
    Raise Exception.Create('Arquivo já existe! Troque o Nome');
end;

procedure TSalvaEspDLG.SalvaPalavrasClick(Sender: TObject);
begin
  if SaveFileDialog.Execute then
     Palavras.Lines.SaveToFile(SaveFileDialog.FileName);
end;

procedure TSalvaEspDLG.RecuperaPalavrasClick(Sender: TObject);
begin
  if OpenFileDialog.Execute then
     Palavras.Lines.LoadFromFile(OpenFileDialog.FileName);
end;

procedure TEditor.Menu_TrocarClick(Sender: TObject);
var OptionSet : TSearchOptionSet;
    i         : Integer;
    s         : String;
begin
  ReplaceDlg.ShowModal;
  if ReplaceDlg.ModalResult = mrOK then
     begin
     OptionSet := [soReplaceAll];

     if ReplaceDlg.rbBT.Checked then
        OptionSet := OptionSet + [soBackward];

     if ReplaceDlg.cbCS.Checked then
        OptionSet := OptionSet + [soMatchCase];

     if ReplaceDlg.cbSPI.Checked then
        OptionSet := OptionSet + [soWholeWord];

     if ReplaceDlg.rbTS.Checked then
        OptionSet := OptionSet + [soSelText];

     if ReplaceDlg.rbTT.Checked then
        OptionSet := OptionSet + [soGlobal];

     i := ActiveDoc.Replace(ReplaceDlg.EdProcurar.Text,
                            ReplaceDlg.EdTrocar.Text, OptionSet);

     if i > -1 then
        begin
        if i = 1 then s := 'palavra trocada' else s := 'palavras trocadas';
        MessageDlg(format('%d %s.', [i, s]), mtInformation, [mbOk], 0);
        end;

     Procurar1Click(Self);
     end;
end;

procedure TEditor.WriteInteger(i: Integer);
begin
  Write(intToStr(i));
end;

procedure TEditor.WriteFmt(const S: String; Arguments: array of const);
begin
  Write(Format(S, Arguments));
end;

procedure TEditor.WriteStringsIdent(SL: TStrings; const Ident: String = '    ');
var i: Integer;
begin
  For i := 0 to SL.Count - 1 do
     Write(Ident + SL[i])
end;

procedure TEditor.Write(const S: String; BreakLineChar: Char; const Ident: String = '');
var SL: TStrings;
begin
  SL := nil;
  Split(S, SL, [BreakLineChar]);
  WriteStringsIdent(SL, Ident);
  SL.Free;
end;

end.

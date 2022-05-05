unit WinUtils;

interface
uses Classes,
     Windows,
     ShellAPI,
     Forms,
     StdCtrls,
     SysUtils,
     Controls,
     Dialogs,
     ExtCtrls,
     ComCTRLs,
     Graphics,
     Messages,
     TypInfo,
     actnList
{$IFDEF ORPHEUS}
     ,OvcEF;
{$ELSE}
     ;
{$ENDIF}

var
  PixelsPerInch: Integer;

  {Rotinas Gerais}
  procedure ShowError(Error: Word);
  procedure ShowShellExecuteError(code: cardinal);
  procedure StartWait();
  procedure StopWait();
  procedure Delay(msecs: integer);
  procedure AjustResolution(Form: TForm);

  {Use esta rotina quando existir a possibilidade de mudança manual de foco
   para o próximo componente que deveria receber automaticamente o foco (ordem)
   Ex: Sejam A e B dois componentes janelados onde A tem TabOrder = 0 e B tem
       TabOrder = 1. O foco está em A e no evento de saída (OnExit) queremos
       que o foco seje passado para B de qualquer maneira.}
  Procedure ForceFocus(C: TWinControl);

  Procedure UpDateCursor(Form: TForm);
  Procedure RemoveMessage(Wnd: HWnd; Message: Word);

  {Rotina que trabalham com componentes -----------------------------------------------}
  Procedure Clear(Component: Array of TObject);
  Procedure SetEnable(Component: Array of TControl; B: Boolean; aColor: TColor = -1);
  Procedure SetColor(Component: Array of TControl; Color: TColor);
  Procedure SetVisible(Component: Array of TControl; V: Boolean);
  Function  FindByTag(Form: TForm; Tag: Longint): TComponent;
  Procedure OnlySelection(List: TListBox; Edit: TEdit);
  Procedure MoreSelection(List: TListBox; Sender: TObject);
  Procedure MemoPos(Memo: TMemo; Var Line, Col: Integer);
  Procedure AddElemToList(Origem, Destino: TListBox; CanDuplicate: Boolean);
  Procedure DeleteElemFromList(LB: TListBox; Key: Word = VK_DELETE);
  Procedure DeleteElemFromListEx(LB: TListBox);
  Procedure WriteStatus(Status: TStatusBar; const SS: Array of String; Beep: Boolean);
  procedure SetFocus(C: TWinControl; var ChangingFocus: Boolean);
  Function  VerificaSelecao(Edits : Array of Const): Boolean;

  // Habilita/Desabilita o component, inclusive seus sub-components.
  Procedure EnableControl(c: TWinControl; Value: Boolean);

  {FindForm:
   Busca por uma janela (all-top level) olhando sua propriedade Tag
   A informação deverá estar referenciada nesta propriedade:
     EX: Form1.Tag := Longint(MyObject)
   Retorna nil se não encontrar}
  Function  FindForm(Information: Pointer): TForm;

  Procedure CascateWindows;

  Function  MessageToApplication(ClassName: String;
            Msg, wParam: Word; lParam: Cardinal): Boolean;

  // Mostra um erro e passa o foco para o controle CTRL_Focar.
  // Se CTRL_Desfocar = nil ou não for passado, CTRL_Desfocar será igual a CTRL_Focar.
  Procedure ShowErrorAndGoTo( Erro: Array of Const;
                              CTRL_Focar: TWinControl;
                              CTRL_Desfocar: TWinControl = nil );

  procedure PosicionarDialogo(DLG: TForm; Controle: TControl; Abaixo: Boolean = True);

  function CreateAction(ActionList   : TActionList;
                        ParentAction : TAction;
                        Text         : String;
                        Checked      : boolean;
                        Event        : TNotifyEvent;
                        aObj         : TObject): TAction;

implementation
uses Mask, SysUtilsEx;

Var
  WaitCount : Integer = 0;
  SaveCursor: TCursor = crDefault;
  WaitCursor: TCursor = crHourGlass;

procedure Delay(msecs: integer);
var
   FirstTickCount: Cardinal;
begin
   FirstTickCount := GetTickCount;
   repeat
     SysUtilsEx.ProcessMessages();
   until ((GetTickCount - FirstTickCount) >= Longint(msecs));
end;

Function VerificaSelecao(Edits : Array of Const): Boolean;
Var i : Byte;
Begin
  Result := True;
  For i := Low(Edits) to High(Edits) do
    If TEdit(Edits[i].VObject).Text = '' Then Begin
      Result := False;
      Break;
    End;
End;

{Use esta rotina quando existir a possibilidade de mudança manual de foco
 para o próximo componente que deveria receber automaticamente o foco (TabOrder)
 Ex: Sejam A e B dois componentes janelados onde A tem TabOrder = 0 e B tem
     TabOrder = 1. O foco está em A e no evento de saída de A (OnExit) queremos
     que o foco seje passado para B de qualquer maneira.}
Procedure ForceFocus(C: TWinControl);
Begin
  GetParentForm(C).ActiveControl := Nil;
  C.ControlState := [];
  Windows.SetFocus(C.Handle);
  PostMessage(C.Handle, WM_LButtonUp, 0,0);
End;

Procedure UpDateCursor(Form: TForm);
Begin
  PostMessage(Form.Handle, WM_LButtonDown, 0,0);
  PostMessage(Form.Handle, WM_LButtonUp, 0,0);
End;

Function  FindForm(Information: Pointer): TForm;
var i: Integer;
Begin
  Result := nil;
  For i := 0 to Screen.FormCount - 1 do
    If Screen.Forms[i].Tag = Longint(Information) Then
       Begin
       Result := Screen.Forms[i];
       Break;
       End;
End;

Procedure CascateWindows;
var i, x, y, tx, ty, cy: Integer;
Begin
  cy := GetSystemMetrics(SM_CYCAPTION) + 5;
  tx := GetSystemMetrics(SM_CXFULLSCREEN);
  ty := GetSystemMetrics(SM_CYFULLSCREEN);

  x := 1; y := 1;
  For i := Screen.FormCount-1 downto 0 do
    if Screen.Forms[i] <> Application.MainForm then
       begin
       Screen.Forms[i].Left := x;
       Screen.Forms[i].Top  := y;
       inc(x, cy); inc(y, cy);
       if (x + Screen.Forms[i].Width > tx) then x := 1;
       if (y + Screen.Forms[i].Height > ty) then y := 1;
       end;
end;

// Mostra um erro e passa o foco para o controle CTRL_Focar.
// Se CTRL_Desfocar = nil ou não for passado, CTRL_Desfocar será igual a CTRL_Focar.
Procedure ShowErrorAndGoTo( Erro: Array of Const;
                            CTRL_Focar: TWinControl;
                            CTRL_Desfocar: TWinControl = nil);

Var S            : String;
    p            : Pointer;
    i            : Integer;
    NumControles : Integer;
    Controle     : TEdit; // Qualquer classe que possua o Evento OnExit Público
    Aux          : Array[1..10] of record Controle: Pointer; OnExit: TNotifyEvent end;
{$IFDEF ORPHEUS}
    OnAfterExit     : TNotifyEvent;
    ControleOrpheus : TWinControl;
{$ENDIF}
Begin
  if CTRL_Desfocar = nil then
     CTRL_Desfocar := CTRL_Focar;

  If Erro[0].vType = vtInteger Then
     S := LoadStr(Erro[0].vInteger)
  Else
     S := String(Erro[0].vPChar);

  { Desarma todos os eventos OnExits do controle que está perdendo
   o foco e de seus pais e salva-os}
  NumControles := 0;
  Controle := TEdit(CTRL_Desfocar);
  while Controle.Parent <> nil do
    begin
    p := GetPropInfo(Controle.ClassInfo, 'OnExit');
    if p <> nil then
       begin
       inc(NumControles);
       Aux[NumControles].OnExit   := Controle.OnExit;
       Aux[NumControles].Controle := Controle;
       Controle.OnExit            := nil;
       end;
    Controle := TEdit(Controle.Parent);
    end;

{$IFDEF ORPHEUS}
  ControleOrpheus := TWinControl(Controle);
  if ControleOrpheus is TOvcBaseEntryField then
     begin
     OnAfterExit := TOvcBaseEntryField(ControleOrpheus).AfterExit;
     TOvcBaseEntryField(ControleOrpheus).AfterExit := nil;
     end;
{$ENDIF}

  try
    Abort;
  finally
    MessageDLG(S, mtError, [mbOk], 0);
    ForceFocus(CTRL_Focar);
    UpDateCursor(TForm(GetParentForm(CTRL_Focar)));

    // Recupera os Eventos salvos
    for i := 1 to NumControles do
      TEdit(Aux[i].Controle).OnExit := Aux[i].OnExit;

{$IFDEF ORPHEUS}
    if ControleOrpheus is TOvcBaseEntryField then
       TOvcBaseEntryField(ControleOrpheus).AfterExit := OnAfterExit;
{$ENDIF}
  end;
End;

Procedure RemoveMessage(Wnd: HWnd; Message: Word);
Var Msg: TMsg;
Begin
  PeekMessage(Msg, Wnd, Message, Message, PM_REMOVE);
End;

Function MessageToApplication(ClassName: String;
                              Msg, wParam: Word; lParam: Cardinal): Boolean;
var H: THandle;
    Tid: Longword;
Begin
  H := FindWindow(pChar(ClassName), nil);
  if H <> 0 then
     begin
     Tid := GetWindowThreadProcessId(H, nil);
     if Tid <> 0 then PostThreadMessage (Tid, Msg, wParam, lParam);
     end;
End; {MessageToApplication}

Procedure Clear(Component: Array of TObject);
var i : Byte;
begin
  For i := 0 to High(Component) do
    If (Component[i] is TCustomEdit) Then
       (Component[i] as TCustomEdit).Clear Else
    If (Component[i] is TCustomComboBox) Then
       (Component[i] as TCustomComboBox).Clear Else
    If (Component[i] is TCustomListBox) Then
       (Component[i] as TCustomListBox).Clear Else
    If (Component[i] is TCustomLabel) Then
       (Component[i] as TCustomLabel).Caption := '' Else
    If (Component[i] is TPanel) Then
       (Component[i] as TPanel).Caption := '' Else
{$IFDEF ORPHEUS}
    if (Component[i] is TOvcBaseEntryField)  Then
       (Component[i] as TOvcBaseEntryField).ClearContents Else
{$ENDIF}
    If (Component[i] is TCustomComboBox)  Then
       begin
       TComboBox(Component[i]).ItemIndex := -1;
       TComboBox(Component[i]).Text := '';
       end
    else //...
End; {05/08/1999}

Procedure SetEnable(Component: Array of TControl; B: Boolean; aColor: TColor = -1);
var i : Byte;
begin
  For i := 0 to High(Component) do
    begin
    Component[i].Enabled := B;
    if aColor <> -1 then
       TForm(Component[i]).Color := aColor;  //soh para enganar o compilador
    end;
End;

Procedure SetColor(Component: Array of TControl; Color: TColor); {21/08/97}
var i : Byte;
begin
  For i := 0 to High(Component) do
    TForm(Component[i]).Color := Color;
End;

Procedure SetVisible(Component: Array of TControl; V: Boolean); {21/03/97}
var i : Byte;
begin
  For i := 0 to High(Component) do
    Component[i].Visible := V;
End;

Function FindByTag(Form: TForm; Tag: Longint): TComponent;
var
  I: Integer;
begin
  Result := Nil;
  for I := 0 to Form.ComponentCount - 1 do
     if Form.Components[I].Tag = Tag then
     Begin
       Result := Form.Components[I];
       Break;
     End;
end;

Procedure OnlySelection(List: TListBox; Edit: TEdit);
Var i : Integer;
Begin
  If List.MultiSelect Then
     Begin
     {Verifica Número de Selecionados}
     If List.SelCount = 1 Then
        For i := 0 to List.Items.Count-1 do
           If List.Selected[i] Then
              Begin
              Edit.Text := List.Items[i];
              Break;
              End
     End
  Else
     If List.ItemIndex > -1 Then
        Edit.Text := List.Items[List.ItemIndex];
End;

Procedure MoreSelection(List: TListBox; Sender: TObject);
Var i : Integer;
    S : String;
Begin
  If Sender is TEdit then
     Begin
     S := '';
     {Faz manualmente}
     For i := 0 to List.Items.Count-1 do
       If List.Selected[i] Then
          S := S + List.Items[i] + ',';
     Delete(S, Length(S),1);
     TEdit(Sender).Text := S
     End;
End;

Procedure MemoPos(Memo: TMemo; Var Line, Col: Integer);
Begin
  With Memo do
    Begin
    Line := Perform(EM_LINEFROMCHAR, SelStart, 0);
    Col := SelStart - Perform(EM_LINEINDEX, Line, 0);
    End;
End;

Procedure AddElemToList(Origem, Destino: TListBox; CanDuplicate: Boolean);

   Procedure Insere(Const s: String);
   Begin
     If Not CanDuplicate Then
        If Destino.Items.IndexOf(s) = -1 Then
           Destino.Items.Add(s)
        Else
           {Nada}
     Else
        Destino.Items.Add(s);
   End;

var i: Integer;
Begin
  If Not Origem.MultiSelect Then
     If Origem.ItemIndex <> -1 Then
        Insere(Origem.Items[Origem.ItemIndex])
     Else
        {Nada}
  Else
     For i := 0 to Origem.Items.Count - 1 do
       If Origem.Selected[i] Then
          Insere(Origem.Items[i]);
End;

Procedure DeleteElemFromList(LB: TListBox; Key: Word = VK_DELETE);
Begin
  If Key = VK_DELETE Then
     If LB.ItemIndex > -1 Then
        LB.Items.Delete(LB.ItemIndex);
End;

{Exclui todos os elementos que estao selecionados na LB}
Procedure DeleteElemFromListEx(LB: TListBox);
var i, ii : integer;
begin
  ii := LB.Items.Count;
  If LB.SelCount > 0 Then
      For i := ii-1 DownTo 0 Do
        If LB.Selected[i] Then
           LB.Items.Delete(i);
end;

Procedure WriteStatus(Status: TStatusBar; const SS: Array of String; Beep: Boolean);
var i: Integer;
    x: TStatusPanel;
    Apagar: Boolean;
Begin
  if Application.MainForm = nil then Exit;
  Apagar := ( Status.Panels.Count <> Length(SS) );

  Status.Panels.BeginUpdate();
  if Apagar then Status.Panels.Clear();
  For i := Low(SS) to High(SS) do
    begin
    if Apagar then x := Status.Panels.Add() else x := Status.Panels[i];
    x.Width := Application.MainForm.Canvas.TextWidth('M' + SS[i] + 'M');

    // ATENCAO: Os textos dos paineis são limitados em 127 caracteres
    x.Text := ' ' + SS[i] + ' ';
    end;
  Status.Panels.EndUpdate();

  // Chama a atenção do usuário
  if Beep then
     begin
     Windows.Beep(2000, 500);
     for i := 1 to 3 do
       begin
       {TODO 1 -cProgramacao: Definir pausa blocante !!}
       Status.Color := clRED;
       Delay(200);
       Status.Color := clBtnFace;
       Delay(200);
       end;
     end;
End;

procedure SetFocus(C: TWinControl; var ChangingFocus: Boolean);
begin
  ChangingFocus := True;
  C.SetFocus;
  ChangingFocus := False;
end;

procedure StartWait;
begin
  if WaitCount = 0 then
     begin
     SaveCursor := Screen.Cursor;
     Screen.Cursor := WaitCursor;
     end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then
     begin
     Dec(WaitCount);
     if WaitCount = 0 then Screen.Cursor := SaveCursor;
     end;
end;

Procedure ShowError(Error: Word);
Begin
  ShowMessage(LoadStr(Error));
End;

procedure PosicionarDialogo(DLG: TForm; Controle: TControl; Abaixo: Boolean = True);
var p: TPoint;
    Pai: TWinControl;
begin
  Pai := Controle.Parent;
  p := Point(Controle.Left, Controle.Top);
  p := Pai.ClientToScreen(p);
  DLG.Left := p.x - byte(Abaixo);
  if Abaixo then
     DLG.Top := p.y + Controle.Height
  else
     DLG.Top := p.y - DLG.Height;
end;

procedure AjustResolution(Form: TForm);
begin
  If Screen.PixelsPerInch <> PixelsPerInch Then
     Form.ScaleBy(PixelsPerInch, Screen.PixelsPerInch);
end;

// Habilita/Desabilita o component, inclusive seus sub-components.
Procedure EnableControl(c: TWinControl; Value: Boolean);
var i: Integer;
begin
  c.Enabled := Value;

  for i := 0 to c.ComponentCount-1 do
    if c.Components[i] is TControl then
       (c.Components[i] as TControl).Enabled := Value;

  for i := 0 to c.ControlCount-1 do
       c.Controls[i].Enabled := Value;
end;

function CreateAction(ActionList   : TActionList;
                      ParentAction : TAction;
                      Text         : String;
                      Checked      : boolean;
                      Event        : TNotifyEvent;
                      aObj         : TObject): TAction;
begin
  Result := TAction.Create(nil);
  Result.Caption := Text;
  Result.OnExecute := Event;
  Result.Tag := integer(aObj);
  Result.Checked := Checked;

  if ParentAction = nil then
     Result.ActionList := ActionList
  else
     ParentAction.InsertComponent(Result);
end;

procedure ShowShellExecuteError(code: cardinal);
var s: string;
begin
  if code <= 32 then
     begin
     case code of
       0                               : s := 'The operating system is out of memory or resources';
       Windows.ERROR_BAD_FORMAT        : s := 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image)';
       ShellAPI.SE_ERR_ACCESSDENIED    : s := 'The operating system denied access to the specified file';
       ShellAPI.SE_ERR_ASSOCINCOMPLETE : s := 'The filename association is incomplete or invalid';
       ShellAPI.SE_ERR_DDEBUSY         : s := 'The DDE transaction could not be completed because other DDE transactions were being processed';
       ShellAPI.SE_ERR_DDEFAIL         : s := 'The DDE transaction failed';
       ShellAPI.SE_ERR_DDETIMEOUT      : s := 'The DDE transaction could not be completed because the request timed out';
       ShellAPI.SE_ERR_DLLNOTFOUND     : s := 'The specified dynamic-link library was not found';
       ShellAPI.SE_ERR_FNF             : s := 'The specified file was not found';
       ShellAPI.SE_ERR_NOASSOC         : s := 'There is no application associated with the given filename extension';
       ShellAPI.SE_ERR_OOM             : s := 'There was not enough memory to complete the operation';
       ShellAPI.SE_ERR_PNF             : s := 'The specified path was not found';
       ShellAPI.SE_ERR_SHARE           : s := 'A sharing violation occurred';
       end;

     raise Exception.Create(s);
     end;
end;

initialization
  PixelsPerInch := 96;
end.

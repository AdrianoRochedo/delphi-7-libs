unit hidro_AreaDeProjeto;

interface

uses Types, Classes, Graphics, Forms, Controls, StdCtrls, ComCtrls, Dialogs, Menus,
     Windows, OleCtrls,
     MapObjects,
     MapObjectsEx,
     hidro_Classes,
     hidro_Tipos,
     hidro_Dialogo_Projeto;
     
type
  // Comunicação extra com outros objetos
  // ID: Identificação da Mensagem
  // Objeto: Objeto que está enviando a mensagem
  TEvento_Mensagem = procedure (const ID: String; Objeto: TObject) of Object;

  // Obtem o Status de um botão, isto é, se ele está precionado ou não
  TEvento_StatusDoBotao = function (const NomeBotao: String): boolean of Object;

  // Notificação de acontecimento
  TEvento_Aviso = procedure (const Aviso: String) of Object;

  // Definição da Área de Projeto
  THidroAreaDeProjeto = class(TForm)
    SaveDialog: TSaveDialog;
    Progresso: TProgressBar;
    mmMensagemInicial: TMemo;
    Map: TMapEx;
    StatusBar: TStatusBar;
    procedure Form_Close(Sender: TObject; var Action: TCloseAction);
    procedure Map_Click(Sender: TObject);
    procedure Map_Dbl_Click(Sender: TObject);
    procedure Map_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Map_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Map_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Form_Create(Sender: TObject);
    procedure Form_Destroy(Sender: TObject);
    procedure Form_Activate(Sender: TObject);
    procedure Form_CloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_Resize(Sender: TObject);
    procedure Form_Show(Sender: TObject);
    procedure Map_AfterLayerDraw(ASender: TObject; index: Smallint; canceled: WordBool; hDC: Cardinal);
    procedure MapBeforeLayerDraw(ASender: TObject; index: Smallint; hDC: Cardinal);
  private
    FProjeto             : TProjeto;

    FZoomFactor          : Real;
    FScale               : Real;

    gOldPos              : TPoint;
    gDesloc              : TPoint;

    FArrastando          : Boolean;
    Fpc1                 : TPC;
    FSB                  : TSubBacia;
    FDer                 : TDerivacao;
    FObjSel              : THidroComponente;
    FTN                  : TStrings;
    FGO_NA               : TSetByte;
    FPerguntar_TemCerteza: Boolean;
    FFechando            : Boolean;
    FBloqueado           : Boolean;
    FMT                  : Boolean;
    FMF                  : Boolean;
    FLendo               : Boolean;

    // Canvas
    FMapCanvas           : TControlCanvas;
    FPenColor            : TColor;
    FBrushColor          : TColor;
    FPenMode             : TPenMode;
    FLockCanvas          : Integer;

    // Eventos
    FOnMessage           : TEvento_Mensagem;
    FOnGetButtonStatus   : TEvento_StatusDoBotao;
    FOnShowMessage       : TEvento_Aviso;

    procedure SaveCanvas;
    procedure RestoreCanvas;

    function ObterObjetoAssociado(Sender: TObject): THidroComponente;
    procedure SetObjSel(const Value: THidroComponente);
    procedure SelecionarObjeto(sender: TObject);

    // Desenha o Frame no objeto selecionado
    procedure DesenharSelecao(Selecionar: Boolean);
  protected
    // Torna possível a persoanalização da atualização da tela
    procedure AtualizacaoVisualDoUsuario; virtual;

    // Verifica se um Frame pode ser desenhado no objeto passado
    function DesenharFrame(obj: THidroComponente): boolean; virtual;

    // Desenha as linhas de conexão entre os objetos
    // Conecta PCs a PCs e PCs a SubBacias
    procedure ConectarObjetos; virtual;

    // Dispara um evento de click em um dos objetos, inclusive a própria Área De Projeto
    procedure ExecutarClick(Sender: TObject; p: ImoPoint; Obj: THidroComponente); virtual;

    // Responsável por criar a instância correta do projeto
    function CriarProjeto(TN: TStrings): TProjeto; virtual;

    // Gerencia as regras de conecção dos objetos
    function ObjetoPodeSerConectadoEm(const ID: String; Objeto: THidroComponente): Boolean; virtual;
  public
    procedure DesenharRede;
    procedure RemoverObjeto(Obj: THidroComponente);

    procedure Salvar;

    // Salva e recupera as propriedades relativas as Área de Projeto
    procedure SalvarEmArquivo(Ini: TIF); virtual;
    procedure LerDoArquivo(Ini: TIF); virtual;

    property  Perguntar_TemCerteza: Boolean
         read FPerguntar_TemCerteza
        write FPerguntar_TemCerteza;

    property  MostrarImagemFundo : Boolean                read FMF          write FMF;
    property  MostrarTrechos     : Boolean                read FMT          write FMT;

    property  Bloqueado          : Boolean                read FBloqueado   write FBloqueado;
    property  ObjetoSelecionado  : THidroComponente       read FObjSel      write SetObjSel;
    property  GO_NosAbertos      : TSetByte               read FGO_NA       write FGO_NA;
    property  TabelaDeNomes      : TStrings               read FTN;

    property  Projeto            : TProjeto               read FProjeto;

    // Avisa que a área de projeto está em processo de leitura
    property  Lendo : Boolean read FLendo write FLendo;

    // Eventos
    property Evento_Mensagem      : TEvento_Mensagem      read FOnMessage         write FOnMessage;
    property Evento_StatusDoBotao : TEvento_StatusDoBotao read FOnGetButtonStatus write FOnGetButtonStatus;
    property Evento_Aviso         : TEvento_Aviso         read FOnShowMessage     write FOnShowMessage;
  end;

implementation
uses // Gerais
     SysUtils,
     WinUtils,
     Shapes,
     ExtCtrls,
     FileCtrl,
     MessageManager,
     GraphicUtils,
     ErrosDLG,

     // Projeto
     hidro_Constantes,
     hidro_Variaveis;

{$R *.DFM}

procedure THidroAreaDeProjeto.SelecionarObjeto(Sender: TObject);
var Obj: THidroComponente;
    p, c: TPoint;
    TD: TTrechoDagua;
    i: Integer;
begin
  if Sender = nil then
     SetObjSel(nil)
  else
     if FOnGetButtonStatus('Selecionar') or FOnGetButtonStatus('Arrastar') then
        begin

        if Sender is TMap then
           begin

           // Pode ser um trecho-dágua
           p := ScreenToClient(Mouse.CursorPos);
           for i := 0 to Projeto.PCs.PCs - 1 do
             if Projeto.PCs[i].TrechoDagua <> nil then
                begin
                TD := Projeto.PCs[i].TrechoDagua;
                c  := CentroDaReta(TD.PC_aMontante.ScreenPos, TD.PC_aJusante.ScreenPos);
                if DistanciaEntre2Pontos(p, c) < 20 {Pixels} then
                   begin
                   SelecionarObjeto(TD);
                   Exit;
                   end;
                end;

           // Senão é a própria janela
           SetObjSel(nil);
           WriteStatus(gSB, ['Objeto selecionado: Janela de Projeto']);
           if Assigned(FOnMessage) then
              FOnMessage('Descrever Objeto', FProjeto);
           end
        else
           SetObjSel(ObterObjetoAssociado(Sender));
        end;
end;

procedure THidroAreaDeProjeto.Form_Close(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  getMessageManager.SendMessage(UM_LM_UPDATE_LAYER, [Map, nil]);
end;

procedure THidroAreaDeProjeto.Map_Click(Sender: TObject);
var p: TPoint;
begin
  if mmMensagemInicial <> nil then
     begin
     self.RemoveControl(mmMensagemInicial);
     mmMensagemInicial := nil;
     end;

  GetCursorPos(p);
  p := ScreenToClient(p);

  ExecutarClick(Sender, Map.ToMapPoint(p.x, p.y), ObterObjetoAssociado(Sender));
end;

procedure THidroAreaDeProjeto.Map_Dbl_Click(Sender: TObject);
var p, c : TPoint;
    i    : Integer;
    TD   : TTrechoDagua;
begin
  if FProjeto.Simulador <> nil then Exit;

  // Verifica se o Duplo clique foi em um Trecho Dagua
  p := ScreenToClient(Mouse.CursorPos);
  for i := 0 to FProjeto.PCs.PCs - 1 do
    if FProjeto.PCs[i].TrechoDagua <> nil then
       begin
       TD := FProjeto.PCs[i].TrechoDagua;
       c  := CentroDaReta(TD.PC_aMontante.ScreenPos, TD.PC_aJusante.ScreenPos);
       if DistanciaEntre2Pontos(p, c) < 20 {Pixels} then
          begin
          TD.MostrarDialogo;
          Exit;
          end;
       end;

  // Se o clique não foi no Trecho Dagua é porque foi na janela
  FProjeto.MostrarDialogo;

  // Notifica possível mudança de status
  if Assigned(FOnMessage) then FOnMessage('Atualizar Opcoes', self);
end;

procedure THidroAreaDeProjeto.Map_MouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
var p, c: TPoint;
    TD: TTrechoDagua;
    i: Integer;
begin
  SaveCanvas;

  if (Button = mbLeft) then
     begin
     if FOnGetButtonStatus('Selecionar') then
        begin
        SelecionarObjeto(Sender);
        if (Sender is TMap) then
           Map.Extent := Map.TrackRectangle
        end
     else
        if FOnGetButtonStatus('Arrastar') then
           begin
           SelecionarObjeto(Sender);
           FMapCanvas.Handle := Windows.GetDC(Map.Handle);
           if FBloqueado Then Exit;
           FArrastando := True;
           gOldPos := TControl(Sender).ScreenToClient(Types.Point(x,y));
           gDesloc := Types.Point(x, y);
           FMapCanvas.Pen.Mode := pmXor;
           Screen.Cursor := crHandPoint;
           end
     end
  else
     // Mostrar Menu
     if Sender is TMap then
        begin
        p := Types.Point(x, y);
        for i := 0 to Projeto.PCs.PCs - 1 do
          if Projeto.PCs[i].TrechoDagua <> nil then
             begin
             TD := Projeto.PCs[i].TrechoDagua;
             c  := CentroDaReta(TD.PC_aMontante.ScreenPos, TD.PC_aJusante.ScreenPos);
             if DistanciaEntre2Pontos(p, c) < 20 {Pixels} then
                begin
                SelecionarObjeto(TD);
                TD.MostrarMenu(Mouse.CursorPos.x, Mouse.CursorPos.y);
                Exit;
                end;
             end;

         // Mostra o Menu da Janela se existir
        end
     else
        begin
        SelecionarObjeto(Sender);
        FObjSel.MostrarMenu;
        end;

  RestoreCanvas;
end;

procedure THidroAreaDeProjeto.Map_MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var p: TPoint;
    c: Tcontrol;
begin
  if FArrastando then
     if Sender is TMap then
        //...
     else
        begin
        c := TControl(Sender);
        p := ScreenToClient(Mouse.CursorPos);
        FMapCanvas.Rectangle(gOldPos.x, gOldPos.y, gOldPos.x + c.Width, gOldPos.y + c.Height);

        dec(p.x, gDesloc.x); dec(p.y, gDesloc.y);
        if p.x < 2 then p.x := 2;
        if p.y < 2 then p.y := 2;

        gOldPos := p;
        FMapCanvas.Rectangle(gOldPos.x, gOldPos.y, gOldPos.x + c.Width, gOldPos.y + c.Height);
        end;
end;

procedure THidroAreaDeProjeto.Map_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  if (Button = mbLeft) and not (Sender is TMap) and
     FOnGetButtonStatus('Arrastar') then
     begin
     if FBloqueado Then Exit;
     FArrastando := False;

     //SaveCanvas;
     p := ScreenToClient(Mouse.CursorPos);
     dec(p.x, gDesloc.x); dec(p.y, gDesloc.y);
     if p.x < 2 then p.x := 2;
     if p.y < 2 then p.y := 2;

     if (TControl(Sender).Left <> p.x) or (TControl(Sender).Top <> p.y) then
        begin
        FObjSel.Modificado := True;
        FObjSel.Pos := Map.ToMapPoint(p.x + TControl(Sender).Width div 2,
                                      p.y + TControl(Sender).Height div 2);
        end;

     Windows.ReleaseDC(Map.hWnd, FMapCanvas.Handle);

     Screen.Cursor := crDefault;
     FMapCanvas.Pen.Mode := pmCopy;
     Map.Refresh;
     end;
end;

procedure THidroAreaDeProjeto.Form_Create(Sender: TObject);
var IM: ImoImageLayer;
begin
  Caption := ' SEM NOME';
  FMT := True;
  FMF := True;
  FTN := TStringList.Create; // Tabela de Nomes
  FPerguntar_TemCerteza := True;
  FProjeto := CriarProjeto(FTN);
  FMapCanvas := TControlCanvas.Create;

  // teste <<<<<
  IM := coImageLayer.Create;
  IM.Name := 'Area de Projeto';
  Map.Layers.Add(IM);
  // teste
end;

procedure THidroAreaDeProjeto.Form_Destroy(Sender: TObject);
var i : Integer;
begin
  WriteStatus(gSB, ['']); // limpa a barra de Status

  if Assigned(FOnMessage) then
     FOnMessage('Destruicao da Area de Projeto', self);

  FProjeto.Free;
  FTN.Free;  // eliminar a tabela de nomes por último

  {É necessário eliminarmos todos os objetos do tipo TdrBaseShape da lista Controls
   que o formulário contém para que o Delphi não destrua os objetos. Os objetos tem
   que serem destruídos por aqueles que os criaram !!}
  i := 0;
  while i < ControlCount do
    begin
    if Controls[i] is TdrBaseShape then
       begin
       RemoveControl(Controls[i]);
       continue;
       end;
    inc(i);
    end;

  FMapCanvas.Free;
end;

procedure THidroAreaDeProjeto.ConectarObjetos;
var i, j, k: Integer;
    p1, p2: TPoint;
    Ob, Ob2: THidroComponente;
    PC: TPC;
begin
  SaveCanvas;

  for i := 0 to FProjeto.PCs.PCs - 1 do
    begin
    // Liga o PC montante ao PC jusante
    PC := FProjeto.PCs[i].PC_aJusante;

    if (PC <> nil) and FMT then
       begin
       FMapCanvas.Pen.Style := psSolid;
       FMapCanvas.Pen.Color := clBlue;
       FMapCanvas.Pen.Width := 1;
       DesenhaSeta(
          FMapCanvas,
          FProjeto.PCs[i].ScreenPos,
          PC.ScreenPos,
          15,
          DistanciaEntre2Pontos(FProjeto.PCs[i].ScreenPos, PC.ScreenPos) div 2
          );
       end;

    // Liga os objetos conectados ao PC com uma linha pontilhada
    FMapCanvas.Pen.Style := psSolid;
    FMapCanvas.Pen.Color := clRed;
    FMapCanvas.Pen.Width := 2;
    for j := 0 to FProjeto.PCs[i].Derivacoes - 1 do
      begin
      Ob := THidroComponente(FProjeto.PCs[i].Derivacao[j]);
      if Ob <> nil then
         begin
         p1 := FProjeto.PCs[i].ScreenPos;
         p2 := Ob.ScreenPos;
         FMapCanvas.MoveTo(p1.x, p1.y);
         FMapCanvas.LineTo(p2.x, p2.y);
         end;
      end; // for j ... --> para todas as Derivacoes

    // Liga os objetos conectados ao PC com uma linha pontilhada
    FMapCanvas.Pen.Style := psDot;
    FMapCanvas.Pen.Color := clBlack;
    FMapCanvas.Pen.Width := 1;
    for j := 0 to FProjeto.PCs[i].SubBacias - 1 do
      begin
      Ob := THidroComponente(FProjeto.PCs[i].SubBacia[j]);
      if Ob <> nil then
         begin
         p1 := FProjeto.PCs[i].ScreenPos;
         p2 := Ob.ScreenPos;
         FMapCanvas.MoveTo(p1.x, p1.y);
         FMapCanvas.LineTo(p2.x, p2.y);
         end;
      end; // for j ... --> para todas as SubBacia
    end; // para todos os PCs

  RestoreCanvas;
end;

procedure THidroAreaDeProjeto.DesenharRede;
begin
  if FLendo then Exit;

  SaveCanvas;

  // Desenha as Conecções
  ConectarObjetos;

  // Força o desenho de cada objeto
  GetMessageManager.SendMessage(UM_REPINTAR_OBJETO, [Self]);

  // Seleciona o Objeto;
  DesenharSelecao(True);

  // Desenho Extra
  AtualizacaoVisualDoUsuario;

  RestoreCanvas;
end;

function THidroAreaDeProjeto.ObterObjetoAssociado(Sender: TObject): THidroComponente;
begin
  Result := nil;
  if (Sender is TdrBaseShape) or (Sender is TImage) then
     Result := THidroComponente(TComponent(Sender).Tag)
  else
  if Sender is TTrechoDagua then
     Result := TTrechoDagua(Sender);
end;

procedure THidroAreaDeProjeto.Form_Activate(Sender: TObject);
begin
  if not FFechando then
     begin
     if Assigned(FOnMessage) then FOnMessage('Selecionar Botao Selecionar', self);
     SetObjSel(FProjeto);
     SetGlobalStatus(FProjeto.RealizarDiagnostico);
     getMessageManager.SendMessage(UM_LM_UPDATE_LAYER, [Map, Map]);
     end;
end;

// Leitura -----------------------------------------------------------------------------
procedure THidroAreaDeProjeto.LerDoArquivo(Ini: TIF);
var s: String;
begin
  s := 'Area de Projeto';
  
  FBloqueado := Ini.ReadBool    (s, 'Bloqueado', False);
  Left       := Ini.ReadInteger (s, 'x', Left);
  Top        := Ini.ReadInteger (s, 'y', Top);
  Width      := Ini.ReadInteger (s, 'Largura', Width);
  Height     := Ini.ReadInteger (s, 'Altura', Height);
  FMF        := Ini.ReadBool    (s, 'Mostrar Fundo', True);
  FMT        := Ini.ReadBool    (s, 'Mostrar Trechos', True);

  WindowState := TWindowState(Ini.ReadInteger (s, 'Estado Janela', ord(wsNormal)));

  Caption := ' ' + Ini.FileName;
end;

// Salvamento --------------------------------------------------------------------------

procedure THidroAreaDeProjeto.SalvarEmArquivo(Ini: TIF);
var s: String;
begin
  s := 'Area de Projeto';

  HorzScrollBar.Position := 0;
  VertScrollBar.Position := 0;

  Ini.WriteBool    (s, 'Bloqueado', FBloqueado);
  Ini.WriteInteger (s, 'x', Left);
  Ini.WriteInteger (s, 'y', Top);
  Ini.WriteInteger (s, 'Largura', Width);
  Ini.WriteInteger (s, 'Altura', Height);
  Ini.WriteBool    (s, 'Mostrar Fundo', FMF);
  Ini.WriteBool    (s, 'Mostrar Trechos', FMT);
  Ini.WriteInteger (s, 'Estado Janela', ord(WindowState));

  Caption := ' ' + Ini.FileName;
end;

procedure THidroAreaDeProjeto.Salvar;
begin
  SaveDialog.InitialDir := gDir;

  if FProjeto.NomeArquivo <> '' then
     FProjeto.SalvarEmArquivo(FProjeto.NomeArquivo)
  else
    if SaveDialog.Execute then
       begin
       gDir := ExtractFilePath(SaveDialog.FileName);
       FProjeto.SalvarEmArquivo(SaveDialog.FileName);
       end;
end;

procedure THidroAreaDeProjeto.DesenharSelecao(Selecionar: Boolean);
var oldBrushColor: TColor;
    oldPenColor  : TColor;
    R            : TRect;
    TD           : TTrechoDagua;
begin
  // desenha a seleção do obj. selecionado, se existe algum.
  if (FObjSel <> nil) then
     begin
     SaveCanvas;

     if DesenharFrame(FObjSel) then
        FObjSel.ImagemDoComponente.Selected := Selecionar
     else
        if (FObjSel is TTrechoDagua) then
           begin
           if Selecionar then
              FMapCanvas.Pen.color := clRed
           else
              FMapCanvas.Pen.color := clBlue;

           FMapCanvas.Pen.Style := psSolid;
           TD := TTrechoDagua(FObjSel);
           DesenhaSeta(FMapCanvas, TD.PC_aMontante.ScreenPos, TD.PC_aJusante.ScreenPos, 15,
              DistanciaEntre2Pontos(TD.PC_aMontante.ScreenPos, TD.PC_aJusante.ScreenPos) div 2);
           end;

     RestoreCanvas;
     Map.Invalidate;
     end;
end;

procedure THidroAreaDeProjeto.SetObjSel(const Value: THidroComponente);
const S1 = 'Objeto selecionado: ';
begin
  DesenharSelecao(False);

  FObjSel := Value;
  if Value = nil then
     WriteStatus(gSB, [''])
  else
     begin
     WriteStatus(gSB, [S1 + Value.Nome]);
     if Assigned(FOnMessage) then FOnMessage('Descrever Objeto', Value);
     end;

   if Assigned(FOnMessage) then FOnMessage('Atualizar Opcoes', self);

  DesenharSelecao(True);
end;

procedure THidroAreaDeProjeto.RemoverObjeto(Obj: THidroComponente);
begin
  if FProjeto.Simulador <> nil then Exit;

  SetObjSel(nil);

  if Obj is TPC then
     if FProjeto.PCs.Remover(TPC(Obj)) then
        // nada
     else
        Exit
  else
     if Obj is TTrechoDagua then
        begin
        TTrechoDagua(obj).PC_aMontante.RemoverTrecho;
        end
  else
     begin
     Obj.Avisar_e_Destruir;
     end;

  Repaint;
  FProjeto.Modificado := True;
end;

procedure THidroAreaDeProjeto.Form_CloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := MessageDLG(Format('Fechar o projeto <%s> ?', [FProjeto.Nome]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes;

  if CanClose Then
     begin
     FFechando := True;
     if FProjeto.Modificado and
        (MessageDLG(Format('Salvar as modificações do projeto "%s"?', [FProjeto.Nome]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        Salvar;
     end;
end;

function THidroAreaDeProjeto.DesenharFrame(obj: THidroComponente): boolean;
begin
  Result := (obj is TPC) or (obj is TSubBacia) or (obj is TDerivacao);
end;

procedure THidroAreaDeProjeto.ExecutarClick(Sender: TObject; p: ImoPoint; Obj: THidroComponente);
var PC: TPC;
    mo_p: ImoPoint;
begin
  if not Assigned(FOnGetButtonStatus) then
     raise Exception.Create('Evento Obter Status não Associado');

  if FOnGetButtonStatus('Conectar SB em PC') then
     FSB := nil;

  if FOnGetButtonStatus('Conectar Derivacao em PC') then
     FDer := nil
  else

  //---------------- Clicks na Janela -------------------------------------------------


  if FProjeto.Simulador <> nil then
     Exit
  else

  if FOnGetButtonStatus('Criar PC') or FOnGetButtonStatus('Criar Reservatorio') then
     begin
     if FOnGetButtonStatus('Criar PC') then
        Obj := Projeto.CriarObjeto('PC', p)
     else
        Obj := Projeto.CriarObjeto('Reservatorio', p);

     Projeto.PCs.Adicionar(TPC(Obj));

     WriteStatus(gSB, ['PC criado: ' + Obj.Nome]);
     FProjeto.Modificado := True;
     end else

//---------------- Clicks em um PC de Controle -----------------------------------

  if FOnGetButtonStatus('Criar SB') and (Obj is TPC) then
     begin
     PC := TPC(Obj);
     if ObjetoPodeSerConectadoEm('Sub-Bacia', PC) then
        begin
        Obj := Projeto.CriarObjeto('Sub-Bacia', PC.Pos);
        PC.ConectarObjeto(Obj);
        WriteStatus(gSB, ['Sub-Bacia criada: ' + Obj.Nome]);
        FProjeto.Modificado := True;
        Map.Refresh;
        end;
     end else

  if FOnGetButtonStatus('Criar Derivacao') and (Obj is TPC) then
     begin
     PC := TPC(Obj);
     if ObjetoPodeSerConectadoEm('Derivacao', PC) then
        begin
        Obj := Projeto.CriarObjeto('Derivacao', PC.Pos);
        PC.ConectarObjeto(Obj);
        WriteStatus(gSB, ['Derivação criada: ' + Obj.Nome]);
        FProjeto.Modificado := True;
        Map.Refresh;
        end;
     end else

  if FOnGetButtonStatus('Conectar SB em PC') then
     begin
     if Obj is TSubBacia then FSB := TSubBacia(Obj);
     if (Obj is TPC) and (FSB <> nil) then
        begin
        TPC(Obj).ConectarObjeto(FSB);
        Map.Refresh;
        FSB := nil;
        FProjeto.Modificado := True;
        end;
     end else

  if FOnGetButtonStatus('Conectar Derivacao em PC') then
     begin
     if Obj is TDerivacao then FDer := TDerivacao(Obj);
     if (Obj is TPC) and (FDer <> nil) then
        begin
        TPC(Obj).ConectarObjeto(FDer);
        Map.Refresh;
        FDer := nil;
        FProjeto.Modificado := True;
        end;
     end else

  if FOnGetButtonStatus('Criar TD') and (Obj is TPC) then
     begin
     if Fpc1 = nil then
        begin
        Fpc1 := TPC(Obj);

        // verifica se o PC não possui conecção a frente
        if Fpc1.PC_aJusante = nil then
           begin
           WriteStatus(gSB, [Format(cMsgStatus02, [Fpc1.Nome]), cMsgStatus03]);
           if Assigned(FOnShowMessage) then FOnShowMessage(cMsgAjuda04);
           end
        else
           begin
           if Assigned(FOnShowMessage) then FOnShowMessage(cMsgErro05);
           Fpc1 := nil;
           end;
        end
     else
        if Obj <> Fpc1 then
           if not Fpc1.Eh_umPC_aMontante(Obj) then
              begin
              if Assigned(FOnShowMessage) then FOnShowMessage('Novamente.'#13 + cMsgAjuda01);
              WriteStatus(gSB, [Format(cMsgStatus01, [Fpc1.Nome, Obj.Nome])]);
              Fpc1.ConectarObjeto(Obj);
              Map.Refresh;
              FProjeto.PCs.CalcularHierarquia;
              Fpc1 := nil;
              FProjeto.Modificado := True;
              end
           else
              begin
              if Assigned(FOnShowMessage) then
                 FOnShowMessage(Format(cMsgErro06, [Obj.Nome, Fpc1.Nome]));
              Fpc1 := nil;
              end;
     end else

  if FOnGetButtonStatus('Inserir PC') and (Obj is TPC) then
     begin
     if (Fpc1 = nil) then
        begin
        Fpc1 := TPC(Obj);
        if Fpc1.TrechoDagua <> nil then
           begin
           if Assigned(FOnShowMessage) then FOnShowMessage(cMsgAjuda06);
           WriteStatus(gSB, [Format(cMsgStatus04, [Fpc1.Nome]), cMsgStatus05]);
           end
        else
           begin
           WriteStatus(gSB, [Format(cMsgErro02, [Fpc1.Nome])]);
           Fpc1 := nil;
           end;
        end
     else
        if Obj = Fpc1.PC_aJusante then
           begin
           if Assigned(FOnShowMessage) then FOnShowMessage('');
           WriteStatus(gSB, [Format(cMsgStatus06, [Fpc1.Nome, Obj.Nome])]);

           Fpc1.RemoverTrecho;

           mo_p := MapObjects.CoPoint.Create;
           mo_p.X := (Fpc1.Pos.x + Obj.Pos.x) / 2;
           mo_p.Y := (Fpc1.Pos.y + Obj.Pos.y) / 2;

           PC := TPC(Projeto.CriarObjeto('PC', mo_p));
           Projeto.PCs.Adicionar(PC);
           Fpc1.ConectarObjeto(PC);
           PC.ConectarObjeto(Obj);

           Map.Refresh;
           FProjeto.PCs.CalcularHierarquia;
           Fpc1 := nil;
           FProjeto.Modificado := True;
           end
        else
           WriteStatus(gSB,
             [
             Format(cMsgStatus04, [Fpc1.Nome]),
             Format(cMsgErro01, [Obj.Nome, Fpc1.Nome])
             ]);
     end;
end;

procedure THidroAreaDeProjeto.AtualizacaoVisualDoUsuario;
begin
  // Nada neste nível
end;

procedure THidroAreaDeProjeto.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
     RemoverObjeto(FObjSel);
end;

procedure THidroAreaDeProjeto.Form_Resize(Sender: TObject);
begin
  if mmMensagemInicial <> nil then
     begin
     mmMensagemInicial.Top := ClientHeight div 2 - mmMensagemInicial.Height div 2;
     mmMensagemInicial.Left := ClientWidth div 2 - mmMensagemInicial.Width div 2;
     end;
end;

procedure THidroAreaDeProjeto.Form_Show(Sender: TObject);
begin
  if Height < 265 then Height := 265;
  if Width < 308 then Width := 308;
  self.Resize;
end;

function THidroAreaDeProjeto.CriarProjeto(TN: TStrings): TProjeto;
begin
  NaoImplementado('CriarProjeto', ClassName);
end;

function THidroAreaDeProjeto.ObjetoPodeSerConectadoEm(const ID: String; Objeto: THidroComponente): Boolean;
begin
  NaoImplementado('ObjetoPodeSerConectadoEm', ClassName);
end;

procedure THidroAreaDeProjeto.MapBeforeLayerDraw(ASender: TObject; index: Smallint; hDC: Cardinal);
var s          : String;
    i          : Integer;
    LEI        : Tmoec_Layer;
    L          : Variant;
begin
  if (index = Map.Layers.Count-1) then
     begin
     FZoomFactor := Map.FullExtent.Width / Map.Extent.Width;
     FScale := 100000 / FZoomFactor;

     s := ' Zoom: ' + FormatFloat('0.####', FZoomFactor);
     StatusBar.Panels.Items[0].Width := Canvas.TextWidth(s) + 15;
     StatusBar.Panels.Items[0].Text := s;

     s := ' Escala: ' + FormatFloat('0.######', FScale);
     StatusBar.Panels.Items[1].Text := s;

     // Visibilidade das camadas
     for i := 0 to Map.Layers.Count-1 do
       begin
       L := Map.Layers[i];
       LEI := Map.Layers.Layer_ExtraInfo[i];
       if LEI.UseScaleForVisibility then
          L.Visible := ((FScale >= LEI.MaxScale) or (LEI.MaxScale = 0)) and
                       ((FScale <= LEI.MinScale) or (LEI.MinScale = 0));
       end;
     end;
end;

procedure THidroAreaDeProjeto.Map_AfterLayerDraw(ASender: TObject; index: Smallint; canceled: WordBool; hDC: Cardinal);
var b: Boolean;
begin
  if index = 0 then
     begin
     FMapCanvas.Handle := hDC;
     DesenharRede;
     {
     b := (FScale <= 70000);
     GetMessageManager.SendMessage(UM_SET_VISIBILITY, [@b]);
     if b then
        DesenharRede;
     }
     end;
end;

procedure THidroAreaDeProjeto.SaveCanvas;
begin
  inc(FLockCanvas);
  if FLockCanvas = 1 then
     begin
     FPenMode := FMapCanvas.Pen.Mode;
     FPenColor := FMapCanvas.Pen.Color;
     FBrushColor := FMapCanvas.Brush.Color;
     end;
end;

procedure THidroAreaDeProjeto.RestoreCanvas;
begin
  Dec(FLockCanvas);
  if FLockCanvas = 0 then
     begin
     FMapCanvas.Pen.Mode := FPenMode;
     FMapCanvas.Pen.Color := FPenColor;
     FMapCanvas.Brush.Color := FBrushColor;
     end;
end;

end.


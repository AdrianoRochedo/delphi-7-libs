unit Form_BaseChart;

{
  AUTOR ............................................... Adriano Rochedo Conceição
  DATA DA CRIAÇÃO ..................................... 14/08/1999
  VERSÃO
    1.01      - Melhoramentos Gerais
    1.02      - Tratamento apropriado para estilo MDIChild
    1.10      - Renomeamento das classes
              - Criacao dos Frames
    1.20      - Series funcoes (Media, tendencia)          
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls, Buttons,
  CandleCh, BubbleCh, OHLChart, TeeProcs, TeEngine, Chart, Series, CurvFitt,
  DiretivasDeCompilacao,
  XML_Interfaces,
  ChartBaseClasses;

type

  TfoBaseChart = class(TForm, IToBXML)
    Menu: TPopupMenu;
    Menu_OpcoesGerais: TMenuItem;
    Menu_Series: TMenuItem;
    N1: TMenuItem;
    Menu_Imprimir: TMenuItem;
    Menu_Copiar: TMenuItem;
    N2: TMenuItem;
    Menu_Destruir: TMenuItem;
    N3: TMenuItem;
    Menu_SalvarComo: TMenuItem;
    Save: TSaveDialog;
    Menu_SempreNoTopo: TMenuItem;
    pNav: TPanel;
    sbLeft: TSpeedButton;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    sbRight: TSpeedButton;
    p3D: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cb3D: TCheckBox;
    sb3D: TScrollBar;
    sbRotacao: TScrollBar;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    sbZoomNormal: TSpeedButton;
    sbOpt: TSpeedButton;
    Menu_Rastrear: TMenuItem;
    Sep4: TMenuItem;
    Menu_MostrarMedia: TMenuItem;
    Menu_MostrarTendencia: TMenuItem;
    procedure MenuPopup(Sender: TObject);
    procedure Menu_SeriesClick(Sender: TObject);
    procedure Menu_OpcoesGeraisClick(Sender: TObject);
    procedure Menu_ImprimirClick(Sender: TObject);
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Evento_Fechar(Sender: TObject; var Action: TCloseAction);
    procedure Menu_DestruirClick(Sender: TObject);
    procedure Menu_SalvarComoClick(Sender: TObject);
    procedure Menu_SempreNoTopoClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure sbZoomNormalClick(Sender: TObject);
    procedure sbLeftClick(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure sbRightClick(Sender: TObject);
    procedure cb3DClick(Sender: TObject);
    procedure sb3DChange(Sender: TObject);
    procedure sbRotacaoChange(Sender: TObject);
    procedure sbOptClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Menu_RastrearClick(Sender: TObject);
    procedure Menu_MostrarMediaClick(Sender: TObject);
    procedure Menu_MostrarTendenciaClick(Sender: TObject);
  private
    FOldCaption: String;
    FLS: TSeriesList;
    FReleaseForm: Boolean;
    FList: TfoChartList;
    FMedia: TLineSeries;
    FTend: TLineSeries;
    Procedure HorizScroll(Const Percent:Double);
    Procedure VertScroll(Const Percent:Double);
    Procedure ScrollAxis(Axis:TChartAxis; Const Percent:Double);
  protected
    function getChart: TChart; virtual; abstract;
    procedure DoSeriesChange(Serie: TChartSeries); virtual;
  public
    Constructor Create(const aCaption: String = '');
    Destructor Destroy; override;

    procedure Init(const aCaption: String = '');
    procedure Refresh();

    // XML interfaces
    procedure ToXML(Buffer: TStrings; Ident: Integer);
    function GetBlockName: String;
    function GetBlockNameComment: String;
    procedure ToBXML(Buffer: TStrings; Ident: Integer);
    function GetClassName: String;

    procedure EditSerie(Serie: TChartSeries);
    procedure Show(FormStyle: TFormStyle);

    property Series : TSeriesList read FLS;
    property List   : TfoChartList read FList write FList;

    // Indica se a janela deve ser destruida quando fechada
    property ReleaseFormOnClose : Boolean read FReleaseForm write FReleaseForm;

    // Componente gráfico
    property Chart : TChart read getChart;
  end;

implementation
uses TeeChartUtils,
     DialogoBase_OpcoesSeries,
     Dialogo_OpcoesBarSeries,
     Dialogo_OpcoesGeraisGrafico,
     Dialogo_OpcoesSerieLinhas,
     Dialogo_OpcoesSeriePontos,
     Dialogo_OpcoesSeriePizza,
     Dialogo_OpcoesSerieVelas;

{$R *.DFM}

{ TfoBaseChart }

procedure TfoBaseChart.Init(const aCaption: String = '');
begin
  Caption := aCaption;
  FoldCaption := aCaption;
  FLS := TSeriesList.Create;
  FLS.Chart := Self.getChart();
  FReleaseForm := True;
  Chart.Title.Text.Clear;
  Chart.Foot.Text.Clear;
  ClientWidth := 500;
  ClientHeight := 400; // - 70 por causa dos paines
end;

constructor TfoBaseChart.Create(const aCaption: String = '');
begin
  inherited Create(nil);
  Init(aCaption);
end;

destructor TfoBaseChart.Destroy;
begin
  FLS.Free;
  inherited Destroy;
end;

procedure TfoBaseChart.Evento_Fechar(Sender: TObject; var Action: TCloseAction);
begin
  if FReleaseForm then
     begin
     Action := caFree;
     if FList <> nil then FList.Remove(self);
     end
  else
     if FormStyle = fsMDIChild then
        Action := caMinimize
     else
        Action := caHide;
end;

procedure TfoBaseChart.Refresh();
begin
  Chart.Invalidate();
end;

procedure TfoBaseChart.MenuPopup(Sender: TObject);
var i: integer;
    m: TMenuItem;
begin
  while Menu_Series.Count > 0 do
    Menu_Series.Delete(Menu_Series.Count-1);

  for i := 0 to Chart.SeriesCount-1 do
    begin
    m := NewItem(FLS[i].Title, 0, False, True, Menu_SeriesClick, 0, 'MS_' + intToStr(i));
    m.Tag := integer(FLS[i]); // põe a referência a série i no Tag do Menu
    Menu_Series.Add(m);
    end;

  i := 1;
  if FMedia <> nil then inc(i);
  if FTend <> nil then inc(i);
  Menu_MostrarTendencia.Enabled := (Chart.SeriesCount = i);
end;

procedure TfoBaseChart.Menu_SeriesClick(Sender: TObject);
var m: TMenuItem;
    s: TChartSeries;
begin
  m := TMenuItem(Sender);
  if m.Tag <> 0 then
     begin
     s := TChartSeries(m.Tag);
     EditSerie(s);
     end;
end;

procedure TfoBaseChart.Menu_OpcoesGeraisClick(Sender: TObject);
var d: TgrDialogo_OpcoesGeraisGrafico;
begin
  d := TgrDialogo_OpcoesGeraisGrafico.Create(nil);
  d.Grafico := Chart;
  d.ShowModal;
  d.Free;
  cb3D.Checked := Chart.View3D;
end;                                         

procedure TfoBaseChart.Menu_ImprimirClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Chart.Print();
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TfoBaseChart.Menu_CopiarClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 1}
  Chart.CopyToClipboardBitmap();
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TfoBaseChart.HorizScroll(const Percent: Double);
begin
  ScrollAxis(Chart.TopAxis, Percent);
  ScrollAxis(Chart.BottomAxis, Percent);
end;

procedure TfoBaseChart.ScrollAxis(Axis: TChartAxis; const Percent: Double);
var Amount:Double;
begin
  With Axis do
  begin
    Amount:=-((Maximum-Minimum)/(100.0/Percent));
    SetMinMax(Minimum-Amount,Maximum-Amount);
  end;
end;

procedure TfoBaseChart.VertScroll(const Percent: Double);
begin
  ScrollAxis(Chart.LeftAxis, Percent);
  ScrollAxis(Chart.RightAxis, Percent);
end;

procedure TfoBaseChart.DoSeriesChange(Serie: TChartSeries);
begin
  // nada
end;

procedure TfoBaseChart.Show(FormStyle: TFormStyle);
begin
  self.FormStyle := FormStyle;
  inherited Show;
end;

function TfoBaseChart.GetBlockName: String;
begin
  Result := 'TeeChart';
end;

function TfoBaseChart.GetBlockNameComment: String;
begin
  Result := 'Plugin para renderização de gráficos TeeChart';
end;

procedure TfoBaseChart.ToXML(Buffer: TStrings; Ident: Integer);
begin
  TeeChartUtils.ToXML(getChart(), Buffer, Ident);
end;

procedure TfoBaseChart.ToBXML(Buffer: TStrings; Ident: Integer);
var s: String;
begin
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<' + GetBlockName + ':block>');
  ToXML(Buffer, Ident);
  Buffer.Add(s + '</' + GetBlockName + ':block>');
end;

function TfoBaseChart.GetClassName: String;
begin
  Result := self.ClassName;
end;

procedure TfoBaseChart.Menu_DestruirClick(Sender: TObject);
begin
  Menu_Destruir.Checked := not Menu_Destruir.Checked;
  FReleaseForm := Menu_Destruir.Checked;
end;

procedure TfoBaseChart.Menu_SalvarComoClick(Sender: TObject);
begin
  {$IF dir_NivelDeRestricao < 2}
  if Save.Execute then
     Chart.SaveToBitmapFile(Save.FileName);
  {$ELSE}
  MessageDLG(mes_NivelDeRestricao, mtInformation, [mbOk], 0);
  {$IFEND}
end;

procedure TfoBaseChart.Menu_SempreNoTopoClick(Sender: TObject);
begin
  Menu_SempreNoTopo.Checked := not Menu_SempreNoTopo.Checked;
  if Menu_SempreNoTopo.Checked then
     self.FormStyle := fsStayOnTop
  else
     self.FormStyle := fsNormal;
end;

procedure TfoBaseChart.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
Var XDelta,YDelta,
    XRange,YRange:Double;
begin
  if Chart.SeriesCount = 0 then Exit;

  { initialize some temporary variables... }
  XDelta:=0;
  YDelta:=0;
  With FLS[0].GetHorizAxis do XRange := Maximum-Minimum;
  With FLS[0].GetVertAxis  do YRange := Maximum-Minimum;

  { handle keyboard !!! }
  if ssShift in Shift then
     begin
     Case key of
       VK_LEFT,VK_UP    : Chart.ZoomPercent( 120 );
       VK_RIGHT,VK_DOWN : Chart.ZoomPercent( 80 );
       end;
     exit;
     end
  else
     Case key of
       VK_LEFT  : XDelta:=-XRange/100;
       VK_RIGHT : XDelta:= XRange/100;
       VK_UP    : YDelta:= YRange/100;
       VK_DOWN  : YDelta:=-YRange/100;
       vk_Next  : YDelta:=-YRange/10;
       vk_Prior : YDelta:= YRange/10;
       VK_SPACE : Begin
                  Chart.UndoZoom();
                  Exit;
                  End;  { <-- reset scrolling }
       end;

  { just to make this example a little better... }
  if {not InvertScroll.Checked} false then
     begin
     XDelta:=-XDelta;
     YDelta:=-YDelta;
     end;

  { apply scrolling !!! }
  With Chart do
    Begin
    LeftAxis.Scroll(YDelta, False);
    RightAxis.Scroll(YDelta, False);
    BottomAxis.Scroll(XDelta, False);
    TopAxis.Scroll(XDelta, False);
    SetFocus();
    End;
end;

procedure TfoBaseChart.sbZoomInClick(Sender: TObject);
begin
  Chart.ZoomPercent(120);
end;

procedure TfoBaseChart.sbZoomOutClick(Sender: TObject);
begin
  Chart.ZoomPercent(80);
end;

procedure TfoBaseChart.sbZoomNormalClick(Sender: TObject);
begin
  Chart.UndoZoom;
end;

procedure TfoBaseChart.sbLeftClick(Sender: TObject);
begin
  HorizScroll(10);
end;

procedure TfoBaseChart.sbUpClick(Sender: TObject);
begin
  VertScroll(-10);
end;

procedure TfoBaseChart.sbDownClick(Sender: TObject);
begin
  VertScroll(10);
end;

procedure TfoBaseChart.sbRightClick(Sender: TObject);
begin
  HorizScroll(-10);
end;

procedure TfoBaseChart.cb3DClick(Sender: TObject);
begin
  Chart.View3D      := cb3D.Checked;
  sb3D.Enabled      := Chart.View3D;
  sbRotacao.Enabled := Chart.View3D;
end;

procedure TfoBaseChart.sb3DChange(Sender: TObject);
begin
  Chart.Chart3DPercent := sb3D.Position;
end;

procedure TfoBaseChart.sbRotacaoChange(Sender: TObject);
var i: Integer;
begin
  if sbRotacao.Enabled then
     With Chart.View3DOptions do
       begin
       Orthogonal := False;
       for i := 0 to Chart.SeriesCount-1 do
          if FLS[i] is TPieSeries then
             Elevation := sbRotacao.Position
          else
             Rotation := sbRotacao.Position;
       end;
end;

procedure TfoBaseChart.sbOptClick(Sender: TObject);
var P: TPoint;
begin
  P := Point(sbOpt.Left, sbOpt.Top);
  P := pNav.ClientToScreen(P);
  Menu.Popup(P.x, P.y);
end;

procedure TfoBaseChart.FormShow(Sender: TObject);
begin
  cb3D.Checked := Chart.View3D;
  Chart.Legend.LegendStyle := lsSeries;
  Menu_SempreNoTopo.Visible := (Self.FormStyle <> fsMDIChild);
  Menu_Destruir.Visible := Menu_SempreNoTopo.Visible;
end;

procedure TfoBaseChart.Menu_RastrearClick(Sender: TObject);
begin
  Menu_Rastrear.Checked := not Menu_Rastrear.Checked;
  if Menu_Rastrear.Checked then
     begin
     FOldCaption := Caption;
     Chart.View3D := False;
     cb3D.Checked := False;
     end
  else
     Caption := FOldCaption;
end;

procedure TfoBaseChart.Menu_MostrarMediaClick(Sender: TObject);
var c: TChart;
    i: integer;
    k: integer;
    t: real;
begin
  Menu_MostrarMedia.Checked := not Menu_MostrarMedia.Checked;
  if FMedia = nil then
     begin
     c := self.getChart();
     FMedia := TLineSeries.Create(c);
     FMedia.Name := 'Media';
     FMedia.ParentChart := c;
     FMedia.Title := 'Média';
     k := 0;
     t := 0;
     for i := 0 to c.SeriesCount-1 do
       begin
       // Nao considera a tendencia
       if (c.Series[i].Name = 'Tendencia') or (c.Series[i].Name = FMedia.Name) then Continue;

       t := t + c.Series[i].YValues.Total;
       k := k + c.Series[i].Count;
       end;
     if k > 0 then t := t / k else t := 0;
     FMedia.AddXY(c.BottomAxis.Minimum, t);
     FMedia.AddXY(c.BottomAxis.Maximum, t);
     end
  else
     begin
     FreeAndNil(FMedia);
     end;
end;

procedure TfoBaseChart.Menu_MostrarTendenciaClick(Sender: TObject);
var c: TChart;
    i: integer;
begin
  Menu_MostrarTendencia.Checked := not Menu_MostrarTendencia.Checked;
  if FTend = nil then
     begin
     c := self.getChart();
     FTend := TLineSeries.Create(c);
     FTend.Name := 'Tendencia';
     FTend.ParentChart := c;
     FTend.Title := 'Tendência';
     for i := 0 to c.SeriesCount-1 do
       begin
       // Nao considera a Media
       if (c.Series[i].Name = 'Media') or (c.Series[i].Name = FTend.Name) then Continue;
       FTend.DataSources.Add(c.Series[i]);
       end;
     FTend.SetFunction(TTrendFunction.Create(FTend));
     end
  else
     begin
     FreeAndNil(FTend);
     end;
end;

procedure TfoBaseChart.EditSerie(Serie: TChartSeries);
var d: TgrDialogo_BaseOpcoesSeries;
begin
  if Serie is TBarSeries    then d := TgrDialogo_OpcoesSerieBarras.Create(nil) else
  if Serie is TCandleSeries then d := TgrDialogo_OpcoesSerieVelas.Create(nil)  else
  if Serie is TLineSeries   then d := TgrDialogo_OpcoesSerieLinhas.Create(nil) else
  if Serie is TPointSeries  then d := TgrDialogo_OpcoesSeriePontos.Create(nil) else
  if Serie is TPieSeries    then d := TgrDialogo_OpcoesSeriePizza.Create(nil)
  else
     Exit;

  d.Serie := Serie;
  if d.ShowModal = mrOk then DoSeriesChange(Serie);
  d.Free();
end;

end.

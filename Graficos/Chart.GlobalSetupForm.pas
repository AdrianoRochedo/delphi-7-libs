unit Chart.GlobalSetupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TeeProcs, TeEngine, Chart, Series, ComCtrls;

type
  TfoGlobalSetup = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    btnCancelar: TButton;
    Arvore: TTreeView;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Chart: TChart;
    Panel3: TPanel;
    cbTextos: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ArvoreDblClick(Sender: TObject);
  private
    FS1 : TLineSeries;
    FS2 : TBarSeries;

    procedure Edit(const item: string);
    procedure Set_SetTitleText(const Value: boolean);
    function get_SetTitleText: boolean;
  public
    // Diz se os textos dos titulos deverao ser substituidos pelos desta janela
    property SetTitleText : boolean read get_SetTitleText write set_SetTitleText;
  end;

implementation
uses TreeViewUtils,
     DialogUtils;

{$R *.dfm}

const
  cAlignment: array[0..2] of string = ('Justificado a esquerda', 'Justificado a direita', 'Centro');

procedure TfoGlobalSetup.FormCreate(Sender: TObject);
begin
  FS1 := TLineSeries.Create(Chart);
  FS1.ParentChart := Chart;
  FS1.FillSampleValues(20);
  FS1.Title := 'Linhas';

  FS2 := TBarSeries.Create(Chart);
  FS2.ParentChart := Chart;
  FS2.FillSampleValues(10);
  FS2.Title := 'Barras';

  Arvore.Items[0].Expand(true);
end;

procedure TfoGlobalSetup.Edit(const item: string);
var i: integer;
begin
  {------------------- LEGENDA -------------------}

  if System.Pos('legenda\visível', item) > 0 then
     Chart.Legend.Visible := not Chart.Legend.Visible
  else
  if System.Pos('legenda\fonte', item) > 0 then
     DialogUtils.selectFont(Chart.Legend.Font)
  else
  if System.Pos('legenda\posição', item) > 0 then
     begin
     i := ord(Chart.Legend.Alignment);
     if DialogUtils.selectItem(i, ['Esquerda', 'Direita', 'Em cima', 'Em baixo']) <> '' then
        Chart.Legend.Alignment := TLegendAlignment(i);
     end
  else

  {------------------- TÍTULOS -------------------}

  if System.Pos('títulos\de cima\visível', item) > 0 then
     Chart.Title.Visible := not Chart.Title.Visible
  else
  if System.Pos('títulos\de baixo\visível', item) > 0 then
     Chart.Foot.Visible := not Chart.Foot.Visible
  else
  if System.Pos('títulos\de cima\cor', item) > 0 then
     Chart.Title.Color := DialogUtils.selectColor(Chart.Title.Color)
  else
  if System.Pos('títulos\de baixo\cor', item) > 0 then
     Chart.Foot.Color := DialogUtils.selectColor(Chart.Foot.Color)
  else
  if System.Pos('títulos\de cima\fonte', item) > 0 then
     DialogUtils.selectFont(Chart.Title.Font)
  else
  if System.Pos('títulos\de baixo\fonte', item) > 0 then
     DialogUtils.selectFont(Chart.Foot.Font)
  else
  if System.Pos('títulos\de cima\texto', item) > 0 then
     DialogUtils.editText(Chart.Title.Text)
  else
  if System.Pos('títulos\de baixo\texto', item) > 0 then
     DialogUtils.editText(Chart.Foot.Text)
  else
  if System.Pos('títulos\de cima\alinhamento', item) > 0 then
     begin
     i := ord(Chart.Title.Alignment);
     if DialogUtils.selectItem(i, cAlignment) <> '' then
        Chart.Title.Alignment := TAlignment(i);
     end
  else
  if System.Pos('títulos\de baixo\alinhamento', item) > 0 then
     begin
     i := ord(Chart.Foot.Alignment);
     if DialogUtils.selectItem(i, cAlignment) <> '' then
        Chart.Foot.Alignment := TAlignment(i);
     end
  else

  {------------------- GERAL -------------------}

  if System.Pos('gerais\cor do gráfico', item) > 0 then
     Chart.BackColor := DialogUtils.selectColor(Chart.BackColor)
  else
  if System.Pos('gerais\cor do painel', item) > 0 then
     Chart.Color := DialogUtils.selectColor(Chart.Color)
  else
  if System.Pos('gerais\eixos visíveis', item) > 0 then
     Chart.AxisVisible := not Chart.AxisVisible
  else
  if System.Pos('gerais\percentagem 3d', item) > 0 then
     Chart.Chart3DPercent := DialogUtils.SelectInteger('Percentagem 3D', Chart.Chart3DPercent)
  else
  if System.Pos('gerais\vis. mono', item) > 0 then
     Chart.Monochrome := not Chart.Monochrome
  else
  if System.Pos('gerais\visualizar em 3d', item) > 0 then
     Chart.View3D := not Chart.View3D
  else
  if System.Pos('gerais\visualizar paredes', item) > 0 then
     Chart.View3DWalls := not Chart.View3DWalls
  else
  if System.Pos('gerais\margens\esq', item) > 0 then
     Chart.MarginLeft := DialogUtils.SelectInteger('Margem Esquerda', Chart.MarginLeft)
  else
  if System.Pos('gerais\margens\dir', item) > 0 then
     Chart.MarginRight := DialogUtils.SelectInteger('Margem Direita', Chart.MarginRight)
  else
  if System.Pos('gerais\margens\de cima', item) > 0 then
     Chart.MarginTop := DialogUtils.SelectInteger('Margem de cima', Chart.MarginTop)
  else
  if System.Pos('gerais\margens\de baixo', item) > 0 then
     Chart.MarginBottom := DialogUtils.SelectInteger('Margem de baixo', Chart.MarginBottom)
  else

end;

procedure TfoGlobalSetup.ArvoreDblClick(Sender: TObject);
var item: string;
begin
  item := TreeViewUtils.MakePath(Arvore.Selected, 1);
  Edit( SysUtils.LowerCase(item) );
end;

procedure TfoGlobalSetup.Set_SetTitleText(const Value: boolean);
begin
  cbTextos.Checked := Value;
end;

function TfoGlobalSetup.get_SetTitleText(): boolean;
begin
  result := cbTextos.Checked;
end;

end.

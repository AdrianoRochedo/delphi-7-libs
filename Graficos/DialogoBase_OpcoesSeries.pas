unit DialogoBase_OpcoesSeries;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, teEngine, Dialogo_Opcoes_Labels;

type
  TgrDialogo_BaseOpcoesSeries = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edTitulo: TEdit;
    cbAtivada: TCheckBox;
    cbLegenda: TCheckBox;
    ColorDLG: TColorDialog;
    btnCor: TBitBtn;
    PainelCor: TPanel;
    cbMarcadores: TCheckBox;
    btnOk: TBitBtn;
    btnLabels: TButton;
    procedure btnCorClick(Sender: TObject);
    procedure cbAtivadaClick(Sender: TObject);
    procedure cbLegendaClick(Sender: TObject);
    procedure cbMarcadoresClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edTituloChange(Sender: TObject);
    procedure btnLabelsClick(Sender: TObject);
  protected
    FSerie: TChartSeries;
    LabelsDLG: TfoDialogo_Opcoes_Labels;
  public
    property Serie: TChartSeries read FSerie write FSerie;
  end;

implementation

{$R *.DFM}

procedure TgrDialogo_BaseOpcoesSeries.btnCorClick(Sender: TObject);
begin
  if ColorDLG.Execute then
     begin
     PainelCor.Color := ColorDLG.Color;
     FSerie.SeriesColor := ColorDLG.Color;
     end;
end;

procedure TgrDialogo_BaseOpcoesSeries.cbAtivadaClick(Sender: TObject);
begin
  FSerie.Active := cbAtivada.Checked;
end;

procedure TgrDialogo_BaseOpcoesSeries.cbLegendaClick(Sender: TObject);
begin
  FSerie.ShowInLegend := cbLegenda.Checked;
end;

procedure TgrDialogo_BaseOpcoesSeries.cbMarcadoresClick(Sender: TObject);
begin
  FSerie.Marks.Visible := cbMarcadores.Checked;
end;

procedure TgrDialogo_BaseOpcoesSeries.FormShow(Sender: TObject);
begin
  edTitulo.Text        := FSerie.Title;
  PainelCor.Color      := FSerie.SeriesColor;
  cbAtivada.Checked    := FSerie.Active;
  cbLegenda.Checked    := FSerie.ShowInLegend;
  cbMarcadores.Checked := FSerie.Marks.Visible;
end;

procedure TgrDialogo_BaseOpcoesSeries.edTituloChange(Sender: TObject);
begin
  FSerie.Title := edTitulo.Text;
end;

procedure TgrDialogo_BaseOpcoesSeries.btnLabelsClick(Sender: TObject);
begin
  with TfoDialogo_Opcoes_Labels.Create(FSerie) do
    begin
    ShowModal();
    Free();
    end;
end;

end.

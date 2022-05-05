unit Dialogo_OpcoesSerieVelas;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogoBase_OpcoesSeries, ExtCtrls, StdCtrls, Buttons, Spin;

type
  TgrDialogo_OpcoesSerieVelas = class(TgrDialogo_BaseOpcoesSeries)
    se_Largura: TSpinEdit;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    se_Espessura: TSpinEdit;
    Label4: TLabel;
    cb_Estilo: TComboBox;
    procedure se_LarguraChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCorClick(Sender: TObject);
    procedure se_EspessuraChange(Sender: TObject);
    procedure cb_EstiloChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  grDialogo_OpcoesSerieVelas: TgrDialogo_OpcoesSerieVelas;

implementation
uses
  CandleCh;
{$R *.DFM}

procedure TgrDialogo_OpcoesSerieVelas.se_LarguraChange(Sender: TObject);
begin
  inherited;
  TCandleSeries(FSerie).CandleWidth:= se_Largura.Value;
end;

procedure TgrDialogo_OpcoesSerieVelas.FormShow(Sender: TObject);
begin
  inherited;
  cb_Estilo.ItemIndex:= Ord(TCandleSeries(FSerie).Pen.Style);

//  se_Altura.Value := TPointSeries(FSerie).Pointer.VertSize;
  se_Largura.Value:= TCandleSeries(FSerie).CandleWidth;
  PainelCor.Color:= TCandleSeries(FSerie).DownCloseColor;
  se_Espessura.Value:= TCandleSeries(FSerie).Pen.Width;
end;

procedure TgrDialogo_OpcoesSerieVelas.btnCorClick(Sender: TObject);
begin
  inherited;
  TCandleSeries(FSerie).DownCloseColor:= ColorDLG.Color;
end;

procedure TgrDialogo_OpcoesSerieVelas.se_EspessuraChange(Sender: TObject);
begin
  inherited;
  TCandleSeries(FSerie).Pen.Width:= se_Espessura.Value;
end;

procedure TgrDialogo_OpcoesSerieVelas.cb_EstiloChange(Sender: TObject);
begin
  inherited;
{  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
    psInsideFrame);}
  TCandleSeries(FSerie).Pen.Style:= TPenStyle(cb_Estilo.ItemIndex);
end;

end.

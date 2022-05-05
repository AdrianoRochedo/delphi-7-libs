unit Dialogo_OpcoesSeriePontos;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogoBase_OpcoesSeries, ExtCtrls, StdCtrls, Buttons, Spin;

type
  TgrDialogo_OpcoesSeriePontos = class(TgrDialogo_BaseOpcoesSeries)
    GroupBox2: TGroupBox;
    se_Largura: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    se_Altura: TSpinEdit;
    Label2: TLabel;
    cb_Formato: TComboBox;
    procedure cb_FormatoChange(Sender: TObject);
    procedure se_LarguraChange(Sender: TObject);
    procedure se_AlturaChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  grDialogo_OpcoesSeriePontos: TgrDialogo_OpcoesSeriePontos;

implementation
uses
  Series;

{$R *.DFM}

procedure TgrDialogo_OpcoesSeriePontos.cb_FormatoChange(Sender: TObject);
begin
  inherited;
{  TSeriesPointerStyle=(psRectangle,psCircle,psTriangle,psDownTriangle,
                       psCross,psDiagCross,psStar,psDiamond,psSmallDot);}
  TPointSeries(FSerie).Pointer.Style:= TSeriesPointerStyle(cb_Formato.ItemIndex);
end;

procedure TgrDialogo_OpcoesSeriePontos.se_LarguraChange(Sender: TObject);
begin
  inherited;
  TPointSeries(FSerie).Pointer.HorizSize:= se_Largura.Value;
end;

procedure TgrDialogo_OpcoesSeriePontos.se_AlturaChange(Sender: TObject);
begin
  inherited;
  TPointSeries(FSerie).Pointer.VertSize:= se_Altura.Value;
end;

procedure TgrDialogo_OpcoesSeriePontos.FormShow(Sender: TObject);
begin
  inherited;
  cb_Formato.ItemIndex:= Ord(TPointSeries(FSerie).Pointer.Style);

  se_Altura.Value := TPointSeries(FSerie).Pointer.VertSize;
  se_Largura.Value:= TPointSeries(FSerie).Pointer.HorizSize;
end;

end.

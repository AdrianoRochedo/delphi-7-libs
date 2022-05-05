unit Form_moLayerProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, drEdit, MapObjectsEx, Buttons;

type
  TForm_moLayerProps = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    cbVC: TCheckBox;
    edEMin: TdrEdit;
    edEMax: TdrEdit;
    btnOk: TBitBtn;
    btnCancelar: TBitBtn;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  protected
    FLayer: Tmoec_Layer;

    // mostra as prop. da camada no diálogo
    procedure ShowLayerProps(Layer: Tmoec_Layer); virtual;

    // pega as prop. da camada do diálogo
    procedure GetLayerProps(Layer: Tmoec_Layer); virtual;
  public
    class procedure ShowProps(Layer: Tmoec_Layer);
  end;

implementation

{$R *.dfm}

{ TForm_moLayerProps }

class procedure TForm_moLayerProps.ShowProps(Layer: Tmoec_Layer);
begin
  with TForm_moLayerProps.Create(nil) do
    begin
    FLayer := Layer;
    ShowModal;
    Free;
    end;
end;

procedure TForm_moLayerProps.FormShow(Sender: TObject);
begin
  ShowLayerProps(FLayer);
end;

procedure TForm_moLayerProps.btnOkClick(Sender: TObject);
begin
  GetLayerProps(FLayer);
  FLayer.Map.Refresh();
  Close;
end;

procedure TForm_moLayerProps.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TForm_moLayerProps.ShowLayerProps(Layer: Tmoec_Layer);
begin
  cbVC.Checked := Layer.UseScaleForVisibility;
  edEMin.AsInteger := Layer.MinScale;
  edEMax.AsInteger := Layer.MaxScale;
end;

procedure TForm_moLayerProps.GetLayerProps(Layer: Tmoec_Layer);
begin
  Layer.UseScaleForVisibility := cbVC.Checked;
  Layer.MinScale := edEMin.AsInteger;
  Layer.MaxScale := edEMax.AsInteger;
end;

end.
 
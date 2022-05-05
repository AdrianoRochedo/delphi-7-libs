unit Dialogo_OpcoesSeriePizza;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogoBase_OpcoesSeries, ExtCtrls, StdCtrls, Buttons, Spin;

type
  TgrDialogo_OpcoesSeriePizza = class(TgrDialogo_BaseOpcoesSeries)
    GroupBox2: TGroupBox;
    cb_Visivel: TCheckBox;
    Label2: TLabel;
    cb_Estilo: TComboBox;
    Label3: TLabel;
    se_Largura: TSpinEdit;
    btn_Cor: TBitBtn;
    PainelCor2: TPanel;
    Label4: TLabel;
    se_AfastarMaior: TSpinEdit;
    cb_Padrao: TCheckBox;
    Label5: TLabel;
    se_Girar: TSpinEdit;
    procedure cb_EstiloChange(Sender: TObject);
    procedure cb_VisivelClick(Sender: TObject);
    procedure btnCorClick(Sender: TObject);
    procedure se_LarguraChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure se_AfastarMaiorChange(Sender: TObject);
    procedure cb_PadraoClick(Sender: TObject);
    procedure se_GirarChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  grDialogo_OpcoesSeriePizza: TgrDialogo_OpcoesSeriePizza;

implementation
uses
  Series;

{$R *.DFM}

procedure TgrDialogo_OpcoesSeriePizza.cb_EstiloChange(Sender: TObject);
begin
  inherited;
{  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
    psInsideFrame);}
  TPieSeries(FSerie).PiePen.Style:= TPenStyle(cb_Estilo.ItemIndex);
end;

procedure TgrDialogo_OpcoesSeriePizza.cb_VisivelClick(Sender: TObject);
begin
  inherited;
  TPieSeries(FSerie).PiePen.Visible:= cb_Visivel.Checked;
end;

procedure TgrDialogo_OpcoesSeriePizza.btnCorClick(Sender: TObject);
begin
  inherited;
    if ColorDLG.Execute then
     begin
     TPieSeries(FSerie).PiePen.Color:= ColorDLG.Color;
     PainelCor2.Color:= ColorDLG.Color;
     end;
end;

procedure TgrDialogo_OpcoesSeriePizza.se_LarguraChange(Sender: TObject);
begin
  inherited;
  TPieSeries(FSerie).PiePen.Width:= se_Largura.Value;
end;

procedure TgrDialogo_OpcoesSeriePizza.FormShow(Sender: TObject);
begin
  inherited;
  cb_Estilo.ItemIndex:= Ord(TPieSeries(FSerie).PiePen.Style);

  PainelCor2.Color:= TPieSeries(FSerie).PiePen.Color;
  se_Largura.Value:= TPieSeries(FSerie).PiePen.Width;
end;

procedure TgrDialogo_OpcoesSeriePizza.se_AfastarMaiorChange(Sender: TObject);
begin
  inherited;
//  TPieSeries(FSerie).InstanceSize:= se_AfastarMaior.Value;
{  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
    pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
    pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);}

//  TPieSeries(FSerie).UsePatterns:= se_AfastarMaior.Value;
end;

procedure TgrDialogo_OpcoesSeriePizza.cb_PadraoClick(Sender: TObject);
begin
  inherited;
  TPieSeries(FSerie).UsePatterns:= cb_Padrao.Checked;
end;

procedure TgrDialogo_OpcoesSeriePizza.se_GirarChange(Sender: TObject);
begin
  inherited;
  TPieSeries(FSerie).RotationAngle:= se_Girar.Value;
end;

end.

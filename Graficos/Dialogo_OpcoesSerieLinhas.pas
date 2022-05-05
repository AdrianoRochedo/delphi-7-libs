unit Dialogo_OpcoesSerieLinhas;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogoBase_OpcoesSeries, ExtCtrls, StdCtrls, Buttons;

type
  TgrDialogo_OpcoesSerieLinhas = class(TgrDialogo_BaseOpcoesSeries)
    GroupBox2: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    procedure TipoTraco_Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses Series;

{$R *.DFM}

procedure TgrDialogo_OpcoesSerieLinhas.TipoTraco_Click(Sender: TObject);
begin
  // psSolid, psDash, psDot, psDashDot, psDashDotDot
  TLineSeries(FSerie).LinePen.Style := TPenStyle(TComponent(Sender).Tag);
end;

end.

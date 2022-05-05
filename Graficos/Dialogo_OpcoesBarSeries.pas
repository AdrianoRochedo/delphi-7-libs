unit Dialogo_OpcoesBarSeries;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogoBase_OpcoesSeries, ExtCtrls, StdCtrls, Buttons;

type
  TgrDialogo_OpcoesSerieBarras = class(TgrDialogo_BaseOpcoesSeries)
    GroupBox2: TGroupBox;
    r0: TRadioButton;
    r1: TRadioButton;
    r2: TRadioButton;
    r3: TRadioButton;
    GroupBox3: TGroupBox;
    e0: TRadioButton;
    e1: TRadioButton;
    e2: TRadioButton;
    e3: TRadioButton;
    e4: TRadioButton;
    e5: TRadioButton;
    e6: TRadioButton;
    procedure Dirtribuicao_Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure estilo_Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses Series;

{$R *.DFM}

procedure TgrDialogo_OpcoesSerieBarras.Dirtribuicao_Click(Sender: TObject);
begin
  TBarSeries(FSerie).MultiBar := TMultiBar(TComponent(Sender).Tag);
end;

procedure TgrDialogo_OpcoesSerieBarras.FormShow(Sender: TObject);
var i: byte;
begin
  inherited;
  i := ord(TBarSeries(FSerie).MultiBar);
  TRadioButton(FindComponent('r' + intToStr(i))).Checked := True;

  i := ord(TBarSeries(FSerie).BarStyle);
  TRadioButton(FindComponent('e' + intToStr(i))).Checked := True;
end;

procedure TgrDialogo_OpcoesSerieBarras.estilo_Click(Sender: TObject);
begin
  TBarSeries(FSerie).BarStyle := TBarStyle(TComponent(Sender).Tag);
end;

end.




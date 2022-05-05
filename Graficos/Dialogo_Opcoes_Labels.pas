unit Dialogo_Opcoes_Labels;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, teEngine;

type
  TfoDialogo_Opcoes_Labels = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Labels: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FSerie: TChartSeries;
  public
    constructor Create(Serie: TChartSeries);
  end;

var
  foDialogo_Opcoes_Labels: TfoDialogo_Opcoes_Labels;

implementation

{$R *.dfm}

procedure TfoDialogo_Opcoes_Labels.Button2Click(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TfoDialogo_Opcoes_Labels.Button1Click(Sender: TObject);
var i: Integer;
begin
  for i := 0 to FSerie.Count-1 do
    FSerie.XLabel[i] := Labels.Lines[i];

  Close;
  ModalResult := mrOk;
end;

constructor TfoDialogo_Opcoes_Labels.Create(Serie: TChartSeries);
var i: Integer;
begin
  inherited Create(nil);
  FSerie := Serie;
  for i := 0 to Serie.Count-1 do
    Labels.Lines.Add(Serie.XLabel[i]);
end;

end.

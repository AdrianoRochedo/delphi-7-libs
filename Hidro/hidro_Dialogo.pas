unit hidro_Dialogo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  THidroDialogo = class(TForm)
    edNome: TEdit;
    P1: TPanel;
    Panel2: TPanel;
    mComentarios: TMemo;
    btnOk: TBitBtn;
    btnCancelar: TBitBtn;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private
    procedure SetBloqueado(const Value: Boolean);
  protected
    ObjetoPai : TObject;
    Nomes     : TStrings;      // Tabela de Nomes dos objetos
  public
    constructor Create(Nomes: TStrings; ObjetoPai: TObject);
    property Bloqueado: Boolean write SetBloqueado;
  end;

implementation
uses hidro_Classes,
     SysUtilsEx;

const
  cMsgErro  = 'O nome "%s" já pertence a outro objeto !'#13 +
              'Por favor, escolha um nome que não exista.';

{$R *.DFM}

constructor THidroDialogo.Create(Nomes: TStrings; ObjetoPai: TObject);
begin
  inherited Create(nil);
  self.Nomes := Nomes;
  self.ObjetoPai := ObjetoPai;
end;

procedure THidroDialogo.btnOkClick(Sender: TObject);
var TudoOk: Boolean;
begin
  edNome.Text := AllTrim(edNome.Text);

  if Nomes.IndexOf(edNome.Text) > -1 then
     raise Exception.CreateFmt(cMsgErro, [edNome.Text]);

  modalResult := mrOk
end;

procedure THidroDialogo.btnCancelarClick(Sender: TObject);
begin
  modalResult := mrCancel;
end;

procedure THidroDialogo.SetBloqueado(const Value: Boolean);
begin
  btnOk.Enabled := not Value;
end;

end.

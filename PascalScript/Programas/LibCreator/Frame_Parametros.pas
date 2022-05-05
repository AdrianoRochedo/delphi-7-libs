unit Frame_Parametros;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Tfrm_Parametros = class(TFrame)
    GbParametros: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    cbTipoPar: TComboBox;
    btnAdicionarPar: TButton;
    btnRemoverPar: TButton;
    btnModificarPar: TButton;
    lbParametros: TListBox;
    GroupBox1: TGroupBox;
    rbC: TRadioButton;
    rbR: TRadioButton;
    cbTipoParClasse: TComboBox;
    procedure btnAdicionarParClick(Sender: TObject);
    procedure btnRemoverParClick(Sender: TObject);
    procedure btnModificarParClick(Sender: TObject);
  private
    function ObtemParametrosDaRotina: String;
  public
    { Public declarations }
  end;

implementation
uses WinUtils;

{$R *.DFM}

function Tfrm_Parametros.ObtemParametrosDaRotina: String;
begin
  Result := cbTipoPar.Text + ':';

  if cbTipoPar.ItemIndex = 0 then
     Result := Result + cbTipoParClasse.Text + ':'
  else
     Result := Result + 'nil:';

  if rbC.Checked then
     Result := Result + 'False'
  else
     Result := Result + 'True';
end;

procedure Tfrm_Parametros.btnAdicionarParClick(Sender: TObject);
begin
  if (cbTipoPar.ItemIndex = 0) and (cbTipoParClasse.Text = '') then
     ShowErrorAndGoto(['Classe do Parâmetro não definida'], cbTipoParClasse);

  lbParametros.Items.Add(ObtemParametrosDaRotina);
end;

procedure Tfrm_Parametros.btnRemoverParClick(Sender: TObject);
begin
  if lbParametros.ItemIndex > -1 then
     lbParametros.Items.Delete(lbParametros.ItemIndex);
end;

procedure Tfrm_Parametros.btnModificarParClick(Sender: TObject);
begin
  if lbParametros.ItemIndex > -1 then
     lbParametros.Items[lbParametros.ItemIndex] := ObtemParametrosDaRotina;
end;

end.

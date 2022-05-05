unit Editor_TrocarTexto;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TReplaceDLG = class(TForm)
    Label1: TLabel;
    edProcurar: TEdit;
    edTrocar: TEdit;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    cbCS: TCheckBox;
    cbSPI: TCheckBox;
    Bevel1: TBevel;
    rbBF: TRadioButton;
    rbBT: TRadioButton;
    Panel1: TPanel;
    rbTT: TRadioButton;
    rbTS: TRadioButton;
    btnOk: TBitBtn;
    btnCancelar: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReplaceDLG: TReplaceDLG;

implementation

{$R *.DFM}

end.

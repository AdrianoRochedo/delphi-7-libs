unit hidro_Dialogo_Projeto;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  hidro_Dialogo;

type
  THidroDialogo_Projeto = class(THidroDialogo)
    SpeedButton1: TSpeedButton;
    Panel7: TPanel;
    edDirSai: TEdit;
    SpeedButton2: TSpeedButton;
    Panel8: TPanel;
    edDirPes: TEdit;
    procedure btnProcurarClick(Sender: TObject);
  private
    { Private declarations }
  end;

implementation
uses // Gerais
     FileCTRL,
     SysUtilsEx,
     ErrosDLG,
     WinUtils,
     FileUtils,

     // Projeto
     Hidro_Classes,
     Hidro_Variaveis,
     Hidro_Constantes;

{$R *.DFM}

procedure THidroDialogo_Projeto.btnProcurarClick(Sender: TObject);
var s: String;
begin
  inherited;
  case TComponent(Sender).Tag of
    0, 1:
      if not SelectDirectory('Selecione um Diretório', '', s) then exit;
    end;

  TProjeto(ObjetoPai).RetirarCaminhoSePuder(s);

  case TComponent(Sender).Tag of
    0: edDirSai.Text    := s;
    1: edDirPes.Text    := s;
    end;
end;

end.

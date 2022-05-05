unit CellPosDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Frame_Memo, ExtCtrls, drEdit;

type
  TCellPosDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    laMSG: TLabel;
    edL: TdrEdit;
    edC: TdrEdit;
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
  private
    { Private declarations }
  public
    class function getCellPos(const MSG: string; var Row, Col: integer): boolean;
  end;

implementation

{$R *.dfm}

{ TCellPosDialog }

class function TCellPosDialog.getCellPos(const MSG: string; var Row, Col: integer): boolean;
var d: TCellPosDialog;
begin
  d := TCellPosDialog.Create(nil);
  d.laMSG.Caption := MSG;
  d.edL.AsInteger := Row;
  d.edC.AsInteger := Col;
  result := (d.ShowModal() = mrOk);
  if result then
     begin
     Row := d.edL.AsInteger;
     Col := d.edC.AsInteger;
     end;
  d.Free();
end;

end.

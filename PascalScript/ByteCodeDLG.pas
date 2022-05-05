unit ByteCodeDLG;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Gridx32;

type
  TByteCode_DLG = class(TForm)
    IC: TdrStringAlignGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ICChooseFont(aCol, aRow: Integer; aState: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TByteCode_DLG.FormCreate(Sender: TObject);
begin
  IC.Cells[0,0] := 'Linha';
  IC.Cells[1,0] := 'Operação';
  IC.Cells[2,0] := 'Operando 1';
  IC.Cells[3,0] := 'Operando 2';
  IC.Cells[4,0] := 'i_Op 1';
  IC.Cells[5,0] := 'i_Op 2';
  IC.Cells[6,0] := 'Resultado';
end;

procedure TByteCode_DLG.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TByteCode_DLG.ICChooseFont(aCol, aRow: Integer; aState: TGridDrawState);
var s: String;
begin
  if (aCol = 0) or (aRow = 0) then
     IC.Canvas.Font.Color := clBlack
  else
     begin
     s := IC.Cells[1, aRow];
     if (s = 'IF') or (s = 'GOTO') then
        IC.Canvas.Font.Color := clRed else
     if (s = ':=') then
        IC.Canvas.Font.Color := clBlue else
     if (s = 'ADD PAR') then
        IC.Canvas.Font.Color := clMaroon else
     if (s = 'CALL') or (s = 'HALT') or (s = 'CO') then
        IC.Canvas.Font.Color := clBlack else
     if (s = '+') or (s = '-') or (s = '*') or (s = '/') or (s = '~') or (s = '!') or
        (s = '=') or (s = '<>') or (s = '<') or (s = '<=') or (s = '>') or (s = '>=') or
        (s = 'DIV') or (s = 'MOD') or (s = 'AND') or (s = 'OR') then
        IC.Canvas.Font.Color := clGreen;
     end;
end;

end.

unit fDBTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, RADBTreeView, Db, DBTables, StdCtrls, Grids, DBGrids,
  RADBUtil, DBCtrls, RAHint, RARegAuto, RADBCt;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Table1: TTable;
    RADBTreeView1: TRADBTreeView;
    DataSource1: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    DBGrid1: TDBGrid;
    RASQLScript1: TRASQLScript;
    Database1: TDatabase;
    DBText1: TDBText;
    RAHint1: TRAHint;
    RegAuto1: TRegAuto;
    Label3: TLabel;
    procedure Table1NewRecord(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.DFM}

procedure TForm1.Table1NewRecord(DataSet: TDataSet);
begin
  Table1['Uni'] := RADBUtil.GetQueryResult(Table1.DatabaseName,
     'select max(Uni) from "Tree.dbf"') + 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TDrawGrid(DBGrid1).OnMouseMove := GridMouseMove;
  try
    Table1.DeleteTable;
  except
  end;
  RASQLScript1.Execute;
  Table1.Open;
  {$IFNDEF VER120}
   Label3.Free;
  {$ENDIF}
end;

procedure TForm1.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  G : TGridCoord;
  R : TRect;
begin
//  G := TDrawGrid(DBGrid1).MouseCoord(X, Y);
  TDrawGrid(DBGrid1).MouseToCell(X, Y, G.X, G.Y);

  R := TDrawGrid(DBGrid1).CellRect(G.X, G.Y);
  OffsetRect(R, DBGrid1.ClientOrigin.X, DBGrid1.ClientOrigin.Y);
  RAHint1.ActivateHint(R, Format('X: %d, Y: %d', [G.X, G.Y]));
end;

end.

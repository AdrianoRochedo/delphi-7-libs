unit fTable;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Grids, DBGrids;

type
  TForm1 = class(TForm)
    RegAuto1: TRegAuto;
    Button1: TButton;
    tEmployeeFirstName: TStringField;
    tEmployeeLastName: TStringField;
    DataSource1: TDataSource;
    dgEmployee: TDBGrid;
    tEmployee: TTable;
    Table1: TTable;
    procedure BtnDeleteRecordClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BtnDeleteRecordClick(Sender: TObject);
begin
  tEmployee.Edit;
  tEmployee.Delete;
end;

end.

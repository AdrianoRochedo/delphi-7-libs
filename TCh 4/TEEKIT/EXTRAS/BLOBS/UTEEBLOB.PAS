unit uteeblob;

interface

{ Place the TEEBLOB.* files on the \Delphi\Demos\Data folder,
  or change the Table1.DatabaseName property.

}

{ This unit shows how to read a Chart component from a BLOB
  field on a TTable or TQuery component.
}
uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, Mask, DBCtrls, TeeProcs, TeEngine, Chart,
  ExtCtrls, Grids, DBGrids;

type
  TFormChartBlob = class(TForm)
    Table1: TTable;
    DataSource1: TDataSource;
    Chart1: TChart;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Table1Description: TStringField;
    Table1Chart: TBlobField;
    procedure Table1NewRecord(DataSet: TDataSet);
    procedure Table1BeforePost(DataSet: TDataSet);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Saving:Boolean;
    Function ChartInBlob:Boolean;
  end;

var
  FormChartBlob: TFormChartBlob;

implementation

{$R *.DFM}
Uses TeeStore,EditChar;

{ Create a new Chart when inserting a new record... }
procedure TFormChartBlob.Table1NewRecord(DataSet: TDataSet);
begin
  Chart1.Free;
  Chart1:=TChart.Create(Self);
  Chart1.Parent:=Self;
  Chart1.Align:=alClient;
end;

{ Save the Chart to the Table record BLOB field }
procedure TFormChartBlob.Table1BeforePost(DataSet: TDataSet);
var tmp:TBlobStream;
begin
  Saving:=True;
  tmp:=TBlobStream.Create(Table1Chart,bmWrite);
  try
    SaveChartToStream(Chart1,tmp);
  finally
    tmp.Free;
  end;
  Saving:=False;
end;

{ Is the blob field empty ? }
Function TFormChartBlob.ChartInBlob:Boolean;
begin
  with TBlobStream.Create(Table1Chart,bmRead) do
  try
    Result := Size>0;
  finally
    Free;
  end;
end;

{ re-load another Chart from another BLOB of another record.... }
procedure TFormChartBlob.DataSource1DataChange(Sender: TObject; Field: TField);
var tmp:TBlobStream;
    tmpChart:TCustomChart;
begin
  if (not Saving) and ChartInBlob then
  begin
    tmp:=TBlobStream.Create(Table1Chart,bmRead);
    try
      Chart1.Free;
      tmpChart:=TChart.Create(Self);
      try
        LoadChartFromStream(tmpChart,tmp);
      except
        on E:Exception do ShowMessage(E.Message);
      end;
      Chart1:=TChart(tmpChart);
      Chart1.Align:=alClient;
      Chart1.Parent:=Self;
      Label2.Caption:=IntToStr(tmp.Size);
    finally
      tmp.Free;
    end;
  end;
end;

{ open the Table }
procedure TFormChartBlob.FormCreate(Sender: TObject);
begin
  Table1.Open;
  TeeEraseBack:=False;
  Saving:=False;
end;

{ Edit the Chart }
procedure TFormChartBlob.Button1Click(Sender: TObject);
begin
  Table1.Edit;
  EditChart(Self,Chart1);
end;

end.

unit fDBMove;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, DBTables, StdCtrls, ComCtrls, RADBMove, RADBCt,
  Buttons, RADlg, ExtCtrls;

type
  TForm1 = class(TForm)
    tSource1: TTable;
    dsSource1: TDataSource;
    tSource2: TTable;
    dsSource2: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    Label7: TLabel;
    Label9: TLabel;
    RADBMove1: TRADBMove;
    RASQLScript1: TRASQLScript;
    DestinationDatabase: TDatabase;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    tDestination1: TTable;
    dsDestination1: TDataSource;
    tDestination2: TTable;
    dsDestination2: TDataSource;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    TabSheet4: TTabSheet;
    BitBtn3: TBitBtn;
    RAProgressForm1: TRAProgressForm;
    Button1: TButton;
    Panel1: TPanel;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    Label11: TLabel;
    Label12: TLabel;
    Memo2: TMemo;
    Label13: TLabel;
    Memo3: TMemo;
    BitBtn4: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RADBMove1MoveRecord(Sender: TRADBMove; Table: TTable;
      var Action: TMoveAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure RAProgressForm1Show(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private
    Table1Uni: integer;
    Table2Uni: integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses RAUtils, RADBUtil;

{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  DestinationDatabase.Close;
  DestinationDatabase.Params.Clear;
  DestinationDatabase.Params.Add('PATH=' + GetTempDir);
  DestinationDatabase.Open;
  try
    RADBUtil.ExecuteSQLScript(DestinationDatabase,
      'drop table "CUSTOMER.DB";', ctAll, nil, 0);
    RADBUtil.ExecuteSQLScript(DestinationDatabase,
      'drop table "MYORDERS.DB";', ctAll, nil, 0);
  except
    { All Ok }
  end;
  RASQLScript1.Execute;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  Table1Uni := 1;
  Table2Uni := 1;
  RAProgressForm1.Execute;
  // see RAProgressForm1.OnShow
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  try
    tDestination1.Close;
    tDestination2.Close;
    RADBUtil.ExecuteSQLScript(DestinationDatabase,
      'drop table "CUSTOMER.DB";', ctAll, nil, 0);
    RADBUtil.ExecuteSQLScript(DestinationDatabase,
      'drop table "MYORDERS.DB";', ctAll, nil, 0);
  except
    { All Ok }
  end;
end;

procedure TForm1.RAProgressForm1Show(Sender: TObject);
begin
  RADBMove1.Execute;
  tDestination1.Open;
  tDestination2.Open;
end;

procedure TForm1.RADBMove1MoveRecord(Sender: TRADBMove; Table: TTable;
  var Action: TMoveAction);
begin
  if RAUtils.Cmp(Table.TableName, 'CUSTOMER') then
  begin
    Table.Fields[0].AsInteger := Table1Uni;
    inc(Table1Uni);
  end
  else if RAUtils.Cmp(Table.TableName, 'MYORDERS') then
  begin
    Table.Fields[0].AsInteger := Table2Uni;
    inc(Table2Uni);
  end;
  RAProgressForm1.InfoLabel := Table.Fields[1].AsString;
  RAProgressForm1.ProgressMax := RADBMove1.RecordCount;
  RAProgressForm1.ProgressPosition := RADBMove1.CurrentRecord;
  Application.ProcessMessages;
  if RAProgressForm1.Cancel then
    Abort;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  tSource1.Open;
  tSource2.Open;
  Memo1.Lines := RADBMove1.Tables;
  Memo2.Lines := RADBMove1.References;
  Memo3.Lines := RADBMove1.Mappings;
end;



procedure TForm1.Button1Click(Sender: TObject);
begin
  PageControl1.SelectNextPage(true);
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  Close;
end;

end.


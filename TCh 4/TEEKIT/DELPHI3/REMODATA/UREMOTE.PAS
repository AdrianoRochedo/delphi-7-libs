unit Uremote;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs, Chart, DBChart, DBClient,
  Db;

type
  TForm1 = class(TForm)
    ClientDataSet1: TClientDataSet;
    RemoteServer1: TRemoteServer;
    DBChart1: TDBChart;
    Series1: THorizBarSeries;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClientDataSet1.Open;
end;


end.

program teeblob;

uses
  Forms,
  uteeblob in 'uteeblob.pas' {FormChartBlob};

{$R *.RES}

begin
  Application.CreateForm(TFormChartBlob, FormChartBlob);
  Application.Run;
end.

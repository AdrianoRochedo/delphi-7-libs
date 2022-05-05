unit RunTimeForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TRunTime = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RunTime: TRunTime;

implementation

{$R *.DFM}

procedure TRunTime.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

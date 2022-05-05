unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses OutPut, sysutil2;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var x: TOutPut;
begin
  x := TOutPut.Create;
  x.write('fjsdlfsldfksdl');
  x.write('fjsdlfsldfksdl');
  x.write('fjsdlfsldfksdl sdsdsdjh sdsjdajsjd');
  x.Show;

  Delay(100);

  x.write('sfsfsfsdffjsdlfsldfksdl');
  x.write('fjsdlfslddsfsfsfsfsfsdffksdl');
  x.Show;
  x.free;
end;

end.

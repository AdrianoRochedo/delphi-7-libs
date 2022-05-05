{$INCLUDE RA.INC}

unit Unit1;

interface
     
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RACt, RAHint, RARegAuto;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    RegAuto1: TRegAuto;
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  if (Memo1.Lines.Count > 0) and
     (Memo1.Lines[Memo1.Lines.Count - 1] = '') then
    Memo1.Lines.Delete(Memo1.Lines.Count - 1);
  Button1.Click;
  Application.HintHidePause := MaxInt;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Hint := Memo1.Text;
end;

initialization
  RegisterHtHints;
end.

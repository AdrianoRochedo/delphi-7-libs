unit QR2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TReport1 = class(TForm)
    QRDBText4: TQRDBText;
    QRDBText3: TQRDBText;
    QRDBText2: TQRDBText;
    QRLabel1: TQRLabel;
    QRShape1: TQRShape;
    QRDBText1: TQRDBText;
    QuickRep1: TQuickRep;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Report1: TReport1;

implementation

{$R *.DFM}

procedure TReport1.FormShow(Sender: TObject);
begin
end;

procedure TReport1.FormCreate(Sender: TObject);
begin
end;


end.

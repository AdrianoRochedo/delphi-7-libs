unit hidro_Form_GlobalMessages;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  ThidroForm_GlobalMessages = class(TForm)
    mm: TMemo;
  private
    procedure setMessage(const Value: String);
    { Private declarations }
  public
    procedure Clear;
    property Message: String write setMessage;
  end;

implementation

{$R *.dfm}

{ ThidroForm_GlobalMessages }

procedure ThidroForm_GlobalMessages.Clear;
begin
  mm.Lines.Clear;
end;

procedure ThidroForm_GlobalMessages.setMessage(const Value: String);
begin
  mm.Lines.Text := Value;
end;

end.

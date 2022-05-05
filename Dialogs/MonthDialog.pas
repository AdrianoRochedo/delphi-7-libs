unit MonthDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMonthDialog = class(TForm)
    Label1: TLabel;
    cbMonth: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    class function getMonth(out Month: integer): boolean;
  end;

implementation

{$R *.dfm}

{ TMonthDialog }

class function TMonthDialog.getMonth(out Month: integer): boolean;
var d: TMonthDialog;
begin
  d := TMonthDialog.Create(nil);
  result := (d.ShowModal() = mrOk);
  if result then
     Month := d.cbMonth.ItemIndex + 1
  else
     Month := 0;
  d.Free();   
end;

procedure TMonthDialog.FormCreate(Sender: TObject);
var i: Integer;
begin
  for i := 1 to 12 do
    cbMonth.Items.Add(SysUtils.LongMonthNames[i]);

  cbMonth.ItemIndex := 0;  
end;

end.

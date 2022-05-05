unit ComboBoxDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TComboBoxDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox: TComboBox;
  private
    { Private declarations }
  public
    class function getSelected(var Selected: integer; const Items: array of string): string;
  end;

implementation

{$R *.dfm}

{ TComboBoxDialog }

class function TComboBoxDialog.getSelected(var Selected: integer; const Items: array of string): string;
var d: TComboBoxDialog;
    i: integer;
begin
  d := TComboBoxDialog.Create(nil);

  for i := 0 to High(Items) do
    d.ComboBox.Items.Add(Items[i]);

  if Selected < d.ComboBox.Items.Count then
     d.ComboBox.ItemIndex := Selected;

  if (d.ShowModal() = mrOk) and (d.ComboBox.itemIndex > -1) then
     begin
     Selected := d.ComboBox.itemIndex;
     result := d.ComboBox.Items[Selected];
     end
  else
     begin
     result := '';
     Selected := -1;
     end
end;

end.

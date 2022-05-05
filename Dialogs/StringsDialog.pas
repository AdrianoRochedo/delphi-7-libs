unit StringsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Frame_Memo;

type
  TStringsDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    fr_Memo: Tfr_Memo;
  public
    class function getStrings(Strings: TStrings): boolean;
  end;

implementation

{$R *.dfm}

{ TMonthDialog }

class function TStringsDialog.getStrings(Strings: TStrings): boolean;
var d: TStringsDialog;
begin
  d := TStringsDialog.Create(nil);
  d.fr_Memo.Memo.Lines.Assign(Strings);
  result := (d.ShowModal() = mrOk);
  if result then
     Strings.Assign(d.fr_Memo.Memo.Lines);
  d.Free();
end;

end.

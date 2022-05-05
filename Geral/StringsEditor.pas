unit StringsEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfoStringsEditor = class(TForm)
    Panel1: TPanel;
    Memo: TMemo;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FStrings: TStrings;
    constructor Create(Strings: TStrings);
  public
    class function Edit(Strings: TStrings): boolean;
  end;

implementation

{$R *.dfm}

procedure TfoStringsEditor.btnCancelClick(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TfoStringsEditor.btnOkClick(Sender: TObject);
begin
  FStrings.Assign(Memo.Lines);
  Close;
  ModalResult := mrOk;
end;

constructor TfoStringsEditor.Create(Strings: TStrings);
begin
  inherited Create(nil);
  FStrings := Strings;
  Memo.Lines.Assign(FStrings);
end;

class function TfoStringsEditor.Edit(Strings: TStrings): boolean;
var d: TfoStringsEditor;
begin
  d := TfoStringsEditor.Create(Strings);
  result := d.ShowModal() = mrOk;
  d.Free();
end;

end.

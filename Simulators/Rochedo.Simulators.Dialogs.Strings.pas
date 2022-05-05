unit Rochedo.Simulators.Dialogs.Strings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StringsDialog, Frame_Memo, StdCtrls,
  Rochedo.Dialogs.EditorServices;

type
  TSC_StringsDialog = class(TStringsDialog, IEditable)
  private
    function edit(obj: TObject): TModalResult;
  end;

implementation
uses Rochedo.Simulators.Components;

{$R *.dfm}

{ TSC_StringsDialog }

function TSC_StringsDialog.edit(obj: TObject): TModalResult;
var o: TStringListObject;
begin
  o := TStringListObject(obj);
  self.fr_Memo.Memo.Lines.Assign(o.Strings);
  result := ShowModal();
  if result = mrOk then o.Strings.Assign(self.fr_Memo.Memo.Lines);
  Release();
end;

initialization
  getEditorServices().registerEditor('TSC_StringsDialog', TSC_StringsDialog);

end.

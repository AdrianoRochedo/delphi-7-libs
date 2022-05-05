var
  MyForm: TForm;
begin
  MyForm := RAI2MakeForm(ExePath + 'samples\fModalForm.pas');
  if MyForm.ShowModal = mrOk then
    showmessage('OK button clicked');
  MyForm.Free;
end;
